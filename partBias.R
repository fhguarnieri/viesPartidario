#
# Calcula viés majoritário e viés partidário nas eleiçõe para a 
# Câmara dos Deputados no Brasil entre 1998-2022.
#

# Com base em Rodden e Calvo (2011) uso um modelo multinomial multinível
# para a distibuição de cadeiras com hiper-priores para o parametro Rho
# dependendo da distribuição de votos de cada partido no interior de cada
# distrito, medida pelo índice de Gini.

# A base de dados consiste dos resultados eleitorais por partido
# e município divulgados pelo TSE e tratados pelo "Base dos Dados".

# Carrega as bibliotecas

library(tidyverse)
library(brms)

# função para calcullar Gini

#gini <- function(y){
#  n <- length(y)
#  i <- 1:length(y)
#  ((sum((2*i - n - 1)*y))/(n*sum(y)))}

# Carrega a base

vp <- read.csv("votoPartidoMun.csv")

vp <- vp[vp$ano > 1994,]

# Como o sistema partidário muda muito juntei alguns partidos
# que se fundiram e criei categorias outros (esquerda centro e direita)
# Isso faz com que tenhamos dados cmpletos para todos anos.

esq <- c("80","77","54","50","43","29","21","18","16","33")
centro <- c("35","90","70","55","31")
dir <- c("51","56","36","30","28","27","26","20","19","10")

# Mudo na base de votação

vp <- vp %>% mutate(
  nov_num = if_else(numero_partido %in% esq, 91,
                    if_else(numero_partido %in% centro, 92,
                            if_else(numero_partido %in% dir, 93,
                                    numero_partido)))
  )


# jogo os votos do psl antes de 18 na direita e do PSD pré 2011 no PTB

vp <- vp %>% mutate(
  nov_num = if_else(nov_num == 17 & ano < 2018, 93,nov_num),
  nov_num = if_else(nov_num == 41 & ano < 2010, 14,nov_num))

# Cria siglas para os 'novos partidos'

vp <- vp %>% mutate(
  nov_sigla = if_else(nov_num == 91, "OE",
                      if_else(nov_num == 92,"OC",
                              if_else(nov_num == 93,"OD",
                                      sigla_partido)))
)


# Coloco os que migraram do UB para o PL no PL
# (ver  "https://g1.globo.com/politica/eleicoes/2022/noticia/2022/03/23/
# entenda-por-que-uniao-brasil-perde-maior-bancada-e-pl-cresce-na-janela-
# partidaria-segundo-especialistas.ghtml")

# Primeiro as bancadas que migraram 100% para o PL
vp <- vp %>% mutate(
  nov_num = if_else(nov_num == 17 &
                      ano == 2018 &
                      sigla_uf %in% c("SC","RO","RN","MT"), 22,nov_num)
  )

# Agora as bancadas que se dividiram. Mudo de PSL para PL aleatoriamete na
# proporção da migração

vp$nov_num[sample(which(vp$nov_num == 17 &
                                 vp$ano == 2018 &
                                 vp$sigla_uf == "SP"),
                         round(length(vp$nov_num[vp$nov_num == 17 &
                                                    vp$ano == 2018 &
                                                    vp$sigla_uf == "SP"])*0.4))] <- 22

vp$nov_num[sample(which(vp$nov_num == 17 &
                                 vp$ano == 2018 &
                                 vp$sigla_uf == "MS"),
                         round(length(vp$nov_num[vp$nov_num == 17 &
                                                   vp$ano == 2018 &
                                                   vp$sigla_uf == "MS"])*0.5))] <- 22

vp$nov_num[sample(which(vp$nov_num == 17 &
                                 vp$ano == 2018 &
                                 vp$sigla_uf == "RS"),
                         round(length(vp$nov_num[vp$nov_num == 17 &
                                                   vp$ano == 2018 &
                                                   vp$sigla_uf == "RS"])*(1/3)))] <- 22
vp$nov_num[sample(which(vp$nov_num == 17 &
                                 vp$ano == 2018 &
                                 vp$sigla_uf == "RJ"),
                         round(length(vp$nov_num[vp$nov_num == 17 &
                                                   vp$ano == 2018 &
                                                   vp$sigla_uf == "RJ"])*(5/12)))] <- 22
vp$nov_num[sample(which(vp$nov_num == 17 &
                                 vp$ano == 2018 &
                                 vp$sigla_uf == "MG"),
                         round(length(vp$nov_num[vp$nov_num == 17 &
                                                   vp$ano == 2018 &
                                                   vp$sigla_uf == "MG"])*(4/6)))] <- 22


# Acerta sigla do PL e do PTB

vp <- vp %>% mutate(
  nov_sigla = if_else(nov_num == 22, "PL",nov_sigla),
  nov_sigla = if_else(nov_num == 14, "PTB",nov_sigla)
)

# Coloca quem sobrou no DEM

vp <- vp %>% mutate(nov_num = if_else(nov_num == 17, 25,nov_num))

# Transforma União Brasil em DEM

vp <- vp %>% mutate(nov_num = if_else(nov_num == 44, 25,nov_num))

# Acerta as siglas

vp <- vp %>% mutate(
  nov_sigla = if_else(nov_num == 25,"DEM", nov_sigla),
  nov_sigla = if_else(nov_num == 23,"CIDADANIA", nov_sigla),
  nov_sigla = if_else(nov_num == 15,"MDB", nov_sigla),
  nov_sigla = if_else(nov_num == 11,"PP", nov_sigla))



rowSums(vs[c(2:6,13,15,24,28)], na.rm = T)

# Calcula votos válidos e share do partido por estado

#vpag_old <- vpag # fiz outra versão sem juntar partidos

vpag <- vp %>%
  group_by(ano,sigla_uf,nov_num,nov_sigla) %>%
  summarise(v = sum(votos_nominais + votos_legenda))

vpag <- vpag %>%
  group_by(ano,sigla_uf) %>%
  mutate(vv = sum(v), vs = v/vv)

# Calcula o gini. Para isso ordenamos a base pela proporção de votos part/mun

vpag <- vpag %>% arrange(ano,sigla_uf,vs)

vpag <- vpag %>%
  group_by(ano, nov_sigla) %>%
  mutate(G = gini(vs)) %>%
  ungroup()

# Junta com base de cadeiras (base de deputados  eleitos TSE)

anos <- c(1998,2002,2006,2010,2014,2018,2022)

bancada <- data.frame(ANO_ELEICAO = NA, SIGLA_UE = NA, NUMERO_PARTIDO = NA, cad = NA)

for(i in 1:length(anos)){
  bk <- read.csv(paste("bancada", anos[i],".csv", sep = ""))
  
  bk <- bk %>%
    group_by(ANO_ELEICAO,SIGLA_UE,NUMERO_PARTIDO) %>%
    summarise(cad = n())
  
  bancada <- rbind(bancada,bk)
}

bancada <- bancada[-1,]

# Muda os partidos conforme feito acima

bancada2 <- bancada %>% mutate(
  nov_num = if_else(NUMERO_PARTIDO %in% esq, 91,
                    if_else(NUMERO_PARTIDO %in% centro, 92,
                            if_else(NUMERO_PARTIDO %in% dir, 93,
                                    NUMERO_PARTIDO)))
)


# jogo os votos do psl antes de 18 na direita e do PSD pré 2011 no PTB

bancada2 <- bancada2 %>% mutate(
  nov_num = if_else(nov_num == 17 & ANO_ELEICAO < 2018, 93,nov_num),
  nov_num = if_else(nov_num == 41 & ANO_ELEICAO < 2010, 14,nov_num))

# Cria siglas para os 'novos partidos'

# Coloco os que migraram do UB para o PL no PL
# (ver  "https://g1.globo.com/politica/eleicoes/2022/noticia/2022/03/23/
# entenda-por-que-uniao-brasil-perde-maior-bancada-e-pl-cresce-na-janela-
# partidaria-segundo-especialistas.ghtml")

# Primeiro as bancadas que migraram 100% para o PL
bancada2 <- bancada2 %>% mutate(
  nov_num = if_else(nov_num == 17 &
                      ANO_ELEICAO == 2018 &
                      SIGLA_UE %in% c("SC","RO","RN","MT"), 22,nov_num)
)

# Agora as bancadas que se dividiram. Mudo de PSL para PL aleatoriamete na
# proporção da migração

bancada2$nov_num[sample(which(bancada2$nov_num == 17 &
                          bancada2$ANO_ELEICAO == 2018 &
                          bancada2$SIGLA_UE == "SP"),
                  round(length(bancada2$nov_num[bancada2$nov_num == 17 &
                                            bancada2$ANO_ELEICAO == 2018 &
                                            bancada2$SIGLA_UE == "SP"])*0.4))] <- 22

bancada2$nov_num[sample(which(bancada2$nov_num == 17 &
                          bancada2$ANO_ELEICAO == 2018 &
                          bancada2$SIGLA_UE == "MS"),
                  round(length(bancada2$nov_num[bancada2$nov_num == 17 &
                                            bancada2$ANO_ELEICAO == 2018 &
                                            bancada2$SIGLA_UE == "MS"])*0.5))] <- 22

bancada2$nov_num[sample(which(bancada2$nov_num == 17 &
                          bancada2$ANO_ELEICAO == 2018 &
                          bancada2$SIGLA_UE == "RS"),
                  round(length(bancada2$nov_num[bancada2$nov_num == 17 &
                                            bancada2$ANO_ELEICAO == 2018 &
                                            bancada2$SIGLA_UE == "RS"])*(1/3)))] <- 22
bancada2$nov_num[sample(which(bancada2$nov_num == 17 &
                          bancada2$ANO_ELEICAO == 2018 &
                          bancada2$SIGLA_UE == "RJ"),
                  round(length(bancada2$nov_num[bancada2$nov_num == 17 &
                                            bancada2$ANO_ELEICAO == 2018 &
                                            bancada2$SIGLA_UE == "RJ"])*(5/12)))] <- 22
bancada2$nov_num[sample(which(bancada2$nov_num == 17 &
                          bancada2$ANO_ELEICAO == 2018 &
                          bancada2$SIGLA_UE == "MG"),
                  round(length(bancada2$nov_num[bancada2$nov_num == 17 &
                                            bancada2$ANO_ELEICAO == 2018 &
                                            bancada2$SIGLA_UE == "MG"])*(4/6)))] <- 22


# Coloca quem sobrou no DEM

bancada2 <- bancada2 %>% mutate(nov_num = if_else(nov_num == 17, 25,nov_num))

# Transforma União Brasil em DEM

bancada2 <- bancada2 %>% mutate(nov_num = if_else(nov_num == 44, 25,nov_num))

bancada2 <- bancada2 %>%
  group_by(ANO_ELEICAO,SIGLA_UE,nov_num) %>%
  summarise(cad = sum(cad))

# Faz a fusão das bases e agrega.

vpag <- left_join(vpag,bancada2,
                    join_by(ano == ANO_ELEICAO,
                            sigla_uf == SIGLA_UE,
                            nov_num == nov_num))

vpag <- vpag %>% mutate(cad = ifelse(is.na(cad), 0, cad))


# Calcula vieses

cd <- vpag %>% group_by(ano, nov_num) %>% summarise(cad = sum(cad))
cd <- cd %>% pivot_wider(names_from = nov_num, values_from = cad)
cd <- cd[-1]

v <- vpag %>% group_by(ano, nov_num) %>% summarise(v = sum(v))
vv <- vpag %>% group_by(ano, nov_num) %>% summarise(vv = sum(vv))

vs <- v
vs$vs <- vs$v/vv$vv
vs <- vs[,-3]
vs <- vs %>% pivot_wider(names_from = nov_num, values_from = vs)
vs <- vs[-1]


g <- vpag[,c("ano","nov_num","G")]
g <- vpag %>% group_by(ano, nov_num) %>%
  summarise(n = n(), G = mean(G)) %>%
  filter(n > 1L)
g <- g[,-3]
g <- g %>% pivot_wider(names_from = nov_num, values_from = G)
g <- g[-1]

N <- 7
K <- 14

dat <- list(
    N = N,
    K = K,
    gini = as.matrix(g),
    S = as.matrix(cd),
    V = as.matrix(vs)
  )

# ChatGPT. https://chatgpt.com


m1 <- "data {
    int<lower=1> N; // número de eleições
    int<lower=1> K; // número de partidos
    array[N, K] int S; // cadeiras ganhas por partido/UF
    matrix[N, K] V; // matriz da proporção de votos partido/UF
    matrix[N, K] gini; // matriz de valores de gini partido/UF
  }
  
  parameters {
    vector[2] delta;
  }
  
  transformed parameters {
    
    matrix[N, K] phi;
    matrix[N, K] rho;
    array[N] simplex[K] p;
    
    for (i in 1:N) {
      
      for (k in 1:K) {
        rho[i, k] = exp(delta[1] * mean(gini) + delta[2] * (gini[i, k] / sum(gini[i, ])));
        phi[i, k] = exp(rho[i, k] * log(V[i, k]));
      }
      
      for (k in 1:K) {
        p[i, k] = phi[i, k] / sum(phi[i, ]);
      }
    }
  }
  
  model {
    delta ~ normal(0,100);  //priors
    
    for (i in 1:N) {
      S[i,] ~ multinomial(p[i,]);
    }
  }
  "

fit1 <-  stan(model_code = m1, data=dat, iter=1000, chains=4, seed=19730715)

samp <- extract.samples(fit1)

meanPhi <- matrix(NA,7,14)

for(i in 1:7){
  for(j in 1:14) meanPhi[i,j] <- mean(samp$phi[,i,j])
} 

meanRho <- matrix(NA,7,14)

for(i in 1:7){
  for(j in 1:14) meanRho[i,j] <- mean(samp$rho[,i,j])
}

colnames(meanRho) <- names(g)
rownames(meanRho) <- seq(1998,2022,4)

meanRhoBR <- meanRho

###################################################
# Faz por estado
# Calcula votos válidos e share do partido por estado

#vpag_old <- vpag # fiz outra versão sem juntar partidos

vp2 <- vp %>%
  group_by(ano,sigla_uf,id_municipio,nov_num, nov_sigla) %>%
  summarise(v = sum(votos_nominais + votos_legenda))

vp2 <- vp2 %>%
  group_by(ano,sigla_uf,id_municipio) %>%
  mutate(vv = sum(v), vs = v/vv)


# Calcula o gini por estado. Para isso ordenamos a base pela proporção de votos
# part/mun

vp2 <- vp2 %>% arrange(ano,sigla_uf,nov_num,vs)

vp2 <- vp2 %>%
  group_by(ano, sigla_uf,nov_num) %>%
  mutate(G = gini(vs)) %>%
  ungroup()

vpest <- vp2 %>%
  group_by(ano,sigla_uf) %>%
  mutate(vv = sum(v))

vpest <- vpest %>%
  group_by(ano,sigla_uf,nov_num,G,vv) %>%
  summarise(v = sum(v))

vpest <- vpest %>%  mutate(vs = v/vv)

# Junta com base de cadeiras (base de deputados  eleitos TSE)

vpest <- left_join(vpest,bancada2,
                  join_by(ano == ANO_ELEICAO,
                          sigla_uf == SIGLA_UE,
                          nov_num == nov_num))

vpest <- vpest %>% mutate(cad = ifelse(is.na(cad), 0, cad))

vpestr <- vpest[vpest$sigla_uf == "BA",]


cd2 <- vpestr %>% group_by(ano, nov_num) %>% summarise(cad = sum(cad))
cd2 <- cd2 %>% pivot_wider(names_from = nov_num, values_from = cad)
#cdtst <- cd2 %>% pivot_longer(!ano, values_drop_na = T)
#cdtst$ano <- as.numeric(as.factor(cdtst$ano))
#cdtst$name <- as.numeric(as.factor(cdtst$name))
cd2 <- cd2[-1]

vs2 <- vpestr %>% group_by(ano, nov_num) %>% summarise(vs = vs)
vs2 <- vs2 %>% pivot_wider(names_from = nov_num, values_from = vs)
#vstst <- vs2 %>% pivot_longer(!ano,values_drop_na = T)
#vstst$ano <- as.numeric(as.factor(vstst$ano))
#vstst$name <- as.numeric(as.factor(vstst$name))
vs2 <- vs2[-1]



g2 <- vpestr[,c("ano","nov_num","G")]
g2 <- g2 %>% pivot_wider(names_from = nov_num, values_from = G)
#gtst <- g2 %>% pivot_longer(!ano, values_drop_na = T)
#gtst$ano <- as.numeric(as.factor(gtst$ano))
#gtst$name <- as.numeric(as.factor(gtst$name))
g2 <- g2[-1]

#gtst$mGini <- mean(gtst$value)
#rGini <- tapply(gtst$value,gtst$ano,mean)
#gtst$rGini <- rGini[gtst$ano]


dat2 <- list(
  N = dim(cd2)[1],
  K = dim(cd2)[2],
  gini = as.matrix(g2),
  S = as.matrix(cd2),
  V = as.matrix(vs2)
)

inits <- function(chain_id = 1) {list( delta=rnorm(2,0.5,0.25),rho = 3, phi = 2)}
n_chains <- 4
init_ll <- lapply(1:n_chains, function(id) inits(chain_id = id))

fit2 <-  stan(model_code = m1,  data=dat2, init = init_ll, chains=4,iter=1000)

samp <- extract.samples(fit2)

meanRho <- matrix(NA,7,14)

for(i in 1:7){
  for(j in 1:14) meanRho[i,j] <- mean(samp$rho[,i,j])
}

colnames(meanRho) <- names(g2)
rownames(meanRho) <- seq(1998,2022, by = 4)

#meanDeltaSP <- rstan::summary(fit2, pars = "delta")
meanRhoSP <- meanRho

#meanDeltaRJ <- rstan::summary(fit2, pars = "delta")
#meanRhoRJ <- meanRho

#meanDeltaBA <- rstan::summary(fit2, pars = "delta")
#meanRhoBA <- meanRho

save(vpag,bancada,bancada2,vpest,vpestr,meanRhoBA,meanRhoRJ,meanRhoSP, meanRhoBR, fit1, fit2, file = "abcp24.RData")
