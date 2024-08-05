#
# Faz gráfico votos cadeiras
#


cds <- cd/513
v]cds$ano <- seq(1998,2022, by = 4)
vs$ano <- seq(1998,2022, by = 4)
vsl <- vs %>% pivot_longer(!ano, names_to = "numpart", values_to = "vs")
cdsl <- cds %>% pivot_longer(!ano, names_to = "numpart", values_to = "cs")

sv <- inner_join(vsl,cdsl, join_by(ano,numpart))

plot(cs ~vs,
     data = sv,
     pch = 16,
     cex = 0.2,
     col = alpha("black",0.3),
     cex.lab = 0.9,
     cex.axis = 0.7)

text(sv$vs, sv$cs, sv$codpart, cex = 0.4)

abline(0,1, lty = 2, col = "black")

plot(cs ~vs, data = sv[sv$ano == 2010,],
     pch = 16,
     cex = 0.2,
     col = alpha("black",0.3),
     cex.lab = 0.9,
     cex.axis = 0.7)
text(sv$vs[sv$ano == 2010],
     sv$cs[sv$ano == 2010],
     sv$codpart[sv$ano == 2010],
     cex = 0.4)
abline(0,1, lty = 2, col = "black")

