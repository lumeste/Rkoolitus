a = 4
a


b<-6
b


a <- 3
a
b <- 6
b
rm(a)


kanadearv <- 4
koerterarv <- 5
loomadearv <- kanadearv + koerterarv

rm(koerterarv)


c (3, 5, 4, 3, 2, 3.4)





pikkJada <- c(3:65)
pikkJada

pikkus <- c(5, 6, 8, 10)
pikkus


pikkus + 5

pikkus + c(1, 2, 3)
pikkus + c(1, 2)


loom <- c("kass", "koer", "karu", "elevant")

loom + "viis"

loom + c("viis")


andmed <- data.frame(pikkus, loom)

andmed

rm(loom, pikkus)
loom
pikkus
rm (all)

andmed$pikkus

andmed$laius <- c(1, 4, 5, 3)
andmed

andmed$pikkusruut <- andmed$pikkus**2
andmed

andmed[c("pikkus", "laius")]


andmed$maht <- andmed$laius*andmed$pikkus
andmed


loomamaht <- andmed[c("loom", "maht")]
loomamaht

andmed$pikkus[0]
andmed$pikkus[c(2, 3)]
andmed[3, 4]
andemd
andmed

karuelevant <- andmed[c(3, 4), c(1, 2, 3)]
karuelevant


mean(andmed$pikkus)


puuduv <- c(2, 3, 5, NA)
mean(puuduv)


mean(puuduv, na.rm = TRUE)

?read.csv


skp <- read.csv("skphinnang.csv")


library(ggplot2)

install.packages("dplyr")
library(dplyr)

summary(skp)
str(skp)
View(skp)



skp$erinevus <- skp$SKP.kasv.praegu - skp$Kiirhinnang
mean(skp$erinevus)

skp

skp$abserinevus <- abs(skp$erinevus)
mean(skp$abserinevus)


skp$correlation <- cor(skp$Kiirhinnang, skp$SKP.kasv.praegu)

skp$correlation
skp

skp


piaac <- read.csv("piaac2_tapitahtedeta.csv")
summary(piaac)
str(piaac)
View(piaac)
rm(piaac_uus)

quantile(piaac$sissetulek, probs= c(0.1, 0.5, 0.9), na.rm = TRUE)
?quantile


write.csv(skp, file = "skp_uus.csv", row.names = FALSE)
?write.csv

save(skp, file="skpuus.Rdat")

load(file="skpuus.Rdat")
skp
dput(skp, file="skp.txt")






tabel1 <- table(piaac$sugu, piaac$haridustase)
tabel1
addmargins(tabel1, c(1,2))
prop.table(tabel1, 2)

ftable(xtabs(~isaharidus + emaharidus + haridustase, data=piaac))
prop.table(xtabs(~isaharidus + emaharidus + haridustase, data=piaac))

library("stargazer")

stargazer(piaac, type="html", out="tabel1.html")

skp$kuup <- as.Date(skp$kuup)

summary(skp)


faktorid <- read.csv("faktoritest.csv")
summary(faktorid)

str(faktorid)


x <- 3

x == 3
z <- 5

x > 2 | z == 3

x > 2 & z == 3

loomad <- c("kana", "kalkun", "elevant")

"konn" %in% loomad

keskharitud <- subset(piaac, haridustase=="Keskmine")
summary(keskharitud)

keskmineK <- mean(piaac$sissetulek[piaac$kaaslane=="Jah"], na.rm= TRUE)
keskmineK

mean(piaac$sissetulek[piaac$vanus>34 & piaac$haridustase=="Keskmine"], na.rm = TRUE)












library(ggplot2)
qplot(data=skp, x=kuup, y=SKP.kasv.praegu, geom=c("line", "point"))

piaac <- subset(piaac, !is.na(haridustase))

qplot(data=piaac,x=haridustase, y=literacy, geom="boxplot", color=sugu)

qplot(data=piaac, x=literacy, y=sissetulek, geom=c("smooth"), color=sugu, facets=.~haridustase)

piaac$haridustase <- factor(piaac$haridustase, levels=c("Madal", "Keskmine", "NA"))

mosaicplot( ~tervis + sugu, data=piaac)







