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
