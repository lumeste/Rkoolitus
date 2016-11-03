# \n reavahe
# \t tabulaator

piaac <- read.csv("piaac2-1.csv", fileEncoding="utf-8", strip.white = TRUE)
View(piaac)
head(piaac)
tail(piaac)
summary(piaac)
str(piaac)
names(piaac)

mean(piaac$sissetulek, na.rm = TRUE)

piaac[piaac$vanus>50 & piaac$sugu=="Mees", c("vanus", "sissetulek")]

suurpalk <- piaac[piaac$sissetulek>500, ]
View(suurpalk)
uusandmestik <- subset(suurpalk, !is.na(suurpalk$sugu))
View(uusandmestik)

mean(uusandmestik$sissetulek)



############################################


nimi <- "Kati"
perenimi <- "Karu"
pikkus <-"1.65"
andmed <- data.frame(sõrmedearv = c(5,5), varvastearv = c(5,5))
karukati <- list(eesnimi=nimi, perenimi=perenimi, pikkus=pikkus, andmed=andmed)


tulemus <- t.test(piaac$literacy)
str(tulemus)
tulemus[["estimate"]]
tulemus[["conf.int"]][1]

alumine <- tulemus[["conf.int"]][1]
ülemine <- tulemus[["conf.int"]][2]

#####################################

korrutakolmega <- function (x) {
  vastus <- x*3
  return(vastus)
}
vektor <- c(1,2, 3.4)
korrutakolmega(vektor)

#for tsüklite kasutamine ei ole hea, aeglased on

for(i in c("Mees", "Naine")) {
  vahetulemus <- subset(piaac, sugu==i)
  n <- length(vahetulemus$sugu)
  cat(paste(i, "n on", n, "\n"))
}


lahuta <- function (a,b) {
  vastus <- a-b
  return(vastus)
}

leiaalumine <- function () {
  tulemus <- t.test(piaac$literacy)
  alumine <- tulemus[["conf.int"]][1]
  return(alumine)
}

leiapiir <- function (andmed,kumbpiir) {
  tulemus <- t.test(andmed)
  
  if (kumbpiir == "Ülemine") {
    vastus <- tulemus[["conf.int"]][2]
  } else if (kumbpiir == "Alumine") {
    vastus <- tulemus[["conf.int"]][1]
  } else {
    vastus <- print("sisesta Ülemine või Alumine")
  }
}

leiapiir(piaac$literacy, "Alumine")

############################################

library(tidyverse)
library(stringr)

palgad <- read.csv("palgad.csv", fileEncoding = "utf-8")
head(palgad)

top_n(palgad, 5, põhipalk.2015)

palgad %>%
  top_n(5, wt=põhipalk.2015)

uustabel <- palgad %>% 
  select(asutus.2014, ametikoht.2014, põhipalk.2014)

uustabel <- palgad %>% 
  select(asutus.2014:ametikoht.2014)
head(uustabel)

uustabel2 <- palgad %>%
  select (ends_with("2015"))
head(uustabel2)

uustabel2 <- palgad %>%
  filter(põhipalk.2015> 5000) %>%
  select(asutus.2015, ametikoht.2015, põhipalk.2015)


names(palgad)


View(m)

palgad %>%
  filter(ametikoht.2015=="valvur") %>%
  summarize(keskminesissetulek = mean(põhipalk.2015, na.rm = TRUE))

palgad %>%
  filter(koormus.2014 < 1) %>%
  summarize(keskminesissetulek = mean(põhipalk.2014, na.rm = TRUE), mediaan = median(põhipalk.2014, na.rm = TRUE))


names(palgad)

palgad %>%
  filter(is.na(asutus.2014))%>%
  summarize(maksimaalne = max(põhipalk.2015, na.rm = TRUE), 
            keskminesissetulek = mean(põhipalk.2015, na.rm = TRUE)
            )

uustabel <- palgad %>%
  group_by(asutus.2015)%>%
  summarize(keskminesissetulek = mean(põhipalk.2015, na.rm = TRUE), kogupalk = sum(põhipalk.2015, na.rm = TRUE))
  
uustabel <- palgad %>%
  arrange(põhipalk.2015)%>%
  select(põhipalk.2015, asutus.2015, koormus.2015)

head(uustabel)


palgad %>%
  group_by(ametikoht.2015) %>%
  summarize(keskminepalk = mean(põhipalk.2015)) %>%
  top_n(10,keskminepalk) %>%
  arrange(keskminepalk)

uustabel <- piaac %>%
  group_by(sugu, vanus3)%>%
  summarise(kesmine = mean(numeracy, na.rm = TRUE),
            alumine = leiaalumine(numeracy))


piaac %>%
  group_by(isaharidus, sugu) %>%
  summarize (keskminepalk = mean (sissetulek, na.rm = TRUE)) %>%
  arrange(keskminepalk)

muutused <- palgad %>%
  mutate(palgamuutus = põhipalk.2015/põhipalk.2014)%>%
  group_by(asutus.2014)%>%
  summarize (keskmine = mean (palgamuutus, na.rm = TRUE)) %>% 
  arrange(desc(keskmine))
  
vignette("databases")

#################### andmestike ühendamine

loomadepikkus <- data.frame(loom= c("kass", "koer", "elevant"), pikkus = c (10, 20, 50))
loomadelaius <- data.frame(loom = c("kass", "koer", "karu"), laius = c(5, 15, 10))

left_join(loomadepikkus, loomadelaius, by = "loom")
right_join(loomadepikkus, loomadelaius, by = "loom")
full_join(loomadepikkus, loomadelaius, by = "loom")
inner_join(loomadepikkus, loomadelaius, by = "loom")
anti_join(loomadepikkus, loomadelaius, by = "loom")

library(pxR)

rahvastik <- as.data.frame(read.px("RV022.px"))
firmad <- as.data.frame(read.px("ER027.px"))

str(rahvastik)
str(firmad)

levels(rahvastik$Maakond)
levels(firmad$Maakond)
anti_join(rahvastik, firmad, by = c("Maakond", "Aasta"))

levels(rahvastik$Maakond)[7] <- "Järva maakond"
levels(rahvastik$Maakond)[9] <- "Lääne-Viru maakond"

andmestik <- right_join(rahvastik, firmad, by = c("Maakond", "Aasta"))

head(andmestik)
names(andmestik)[5] <- "Elanikke" 
names(andmestik)[7] <- "Firmasid" 

andmestik %>%
  select(Maakond, Aasta, Elanikke, Firmasid)%>%
  mutate(ev_suhtarv = Firmasid / 1000)%>%
  qplot(aes(x = "Aasta", y="ev_suhtarv")) + geom_point() + facet_wrap(~Maakond, nrow = 6)







