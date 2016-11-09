#######
# kolmas
######

piaac <- read.csv("http://ut.ee/~iseppo/piaac3.csv", na.strings = c(NA, ""))

summary(piaac)
str(piaac)

library(plyr)
library(dplyr)

piaac %>%
  filter(!is.na(lapsed)) %>%
  group_by(haridustase) %>%
  summarize(keskmine = mean(vanus, na.rm = TRUE), inimestearv = n())

library(lattice)
library(Rmisc)

p.keskmine <- piaac %>%
  group_by (hvaldkond)%>%
  filter(!is.na(numeracy), !is.na(hvaldkond), !is.na(sissetulek)) %>%
  summarize(keskmine = mean(numeracy), 
            alumine = CI(numeracy)["lower"], 
            ülemine = CI(numeracy)["upper"], 
            protsentiil75 = quantile(numeracy, probs = 0.75, na.rm = TRUE), 
            protsentiil25 = quantile(numeracy, probs = 0.25, na.rm = TRUE)
            )

names(p.keskmine)
p.keskmine
?quantile

## andmed tidyr kujule - igas reas ainult 1! key-value pair

loomad <- read.csv("http://www.ut.ee/~iseppo/loomad.csv")
View(loomad)

library(tidyverse)

gather(loomad, key = mootmine, value = vaartus, pikkus, laius, kõrgus)

skp <- read.csv("http://www.ut.ee/~iseppo/skpuus.csv")
skp$kuup <- as.Date(skp$kuup)
?as.Date

skp.pikk <- gather(skp, key = näitaja, value = väärtus, Kiirhinnang, SKP.kasv.praegu)
?gather
skp.tidy <- gather(skp, key = näitja, value = väärtus, -kuup)
skp.wide <- spread(skp.tidy, key = kuup, value = väärtus)


## the grammar of graphics

p <- ggplot(data=piaac, aes(x=numeracy, y=sissetulek)) +
  geom_point()+
  geom_smooth()

p
ggsave(p, filename = "Ppilt.png", height = 5, width = 6, scale = 1.5)

graafik <- ggplot(skp, aes(x=kuup, y=SKP.kasv.praegu))+
  geom_line()+
  geom_line(aes(y=Kiirhinnang), color = "blue")

?ggplot
graafik

graafik2 <- ggplot(piaac, aes(x=sugu, y=sissetulek))+
  geom_boxplot(aes(color=haridustase), outlier.shape = NA)+
  

  
graafik4 <- ggplot(skp, aes(x=kuup, y=SKP.kasv.praegu))+
  geom_line(data=skp.pikk, aes(y=väärtus, color=näitaja))+
  xlab("Kuupäev")+
  ylab("SKP kasv praegu")
  
library(ggiraph) 
library(plotly)

ggplotly(graafik4)
ggiraph(graafik3)

g5 <- ggplot(data = piaac, aes(x=tervis))+
  geom_bar(aes(fill=sugu), position = "dodge")

ggplotly(g5)

palksynniaasta <- read.csv2("http://www.ut.ee/~iseppo/palkSynniaasta.csv")
palksynniaasta$vanus <- palksynniaasta$AASTA - palksynniaasta$SYNNIAASTA

graafikule <- palksynniaasta %>%
  filter(vanus %in% c(25,35,45,55,65))

ggplot(graafikule, aes(x=factor(vanus)))+
  geom_boxplot(aes(y=keskminepalk, lower=pc25,
                   middle=mediaanpalk,
                   upper=pc75,
                   ymin=pc10, ymax=pc90), stat = "identity")


ggplot(palksynniaasta, aes(x=vanus))+
  geom_ribbon(fill="grey", alpha="0.3", aes(ymin=pc25, ymax=pc75))+
  geom_line(aes(y=keskminepalk), color= "blue")+
  geom_line(aes(y=mediaanpalk), color= "orange")+
  theme_minimal()

names(palksynniaasta)


## faktorid

tekstivektor <- c("üks", "kaks", "kolm")
str(tekstivektor)

data <- data.frame(arvtunnus = c(1,2,3), tekstivektor)
str(data)

piaac$tervis = as.factor(piaac$tervis)
levels(piaac$tervis)

library(forcats)

piaac <- piaac%>%
  mutate(tervis3 = fct_relevel(tervis, "Halb", "Rahuldav", "Hea", "Väga hea", "Suurepärane"))

levels(piaac$tervis3)
ggplot(data = piaac, aes(x=tervis3))+
  geom_bar()

piaac <- piaac %>%
  mutate(tervis4 = fct_recode(tervis3, 
                              kesine = "Halb", 
                              kesine = "Rahuldav", 
                              parem = "Väga hea",
                              parem = "Suurepärane"))

levels(piaac$tervis4)
table(piaac$tervis4, piaac$tervis3)

piaac_joonis <- piaac%>%
  mutate(haridustase2 = fct_relevel(haridustase, "Madal", "Keskmine", "Kõrge"))%>%
  mutate(haridustase3 = fct_recode(haridustase2, 
                                   põhiharidus = "Madal",
                                   keskharidus = "Keskmine",
                                   kõrgharidus = "Kõrge"
                                   ))%>%
  ggplot(aes(x=haridustase3, y=numeracy))+
    geom_boxplot()+
    theme_minimal()


piaac <- piaac %>%
  mutate(hvaldkond = fct_reorder(hvaldkond, numeracy, fun = median, na.rm = TRUE))

         
ggplot(filter(piaac, !is.na(hvaldkond)), aes(x=hvaldkond, y=numeracy))+
  geom_boxplot()


head(p.keskmine)

p.keskmine <- p.keskmine %>%
  mutate(hvaldkond2 = fct_reorder(hvaldkond, keskmine, fun = median, na.rm = TRUE))

ggplot(p.keskmine, aes(x=keskmine, y=hvaldkond2))+
  geom_point()


ggplot(p.keskmine, aes(x=keskmine, y=hvaldkond2))+
  geom_errorbarh(aes(x=keskmine, xmin = alumine, xmax = ülemine, y=hvaldkond2))+
  geom_point()

ggplot(piaac, aes(x=sugu, y=numeracy))+
  geom_boxplot()+
  facet_grid(haridustase~vanus3)

ggplot(filter(piaac, !is.na(haridustase)), aes(x=sugu, y=numeracy))+
  geom_boxplot()+
  facet_wrap(~vanus3 + haridustase, nrow = 2)











