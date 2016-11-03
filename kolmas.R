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


