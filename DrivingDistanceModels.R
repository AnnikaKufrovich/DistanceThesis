library(dplyr)
library(RcmdrMisc)
library(pryr)
library(VGAM)
library(boot)


##all of this code is very similar to the haversine distance models with small adjustments
##if you understand works in that file, this should be easier as there is less overall

#loading both distance data
full.drivedist.2018 <- read.csv("2018VoterBothDist.csv")

#removing excess NA values and recoding both race/ethnicity and vote type
full.drivedist2018.narm <- na.omit(full.drivedist.2018) %>% 
  mutate(race.eth = ifelse(white == 1 , "White", 
                           ifelse(black == 1, "Black", 
                                  ifelse(hispanic == 1, "Hispanic", "Other")))) %>%
  select(-V1, - V25, -white, -black, -hispanic) %>% 
  mutate(vt2018 = ifelse(V5.x == "Y", "Elday", 
                         ifelse(V5.x == "E", "Early", 
                                ifelse(V5.x == "A", "Mail", "N")))) %>%
  mutate(vt2016 = ifelse(V5.y == "Y", "Elday", 
                         ifelse(V5.y == "E", "Early", 
                                ifelse(V5.y == "A", "Mail", "N"))))

rm(full.drivedist.2018) #removing to free up environment space

#releveling race/ethnicity so that white is the baseline
full.drivedist2018.narm$race.eth <- relevel(
  as.factor(full.drivedist2018.narm$race.eth), ref = "White")  


#Making driving distances in miles and limiting distances, based on haversine
#this ensures similar data is used

fdd2018.reasonable <- full.drivedist2018.narm %>%
  mutate(ddistmiles = drivedistance/1609.34) %>% 
  mutate(vta2018 = ifelse(vt2018 == "Early", "Alt", 
                          ifelse(vt2018 == "Mail", "Alt", vt2018))) %>%
  mutate(vta2016 = ifelse(vt2016 == "Early", "Alt", 
                          ifelse(vt2016 == "Mail", "Alt", vt2016))) %>%
  mutate(voted2016b = ifelse(V5.y == "Y", 1, 
                             ifelse(V5.y == "A", 1, 
                                    ifelse(V5.y == "E", 1, 0)))) %>%
  filter(haverdistance <= 8045) %>% 
  filter(ddistmiles >= 0) %>% 
  select(-dem, -rep, -npa)

rm(full.drivedist2018.narm)


#version of above without weird precincts
fdd2018r.noweirdprec <- fdd2018.reasonable %>% 
  filter(precID != "LEE46") %>%
  filter(precID != "PAL1244") %>%
  filter(precID != "PAL1242") %>%
  filter(precID != "STL16") %>%
  filter(precID != "PAS108") %>%
  filter(precID != "LE03409") %>%
  filter(precID != "BROW020")



##clean environment as much as possible before running each
##they take up a lot of memory
##remember than distance is in meters

##estimate is income estimate

#main binomial logitsic model
binomlogit.ddfull <- glm(data = fdd2018.reasonable, 
                         voted2018 ~ female + age + 
                           race.eth + log(estimate) + ddistmiles + 
                           voted2016b + race.eth*log(estimate), 
                         family = binomial(link = "logit"))

#above but without the weird precincts
binomlogit.ddfull <- glm(data = fdd2018r.noweirdprec, 
                         voted2018 ~ female + age + 
                           race.eth + log(estimate) + ddistmiles + 
                           race.eth*log(estimate), 
                         family = binomial(link = "logit"))

#defining cost function for misclassification error rate
cost.func <- function(r, pi = 0) {
  mean((pi < 0.5) & r == 1 | (pi >= 0.5) & r == 0)
}

#cross-validated error rate
set.seed(795861)
cv.bl <- cv.glm(fdd2018.reasonable, binomlogit.ddfull, cost = cost.func, K = 10)

cv.bl$delta[1]


#model with squared distance
binomlogit.ddfull.square <- glm(data = fdd2018.reasonable, 
                         voted2018 ~ female + age + 
                           race.eth + log(estimate) + 
                           poly(ddistmiles, degree = 2) + 
                           voted2016b + race.eth*log(estimate), 
                         family = binomial(link = "logit"))

#model with race distance interaction
binomlogit.ddfullinter2 <- glm(data = fdd2018.reasonable, 
                         voted2018 ~ female + age + 
                           race.eth + log(estimate) + ddistmiles + 
                           voted2016b + race.eth*log(estimate), 
                         family = binomial(link = "logit"))

cv.bl <- cv.glm(fdd2018.reasonable, binomlogit.ddfull, K = 10)

cv.bl$delta[1]




##multinomial logistic regression

##setting up pairwise things because of what happened with haversine models

fdd2018r.eldayn <- fdd2018.reasonable %>%
  filter(vta2018 != "Alt") %>% 
  mutate(vta2018en = ifelse(vta2018 == "Elday", 1, 0))

fdd2018r.altn <- fdd2018.reasonable %>% 
  filter(vta2018 != "Elday")  %>% 
  mutate(vta2018an = ifelse(vta2018 == "Alt", 1, 0))


fdd2018r.altn$vta2016 <- relevel(
  as.factor(fdd2018r.altn$vta2016), ref = "N") 

fdd2018r.eldayn$vta2016 <- relevel(
  as.factor(fdd2018r.eldayn$vta2016), ref = "N")


##Regressions for elday and alternative

multlogit1.elday <- glm(data = fdd2018r.eldayn, 
                        vta2018en ~ female + age + 
                          race.eth + log(estimate) + ddistmiles + 
                          vta2016 + race.eth*log(estimate), 
                        family = binomial(link = "logit"))


multlogit2.alt <- glm(data = fdd2018r.altn, 
                      vta2018an ~ female + age + 
                        race.eth + log(estimate) + ddistmiles + 
                        vta2016 + race.eth*log(estimate), 
                      family = binomial(link = "logit"))


##Now with elday, early, mail

#elday, mail, early
fdd2018r.eldayn <- fdd2018.reasonable %>%
  filter(vta2018 != "Alt") %>% 
  mutate(vta2018en = ifelse(vta2018 == "Elday", 1, 0)) 


fdd2018r.mailn <- fdd2018.reasonable %>% 
  filter(vt2018 != "Elday")  %>% 
  filter(vt2018 != "Early") %>%
  mutate(vta2018mn = ifelse(vt2018 == "Mail", 1, 0))


fdd2018r.earlyn <- fdd2018.reasonable %>% 
  filter(vt2018 != "Elday")  %>% 
  filter(vt2018 != "Mail") %>%
  mutate(vta2018e2n = ifelse(vt2018 == "Early", 1, 0))


fdd2018r.eldayn$vt2016 <- relevel(
  as.factor(fdd2018r.eldayn$vt2016), ref = "N")

fdd2018r.mailn$vt2016 <- relevel(
  as.factor(fdd2018r.mailn$vt2016), ref = "N") 

fdd2018r.earlyn$vt2016 <- relevel(
  as.factor(fdd2018r.earlyn$vt2016), ref = "N") 

#regressions
multlogit1.elday <- glm(data = fdd2018r.eldayn, 
                        vta2018en ~ female + age + 
                          race.eth + log(estimate) + ddistmiles + 
                          vt2016 + race.eth*log(estimate), 
                        family = binomial(link = "logit"))


multlogit2.early <- glm(data = fdd2018r.earlyn, 
                        vta2018e2n ~ female + age + 
                          race.eth + log(estimate) + ddistmiles + 
                          vt2016 + race.eth*log(estimate), 
                        family = binomial(link = "logit"))

multlogit3.mail <- glm(data = fdd2018r.mailn, 
                       vta2018mn ~ female + age + 
                         race.eth + log(estimate) + ddistmiles + 
                         vt2016 + race.eth*log(estimate), 
                       family = binomial(link = "logit"))

