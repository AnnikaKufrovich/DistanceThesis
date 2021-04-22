library(dplyr)
library(RcmdrMisc)
library(pryr)
library(VGAM)
library(randomForest)
library(rpart)

full.havdist.2018 <- read.csv("2018VoterHavDist.csv")

full.havdist2018.narm <- na.omit(full.havdist.2018) %>% 
  mutate(race.eth = ifelse(white == 1 , "White", 
                       ifelse(black == 1, "Black", 
                              ifelse(hispanic == 1, "Hispanic", "Other")))) %>%
  select(-lon, -lat, -V1, - V25, -white, -black, -hispanic) %>% 
  mutate(vt2018 = ifelse(V5.x == "Y", "Elday", 
                         ifelse(V5.x == "E", "Early", 
                                ifelse(V5.x == "A", "Mail", "N")))) %>%
  mutate(vt2016 = ifelse(V5.y == "Y", "Elday", 
                         ifelse(V5.y == "E", "Early", 
                                ifelse(V5.y == "A", "Mail", "N"))))

rm(full.havdist.2018) #removing to free up environment space

full.havdist2018.narm$race.eth <- relevel(
  as.factor(full.havdist2018.narm$race.eth), ref = "White")  


#alternative datasets
full.havdist2018.narmalt <- full.havdist2018.narm %>% 
  mutate(vta2018 = ifelse(vt2018 == "Early", "Alt", 
                          ifelse(vt2018 == "Mail", "Alt", vt2018))) %>%
  mutate(vta2016 = ifelse(vt2016 == "Early", "Alt",
                          ifelse(vt2016 == "Mail", "Alt", vt2016))) %>%
  select(-vt2018, -vt2016)

ratehd.40andless.2018 <- full.havdist2018.narm %>%
  filter(as.numeric(as.character(geopollrating)) <= 40)

ratehd.20andless.2018 <- full.havdist2018.narm %>%
  filter(as.numeric(as.character(geopollrating)) <= 20)


#removing ridiculous distances
fhd2018.reasonable <- full.havdist2018.narm %>%
  filter(haverdistance <= 8045) %>% 
  mutate(hdistmiles = haverdistance/1609.34) %>% 
  mutate(vta2018 = ifelse(vt2018 == "Early", "Alt", 
                          ifelse(vt2018 == "Mail", "Alt", vt2018))) %>%
  mutate(vta2016 = ifelse(vt2016 == "Early", "Alt", 
                          ifelse(vt2016 == "Mail", "Alt", vt2016)))

fhd2018r.eldayn <- fhd2018.reasonable %>%
  filter(vta2018 != "Alt") %>% 
  mutate(vta2018en = ifelse(vta2018 == "Elday", 1, 0))

fhd2018r.altn <- fhd2018.reasonable %>% 
  filter(vta2018 != "Elday")  %>% 
  mutate(vta2018an = ifelse(vta2018 == "Alt", 1, 0))


fhd2018r.altn$vta2016 <- relevel(
  as.factor(fhd2018r.altn$vta2016), ref = "N") 

fhd2018r.eldayn$vta2016 <- relevel(
  as.factor(fhd2018r.eldayn$vta2016), ref = "N")

#elday, mail, early
fhd2018r.eldayn <- fhd2018.reasonable %>%
  filter(vta2018 != "Alt") %>% 
  mutate(vta2018en = ifelse(vta2018 == "Elday", 1, 0)) 


fhd2018r.mailn <- fhd2018.reasonable %>% 
  filter(vt2018 != "Elday")  %>% 
  filter(vt2018 != "Early") %>%
  mutate(vta2018mn = ifelse(vt2018 == "Mail", 1, 0))


fhd2018r.earlyn <- fhd2018.reasonable %>% 
  filter(vt2018 != "Elday")  %>% 
  filter(vt2018 != "Mail") %>%
  mutate(vta2018e2n = ifelse(vt2018 == "Early", 1, 0))


fhd2018r.eldayn$vt2016 <- relevel(
  as.factor(fhd2018r.eldayn$vt2016), ref = "N")

fhd2018r.mailn$vt2016 <- relevel(
  as.factor(fhd2018r.mailn$vt2016), ref = "N") 

fhd2018r.earlyn$vt2016 <- relevel(
  as.factor(fhd2018r.earlyn$vt2016), ref = "N") 

##clean environment as much as possible before running each
##they take up a lot of memory
##remember than distance is in meters

##estimate is income estimate
binomlogit.hdfull <- glm(data = fhd2018.reasonable, 
                          voted2018 ~ female + age + 
                            race.eth + log(estimate) + hdistmiles + 
                            voted2016b + race.eth*log(estimate), 
                         family = binomial(link = "logit"))

cv.bl <- cv.glm(fhd2018.reasonable, binomlogit.hdfull, K = 10)

cv.bl$delta[1]

#hv <- hatvalues(binomlogit.hdfull)

#plot(hatvalues(binomlogit.hdfull), type = "h")

##AIC

stepwise(binomlogit.hdfull, direction = "forward", criterion = "AIC")

stepwise(binomlogit.hdfull, direction = "backward", criterion = "AIC")

stepwise(binomelogit.hdfull, direction = "backward/forward", 
         criterion = "AIC")

stepwise(binomlogit.hdfull, direction = "forward/backward", 
         criterion = "AIC")

##BIC

stepwise(binomlogit.hdfull, direction = "forward", criterion = "BIC")

stepwise(binomlogit.hdfull, direction = "backward", criterion = "BIC")

stepwise(binomlogit.hdfull, direction = "backward/forward", 
         criterion = "BIC")

stepwise(binomlogit.hdfull, direction = "forward/backward", 
         criterion = "BIC")


##remove logit from environment for space

rm(binomlogit.hdfull)

fhdr2018.stand <- fhd2018.reasonable %>%
  mutate(s.inc = log(estimate)/sd(log(estimate)), 
         s.hdist = haverdistance/sd(haverdistance), 
         s.age = age/sd(age))

binomlogit.stand <- glm(data = fhdr2018.stand, 
                         voted2018 ~ female + s.age + 
                           race.eth + s.inc + s.hdist + 
                           voted2016b + race.eth*s.inc
                         , family = binomial(link = "logit"))



##reduced
##simplelogit.hdfull <- glm(data = full.havdist2018.narm, 
##                          voted2018 ~ age + 
##                            white + black + hispanic + 
##                            estimate + haverdistance + 
##                            voted2016b, family = binomial(link = "logit"))


##Multiple logistic

##piecemeal version

multlogit1.elday <- glm(data = fhd2018r.eldayn, 
                         vta2018en ~ female + age + 
                           race.eth + log(estimate) + hdistmiles + 
                           vta2016 + race.eth*log(estimate), 
                         family = binomial(link = "logit"))


multlogit2.alt <- glm(data = fhd2018r.altn, 
                        vta2018an ~ female + age + 
                          race.eth + log(estimate) + hdistmiles + 
                          vta2016 + race.eth*log(estimate), 
                        family = binomial(link = "logit"))

##Now with elday, early, mail

multlogit1.elday <- glm(data = fhd2018r.eldayn, 
                        vta2018en ~ female + age + 
                          race.eth + log(estimate) + hdistmiles + 
                          vt2016 + race.eth*log(estimate), 
                        family = binomial(link = "logit"))


multlogit2.early <- glm(data = fhd2018r.earlyn, 
                      vta2018e2n ~ female + age + 
                        race.eth + log(estimate) + hdistmiles + 
                        vt2016 + race.eth*log(estimate), 
                      family = binomial(link = "logit"))

multlogit3.mail <- glm(data = fhd2018r.mailn, 
                        vta2018mn ~ female + age + 
                          race.eth + log(estimate) + hdistmiles + 
                          vt2016 + race.eth*log(estimate), 
                        family = binomial(link = "logit"))


set.seed(58496)
fhd.samp <- full.havdist2018.narm[sample(nrow(full.havdist2018.narm), 
                                         size = 500000), ]


##Works Now
multilogit.hdfull <- vglm(data = full.havdist2018.narm, 
                          formula = vt2018 ~ female + age + 
                            race.eth + estimate + haverdistance + 
                            vt2016 + race.eth*estimate, 
                         family = multinomial(refLevel = "N"))

#grouping mail and early as alternate vote to reduce size of vglm

full.havdist2018.narmalt$race.eth <- relevel(
  as.factor(full.havdist2018.narmalt$race.eth), ref = "White")  

full.havdist2018.narmalt$vta2016 <- relevel(
  as.factor(full.havdist2018.narmalt$vta2016), ref = "N")  

multilogit.hdfull2 <- vglm(data = full.havdist2018.narmalt, 
                          formula = vta2018 ~ female + age + 
                            race.eth + estimate + haverdistance + 
                            vta2016 + race.eth*estimate, 
                          family = multinomial(refLevel = "N"))



##Random Forest

set.seed(946853)

samp.rf <- fhd2018.reasonable[sample(1:nrow(fhd2018.reasonable), 2500000),] %>%
  mutate(l.inc = log(estimate))

rf.binom <- randomForest(data = samp.rf, 
             as.factor(voted2018) ~ female + age + 
               race.eth + l.inc + haverdistance + 
               voted2016b + race.eth*l.inc, 
             ntree = 50, importance = TRUE)


##+ race.eth*log(estimate) removed because help file says 
##rf.binom2 <- rpart(data = fhd2018.reasonable, 
##             formula = as.factor(voted2018) ~ female + age + 
##               race.eth + log(estimate) + haverdistance + 
##               voted2016b)


#same model but most accurate geocoded polls

binomlogit.hdfull <- glm(ratehd.20andless.2018, 
                           voted2018 ~ female + age + 
                             race.eth + estimate + haverdistance + 
                             voted2016b + race.eth*estimate, 
                           family = binomial(link = "logit"))
