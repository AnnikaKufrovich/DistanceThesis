library(dplyr)
library(RcmdrMisc)
library(pryr)
library(VGAM)
library(randomForest)
library(rpart)
library(boot)
library(effects)

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

#removing ridiculous distances
fhd2018.reasonable <- full.havdist2018.narm %>%
  filter(haverdistance <= 8045) %>% 
  mutate(hdistmiles = haverdistance/1609.34) %>% 
  mutate(vta2018 = ifelse(vt2018 == "Early", "Alt", 
                          ifelse(vt2018 == "Mail", "Alt", vt2018))) %>%
  mutate(vta2016 = ifelse(vt2016 == "Early", "Alt", 
                          ifelse(vt2016 == "Mail", "Alt", vt2016)))


##clean environment as much as possible before running each
##they take up a lot of memory
##remember than distance is in meters

##estimate is income estimate
binomlogit.hdfull <- glm(data = fhd2018.reasonable, 
                          voted2018 ~ female + age + 
                            race.eth + log(estimate) + hdistmiles + 
                            voted2016b + race.eth*log(estimate), 
                         family = binomial(link = "logit"))

binomlogit.hdfull2 <- glm(data = fhd2018.reasonable, 
                         voted2018 ~ female + age + 
                           race.eth + log(estimate) + hdistmiles + 
                           + race.eth*log(estimate), 
                         family = binomial(link = "logit"))


cost.func <- function(r, pi = 0) {
  mean((pi < 0.5) & r == 1 | (pi >= 0.5) & r == 0)
}

set.seed(795861)
cv.bl <- cv.glm(fhd2018.reasonable, binomlogit.hdfull, cost = cost.func, K = 10)

cv.bl$delta[1]


### no distance
binomlogit.hdfull.nodist <- glm(data = fhd2018.reasonable, 
                         voted2018 ~ female + age + 
                           race.eth + log(estimate) + 
                           voted2016b + race.eth*log(estimate), 
                         family = binomial(link = "logit"))

set.seed(795861)
cv.bl2 <- cv.glm(fhd2018.reasonable, binomlogit.hdfull.nodist, 
                 cost = cost.func, K = 10)

cv.bl2$delta[1]

###


### (income and distance interaction, not even statistically significant)
binomlogit.hdfullinter1 <- glm(data = fhd2018.reasonable, 
                         voted2018 ~ female + age + 
                           race.eth + log(estimate) + hdistmiles + 
                           voted2016b + race.eth*log(estimate) + 
                           hdistmiles*log(estimate), 
                         family = binomial(link = "logit"))
###


### (distance and race interaction)
binomlogit.hdfullinter2 <- glm(data = fhd2018.reasonable, 
                               voted2018 ~ female + age + 
                                 race.eth + log(estimate) + hdistmiles + 
                                 voted2016b + race.eth*log(estimate) + 
                                 race.eth*hdistmiles, 
                               family = binomial(link = "logit"))

stepwise(binomlogit.hdfullinter2, direction = "forward", criterion = "AIC")

stepwise(binomlogit.hdfullinter2, direction = "backward", criterion = "AIC")

stepwise(binomlogit.hdfullinter2, direction = "forward", criterion = "BIC")

stepwise(binomlogit.hdfullinter2, direction = "backward", criterion = "BIC")

eff.test <- effect("race.eth*hdistmiles", binomlogit.hdfullinter2)
plot(eff.test, style = "stacked", colors = c("blue"), rug = FALSE)

library(ggeffects)
library(ggplot2)

effect.df <- ggpredict(binomlogit.hdfullinter2, terms = c("hdistmiles", "race.eth"))
ggplot(effect.df, aes(x, predicted, color = group)) + geom_line() + 
  scale_color_brewer(palette = "Dark2") + 
  labs(x = "Distance in Miles (Haversine)", 
       y = "Predicted Probabilities", color = "Race/Ethnicity", 
       title = "Marginal effects of Distance and Race/Ethnicity") + 
  theme_classic()


set.seed(795861)
cv.bl3 <- cv.glm(fhd2018.reasonable, binomlogit.hdfullinter2, 
                 cost = cost.func, K = 10)

cv.bl3$delta[1]
###



binomlogit.hdfull.square <- glm(data = fhd2018.reasonable, 
                         voted2018 ~ female + age + 
                           race.eth + log(estimate) + 
                           poly(hdistmiles, degree = 2) + 
                           voted2016b + race.eth*log(estimate), 
                         family = binomial(link = "logit"))


stepwise(binomlogit.hdfull.square, direction = "forward", criterion = "AIC")

stepwise(binomlogit.hdfull.square, direction = "backward", criterion = "AIC")

stepwise(binomlogit.hdfull.square, direction = "forward", criterion = "BIC")

stepwise(binomlogit.hdfull.square, direction = "backward", criterion = "BIC")

set.seed(795861)
cv.bl4 <- cv.glm(fhd2018.reasonable, binomlogit.hdfull.square, 
                 cost = cost.func, K = 10)

cv.bl4$delta[1]

#hv <- hatvalues(binomlogit.hdfull)

#plot(hatvalues(binomlogit.hdfull), type = "h")

##AIC

stepwise(binomlogit.hdfull, direction = "forward", criterion = "AIC")

stepwise(binomlogit.hdfull, direction = "backward", criterion = "AIC")

stepwise(binomlogit.hdfull, direction = "backward/forward", 
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

binomlogit.hdfull <- glm(data = fhd2018.reasonable, 
                         voted2018 ~ female + age + 
                           race.eth + log(estimate) + hdistmiles + 
                           voted2016b + race.eth*log(estimate), 
                         family = binomial(link = "logit"))

y <- fhd2018.reasonable$voted2018
x.fullyscaled <- scale(model.matrix(binomlogit.hdfull)[,-1])

scaled.hdist2018 <- data.frame(voted2018 = y, x.fullyscaled)

binomlogit.scale <- glm(data = scaled.hdist2018, 
                        voted2018 ~ female + age + 
                          race.ethBlack + race.ethHispanic + race.ethOther
                          + log.estimate. + hdistmiles + 
                          voted2016b + race.ethBlack.log.estimate. + 
                          race.ethHispanic.log.estimate. + 
                          race.ethOther.log.estimate., 
                        family = binomial(link = "logit"))


#polynomial scales

y <- fhd2018.reasonable$voted2018
x.fullyscaled <- scale(model.matrix(binomlogit.hdfull.square)[,-1])

scaled.hdist2018 <- data.frame(voted2018 = y, x.fullyscaled)

binomlogit.scale.square <- glm(data = scaled.hdist2018, 
                        voted2018 ~ female + age + 
                          race.ethBlack + race.ethHispanic + race.ethOther
                        + log.estimate. + poly.hdistmiles..degree...2.1 +
                          poly.hdistmiles..degree...2.2 +
                          voted2016b + race.ethBlack.log.estimate. + 
                          race.ethHispanic.log.estimate. + 
                          race.ethOther.log.estimate., 
                        family = binomial(link = "logit"))



#race/dist interaction scales

y <- fhd2018.reasonable$voted2018
x.fullyscaled <- scale(model.matrix(binomlogit.hdfullinter2)[,-1])

scaled.hdist2018 <- data.frame(voted2018 = y, x.fullyscaled)

binomlogit.scale.inter <- glm(data = scaled.hdist2018, 
                               voted2018 ~ female + age + 
                                 race.ethBlack + race.ethHispanic + race.ethOther
                               + log.estimate. + hdistmiles +
                                 voted2016b + race.ethBlack.log.estimate. + 
                                 race.ethHispanic.log.estimate. + 
                                 race.ethOther.log.estimate. + 
                                 race.ethBlack.hdistmiles + 
                                race.ethHispanic.hdistmiles + 
                                race.ethOther.hdistmiles, 
                               family = binomial(link = "logit"))




##Random Forest

set.seed(795861)
samp.rf <- fhd2018.reasonable[sample(1:nrow(fhd2018.reasonable), 2500000),] %>%
  mutate(LLMI = log(estimate), Gender.Fem = female, Age = age, 
         Race.Ethnicity = race.eth, Haversine.Distance = haverdistance, 
         Voted.2016 = voted2016b, Voted.2018 = voted2018) %>%
  select(-estimate, -female, -age, - haverdistance, -race.eth, -voted2016b, 
         -voted2018)


set.seed(795861)
rf.binom <- randomForest(data = samp.rf, 
                         as.factor(Voted.2018) ~ Gender.Fem + Age + 
                           Race.Ethnicity + LLMI + Haversine.Distance + 
                           Voted.2016, 
                         ntree = 50, importance = TRUE)

varImpPlot(rf.binom, type = 2, main = "Random Forest Variable Importance")

plot(rf.binom$err.rate[,1], type = "l", main = "Random Forest OOB Error Rate", 
     ylab = "Misclassification Rate", xlab = "Tree Index")

min(rf.binom$err.rate[,1])

set.seed(795861)
sample.tree <- tree(data = samp.rf, 
                    as.factor(voted2018) ~ female + age + 
                      race.eth + l.inc + haverdistance + voted2016b)

plot(sample.tree)
text(sample.tree)



##Multinomial logistic

##Can't call summary on this
fhd2018.reasonable$vta2016 <- relevel(
  as.factor(fhd2018.reasonable$vta2016), ref = "N")  

multilogit.hdfull <- vglm(data = full.havdist2018.narm, 
                          formula = vt2018 ~ female + age + 
                            race.eth + estimate + haverdistance + 
                            vt2016 + race.eth*estimate, 
                          family = multinomial(refLevel = "N"))

#grouping mail and early as alternate vote to reduce size of vglm

multilogit.hdfull2 <- vglm(data = fhd2018.reasonable, 
                           formula = vta2018 ~ female + age + 
                             race.eth + estimate + haverdistance + 
                             vta2016 + race.eth*estimate, 
                           family = multinomial(refLevel = "N"))

##still can't call summary on this


##data sets for binomials to make up a multinomial model
##because we can't seem to call summary on an actual multinomial logistic regression
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



##piecemeal version

multlogit1.elday <- glm(data = fhd2018r.eldayn, 
                         vta2018en ~ female + age + 
                           race.eth + log(estimate) + hdistmiles + 
                           vta2016 + race.eth*log(estimate), 
                         family = binomial(link = "logit"))

y <- fhd2018r.eldayn$vta2018en
x.fullyscaled <- scale(model.matrix(multlogit1.elday)[,-1])

scaled.hdist2018 <- data.frame(vta2018en = y, x.fullyscaled)

multlogit.scale <- glm(data = scaled.hdist2018, 
                        vta2018en ~ female + age + 
                          race.ethBlack + race.ethHispanic + race.ethOther + 
                         log.estimate. + hdistmiles + 
                          vta2016Elday + vta2016Alt + 
                         race.ethBlack.log.estimate. + 
                          race.ethHispanic.log.estimate. + 
                          race.ethOther.log.estimate., 
                        family = binomial(link = "logit"))



multlogit2.alt <- glm(data = fhd2018r.altn, 
                        vta2018an ~ female + age + 
                          race.eth + log(estimate) + hdistmiles + 
                          vta2016 + race.eth*log(estimate), 
                        family = binomial(link = "logit"))

y <- fhd2018r.altn$vta2018an
x.fullyscaled <- scale(model.matrix(multlogit2.alt)[,-1])

scaled.hdist2018 <- data.frame(vta2018an = y, x.fullyscaled)

multlogit.scale2 <- glm(data = scaled.hdist2018, 
                       vta2018an ~ female + age + 
                         race.ethBlack + race.ethHispanic + race.ethOther + 
                         log.estimate. + hdistmiles + 
                         vta2016Elday + vta2016Alt + 
                         race.ethBlack.log.estimate. + 
                         race.ethHispanic.log.estimate. + 
                         race.ethOther.log.estimate., 
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
