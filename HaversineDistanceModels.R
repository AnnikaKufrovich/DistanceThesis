library(dplyr)
library(RcmdrMisc)
library(pryr)
library(VGAM)

full.havdist.2018 <- read.csv("2018VoterHavDist.csv")

full.havdist2018.narm <- na.omit(full.havdist.2018)

rm(full.havdist.2018) #removing to free up environment space

ratehd.40andless.2018 <- full.havdist2018.narm %>%
  filter(as.numeric(as.character(geopollrating)) <= 40)

ratehd.20andless.2018 <- full.havdist2018.narm %>%
  filter(as.numeric(as.character(geopollrating)) <= 20)


##clean environment as much as possible before running each
##they take up a lot of memory
##remember than distance is in meters

##estimate is income estimate
simplelogit.hdfull <- glm(data = full.havdist2018.narm, 
                          voted2018 ~ female + age + 
                            white + black + hispanic + 
                            estimate + haverdistance + 
                            voted2016b, family = binomial(link = "logit"))

##AIC

stepwise(simplelogit.hdfull, direction = "forward", criterion = "AIC")

stepwise(simplelogit.hdfull, direction = "backward", criterion = "AIC")

stepwise(simplelogit.hdfull, direction = "backward/forward", 
         criterion = "AIC")

stepwise(simplelogit.hdfull, direction = "forward/backward", 
         criterion = "AIC")

##BIC

stepwise(simplelogit.hdfull, direction = "forward", criterion = "BIC")

stepwise(simplelogit.hdfull, direction = "backward", criterion = "BIC")

stepwise(simplelogit.hdfull, direction = "backward/forward", 
         criterion = "BIC")

stepwise(simplelogit.hdfull, direction = "forward/backward", 
         criterion = "BIC")


##remove logit from environment for space

rm(simplelogit.hdfull)


##reduced
simplelogit.hdfull <- glm(data = full.havdist2018.narm, 
                          voted2018 ~ age + 
                            white + black + hispanic + 
                            estimate + haverdistance + 
                            voted2016b, family = binomial(link = "logit"))



multilogit.hdfull <- vglm(data = full.havdist2018.narm, 
                          formula = V5.x ~ female + age + 
                            white + black + hispanic + 
                            estimate + haverdistance + 
                            voted2016b, 
                         family = multinomial(refLevel = "N"))





#same model but most accurate geocoded polls
simplelogit.hdfull <- vglm(data = ratehd.40andless.2018, 
                          voted2018 ~ female + age + 
                            white + black + hispanic + 
                            estimate + haverdistance + 
                            voted2016b, family = binomial(link = "logit"))