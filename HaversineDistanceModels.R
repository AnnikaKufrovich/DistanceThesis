library(dplyr)
library(RcmdrMisc)

full.havdist.2018 <- read.csv("2018VoterHavDist.csv")

ratehd.40andless.2018 <- full.havdist.2018 %>%
  filter(as.numeric(as.character(geopollrating)) <= 40)

ratehd.20andless.2018 <- full.havdist.2018 %>%
  filter(as.numeric(as.character(geopollrating)) <= 20)

##clean environment as much as possible before running each
##they take up a lot of memory
##remember than distance is in meters

simplelogit.hdfull <- glm(data = full.havdist.2018, 
                          voted2018 ~ female + age + 
                            white + black + hispanic + 
                            estimate + haverdistance + 
                            voted2016b, family = binomial(link = "logit"))


##AIC
stepwise(simplelogit.hdfull, direction = "backward", criterion = "AIC")

stepwise(simplelogit.hdfull, direction = "backward/forward", 
         criterion = "AIC")

stepwise(simplelogit.hdfull, direction = "forward/backward", 
         criterion = "AIC")

##BIC




#same model but better geocoded polls
simplelogit.hdfull <- glm(data = ratehd.40andless.2018, 
                          voted2018 ~ female + age + 
                            white + black + hispanic + 
                            estimate + haverdistance + 
                            voted2016b, family = binomial(link = "logit"))


#same model but most accurate geocoded polls
simplelogit.hdfull <- glm(data = ratehd.40andless.2018, 
                          voted2018 ~ female + age + 
                            white + black + hispanic + 
                            estimate + haverdistance + 
                            voted2016b, family = binomial(link = "logit"))