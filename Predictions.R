library(dplyr)

orange2018 <- fhd2018.reasonable %>%
  mutate(county = substr(precID, start = 1, stop = 3)) %>%
  filter(county == "ORA")

norm.pred <- predict(binomlogit.hdfull, newdata = orange2018, type = "response")

norm.predvs <- ifelse(norm.pred >= 0.5, TRUE, FALSE)

orange2018.plus1mile <- orange2018
orange2018.plus1mile$haverdistance <- orange2018.plus1mile$haverdistance + 1


new.pred <- predict(binomlogit.hdfull, newdata = orange2018.plus1mile, type = "response")

new.predvs <- ifelse(new.pred >= 0.5, TRUE, FALSE)



broward2018 <- fhd2018.reasonable %>%
  mutate(county = substr(precID, start = 1, stop = 3)) %>%
  filter(county == "BRO")

norm.pred2 <- predict(binomlogit.hdfull, newdata = broward2018, type = "response")

norm.predvs2 <- ifelse(norm.pred2 >= 0.5, TRUE, FALSE)

broward2018.plus1mile <- broward2018
broward2018.plus1mile$haverdistance <- broward2018.plus1mile$haverdistance + 1


new.pred2 <- predict(binomlogit.hdfull, newdata = broward2018.plus1mile, type = "response")

new.predvs2 <- ifelse(new.pred2 >= 0.5, TRUE, FALSE)




norm.pred <- predict(binomlogit.hdfull, newdata = fhd2018.reasonable, 
                     type = "response")

norm.predvs <- ifelse(norm.pred >= 0.5, TRUE, FALSE)

fhd2018.reasonable.plus1mile <- fhd2018.reasonable
fhd2018.reasonable.plus1mile$hdistmiles <- fhd2018.reasonable.plus1mile$hdistmiles + 1


new.pred <- predict(binomlogit.hdfull, newdata = fhd2018.reasonable.plus1mile, 
                    type = "response")

new.predvs <- ifelse(new.pred >= 0.5, TRUE, FALSE)

table(new.predvs, norm.predvs)


fhd2018.reasonable.plus2.5mile <- fhd2018.reasonable
fhd2018.reasonable.plus2.5mile$hdistmiles <- fhd2018.reasonable.plus2.5mile$hdistmiles + 2.5


new.pred <- predict(binomlogit.hdfull, newdata = fhd2018.reasonable.plus2.5mile, 
                    type = "response")

new.predvs <- ifelse(new.pred >= 0.5, TRUE, FALSE)

table(new.predvs, norm.predvs)



actual.votes <- ifelse(fhd2018.reasonable$voted2018 == 1, TRUE, FALSE)

table(norm.predvs, actual.votes)
