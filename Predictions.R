library(dplyr)

#run code for the fhd.reasonable object in HaversineModels.R 
#and binomlogit.hdfull for this file to work


##testing out predictions on orange county for quick running
#filter to orange county
orange2018 <- fhd2018.reasonable %>%
  mutate(county = substr(precID, start = 1, stop = 3)) %>%
  filter(county == "ORA")

#probability of turning out
norm.pred <- predict(binomlogit.hdfull, newdata = orange2018, type = "response")

#assigning probabilities to TRUE/FALSE prediction (True = vote, False == no vote)
norm.predvs <- ifelse(norm.pred >= 0.5, TRUE, FALSE)


#adding 1 mile to distances
orange2018.plus1mile <- orange2018
orange2018.plus1mile$hdistmiles <- orange2018.plus1mile$hdistmiles + 1

#predicting with new data
new.pred <- predict(binomlogit.hdfull, newdata = orange2018.plus1mile, type = "response")

#assigning probabilities to TRUE/FALSE preddictions
new.predvs <- ifelse(new.pred >= 0.5, TRUE, FALSE)


#table for comparison
table(new.predvs, norm.predvs)



##repeat with broward
broward2018 <- fhd2018.reasonable %>%
  mutate(county = substr(precID, start = 1, stop = 3)) %>%
  filter(county == "BRO")

norm.pred2 <- predict(binomlogit.hdfull, newdata = broward2018, type = "response")

norm.predvs2 <- ifelse(norm.pred2 >= 0.5, TRUE, FALSE)

broward2018.plus1mile <- broward2018
broward2018.plus1mile$hdistmiles <- broward2018.plus1mile$hdistmiles + 1


new.pred2 <- predict(binomlogit.hdfull, newdata = broward2018.plus1mile, 
                     type = "response")

new.predvs2 <- ifelse(new.pred2 >= 0.5, TRUE, FALSE)

table(new.predvs, norm.predvs)




#predicting with whole dataset

#normal predicted probabilities
norm.pred <- predict(binomlogit.hdfull, newdata = fhd2018.reasonable, 
                     type = "response")

#TRUE/FALSE assignments
norm.predvs <- ifelse(norm.pred >= 0.5, TRUE, FALSE)

#adding 1 mile to all distances
fhd2018.reasonable.plus1mile <- fhd2018.reasonable
fhd2018.reasonable.plus1mile$hdistmiles <- fhd2018.reasonable.plus1mile$hdistmiles + 1

#new predictions with additional mile
new.pred <- predict(binomlogit.hdfull, newdata = fhd2018.reasonable.plus1mile, 
                    type = "response")

#TRUE/FALSE assignments
new.predvs <- ifelse(new.pred >= 0.5, TRUE, FALSE)

#table comparison
table(new.predvs, norm.predvs)

#now adding 2.5 miles to base line 
fhd2018.reasonable.plus2.5mile <- fhd2018.reasonable
fhd2018.reasonable.plus2.5mile$hdistmiles <- fhd2018.reasonable.plus2.5mile$hdistmiles + 2.5

#new predictions with additional 2.5 miles
new.pred <- predict(binomlogit.hdfull, newdata = fhd2018.reasonable.plus2.5mile, 
                    type = "response")

#TRUE/FALSE assignments
new.predvs <- ifelse(new.pred >= 0.5, TRUE, FALSE)

#comparing to baseline models
table(new.predvs, norm.predvs)


#comparing baseline predictions to actual turnout 
actual.votes <- ifelse(fhd2018.reasonable$voted2018 == 1, TRUE, FALSE)

table(norm.predvs, actual.votes)
