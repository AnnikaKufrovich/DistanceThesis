library(dplyr)

#similar to Predictions.R, need to have fhd2018.resonable for this to run

nrow(fhd2018.reasonable)

#trying 3 sub samples each of various sizes to check pvalue rankings
#if p values bottom out can look at z scores

#same seeded sample for random forest, slicing part of it into 3 dataframes
set.seed(795861)
samp.rf <- fhd2018.reasonable[sample(1:nrow(fhd2018.reasonable), 2500000),] %>%
  mutate(l.inc = log(estimate))

samp1 <- samp.rf %>% slice_head(prop = .2)
samp.rf <- samp.rf %>% slice_tail(prop = 0.8)

samp2 <- samp.rf %>% slice_head(prop = .25)

samp3 <- samp.rf %>% slice_tail(prop = 0.25)


#binomial logistic regression fits
binomlogit1 <- glm(data = samp1, 
                       voted2018 ~ female + age + 
                         race.eth + log(estimate) + hdistmiles + 
                         voted2016b + race.eth*log(estimate), 
                       family = binomial(link = "logit"))

binomlogit2 <- glm(data = samp2, 
                       voted2018 ~ female + age + 
                         race.eth + log(estimate) + hdistmiles + 
                         voted2016b + race.eth*log(estimate), 
                       family = binomial(link = "logit"))

binomlogit3 <- glm(data = samp3, 
                       voted2018 ~ female + age + 
                         race.eth + log(estimate) + hdistmiles + 
                         voted2016b + race.eth*log(estimate), 
                       family = binomial(link = "logit"))


#summaries, the rankings all look very similar to our standardized model rankings
summary(binomlogit1)
summary(binomlogit2)
summary(binomlogit3)