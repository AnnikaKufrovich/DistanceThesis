nrow(fhd2018.reasonable)

set.seed(189246)

#samp.ord <- sample(1:nrow(fhd2018.reasonable))

#try 3 sub samples each of various sizes to check pvals
#then automate for whole data set if needed

samp1 <- fhd2018.reasonable[sample(1:nrow(fhd2018.reasonable), 500000),]
samp2 <- fhd2018.reasonable[sample(1:nrow(fhd2018.reasonable), 500000),]
samp3 <- fhd2018.reasonable[sample(1:nrow(fhd2018.reasonable), 500000),]


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

summary(binomlogit1)
summary(binomlogit2)
summary(binomlogit3)