library(dplyr)
library(tidyverse)
library(tidycensus)
library(parallel)
library(car)

#Point this at whatever directory you have placed Florida voter files into.  Make sure that ONLY the county voter files are in this directory
#Get all the filenames for each county and place them in an object
filenames <- dir("20190212_VoterDetail", full.names=TRUE)

#For all those filenames, read the files in as objects in an array (list)
all <- mclapply(filenames,function(i){
  read.delim(i, header=F, sep="\t", fill=TRUE)
})

#Eliminate variables we don't need, keeping merging data (V2 and V12) + demographics (V20-23)
pruned <- all %>% mclapply(. %>% select(V2, V12, V20:V23))

#Combined all datasets in the array into a single dataframe
#This step is slow but does not parellelize easily
combine <- do.call(rbind, pruned)

#Point this at whatever directory you have placed Florida voter history files into.  Make sure that ONLY the county voter files are in this directory
#Get all the filenames for each county and place them in an object
filenames2 <- dir("20190212_VoterHistory", full.names=TRUE)

#For all those filenames, read the files in as objects in an array (list)
all2 <- mclapply(filenames2,function(i){
  read.delim(i, header=F, sep="\t", fill=TRUE)
})

#Eliminate variables we don't need, which is county (V1) and type of election (V4)
pruned2 <- all2 %>% mclapply(. %>% select(V2:V3, V5))

#Combined all datasets in the array into a single dataframe
#This step is again slow but does not parellelize easily
combine2 <- do.call(rbind, pruned2)

#filtering to only 2018 general election
genelec2018 <- combine2 %>%
  filter(V3 == "11/06/2018") %>%
  mutate(vote2018 = ifelse(V5 == "A" | V5 == "E" | V5 == "Y", 1, 0)) %>%
  select(V2, vote2018)


#filtering to only 2016 general election
genelec2016 <- combine2 %>%
  filter(V3 == "11/08/2016") %>%
  mutate(vote2016 = ifelse(V5 == "A" | V5 == "E" | V5 == "Y", 1, 0)) %>%
  select(V2, vote2016)

#joining dataframes by voterID
voterdethist <- left_join(combine, genelec2018, by = "V2")
voterdethistfull <- left_join(voterdethist, genelec2016, by = "V2")


##creating age and years registered
voterdethistfull$birthdate <- as.Date(voterdethistfull$V22, "%m/%d/%Y")
voterdethistfull$age <- as.numeric((as.Date("11/06/2018", "%m/%d/%Y")-voterdethistfull$birthdate)/365)
voterdethistfull$regdate <- as.Date(voterdethistfull$V23, "%m/%d/%Y")
voterdethistfull$regyears <- as.numeric((as.Date("11/06/2018", "%m/%d/%Y")-voterdethistfull$regdate)/365)


combined <- voterdethistfull %>%
  #Encode gender
  mutate(female = ifelse(V20 == "F", 1, 0)) %>% 
  #create dummies for white, black, hispanic
  mutate(white = ifelse(V21 == "5", 1, 0)) %>% 
  mutate(black = ifelse(V21 == "3", 1, 0)) %>% 
  mutate(hispanic = ifelse(V21 == "4", 1, 0)) %>% 
  mutate(other = ifelse(V21 == "4" | V21 == "3" | V21 == "5", 0, 1)) %>% 
  mutate(zipcode = as.numeric(
    substr(x = as.character(V12), start = 1, stop = 5)))


census_api_key("864d0c376476571c0dee5dff3422e4a3b75ab093")

medinc.zip <- get_acs(geography = "zcta", 
                      variables = c(medincome = "B19013_001"))
medinc.zipselect <- medinc.zip %>%
  mutate(zipcode = as.numeric(GEOID)) %>%
  select(zipcode, estimate)

medhouse.zip <- get_acs(geography = "zcta", 
                      variables = c(medhouse = "B25077_001"))
medhouse.zipselect <- medhouse.zip %>%
  mutate(zipcode = as.numeric(GEOID)) %>%
  select(zipcode, estimate)

combined <- left_join(combined, medinc.zipselect, by = "zipcode")
combined <- left_join(combined, medhouse.zipselect, by = "zipcode")



combine_clean <- combined %>% 
  mutate(medincome = estimate.x) %>% 
  mutate(medhouse = estimate.y) %>%
  select(female, age, regyears, white, black, hispanic, other, medincome, medhouse, vote2018, vote2016, zipcode)


ind.voters <- combine_clean %>%
  mutate(vote.2018 = ifelse(is.na(vote2018) == TRUE, 0, vote2018)) %>% 
  mutate(vote.2016 = ifelse(is.na(vote2016) == TRUE, 0, vote2016)) %>% 
  select(female, age, regyears, white, black, hispanic, other, medincome, medhouse, vote.2018, vote.2016, zipcode)


set.seed(128469)

votersamp <- ind.voters[sample(nrow(ind.voters), 500000), ]

write.csv(votersamp, "Votersample.csv")

zip.voters <- ind.voters %>%
  group_by(zipcode) %>%
  summarize(female.prop = mean(female, na.rm = T), mean.age = mean(age, na.rm = T), 
            black.prop = mean(black, na.rm = T), hispanic.prop = mean(hispanic, na.rm = T), 
            other.prop = mean(other, na.rm = T), medincome = mean(medincome, na.rm = T), 
            medhouse = mean(medhouse, na.rm = T))

write.csv(zip.voters, "zipcodevoters.csv")

zip.voters <- read.csv("zipcodevoters.csv")

cor(zip.voters)


lm.house <- lm(medhouse ~ medincome + female.prop + mean.age + black.prop + hispanic.prop + other.prop, data = zip.voters)

lm.house2 <- lm(medhouse ~ medincome + female.prop + mean.age + black.prop + hispanic.prop, data = zip.voters)

anova(lm.house2, lm.house)

lm.house3 <- lm(log(medhouse) ~ medincome + female.prop + mean.age + black.prop + hispanic.prop, data = zip.voters)

lm.house4 <- lm(log(medhouse) ~ log(medincome) + female.prop + mean.age + black.prop + hispanic.prop, data = zip.voters)





votersamp <- read.csv("Votersample.csv")

glm.vote <- glm(vote.2018 ~ vote.2016 + female + black + hispanic + other + age + regyears + medincome, 
                data = votersamp, family = binomial)

glm.vote2 <- glm(vote.2018 ~ vote.2016 + black + hispanic + other + age + regyears + medincome, 
                data = votersamp, family = binomial)

glm.vote3 <- glm(vote.2018 ~ vote.2016 + black + hispanic + other + age + medincome, 
                 data = votersamp, family = binomial)

glm.probs <- predict(glm.vote3, type='response')
glm.pred <- ifelse(glm.probs > 0.50, 1, 0)

mean(glm.pred == votersamp$vote.2018, na.rm = TRUE)
