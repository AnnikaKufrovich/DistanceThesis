library(parallel)
library(tidyverse)
library(tidycensus)
library(stringr)

#Detect local core count for multithreaded commands
cpucores<-detectCores()

#Point this at whatever directory you have placed Florida voter files into.  Make sure that ONLY the county voter files are in this directory
#Get all the filenames for each county and place them in an object
filenames <- dir("geocoded", full.names=TRUE)

#For all those filenames, read the files in as objects in an array (list) and limit variables
all <- lapply(filenames,function(i){
  read.delim(i, header=T, sep="\ ", fill=FALSE) %>% select(V1:V2, V12, V20:V22, V24:V28, lon, lat)
})


#Combined all datasets in the array into a single dataframe
combine <- do.call(rbind, all)

#Point this at whatever directory you have placed Florida voter history files into.  Make sure that ONLY the county voter files are in this directory
#Get all the filenames for each county and place them in an object
filenames2 <- dir("20190604_VoterHistory", full.names=TRUE)

#For all those filenames, read the files in as objects in an array (list)
all2 <- mclapply(filenames2,function(i){
  read.delim(i, header=F, sep="\t", fill=TRUE)
}, mc.cores=cpucores)

#Eliminate variable we don't need, which is county (V1) and type of election (V4)
pruned2 <- all2 %>% mclapply(. %>% select(V2:V3, V5), mc.cores=cpucores)

#Combined all datasets in the array into a single dataframe
#This step is slow but does not parellelize easily
combine2 <- do.call(rbind, pruned2)

#filtering to only 2018 general election
genelec2018 <- combine2 %>%
  filter(V3 == "11/06/2018")

genelec2016 <- combine2 %>%
  filter(V3 == "11/08/2016")

#joining dataframes by voterID
voterdethist <- combine %>% 
  left_join(genelec2018, by = "V2") %>%
  left_join(genelec2016, by = "V2")



#Making binary variables for turnout
voters <- voterdethist %>%
  mutate(voted2018 = ifelse(V5.x == "A", 1, 
                        ifelse(V5.x == "E", 1, 
                               ifelse(V5.x == "Y", 1, 0)))) %>%
  mutate(voted2016 = ifelse(V5.x == "A", 1, 
                            ifelse(V5.x == "E", 1, 
                                   ifelse(V5.x == "Y", 1,  0)))) %>%
  select(V1, V12, V20:V22, V24:V28, lon, lat, V5.x, voted2018, V5.y, voted2016)

voters$V5.x[is.na(voters$V5.x)] = "N"
voters$V5.y[is.na(voters$V5.y)] = "N"
voters$voted2018[is.na(voters$voted2018)] = 0
voters$voted2016[is.na(voters$voted2016)] = 0

#loading in ACS data
census_api_key("864d0c376476571c0dee5dff3422e4a3b75ab093")

medinc.zip <- get_acs(geography = "zcta", 
                      variables = c(medincome = "B19013_001"))
medinc.zipselect <- medinc.zip %>%
  mutate(zipcode = as.factor(str_remove(GEOID, "NA"))) %>%
  select(zipcode, estimate)

##Recode as necessary (may be faster in MRO)
#Encode age
voters$birthdate <- as.Date(voters$V22, "%m/%d/%Y")
voters$age<-as.numeric((as.Date("11/06/2018", "%m/%d/%Y")-voters$birthdate)/365)

combined <- voters %>%
  #Encode gender
  mutate(female = ifelse(V20 == "F", 1, 0)) %>% 
  #create ID
  filter(V25 != "*") %>% 
  mutate(precID = paste0(V1, trimws(V25))) %>% 
  #create dummies for white, black, hispanic
  mutate(white = ifelse(V21 == "5", 1, 0)) %>% 
  mutate(black = ifelse(V21 == "3", 1, 0)) %>% 
  mutate(hispanic = ifelse(V21 == "4", 1, 0)) %>% 
  #create dummies for Dem, Rep, Ind
  mutate(dem = ifelse(V24 == "DEM", 1, 0)) %>% 
  mutate(rep = ifelse(V24 == "REP", 1, 0)) %>% 
  mutate(npa = ifelse(V24 == "NPA", 1, 0)) %>%
  mutate(zipcode = as.factor(
    substr(x = as.character(V12), start = 1, stop = 5)))

##Join with median income data from ACS

combined <- left_join(combined, medinc.zipselect, by = "zipcode")

#drop vars we don't want
combine_clean<- combined%>% select(V1, female, age, precID, V25:V28, white, black, hispanic, dem, rep, npa, estimate)

#comparing precID and other precinct variables to abrprecincts and fixing issues for haversine loop

pollplace2018 <- read.csv("GEOCODED_2018PollPlace", header = TRUE)



#Write the file out as a .csv
#write.csv(, "2018voterscleaned.csv")
