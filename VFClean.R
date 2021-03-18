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

pollplace2018 <- read.csv("GEOCODED_2018PollPlace.txt", header = TRUE)

#fixing volusia coding in poll data
pollplace2018$abr <- recode(pollplace2018$abr, "127" = "VOL")
pollplace2018$abrprecincts <- paste0(pollplace2018$abr, pollplace2018$precinct)

pollplace2018$abrprecincts <- recode(pollplace2018$abrprecincts, 
                                     "ALA1" = "ALA01", "ALA2" = "ALA02", 
                                     "ALA3" = "ALA03", "ALA4" = "ALA04", 
                                     "ALA5" = "ALA05", "ALA6" = "ALA06", 
                                     "ALA7" = "ALA07", "ALA8" = "ALA08", 
                                     "ALA9" = "ALA09")

pollplace2018$abrprecincts <- recode(pollplace2018$abrprecincts, 
                                     "FRA1" = "FRA01", "FRA2" = "FRA02", 
                                     "FRA3" = "FRA03", "FRA4" = "FRA04", 
                                     "FRA5" = "FRA05", "FRA6" = "FRA06", 
                                     "FRA7" = "FRA07", "FRA8" = "FRA08")

pollplace2018$abrprecincts <- recode(pollplace2018$abrprecincts, 
                                     "LAK1" = "LAK01", "LAK2" = "LAK02", 
                                     "LAK3" = "LAK03", "LAK4" = "LAK04", 
                                     "LAK5" = "LAK05", "LAK6" = "LAK06", 
                                     "LAK7" = "LAK07", "LAK8" = "LAK08", 
                                     "LAK9" = "LAK09")

pollplace2018$abrprecincts <- recode(pollplace2018$abrprecincts, 
                                     "LEV1" = "LEV01", "LEV2" = "LEV02", 
                                     "LEV3" = "LEV03", "LEV4" = "LEV04", 
                                     "LEV5" = "LEV05", "LEV6" = "LEV06", 
                                     "LEV7" = "LEV07", "LEV8" = "LEV08", 
                                     "LEV9" = "LEV09")

pollplace2018$abrprecincts <- recode(pollplace2018$abrprecincts, 
                                     "MRN1" = "MRN0001", "MRN2" = "MRN0002", 
                                     "MRN3" = "MRN0003", "MRN4" = "MRN0004", 
                                     "MRN5" = "MRN0005", "MRN6" = "MRN0006", 
                                     "MRN7" = "MRN0007", "MRN8" = "MRN0008", 
                                     "MRN9" = "MRN0009", "MRN10" = "MRN0010", 
                                     "MRN11" = "MRN0011", "MRN12" = "MRN0012", 
                                     "MRN13" = "MRN0013", "MRN14" = "MRN0014", 
                                     "MRN15" = "MRN0015", "MRN16" = "MRN0016", 
                                     "MRN17" = "MRN0017", "MRN18" = "MRN0018", 
                                     "MRN19" = "MRN0019", "MRN40" = "MRN0040", 
                                     "MRN50" = "MRN0050", "MRN60" = "MRN0060", 
                                     "MRN70" = "MRN0070")

pollplace2018$abrprecincts <- recode(pollplace2018$abrprecincts, 
                                     "MRT1" = "MRT01", "MRT2" = "MRT02", 
                                     "MRT3" = "MRT03", "MRT4" = "MRT04", 
                                     "MRT5" = "MRT05", "MRT6" = "MRT06", 
                                     "MRT7" = "MRT07", "MRT8" = "MRT08", 
                                     "MRT9" = "MRT09")
pollplace2018$abrprecincts <- if_else(pollplace2018$abr == "PAS", 
                                 true = (pollplace2018$abrprecincts = 
                                 paste0(pollplace2018$abr, 
                                        sprintf("%03s", pollplace2018$precinct))), 
                                 false = (pollplace2018$abrprecincts = pollplace2018$abrprecincts))

pollplace2018$abrprecincts <- str_replace(pollplace2018$abrprecincts, "PUT0", "PUT")

pollplace2018$abrprecincts <- if_else(pollplace2018$abr == "SEM", 
                                      true = (pollplace2018$abrprecincts = 
                                                paste0(pollplace2018$abr, 
                                                       sprintf("%03s", pollplace2018$precinct))), 
                                      false = (pollplace2018$abrprecincts = pollplace2018$abrprecincts))

pollplace2018$abrprecincts <- str_replace(pollplace2018$abrprecincts, "STL0", "STL")
pollplace2018$abrprecincts <- str_replace(pollplace2018$abrprecincts, "STL0", "STL")

pollplace2018$abrprecincts <- if_else(pollplace2018$abr == "SUW", 
                                      true = (pollplace2018$abrprecincts = 
                                                paste0(pollplace2018$abr, 
                                                       sprintf("%02s", pollplace2018$precinct))), 
                                      false = (pollplace2018$abrprecincts = pollplace2018$abrprecincts))

pollplace2018$abrprecincts <- str_replace(pollplace2018$abrprecincts, "WAS0", "WAS")

#Write the file out as a .csv
write.csv(combine_clean, "2018voterscleaned.csv")
write.csv(pollplace2018, "adjustgeo2018polls.csv")
