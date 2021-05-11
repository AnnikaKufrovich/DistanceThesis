library(tidyverse)
library(data.table)
library(parallel)
library(rlist)
library(geosphere)
library(sf)
library(tigris)
library(osrm)


#setting to correct server

#before doing this need to set the server up on Bertha by doing the following:
#start a new screen
#navigate to Users/Bertha/osrm-backend
#run the command osrm-routed north-america-latest.osrm

options(osrm.server = "http://0.0.0.0:5000/") #server to use
options(osrm.profile = "car") #which transportation method to use

#test data
testpolls <- data.frame(abrprecs = c("ALA1", "ALA2", "BAY3", "CAL1", "CAL2"),
                        longitude = c(20, 21, 24, NA, NA), 
                        latitude = c(30, 30, 34, NA, NA), 
                        georating = c(100, 80, 50, NA, NA))

testvoters <- data.frame(voterid = c(1, 2, 3, 4, 5, 6), 
                         precid = c("ALA1", "ALA1", "ALA2", "ALA2", 
                                    "CAL1", "CAL2"), 
                         lon = c(20.5, 21.5, 23, NA, 40, NA), 
                         lat = c(31, 31.5, 35, NA, 42, NA))

#final function
osrmloop <- function(voterdata, polldata, voterabrprecs, 
                      pollabrprecs, georates){
  pollcount <- 0 #start a poll count
  final.calcs <- slice(voterdata, 0) #empty data frame for easy joining
  for(i in 1:nrow(polldata)){
    poll <- polldata[i, pollabrprecs] #first poll abbreviation and precinct
    poll.ll <- polldata[i,] %>% select(pollabrprecs, longitude, latitude) #longitude and latitude of first poll
    voters <- voterdata %>% 
      filter(voterdata[, voterabrprecs] == as.character(poll)) %>% #only voters with matching abreviation and precinct
      mutate(geopollrating = polldata[i, georates]) #add a variable with the poll's geocode rating
    voters.ll <- voters %>% 
      select(voterid, lon, lat) #longitude and latitude of matching voters only
    if(nrow(voters.ll) == 0){ #if there are no matching voters..
      final.calcs <- final.calcs %>% 
        full_join(voters) #join to final data frame
      #as with haverloop, above line may not be necessary but doesn't hurt
      pollcount <- pollcount + 1 #add to the poll count
      print(paste0(poll, ", poll #", pollcount, " skipped")) #notify that this poll was skipped
      next #next iteration
    }
    results <- osrmTable(src = voters.ll[1:nrow(voters.ll), c("voterid", "lon", "lat")], 
                         dst = poll.ll[1 , c(pollabrprecs, "longitude", "latitude")], 
                                           measure = "distance") #calculate driving distance between poll and its voters, in meters
    results.vec <- as.vector(results$distances) #turn results into a vector
    voters$drivedistance <- results.vec #add the vector to voter data as a column
    final.calcs <- final.calcs %>% 
      full_join(voters) #join to final dataframe
    pollcount <- pollcount + 1 #add to pollcount
    print(pollcount) #let me know what poll we are on
  }
  return(final.calcs)
}


#testing code
test <- osrmloop(voterdata = testvoters, polldata = testpolls, 
         voterabrprecs = "precid", 
         pollabrprecs = "abrprecs", georates = "georating")


test.dist <- osrmTable(src = testvoters[1:3, c("voterid", "lon", "lat")], 
          dst = testpolls[1, c("abrprecs", "longitude", "latitude")], 
          measure = "distance")


#loading in cleaned data
voterclean <- read.csv("2018voterscleaned.csv")
pollplace2018 <- read.csv("adjustgeo2018polls.csv")


#testing on sarasota county to check speed
voter.sarasota <- voterclean %>% 
  filter(V1 == "SAR") %>% 
  mutate(voterid = X)

poll.sarasota <- pollplace2018 %>% 
  filter(abr == "SAR")

voterhaverdistances2018 <- osrmloop(voterdata = voter.sarasota, 
                                     polldata = poll.sarasota, 
                                     voterabrprecs = "precID", 
                                     pollabrprecs = "abrprecincts", 
                                     georates = "geocode_rating")


#using original row numbers as voter id
#could alternatively adjust cleaning code in VF.clean to keep voterid, but this works
voterclean$voterid <- voterclean$X

#running osrm distances
voterosrmdistances2018 <- osrmloop(voterdata = voterclean, 
                                   polldata = pollplace2018, 
                                   voterabrprecs = "precID", 
                                   pollabrprecs = "abrprecincts", 
                                   georates = "geocode_rating")

#making voted 2016b for models, see haversine loop comments if confused
voterdrive2018 <- voterosrmdistances2018 %>% 
  mutate(voted2016b = ifelse(V5.y == "A", 1, 
                             ifelse(V5.y == "E", 1, 
                                    ifelse(V5.y == "Y", 1,  0)))) %>%
  select(-voted2016, -lon, -lat)

#saving dataframe
write_csv(voterdrive2018, "2018VoterDriveDist.csv")


#redoing haversine driving distances with "id" included
voterhaverdrivedistances2018 <- haverloop(voterdata = voterclean, 
                                     polldata = pollplace2018, 
                                     voterabrprecs = "precID", 
                                     pollabrprecs = "abrprecincts", 
                                     georates = "geocode_rating")

#saving so that work is not lost
voterhaverdistances2018 <- voterhaverdrivedistances2018 %>%
  mutate(voted2016b = ifelse(V5.y == "A", 1, 
                             ifelse(V5.y == "E", 1, 
                                    ifelse(V5.y == "Y", 1,  0)))) %>%
  select(-voted2016)

write_csv(voterhaverdistances2018, "2018VoterHaverDist2.csv")

#creating skeleton dataframe of "id" and haverdistance to join with osrm distances
haver.id <- voterhaverdistances2018 %>%
  select(X, haverdistance)

#joining to osrm distances
voterbothdistances2018  <- full.drivedist.2018 %>%
  left_join(haver.id)
  
#saving combined datafram for later comparisons
write_csv(voterbothdistances2018, "2018VoterBothDist.csv")
