library(tidyverse)
library(data.table)
library(parallel)
library(rlist)
library(geosphere)
library(sf)
library(tigris)

testpolls <- data.frame(abrprecs = c("ALA1", "ALA2", "BAY3", "CAL1", "CAL2"),
                        longitude = c(20, 21, 24, NA, NA), 
                        latitude = c(30, 30, 34, NA, NA), 
                        georating = c(100, 80, 50, NA, NA))

testvoters <- data.frame(voterid = c(1, 2, 3, 4, 5, 6), 
                         precid = c("ALA1", "ALA1", "ALA2", "ALA2", 
                                    "CAL1", "CAL2"), 
                         lon = c(20.5, 21.5, 23, NA, 40, NA), 
                         lat = c(31, 31.5, 35, NA, 42, NA))



haverloop <- function(voterdata, polldata, voterabrprecs, 
                      pollabrprecs, georates){
  pollcount <- 0 #start a poll count
  final.calcs <- slice(voterdata, 0) #empty dataframe for easy joining
  for(i in 1:nrow(polldata)){
    poll <- polldata[i, pollabrprecs] #first poll abreviation and precinct
    poll.ll <- polldata[i,] %>% select(longitude, latitude) #longitude and latitude of first poll
    voters <- voterdata %>% 
      filter(voterdata[, voterabrprecs] == as.character(poll)) %>% #only voters with matching abreviation and precinct
      mutate(geopollrating = polldata[i, georates]) #add a variable with the poll's geocode rating
    voters.ll <- voters %>% 
      select(lon, lat) #longitude and latitude of matching voters only
    if(nrow(voters.ll) == 0){ #if there are no matching voters..
      final.calcs <- final.calcs %>% 
        full_join(voters) #join to final data frame
      pollcount <- pollcount + 1 #add to the poll count
      print(paste0(poll, ", poll #", pollcount, " skipped")) #notify that this poll was skipped
      next #next iteration
    }
    results <- distHaversine(p1 = poll.ll, p2 = voters.ll) #calculate haversine distance between poll and its voters, in meters
    results.vec <- as_vector(results) #turn results into a vector
    voters$haverdistance <- results.vec #add the vector to voter data as a column
    final.calcs <- final.calcs %>% 
      full_join(voters) #join to final dataframe
    pollcount <- pollcount + 1 #add to pollcount
    print(pollcount) #let me know what poll we are on
  }
  return(final.calcs)
}


haverloop(voterdata = testvoters, polldata = testpolls, 
          voterabrprecs = "precid", 
          pollabrprecs = "abrprecs", 
          georates = "georating")

finaldata <- haverloop(voterdata = testvoters, polldata = testpolls, 
                       voterabrprecs = "precid", 
                       pollabrprecs = "abrprecs", 
                       georates = "georating")


voterhaverdistances2018 <- haverloop(voterdata = combine_clean, 
                                     polldata = pollplace2018, 
                                     voterabrprecs = "precID", 
                                     pollabrprecs = "abrprecincts", 
                                     georates = "geocode_rating")

voterhaverdistances2018 <- voterhaverdistances2018 %>%
  mutate(voted2016b = ifelse(V5.y == "A", 1, 
                                    ifelse(V5.y == "E", 1, 
                                           ifelse(V5.y == "Y", 1,  0)))) %>%
  select(-voted2016)

write_csv(voterhaverdistances2018, "2018VoterHavDist.csv")


##code testing
#poll <- testpolls$abrprecs[1] # taking first row
#poll.ll <- testpolls[1,] %>% select(longitude, latitude)

#voters <- testvoters %>% filter(precid == as.character(poll))
#voters.ll <- voters %>% select(lon, lat)

#results <- distHaversine(p1 = poll.ll, p2 = voters.ll)

#results.vec <- as_vector(results)

#results.tib$destination <- poll

#voters$distance <- results.vec

#testvoters <- testvoters %>% left_join(voters)

##code testing 2

#poll <- testpolls$abrprecs[2] # taking first row
#poll.ll <- testpolls[2,] %>% select(longitude, latitude)

#voters <- testvoters %>% filter(precid == as.character(poll))
#voters.ll <- voters %>% select(lon, lat)

#results <- distHaversine(p1 = poll.ll, p2 = voters.ll)

#results.vec <- as_vector(results)

#results.tib$destination <- poll

#voters$distance <- results.vec

#testvoters <- testvoters %>% left_join(voters)


##code testing 3

#poll <- testpolls$abrprecs[3] # taking first row
#poll.ll <- testpolls[3,] %>% select(longitude, latitude)

#voters <- testvoters %>% filter(precid == as.character(poll))
#voters.ll <- voters %>% select(lon, lat)

#results <- distHaversine(p1 = poll.ll, p2 = voters.ll)

#results.vec <- as_vector(results)

#results.tib$destination <- poll

#voters$distance <- results.vec

#testvoters <- testvoters %>% left_join(voters)


##code testing 4

#poll <- testpolls$abrprecs[4] # taking first row
#poll.ll <- testpolls[4,] %>% select(longitude, latitude)

#voters <- testvoters %>% filter(precid == as.character(poll))
#voters.ll <- voters %>% select(lon, lat)

#results <- distHaversine(p1 = poll.ll, p2 = voters.ll)

#results.vec <- as_vector(results)

#results.tib$destination <- poll

#voters$distance <- results.vec

#testvoters <- testvoters %>% left_join(voters)