library(dplyr)
library(ggplot2)

#to look at the correlation between Haversine distances and OSRM Driving distances

#Can join data this way or load the both distances data created from OSRMDistanceCalcs.R

haverdistances <- read.csv("2018VoterHavDist.csv")


bothdistances <- voterosrmdistances2018 %>% 
  left_join(id.havdist, by = "X") %>%
  select(drivedistance, haverdistance, precID, geopollrating)

#alternative loading
#bothdistances <- read.csv("2018VoterBothDist.csv")

bothdistances2 <- na.omit(bothdistances) %>% 
  select(drivedistance, haverdistance, precID) %>% 
  mutate(drivemiles = drivedistance/1609.34, 
         havermiles = haverdistance/1609.34) %>% 
  filter(haverdistance <= 8045)
  

#initial correlation
cor(na.omit(bothdistances2[,4:5]))

#expoloratory plot that is not very useful
#plot(data = bothdistances, drivedistance ~ haverdistance)


#pretty correlation plot
cors <- ggplot(data = bothdistances2, 
               mapping = aes(x = havermiles, y = drivemiles)) + 
  geom_point(alpha = 0.1, fill = NA) + 
  labs(title = "Haversine and Driving Distances", 
       x = "Haversine Distances (miles)", 
       y = "Driving Distances (miles)")
cors + theme_classic() #+ geom_hline(yintercept = 10000) + geom_vline(xintercept = 3000)

#commented out lines are to help pinpoint weird date


#figuting out precincts contibuting to the weird stream in the lower left
weird.data <- bothdistances2 %>% 
  filter(haverdistance < 3150) %>%
  filter(drivedistance > 10000) %>%
  filter(drivedistance < 15000)

weird.data2 <- bothdistances2 %>%
  filter(precID != "LEE46") %>%
  filter(haverdistance < 2000) %>%
  filter(drivedistance > 10000) %>%
  filter(drivedistance < 15000)


# filtering out higest contributing weird precincts
bothdistances.filter <- bothdistances2 %>%
  filter(precID != "LEE46") %>%
  filter(precID != "PAL1244") %>%
  filter(precID != "PAL1242") %>%
  filter(precID != "STL16") %>%
  filter(precID != "PAS108") %>%
  filter(precID != "LE03409") %>%
  filter(precID != "BROW020")


#correlation
cor(na.omit(bothdistances.filter[,4:5]))


#prettyplot take 2
cors <- ggplot(data = bothdistances.filter, 
               mapping = aes(x = havermiles, y = drivemiles)) + 
  geom_point(alpha = 0.1, fill = NA) + 
  labs(title = "Haversine and Driving Distances", 
       x = "Haversine Distances (miles)", 
       y = "Driving Distances (miles)")
cors + theme_classic()
