library(dplyr)
library(ggplot2)

haverdistances <- read.csv("2018VoterHavDist.csv")


bothdistances <- voterosrmdistances2018 %>% 
  left_join(id.havdist, by = "X") %>%
  select(drivedistance, haverdistance, precID, geopollrating)

bothdistances <- fdd2018.reasonable %>% 
  select(drivedistance, haverdistance, precID)

cor(na.omit(bothdistances[,1:2]))

#plot(data = bothdistances, drivedistance ~ haverdistance)


cor(na.omit(bothdistances.filter[,1:2]))


cors <- ggplot(data = bothdistances, 
               mapping = aes(x = haverdistance, y = drivedistance)) + 
  geom_point(alpha = 0.1, fill = NA) + 
  labs(title = "Haversine and Driving Distances", 
       x = "Haversine Distances (meters)", 
       y = "Driving Distances (meters)")
cors + theme_classic() #+ geom_hline(yintercept = 10000) + geom_vline(xintercept = 3000)





weird.data <- fdd2018.reasonable %>% 
  filter(haverdistance < 3150) %>%
  filter(drivedistance > 10000) %>%
  filter(drivedistance < 15000)

weird.data2 <- bothdistances %>%
  filter(precID != "LEE46") %>%
  filter(haverdistance < 2000) %>%
  filter(drivedistance > 10000) %>%
  filter(drivedistance < 15000)



bothdistances.filter <- bothdistances %>%
  filter(precID != "LEE46") %>%
  filter(precID != "PAL1244") %>%
  filter(precID != "PAL1242") %>%
  filter(precID != "STL16") %>%
  filter(precID != "PAS108") %>%
  filter(precID != "LE03409") %>%
  filter(precID != "BROW020")


cors <- ggplot(data = bothdistances.filter, 
               mapping = aes(x = haverdistance, y = drivedistance)) + 
  geom_point(alpha = 0.1, fill = NA) + 
  labs(title = "Haversine and Driving Distances", 
       x = "Haversine Distances (meters)", 
       y = "Driving Distances (meters)")
cors + theme_classic()
