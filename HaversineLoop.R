library(c(tidyverse, data.table, parallel, rlist, geosphere, sf, tigris))

pplaces <- read.csv("webpollplaces")  ##because we already got the google data downloaded

church <- pplaces %>% select (longitude, latitude) %>% slice(1) # taking first row

others <- pplaces %>% select (longitude, latitude)

results <- distHaversine(p1 = church, p2 = others)

results.tib <- as_tibble(results)

results.tib$destination <- pplaces$pollname

