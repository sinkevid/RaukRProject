install.packages("tidyverse", "ggmap", "ggplot2", "mapproj", "purrr")

library(tidyverse)
library(ggmap)
library(ggplot2)
library(maps)
library(mapproj)


europe.map <- get_stamenmap(c(left = -25.410724, bottom = 35.479777,
                              right = 35.618354, top = 71.280356), zoom = 5)

ggmap(europe.map)

sweden.map <- get_stamenmap(c(left = -25.410724, bottom = 35.479777,
                                            right = 35.618354, top = 71.280356), zoom = 5)

library(dplyr)
data_raw@phdata %>%
  group_by(Breed, Country) %>%
  summarize(n = n()) %>%
  arrange(desc(n)) %>%
  arrange(desc(Country))
