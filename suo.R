# Dovile's funcitons

## used for dogs phenotype summary
funm <- function(x){mean(x, na.rm = T)}
funsd <- function(y){sd(y, na.rm = T)}

length(unique(data_raw@phdata$Country)) ## data from 5 countries
length(unique(data_raw@phdata$Breed)) ## data covers 9 dog breeds



# country with the most dogs
data_raw@phdata %>% 
  group_by(Country) %>%
  summarize(total_dogs = sum(n())) %>%
  arrange(desc(total_dogs))


# summary of dog breeds per country
data_raw@phdata %>%
  group_by(Breed, Country) %>%
  summarize(n = n()) %>%
  arrange(desc(n)) %>%
  arrange(desc(Country))
head(df)



