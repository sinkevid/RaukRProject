
# general info on the participants of the study
length(unique(data_raw@phdata$Country)) ## data from 5 countries
length(unique(data_raw@phdata$Breed)) ## data covers 9 dog breeds

library(tidyverse)

# number of dogs per breed (male, female)
dogs <- data_raw@phdata %>%
  group_by(Breed, sex) %>%
  summarize(n = n()) %>%
  group_by(Breed) %>%
  spread(key="sex", value = "n" ) %>%
  mutate_all(~replace(., is.na(.), 0))

dogs$Breed_name <- c("Boxer",
                     "Belgian Shepperd",
                     "Cavalier King Charles spaniel",
                     "Dachshund", 
                     "Doberman pinscher",
                     "Finnish lapphund",
                     "German Shepperd",
                     "Labrador retriever", 
                     "Newfoundland")

colnames(dogs)<- c("Abbreviation", "Female", "Male", "Breed")
dogs[,c("Breed", "Abbreviation", "Female", "Male")]

# Info on dogs per country
country_info <- data_raw@phdata %>% 
  group_by(Country) %>%
  summarize(total_dogs = sum(n())) %>%
  arrange(desc(total_dogs))

country_info$fullname <- c("Finland", "Belgium", "Sweden", "France", "Denmark") 
colnames(country_info)<- c("Abbreviation", "Total number of dogs", "Country")
country_info[,c("Country", "Abbreviation", "Total number of dogs")]

# Table of mean dog weight and age
# mean ans sd functions
funm <- function(x){mean(x, na.rm = T)}

#table
dogsWH <- data_raw@phdata %>%
  group_by(Breed, sex) %>%
  summarize(n = n(),
            mean.Age = funm(Age), 
            mean.Weight = funm(Body_weight)) 

dogsWH$sex[dogsWH$sex == 0]<- "female"
dogsWH$sex[dogsWH$sex == 1]<- "male"

print(dogsWH)



  

