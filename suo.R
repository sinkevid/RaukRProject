# Dovile's functions & data analysis

## used for dogs phenotype summary
funm <- function(x){mean(x, na.rm = T)}
funsd <- function(y){sd(y, na.rm = T)}


# general info on the participants of the study
length(unique(data_raw@phdata$Country)) ## data from 5 countries
length(unique(data_raw@phdata$Breed)) ## data covers 9 dog breeds


# country with the most dogs
res1 <- data_raw@phdata %>% 
  group_by(Country) %>%
  summarize(total_dogs = sum(n())) %>%
  arrange(desc(total_dogs))
print(res1)

ggplot(data= res1, aes(x = "", y = total_dogs, fill = Country))+
  geom_bar(width = 1, stat = "identity")+
  coord_polar("y", start=0)
  


# summary of dog breeds per country
data_raw@phdata %>%
  group_by(Breed, Country) %>%
  summarize(n = n()) %>%
  arrange(desc(n)) %>%
  arrange(desc(Country))


# top1 breed per country
data_raw@phdata %>%
  group_by(Breed, Country) %>%
  summarize(n = n()) %>%
  group_by(Country)%>%
  slice(which.max(n))
  
  




