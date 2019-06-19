#Plot weight for each breed in a ggplot

data_raw@phdata %>% 
  select(Breed, Body_weight) %>% 
  na.omit() %>% 
  ggplot(aes(x=Breed, y=Body_weight, fill=Breed)) +
  geom_boxplot() +
  geom_jitter(width = 0.2, alpha=0.3) +
  labs(title="Weight distribution for all breeds",
       subtitle="RaukR project 2019",
       caption="Sources: PLoS ONE, 2015")+
  theme_bw() + 
  theme(legend.position = "none")


