#Plot weight for each breed in a ggplot
df_data_raw <- data.frame(sex =data_raw@phdata$sex, weight =data_raw@phdata$Body_weight, Bread = data_raw@phdata$Breed)

ggplot(df_data_raw,aes(x=Breed, y=weight)) +
  geom_boxplot()

