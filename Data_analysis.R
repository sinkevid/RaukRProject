
load("geno_data_raw.rdat")

#Data is S4 format
data_raw@phdata$Breed #just the breeds

data_raw@gtdata@idnames
data_raw@gtdata@coding

library(tidyverse)

phdata <- data_raw@phdata #table from the phdata file

summary(data_raw@gtdata)#summary of the genetic data

#playing with plotting.. nothing real stuff :)
plot(p$Chromosome)

plot(phdata$Body_weight)