
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

plot(phdata$FRUCTO,phdata$Body_weight)

#Breed abbreviation and name table
Breed<- data.matrix(table(data_raw@phdata$Breed)) #how many dog breeds we have

Breed_name <- c("Boxer","Belgian shepher", "Cavalier King Charles spaniel", "Dachshund", "Doberman pinsche", 
                "Finnish lapphund", "German shepherd", "Labrador retrieve", "Newfoundland") #vector of the names

Sex <- data.matrix(table(data_raw@phdata$sex))

Breed_table <- data.matrix(cbind(Breed,Breed_name)) #table of the breeds

