#From Marcin: HapMap Autoencoder

#autoencoder as a replacement/complement to MDS/PCA used for visualising population structure in genetics

head(summary(data_raw@gtdata))#summary of the genetic data

# Compute genomic kinship-based distances
gkin <- ibs(data_raw, weight = 'freq')

head(gkin)

dm <- as.dist(.5 - gkin) # Normalize it

#PCA

# PCA
pca <- prcomp(dm) #Principal components analysis

library(devtools)
install_github("vqv/ggbiplot") #to get the ggbiplot
library(ggbiplot)

g <- ggbiplot(pca, obs.scale = 1, var.scale = 1,
              groups = data_raw@phdata$Breed, ellipse = F, #using breed as a group
              circle = TRUE, var.axes = F) +
  ggtitle("PCA")
g <- g + scale_color_discrete(name = '')
g <- g + theme()

print(g)

#MDS
# MDS
ibs <- as.data.frame(cmdscale(dm))

ibs <- cbind(ibs, pop = data_raw@phdata$Breed) #same as group at PCA

z <-ggplot(ibs, mapping = aes(x=V1, y=V2, col=pop)) + 
  geom_point() +
  ggtitle("MDS") +
  theme(legend.position = 'none') #removing legend

print(z)

library(gridExtra)
library(grid)
library(ggplot2)
library(lattice)
library(ggpubr)

#arranging the two grids side by side with one legend
ggarrange(g,z, ncol=2, nrow=1, common.legend = TRUE, legend="bottom")

#Autoencoder
#we define model parameters: loss function set to the mean squared error and activation layer set to ReLU.

loss_fn <- 'binary_crossentropy'
act <- 'relu'

# Input data is first normalized so that: 
#* homozygotes AA are set to 1 
#* heterozygotes to 0.5 * homozygotes aa to 0 Next, 
#the data are split into the validation (20%) and the training (80%) set.



