#From Marcin: HapMap Autoencoder

#autoencoder as a replacement/complement to MDS/PCA used for visualising population structure in genetics

head(summary(data_raw@gtdata))#summary of the genetic data

#Getting rid off the chromosome 39 (X chromosome)
#data_autosomal <- data_raw[,data_raw@gtdata@chromosome != "39"]

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

color_9 <- c("black", "dodgerblue4", "bisque4", "darkgreen","yellow3", "darkgoldenrod3", "darkorchid4", "darkred", "grey")

g <- ggbiplot(pca, obs.scale = 1, var.scale = 1,
              groups = data_raw@phdata$Breed, ellipse = F, #using breed as a group
              circle = TRUE, var.axes = F) +
  ggtitle("PCA")+
  scale_color_manual(values = color_9)

#g <- g + scale_color_discrete(name = '')
#g <- g + theme()

#PCA without X chromosome
#ga <- ggbiplot(pca, obs.scale = 1, var.scale = 1,
#              groups = data_autosomal@phdata$Breed, ellipse = F, #using breed as a group
#              circle = TRUE, var.axes = F) +
#  ggtitle("PCA autosomal") +
#  scale_color_manual(values = color_9)
#ga <- ga + scale_color_discrete(name = '')
#ga <- ga + theme()

#MDS
# MDS
ibs <- as.data.frame(cmdscale(dm))

ibs <- cbind(ibs, pop = data_raw@phdata$Breed) #same as group at PCA

z <-ggplot(ibs, mapping = aes(x=V1, y=V2, col=pop)) + 
  geom_point() +
  ggtitle("MDS") +
  scale_color_manual(values = color_9) +
  theme(legend.position = 'none') #removing legend

#MDS from autosomal
#ibsa <- as.data.frame(cmdscale(dm))
#ibsa <- cbind(ibs, pop = data_autosomal@phdata$Breed) #same as group at PCA
#za <-ggplot(ibsa, mapping = aes(x=V1, y=V2, col=pop)) + 
#  geom_point() +
#  ggtitle("MDS autosomal") +
#  scale_color_manual(values = color_9)+
#  theme(legend.position = 'none') #removing legend


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

# Encode genotypes
geno_matrix <- as.double(data_raw)
geno_tensor <- geno_matrix/2 #keras::to_categorical(geno_matrix)

# Split into the training and the validation set
n_rows <- dim(geno_tensor)[1]
train_idx <- sample(1:n_rows, size = 0.8 * n_rows, replace = F)
train_data <- geno_tensor[train_idx, ]
valid_data <- geno_tensor[-train_idx, ]


#Autoencoder
#we define model parameters: loss function set to the mean squared error and activation layer set to ReLU.

loss_fn <- 'binary_crossentropy'
act <- 'relu'

# Input data is first normalized so that: 
#* homozygotes AA are set to 1 
#* heterozygotes to 0.5 * homozygotes aa to 0 Next, 
#the data are split into the validation (20%) and the training (80%) set.

# Encode genotypes, with sample size 2000 (randomly chosen), not the whole data
set.seed(12345)
geno_matrix <- as.double(data_raw[, sample(1:nsnps(data_raw), size = 2000, replace = F)])
geno_tensor <- geno_matrix/2 #keras::to_categorical(geno_matrix)

# Split into the training and the validation set
n_rows <- dim(geno_tensor)[1]
train_idx <- sample(1:n_rows, size = 0.8 * n_rows, replace = F)
train_data <- geno_tensor[train_idx, ]
valid_data <- geno_tensor[-train_idx, ]

#Define the architecture

library(tensorflow)
library(keras)
#Did not work for me I had to run these commands
#"devtools::install_github("rstudio/tensorflow")
#devtools::install_github("rstudio/keras")"
#tensorflow::install_tensorflow()
#tensorflow::tf_config()

input_layer <- layer_input(shape = dim(train_data)[2])

encoder <-
  input_layer %>%
  layer_dense(units = 1500, activation = act) %>%
  layer_batch_normalization() %>%
  layer_dropout(rate = 0.2) %>%
  layer_dense(units = 500, activation = act) %>%
  layer_dropout(rate = 0.1) %>%
  layer_dense(units = 25, activation = act) %>%
  layer_dense(units = 2) # bottleneck

decoder <-
  encoder %>%
  layer_dense(units = 25, activation = act) %>%
  layer_dropout(rate = 0.2) %>%
  layer_dense(units = 500, activation = act) %>%
  layer_dropout(rate = 0.1) %>%
  layer_dense(units = 1500, activation = act) %>%
  layer_dense(units = dim(train_data)[2], activation = "sigmoid")

autoencoder_model <- keras_model(inputs = input_layer, outputs = decoder)

autoencoder_model %>% compile(
  loss = loss_fn,
  optimizer = 'adam',
  metrics = c('accuracy')
)
summary(autoencoder_model)

#Training phase
#Now the model is trained, loss and accuracy are evaluated on both the training and the external validation set.

#This run takes some time
history <- autoencoder_model %>% fit(
  x = train_data,
  y = train_data,
  epochs = 120,
  shuffle = T,
  batch_size = 256,
  validation_data = list(valid_data, valid_data)
)

plot(history)

#visualize the embeddings
# difference between the original data and the reconstructed points

reconstructed_points <-
  keras::predict_on_batch(autoencoder_model, x = train_data)

delta <- abs(train_data - reconstructed_points)

heatmap(delta[1:100, 1:100], Rowv = NA, Colv = NA,
        col=heat.colors(5), scale = 'none')

#Building the encoder
autoencoder_weights <- autoencoder_model %>% keras::get_weights()
keras::save_model_weights_hdf5(object = autoencoder_model,
                               filepath = './autoencoder_weights.hdf5',
                               overwrite = TRUE)
encoder_model <- keras_model(inputs = input_layer, outputs = encoder)

#Gives an error: You are trying to load a weight file containing 9 layers into a model with 5 layers. 
encoder_model %>% keras::load_model_weights_hdf5(filepath = "./autoencoder_weights.hdf5",
                                                 skip_mismatch = TRUE,
                                                 by_name = F)
encoder_model %>% compile(
  loss = loss_fn,
  optimizer = 'adam',
  metrics = c('accuracy')
)

#Embedding original data
embeded_points <-
  encoder_model %>%
  keras::predict_on_batch(x = geno_tensor)

#Final results
embedded <- data.frame(embeded_points[,1:2],
                       pop = data_autosomal@phdata$Breed,
                       type='emb')
mds <- cbind(ibs, type='mds')
colnames(mds) <- c('x', 'y', 'type')
colnames(embedded) <- c('x', 'y', 'type')
dat <- rbind(embedded, mds)
dat %>% ggplot(mapping = aes(x=x, y=y, col=pop)) +
  geom_point() +
  facet_wrap(~type, scales = "free")
