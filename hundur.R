#PCA plot 
library(ggplot2)
library(GenABEL)
# Read in the data
load("geno_data_raw.rdat")
# Find marker for PCA analysis
# qc0 <- check.marker(data_raw, call = 0.95, perid.call=0.95,
#                     maf=1e-06, p.lev=1e-08, ibs.exclude="lower")
# # Save output from check.marker
# data.qc0 <- data_raw[qc0$idok, qc0$snpok]
# 
# # Remove chromosome 39 (X) from dataset
# autosomalMarkerNames <- snpnames(data.qc0)[chromosome(data.qc0) != 39]
# # Compute genomic kinship matrix
# data.qc0.gkin <- ibs(data.qc0[, autosomalMarkerNames], weight = "freq")
# # Transform it to a distance matrix
# data.qc0.dist <- as.dist(0.5 - data.qc0.gkin)
# # Perform multidimensional scaling to display individuals on a 2D
# # plot preserving genomic distances between them
# data.qc0.mds <- cmdscale(data.qc0.dist)
# # Save the results so no need to run every single time
# pca_result <- save(data.qc0.mds, file="pca_results.rdat")

# Plot the result
load("pca_results.rdat")
df_data_raw <- data.frame(Dim_1 = data.qc0.mds[,1], Dim_2 = data.qc0.mds[,2], Breed= data_raw@phdata$Breed)
ggplot(df_data_raw, aes(Dim_1, Dim_2, color=Breed)) + 
  geom_point(alpha = 0.8) + 
  labs(title="Principal Component Analysis",
       subtitle="RaukR project 2019") +
  theme_bw() +
  scale_color_manual(values = c("black", "dodgerblue4", "bisque4", "darkgreen","yellow3", "darkgoldenrod3", "darkorchid4", "darkred", "grey")) 
       