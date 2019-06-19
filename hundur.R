#PCA plot 

load("geno_data_raw.rdat")


qc0 <- check.marker(data_raw, call = 0.95, perid.call=0.95,
                    maf=1e-06, p.lev=1e-08, ibs.exclude="lower")


data.qc0 <- data_raw[qc0$idok, qc0$snpok]

autosomalMarkerNames <- snpnames(data.qc0)[chromosome(data.qc0) != 39]
# Compute genomic kinship matrix
data.qc0.gkin <- ibs(data.qc0[, autosomalMarkerNames], weight = "freq")
# Transform it to a distance matrix
data.qc0.dist <- as.dist(0.5 - data.qc0.gkin)
# Perform multidimensional scaling to display individuals on a 2D
# plot preserving genomic distances between them
data.qc0.mds <- cmdscale(data.qc0.dist)
# Plot the result
test <- data.frame(Dim_1 = data.qc0.mds[,1], Dim_2 = data.qc0.mds[,2], Breed= data_raw@phdata$Breed)
ggplot(test, aes(Dim_1, Dim_2, color=Breed)) + geom_point()

       