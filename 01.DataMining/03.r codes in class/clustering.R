

# installling packages ----------------------------------------------------
library(NbClust)
library(factoextra)

df <- scale(USArrests)
summary(df)
head(df)

USArrests.dist <- dist(scale(USArrests), method="manhattan")


# hierarchical clustering -------------------------------------------------

USArrests.hclust <- hclust(USArrests.dist, method="complete")
names(USArrests.hclust)
plot(USArrests.hclust)


# drawing dendrogram ------------------------------------------------------
library(RColorBrewer)
library(factoextra)
fviz_dend(USArrests.hclust, k=4, cex=-.5,
          k_colors=brewer.pal(4,"Set1"),
          color_labels_by_k = TRUE,
          rect=TRUE)
fviz_dend(USArrests.hclust, k=4, cex=-.5,
          k_colors=brewer.pal(4,"Dark2"),
          color_labels_by_k = TRUE,
          rect=TRUE)
fviz_dend(USArrests.hclust, k=4, cex=-.5,
          k_colors=rainbow(4),
          color_labels_by_k = TRUE,
          rect=TRUE)



# cutree ------------------------------------------------------------------

hc.class <- cutree(USArrests.hclust, k=4)
hc.class
which(hc.class==1)

plot(USArrests, col=hc.class, pch=hc.class)


# Selecting group number of clusterings 
## Elbow mehod  -----------------------------------------------------------------
fviz_nbclust(df, kmeans, method = 'wss')   +
geom_vline(xintercept = 4, linetype = 2)+
labs(subtitle = 'Elbow method')

## Silhouette method ----------------------------------------------------------------------
fviz_nbclust(df, kmeans, method ='silhouette') +# This stays 2clusters are optimal cluster. 
labs(subtitle = 'Silhouette method') 

# Gap stats ----------------------------------------------------------------
nboot =  50  # to keep the function speedy. recommended value: 
# nboot= 500 for your analysis. Use verbose = FALSE to hide computing progression.
set.seed(123)  # for reproduction. we set seeds. 
fviz_nbclust(df, kmeans, 
             nstart = 25, method = 'gap_stat',
             nboot = 50)+
  labs(subtitle = 'Gap statistic method') 
#bar represents   1 standard deviation 


# determine which clusterign method is good  ------------------------------------------------------------
library(NbClust)
nb <- NbClust(df, distance = "euclidean", 
              min.nc = 2, max.nc = 10, method = "kmeans")

nbc <- fviz_nbclust(nb)
nbc



# k-means -----------------------------------------------------------------

m=20
set.seed(10)
wss=numeric(m)
for(i in 1:m){
  US.km <- kmeans(scale(USArrests), centers=i)
  wss[i] = US.km$tot.withinss
}

plot(wss, type='b', 
     xlab='K ( number of clusters)',
     ylab='Total within sum of squares')

US.km.4 <- kmeans(scale(USArrests), centers=4)

plot(USArrests, col=US.km.4$cluster, pch=US.km.4$cluster)

US.pca <- prcomp(USArrests, center=TRUE, scale=TRUE)$x[,c(1,2)]
plot(US.pca, 
     col=US.km.4$cluster, 
     pch=US.km.4$cluster)


# mixture model -----------------------------------------------------------
library(mclust)
fit <- Mclust(USArrests)
plot(fit, what='classification')
plot(fit)

fit$modelName
fit$BIC
fit$classification

plot(USArrests, col=fit$classification)
fitBIC <- mclustBIC(USArrests)
plot(fitBIC)
fitModel <- mclustModel(USArrests, fitBIC, g=1:5)
fitModel
