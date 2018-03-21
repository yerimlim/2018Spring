
# install.packages("NbClust")
# install.packages("factoextra")
library(NbClust)
library(factoextra)

df <- scale(USArrests)
summary(df)
head(df)


# Elbow mehod  -----------------------------------------------------------------
fviz_nbclust(df, kmeans, method = 'wss')   +
geom_vline(xintercept = 4, linetype = 2)+
labs(subtitle = 'Elbow method')

# Silhouette method ----------------------------------------------------------------------
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
library("NbClust")
nb <- NbClust(df, distance = "euclidean", 
              min.nc = 2, max.nc = 10, method = "kmeans")

nbc <- fviz_nbclust(nb)
nbc
