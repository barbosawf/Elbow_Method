

# Packages ----------------------------------------------------------------

library(factoextra)
library(NbClust)


# Data --------------------------------------------------------------------

df <- scale(USArrests)
head(df)


# Elbow Method ------------------------------------------------------------

# Elbow method
fviz_nbclust(df,
             kmeans,
             method = "wss",
             nstart = 25) +
  geom_vline(xintercept = 4, linetype = 2) +
  labs(subtitle = "Elbow method")

# Silhouette method
fviz_nbclust(df, kmeans, method = "silhouette", nstart = 25) +
  labs(subtitle = "Silhouette method")

# Gap statistic
# nboot = 50 to keep the function speedy.
# recommended value: nboot= 500 for your analysis.
# Use verbose = FALSE to hide computing progression.
set.seed(123)
fviz_nbclust(df,
             kmeans,
             nstart = 25,
             method = "gap_stat",
             nboot = 50) +
  labs(subtitle = "Gap statistic method")



# Useful links ------------------------------------------------------------


# https://www.datanovia.com/en/lessons/determining-the-optimal-number-of-clusters-3-must-know-methods/

# https://www.datanovia.com/en/lessons/k-means-clustering-in-r-algorith-and-practical-examples/
