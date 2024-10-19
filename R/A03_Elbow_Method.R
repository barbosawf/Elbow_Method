# Packages ----------------------------------------------------------------

library(factoextra)
library(NbClust)
library(fpc)
library(clValid)



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


# Calinski — Harabasz method

fviz_ch <- function(data) {
  ch <- c()
  for (i in 2:10) {
    km <- kmeans(data, i) # perform clustering
    ch[i] <- calinhara(data, # data
                       km$cluster, # cluster assignments
                       cn=max(km$cluster) # total cluster number
    )
  }
  ch <-ch[2:10]
  k <- 2:10
  plot(
    k,
    ch,
    xlab =  "Cluster number k",
    ylab = "Caliński - Harabasz Score",
    main = "Caliński - Harabasz Plot",
    cex.main = 1,
    col = "dodgerblue1",
    cex = 0.9 ,
    lty = 1 ,
    type = "o" ,
    lwd = 1,
    pch = 4,
    bty = "l",
    las = 1,
    cex.axis = 0.8,
    tcl  = -0.2
  )
  abline(
    v = which(ch == max(ch)) + 1,
    lwd = 1,
    col = "red",
    lty = "dashed"
  )
}

fviz_ch(df)


# Davies-Bouldin method
fviz_db <- function(data) {
  k <- c(2:10)
  nb <- NbClust(
    data,
    min.nc = 2,
    max.nc = 10,
    index = "db",
    method = "kmeans"
  )
  db <- as.vector(nb$All.index)
  plot(
    k,
    db,
    xlab =  "Cluster number k",
    ylab = "Davies-Bouldin Score",
    main = "Davies-Bouldin Plot",
    cex.main = 1,
    col = "dodgerblue1",
    cex = 0.9 ,
    lty = 1 ,
    type = "o" ,
    lwd = 1,
    pch = 4,
    bty = "l",
    las = 1,
    cex.axis = 0.8,
    tcl  = -0.2
  )
  abline(
    v = which(db == min(db)) + 1,
    lwd = 1,
    col = "red",
    lty = "dashed"
  )
}


fviz_db(df)


# Dunn method


fviz_dunn <- function(data) {
  k <- c(2:10)
  dunnin <- c()
  for (i in 2:10) {
    dunnin[i] <- dunn(distance = dist(data),
                      clusters = kmeans(data, i)$cluster)
  }
  dunnin <- dunnin[2:10]
  plot(
    k,
    dunnin,
    xlab =  "Cluster number k",
    ylab = "Dunn Index",
    main = "Dunn Plot",
    cex.main = 1,
    col = "dodgerblue1",
    cex = 0.9 ,
    lty = 1 ,
    type = "o" ,
    lwd = 1,
    pch = 4,
    bty = "l",
    las = 1,
    cex.axis = 0.8,
    tcl  = -0.2
  )
  abline(
    v = which(dunnin == max(dunnin)) + 1,
    lwd = 1,
    col = "red",
    lty = "dashed"
  )
}

fviz_dunn(df)



# Useful links ------------------------------------------------------------


# https://www.datanovia.com/en/lessons/determining-the-optimal-number-of-clusters-3-must-know-methods/

# https://www.datanovia.com/en/lessons/k-means-clustering-in-r-algorith-and-practical-examples/

# https://medium.com/@ozturkfemre/unsupervised-learning-determination-of-cluster-number-be8842cdb11
