# Packages ----------------------------------------------------------------


library(tidyverse)
library(dplyr)
library(ggrepel)



# Data --------------------------------------------------------------------


iris_data <- scale(iris[, -5])  # Remove the species column and scale the data



# Calculate the within-cluster sum of squares (WCSS) ----------------------


Clusters <- 2:20


Clusters |>
  map_dfr(\(k) {
    set.seed(123)
    kmeans_result <- kmeans(iris_data, centers = k, nstart = 50)
    wcss <- kmeans_result$tot.withinss

    tibble(Clusters = k, WCSS = wcss)


  }) ->
  cluster_data



# Distance function -------------------------------------------------------


calculate_distance <- function(Clusters, WCSS, x1, y1, x2, y2) {
  # Vectors for the points

  A_coef <- (y2 - y1)
  B_coef <- (x1 - x2)
  C_coef <- (x2 * y1 - x1 * y2)


  # Distance
  distance <- abs(A_coef * Clusters + B_coef * WCSS + C_coef) / sqrt(A_coef ^
                                                                       2 + B_coef ^ 2)


  # Calculating the coordinates of the nearest point on the line
  # Using projection to find the nearest point on the line
  t <- ((Clusters - x1) * (x2 - x1) + (WCSS - y1) * (y2 - y1)) / ((x2 - x1) ^
                                                                    2 + (y2 - y1) ^ 2)

  # Clamping t to ensure it's within the segment
  t <- max(0, min(1, t))

  # Nearest point coordinates
  nearest_point <- c(x1 + t * (x2 - x1), y1 + t * (y2 - y1))


  if (distance == 0) {
    distance <- NA
    nearest_point <- c(NA, NA)
  }

  return(
    tibble(
      Clusters = Clusters,
      WCSS = WCSS,
      Distance = distance,
      Nearest_X = nearest_point[1],
      Nearest_Y = nearest_point[2]
    )
  )

}



# Scale function ----------------------------------------------------------


scale_to_custom_range <- function(x, min = 0, max = 1) {
  if (length(x) == 0) {
    stop("Input vector is empty.")
  }

  # Calculate the min and max of the input vector
  old_min <- min(x)
  old_max <- max(x)

  # Scale using the specified range
  scaled_x <- min + (x - old_min) * (max - min) / (old_max - old_min)

  return(scaled_x)
}



# Data with distances -----------------------------------------------------


cluster_data |>
 #  mutate_at(vars(WCSS), \(x) scale_to_custom_range(x, min = 0, max = 20)) |>
  pmap_dfr(\(Clusters, WCSS) {
    calculate_distance(
      Clusters = Clusters,
      WCSS = WCSS,
      x1 = first(cluster_data$Clusters),
      y1 = first(cluster_data$WCSS),
      x2 = last(cluster_data$Clusters),
      y2 = last(cluster_data$WCSS)
    )

  }) ->
  df


optimal_clusters <- which.max(df$Distance) + 2



# Graphic -----------------------------------------------------------------


df |>
  ggplot(aes(x = Clusters, y = WCSS)) +
  geom_point(size = 3) +
  geom_line(linewidth = 1) +
  # Plot the blue line using geom_segment
  geom_segment(
    x = first(df$Clusters),
    y = first(df$WCSS),
    xend = last(df$Clusters),
    yend = last(df$WCSS),
    color = "blue",
    linetype = "dashed"
  ) +
  # Red dotted lines representing the perpendicular distances to the blue line
  geom_segment(
    aes(
      x = Clusters,
      y = WCSS,
      xend = Nearest_X,
      yend = Nearest_Y
    ),
    linetype = "dotted",
    color = "red",
    na.rm = TRUE
  ) +
  # Labels for the calculated distances using geom_text_repel to avoid overlaps
  geom_text_repel(
    aes(
      x = (Clusters + Nearest_X) / 2,
      y = (WCSS + Nearest_Y) / 2,
      label = round(Distance, 2)
    ),
    color = "black",
    size = 3,
    na.rm = TRUE
  ) +
  # Vertical line indicating the optimal number of clusters
  geom_vline(
    xintercept = optimal_clusters,
    color = "green",
    linetype = "solid",
    linewidth = 1
  ) +
  labs(title = "Elbow Method for Optimal Clusters with Perpendicular Distances",
       x = "Number of Clusters",
       y = "Within-Cluster Sum of Squares (WCSS)") +
  theme_minimal() +
  theme(plot.title = element_text(hjust = 0.5))
