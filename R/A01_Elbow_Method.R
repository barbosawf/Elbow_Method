# Required libraries
library(ggplot2)
library(cluster)
library(tibble)
library(ggrepel)

# Function to calculate the optimal number of clusters and vertical distances
optimal_number_of_clusters <- function(wcss, x2) {
  x1 <- 2
  y1 <- wcss[1]
  y2 <- wcss[length(wcss)]

  slope <- (y2 - y1) / (x2 - x1)  # Slope of the blue line
  intercept <- y1 - slope * x1    # Intercept of the blue line

  distances <- c()
  intersection_y <- c()

  for (i in 2:(length(wcss) - 1)) {
    # Exclude first and last points
    x0 <- i + 1
    y0 <- wcss[i]

    # Calculate y-value on the blue line at the same x-coordinate (vertical distance)
    y_line <- slope * x0 + intercept
    intersection_y <- c(intersection_y, y_line)

    # Calculate vertical distance
    distances <- c(distances, abs(y0 - y_line))
  }

  return(
    list(
      optimal_k = which.max(distances) + 2,
      distances = distances,
      intersection_y = intersection_y
    )
  )
}

# Preparing the iris dataset
iris_data <- scale(iris[, -5])  # Remove the species column and scale the data

# Calculate the within-cluster sum of squares (WCSS) for different numbers of clusters
wcss <- c()
for (k in 2:20) {
  set.seed(123)
  kmeans_result <- kmeans(iris_data, centers = k, nstart = 50)
  wcss <- c(wcss, kmeans_result$tot.withinss)
}

# Apply the optimal number of clusters function with a flexible x2 parameter
x2 <- 20  # This is the final point (adjustable as needed)
result <- optimal_number_of_clusters(wcss, x2)
optimal_k <- result$optimal_k
distances <- result$distances
intersection_y <- result$intersection_y

# Prepare data for plotting (excluding first and last points)
cluster_data <- tibble(
  Clusters = 2:20,
  WCSS = wcss,
  Intersection_Y = c(NA, intersection_y, NA),
  Distances = c(NA, distances, NA)
)


# Plot the elbow method with ggplot2
ggplot(cluster_data, aes(x = Clusters, y = WCSS)) +
  geom_point(size = 3) +
  geom_line(size = 1) +
  # Plot the blue line using geom_segment without generating warnings
  geom_segment(
    x = 2,
    y = wcss[1],
    xend = x2,
    yend = wcss[length(wcss)],
    color = "blue",
    linetype = "dashed"
  ) +
  # Red vertical lines representing the vertical distances to the blue line
  geom_segment(
    aes(xend = Clusters, yend = Intersection_Y),
    linetype = "dotted",
    color = "red",
    na.rm = TRUE
  ) +  # Add na.rm = TRUE
  # Labels for the calculated distances using geom_text_repel to avoid overlaps
  geom_text_repel(
    aes(
      x = Clusters,
      y = (WCSS + Intersection_Y) / 2,
      label = round(Distances, 2)
    ),
    color = "black",
    size = 3,
    na.rm = TRUE
  ) +  # Add na.rm = TRUE
  # Vertical line indicating the optimal number of clusters
  geom_vline(
    xintercept = optimal_k,
    color = "green",
    linetype = "solid",
    size = 1
  ) +
  labs(title = "Elbow Method for Optimal Clusters with Vertical Distances", x = "Number of Clusters", y = "Within-Cluster Sum of Squares (WCSS)") +
  theme_minimal() +
  theme(plot.title = element_text(hjust = 0.5))
