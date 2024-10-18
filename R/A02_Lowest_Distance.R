library(ggplot2)
library(dplyr)

# Function to calculate the shortest distance from a point (x0, y0) to a line segment (x1, y1) -> (x2, y2)
calculate_distance <- function(x0, y0, x1, y1, x2, y2) {
  # Vectors for the points
  A <- c(x1, y1)
  B <- c(x2, y2)
  P <- c(x0, y0)

  # Vectors AB and AP
  AB <- B - A
  AP <- P - A

  # Projection of AP onto AB
  proj <- sum(AP * AB) / sum(AB * AB)

  # Clamping the projection to ensure it's between 0 and 1, i.e., within the segment
  proj <- max(0, min(1, proj))

  # Nearest point on the segment
  nearest_point <- A + proj * AB

  # Euclidean distance between the point (x0, y0) and the nearest point on the segment
  distance <- sqrt(sum((P - nearest_point) ^ 2))

  return(list(distance = distance, nearest_point = nearest_point))
}

# Coordinates of the point and the line segment
x0 <- 5  # Point (x0, y0)
y0 <- 7
x1 <- 2  # Line segment (x1, y1) -> (x2, y2)
y1 <- 9
x2 <- 8
y2 <- 10

# Calculate the distance and the nearest point
result <- calculate_distance(x0, y0, x1, y1, x2, y2)
distance <- result$distance
nearest_point <- result$nearest_point

# Create a data frame for ggplot2
df <- tibble(x = c(x1, x2), y = c(y1, y2))

# Plot with ggplot2
ggplot() +
  # Line segment
  geom_segment(aes(
    x = x1,
    y = y1,
    xend = x2,
    yend = y2
  ), color = "black") +
  # Point
  geom_point(aes(x = x0, y = y0), color = "black") +
  # Dashed line representing the shortest distance
  geom_segment(
    aes(
      x = x0,
      y = y0,
      xend = nearest_point[1],
      yend = nearest_point[2]
    ),
    color = "red",
    linetype = "dotted"
  ) +
  # Nearest point on the segment
  geom_point(aes(x = nearest_point[1], y = nearest_point[2]), color = "black") +
  # Add the label for the distance
  geom_text(aes(
    x = (x0 + nearest_point[1]) / 2,
    y = (y0 + nearest_point[2]) / 2,
    label = paste0("Distance: ", round(distance, 2))
  ),
  color = "red",
  vjust = -1) +
  # Titles and axis adjustments
  labs(title = "Distance from a point to a line segment", x = "X axis", y = "Y axis") +
  theme_minimal()
