
library(ggplot2)
library(sf) 
library(dplyr)

library(raster)
# Load the data
clim <- read.csv("https://userpage.fu-berlin.de/soga/data/raw-data/Climfrance.csv", sep = ";")

# Inspect the structure of the dataset
str(clim)
head(clim)
clim
# Fix issues with the "altitude" and "p_mean" variables
# Convert them to numeric, handling non-numeric entries (if any)
clim$altitude <- as.numeric(gsub(",", ".", clim$altitude))
clim$p_mean <- as.numeric(gsub(",", ".", clim$p_mean))

# Exclude the two high mountain extremes (rows 35 and 36)
clim1 <- clim[1:34, ]

# Build the multiple linear regression model
model <- lm(t_mean ~ altitude + lat + lon, data = clim1)

# Display the summary of the model
summary(model)
#t_mean=37.265+(−0.006)×Altitude+(−0.534)×Lat


# Interpretation:
# Altitude and latitude are significant predictors of mean temperature in France, 
#with altitude having a smaller effect but still important.
# Longitude does not significantly contribute to explaining mean temperature and 
#could potentially be removed from the model.
# The model fits well, explaining a large portion of the variability in temperature,
#but there remains some unexplained variation likely due to other factors not included in the model.

Graph <- raster::getData(country = "France", level = 1) #use version 3.6-26 of raster
library(ggplot2)
ggplot() +
  geom_polygon(
    data = Graph,
    aes(x = long, y = lat, group = group),
    colour = "black", fill = "yellow"
  ) +
  geom_point(
    data = clim,
    aes(x = lon, y = lat),
    alpha = .5,
    size = 2
  ) +
  theme_bw() +
  xlab("Longitude") +
  ylab("Latitude") +
  coord_map()


# Updated linear regression model without longitude
model_updated <- lm(t_mean ~ altitude + lat, data = clim1)

# Display the updated model summary
summary(model_updated)
#t_mean=37.915+(−0.006)×Altitude+(−0.547)×Latitude
 


# Create a data frame for the predictions
new_data <- data.frame(
  altitude = c(1212, 2860),
  lat = c(44.16, 42.93)
)

# Predict mean temperatures with confidence intervals
predictions <- predict(model_updated, newdata = new_data, interval = "confidence")

# Display predictions
predictions
#  fit       lwr      upr
#1  6.187574  4.333741 8.041407
#2 -3.463727 -8.134782 1.207329


install.packages("scatterplot3d") # Install
# Load necessary library
library("scatterplot3d")

# Create a 3D scatterplot
scatterplot3d(
  x = clim1$altitude,
  y = clim1$lat,
  z = clim1$t_mean,
  pch = 16, 
)
scatter_3d$plane3d(model_updated)
