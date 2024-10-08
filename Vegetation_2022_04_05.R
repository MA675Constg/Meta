
############### Loading ################
library(terra)
library(ggplot2)
library(scales)

# Set the working directory where the TIF files are located
setwd("E:/Desktop/BU/2024 Fall/MA675/Meta/Data")

# Get a list of all the TIF files in the directory
tif_files <- list.files(pattern = "\\.tif$", full.names = TRUE)
tif_names <- list.files(pattern = "\\.tif$", full.names = FALSE)

# Load all TIF files into a named list of rasters
raster_list <- setNames(lapply(tif_files, rast), tif_names)

############# Counting table ###########

# Count the number of vegetated pixels (value = 1) for each raster
count_values <- sapply(raster_list, function(r) sum(values(r) == 1, na.rm = TRUE))

# Calculate the mean for each raster
mean_values <- sapply(raster_list, function(r) mean(values(r), na.rm = TRUE))

# Extract time periods from the file names
time_periods <- as.Date(sub(".*_(\\d{4}-\\d{2}-\\d{2})_.*", "\\1", tif_names))

# Create a data frame for ggplot
df3 <- data.frame(
  Vegetation_Count = count_values,
  Mean_Vegetation = mean_values,
  Time_Period = time_periods
)

################  Viz ################

# Find the maximum and minimum vegetation counts
max_point <- df3[which.max(df3$Vegetation_Count), ]
min_point <- df3[which.min(df3$Vegetation_Count), ]

# Create a combined plot with ggplot, highlighting max/min and removing mean line
ggplot(df3, aes(x = Time_Period)) +
  geom_line(aes(y = Vegetation_Count, color = "Vegetation Count"), size = 1) +
  geom_point(aes(y = Vegetation_Count, color = "Vegetation Count")) +
  
  # Highlight the max and min points
  geom_point(data = max_point, aes(x = Time_Period, y = Vegetation_Count), color = "green", size = 4) +
  geom_point(data = min_point, aes(x = Time_Period, y = Vegetation_Count), color = "brown", size = 4) +
  
  # Adding labels to the max and min points
  geom_text(aes(x = max_point$Time_Period, y = max_point$Vegetation_Count, label = "Max"), vjust = -1.5) +
  geom_text(aes(x = min_point$Time_Period, y = min_point$Vegetation_Count, label = "Min"), vjust = 1.5) +
  
  # Formatting the x-axis with months
  scale_x_date(date_labels = "%b %Y", breaks = "3 months") +
  
  scale_y_continuous(
    name = "Count of Vegetated Pixels",
  ) +
  labs(title = "Vegetation Count Over Time", x = "Time Period") +
  theme_minimal() +
  theme(
    axis.text.x = element_text(angle = 45, hjust = 1, size = 25),  # Enlarge x-axis text
    axis.text.y = element_text(size = 25),  # Enlarge y-axis text
    axis.title.x = element_text(size = 30),  # Enlarge x-axis title
    axis.title.y = element_text(size = 30),  # Enlarge y-axis title
    plot.title = element_text(size = 35, hjust = 0.5),  # Enlarge and center title
    legend.text = element_text(size = 20),  # Enlarge legend text
    legend.title = element_text(size = 21)  # Enlarge legend title
  ) +
  scale_color_manual(name = "Lable",
    values = c("Vegetation Count" = "darkgreen")
  )


#######################################

# ne_shp <- vect("E:/Desktop/BU/2024 Fall/MA675/Meta/SHP/gadm41_ABW_shp/gadm41_ABW_0.shp")
# plot(ne_shp, main = "Natural Earth Shapefile - Country Boundaries")
# 
# 
# par(mfrow=c(2, 3))
# # 22: 04-05
# # Load the two rasters (replace with your actual file paths)
# raster1 <- rast("E:/Desktop/BU/2024 Fall/MA675/Meta/Data/SentinelMangroveExtent_Aruba_2022-04-01_2022-05-01.tif")
# raster2 <- rast("E:/Desktop/BU/2024 Fall/MA675/Meta/Data/SentinelMangroveExtent_Aruba_2022-05-01_2022-06-01.tif")
# 
# # Perform change detection by subtracting raster1 from raster2
# change_detection1 <- raster2 - raster1
# 
# # Plot the original rasters and the change detection results
# #par(mfrow=c(1, 3))  # Set up plot window for 3 plots side-by-side
# 
# # Plot the two original rasters
# plot(raster1, main = "Raster 1 (Original)")
# plot(raster2, main = "Raster 2 (Original)")
# 
# # Plot the change detection (areas of change)
# plot(change_detection1, main = "Change Detection (Raster 2 - Raster 1)", col = terrain.colors(10))
# 
# 
# # 23: 04-05
# raster3 <- rast("E:/Desktop/BU/2024 Fall/MA675/Meta/Data/SentinelMangroveExtent_Aruba_2023-04-01_2023-05-01.tif")
# raster4 <- rast("E:/Desktop/BU/2024 Fall/MA675/Meta/Data/SentinelMangroveExtent_Aruba_2023-05-01_2023-06-01.tif")
# 
# # Perform change detection by subtracting raster1 from raster2
# change_detection2 <- raster4 - raster3
# 
# # Plot the original rasters and the change detection results
# #par(mfrow=c(1, 6))  # Set up plot window for 3 plots side-by-side
# 
# # Plot the two original rasters
# plot(raster3, main = "Raster 3 (Original)")
# plot(raster4, main = "Raster 4 (Original)")
# 
# # Plot the change detection (areas of change)
# plot(change_detection2, main = "Change Detection (Raster 4 - Raster 3)", col = terrain.colors(10))

###########
# Check if the file GEO info match

# Load the shapefile and rasters
shapefile <- vect("E:/path_to_shapefile/gadm41_ABW_0.shp")
raster1 <- rast("E:/path_to_raster/raster1.tif")

# Check the CRS of both
shapefile_crs <- crs(ne_shp)
raster_crs <- crs(raster1)

# Print the CRS information
cat("Shapefile CRS:", shapefile_crs, "\n")
cat("Raster CRS:", raster_crs, "\n")

##################

par(mfrow=c(1, 3))
ne_shp <- vect("E:/Desktop/BU/2024 Fall/MA675/Meta/SHP/gadm41_ABW_shp/gadm41_ABW_0.shp")
# 22: April - May 2022
# Load rasters
raster1 <- rast("E:/Desktop/BU/2024 Fall/MA675/Meta/Data/SentinelMangroveExtent_Aruba_2022-04-01_2022-05-01.tif")
raster2 <- rast("E:/Desktop/BU/2024 Fall/MA675/Meta/Data/SentinelMangroveExtent_Aruba_2022-05-01_2022-06-01.tif")

# Load rasters
raster3 <- rast("E:/Desktop/BU/2024 Fall/MA675/Meta/Data/SentinelMangroveExtent_Aruba_2023-04-01_2023-05-01.tif")
raster4 <- rast("E:/Desktop/BU/2024 Fall/MA675/Meta/Data/SentinelMangroveExtent_Aruba_2023-05-01_2023-06-01.tif")

# Perform change detection
change_detection1 <- raster2 - raster1
change_detection2 <- raster4 - raster3

#plot(change_detection1, col = change_colors, main = "Change Detection 2022")
main_size <- 2  # For the main title
label_size <- 1.5  # For axis labels
axis_size <- 1.2  # For axis text

# Plot April-May 2022 Raster
plot(raster1, main = "April-May 2022", cex.main = main_size, cex.lab = label_size, cex.axis = axis_size)
plot(ne_shp, add = TRUE, border = "white", lwd = 1)  # Overlay shapefile

# Plot May-June 2022 Raster
plot(raster2, main = "May-June 2022", cex.main = main_size, cex.lab = label_size, cex.axis = axis_size)
plot(ne_shp, add = TRUE, border = "white", lwd = 1)  # Overlay shapefile

# Plot Change Detection 2022
plot(change_detection1, main = "Change Detection 2022", col = terrain.colors(10), cex.main = main_size, cex.lab = label_size, cex.axis = axis_size)
plot(ne_shp, add = TRUE, border = "white", lwd = 1)  # Overlay shapefile

# Plot April-May 2023 Raster
plot(raster3, main = "April-May 2023", cex.main = main_size, cex.lab = label_size, cex.axis = axis_size)
plot(ne_shp, add = TRUE, border = "white", lwd = 0.5)  # Overlay shapefile

# Plot May-June 2023 Raster
plot(raster4, main = "May-June 2023", cex.main = main_size, cex.lab = label_size, cex.axis = axis_size)
plot(ne_shp, add = TRUE, border = "white", lwd = 0.5)  # Overlay shapefile

# Plot Change Detection 2023
plot(change_detection2, main = "Change Detection 2023", col = terrain.colors(10), cex.main = main_size, cex.lab = label_size, cex.axis = axis_size)
plot(ne_shp, add = TRUE, border = "white", lwd = 0.5)  # Overlay shapefile

################################    STAT    #############

vg_vegetation_count <- mean(df3$Vegetation_Count, na.rm = TRUE)
vg_vegetation_count

# Assuming change_detection1 is your change detection raster
# Calculate the number of pixels with positive change (1) and negative change (-1)
positive_change_pixels <- sum(values(change_detection1) == 1, na.rm = TRUE)
negative_change_pixels <- sum(values(change_detection1) == -1, na.rm = TRUE)

cat("Positive Change (in number of pixels):", positive_change_pixels, "\n")
cat("Negative Change (in number of pixels):", negative_change_pixels, "\n")

pixel_area_m2 <- 10 * 10  # Each pixel represents 100 square meters

# Calculate the real-world area of positive and negative changes
positive_change_area_m2 <- positive_change_pixels * pixel_area_m2
negative_change_area_m2 <- negative_change_pixels * pixel_area_m2

# Convert to hectares (1 hectare = 10,000 square meters)
positive_change_area_ha <- positive_change_area_m2 / 10000
negative_change_area_ha <- negative_change_area_m2 / 10000

cat("Positive Change Area (in hectares):", positive_change_area_ha, "\n")
cat("Negative Change Area (in hectares):", negative_change_area_ha, "\n")

# Convert the Vegetation_Count to a time series object with a monthly frequency
ts_data <- ts(df3$Vegetation_Count, start = c(2019, 5), frequency = 12)

# Decompose the time series into seasonal, trend, and residual components
decomposed_data <- decompose(ts_data)

# Plot the decomposition
plot(decomposed_data)

##### Wet Dry season

# Create a season column based on the month
df3$Season <- ifelse(format(df3$Time_Period, "%m") %in% c("10", "11", "12"), "Wet", "Dry")

# Calculate average vegetation count for wet and dry seasons
wet_season_avg <- mean(df3$Vegetation_Count[df3$Season == "Wet"], na.rm = TRUE)
dry_season_avg <- mean(df3$Vegetation_Count[df3$Season == "Dry"], na.rm = TRUE)

cat("Average Vegetation Count in Wet Season:", wet_season_avg, "\n")
cat("Average Vegetation Count in Dry Season:", dry_season_avg, "\n")

# Plot the seasonal differences
ggplot(df3, aes(x = Season, y = Vegetation_Count)) +
  geom_boxplot(fill = c("lightblue", "lightgreen")) +
  labs(title = "Vegetation Count by Season", x = "Season", y = "Vegetation Count") +
  theme_minimal()
##### 
# Plot Vegetation Count over time, colored by season
ggplot(df3, aes(x = Time_Period, y = Vegetation_Count, color = Season)) +
  geom_line(size = 1) +
  scale_x_date(date_labels = "%b %Y", breaks = "3 months") +
  labs(title = "Seasonal Vegetation Changes Over Time", x = "Time Period", y = "Vegetation Count") +
  theme_minimal() +
  scale_color_manual(values = c("Wet" = "blue", "Dry" = "red"))

# Compare vegetation count before, during, and after the wet season
df3$Period <- ifelse(format(df3$Time_Period, "%m") %in% c("09"), "Pre-Wet",
                     ifelse(format(df3$Time_Period, "%m") %in% c("10", "11", "12"), "Wet", "Post-Wet"))

# Plot the differences
ggplot(df3, aes(x = Period, y = Vegetation_Count)) +
  geom_boxplot(fill = c("lightgray", "lightblue", "lightgreen")) +
  labs(title = "Vegetation Count: Pre-Wet, Wet, and Post-Wet Season", x = "Period", y = "Vegetation Count") +
  theme_minimal()



