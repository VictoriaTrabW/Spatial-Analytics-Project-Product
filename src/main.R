# Installing and loading required packages
install.packages("dplyr")
install.packages("sf")
install.packages("ggplot2")
install.packages("leaflet")
install.packages("htmlwidgets")
library(dplyr)
library(sf)
library(ggplot2)
library(leaflet)
library(htmlwidgets)

# Reading the data and filtering relevant columns, removing rows with missing values
accidents <- read.csv("data/NYC_Accidents_2020.csv") %>%
  na.omit()

accidents_sf <- st_as_sf(accidents,
                         coords = c("LONGITUDE", "LATITUDE"),
                         crs = 2263) 

# reading shapefile
zipcodesNYC <- st_read("data/ZIP_CODE_040114.shp")

# Convert ZIP.CODE column in accidents to uppercase and character type
accidents$ZIP.CODE <- toupper(as.character(accidents$ZIP.CODE))

# Calculate the total accidents per ZIP code
total_accidents <- accidents %>%
  group_by(ZIP.CODE) %>%
  summarize(Total_Accidents = n())

# Convert ZIPCODE column in the shapefile to character type
zipcodesNYC$ZIPCODE <- as.character(zipcodesNYC$ZIPCODE)

# Filter the shapefile to include only relevant ZIP codes
filtered_zipcodesNYC <- zipcodesNYC %>%
  filter(ZIPCODE %in% unique(accidents$ZIP.CODE))

# Join the total accidents with the filtered shapefile
zipcodesNYC_with_accidents <- left_join(filtered_zipcodesNYC, total_accidents, by = c("ZIPCODE" = "ZIP.CODE"))

# Print the resulting shapefile with total accidents
print(zipcodesNYC_with_accidents)

# Sort the ZIP codes by the total number of accidents in descending order
sorted_zipcodes <- zipcodesNYC_with_accidents %>%
  arrange(desc(Total_Accidents))

# Print the top 10 ZIP codes with the most accidents
print(sorted_zipcodes[1:10, ])

# Print the bottom 10 ZIP codes with the least accidents
print(sorted_zipcodes[(nrow(sorted_zipcodes)-9):nrow(sorted_zipcodes), ])

# Extract the hour from the CRASH.TIME column
accidents$Hour <- substr(accidents$CRASH.TIME, 1, 2)

# Group the accidents by hour and count the occurrences
hourly_accident_count <- accidents %>%
  group_by(Hour) %>%
  summarize(Count = n())

# Sorting the hourly_accident_count dataframe by count in descending order
sorted_hourly_accident_count <- hourly_accident_count %>%
  arrange(desc(Count))

# Creating a bar plot of the hourly accident count
plot_hourly_accident_count <- ggplot(sorted_hourly_accident_count, aes(x = Hour, y = Count)) +
  geom_bar(stat = "identity", fill = "blue") +
  labs(title = "Hourly Accident Count", x = "Hour", y = "Count")

# Saving the plot as a figure
ggsave("hourly_accident_count.png", plot_hourly_accident_count)

# Reprojecting the shapefile to WGS84 CRS
sorted_zipcodes <- st_transform(sorted_zipcodes, crs = 4326)  # Reproject to WGS84 CRS (EPSG:4326)

# visualising a sort of a heat map
# Defining breaks and colors for color categories
breaks <- c(0, 100, 200, 300, 400, 500, 600, 700, 800, 900, 1107)
colors <- c("#FFFFCC", "#FFFF66", "#FFCC33", "#FF9933", "#FF6633",
            "#FF3333", "#FF0000", "#CC0000", "#990000", "#660000")

# Categorizing total accidents into color categories
sorted_zipcodes$ColorCategory <- cut(sorted_zipcodes$Total_Accidents, breaks = breaks, labels = FALSE)

# Creating the map and storing it in a variable to be able to save it
NYC_map <- leaflet() %>%
  setView(lng = -74.006, lat = 40.7128, zoom = 10) %>%
  addProviderTiles("CartoDB.Positron") %>%
  addPolygons(
    data = sorted_zipcodes,
    fillColor = ~colors[ColorCategory],
    fillOpacity = 0.7,
    color = "#ffffff",
    weight = 1,
    smoothFactor = 0.5,
    label = ~ZIPCODE
  )

# showing the map
NYC_map

# Saving the leaflet map as an HTML file
saveWidget(NYC_map, "NYC_accidents_map.html")

# performing correlation analysis
correlation <- cor(sorted_zipcodes$POPULATION, sorted_zipcodes$Total_Accidents)
print(correlation)

# since there seem to be a correlation between population size and amount of accidents
# i am calculating accidents per inhabitant
accidents_per_inh <- sorted_zipcodes %>%
  mutate(Accidents_per_inhabitant = Total_Accidents / POPULATION)

# Printing the results
print(accidents_per_inh)


