### Isochrone Map for Coffee Shops in Culver City ###

# Load required libraries
library(tidyverse)
library(openrouteservice)
library(osmdata)
library(ggplot2)
library(ggmap)
library(sf)
library(leaflet)
library(dotenv)


### Part 1 - OSM Data ----------------------


# Create a bounding box for Culver City, our area of interest
cc_bb <- getbb("Culver City")
#yaha_bb <- getbb("Tel Aviv")


# Use Open Street Map data to retrieve coffee shops within our bounding box
cc_coffee <- cc_bb %>%
  opq() %>%
  add_osm_feature(key = "cuisine", value = "coffee_shop") %>%
  osmdata_sf()


# Clean up the point data so we only have locations with associated names
coffee_df <- cc_coffee$osm_points %>%
  drop_na(name) %>%
  mutate(char_geo = as.character(geometry)) %>%
  separate(char_geo, c("lon", "lat"), sep = ",") %>%
  mutate(lon = as.numeric(gsub(x = lon,
                               pattern = "c(", 
                               replacement = "",
                               fixed = TRUE)),
         lat = as.numeric(gsub(x = lat,
                               pattern = ")", 
                               replacement = "",
                               fixed = TRUE))) %>%
  select(osm_id, name, lon, lat, geometry)


### Part 2 - ORS Isochrones ----------------------

# Add personal API key
load_dot_env(file = '.env')
ors_api_key(Sys.getenv("ORS_API"))


# Create a function to retrieve an isochrone map for a specified location
get_iso <- function(name, longitude, latitude) {
  
  coordinates <- data.frame(lon = c(longitude), lat = c(latitude))
  cc_iso <- ors_isochrones(locations = coordinates, profile = "foot-walking", range = 600, interval = 150, output = "sf")
  cc_iso$shop <- name
  cc_iso
  
}


# Iterate over all locations in our data frame
# Add walk distance (time) column and dissolve by walk time
Coffee_Shops <- pmap(
  list(name = coffee_df$name,
       longitude = coffee_df$lon,
       latitude = coffee_df$lat),
  .f = get_iso) %>%
  bind_rows() %>%
  mutate(
    dist_cat = paste0(value/60, " min"),
    dist_cat = fct_reorder(dist_cat, desc(value))) %>%
  group_by(dist_cat) %>%
  summarize(geometry = st_union(geometry))



### Part 3 - Visualize with ggmap ----------------------

# Create the base map
cc_map <- get_map(cc_bb,
                  source = "stamen", 
                  maptype = "terrain",
                  color = "bw")

# Generate the final map! 
ggmap(cc_map) + 
  geom_sf(data = Coffee_Shops,
          aes(fill = dist_cat),
          color = NA,
          inherit.aes = FALSE,
          alpha = 0.6) +
  scale_fill_manual(values = c("#587291", "#1CCAD8", "#15E6CD", "#11EEA1")) +
  geom_point(data = coffee_df, 
             aes(x = lon, y = lat),
             inherit.aes = FALSE) +
  scale_color_identity() +
  labs(title = "Isochrone Map of Coffee Shops Near Culver City, CA",
       fill = "Walking Time",
       shape = "Franchise",
       x = "",
       y = "")

dev.off()


### Part 4 - Visualize with Leaflet (Interactive) ----------------------


# Create the icon sets to be used with our leaflet map
icons <- iconList(
  "Starbucks" = makeIcon(
    iconUrl = "https://img.icons8.com/color/344/starbucks.png",
    iconWidth = 30,
    iconHeight = 30),
  "Other" = makeIcon(
    iconUrl = "https://img.icons8.com/ios-filled/452/coffee-beans---v2.png",
    iconWidth = 20,
    iconHeight = 20))


# Create the distance interval color pallete 
factpal <- colorFactor(topo.colors(4), Coffee_Shops$dist_cat)


# Creat the leaflet map! 
coffee_leaf <- leaflet() %>%
  addProviderTiles("CartoDB.Positron", group = "Light") %>%
  addProviderTiles("Esri.WorldStreetMap", group = "Street Map") %>%
  addProviderTiles("Esri.WorldImagery", group = "Satellite") %>%
  addLayersControl(
    baseGroups = c("Light", "Street Map", "Satellite"),
    position = "topleft") %>%
  addMarkers(
    data = coffee_df %>%
      mutate(name2 = ifelse(grepl("Starbucks", name, fixed = TRUE), 
                            "Starbucks", 
                            "Other")),
    lat = coffee_df$lat,
    lng = coffee_df$lon,
    label = coffee_df$name,
    icon = ~ icons[name2],
  ) %>%
  addPolygons(
    data = Coffee_Shops,
    color = ~factpal(dist_cat),
    opacity = 0,
    fillOpacity = 0.4) %>%
  addLegend(
    pal = factpal,
    values = Coffee_Shops$dist_cat,
    position = "bottomright",
    title = "Walking Time"
  )

coffee_leaf


## To do...
# Add culver city outline? Add bike baths? 
# Allow users to view either walking or biking layers? Toggles! 


### Part 5 - Export (optional) ----------------------


# Export isochrone to geojson
st_as_sf(Coffee_Shops) %>%
  st_write(geo_mtg, "CC_WalkIso.geojson")

# Export center points
write.csv(Coffee_Shops, "CC_CShops.csv")

