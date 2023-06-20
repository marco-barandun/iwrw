library(leaflet)
library(leaflet.extras)
library(DT)
library(scales)
library(tidyverse)
library(sf)
library(htmltools)
library(readr)

setwd("/Users/marco/GitHub/iwrw/R_files")
source("./config_plot_map.R")

plots <- read_csv("./iwrw-plots.csv", locale = locale(encoding = "UTF-8")) %>%
  mutate(link = paste0("http://www.google.ch/maps/place/", Latitude, ",", Longitude))

#be <- rgdal::readOGR("/Users/marco/kDocuments_Marco/PhD/old/server/1_original_data/shapefiles/be_bewirtschaftungseinheit_view.shp")
#
#poly <- be %>%
#  get_polygons(plots = plots, shapefile = ., radius_m = 150)
#
#writeOGR(poly, dsn = "./plots-with-be-poly.geojson", 
#         layer = ogrListLayers("/Users/marco/kDocuments_Marco/PhD/old/server/1_original_data/shapefiles/be_bewirtschaftungseinheit_view.shp")[1],
#         driver = "GeoJSON")

poly <- rgdal::readOGR("./plots-with-be-poly.geojson")

########################################################################################################################################
### Create plot table #################################################################################################################
########################################################################################################################################
  
(t <- DT::datatable(plots %>%
                      mutate(ID = paste0('<a target="_parent" href=', .$link, '>', .$ID, ' </a>', sep = "")) %>%
                      select(-Latitude, -Longitude, -link),
                    class = "display nowrap",
                    escape = F,
                    rownames = FALSE))

htmltools::save_html(t, file="plot-table.html")


########################################################################################################################################
### Create a leaflet map with the Swiss Topographic Map as a basemap ###################################################################
########################################################################################################################################

# Define a color palette with distinct colors
pal <- colorFactor(
  palette = c("red", "orange", "yellow", "green", "blue", "purple", "magenta"),
  domain = plots$priority
)

addIconMarker <- function(lat, lng, map) {
  addAwesomeMarkers(
    map = map,
    lng = lng,
    lat = lat,
    icon = awesomeIcon(
      icon = "crosshairs",
      markerColor = "blue",
      library = "fa",
      iconColor = "white"
    )
  )
}

# Filter the points with LU2020 column set to true
#BFF <- plots %>% filter(LU2020 == TRUE)
#non_BFF <- plots %>% filter(LU2020 == FALSE | is.na(LU2020))

(m <- leaflet(plots) %>%
    addTiles(urlTemplate = "https://wmts20.geo.admin.ch/1.0.0/ch.swisstopo.pixelkarte-farbe/default/current/3857/{z}/{x}/{y}.jpeg",
             attribution = '&copy; <a href="https://www.geo.admin.ch/de/about-swiss-geoportal/impressum.html#copyright">swisstopo</a>',
             group = "Swiss Topographic Map") %>%
    addTiles(urlTemplate = "https://wmts20.geo.admin.ch/1.0.0/ch.swisstopo.swissimage/default/current/3857/{z}/{x}/{y}.jpeg",
             attribution = '&copy; <a href="https://www.geo.admin.ch/de/about-swiss-geoportal/impressum.html#copyright">swisstopo</a>',
             group = "Satellite View") %>%
    addCircleMarkers(data = plots, 
                     lat = ~Latitude, lng = ~Longitude,
                     popup = ~paste(ID),#, round(elevation, 0), sep = " - "),
                     radius = 8, stroke = FALSE, fillOpacity = 1, color = "magenta",#color = ~pal(priority),
                     group = "BFF") %>%
    #addCircleMarkers(data = non_BFF, 
    #                 lat = ~Latitude, lng = ~Longitude,
    #                 popup = ~paste(ID, round(elevation, 0), sep = " - "),
    #                 radius = 8, stroke = FALSE, fillOpacity = 1, color = ~pal(priority),
    #                 group = "non_BFF") %>%
    #addLegend(pal = pal, values = plots$priority,
    #          position = "bottomright", title = "Value") %>%
    addScaleBar(position = "bottomleft") %>%
    setView(lng = 10.36, lat = 46.62, zoom = 13) %>%
    addPolygons(data = poly, 
                fill = FALSE, 
                color = "darkorange", 
                opacity = 0.9,
                group = "Bewirtschaftungseinheiten") %>%
    #addAwesomeMarkers(data = donePlots,
    #                  lat = ~Latitude, lng = ~Longitude,
    #                  icon = ~awesomeIcons(
    #                    icon = "leaf",
    #                    markerColor = "green",
    #                    iconColor = "white",
    #                    library = "fa"
    #                  ),
    #                  labelOptions = labelOptions(noHide = TRUE, textOnly = TRUE),
    #                  label = lapply(donePlots$ID, HTML),
    #                  clusterOptions = markerClusterOptions(removeOutsideVisibleBounds = FALSE),
    #                  group = "Done Plots") %>% 
    addWMSTiles(
      baseUrl = "https://wms.geo.admin.ch/",
      layers = "ch.bav.haltestellen-oev",
      group = "Public Transport Stops",
      options = WMSTileOptions(format = "image/png", transparent = TRUE)
    ) %>%
    addLayersControl(
      baseGroups = c("Swiss Topographic Map", "Satellite View"),
      overlayGroups = c("Bewirtschaftungseinheiten", "Public Transport Stops"),
      options = layersControlOptions(collapsed = TRUE)
    ) %>%
    hideGroup(c("Bewirtschaftungseinheiten", "Public Transport Stops")) %>%
    addFullscreenControl() %>%
    addEasyButton(
      easyButton(
        icon = "fa-crosshairs",
        title = "Locate Me",
        onClick = JS(
          "function(btn, map) {
          map.locate({setView: true, enableHighAccuracy: true});
          
          map.on('locationfound', function(e) {
            var lat = e.latlng.lat;
            var lng = e.latlng.lng;
            
            var customIcon = L.icon({
              iconUrl: './icons/my_position.png',
              iconSize: [52, 52],
              iconAnchor: [16, 32]
            });
            
            L.marker([lat, lng], { icon: customIcon }).addTo(map);
          });
        }"
        )
      )
    )
  )

# Add fullscreen button

htmlwidgets::saveWidget(m, file=paste("./plot-map.html", sep = ""))


###### SAVING AN IMAGE OF EVERY LOCATION ###### 

# Cluster the points based on distance
coords <- plots[, c("Longitude", "Latitude")]

# Maximum distance in meters
max_distance <- 3800

# Helper function to convert coordinates to Cartesian coordinates in meters
coord2cartesian <- function(coords) {
  lat_rad <- coords$Latitude * pi / 180
  x <- coords$Longitude * 6378137 * pi / 180
  y <- log(tan((90 + coords$Latitude) * pi / 360)) * 6378137
  return(data.frame(Longitude = x, Latitude = y))
}

# Convert coordinates to Cartesian coordinates in meters
coords_cartesian <- coord2cartesian(coords)

# Perform DBSCAN clustering
db_clusters <- dbscan(coords_cartesian, eps = max_distance, minPts = 2); table(db_clusters$cluster)

# Create the mini-map
mini_map <- leaflet(options = leafletOptions(minZoom = 0, maxZoom = 13)) %>%
  setView(lng = 8.2, lat = 46.8, zoom = 8)

# Generate and save images for each cluster
coords$cluster <- db_clusters$cluster
unique_clusters <- unique(db_clusters$cluster)

for (i in unique_clusters) { #unique(db_clusters$cluster)
  cluster_coords <- coords %>%
    filter(cluster == i)
  
  # Calculate bounding box
  min_lng <- min(cluster_coords$Longitude)
  max_lng <- max(cluster_coords$Longitude)
  min_lat <- min(cluster_coords$Latitude)
  max_lat <- max(cluster_coords$Latitude)
  
  # Calculate zoom level based on bounding box dimensions
  lng_diff <- max_lng - min_lng
  lat_diff <- max_lat - min_lat
  zoom <- 14 #ifelse(min(18, floor(-log2(max(lng_diff, lat_diff)) + 8)) > 14, 14, min(18, floor(-log2(max(lng_diff, lat_diff)) + 8)))
  
  # Set view to bounding box
  cluster_map <- m %>%
    hideGroup("Done Plots") %>%
    setView(lng = (min_lng + max_lng) / 2, lat = (min_lat + max_lat) / 2, zoom = zoom) %>%
    addMiniMap(mini_map, width = 150, height = 100, position = "bottomleft", zoomLevelFixed = 6,
               tiles = "https://wmts20.geo.admin.ch/1.0.0/ch.swisstopo.pixelkarte-farbe/default/current/3857/{z}/{x}/{y}.jpeg") %>%
    addLabelOnlyMarkers(
      data = plots,
      lng = ~Longitude,
      lat = ~Latitude,
      label = ~as.character(ID),
      labelOptions = labelOptions(noHide = T)
    )
  
  mapshot(cluster_map, file = paste0("./plot_maps/cluster_", i, ".png"), remove_controls = TRUE, delay = 2, vwidth = 1500, vheight = 1000)
  
}


###### CREATING THE MUNICIPALITY MAP ############################################

#municipalities <- read_csv("./2023-joinedPlotSelection_v2.csv") %>%
#  get_municipality(., municipalities, what = c("NAME_1", "NAME_3")) %>%
#  rename(canton = NAME_1,
#         municipality = NAME_3) %>%
#  select(canton, municipality)


