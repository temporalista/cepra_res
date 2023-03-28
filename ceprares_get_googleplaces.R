# Load required libraries
library(httr)
library(jsonlite)
library(sf)
library(tmap)


# Set API parameters

lat <- 0.35475
lon <- -78.12041
radius <- 200
tipo <- 'point_of_interest'

api_key <- "AIzaSyBQF7j0IXXsmW5fY_1gl6vyP-jxU8dVbps"


ori <- st_sfc(st_point(c(lon,lat)), crs = 4326)|>
  st_as_sf()

#build the call
gcall <- paste0("https://maps.googleapis.com/maps/api/place/nearbysearch/json?location=",
       lat, ",", lon, 
       # "&radius=", radius,
       "&type=",tipo,
       "&rankby=distance",
       "&key=", api_key)

# Make the request
response <- GET(gcall)

response


results <- data.frame()

places_list <- content(response, "text")|>
  fromJSON()


p=1

places_sf <- as.data.frame(cbind(
  pag=p,
  tipo=data.frame(tipo = unlist(lapply(places_list$results$types, paste, collapse = ", "))),
  nombre=places_list$results$name,
  lon=places_list$results$geometry$location$lng,
  lat= places_list$results$geometry$location$lat)
  )|>
  st_as_sf(coords = c("lon", "lat"), crs = 4326)

results <- rbind(results, places_sf)

# Check if there are more pages of results
while (!is.null(places_list$next_page_token)) {
  # Wait for a few seconds to avoid OVER_QUERY_LIMIT error
  Sys.sleep(2)
  p=p+1
  # Make the next request with the pagetoken parameter
  response <- GET(paste0("https://maps.googleapis.com/maps/api/place/nearbysearch/json?key=", 
                         api_key, "&pagetoken=", places_list$next_page_token))
  
  # Extract the JSON content from the response
  places_json <- content(response, "text")
  
  # Convert the JSON content to a list
  places_list <- fromJSON(places_json)
  
  # Convert the results to an sf object
  places_sf <- as.data.frame(cbind(
    pag=p,
    tipo=data.frame(tipo = unlist(lapply(places_list$results$types, paste, collapse = ", "))),
    nombre=places_list$results$name,
    lon=places_list$results$geometry$location$lng,
    lat= places_list$results$geometry$location$lat),
  )|>
    st_as_sf(coords = c("lon", "lat"), crs = 4326)
  
  # Add the results to the data frame
  results <- rbind(results, places_sf)
}

# View the results
results



# View the sf object
tm_shape(results)+
  tm_dots(col="pag")+
  tm_shape(ori)+
  tm_dots(size=0.1, col="blue")


