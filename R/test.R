library(tidyverse)
library(sf)
library(units)
library(leaflet)

states = USAboundaries::us_states()


state.of.interest = name

soi = filter(states, state_name == state.of.interest)

adjoining = st_filter(states, soi, .predicate = st_touches)


closest = st_make_grid(soi, n = 70, square = F) %>%
  st_centroid() %>%
  st_sf() %>%
  st_join(adjoining, join = st_nearest_feature)

vor = closest %>%
  st_union() %>%
  st_voronoi() %>%
  st_cast() %>%
  st_sf() %>%
  st_join(closest) %>%
  summarise() %>%
  st_intersection(soi)


leaflet() %>%
  addProviderTiles(providers$CartoDB) %>%
  addCircles(data = st_transform(closest, 4326),
             radius = 1, color = ~colorFactor("YlOrRd", state_name)(state_name)) %>%
  addPolygons(data = st_transform(soi, 4326),
              fillColor = "transparent",
              color = "black") %>%
  addPolygons(data = st_transform(adjoining, 4326), fillColor = ~colorFactor("YlOrRd", state_name)(state_name), col = NA) %>%
  addLayersControl(overlayGroups = c("SOI"))


