



##########


#
bbwgs = bb %>% st_transform(4326)
bb = st_bbox(bbwgs)


scenes = lsat_scenes()

down = scenes %>%
  filter(min_lat <= bb$ymin, max_lat >= bb$ymax,
         min_lon <= bb$xmin, max_lon >= bb$xmax,
         as.Date(acquisitionDate)== as.Date("2016-09-26"))

write.csv(down, file = "data/palo-flood.csv", row.names = F)


###### in rmd


