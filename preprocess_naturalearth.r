library(rnaturalearth)
library(sf)

# Load data
land   <- ne_download(scale = 10, type = "geography_regions_polys", category = "physical", returnclass = "sf")
marine <- ne_download(scale = 10, type = "geography_marine_polys",  category = "physical", returnclass = "sf")

# Create a data frame with bounding boxes for countries and oceans
get_bbox_data <- function(data) {
  bbox_data <- data.frame(
    as.data.frame(data)[grep('name', colnames(data), ignore.case=TRUE)],
    min_lat = st_bbox(data)$ymin,
    min_lon = st_bbox(data)$xmin,
    max_lat = st_bbox(data)$ymax,
    max_lon = st_bbox(data)$xmax
  )
  return(bbox_data)
}

land_bbox <- do.call(rbind, lapply(1:nrow(land), function(i) get_bbox_data(land[i, ])))
colnames(land_bbox) <- tolower(colnames(land_bbox))
marine_bbox <- do.call(rbind, lapply(1:nrow(marine), function(i) get_bbox_data(marine[i, ])))
colnames(marine_bbox) <- tolower(colnames(marine_bbox))

# Combine the datasets
combined_bbox <- rbind(land_bbox, marine_bbox[,colnames(land_bbox)])

# Save to a tab-delimited file
write.table(combined_bbox, "supernational_bounding_boxes.txt", sep = "\t", row.names = FALSE, quote = TRUE)
