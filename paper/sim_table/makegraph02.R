library(here)
library(readr)
library(tidyverse)
# run for  norm MMPD files aggregation
#all_data <- read_rds("simulations/result_report/all_data_ norm.rds")
# run for  norm max pairwise distances files aggregation

all_data <- read_rds("paper/sim_table/all_data.rds")


#all_data <- write_rds(all_data, "paper/sim_table/all_data.rds")

facet_variable = "day_week"
x_variable = "hour_day"

one_harmony_data <- all_data %>% filter(facet_variable == "day_week",
                    x_variable == "hour_day")

mds<- one_harmony_data %>%
  select(customer_id, facet_variable, x_variable, wpd_norm) %>% 
  dist() %>%          
  cmdscale() %>%
  as_tibble()

colnames(mds) <- c("Dim.1", "Dim.2")
rownames(mds) <- unique(one_harmony_data$customer_id)
mds <- mds %>%
  mutate(customer_id = unique(one_harmony_data$customer_id)) %>% 
  select(customer_id, Dim.1, Dim.2)
?
# Plot MDS
df <- ggpubr::ggscatter(mds, x = "Dim.1", y = "Dim.2", 
          label = rownames(mds),
          size = 1,
          repel = TRUE)

library(leaflet)
library(sp)
mds_new <- mds %>% scale() 
Sr1 = Polygon(mds)

Srs1 = Polygons(list(mds), "s1")
SpP = SpatialPolygons(list(Srs1), 1L)
leaflet(mds) %>% addPolygons(data = PolygonData)
leaflet(options = leafletOptions(crs = leafletCRS(crsClass = "L.CRS.Simple"))) %>% addPolygons(data = Srs1)
mapview(SpP, native.crs = TRUE)
