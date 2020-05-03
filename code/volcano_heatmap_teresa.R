library(ggplot2)
library(tidyverse)
volcano_tbl <- as_tibble(volcano)
colnames(volcano_tbl) <- 1:ncol(volcano) 
volcano_tbl$row <- 1:nrow(volcano_tbl)

heat_map <- volcano_tbl %>% pivot_longer(-contains("row"), names_to = "y", 
                                       values_to = "elevation")
names(heat_map) <- c("x", "y", "elevation")
heat_map$y <- as.integer(heat_map$y)
ggplot(heat_map)+
  geom_tile(aes(x,y, fill = elevation))+
  scale_fill_distiller(palette = "Spectral")+
  coord_equal() +
  geom_contour(aes(x = x, y = y, z=elevation)) +
  theme_bw()
  
  
filled.contour(volcano, color.palette = terrain.colors, asp = 1)


