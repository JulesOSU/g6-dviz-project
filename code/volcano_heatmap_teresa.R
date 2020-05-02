volcano_tbl <- as_tibble(volcano)
colnames(volcano_tbl) <- 1:ncol(volcano) 
volcano_tbl$row <- 1:nrow(volcano_tbl)

heat_map <- volcano_tbl %>% pivot_longer(-contains("row"), names_to = "y", 
                                       values_to = "elevation")

heat_map$y <- as.integer(heat_map$y)
ggplot(heat_map)+
  geom_tile(aes(row,y, fill = elevation))+
  scale_fill_distiller(palette = "Spectral")
  
filled.contour(volcano, color.palette = terrain.colors, asp = 1)


