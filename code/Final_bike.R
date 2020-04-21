library(tidyverse)
library(grid)
library(magick)
library(ggpl)
commute <- read_rds(url("http://data.cwick.co.nz/commute.rds"))
# a subset of "Western" states for class
states <- c("or", "ak", "ca", "wa", "id", "nv")
commute_nw <- filter(commute, state %in% states)
#Filter for bikes only
commute_nw_bike <- commute_nw %>% 
  filter(transport_type == "Bicycle") %>%
  mutate(percent = prop*100)
#Reorder the factors so that they appear in this order on the plot
commute_nw_bike$state_name <- as_factor(commute_nw_bike$state_name) %>%
  fct_relevel("Oregon", "Alaska", "Idaho", "California", "Washington", "Nevada")
#Bike Picture
Bike_background<-image_read("https://www.asturiascycling.com/wp-content/uploads/2018/10/road-bike-png-5.png")
#Plot
ggplot(commute_nw_bike) +
  #Bike Pic
  annotation_custom(rasterGrob(Bike_background, width = unit(1,"npc"), height = unit(1,"npc")), -Inf, Inf, -Inf, Inf) +
  #Color (fill) the bars based on a logical statement, (i.e. only color Oregon)
  geom_col(aes(x = state_name, y = percent, fill = (state_name == "Oregon"))) +
  labs(x = NULL, y = NULL, 
       title = "People in Oregon commute by bicycle more \n than in other (nearby) states",
       subtitle = "Percent of respondents who said they bike to work") +
  scale_y_continuous(labels = function(x) paste0(x, "%"),expand = expansion(mult = c(0,0.1)))+
  #Set the colors manually
  scale_fill_manual(values = c("#002a86","#D73F09")) +
  #Minimal theme removes plot background and a few other small things
  theme_minimal() +
  #Remove legend, colors don't mean anything anyways
  theme(legend.position = "none",
        #We don't need vertical grid lines since we are comparing heights
        panel.grid.major.x = element_blank(),
        axis.text.x = element_text(face = "bold",size = 13.5),
        axis.text.y = element_text(face = "bold",size = 14),
        plot.title = element_text(face = "bold", size = 18),
        plot.subtitle = element_text(size = 16))
#save
ggsave("ST537_CommutePlot.png", last_plot(), height = 6, width = 10)
