library(tidyverse)
library(ggplot2)
library(plyr)
library(airportr)
library(hflights)

#Get data for base map
US <- map_data("state")

#Get data for airport locations
US_airports <- airports %>%
  filter(Country == "United States" & 
           (24 < Latitude & Latitude < 48.5) & 
           (-65 > Longitude & Longitude > -130)) %>%
  select(Longitude, Latitude, IATA, Name, City) 

#Test it out
ggplot() + geom_polygon(data = US, aes(x = long, y = lat, group = group),
                        fill = "dark gray", color = "light gray")

#Import flights dataset
hflights_df <- tbl_df(hflights)

#Group by Origin (HOU vs. IAH) and then by destination
by_dest <- hflights_df %>% 
  group_by(Origin, Dest)

#Summarize by group, so we calculate the mean delay (or other statistic)
#for each destination and each origin
by_dest_sum <- by_dest %>% 
  dplyr::summarise(
    mean_delay = mean(DepDelay, na.rm = TRUE),
    prop_over_15 = mean(DepDelay > 15, na.rm = TRUE),
    nflights  = n()
  )

#Change the names for consistency in the join
colnames(US_airports) <- c("lon", "lat", "dest", "city", "name")
colnames(by_dest_sum) <- c("origin", "dest", "mean_delay", "prop_over_15", "nflights")

#Extract lon lat for HOU and IAH
Hlon <- US_airports$lon[US_airports$dest == "HOU"]
Hlat <- US_airports$lat[US_airports$dest == "HOU"]
Ilon <- US_airports$lon[US_airports$dest == "IAH"]
Ilat <- US_airports$lat[US_airports$dest == "IAH"]
origins <- data.frame(lon = c(Hlon, Ilon), lat = c(Hlat, Ilat))

#Put everything together in the plotting data frame:
#Join the airport and flight data frames with inner_join(),
#Reorder the columns with select, 
#Add the location data for the Houston airports, 
plot_df <- inner_join(by_dest_sum, US_airports, by = "dest") %>%
  select(lon, lat, dest, origin, prop_over_15, nflights, city, name, mean_delay) %>%
  mutate(start_lon = ifelse(dest == "HOU", origins$lon[1], origins$lon[2]),
         start_lat = ifelse(dest == "HOU", origins$lat[1], origins$lat[2]))

#Filter for destinations with more than 100 flights
plot_df <- plot_df %>% filter(nflights > 100)

plot_df$mean_delay[(plot_df$mean_delay > 29 | plot_df$mean_delay < 0)] <- NA

#Add a factor column for bins of mean_delay, if you want to map color 
#to a categorical variable
plot_df <- plot_df %>%
  mutate(mean_delay_f = as_factor(round_any(plot_df$mean_delay,5,floor)))

plot_df$mean_delay_f2 <- cut(plot_df$mean_delay, breaks = seq(0,20, length.out = 4))

text_df <- data.frame(x = c(-80,-95), y = c(43,40),
                      origin = factor(c("HOU","HOU"), levels = c("HOU","IAH")),
                      label = c("Flights to Newark (EWR)\nand Philadelphia (PHL)\nwere delayed for 30+\nminutes, on average.",
                                "Over 110 trips, flights to\nBranson Airport (BKG)\ndeparted 3 minutes early,\non average."))

#Plot the map using geom_polygon
ggplot() + geom_polygon(data = US, aes(x = long, y = lat, group = group), 
                        fill = "#545454", color = "#666666") + 
  #Mark the airports with points
  geom_point(data = plot_df, 
             aes(x = lon, y = lat, 
                 color = mean_delay,
                 fill = ""), 
             size = 0.1) +
  #Mark the flight paths with neat looking curves
  geom_curve(data = plot_df, 
             aes(x = start_lon, y = start_lat,
                 xend = lon, yend = lat,
                 color = mean_delay),
             #Make them all just slightly transparent, so they don't block each other out so much
             alpha = 0.75, curvature = 0.3, size = 0.6, angle = 130) +
  labs(title = "Average delay based on destination",
       subtitle = "Departures from HOU vs. IAH",
       x = NULL, y = NULL) +
  #Set color palette for continuous scale
  scale_color_viridis_c(NULL, option = "plasma",
                        breaks = c(min(plot_df$mean_delay, na.rm = T) + 2, 
                                   max(plot_df$mean_delay, na.rm = T)) -1.5,
                        labels = c("On time","20 minutes\nlate"),
                        aesthetics = "color",
                        guide = guide_colorbar(ticks = F),
                        na.value = "white") +
  scale_fill_manual(values = NA, name = NULL, guide = "none", labels = "Outlier") +
  guides(fill=guide_legend(override.aes=list(shape=NA))) +
  #A whole bunch of tweaks to the theme
  theme(panel.background = element_rect(color = "#c2c2c2", fill = "#121212"),
        plot.background = element_rect(color = "#121212", fill = "#121212"),
        plot.margin = margin(0.5,0.5,0.5,0.5,"cm"),
        panel.grid = element_blank(),
        axis.ticks = element_blank(),
        axis.text = element_blank(),
        # legend.key.height = unit(.4,"cm"),
        # legend.key.width = unit(1,"cm"),
        # legend.position = c(0.82,1.23),
        # legend.direction = "horizontal",
        # legend.title = element_blank(),
        legend.background = element_rect(fill = "#121212", color = "#121212"),
        text = element_text(color = "#c2c2c2"),
        #strip. arguments alter the facet labels
        strip.background = element_rect(fill = "#121212", color = "#c2c2c2"),
        strip.text = element_text(color = "#c2c2c2")) +
  #See note below:
  coord_quickmap() +
  facet_grid(vars(origin), switch = "y",
             labeller = labeller(origin = c("HOU" = "Departing from HOU",
                                            "IAH" = "Departing from IAH"))) +
  geom_text(data = text_df, aes(x = x, y = y, label = label),
            size = 2.1, color = "white")

ggsave("FlightMap3.png", height = 6, width = 7, bg = "#121212")

#Note:
#This sets the coordinate projection for the plot. coord_quickmap is not very 
#scientific, that is, not the most accurate representation of a 3D globe, but it 
#preserves straight lines which makes for easy plotting, and it's good enough if 
#you're not too far from the equator.
