library(tidyverse)

commute <- read_rds(url("http://data.cwick.co.nz/commute.rds"))

# a subset of "Western" states for class
states <- c("or", "ak", "ca", "wa", "id", "nv")
commute_nw <- filter(commute, state %in% states)

#Filter for bikes only
commute_nw_bike <- commute_nw %>% 
  filter(transport_type == "Bicycle")

#Reorder the factors so that they appear in this order on the plot
commute_nw_bike$state_name <- as_factor(commute_nw_bike$state_name) %>%
  fct_relevel("Oregon", "Alaska", "Idaho", "California", "Washington", "Nevada")

#Plot
ggplot(commute_nw_bike) +
  #Color (fill) the bars based on a logical statement, (i.e. only color Oregon)
  geom_col(aes(x = state_name, y = prop, fill = (state_name == "Oregon"))) +
  labs(x = NULL, y = NULL, 
       title = "Commuters in Oregon bike more than in other Western states",
       subtitle = "Proportion of respondents who said they bike to work") +
  #Set the colors manually
  scale_fill_manual(values = c("#454647","#1e5e3a")) +
  #Minimal theme removes plot background and a few other small things
  theme_minimal() +
  #Remove legend, colors don't mean anything anyways
  theme(legend.position = "none",
        #We don't need vertical grid lines since we are comparing heights
        panel.grid.major.x = element_blank())

#save
ggsave("ST537_CommutePlot.png", last_plot(), height = 4, width = 6)

#Test edit
