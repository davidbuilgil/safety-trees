
###########################################
#                                         #
# Perceived safety and greenery in London #
#                                         #
#      D. Buil-Gil and R. Solymosi        #
#    for book chapter led by M. Sery      #
#                                         #
#             03/04/2021                  #
#                                         #
###########################################

rm(list=ls())
options(scipen=999)

#load libraries
library(ggplot2)
library(sf)
library(here)
library(ggpubr)
library(tidyr)
library(dplyr)

#load Place Pulse data
#pp_data <- read.csv('https://ndownloader.figshare.com/files/21739137')

#select Place Pulse data about safety in London
#pp_data <- pp_data %>%
#  filter(study_question == "safer") %>%
#  filter(place_name_right == "London" | place_name_left == "London")

#rapid alternative (load from Github)
pp_data <- read.csv(here("data/pp_data.csv"))

#duplicate comparisons in which both images are from London
lon_v_others <- pp_data %>%
  # select where only one image is from London
  filter((place_name_right == "London" & place_name_left != "London") | 
           (place_name_right != "London" & place_name_left == "London")) %>% 
  # get coordinates from London side, label win, and ID of image
  mutate(lat_Lon   = if_else(place_name_right == "London", 
                             lat_right, lat_left),
         long_Lon  = if_else(place_name_right == "London", 
                             long_right, long_left),
         win       = if_else((place_name_left == "London" & 
                                choice == "left") |
                               (place_name_right == "London" & 
                                  choice == "right"), 1, 0),
         unique_id = if_else(place_name_right == "London", right, left))

#when two images are from London: create dataset from London images on right side
right_side <- pp_data %>%
  filter(place_name_right == "London" & place_name_left == "London") %>% 
  mutate(lat_Lon  = lat_right,
         long_Lon  = long_right, 
         win       = if_else(choice == "right", 1, 0), 
         unique_id = right)

#when two images are from London: create dataset from London images on left side
left_side <- pp_data %>%
  filter(place_name_right == "London" & place_name_left == "London") %>% 
  mutate(lat_Lon  = lat_left,
         long_Lon  = long_left, 
         win       = if_else(choice == "left", 1, 0), 
         unique_id = left)

#merge the three files together
pp_london <- rbind(lon_v_others, left_side, right_side) 

#download London tree canopy data
download.file("https://data.london.gov.uk/download/curio-canopy/1e138cef-bdb5-4ad9-8bc8-83beb7ac6b16/gla-hexagon-grid-canopy-cover.kml",
              destfile = here("data/gla-hexagon-grid-canopy-cover.kml"))

#load London tree canopy data
trees <- st_read("data/gla-hexagon-grid-canopy-cover.kml")

#create column with percentage vegetation in hexagons
trees <- trees %>%
  separate(Description, sep = "<br>", into = c("a", "b", "c", "d", "e", "f", "cover")) %>%
  mutate(cover = sub(' <b>percentage_vegetation_cover:</b> ', '', cover),
         cover = as.numeric(cover),
         cover = ifelse(cover > 99.999, 100, cover),
         hex_id = row_number()) %>%
  select(hex_id, cover, geometry)

#geocode Place Pulse votes
pp_data_sf <- st_as_sf(pp_london, coords = c("lat_Lon", "long_Lon"))

#match their CRS
st_crs(pp_data_sf) <- st_crs(trees)

#check if CRS is the same in both layers
st_crs(pp_data_sf) == st_crs(trees)

#calculate the proportion of wins in each hexagon
pp_data_hex <- st_intersection(trees, pp_data_sf) %>%
  group_by(hex_id) %>%
  summarise(winscore = mean(win, na.rm = TRUE) * 100,
            num_votes = n()) %>%
  st_set_geometry(NULL)

# merge hexagons and Place Pulse votes
pp_wins <- left_join(trees, pp_data_hex, by = c("hex_id"))

#visualise maps of vegetation and safety

trees <- ggplot(data = pp_wins)+
  labs(title = "Vegetation")+
  geom_sf(aes(fill = cover), color = NA)+
  theme_void()+
  scale_fill_viridis_c(option ="viridis", begin = 1, end = 0, na.value = "lightgrey",
                       name = "%") +
  theme(axis.title = element_text(size = 10),
        plot.title = element_text(size = 15))

safety <- ggplot(data = pp_wins)+
  labs(title = "Perceived safety")+
  geom_sf(aes(fill = winscore), color = NA)+
  theme_void()+
  scale_fill_viridis_c(option ="viridis", begin = 1, end = 0, na.value = "lightgrey",
                       name = "%") +
  theme(axis.title = element_text(size = 10),
        plot.title = element_text(size = 15))

ggarrange(trees, safety, ncol = 2)

#visualise correlation
lm <- pp_wins %>%
  filter(num_votes > 9) %>%
  ggplot(aes(x = winscore, y = cover, colour = "red")) +
  geom_point(alpha = 0.3,  position = position_jitter()) + 
  stat_smooth(method = "lm") +
  coord_fixed() +
  labs(x = "Perceived safety", 
       y = "Vegetation",
       title = "Correlation between vegetation \n and perceived safety",
       subtitle = "Hexagons with 10 votes or more")+
  theme_light() +
  theme(legend.position = "none")

#create plot for paper
ggarrange(lm,
          ggarrange(trees, safety, nrow = 2),
          ncol = 2
) 

#calculate correlation
cor.test(pp_wins$cover, pp_wins$winscore, method = "spearman")
cor.test(pp_wins$cover, pp_wins$winscore, method = "pearson")

pp_wins2 <- pp_wins %>%
  mutate(winscore = ifelse(num_votes < 10, NA, winscore))

cor.test(pp_wins2$cover, pp_wins2$winscore, method = "spearman")
cor.test(pp_wins2$cover, pp_wins2$winscore, method = "pearson")
