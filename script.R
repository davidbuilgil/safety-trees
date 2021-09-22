
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
library(tibble)
library(cowplot)
library(ggspatial)
library(sp)
library(spdep)
library(spatialreg)
library(spgwr)

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
 # (the error is because you could have more columns with <br> as separator but only capture 7)
trees <- trees %>%
  separate(Description, sep = "<br>", into = c("a", "b", "c", "d", "e", "f", "cover")) %>%
  select(cover, geometry) %>% 
  mutate(cover = sub(' <b>percentage_vegetation_cover:</b> ', '', cover),
         cover = round(as.numeric(cover), 2),
         # cover = ifelse(cover > 99.999, 100, cover),
         hex_id = row_number())


plot(st_geometry(trees))
  


#geocode Place Pulse votes
pp_data_sf <- st_as_sf(pp_london, coords = c("lat_Lon", "long_Lon"))

#match their CRS
st_crs(pp_data_sf) <- st_crs(trees)

#check if CRS is the same in both layers
st_crs(pp_data_sf) == st_crs(trees)


plot(st_geometry(trees))
plot(st_geometry(pp_data_sf), add = T, col = 'red')

#calculate the proportion of wins in each hexagon this kept crashing: 
# pp_data_hex <- st_intersection(trees, pp_data_sf) %>%
#   group_by(hex_id) %>%
#   summarise(winscore = mean(win, na.rm = TRUE) * 100,
#             num_votes = n()) %>%
#   st_set_geometry(NULL)


# this one works
pp_data_hex <- pp_data_sf %>% 
  st_join(trees, ., left = FALSE) %>% 
  group_by(hex_id) %>% 
    summarise(winscore = round(mean(win, na.rm = TRUE) * 100,2),
              num_votes = n()) %>% 
  st_drop_geometry()

# merge hexagons and Place Pulse votes
pp_wins <- left_join(trees, pp_data_hex, by = c("hex_id"))

#visualise maps of vegetation and safety

trees_plot <- ggplot(data = pp_wins)+
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

ggarrange(trees_plot, safety, ncol = 2)

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



# bivariate map

# remove NAs
pp_wins_no_na <- pp_wins %>% 
  filter(!is.na(winscore))

# Bin variables of interest

wins_breaks <- quantile(pp_wins_no_na$winscore,
                         probs = seq(0,1, by = 1/3), # probabilities
                         na.rm=TRUE, # remove NAs
                         names=TRUE, # keep names attribute
                         include.lowest=TRUE) # include value equal to lowest ‘breaks’ value

cover_breaks <- quantile(pp_wins_no_na$cover, probs = seq(0,1, by = 1/3), na.rm=TRUE, names=TRUE, include.lowest=TRUE)

pp_wins_no_na <- pp_wins_no_na %>%
  mutate(wins_quantiles = cut(winscore, breaks = wins_breaks, include.lowest = TRUE),
         cover_quantiles = cut(cover, breaks = cover_breaks, include.lowest = TRUE))

# create bivariate variable

pp_wins_no_na <- pp_wins_no_na %>%
  mutate(group = paste(as.numeric(wins_quantiles), "-", as.numeric(cover_quantiles)))


# check we good
# pp_wins_no_na %>% st_drop_geometry() %>% group_by(group) %>%  count()


# make colour scheme


bivariate_color_scale_1 <- tibble(
  "3 - 3" = "#574249", # high - high
  "2 - 3" = "#985356",
  "1 - 3" = "#c85a5a", # low - high
  "3 - 2" = "#627f8c",
  "2 - 2" = "#ad9ea5", # medium - medium
  "1 - 2" = "#e4acac",
  "3 - 1" = "#64acbe", # high - low
  "2 - 1" = "#b0d5df",
  "1 - 1" = "#e8e8e8" # low - low
) %>%
  gather("group", "fill_col")


# join colour scheme

pp_wins_no_na <- left_join(pp_wins_no_na, bivariate_color_scale_1, by = "group")

# create legend

bivariate_color_scale <- bivariate_color_scale_1 %>%
  separate(group, into = c("wins", "cover"), sep = " - ")

legend <- ggplot() +
  geom_tile( data = bivariate_color_scale,
             aes(x = wins, y = cover, fill = fill_col)) +
  scale_fill_identity() +
  labs(x = "Higher % wins ->",
       y = "Higher % cover ->") +
  theme(axis.title = element_text(size = 9)) + # makes text small for
  # adding legend to map
  coord_fixed() # forces a specified ratio to create quadratic tile

#check up on it
# legend

# now make the map

map <- ggplot(pp_wins_no_na) +
  annotation_map_tile("stamenbw", zoom = 12) + 
  geom_sf(aes( fill = fill_col)) +
  scale_fill_identity() +
  theme_void()

ggdraw() +
  draw_plot(map, 0, 0, 1, 1) + 
  draw_plot(legend, 0.02, 0.725, 0.25, 0.25)
 
# create distance based weights matrix

pp_proj <- st_transform(pp_wins_no_na, crs = 27700) # project the hex layer

pp_sp <- as(pp_proj, "Spatial") #turn to sp object because I'm oldschool

xy <- coordinates(pp_sp) # get coords

nearest_n <- knearneigh(xy) # find nearest neighbours for each centroid coordiate pair
nb_nearest_n <- knn2nb(nearest_n) # transform into nb class object

radius <- max(unlist(nbdists(nb_nearest_n, xy)))
radius

#the max radius to avoid isolates is 700 meters, sounds reasonable so we use this to make our matrix

nb_distance <- dnearneigh(xy, 0, radius)

summary(nb_distance) #check it out

#great, now use to make spatial weights matrix
wm_distance <- nb2mat(nb_distance, style='W')
rwm <- mat2listw(wm_distance, style='W')


# fit a non spatial regression

non_spatial_model <- lm(winscore ~ cover, data = pp_sp)
summary(non_spatial_model) # significant but tiny effect size

# check moran's I for residuals
lm.morantest(non_spatial_model, rwm, alternative="two.sided") 
# yes obvs we have spatial autocorrelation in regression results

# check what model to use
summary(lm.LMtests(non_spatial_model, rwm, test = c("LMerr","LMlag","RLMerr","RLMlag")))

# use lag model

lag_model <- lagsarlm(winscore ~ cover, data = pp_sp, rwm)

summary(lag_model)

# make coefficient interpretable

W <- as(rwm, "CsparseMatrix")
trMC <- trW(W, type="MC")
lag_model_impacts <- impacts(lag_model, tr=trMC, R=100)
summary(lag_model_impacts, zstats=TRUE, short=TRUE)

# Direct effect: average impact of cover on perception in same hexagon
# Indirect effect: average impact of cover in neighbour hexagons on outcome
# Total effect: both together


# still not a big effect size, basically increase in 1% cover is increase of about 0.03% of safety wins. 
# Does relationship change in different areas of London?


# Map residuals

pp_sp$resids <- residuals(lag_model)
tmap::qtm(pp_sp, fill = "resids")

#uhh don't know... let's check a gwr anyway

# get centroid coordinates
pp_wins_no_na <- pp_wins_no_na %>%
  mutate(cent_coords = st_coordinates(st_centroid(.)))

# get bandwidth
gwr_bandwidth <- gwr.sel(winscore ~ cover,
                         data = pp_wins_no_na,
                         coords = pp_wins_no_na$cent_coords,
                         adapt = T)


gwr_bandwidth

#build gwr model

gwr_model <- gwr(winscore ~ cover, data = pp_wins_no_na,
                coords = pp_wins_no_na$cent_coords,
                adapt=gwr_bandwidth,
                hatmatrix=TRUE,
                se.fit=TRUE)
gwr_model

#get results
gwr_results <- as.data.frame(gwr_model$SDF)

#bind with data

gwr_results <- cbind(pp_wins_no_na, gwr_results)


#now map coefficients as well as standard error

coefficient_map <- ggplot() + 
  annotation_map_tile("stamenbw", zoom = 12) + 
  geom_sf(data = gwr_results, aes(fill = cover.1), color = NA) + 
  labs(title = "Coefficient")+
  theme_void()+
  scale_fill_viridis_c(option ="viridis", begin = 1, end = 0, na.value = "lightgrey",
                       name = "") +
  theme(axis.title = element_text(size = 10),
        plot.title = element_text(size = 15))

se_map <- ggplot() + 
  annotation_map_tile("stamenbw", zoom = 12) + 
  geom_sf(data = gwr_results, aes(fill = cover_se), color = NA) + 
  labs(title = "Standard Error")+
  theme_void()+
  scale_fill_viridis_c(option ="viridis", begin = 1, end = 0, na.value = "lightgrey",
                       name = "") +
  theme(axis.title = element_text(size = 10),
        plot.title = element_text(size = 15))

ggarrange(coefficient_map, se_map, ncol = 2)


# We can see in the coefficient map some regional patterns in the coefficient of the effect of green space cover on perceived safety. 
# Specifically, looking at North East London we see the relationship change direction; whereby in this area
# green space coverage is *negatively* associated with perceived safety, while elsewhere the relationship is positive.
# This suggests the relationship between green space and perceived safety is context-dependent, and while
# green space can offer an increase in perceived safety, in some cases, it might achieve the inverse. 
# NOte: it is important to consider not only
# the coefficients, but also the standard errors associated with them, so we can get some ideas
# behind the reliability of these estimates. 