# mapping covid19
# scratch script while I learn spatial data plotting 
setwd("COVID19_data_explore/")
library(tidyverse)
library(tidycensus)
library(magrittr)
library(zoo)
library(sf)

# load in latest data
nytimes_county <- read.csv(file = "NY_Times_COVID19_data/covid-19-data/us-counties.csv",
                           stringsAsFactors = T) 

# makes dates a date
nytimes_county$date <- as.Date(nytimes_county$date,format = "%Y-%m-%d")

# no duplicate data
# nrow(nytimes_county)
# nrow(dplyr::distinct(nytimes_county))


# https://guides.newman.baruch.cuny.edu/nyc_data
# The Bronx is Bronx County (ANSI / FIPS 36005)
# Brooklyn is Kings County (ANSI / FIPS 36047)
# Manhattan is New York County (ANSI / FIPS 36061)
# Queens is Queens County (ANSI / FIPS 36081)
# Staten Island is Richmond County (ANSI / FIPS 36085)


# need to eventually make NYC split into above FIPS


nyc_to_duplicate <- nytimes_county[nytimes_county$county == "New York City",]
bronx <- nyc_to_duplicate
bronx[,"county"] <- "Bronx"; bronx[,"fips"] <- 36005
brooklyn <- nyc_to_duplicate
brooklyn[,"county"] <- "Kings"; brooklyn[,"fips"] <- 36047 # no sleep 'til 
manhattan <- nyc_to_duplicate
manhattan[,"county"] <- "New York"; manhattan[,"fips"] <- 36061
queens <- nyc_to_duplicate
queens[,"county"] <- "Queens"; queens[,"fips"] <- 36081
staten <- nyc_to_duplicate
staten[,"county"] <- "Richmond Island"; staten[,"fips"] <- 36085


nytimes_county <- rbind(nytimes_county,bronx,brooklyn,manhattan,queens,staten)

# remove NYC
nytimes_county <- nytimes_county %>% 
  filter(county != "New York City")

nytimes_county[nytimes_county$county == "Kansas City","fips"] <- 29095 

# fill in missing data
nytimes_complete <- nytimes_county %>%
  tidyr::complete(date = tidyr::full_seq(nytimes_county$date, period = 1),
                  fips,
                  fill=list(cases=0,deaths=0))

# nytimes_complete %>% filter(county=="Suffolk" & state == "New York")

# new cases per time point
nytimes_complete <- nytimes_complete %>%
  group_by(fips) %>%
  mutate(lag_cases = cases - dplyr::lag(cases, default = 0)) %>%
  # will join with this data soon
  select(-county,-state) %>%
  filter(lag_cases>=0)

# View(nytimes_complete %>% filter(fips==1003))
# nytimes_complete <- nytimes_complete %>%
#   group_by(fips) %>%
#   mutate(lag_cases = cases - dplyr::lag(cases)) %>%
#   # will join with this data soon
#   # select(-county,-state) %>%
#   filter(lag_cases>=0)

# maybe want: county-resolution map of new cases / people in the county. 

# ran tidycensus::census_api_key() first with key sent to inbox. 
# More info: https://walker-data.com/tidycensus/articles/basic-usage.html

## Ran this once and commented out so I do not have to keep bothering
## the API
# county_pop <- 
#   tidycensus::get_acs(geography = "county",
#           variables = "B01003_001",
#           year = 2018,
#           geometry = TRUE)
# save(county_pop,file = "output_data/county_pop.RData")


load("output_data/county_pop.RData")

county_pop$GEOID <- as.numeric(county_pop$GEOID) # change to numeric to match NYTIMES 
# nytimes_data_lagged$fips

# need to match up certain NYT data choices with FIPS
# from https://github.com/seanchen7/COVID19-TRACKER/blob/master/data.R 
# case_raw[county=="New York City", FIPSCOUNTY:="36061"] # New York City data exception
# case_raw[county=="Kansas City", FIPSCOUNTY:="29095"] #Kansas city MO
# nytimes_county[nytimes_county$county == "New York City","fips"] <- 36061
# 29095 -> nytimes_county[nytimes_county$county == "Kansas City","fips"]

# joining the data by FIPS
county_data <- dplyr::left_join(nytimes_complete,county_pop, by= c("fips" = "GEOID"))

# remove the unknown counties 
county_data <- county_data %>%
  filter(!is.na(fips))

# get county polygons 
counties <- sf::st_as_sf(maps::map("county", plot = FALSE, fill = TRUE))
county_polygons <- dplyr::left_join(counties,maps::county.fips,by = c("ID"="polyname"))


county_data <- left_join(county_data,county_polygons,by = "fips")

county_data <- county_data %>%
  filter(!is.na(fips))

# 

# 
# county_data %>%
#   filter(county=="Trinity") %>% View()

## exploring geom_sf() 
# ggplot(data=county_data %>% filter(date == "2020-06-10" )) + 
#   geom_sf(aes(geometry=geom,fill=lag_cases/estimate)) 
# 
# ggplot(data=county_data %>% filter(date == "2020-03-01" )) + 
#   geom_sf(aes(geometry=geom,fill=lag_cases/estimate)) 
# 
# ggplot(data=county_data %>% filter(date == "2020-03-01" )) +
#   geom_sf(data = county_polygons) +
#   geom_sf(aes(geometry=geom,fill=lag_cases/estimate)) 
# 
# ggplot(data=county_data %>% filter(date == "2020-05-01" & state== "Massachusetts")) + 
#   geom_sf(aes(geometry=geom,fill=lag_cases/estimate))


# county_data %>% filter(date == "2020-05-01" & state== "Massachusetts") %>% View()

# double rows... 
nrow(county_data %>% select(date,fips,cases,NAME))
nrow(dplyr::distinct(county_data %>% select(date,fips,cases,NAME)))

# deduplicate a county that got added in
county_data <- county_data[!duplicated(county_data %>% select(date,fips,cases,NAME)),]



# View(county_data[duplicated(county_data %>% select(date,fips,cases,NAME)),])


# probably have to add in the "zero" data for earlier dates. 

county_data$lag_cases_over_pop <- county_data$lag_cases/county_data$estimate

# nyc need to combine boroughs



NYC_index <- which(county_data$NAME %in% c("Kings County, New York","Richmond County, New York",
                                           "Bronx County, New York","New York County, New York",
                                           "Queens County, New York"))
NYC_pop <- as.matrix(county_pop[county_pop$NAME %in% c("Kings County, New York","Richmond County, New York",
                                                       "Bronx County, New York","New York County, New York",
                                                       "Queens County, New York"),])
NYC_pop <- sum(unlist(NYC_pop[,"estimate"]))


county_data[NYC_index,"lag_cases_over_pop"] <- county_data[NYC_index,"lag_cases"]/NYC_pop






# better to do a running mean
county_data <- county_data %>%
  group_by(fips) %>%
  mutate(roll_mean = zoo::rollmean(x= lag_cases_over_pop, k= 7, na.pad=T)) %>%
  mutate(roll_new_cases = zoo::rollmean(x = lag_cases, k=7, na.pad=T)) %>%
  filter(!is.na(roll_mean ))




# test_county <- county_data %>% filter(fips==1003)
# zoo::rollmean(test_county$lag_cases_over_pop,7,na.pad=T) == test_county$roll_mean
# 
# # test plot
# ggplot(data=county_data %>% filter(date == "2020-03-30" )) +
#   geom_sf(data = county_polygons) +
#   geom_sf(aes(geometry=geom,fill=roll_mean)) + 
#   scale_fill_gradient(low = "black",high= "red")



county_data_sf <- left_join(county_polygons,county_data,by = "fips")

ggplot(data=county_polygons[grep("new york",county_polygons$ID),]) +
  geom_sf()


# starting off with a cartogram
county_data_sf <- sf::st_transform(x = county_data_sf, 5070)

# date_to_plot <- "2020-02-12"




data_subset_all <- county_data_sf %>% 
  dplyr::filter(date == as.Date("2020-06-23")) 



# ny_county_polygons <- county_polygons[grep(pattern = "new york",x = county_polygons$ID),]

data_subset_just_params <- data_subset_all[,c("ID.x","cases","lag_cases","roll_mean","roll_new_cases")]

data_subset_just_params <- data_subset_just_params %>%
  mutate(ID=ID.x)

# ny_county_polygons <- sf::st_join(ny_county_polygons,data_subset_just_params,join="ID")
polygons <- left_join(county_polygons,as.data.frame(data_subset_just_params),"ID")


polygons[which(is.na(polygons$roll_mean)),]

polygons <- polygons %>%
  filter(!is.na(roll_mean))


library(rayrender)
# 
# scene <- generate_ground(depth=0,spheresize=1000, 
#                          material=diffuse(color="#000000",
#                                           noise=1/10,
#                                           noisecolor = "#654321")) %>%
#   add_object(extruded_polygon(polygons, center = T,data_column_top = "roll_new_cases",
#                               scale_data = 1/max(polygons$roll_new_cases,na.rm = T)*5,
#                               material= dielectric(color="darkgreen"#,
#                                                    # attenuation = c(1,1,0.3)/200)
#                               ),
#                               material_id = 1)) %>%
#   add_object(sphere(y=20,x=0,z = 0,radius=7,
#                     material=light(color="lightblue",intensity=70)))


scene <- generate_ground(depth=0,spheresize=1000, 
                         material=diffuse(color="#000000",
                                          noise=1/10,
                                          noisecolor = "#654321")) %>%
  add_object(extruded_polygon(polygons, center = T,data_column_top = "roll_new_cases",
                              scale_data = 1/max(polygons$roll_new_cases,na.rm = T)*5,
                              material= diffuse(color="forestgreen"))) %>%
  add_object(sphere(y=30,x=0,z = 0,radius=5,
                    material=light(color="lightblue",intensity=50)))

# render_scene(scene = scene, parallel=TRUE,samples=600,
#              lookfrom = c(50,15,-15),fov=60,width=500, height=500)


frames = 360

camerax=-35*cos(seq(0,360,length.out = frames+1)[-frames-1]*pi/180)
cameraz=35*sin(seq(0,360,length.out = frames+1)[-frames-1]*pi/180)

# render_scene(scene = scene, parallel=TRUE,samples=600,
#              lookfrom = c(camerax[1],10,cameraz[1]),fov=60,width=500, height=500)


for(i in 1:frames) {
  
  render_scene(scene, width=500, height=500, fov=60,
               lookfrom = c(camerax[i], 7, cameraz[i]),
               samples = 400, parallel = TRUE,
               filename=glue::glue("output_data/figures/tests/frames/USA_diffuse{i}.png"))
  print(i)
}

av::av_encode_video(glue::glue("output_data/figures/tests/frames/USA_diffuse{1:(frames-1)}.png"),
                    framerate=60, output = "output_data/figures/tests/USA_rotate_diffuse.mp4")
file.remove(glue::glue("output_data/figures/tests/frames/USA_diffuse{1:(frames-1)}.png"))


# # 
# av::av_capture_graphics(expr = {
#   for(i in 1:frames) {
#     render_scene(scene, width=500, height=500, fov=60,
#                  lookfrom = c(camerax[i], 7, cameraz[i]),samples = 400, parallel = TRUE)
#     print(i)
#   }
# }, width=500,height=500, framerate = 60, output = "output_data/figures/tests/USA_rotate_diffuse.mp4")
# 
