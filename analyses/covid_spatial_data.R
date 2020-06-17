# mapping covid19

library(tidyverse)
library(tidycensus)
library(magrittr)

# load in latest data
nytimes_county <- read.csv(file = "NY_Times_COVID19_data/covid-19-data/us-counties.csv",
                           stringsAsFactors = T) 

# makes dates a date
nytimes_county$date <- as.Date(nytimes_county$date,format = "%Y-%m-%d")

# no duplicate data
# nrow(nytimes_county)
# nrow(dplyr::distinct(nytimes_county))
nytimes_county[nytimes_county$county == "New York City","fips"] <- 36061
29095 -> nytimes_county[nytimes_county$county == "Kansas City","fips"]

# fill in missing data
nytimes_complete <- nytimes_county %>%
  tidyr::complete(date = tidyr::full_seq(date, period = 1),
                  fips,
                  fill=list(cases=0,deaths=0))

# test_fips <- nytimes_complete %>% filter(date == "2020-04-01" & state=="Nevada") %$% fips 
# test_fips <- test_fips[!is.na(test_fips)]
# nytimes_complete %>% filter(fips == 36061) %>%
#   View()
# # 
# 
# 
# # fill in county and state data
# fips_data <- nytimes_county %>%
#   select(fips,county,state) %>%
#   dplyr::distinct()
# 
# rownames(fips_data) <- fips_data$fips
# 
# fips_data %>% filter(fips == 29095)

# new cases per time point
nytimes_complete <- nytimes_complete %>%
  group_by(fips) %>%
  mutate(lag_cases = cases - dplyr::lag(cases, default = 0)) %>%
  # will join with this data soon
  select(-county,-state) %>%
  filter(lag_cases>=0)

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
county_polygons <- left_join(counties,maps::county.fips,by = c("ID"="polyname"))


county_data <- left_join(county_data,county_polygons,by = "fips")

county_data <- county_data %>%
  filter(!is.na(fips))
# checking the join 
# county_data %>%
#   filter(fips ==  36061 &
#            date == "2020-01-21") %>%
#   View()


# county_data %>%
#   filter(state == "New York" &
#            date == "2020-01-22") %>%
#   View()
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

# county_data %>%
  # filter(lag_cases_over_pop > 0.01) %>% View()


# better to do a running mean
county_data <- county_data %>%
  group_by(fips) %>%
  mutate(roll_mean = zoo::rollmean(x= lag_cases_over_pop, k= 7, na.pad=T)) %>%
  filter(!is.na(roll_mean ))





# test_county <- county_data %>% filter(fips==1003)
# zoo::rollmean(test_county$lag_cases_over_pop,7,na.pad=T) == test_county$roll_mean

# test plot
ggplot(data=county_data %>% filter(date == "2020-03-30" )) +
  geom_sf(data = county_polygons) +
  geom_sf(aes(geometry=geom,fill=roll_mean)) + 
  scale_fill_gradient(low = "black",high= "red")



county_data_sf <- left_join(county_polygons,county_data,by = "fips")

ggplot(data=county_data_sf %>% filter(date == "2020-03-30" )) +
  geom_sf(data = county_polygons) +
  geom_sf(aes(geometry=geom.x,fill=roll_mean)) + 
  scale_fill_gradient(low = "black",high= "red")
# + 
  # scale_fill_gradientn(colors=viridis::viridis(10))
# 

# starting off with a cartogram
county_data_sf <- sf::st_transform(x = county_data_sf, 5070)

county_data_sf_NY <- county_data_sf[grep(x = county_data_sf$NAME,pattern = "New York"),]

# cart_data <- cartogram::cartogram_cont(county_data_sf %>% filter(date == "2020-03-30" ),weight="roll_mean",itermax=7)
cart_data_NY <- cartogram::cartogram_cont(county_data_sf_NY %>% filter(date == "2020-03-30" ),weight="roll_mean",itermax=100)


ggplot(data=cart_data_NY) +
  # geom_sf(data = county_polygons) +
  geom_sf(aes(geometry=geom.x,fill=roll_mean)) + 
  scale_fill_gradient(low = "black",high= "red")



