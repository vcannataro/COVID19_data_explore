latest <- read.csv("JHU_COVID19_data/COVID-19/csse_covid_19_data/csse_covid_19_daily_reports/03-28-2020.csv")
library(tidyverse)


# find column names
data_files <- dir(path = "JHU_COVID19_data/COVID-19/csse_covid_19_data/csse_covid_19_daily_reports/",
                  pattern = ".csv",full.names = T)

col_names_in_csv <- vector(mode = "list",length = length(data_files))
date_style <- vector(mode = "list",length = length(data_files))

for(file_ind in 1:length(data_files)){
 this_file <- read.csv(file = data_files[file_ind])
 col_names_in_csv[[file_ind]] <- colnames(this_file)
 date_style[[file_ind]] <- as.character(this_file %>% select(ends_with("Update")) %>% .[1,])
}

## Seems March 22 has a different date style, but consistent dating style before 
## and after this date
# date_mar_22 <- read.csv(file = "JHU_COVID19_data/COVID-19/csse_covid_19_data/csse_covid_19_daily_reports/03-22-2020.csv")
# date_mar_22$Last_Update


# What columns do we want?

# What do we have?
# col_names_in_csv

# before 03-01-2020.csv
# [1] "Province.State" "Country.Region" "Last.Update"    "Confirmed"      "Deaths"        
# [6] "Recovered"  



# 03-01-2020.csv through 03-21-2020.csv

# [1] "Province.State" "Country.Region" "Last.Update"    "Confirmed"      "Deaths"        
# [6] "Recovered"      "Latitude"       "Longitude" 

# 03-22-2020.csv to end 
# [1] "FIPS"           "Admin2"         "Province_State" "Country_Region" "Last_Update"   
# [6] "Lat"            "Long_"          "Confirmed"      "Deaths"         "Recovered"     
# [11] "Active"         "Combined_Key"  



# We want, if they have it: FIPS. Admin2, State, Country, Last Update, Lat, Long, Confirmed, Deaths, Recovered, Active

# 
# data_files <- dir(path = "JHU_COVID19_data/COVID-19/csse_covid_19_data/csse_covid_19_daily_reports/",
#                   pattern = ".csv",full.names = T)
# data_files_name <- dir(path = "JHU_COVID19_data/COVID-19/csse_covid_19_data/csse_covid_19_daily_reports/",
#                   pattern = ".csv",full.names = F)
# 
# # getting USA states into a df 
# state_names <- data.frame(state_name = state.name, state_abb = state.abb,stringsAsFactors = F)
# state_names <- rbind(state_names,c("D.C.","D.C."))
# rownames(state_names) <- state_names$state_abb
# 
# 
#  
# 
# combined_CDR_data <- NULL
# 
# for(file_ind in 1:length(data_files)){
#   
#   this_file <- data_files_name[file_ind]
#   date <- gsub(pattern = ".csv",replacement = "",x = this_file)
#   date <- as.Date(date,format = "%m-%d-%y")
#   
#   
#   # first data structure 
#   if(date < as.Date("2020-02-01")){
#     
#     these_data <- read.csv(file = data_files[file_ind],stringsAsFactors = F)
#     
#     this_df <- data.frame(State=these_data$Province.State, 
#                           Country = these_data$Country.Region, 
#                           Updated = these_data$Last.Update, 
#                           Confirmed = these_data$Confirmed, 
#                           Deaths = these_data$Deaths, 
#                           Recovered = these_data$Recovered, 
#                           Active = NA, 
#                           Lat = NA, Long = NA, Admin2 = NA, FIPS = NA,
#                           stringsAsFactors = F)
#     
#     this_df$Updated <- lubridate::mdy_hm(this_df$Updated)
#     
#     
#     combined_CDR_data <- rbind(combined_CDR_data, this_df)
#     
#   }
#   # Feb 1 started new US state format ---- 
#   
#   if(date == as.Date("2020-02-01")){
#     
#     these_data <- read.csv(file = data_files[file_ind],stringsAsFactors = F)
#     
#     town_and_state <- these_data$Province.State[which(these_data$Country.Region=="US")]
#     
#     state_vec <- rep(NA,length=length(town_and_state))
#     admin_our_vec <- rep(NA,length=length(town_and_state))
#     
#     state_split <- strsplit(x = as.character(town_and_state),split = ",")
#     
#     # loop through to bring county --> state level 
#     for(state_ind in 1:length(state_split)){
#       if(length(state_split[[state_ind]])==1){
#         state_vec[state_ind] <- state_split[[state_ind]]
#       }else{
#         state_vec[state_ind] <- state_names[trimws(state_split[[state_ind]][2]),"state_name"]
#         admin_our_vec[state_ind] <- state_split[[state_ind]][1]
#       }
#     }
#     
#     
#     this_df <- data.frame(State=these_data$Province.State, 
#                           Country = these_data$Country.Region, 
#                           Updated = these_data$Last.Update, 
#                           Confirmed = these_data$Confirmed, 
#                           Deaths = these_data$Deaths, 
#                           Recovered = these_data$Recovered, 
#                           Active = NA, 
#                           Lat = NA, Long = NA, Admin2 = NA, FIPS = NA,
#                           stringsAsFactors = F)
#     
#     this_df$State[which(this_df$Country=="US")] <- state_vec
#     this_df$Admin2[which(this_df$Country=="US")] <- admin_our_vec
# 
#         
#     this_df$Updated <- lubridate::mdy_hm(this_df$Updated)
#     
#     
#     combined_CDR_data <- rbind(combined_CDR_data, this_df)
#   
#   }
#   
#   # Feb 2 started new time format ----- 
#   if(date >= as.Date("2020-02-02") & 
#      date < as.Date("2020-03-01")){
#     # break
#     these_data <- read.csv(file = data_files[file_ind],stringsAsFactors = F)
#     
#     
#     town_and_state <- these_data$Province.State[which(these_data$Country.Region=="US")]
#     town_and_state <- gsub(pattern = " (From Diamond Princess)",replacement = "",x = town_and_state,fixed = T)
#     
#     state_vec <- rep(NA,length=length(town_and_state))
#     admin_our_vec <- rep(NA,length=length(town_and_state))
#     
#     state_split <- strsplit(x = as.character(town_and_state),split = ",")
#     
#     # loop through to bring county --> state level 
#     for(state_ind in 1:length(state_split)){
#       if(length(state_split[[state_ind]])==1){
#         state_vec[state_ind] <- state_split[[state_ind]]
#       }else{
#         state_vec[state_ind] <- state_names[trimws(state_split[[state_ind]][2]),"state_name"]
#         admin_our_vec[state_ind] <- state_split[[state_ind]][1]
#       }
#     }
#     
#     this_df <- data.frame(State=these_data$Province.State, 
#                           Country = these_data$Country.Region, 
#                           Updated = these_data$Last.Update, 
#                           Confirmed = these_data$Confirmed, 
#                           Deaths = these_data$Deaths, 
#                           Recovered = these_data$Recovered, 
#                           Active = NA, 
#                           Lat = NA, Long = NA, Admin2 = NA, FIPS = NA,stringsAsFactors = F)
#     
#     this_df$State[which(this_df$Country=="US")] <- state_vec
#     this_df$Admin2[which(this_df$Country=="US")] <- admin_our_vec
#     
#     this_df$Updated <- strptime(this_df$Updated, format = '%Y-%m-%dT%H:%M:%S',tz = "UTC")
#     
#     
#     combined_CDR_data <- rbind(combined_CDR_data, this_df)
#     
#   }
#   
#   
#   
#   # Consistent header until March 22 ---- 
#   if(date >= as.Date("2020-03-01") & 
#      date <= as.Date("2020-03-21")){
#     # break
#     these_data <- read.csv(file = data_files[file_ind],stringsAsFactors = F)
#     
#     
#     town_and_state <- these_data$Province.State[which(these_data$Country.Region=="US")]
#     
#     town_and_state <- gsub(pattern = " (From Diamond Princess)",replacement = "",x = town_and_state,fixed = T)
#     
#     state_vec <- rep(NA,length=length(town_and_state))
#     admin_our_vec <- rep(NA,length=length(town_and_state))
#     
#     state_split <- strsplit(x = as.character(town_and_state),split = ",")
#     
#     # loop through to bring county --> state level 
#     for(state_ind in 1:length(state_split)){
#       if(length(state_split[[state_ind]])==1){
#         state_vec[state_ind] <- state_split[[state_ind]]
#       }else{
#         state_vec[state_ind] <- state_names[trimws(state_split[[state_ind]][2]),"state_name"]
#         admin_our_vec[state_ind] <- state_split[[state_ind]][1]
#       }
#     }
#     
#     this_df <- data.frame(State=these_data$Province.State, 
#                           Country = these_data$Country.Region, 
#                           Updated = these_data$Last.Update, 
#                           Confirmed = these_data$Confirmed, 
#                           Deaths = these_data$Deaths, 
#                           Recovered = these_data$Recovered, 
#                           Active = NA, 
#                           Lat = these_data$Latitude, Long = these_data$Longitude,
#                           Admin2 = NA, FIPS = NA,stringsAsFactors = F)
#     
#     this_df$State[which(this_df$Country=="US")] <- state_vec
#     this_df$Admin2[which(this_df$Country=="US")] <- admin_our_vec
#     
#     this_df$Updated <- strptime(this_df$Updated, format = '%Y-%m-%dT%H:%M:%S',tz = "UTC")
#     
#     
#     combined_CDR_data <- rbind(combined_CDR_data, this_df)
#     
#   }
#   
#   
#   # March 22 has a unique date structure -----
#   if(date == as.Date("2020-03-22")){
#     # break
#     these_data <- read.csv(file = data_files[file_ind],stringsAsFactors = F)
#     
#     
#     # town_and_state <- these_data$Province.State[which(these_data$Country.Region=="US")]
#     # 
#     # town_and_state <- gsub(pattern = " (From Diamond Princess)",replacement = "",x = town_and_state,fixed = T)
#     # 
#     # state_vec <- rep(NA,length=length(town_and_state))
#     # admin_our_vec <- rep(NA,length=length(town_and_state))
#     # 
#     # state_split <- strsplit(x = as.character(town_and_state),split = ",")
#     # 
#     # # loop through to bring county --> state level 
#     # for(state_ind in 1:length(state_split)){
#     #   if(length(state_split[[state_ind]])==1){
#     #     state_vec[state_ind] <- state_split[[state_ind]]
#     #   }else{
#     #     state_vec[state_ind] <- state_names[trimws(state_split[[state_ind]][2]),"state_name"]
#     #     admin_our_vec[state_ind] <- state_split[[state_ind]][1]
#     #   }
#     # }
#     # 
#     this_df <- data.frame(State=these_data$Province_State, 
#                           Country = these_data$Country_Region, 
#                           Updated = these_data$Last_Update, 
#                           Confirmed = these_data$Confirmed, 
#                           Deaths = these_data$Deaths, 
#                           Recovered = these_data$Recovered, 
#                           Active = these_data$Active, 
#                           Lat = these_data$Lat, Long = these_data$Long_,
#                           Admin2 = these_data$Admin2, FIPS = these_data$FIPS,stringsAsFactors = F)
#     
#     # this_df$State[which(this_df$Country=="US")] <- state_vec
#     # this_df$Admin2[which(this_df$Country=="US")] <- admin_our_vec
#     # 
#     this_df$Updated <- strptime(this_df$Updated, format = '%m/%d/%y %H:%M',tz = "UTC")
#     
#     
#     combined_CDR_data <- rbind(combined_CDR_data, this_df)
#     
#   }
#   
#   
#   
#   # After March 22  -----
#   if(date > as.Date("2020-03-22")){
#     # break
#     these_data <- read.csv(file = data_files[file_ind],stringsAsFactors = F)
#     
#     
#     # town_and_state <- these_data$Province.State[which(these_data$Country.Region=="US")]
#     # 
#     # town_and_state <- gsub(pattern = " (From Diamond Princess)",replacement = "",x = town_and_state,fixed = T)
#     # 
#     # state_vec <- rep(NA,length=length(town_and_state))
#     # admin_our_vec <- rep(NA,length=length(town_and_state))
#     # 
#     # state_split <- strsplit(x = as.character(town_and_state),split = ",")
#     # 
#     # # loop through to bring county --> state level 
#     # for(state_ind in 1:length(state_split)){
#     #   if(length(state_split[[state_ind]])==1){
#     #     state_vec[state_ind] <- state_split[[state_ind]]
#     #   }else{
#     #     state_vec[state_ind] <- state_names[trimws(state_split[[state_ind]][2]),"state_name"]
#     #     admin_our_vec[state_ind] <- state_split[[state_ind]][1]
#     #   }
#     # }
#     # 
#     this_df <- data.frame(State=these_data$Province_State, 
#                           Country = these_data$Country_Region, 
#                           Updated = these_data$Last_Update, 
#                           Confirmed = these_data$Confirmed, 
#                           Deaths = these_data$Deaths, 
#                           Recovered = these_data$Recovered, 
#                           Active = these_data$Active, 
#                           Lat = these_data$Lat, Long = these_data$Long_,
#                           Admin2 = these_data$Admin2, FIPS = these_data$FIPS,stringsAsFactors = F)
#     
#     # this_df$State[which(this_df$Country=="US")] <- state_vec
#     # this_df$Admin2[which(this_df$Country=="US")] <- admin_our_vec
#     # 
#     this_df$Updated <- strptime(this_df$Updated, format = '%Y-%m-%d %H:%M:%S',tz = "UTC")
#     
#     
#     combined_CDR_data <- rbind(combined_CDR_data, this_df)
#     
#   }
#   
# }
# 
# 
# 
# 
# 
# 
# 
# 
# 
# 
#  
# 
# 
# 
# 
# 
# 
# 
# 
# 
# 
# 
# 
# 
# 
# 
# 
# 
# 
