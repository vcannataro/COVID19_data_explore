# JHU data explore 
# 

library(tidyverse)

# data obtained from https://github.com/CSSEGISandData/COVID-19.git


# New cleaning pipeline given March 22, 2020 update

data_files <- dir(path = "JHU_COVID19_data/COVID-19/csse_covid_19_data/csse_covid_19_daily_reports/",
                  pattern = ".csv",full.names = T)
data_files_name <- dir(path = "JHU_COVID19_data/COVID-19/csse_covid_19_data/csse_covid_19_daily_reports/",
                       pattern = ".csv",full.names = F)

# getting USA states into a df 
state_names <- data.frame(state_name = state.name, state_abb = state.abb,stringsAsFactors = F)
state_names <- rbind(state_names,c("D.C.","D.C."))
rownames(state_names) <- state_names$state_abb




combined_CDR_data <- NULL

for(file_ind in 1:length(data_files)){
  
  this_file <- data_files_name[file_ind]
  date <- gsub(pattern = ".csv",replacement = "",x = this_file)
  date <- as.Date(date,format = "%m-%d-%y")
  
  
  # first data structure 
  if(date < as.Date("2020-02-01")){
    
    these_data <- read.csv(file = data_files[file_ind],stringsAsFactors = F)
    
    this_df <- data.frame(State=these_data$Province.State, 
                          Country = these_data$Country.Region, 
                          Updated = these_data$Last.Update, 
                          Confirmed = these_data$Confirmed, 
                          Deaths = these_data$Deaths, 
                          Recovered = these_data$Recovered, 
                          Active = NA, 
                          Lat = NA, Long = NA, Admin2 = NA, FIPS = NA,
                          stringsAsFactors = F)
    
    this_df$Updated <- lubridate::mdy_hm(this_df$Updated)
    
    
    combined_CDR_data <- rbind(combined_CDR_data, this_df)
    
  }
  # Feb 1 started new US state format ---- 
  
  if(date == as.Date("2020-02-01")){
    
    these_data <- read.csv(file = data_files[file_ind],stringsAsFactors = F)
    
    town_and_state <- these_data$Province.State[which(these_data$Country.Region=="US")]
    
    state_vec <- rep(NA,length=length(town_and_state))
    admin_our_vec <- rep(NA,length=length(town_and_state))
    
    state_split <- strsplit(x = as.character(town_and_state),split = ",")
    
    # loop through to bring county --> state level 
    for(state_ind in 1:length(state_split)){
      if(length(state_split[[state_ind]])==1){
        state_vec[state_ind] <- state_split[[state_ind]]
      }else{
        state_vec[state_ind] <- state_names[trimws(state_split[[state_ind]][2]),"state_name"]
        admin_our_vec[state_ind] <- state_split[[state_ind]][1]
      }
    }
    
    
    this_df <- data.frame(State=these_data$Province.State, 
                          Country = these_data$Country.Region, 
                          Updated = these_data$Last.Update, 
                          Confirmed = these_data$Confirmed, 
                          Deaths = these_data$Deaths, 
                          Recovered = these_data$Recovered, 
                          Active = NA, 
                          Lat = NA, Long = NA, Admin2 = NA, FIPS = NA,
                          stringsAsFactors = F)
    
    this_df$State[which(this_df$Country=="US")] <- state_vec
    this_df$Admin2[which(this_df$Country=="US")] <- admin_our_vec
    
    
    this_df$Updated <- lubridate::mdy_hm(this_df$Updated)
    
    
    combined_CDR_data <- rbind(combined_CDR_data, this_df)
    
  }
  
  # Feb 2 started new time format ----- 
  if(date >= as.Date("2020-02-02") & 
     date < as.Date("2020-03-01")){
    # break
    these_data <- read.csv(file = data_files[file_ind],stringsAsFactors = F)
    
    
    town_and_state <- these_data$Province.State[which(these_data$Country.Region=="US")]
    town_and_state <- gsub(pattern = " (From Diamond Princess)",replacement = "",x = town_and_state,fixed = T)
    
    state_vec <- rep(NA,length=length(town_and_state))
    admin_our_vec <- rep(NA,length=length(town_and_state))
    
    state_split <- strsplit(x = as.character(town_and_state),split = ",")
    
    # loop through to bring county --> state level 
    for(state_ind in 1:length(state_split)){
      if(length(state_split[[state_ind]])==1){
        state_vec[state_ind] <- state_split[[state_ind]]
      }else{
        state_vec[state_ind] <- state_names[trimws(state_split[[state_ind]][2]),"state_name"]
        admin_our_vec[state_ind] <- state_split[[state_ind]][1]
      }
    }
    
    this_df <- data.frame(State=these_data$Province.State, 
                          Country = these_data$Country.Region, 
                          Updated = these_data$Last.Update, 
                          Confirmed = these_data$Confirmed, 
                          Deaths = these_data$Deaths, 
                          Recovered = these_data$Recovered, 
                          Active = NA, 
                          Lat = NA, Long = NA, Admin2 = NA, FIPS = NA,stringsAsFactors = F)
    
    this_df$State[which(this_df$Country=="US")] <- state_vec
    this_df$Admin2[which(this_df$Country=="US")] <- admin_our_vec
    
    this_df$Updated <- strptime(this_df$Updated, format = '%Y-%m-%dT%H:%M:%S',tz = "UTC")
    
    
    combined_CDR_data <- rbind(combined_CDR_data, this_df)
    
  }
  
  
  
  # Consistent header until March 22 ---- 
  if(date >= as.Date("2020-03-01") & 
     date <= as.Date("2020-03-21")){
    # break
    these_data <- read.csv(file = data_files[file_ind],stringsAsFactors = F)
    
    
    town_and_state <- these_data$Province.State[which(these_data$Country.Region=="US")]
    
    town_and_state <- gsub(pattern = " (From Diamond Princess)",replacement = "",x = town_and_state,fixed = T)
    
    state_vec <- rep(NA,length=length(town_and_state))
    admin_our_vec <- rep(NA,length=length(town_and_state))
    
    state_split <- strsplit(x = as.character(town_and_state),split = ",")
    
    # loop through to bring county --> state level 
    for(state_ind in 1:length(state_split)){
      if(length(state_split[[state_ind]])==1){
        state_vec[state_ind] <- state_split[[state_ind]]
      }else{
        state_vec[state_ind] <- state_names[trimws(state_split[[state_ind]][2]),"state_name"]
        admin_our_vec[state_ind] <- state_split[[state_ind]][1]
      }
    }
    
    this_df <- data.frame(State=these_data$Province.State, 
                          Country = these_data$Country.Region, 
                          Updated = these_data$Last.Update, 
                          Confirmed = these_data$Confirmed, 
                          Deaths = these_data$Deaths, 
                          Recovered = these_data$Recovered, 
                          Active = NA, 
                          Lat = these_data$Latitude, Long = these_data$Longitude,
                          Admin2 = NA, FIPS = NA,stringsAsFactors = F)
    
    this_df$State[which(this_df$Country=="US")] <- state_vec
    this_df$Admin2[which(this_df$Country=="US")] <- admin_our_vec
    
    this_df$Updated <- strptime(this_df$Updated, format = '%Y-%m-%dT%H:%M:%S',tz = "UTC")
    
    
    combined_CDR_data <- rbind(combined_CDR_data, this_df)
    
  }
  
  
  # March 22 has a unique date structure -----
  if(date == as.Date("2020-03-22")){
    # break
    these_data <- read.csv(file = data_files[file_ind],stringsAsFactors = F)
    
    
    # town_and_state <- these_data$Province.State[which(these_data$Country.Region=="US")]
    # 
    # town_and_state <- gsub(pattern = " (From Diamond Princess)",replacement = "",x = town_and_state,fixed = T)
    # 
    # state_vec <- rep(NA,length=length(town_and_state))
    # admin_our_vec <- rep(NA,length=length(town_and_state))
    # 
    # state_split <- strsplit(x = as.character(town_and_state),split = ",")
    # 
    # # loop through to bring county --> state level 
    # for(state_ind in 1:length(state_split)){
    #   if(length(state_split[[state_ind]])==1){
    #     state_vec[state_ind] <- state_split[[state_ind]]
    #   }else{
    #     state_vec[state_ind] <- state_names[trimws(state_split[[state_ind]][2]),"state_name"]
    #     admin_our_vec[state_ind] <- state_split[[state_ind]][1]
    #   }
    # }
    # 
    this_df <- data.frame(State=these_data$Province_State, 
                          Country = these_data$Country_Region, 
                          Updated = these_data$Last_Update, 
                          Confirmed = these_data$Confirmed, 
                          Deaths = these_data$Deaths, 
                          Recovered = these_data$Recovered, 
                          Active = these_data$Active, 
                          Lat = these_data$Lat, Long = these_data$Long_,
                          Admin2 = these_data$Admin2, FIPS = these_data$FIPS,stringsAsFactors = F)
    
    # this_df$State[which(this_df$Country=="US")] <- state_vec
    # this_df$Admin2[which(this_df$Country=="US")] <- admin_our_vec
    # 
    this_df$Updated <- strptime(this_df$Updated, format = '%m/%d/%y %H:%M',tz = "UTC")
    
    
    combined_CDR_data <- rbind(combined_CDR_data, this_df)
    
  }
  
  
  
  # After March 22  -----
  if(date > as.Date("2020-03-22")){
    # break
    these_data <- read.csv(file = data_files[file_ind],stringsAsFactors = F)
    
    
    # town_and_state <- these_data$Province.State[which(these_data$Country.Region=="US")]
    # 
    # town_and_state <- gsub(pattern = " (From Diamond Princess)",replacement = "",x = town_and_state,fixed = T)
    # 
    # state_vec <- rep(NA,length=length(town_and_state))
    # admin_our_vec <- rep(NA,length=length(town_and_state))
    # 
    # state_split <- strsplit(x = as.character(town_and_state),split = ",")
    # 
    # # loop through to bring county --> state level 
    # for(state_ind in 1:length(state_split)){
    #   if(length(state_split[[state_ind]])==1){
    #     state_vec[state_ind] <- state_split[[state_ind]]
    #   }else{
    #     state_vec[state_ind] <- state_names[trimws(state_split[[state_ind]][2]),"state_name"]
    #     admin_our_vec[state_ind] <- state_split[[state_ind]][1]
    #   }
    # }
    # 
    this_df <- data.frame(State=these_data$Province_State, 
                          Country = these_data$Country_Region, 
                          Updated = these_data$Last_Update, 
                          Confirmed = these_data$Confirmed, 
                          Deaths = these_data$Deaths, 
                          Recovered = these_data$Recovered, 
                          Active = these_data$Active, 
                          Lat = these_data$Lat, Long = these_data$Long_,
                          Admin2 = these_data$Admin2, FIPS = these_data$FIPS,stringsAsFactors = F)
    
    # this_df$State[which(this_df$Country=="US")] <- state_vec
    # this_df$Admin2[which(this_df$Country=="US")] <- admin_our_vec
    # 
    this_df$Updated <- strptime(this_df$Updated, format = '%Y-%m-%d %H:%M:%S',tz = "UTC")
    
    
    combined_CDR_data <- rbind(combined_CDR_data, this_df)
    
  }
  
}


save(combined_CDR_data, file="output_data/cleaned_data/combined_CDR_data.RData")






# Confirmed cases ----- 

jhu_data <- read.csv(check.names = F,
                     file = "JHU_COVID19_data/COVID-19/csse_covid_19_data/csse_covid_19_time_series/time_series_19-covid-Confirmed.csv")

# clean data types prior to melting
jhu_data <- jhu_data %>%
  mutate_if(is.numeric,as.character, 
            is.factor, as.character)

jhu_data_m <- tidyr::pivot_longer(data = jhu_data, 
                                  cols=`1/22/20`:colnames(jhu_data)[ncol(jhu_data)], 
                                  values_to = "Confirmed_cases",names_to = "Date") %>%
  mutate(Confirmed_cases = as.numeric(Confirmed_cases))

jhu_data_m <- jhu_data_m %>%
  mutate(Date = as.Date(Date, format= "%m/%d/%y"))

countries_unique <- unique(jhu_data_m$`Country/Region`)

for(country in 1:length(countries_unique)){
 this_df <-  jhu_data_m %>% 
   filter(`Country/Region` == countries_unique[country]) %>%
   group_by(`Country/Region`,Date) %>%
   summarize(all_cases = sum(Confirmed_cases)) %>%
   filter(!is.na(all_cases)) 
 
 country_for_save <- gsub(x = as.character(countries_unique[country]),replacement = "_",pattern = " ")
 
 write.csv(x = this_df,file = paste0("output_data/cleaned_data/confirmed_cases/",
                  country_for_save,"_confirmed_cases",
                  ".csv"),row.names = F,quote = F)
 
}





# Deaths ----- 

jhu_data <- read.csv(check.names = F,
                     file = "JHU_COVID19_data/COVID-19/csse_covid_19_data/csse_covid_19_time_series/time_series_19-covid-Deaths.csv")

# clean data types prior to melting
jhu_data <- jhu_data %>%
  mutate_if(is.numeric,as.character, 
            is.factor, as.character)

jhu_data_m <- tidyr::pivot_longer(data = jhu_data, 
                                  cols=`1/22/20`:colnames(jhu_data)[ncol(jhu_data)], 
                                  values_to = "Deaths",names_to = "Date") %>%
  mutate(Deaths = as.numeric(Deaths))

jhu_data_m <- jhu_data_m %>%
  mutate(Date = as.Date(Date, format= "%m/%d/%y"))

countries_unique <- unique(jhu_data_m$`Country/Region`)

for(country in 1:length(countries_unique)){
  this_df <-  jhu_data_m %>% 
    filter(`Country/Region` == countries_unique[country]) %>%
    group_by(`Country/Region`,Date) %>%
    summarize(all_cases = sum(Deaths)) %>%
    filter(!is.na(all_cases)) 
  
  country_for_save <- gsub(x = as.character(countries_unique[country]),replacement = "_",pattern = " ")
  
  write.csv(x = this_df,file = paste0("output_data/cleaned_data/deaths/",
                                      country_for_save,"_deaths",
                                      ".csv"),row.names = F,quote = F)
  
}


# Recovered ----- 

jhu_data <- read.csv(check.names = F,
                     file = "JHU_COVID19_data/COVID-19/csse_covid_19_data/csse_covid_19_time_series/time_series_19-covid-Recovered.csv")

# clean data types prior to melting
jhu_data <- jhu_data %>%
  mutate_if(is.numeric,as.character, 
            is.factor, as.character)

jhu_data_m <- tidyr::pivot_longer(data = jhu_data, 
                                  cols=`1/22/20`:colnames(jhu_data)[ncol(jhu_data)], 
                                  values_to = "Recovered",names_to = "Date") %>%
  mutate(Recovered = as.numeric(Recovered))

jhu_data_m <- jhu_data_m %>%
  mutate(Date = as.Date(Date, format= "%m/%d/%y"))

countries_unique <- unique(jhu_data_m$`Country/Region`)

for(country in 1:length(countries_unique)){
  this_df <-  jhu_data_m %>% 
    filter(`Country/Region` == countries_unique[country]) %>%
    group_by(`Country/Region`,Date) %>%
    summarize(all_cases = sum(Recovered)) %>%
    filter(!is.na(all_cases)) 
  
  country_for_save <- gsub(x = as.character(countries_unique[country]),replacement = "_",pattern = " ")
  
  write.csv(x = this_df,file = paste0("output_data/cleaned_data/recovered/",
                                      country_for_save,"_recovered",
                                      ".csv"),row.names = F,quote = F)
  
}








