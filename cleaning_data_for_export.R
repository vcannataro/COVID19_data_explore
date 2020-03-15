# JHU data explore 
# scratch file

library(tidyverse)

# data obtained from https://github.com/CSSEGISandData/COVID-19.git


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
  
  write.csv(x = this_df,file = paste0("output_data/cleaned_data/recovered/",
                                      country_for_save,"_recovered",
                                      ".csv"),row.names = F,quote = F)
  
}








