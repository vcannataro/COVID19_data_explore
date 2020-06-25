library(tidyverse)


# load in latest data
nytimes_county <- read.csv(file = "NY_Times_COVID19_data/covid-19-data/us-counties.csv",
                           stringsAsFactors = T)

# makes dates a date
nytimes_county$date <- as.Date(nytimes_county$date,format = "%Y-%m-%d")


# new cases per time point
nytimes_data_lagged <- nytimes_county %>%
  group_by(state,county) %>%
  mutate(lag_cases = cases - dplyr::lag(cases))

# plotting some hometown data 

ggplot(nytimes_data_lagged %>% filter(state=="New York" & county=="Suffolk")) +
  geom_bar(aes(x=date,y=lag_cases), stat = "identity")



# ggplot(nytimes_data_lagged %>% filter(state=="New York" & county=="Nassau")) + 
#   geom_bar(aes(x=date,y=lag_cases), stat = "identity")
# 
# 
ggplot(nytimes_data_lagged %>% filter(state=="Massachusetts" & county=="Suffolk")) +
  geom_bar(aes(x=date,y=lag_cases), stat = "identity") +
  theme_bw()



# per state
nytimes_data_lagged_state <- nytimes_county %>%
  group_by(state,date) %>%
  summarize(total_state = sum(cases)) %>%
  arrange(state,date) %>%
  group_by(state) %>%
  mutate(lag_cases = total_state - dplyr::lag(total_state,default = 0)) %>%
  filter(lag_cases >= 0) 


nytimes_data_lagged_state$`Day of the week` <- lubridate::wday(nytimes_data_lagged_state$date,
                                                  label = TRUE, abbr = FALSE)


ggplot(data = nytimes_data_lagged_state) + 
  geom_bar(aes(x=date,y=lag_cases,fill=`Day of the week`),stat= "identity") + 
  theme_bw() + 
  coord_cartesian(xlim=as.Date(c("2020-03-15",max(nytimes_data_lagged_state$date)))) +
  labs(y="Total new cases per day", 
       x= "Date", 
       title = "Total new COVID19 cases per day in the USA",
       caption = paste("Data: The New York Times, https://github.com/nytimes/covid-19-data
       Plot: @VinCannataro on",Sys.Date(),"https://github.com/vcannataro/COVID19_data_explore")) + 
  scale_x_date(date_breaks = "1 week") + 
  theme(axis.text.x = element_text(angle=90,hjust=1,vjust=0.5))

ggsave(filename = "output_data/figures/cases_V_day_of_week.png")






