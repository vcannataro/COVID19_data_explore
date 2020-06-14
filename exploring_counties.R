
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

# plotting some data 
# ggplot(nytimes_data_lagged %>% filter(state=="New York" & county=="Suffolk")) + 
#   geom_bar(aes(x=date,y=cases), stat = "identity")


# tail(nytimes_data_lagged %>% filter(state=="New York" & county=="Suffolk"))
# 
ggplot(nytimes_data_lagged %>% filter(state=="New York" & county=="Suffolk")) +
  geom_bar(aes(x=date,y=lag_cases), stat = "identity")
# 
# ggplot(nytimes_data_lagged %>% filter(state=="New York" & county=="Nassau")) + 
#   geom_bar(aes(x=date,y=lag_cases), stat = "identity")
# 
# 
# ggplot(nytimes_data_lagged %>% filter(state=="Massachusetts" & county=="Suffolk")) + 
#   geom_bar(aes(x=date,y=lag_cases), stat = "identity") + 
#   theme_bw()



# per state
nytimes_data_lagged_state <- nytimes_county %>%
  group_by(state,date) %>%
  summarize(total_state = sum(cases)) %>%
  group_by(state) %>%
  mutate(lag_cases = total_state - dplyr::lag(total_state,default = 0)) %>%
  filter(lag_cases >= 0) 
  
  
  # nytimes_data_lagged %>%
  # group_by(date, state) %>%
  # summarize(state_totals = sum(lag_cases,na.rm = T)) %>%
  # filter(state_totals >=0)

# ggplot(data = nytimes_data_lagged_state %>% filter(state=="New York")) + 
#   geom_bar(aes(x=date, y=state_totals),stat="identity")

# nytimes_data_lagged_state[nytimes_data_lagged_state$lag_cases < 0,]


# nytimes_data_lagged %>%
  # filter(state=="Georgia")

# 2020-04-12 Georgia 

ggplot(data = nytimes_data_lagged_state) + 
  geom_bar(aes(x=date, y=lag_cases),stat="identity") + 
  facet_wrap(~state,scales = "free_y") +
  theme_bw() + 
  labs(y="New cases per day",x="Date") -> 
  all_states


ggsave(filename = "output_data/figures/all_states.pdf",plot = all_states,height = 15,width = 20)
