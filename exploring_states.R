# exploring state data

# seems county data is better as state has negative values when lagging, 
# switching to exploring_counties.R for now

library(tidyverse)

# load in latest data
nytimes_state <- read.csv(file = "NY_Times_COVID19_data/covid-19-data/us-states.csv",
                           stringsAsFactors = T)

# makes dates a date
nytimes_state$date <- as.Date(nytimes_state$date,format = "%Y-%m-%d")


# new cases per time point
nytimes_state <- nytimes_state %>%
  arrange(date) %>%
  group_by(state) %>%
  mutate(lag_cases = cases - dplyr::lag(cases,default = 0)) %>%
  arrange(state) %>%
  filter(lag_cases >= 0)

# plot and save all states 
ggplot(data = nytimes_state) + 
  geom_bar(aes(x=date,y=lag_cases),stat = "identity") + 
  facet_wrap(~state,scales = "free_y") + 
  theme_bw() -> 
  all_states

ggsave(filename = "output_data/figures/all_states.pdf",plot = all_states,height = 15,width = 20)



# looking at all states 
all_country <- nytimes_state %>%
  group_by(date) %>%
  summarize(total_cases = sum(cases)) %>%
  mutate(lag_cases = total_cases - dplyr::lag(total_cases, default = 0))


states_to_plot <- c("Alabama",
                    "Arizona",
                    "California",
                    "Florida",
                    "Georgia",
                    "Louisiana",
                    "North Carolina",
                    "South Carolina",
                    "Texas",
                    "New York",
                    "New Jersey",
                    "Massachusetts")

states_data_to_plot <- nytimes_state %>%
  filter(state %in% states_to_plot)
states_data_to_plot$state <- factor(states_data_to_plot$state, levels= states_to_plot)


ggplot(data = all_country) + 
  geom_bar(aes(x=date,y=lag_cases),stat="identity",alpha=0.5,fill="black") + 
  geom_bar(data = states_data_to_plot,
           aes(x=date,y=lag_cases,fill=state),
           stat="identity",
           alpha=0.9,
           position = "stack",
           color="black") + 
  theme_bw() +
  coord_cartesian(xlim=as.Date(c("2020-03-15","2020-06-13"))) + 
  scale_x_date(date_labels = "%b %d",date_breaks = "7 days") +
  theme(axis.text.x = element_text(angle = 25,hjust = 1,vjust = 1)) +
  labs(y="Total new cases per day", 
       x= "Date", 
       title = "Total new cases per day in the entire USA and select states",
       caption = "Data: The New York Times, https://github.com/nytimes/covid-19-data\nPlot: @VinCannataro")







