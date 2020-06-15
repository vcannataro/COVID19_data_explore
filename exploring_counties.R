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




ggplot(data = nytimes_data_lagged_state) + 
  geom_bar(aes(x=date, y=lag_cases),stat="identity") + 
  facet_wrap(~state,scales = "free_y") +
  theme_bw() + 
  labs(y="New cases per day",x="Date") -> 
  all_states


ggsave(filename = "output_data/figures/all_states.pdf",plot = all_states,height = 15,width = 20)





all_country <- nytimes_county %>%
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

states_data_to_plot <- nytimes_data_lagged_state %>%
  filter(state %in% states_to_plot)
states_data_to_plot$state <- factor(states_data_to_plot$state, levels= states_to_plot)

states_data_to_plot_collapse <- states_data_to_plot
states_data_to_plot_collapse$state <- forcats::fct_collapse(states_data_to_plot_collapse$state, `NY, NJ, MA` = c("New York","New Jersey","Massachusetts"))

gg_color_hue <- function(n) {
  hues = seq(15, 375, length = n + 1)
  hcl(h = hues, l = 65, c = 100)[1:n]
}

col_vec_collapse <- setNames(object = c(gg_color_hue(n = 
                                                       length(levels(states_data_to_plot_collapse$state))-1),
                                        "gray10"),nm = levels(states_data_to_plot_collapse$state))

col_vec <- setNames(object = c(gg_color_hue(n = 
                                              length(levels(states_data_to_plot$state))-3),
                               rep("gray10",3)),nm = levels(states_data_to_plot$state))


all_states <- ggplot(data = all_country) + 
  geom_bar(aes(x=date,y=lag_cases),stat="identity",alpha=0.5,fill="black") + 
  geom_bar(data = states_data_to_plot_collapse,
           aes(x=date,y=lag_cases,fill=state),
           stat="identity",
           alpha=1,
           position = "stack",
           color="gray10") + 
  theme_bw() +
  coord_cartesian(xlim=as.Date(c("2020-03-15","2020-06-13"))) + 
  scale_x_date(date_labels = "%b %d",date_breaks = "7 days") +
  scale_fill_manual(values = col_vec_collapse, name="State") + 
  theme(axis.text.x = element_text(angle = 25,hjust = 1,vjust = 1)) +
  labs(y="Total new cases per day", 
       x= "Date", 
       title = "Total new cases per day in the entire USA and select states",
       caption = "Data: The New York Times, https://github.com/nytimes/covid-19-data\nPlot: @VinCannataro")




each_state <- ggplot(data = states_data_to_plot) + 
  geom_bar(data = states_data_to_plot,
           aes(x=date,y=lag_cases,fill=state),
           stat="identity",
           alpha=1) + 
  scale_fill_manual(values = col_vec,name="State") +
  theme_bw() + 
  facet_wrap(~state,scales = "free_y",nrow = 4) + 
  labs(y="Total new cases per day", 
       x= "Date") + 
  coord_cartesian(xlim=as.Date(c("2020-03-15","2020-06-13"))) + 
  scale_x_date(date_labels = "%b %d",date_breaks = "1 month") 




all_states_each_state_plot <- cowplot::plot_grid(all_states,each_state,nrow = 2)

cowplot::save_plot(plot = all_states_each_state_plot,
                   filename = "output_data/figures/all_states_VS_each_state.pdf",
                   base_height = 10,base_width = 8)




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


