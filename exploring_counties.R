library(tidyverse)

add_live <- F

# load in latest data
nytimes_county <- read.csv(file = "NY_Times_COVID19_data/covid-19-data/us-counties.csv",
                           stringsAsFactors = T)

if(add_live){
  nytimes_county_live <- read.csv(file = "NY_Times_COVID19_data/covid-19-data/live/us-counties.csv",
                                  stringsAsFactors = T)  
  nytimes_county <- dplyr::bind_rows(nytimes_county,nytimes_county_live)
}

# makes dates a date
nytimes_county$date <- as.Date(nytimes_county$date,format = "%Y-%m-%d")


# new cases per time point
nytimes_data_lagged <- nytimes_county %>%
  group_by(state,county) %>%
  mutate(lag_cases = cases - dplyr::lag(cases,default = 0))
# 
# nytimes_data_lagged %>% filter(state == "Wisconsin") %>%
#   filter(date %in% as.Date(c("2020-06-25","2020-06-26"))) %>% View()


# plotting some hometown data 

ggplot(nytimes_data_lagged %>% filter(state=="New York" & county=="Suffolk")) +
  geom_bar(aes(x=date,y=lag_cases), stat = "identity") + 
  scale_x_date(date_labels = "%b %d",date_breaks = "14 days") +
  theme_bw() + 
  theme(axis.text.x = element_text(angle=90))


# ggplot(nytimes_data_lagged %>% filter(state=="New York" & county=="Suffolk" & date > "2020-07-01")) +
#   geom_bar(aes(x=date,y=lag_cases), stat = "identity") +
#   theme_bw()


# ggplot(nytimes_data_lagged %>% filter(state=="New York"))+
#   geom_bar(aes(x=date,y=lag_cases), stat = "identity") +
#   theme_bw()



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
  labs(y="New cases per day",x="Date",
       caption = "Data: The New York Times, https://github.com/nytimes/covid-19-data\nPlot: @VinCannataro https://github.com/vcannataro/COVID19_data_explore") -> 
  all_states


ggsave(filename = "output_data/figures/all_states.pdf",plot = all_states,height = 15,width = 20)





all_country <- nytimes_county %>%
  group_by(date) %>%
  summarize(total_cases = sum(cases)) %>%
  mutate(lag_cases = total_cases - dplyr::lag(total_cases, default = 0))



top_states <- nytimes_data_lagged_state %>%
  filter(date == max(date)) %>%
  group_by(state) %>%
  arrange(desc(lag_cases)) %>%
  select(state) %>%
  as.matrix()

top_states <- top_states[!top_states %in% c("New York",
                  "New Jersey",
                  "Massachusetts")][1:9]


states_to_plot <- c(top_states,
                    "New York",
                    "New Jersey",
                    "Massachusetts")



states_data_to_plot <- nytimes_data_lagged_state 
states_data_to_plot$state <- factor(states_data_to_plot$state, levels= unique(states_data_to_plot$state))


other_states <- as.character(unique(states_data_to_plot$state))[!as.character(unique(states_data_to_plot$state)) %in% states_to_plot]

states_data_to_plot_collapse <- states_data_to_plot
states_data_to_plot_collapse$state <- forcats::fct_collapse(states_data_to_plot_collapse$state, 
                                                            `NY, NJ, MA` = c("New York","New Jersey","Massachusetts"),
                                                            `Other states` = other_states)

states_data_to_plot_collapse$state <- forcats::fct_relevel(states_data_to_plot_collapse$state,
                                                           c("Other states",
                                                             states_to_plot[1:(length(states_to_plot)-3)],
                                                             "NY, NJ, MA"))

states_data_to_plot_filtered <- states_data_to_plot %>%
  filter(state %in% states_to_plot)


states_data_to_plot_filtered$state <- forcats::fct_drop(states_data_to_plot_filtered$state)
states_data_to_plot_filtered$state <- forcats::fct_relevel(states_data_to_plot_filtered$state,states_to_plot)

gg_color_hue <- function(n) {
  hues = seq(15, 375, length = n + 1)
  hcl(h = hues, l = 65, c = 100)[1:n]
}

col_vec_collapse <- setNames(object = c("gray50",gg_color_hue(n = 
                                                       length(levels(states_data_to_plot_collapse$state))-2),
                                        rep("gray10",1)),nm = levels(states_data_to_plot_collapse$state))

col_vec <- c(col_vec_collapse,setNames(object = rep("gray10",3),nm = c("New York","New Jersey","Massachusetts")))


# rolling mean
all_country <- all_country %>%
  mutate(rolling_mean = zoo::rollmean(lag_cases,7,na.pad=T))



all_states <- ggplot(data = all_country) + 
  # geom_bar(aes(x=date,y=lag_cases),stat="identity",alpha=0.5,fill="black") + 
  geom_bar(data = states_data_to_plot_collapse,
           aes(x=date,y=lag_cases,fill=state),
           stat="identity",
           alpha=1,
           position = "stack") + 
  geom_line(aes(x=date,y=rolling_mean),
            color="red",size=2,lineend = "round",linetype=1) + 
  theme_bw() +
  coord_cartesian(xlim=as.Date(c("2020-03-15",max(nytimes_county$date)))) + 
  scale_x_date(date_labels = "%b %d",date_breaks = "14 days") +
  scale_fill_manual(values = col_vec_collapse, name="State") + 
  scale_y_continuous(labels = scales::comma) + 
  theme(axis.text.x = element_text(angle = 25,hjust = 1,vjust = 1)) +
  labs(y="Total new cases per day", 
       x= "Date", 
       title = "Total new COVID19 cases per day in the USA and select states",
       subtitle="Red line indicates 7-day new cases rolling average in USA",
       caption = "Data: The New York Times, https://github.com/nytimes/covid-19-data\nPlot: @VinCannataro https://github.com/vcannataro/COVID19_data_explore")




each_state <- ggplot(data = states_data_to_plot_filtered) + 
  geom_bar(aes(x=date,y=lag_cases,fill=state),
           stat="identity",
           alpha=1) + 
  scale_fill_manual(values = col_vec,name="State") +
  theme_bw() + 
  facet_wrap(~state,scales = "free_y",nrow = 4) + 
  labs(y="Total new cases per day", 
       x= "Date",
       caption = "Data: The New York Times, https://github.com/nytimes/covid-19-data\nPlot: @VinCannataro https://github.com/vcannataro/COVID19_data_explore") + 
  coord_cartesian(xlim=as.Date(c("2020-03-15",max(nytimes_county$date)))) + 
  scale_x_date(date_labels = "%b %d",date_breaks = "1 month") + 
  theme(axis.text.x = element_text(angle = 45,hjust = 1,vjust = 1))




all_states_each_state_plot <- cowplot::plot_grid(all_states,each_state,nrow = 2)

cowplot::save_plot(plot = all_states_each_state_plot,
                   filename = "output_data/figures/all_states_VS_each_state.pdf",
                   base_height = 10,base_width = 8)
cowplot::save_plot(plot = all_states_each_state_plot,
                   filename = "output_data/figures/all_states_VS_each_state.png",
                   base_height = 10,base_width = 8)






nytimes_data_lagged %>% 
  filter(state == "Massachusetts") %>%
  filter(lag_cases>=0) %>%
  mutate(roll_mean_lag_cases = zoo::rollmean(lag_cases,7,na.pad=T)) %>%
  ggplot() + 
  geom_bar(aes(x=date,y=lag_cases),stat="identity") + 
  geom_line(aes(x=date, y=roll_mean_lag_cases),color="red",size=1,alpha=0.5) + 
  facet_wrap(~county, scales = "free_y") + 
  theme_bw()



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


