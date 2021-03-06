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
  mutate(lag_cases = cases - dplyr::lag(cases,default = 0)) %>%
  filter(lag_cases >= 0) %>%
  # filter(county != "Unknown") %>%
  mutate(county = dplyr::recode(county, Unknown = "County not specified")) %>%
  mutate(lag_cases_rollmean = zoo::rollmean(x = lag_cases,7,na.pad=T))
# 
# nytimes_data_lagged %>% filter(state == "Wisconsin") %>%
#   filter(date %in% as.Date(c("2020-06-25","2020-06-26"))) %>% View()


# plotting some hometown data 

ggplot(nytimes_data_lagged %>% filter(state=="New York" & county=="Suffolk")) +
  geom_bar(aes(x=date,y=lag_cases), stat = "identity") + 
  theme_bw()

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

just_MA <- nytimes_data_lagged %>% 
  filter(state=="Massachusetts") %>% 
  group_by(date) %>%
  summarize(total_lag_cases = sum(lag_cases)) %>%
  ungroup() %>%
  mutate(lag_cases_rollmean = zoo::rollmean(x = total_lag_cases,7,na.pad=T))

just_MA_last_date <- just_MA %>%
  filter(date == max(date))


all_MA_plot <- ggplot(just_MA) + 
  geom_bar(aes(x=date,y=total_lag_cases),stat="identity") + 
  geom_segment(data = just_MA_last_date,
               aes(x=date-5, y=total_lag_cases+1500,yend=total_lag_cases,xend=date),alpha=0.4,color="blue") + 
  geom_label(data = just_MA_last_date, 
             aes(x=date-5, y=total_lag_cases+1500,label=paste(total_lag_cases,"\nnew cases"))) +
  theme_bw() + 
  scale_x_date(date_breaks = "2 weeks",date_labels = "%b %d") + 
  theme(axis.text.x = element_text(angle = 45,hjust=1)) + 
  geom_line(aes(x=date,y=lag_cases_rollmean),col="red") + 
  labs(y="New cases per day in MA", x="Date",title="New cases per day in MA", 
       subtitle="Red line indicates 7-day rolling mean",
       caption = "Data: The New York Times, https://github.com/nytimes/covid-19-data\nPlot: @VinCannataro https://github.com/vcannataro/COVID19_data_explore")



all_MA_plot_recent<- ggplot(just_MA %>%
                                 filter(date > "2020-06-15")) + 
  geom_bar(aes(x=date,y=total_lag_cases),stat="identity") + 
  # geom_segment(data = just_MA_last_date,
  #              aes(x=date-1, y=total_lag_cases+150,yend=total_lag_cases,xend=date)) + 
  # geom_label(data = just_MA_last_date, 
  #            aes(x=date-1, y=total_lag_cases+150,label=paste(total_lag_cases,"\nnew cases"))) +
  theme_bw() + 
  # scale_x_date(limits = as.Date(c("2020-06-15",max(just_MA$date)))) + 
  geom_line(aes(x=date,y=lag_cases_rollmean),col="red",size=3 , lineend="round") + 
  labs(y="New cases per day in MA", x="Date",title="New cases per day in MA", 
       subtitle="Red line indicates 7-day rolling mean",
       caption = "Data: The New York Times, https://github.com/nytimes/covid-19-data\nPlot: @VinCannataro https://github.com/vcannataro/COVID19_data_explore")+ 
  theme(axis.text = element_text(size=20),
        text = element_text(size = 20))

ggsave(filename = "output_data/figures/MA_state_recent_cases.png",plot = all_MA_plot_recent,height = 6,width = 8)


MA_counties <- nytimes_data_lagged %>% 
  filter(state=="Massachusetts") 

MA_counties_last_date <- MA_counties %>%
  filter(date == max(date))

MA_counties_plot <- MA_counties %>% 
  ggplot() + 
  geom_bar(aes(x=date,y=lag_cases),stat="identity") + 
  # geom_segment(data = MA_counties_last_date,
  #              aes(x=date-5, y=lag_cases+900,yend=lag_cases,xend=date)) + 
  # geom_label(data = MA_counties_last_date, 
  #            aes(x=date-5, y=lag_cases+900,label=paste(lag_cases,"\nnew cases"))) +
  facet_wrap(~county, scales = "free_y") + 
  theme_bw() + 
  geom_line(aes(x=date,y=lag_cases_rollmean),col="red") + 
  labs(y="New cases per day in MA counties", x="Date",title="New cases per day in MA", 
       subtitle="Red line indicates 7-day rolling mean",
       caption = "Data: The New York Times, https://github.com/nytimes/covid-19-data\nPlot: @VinCannataro https://github.com/vcannataro/COVID19_data_explore")
  

MA_plot_combined <- cowplot::plot_grid(all_MA_plot,MA_counties_plot,
                   nrow = 2)

cowplot::save_plot(filename = "output_data/figures/MA_plot_state_and_counties.png",
                   plot = MA_plot_combined,base_height = 10,base_width = 8)



write.csv(x = MA_counties %>% arrange(county),file = "output_data/cleaned_data/MA_counties.csv",quote = F,row.names = F)
write.csv(x = just_MA ,file = "output_data/cleaned_data/MA_state.csv",quote = F,row.names = F)

# 
# nytimes_data_lagged %>%
#   filter(state=="Massachusetts") %>%
#   group_by(date) %>%
#   summarize(lag_cases = sum(lag_cases)) %>%
#   tail()
# 
# 
# 
# 
# 
# # per state
# nytimes_data_lagged_state <- nytimes_county %>%
#   group_by(state,date) %>%
#   summarize(total_state = sum(cases)) %>%
#   arrange(state,date) %>%
#   group_by(state) %>%
#   mutate(lag_cases = total_state - dplyr::lag(total_state,default = 0)) %>%
#   filter(lag_cases >= 0) 
# 
# 
# 
# 
# ggplot(data = nytimes_data_lagged_state) + 
#   geom_bar(aes(x=date, y=lag_cases),stat="identity") + 
#   facet_wrap(~state,scales = "free_y") +
#   theme_bw() + 
#   labs(y="New cases per day",x="Date") -> 
#   all_states
# 
# 
# ggsave(filename = "output_data/figures/all_states.pdf",plot = all_states,height = 15,width = 20)
# 
# 
# 
# 
# 
# all_country <- nytimes_county %>%
#   group_by(date) %>%
#   summarize(total_cases = sum(cases)) %>%
#   mutate(lag_cases = total_cases - dplyr::lag(total_cases, default = 0))
# 
# 
# 
# top_states <- nytimes_data_lagged_state %>%
#   filter(date == max(date)) %>%
#   group_by(state) %>%
#   arrange(desc(lag_cases)) %>%
#   select(state) %>%
#   as.matrix()
# 
# top_states <- top_states[!top_states %in% c("New York",
#                                             "New Jersey",
#                                             "Massachusetts")][1:9]
# 
# 
# states_to_plot <- c(top_states,
#                     "New York",
#                     "New Jersey",
#                     "Massachusetts")
# 
# 
# 
# states_data_to_plot <- nytimes_data_lagged_state 
# states_data_to_plot$state <- factor(states_data_to_plot$state, levels= unique(states_data_to_plot$state))
# 
# 
# other_states <- as.character(unique(states_data_to_plot$state))[!as.character(unique(states_data_to_plot$state)) %in% states_to_plot]
# 
# states_data_to_plot_collapse <- states_data_to_plot
# states_data_to_plot_collapse$state <- forcats::fct_collapse(states_data_to_plot_collapse$state, 
#                                                             `NY, NJ, MA` = c("New York","New Jersey","Massachusetts"),
#                                                             `Other states` = other_states)
# 
# states_data_to_plot_collapse$state <- forcats::fct_relevel(states_data_to_plot_collapse$state,
#                                                            c("Other states",
#                                                              states_to_plot[1:(length(states_to_plot)-3)],
#                                                              "NY, NJ, MA"))
# 
# states_data_to_plot_filtered <- states_data_to_plot %>%
#   filter(state %in% states_to_plot)
# 
# 
# states_data_to_plot_filtered$state <- forcats::fct_drop(states_data_to_plot_filtered$state)
# states_data_to_plot_filtered$state <- forcats::fct_relevel(states_data_to_plot_filtered$state,states_to_plot)
# 
# gg_color_hue <- function(n) {
#   hues = seq(15, 375, length = n + 1)
#   hcl(h = hues, l = 65, c = 100)[1:n]
# }
# 
# col_vec_collapse <- setNames(object = c("gray50",gg_color_hue(n = 
#                                                                 length(levels(states_data_to_plot_collapse$state))-2),
#                                         rep("gray10",1)),nm = levels(states_data_to_plot_collapse$state))
# 
# col_vec <- c(col_vec_collapse,setNames(object = rep("gray10",3),nm = c("New York","New Jersey","Massachusetts")))
# 
# 
# # rolling mean
# all_country <- all_country %>%
#   mutate(rolling_mean = zoo::rollmean(lag_cases,7,na.pad=T))
# 
# 
# 
# all_states <- ggplot(data = all_country) + 
#   # geom_bar(aes(x=date,y=lag_cases),stat="identity",alpha=0.5,fill="black") + 
#   geom_bar(data = states_data_to_plot_collapse,
#            aes(x=date,y=lag_cases,fill=state),
#            stat="identity",
#            alpha=1,
#            position = "stack") + 
#   geom_line(aes(x=date,y=rolling_mean),
#             color="red",size=2,lineend = "round",linetype=1) + 
#   theme_bw() +
#   coord_cartesian(xlim=as.Date(c("2020-03-15",max(nytimes_county$date)))) + 
#   scale_x_date(date_labels = "%b %d",date_breaks = "7 days") +
#   scale_fill_manual(values = col_vec_collapse, name="State") + 
#   theme(axis.text.x = element_text(angle = 25,hjust = 1,vjust = 1)) +
#   labs(y="Total new cases per day", 
#        x= "Date", 
#        title = "Total new COVID19 cases per day in the USA and select states",
#        subtitle="Red line indicates 7-day new cases rolling average in USA",
#        caption = "Data: The New York Times, https://github.com/nytimes/covid-19-data\nPlot: @VinCannataro https://github.com/vcannataro/COVID19_data_explore")
# 
# 
# 
# 
# each_state <- ggplot(data = states_data_to_plot_filtered) + 
#   geom_bar(aes(x=date,y=lag_cases,fill=state),
#            stat="identity",
#            alpha=1) + 
#   scale_fill_manual(values = col_vec,name="State") +
#   theme_bw() + 
#   facet_wrap(~state,scales = "free_y",nrow = 4) + 
#   labs(y="Total new cases per day", 
#        x= "Date",
#        caption = "Data: The New York Times, https://github.com/nytimes/covid-19-data\nPlot: @VinCannataro https://github.com/vcannataro/COVID19_data_explore") + 
#   coord_cartesian(xlim=as.Date(c("2020-03-15",max(nytimes_county$date)))) + 
#   scale_x_date(date_labels = "%b %d",date_breaks = "1 month") 
# 
# 
# 
# 
# all_states_each_state_plot <- cowplot::plot_grid(all_states,each_state,nrow = 2)
# 
# cowplot::save_plot(plot = all_states_each_state_plot,
#                    filename = "output_data/figures/all_states_VS_each_state.pdf",
#                    base_height = 10,base_width = 8)
# cowplot::save_plot(plot = all_states_each_state_plot,
#                    filename = "output_data/figures/all_states_VS_each_state.png",
#                    base_height = 10,base_width = 8)
# 
# 
# 
# 
# 
# 
# nytimes_data_lagged %>% 
#   filter(state == "Massachusetts") %>%
#   filter(lag_cases>=0) %>%
#   mutate(roll_mean_lag_cases = zoo::rollmean(lag_cases,7,na.pad=T)) %>%
#   ggplot() + 
#   geom_bar(aes(x=date,y=lag_cases),stat="identity") + 
#   geom_line(aes(x=date, y=roll_mean_lag_cases),color="red",size=1,alpha=0.5) + 
#   facet_wrap(~county, scales = "free_y") + 
#   theme_bw()
# 
# 
# 
# # nytimes_data_lagged %>%
# # group_by(date, state) %>%
# # summarize(state_totals = sum(lag_cases,na.rm = T)) %>%
# # filter(state_totals >=0)
# 
# # ggplot(data = nytimes_data_lagged_state %>% filter(state=="New York")) + 
# #   geom_bar(aes(x=date, y=state_totals),stat="identity")
# 
# # nytimes_data_lagged_state[nytimes_data_lagged_state$lag_cases < 0,]
# 
# 
# # nytimes_data_lagged %>%
# # filter(state=="Georgia")
# 
# # 2020-04-12 Georgia 
# 
# 
