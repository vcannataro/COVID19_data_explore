# JHU data explore 
# scratch file

library(tidyverse)

# data obtained from https://github.com/CSSEGISandData/COVID-19.git

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




# initial data exploration -----

all_countries <- jhu_data_m %>% 
  group_by(`Country/Region`,Date) %>%
  summarize(all_cases = sum(Confirmed_cases)) %>%
  filter(!is.na(all_cases))
  

ggplot(data = all_countries) + 
  geom_point(aes(x=Date, y=all_cases)) + 
  theme_classic() 




## + Compare Italy and USA ---- 

IT_US <- jhu_data_m %>% 
  filter(`Country/Region` %in% c("Italy","US")) %>%
  group_by(`Country/Region`,Date) %>%
  summarize(all_cases = sum(Confirmed_cases)) %>%
  filter(!is.na(all_cases)) 


ggplot(data = IT_US) + 
  geom_point(aes(x=Date, y=all_cases,color=`Country/Region`)) + 
  theme_classic()  + 
  labs(y="All confirmed cases")


ggplot(data = IT_US) + 
  geom_point(aes(x=Date, y=all_cases,color=`Country/Region`)) + 
  theme_classic()  + 
  labs(y="All confirmed cases") + 
  scale_y_log10()



## + just USA ---- 



US_data <- jhu_data_m %>% 
  filter(`Country/Region` %in% c("US")) %>%
  group_by(`Country/Region`,Date) %>%
  summarize(all_cases = sum(Confirmed_cases)) %>%
  filter(!is.na(all_cases)) 


ggplot(data = US_data) + 
  geom_point(aes(x=Date, y=all_cases,color=`Country/Region`)) + 
  theme_classic()  + 
  labs(y="All confirmed cases") + 
  scale_y_log10()


ggplot(data = US_data) + 
  geom_point(aes(x=Date, y=all_cases,color=`Country/Region`)) + 
  theme_classic()  + 
  labs(y="All confirmed cases") 


# ++ Does the USA fit an exponential curve? ---- 

start_date <- as.Date(x = "03/01/2020",format= "%m/%d/%y")

US_data_recent <- US_data %>%
  filter(Date > start_date)


ggplot(data = US_data_recent) + 
  geom_point(aes(x=Date, y=all_cases,color=`Country/Region`)) + 
  theme_classic()  + 
  labs(y="All confirmed cases") + 
  scale_y_log10()


fit_lm_exp <- lm(formula = log10(all_cases) ~ Date, data = US_data_recent)

fit_lm_exp$model

end_date <- as.Date(x = "04/12/2020",format= "%m/%d/%y")

future_predictions <- data.frame(Date = seq(start_date,end_date,by = "1 day"))

future_predictions$log10_count <- predict(fit_lm_exp,newdata = future_predictions)


US_data_recent_log <- US_data_recent %>%
  mutate(all_cases = log10(all_cases))


ggplot(data = US_data) + 
  geom_point(aes(x=Date, y=log10(all_cases),color=`Country/Region`)) + 
  geom_line(data = future_predictions, aes(x=Date,y=log10_count),linetype="dashed") + 
  theme_classic()  + 
  labs(y="log10(All confirmed cases)") + 
  scale_x_date(date_breaks = "2 days",date_labels = "%b %d") + 
  theme(axis.text.x = element_text(angle=90))



ggplot(data = US_data) + 
  geom_point(aes(x=Date, y=log10(all_cases),color=`Country/Region`)) + 
  geom_line(data = future_predictions, aes(x=Date,y=log10_count),linetype="dashed") + 
  geom_hline(yintercept = log10(924100 ), color="red") +
  geom_text(aes(x=as.Date("2020-01-19"),y=log10(924100 ),
                label="Number of hospital beds in USA"),hjust=0,vjust=0) +
  theme_classic()  + 
  labs(y="log10(All confirmed cases in USA)") + 
  scale_x_date(date_breaks = "2 days",date_labels = "%b %d") + 
  theme(axis.text.x = element_text(angle=90))


# ggplot(data = US_data) + 
#   geom_point(aes(x=Date, y=all_cases,color=`Country/Region`)) + 
#   geom_line(data = future_predictions, aes(x=Date,y=10^(log10_count)),linetype="dashed") + 
#   theme_classic()  + 
#   labs(y="log10(All confirmed cases)") + 
#   scale_x_date(date_breaks = "2 days",date_labels = "%b %d") + 
#   theme(axis.text.x = element_text(angle=90)) + 
#   scale_y_log10()



data_OI<- jhu_data_m %>%
  filter(`Country/Region` != "Mainland China") %>%
  filter(`Country/Region` != "China")

over_100 <- data_OI %>%
  filter(Confirmed_cases > 100)
as.character(unique(over_100$`Country/Region`))
# no_china[which.max(no_china$Confirmed_cases),]

data_OI <- data_OI %>% 
  filter(`Country/Region` %in% as.character(unique(over_100$`Country/Region`)))


ggplot(data = data_OI) + 
  geom_point(aes(x=Date, y= Confirmed_cases, color=`Country/Region`)) + 
  theme_classic() + 
  scale_y_log10()


US_data <- jhu_data_m %>%
  filter(`Country/Region` == "US") %>%
  group_by(Date, `Country/Region`) %>%
  summarize(all_cases = sum(Confirmed_cases))


ggplot(data = US_data,aes(x=Date, y= all_cases, color=`Country/Region`)) + 
  geom_line(size=1,linetype="dashed",color="black") + 
  geom_point(size=2) + 
  theme_classic() + 
  labs(y="Confirmed cases",title="Confirmed cases in the USA")

ggplot2::ggsave()


US_data_by_state <- jhu_data_m %>%
  filter(`Country/Region` == "US") %>%
  group_by(Date, `Province/State`) %>%
  summarize(all_cases = sum(Confirmed_cases))



ggplot(data = US_data_by_state,aes(x=Date, y= all_cases, color=`Province/State`)) + 
  geom_line(size=1,linetype="dashed",color="black") + 
  geom_point(size=2) + 
  theme_classic() + 
  labs(y="Confirmed cases",title="Confirmed cases in the USA")


italy_data <- jhu_data_m %>%
  filter(`Country/Region` == "Italy") 

ggplot(data = italy_data, aes(x=Date, y=Confirmed_cases)) + 
  geom_point() + 
  theme_classic() 












