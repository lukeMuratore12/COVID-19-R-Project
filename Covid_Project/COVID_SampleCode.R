library(dplyr)
library(ggplot2)
library(tidyverse)
library(janitor)
library(gganimate)
library(gifski)
library(png)
library(ggrepel)
library(scales)
#change the path below to the address to the file on your computer.

#setwd(dir = "Desktop/Jonathan/Teaching/Spring2021/Stat400/Covid_Stat400/")
#path <- "~/Desktop/Jonathan/Teaching/Spring2021/Stat400/Covid_Stat400/COVID-19-master_Nov3/csse_covid_19_data/csse_covid_19_time_series/time_series_covid19_confirmed_US.csv"
path <- "~/Desktop/Jonathan/Teaching/Spring2021/Stat400/Covid_Stat400/COVID-19-master_March12/csse_covid_19_data/csse_covid_19_time_series/time_series_covid19_confirmed_US.csv"
df <- read.csv(path)
head(df)

covid_full <- df %>% select(c(7, 12:426)) 
covid_map_state <- covid_full %>% group_by(Province_State) %>% 
  summarize_at(vars(2:416), sum) %>% adorn_totals('row')
covid_map_state$Province_State <- tolower(covid_map_state$Province_State)

#unique(covid_map_full$Province_State)
us_states <- map_data("state")
cont_states <- unique(us_states$region)

covid_cont_states <- covid_map_state %>% 
  filter(Province_State %in%  cont_states) %>%
  dplyr::rename(region = Province_State)

col_names<- colnames(covid_cont_states)[c(2:416)]
new_col_names <- seq(as.Date("2020/1/21"), by = "day", length.out = 415) #get dates in date-time format

covid_cont_states<- covid_cont_states %>% 
  setNames(new_col_names) %>%
  dplyr::rename(region = "2020-01-21")




#Prepare data for plotting
covid_cont_state.long <- pivot_longer(covid_cont_states, 
                                      cols = c(2:415), 
                                      names_to = "date", 
                                      values_to = "cases")


#Choose states to plot
plot_states <- c("new york", "florida", "texas", "california")

#data for plotting specific states
covid_plot_data <- covid_cont_state.long %>% filter(region == plot_states)

#Plot the data
q <- covid_plot_data %>% 
  ggplot(aes(x = as.Date(date), 
             y = cases, 
             group = region,
             color = region))+
  labs(title = "Time Series Plot of Confirmed Cases of COVID",
       subtitle = "Jonathan Fernandes PhD, University of Maryland, College Park.",
       x = "Date", 
       y = "Number of Cases")+
  geom_line()+
  geom_point()+
  geom_text(aes(label = factor(region)), 
            hjust = 0, 
            position = position_dodge(width=0.9),  
            size=4)+
  scale_x_date(limits = as.Date(c("2020-1-22","2021-03- 12")), 
               date_breaks = "1 month",
               date_labels="%B",
               guide = guide_axis(angle = 45))+
  scale_y_continuous(labels = scales::comma)+
  theme(
    plot.title = element_text(color="black", size=14, face="bold.italic"),
    axis.title.x = element_text(color="black", size=10),
    axis.title.y = element_text(color="black", size=10),
    legend.position = "none")#+

q

p <- covid_plot_data %>% 
  ggplot(aes(x = as.Date(date), 
             y = cases, 
             group = region,
             color = region))+
  labs(title = "Time Series Plot of Confirmed Cases of COVID",
       subtitle = "Jonathan Fernandes PhD, University of Maryland, College Park.",
       x = "Date", 
       y = "Number of Cases")+
  geom_line()+
  scale_x_date(limits = as.Date(c("2020-1-22","2021-03- 12")), 
               date_breaks = "1 month",
               date_labels="%B",
               guide = guide_axis(angle = 45))+
  scale_y_continuous(labels = scales::comma)+
  theme(
    plot.title = element_text(color="black", size=14, face="bold.italic"),
    axis.title.x = element_text(color="black", size=10),
    axis.title.y = element_text(color="black", size=10))
  
p  
  
q <- q +  theme(
  #panel.background = element_rect(fill = "white"),
  plot.margin = margin(5,10 ,5, 1, "mm"),
  plot.background = element_rect(
    fill = "white",
    colour = "black",
    size = 1
  )
)


anim <- q + transition_reveal(as.Date(date))
animation <- animate(anim,
                     width = 750, 
                     height = 650,
                     duration = 20,
                     end_pause = 20,
                     renderer = gifski_renderer())

animation
#facet_grid(~region)


