# This is the exact R code from the R markdown file, here for extra clarity

library(dplyr)
library(janitor)
library(gganimate)
library(gifski)
library(png)
library(plyr)
library(readr)
library(ggplot2)
library(tidyverse)
library(ggrepel)
library(scales)
library(gapminder)
library(shinyWidgets)
library(data.table)
library(ggplot2)
library(gganimate)
library(knitr)

path_obesity <- "./Obesity_Rates.csv"
obesity <- read.csv(path_obesity)
obesity[c(1, 50),]

path_pd <- "./Population_Density.csv"
pd <- read.csv(path_pd)
pd[c(32, 36),]

plot(1:50, pd$Population_Density, xlab="States", ylab="Obesity Rates", type = "h",)
ms_pd = pd %>% filter(State == "Mississippi")
co_pd = pd %>% filter(State == "Colorado")
abline(h = ms_pd$Population_Density, col = "red") # abline for Mississippi
abline(h = co_pd$Population_Density, col = "blue") # abline for Colorado

path_US_Death <- "./Covid_Project/COVID-19-master_March12/csse_covid_19_data/csse_covid_19_time_series/time_series_covid19_deaths_US.csv"
path_US_confirmed <- "./Covid_Project/COVID-19-master_March12/csse_covid_19_data/csse_covid_19_time_series/time_series_covid19_confirmed_US.csv"

deaths <- read.csv(path_US_Death) #Reads in Deaths Over Time csv
cases <- read.csv(path_US_confirmed) #Reads in Cases Over Time csv

cases = cases %>% select("Province_State" | starts_with("X"))

deaths = deaths %>% select("Province_State" | starts_with("X"))

cases = cases %>% group_by(Province_State) %>% dplyr::summarise(across(starts_with("X"), sum))

deaths = deaths %>% group_by(Province_State) %>% dplyr::summarise(across(starts_with("X"), sum))

setDT(cases)
cases = cases %>% melt(cases, id=c("Province_State"), measure=patterns("^X"), value.name="Cases", variable.name="Date")
setDT(deaths)
deaths = deaths %>% melt(deaths, id=c("Province_State"), measure=patterns("^X"), value.name="Deaths", variable.name="Date")
cases$Date = as.Date(cases$Date, format="X%m.%d.%y")
deaths$Date = as.Date(deaths$Date, format="X%m.%d.%y")
covid <- merge(cases, deaths, by=c("Province_State", "Date"))
ms <- "Mississippi"
co <- "Colorado"
ms_covid = covid %>% filter(Province_State == ms)
co_covid = covid %>% filter(Province_State == co)


cases_plt_ms = ggplot( ms_covid,
                       aes(x=Date,y=Cases)) + 
  geom_point(show.legend = FALSE, alpha = .7) + 
  labs(x="Date", 
       y="Cases", 
       title=paste("COVID Cases Over Time For", ms)) 
plot(cases_plt_ms)

deaths_plt_ms = ggplot( ms_covid, 
                        aes(x=Date,y=Deaths)) +
  geom_point(show.legend = FALSE, alpha = .7) + 
  labs(x="Date",
       y="Deaths",
       title=paste("COVID Deaths Over Time For", ms))
plot(deaths_plt_ms)

cases_plt_co = ggplot( co_covid,
                       aes(x=Date,y=Cases)) + 
  geom_point(show.legend = FALSE, alpha = .7) + 
  labs(x="Date", 
       y="Cases", 
       title=paste("COVID Cases Over Time For", co)) 
plot(cases_plt_co)

deaths_plt_co = ggplot( co_covid, 
                        aes(x=Date,y=Deaths)) +
  geom_point(show.legend = FALSE, alpha = .7) + 
  labs(x="Date",
       y="Deaths",
       title=paste("COVID Deaths Over Time For", co))
plot(deaths_plt_co)
