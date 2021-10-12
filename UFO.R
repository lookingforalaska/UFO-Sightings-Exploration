# title: "ISTA 320 Final Project - UFOs"
# author: "Kalyssa Harris"
# date: "Fall 2021"
# desc: Over the last couple of years, there has been a marked increase in UFO 
#       related activity following government declassifications on military 
#       encounters with them which sparks interest in this topic. This project
#       is an introductory exploration of UFO sightings in the United States.
#       
#       Original data includes datetime, city, state, country, shape, duration,
#       description of encounter, date posted, and the longitude and latitude
#       The data set can be found on https://www.kaggle.com/NUFORC/ufo-sightings

#import libraries
library(scales)
library(dplyr)
library(lubridate)
library(ggrepel)
library(usmap)
library(ggplot2)
library(tidyverse)

#read data in
ufo_data <- read.csv("data/ufo_data.csv")

head(ufo_data) #preview

usa_ufos <- ufo_data %>%
  filter(country == "us", state != 'pr', state != 'dc') #filter for only 50 US states

usa_frequency <- as.data.frame(table(usa_ufos$state)) %>%
  setNames(c("state", "freq"))

usa_frequency$state <- as.character(usa_frequency$state) #convert to char
usa_frequency$state <- str_to_upper(usa_frequency$state) #uppercase abbreviations

#get our labels
centroid_labels <- utils::read.csv(
  system.file("extdata", 
              paste0("us_", "states", "_centroids.csv"),
              package = "usmap"), 
  stringsAsFactors = FALSE
) %>% 
  mutate(fips = stringr::str_pad(fips, 2, pad = "0"))


usa_frequency_complete <- merge(usa_frequency, centroid_labels, by.x=c("state"),
                                by.y=c("abbr"))

usa_frequency %>%
  plot_usmap(data = ., 
             values = "freq") +
  theme(legend.position = "right") +
  scale_fill_continuous(name = "Number of Sightings", low = "cornflowerblue", high = "coral4") +
  ggtitle("Frequency of UFO Sightings By US State") +
  theme(panel.background = element_rect(colour = "black")) +
  geom_label_repel(data = usa_frequency_complete,
                   aes(x = x,
                       y = y,
                       label = paste(state,freq)),
                   size = 2.9, max.overlaps = 19) 

usa_ufos <- usa_ufos %>% separate(datetime, c("date", "time"), sep = " ") #separate date and time
usa_ufos$date <- as.Date(usa_ufos$date, format = "%m/%d/%Y") #convert to date type

seasons = function(x){
  if(x %in% 2:4) return("Spring")
  if(x %in% 5:7) return("Summer")
  if(x %in% 8:10) return("Fall")
  if(x %in% c(11,12,1)) return("Winter")
}

usa_ufos$season = sapply(month(usa_ufos$date), seasons) #assigns a season value based off month

usa_ufos$convert_time <- hour(hm(usa_ufos$time))
your_breaks <- hour(hm("00:00", "6:00", "12:00", "18:00", "24:00"))
your_labels <- c("Night", "Morning", "Afternoon", "Evening")
usa_ufos$time_of_day <- cut(x=usa_ufos$convert_time, breaks=your_breaks, #assigns time of day value
                            labels=your_labels, include.lowest=TRUE)

usa_ufos %>%
  mutate(season = factor(season, levels = c("Spring",
                                        "Winter",
                                        "Summer",
                                        "Fall"))) %>%
ggplot(aes(x = season, fill = time_of_day)) + 
  geom_bar() +
  labs(title = "Reported UFO Sightings in the USA (1910-2014)", x = "Season", y = "Number of Sightings") +
  guides(fill = guide_legend(title = "Time of Day")) +
  scale_fill_manual(values = c("#8DB7FD", "#4D91FF", "#1757D1", "#003394")) +
  geom_text(stat='count', aes(label=..count..), position = position_stack(vjust=0.5),
            colour = "white", size = 5)



populous_cities <- usa_ufos %>%
  filter(date <= as.Date("2014-04-30") & date >= as.Date("2010-01-01")) %>%
  filter(city == 'new york city' | city == 'los angeles' | city == 'chicago' | 
           city == 'houston' | city == 'phoenix' | city == 'philadelphia' )


populous_cities <- populous_cities %>% separate(date, c("year"), sep = "-")
populous_cities <- subset(populous_cities, select=c("year", "city"))
populous_cities <- rename(count(populous_cities, year, city), Freq = n)
populous_cities$year <- as.integer(populous_cities$year)

populous_cities %>%
  ggplot(aes(x = year,
             y = Freq,
             label = Freq)) +
  geom_line() +
  labs(title = "Reported UFO Sightings for 6 Largest US Cities (2010-2014)", 
       x = "Year", y = "Number of Sightings") +
  scale_x_continuous(breaks = c(2010, 2011, 2012, 2013, 2014),
                     guide = guide_axis(check.overlap = TRUE),
                     expand = expansion(mult = c(.5, .5))) +
  scale_y_continuous(expand = expansion(mult = c(.5, .5))) +
  facet_wrap(city~., scales = "free") +
  labs(caption = "y axis scale is different in each subplot") +
  geom_label()