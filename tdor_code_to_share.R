library(tidyverse)
library(lubridate)
data <- readr::read_csv("https://raw.githubusercontent.com/brennanpincardiff/tg_data_working/master/TDOR_15to18_for_R_20181113.csv")

data <- data %>%
    mutate(year = year(Date))
data_s <- filter(data, year !=2014)

data_s %>%
    group_by(year) %>%
    summarise(n = n()) -> year_total

# facetted plot
ggplot(data_s, aes(Country)) + geom_bar() + 
    labs(y = "Deaths", x = "") +
    ggtitle("Deaths in different countries (2015-2018)") +
    theme_bw() +
    theme(axis.text.x = element_text(angle=45, hjust=1)) +
    facet_wrap(~year) +
    theme(axis.text.x = element_text(size = 8)) +
    geom_text(data = year_total,
        mapping = aes(x=35, y=150, label=paste0("Total Deaths=",n)))


# make the histogram animation
library(ggplot2)
library(gganimate)

# basic animation works 
ggplot(data_s, aes(Country)) + geom_bar() + 
    labs(y = "Deaths", x = "") +
    theme_bw() +
    theme(axis.text.x = element_text(angle=45, hjust=1)) +
    theme(axis.text.x = element_text(size = 8)) +
    # geom_text(data = year_total,
    #     mapping = aes(x=35, y=150, label=paste0("Total Deaths=",n)))
    labs(title = 'Deaths by country: {closest_state}') +
    transition_states(year,
        transition_length = 1,
        state_length = 2) +
    enter_fade() + 
    exit_shrink() +
    ease_aes('sine-in-out')


# world map and animation
library(ggplot2)
library(maps)
library(ggthemes)

world <- ggplot() +
    borders("world", colour = "gray85", fill = "gray80") +
    theme_map() 

data_18 <- readr::read_csv("https://raw.githubusercontent.com/brennanpincardiff/tg_data_working/master/TDOR_for_R_20181113.csv")
death_map_18 <- world +
    geom_point(aes(x = Longitude, y = Latitude),
        data = data_18, 
        colour = 'purple', alpha = .4) +
    ggtitle("Map of deaths during 2018")
death_map_18


data_17 <- readr::read_csv("https://raw.githubusercontent.com/brennanpincardiff/tg_data_working/master/TDOR_2017_for_R_20181113.csv")
map_anim_name_17 <- world +
    geom_point(aes(x = Longitude, y = Latitude),
        data = data_17, 
        colour = 'red', alpha = .5) +
    labs(title = paste('{closest_state}', "died here")) +
    transition_states(
        Name,
        transition_length = 1,
        state_length = 1) + shadow_mark()

# to get full animation...
animate(map_anim_name, nframes = 706)