library(tidyverse)
library(dplyr)
library(usdata)
library(ggplot2)

# The functions might be useful for A4
setwd("~/Documents/info201/assignments/a4-lyarrington/")
source("~/Documents/info201/assignments/a4-lyarrington//source/a4-helpers.R")

incarceration_df <- read.csv("incarceration_trends.csv")

## Section 2  ---- 
#----------------------------------------------------------------------------#
total_by_year <- incarceration_df %>% 
  select(year, white_pop_15to64, black_pop_15to64) %>% 
  drop_na() %>% 
  group_by(year) %>% 
  summarise(total_white = sum(white_pop_15to64), total_black = sum(black_pop_15to64))

total_1990_white <- total_by_year %>% 
  filter(year == "1990") %>% 
  pull(total_white) 

total_1990_black <- total_by_year %>% 
  filter(year == "1990") %>% 
  pull(total_black)
  
total_2018_white <- total_by_year %>% 
  filter(year == "2018") %>% 
  pull(total_white)

total_2018_black <- total_by_year %>% 
  filter(year == "2018") %>% 
  pull(total_black)

increase_white <- total_2018_white - total_1990_white
increase_black <- total_2018_black - total_1990_black

  
#----------------------------------------------------------------------------#

## Section 3  ---- 
#----------------------------------------------------------------------------#
# Growth of the U.S. Prison Population

get_year_jail_pop <- function() {
  df <- incarceration_df %>% 
    select(year, total_jail_pop) %>% 
    drop_na() %>% 
    group_by(year) %>% 
    summarise(total = sum(total_jail_pop))
  return(df)   
}

plot_jail_pop_for_us <- function()  {
  bar_chart <- ggplot(get_year_jail_pop(), aes(year, total)) +
    geom_bar(stat='identity') +
    labs(y = "Total Jail Population", x = "Year") +
    scale_y_continuous(labels = scales::comma) +
    ggtitle("Increase of Jail Population in U.S. (1970-2018)")
  return(bar_chart)   
}

#----------------------------------------------------------------------------#

## Section 4  ---- 
#----------------------------------------------------------------------------#
# Growth of Prison Population by State 

get_jail_pop_by_states <- function(states) {
  state1 <- incarceration_df %>% 
    select(year, total_jail_pop, state) %>% 
    drop_na() %>% 
    filter(state == states[1]) %>% 
    group_by(year) %>% 
    summarize(total = sum(total_jail_pop))
  
  state2 <- incarceration_df %>% 
    select(year, total_jail_pop, state) %>% 
    drop_na() %>% 
    filter(state == states[2]) %>% 
    group_by(year) %>% 
    summarize(total = sum(total_jail_pop))
  
  state3 <- incarceration_df %>% 
    select(year, total_jail_pop, state) %>% 
    drop_na() %>% 
    filter(state == states[3]) %>% 
    group_by(year) %>% 
    summarize(total = sum(total_jail_pop))
  
  state4 <- incarceration_df %>% 
    select(year, total_jail_pop, state) %>% 
    drop_na() %>% 
    filter(state == states[4]) %>% 
    group_by(year) %>% 
    summarize(total = sum(total_jail_pop))

  df <- data.frame(x = 1970:2018, 
             y1 = c(state1$total),
             y2 = c(state2$total),
             y3 = c(state3$total),
             y4 = c(state4$total))
  
  return(df)
}

plot_jail_pop_by_states <- function(states) {
  line_chart <- ggplot(get_jail_pop_by_states(states), aes(x)) +  
    geom_line(aes(y = y1), color = "black") +
    geom_line(aes(y = y2), color = "red") +
    geom_line(aes(y = y3), color = "green") +
    geom_line(aes(y = y4), color = "blue") +
    labs(y = "Total Jail Population", x = "Year") +
    scale_y_continuous(labels = scales::comma) +
    ggtitle("Increase of Jail Population in U.S. by State (1970-2018)")
  return(line_chart)
}

#----------------------------------------------------------------------------#

## Section 5  ---- 
#----------------------------------------------------------------------------#
# White vs. Black Jail Population by State in 2018

get_black_jail_pop_by_state <- function() {
  df <- incarceration_df %>% 
    select(year, state, black_jail_pop) %>% 
    drop_na() %>% 
    filter(year == 2018) 
  df <- aggregate(df$black_jail_pop, list(df$state), sum)
  colnames(df)[1] <- "State"
  colnames(df)[2] <- "Black_Pop"
  return(df)
}

get_white_jail_pop_by_state <- function() {
  df <- incarceration_df %>% 
    select(year, state, white_jail_pop) %>% 
    drop_na() %>% 
    filter(year == 2018) 
  df <- aggregate(df$white_jail_pop, list(df$state), sum)
  colnames(df)[1] <- "State"
  colnames(df)[2] <- "White_Pop"
  return(df)
}

get_jail_pop_by_state <- function() {
  df <- inner_join(get_black_jail_pop_by_state(), get_white_jail_pop_by_state(), by = 'State')
  return(df)
}

plot_race_jail_pop <- function() {
  df <- get_jail_pop_by_state()
  p <- ggplot(df, aes (x = White_Pop, y = Black_Pop, fill = State)) +
    geom_point() +
    geom_text(label=df$State, hjust=1, vjust=-0.5, size = 2.5) +
    theme(legend.position="none") +
    ggtitle("White vs. Black Jail Population by State in 2018") +
    xlab("White Jail Population") +
    ylab("Black Jail Population")
  return(p)
} 

#----------------------------------------------------------------------------#

## Section 6  ---- 
#----------------------------------------------------------------------------#

get_poc_jail_percentage <- function() {
  main_states <- map_data("state")
  df <- incarceration_df %>% 
    select(year, state, aapi_pop_15to64, black_pop_15to64, latinx_pop_15to64, native_pop_15to64,
           aapi_jail_pop, black_jail_pop, latinx_jail_pop, native_jail_pop) %>% 
    filter(year == "2018") %>% 
    drop_na()
  df$total_poc <- df$aapi_pop_15to64 + df$black_pop_15to64 + df$latinx_pop_15to64 + df$native_pop_15to64
  df$total_poc_jail <- df$aapi_jail_pop + df$black_jail_pop + df$latinx_jail_pop + df$native_jail_pop
  df$total_poc <- df$total_poc / 100
  df <- df %>% 
    select(state, total_poc, total_poc_jail) %>% 
    group_by(state) %>% 
    summarise(across(c(total_poc, total_poc_jail), mean))
  df$state <- abbr2state(df$state)
  df$state <- tolower(df$state)
  colnames(df)[1] <- "region"
  df$percentage <- df$total_poc_jail / df$total_poc
  merged_states <- inner_join(main_states, df, by = "region")
  return(merged_states)
} 

plot_map <- function() {
  df <- get_poc_jail_percentage()
  map <- ggplot(df, aes(long, lat)) +
    geom_polygon(data=df, aes(x=long, y=lat, group=group, fill = percentage * 100)) +
    ggtitle("Percentage of POC in Jail Across U.S") +
    xlab("Longitute") +
    ylab("Latitude") +
    scale_fill_continuous(name = "% of POC in Jail")
  return(map)
}

#----------------------------------------------------------------------------#