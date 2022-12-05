library(tidyverse)
library(dplyr)
library(usdata)

# The functions might be useful for A4
setwd("~/Documents/info201/assignments/a4-lyarrington/")
source("~/Documents/info201/assignments/a4-lyarrington//source/a4-helpers.R")

incarceration_df <- read.csv("incarceration_trends.csv")

## Test queries ----
#----------------------------------------------------------------------------#
# Simple queries for basic testing
#----------------------------------------------------------------------------#
# Return a simple string
test_query1 <- function() {
  return ("Hello world")
}

# Return a vector of numbers
test_query2 <- function(num=6) {
  v <- seq(1:num)
  return(v)
}

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
# Your functions might go here ... <todo:  update comment>
#----------------------------------------------------------------------------#
# This function ... <todo:  update comment>
get_year_jail_pop <- function() {
  df <- incarceration_df %>% 
    select(year, total_jail_pop) %>% 
    drop_na() %>% 
    group_by(year) %>% 
    summarise(total = sum(total_jail_pop))
  return(df)   
}

# This function ... <todo:  update comment>
plot_jail_pop_for_us <- function()  {
  bar_chart <- ggplot(get_year_jail_pop(), aes(year, total)) +
    geom_bar(stat='identity') +
    labs(y = "Total Jail Population", x = "Year") +
    scale_y_continuous(labels = scales::comma) +
    ggtitle("Increase of Jail Population in U.S. (1970-2018)")
  return(bar_chart)   
}

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

# Your functions might go here ... <todo:  update comment>
# See Canvas
#----------------------------------------------------------------------------#

## Section 5  ---- 
#----------------------------------------------------------------------------#
# Increase in Various Races in Jail Population in U.S. 

get_white_jail_pop <- function() {
  df <- incarceration_df %>% 
    select(year, white_jail_pop) %>% 
    drop_na()
  df <- aggregate(df$white_jail_pop, list(df$year), sum)
  df$Race <- "White"
  colnames(df)[1] <- "Year"
  colnames(df)[2] <- "Total"
  df <- subset(df, Total > 0)
  return(df)   
}

get_black_jail_pop <- function() {
  df <- incarceration_df %>% 
    select(year, black_jail_pop) %>% 
    drop_na()
  df <- aggregate(df$black_jail_pop, list(df$year), sum)
  df$Race <- "Black"
  colnames(df)[1] <- "Year"
  colnames(df)[2] <- "Total"
  df <- subset(df, Total > 0)
  return(df)   
}

get_latinx_jail_pop <- function() {
  df <- incarceration_df %>% 
    select(year, latinx_jail_pop) %>% 
    drop_na()
  df <- aggregate(df$latinx_jail_pop, list(df$year), sum)
  df$Race <- "Latinx"
  colnames(df)[1] <- "Year"
  colnames(df)[2] <- "Total"
  df <- subset(df, Total > 0)
  return(df)   
}

get_aapi_jail_pop <- function() {
  df <- incarceration_df %>% 
    select(year, aapi_jail_pop) %>% 
    drop_na()
  df <- aggregate(df$aapi_jail_pop, list(df$year), sum)
  df$Race <- "AAPI"
  colnames(df)[1] <- "Year"
  colnames(df)[2] <- "Total"
  df <- subset(df, Total > 0)
  return(df)   
}

get_native_jail_pop <- function() {
  df <- incarceration_df %>% 
    select(year, native_jail_pop) %>% 
    drop_na()
  df <- aggregate(df$native_jail_pop, list(df$year), sum)
  df$Race <- "Native"
  colnames(df)[1] <- "Year"
  colnames(df)[2] <- "Total"
  df <- subset(df, Total > 0)
  return(df)   
}

get_race_jail_pop <- function() {
  df <- full_join(get_white_jail_pop(), get_black_jail_pop())
  df <- full_join(df, get_latinx_jail_pop())
  df <- full_join(df, get_aapi_jail_pop())
  df <- full_join(df, get_native_jail_pop())
  return(df)
}

plot_race_jail_pop <- function() {
  p <- ggplot(get_race_jail_pop(), aes (x = Year, y = Total, fill = Race)) +
    geom_bar(stat = "identity",
             position = "dodge") +
    ggtitle("Increase in White and Black Jail Population in U.S. (1970-2018)") +
    xlab("Year") +
    ylab("Total Jail Population")
  return(p)
}

# Your functions might go here ... <todo:  update comment>
# See Canvas
#----------------------------------------------------------------------------#

## Section 6  ---- 
#----------------------------------------------------------------------------#
# <a map shows potential patterns of inequality that vary geographically>

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


# Your functions might go here ... <todo:  update comment>
# See Canvas
#----------------------------------------------------------------------------#

## Load data frame ---- 


