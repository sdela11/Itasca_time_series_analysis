#Degree_day_linear_models_V1.R
#Degree Day Statistical Analysis with linear models V1
#Created by: Sara DeLaurentis
#Date: 2023-02-22
#Purpose: Final coding for Degree Day data analysis outputs

library(tidyverse)
library(lubridate)
library(stringr)
library(dplyr)
library(glue)
library(lme4)
library(lmerTest)
library(emmeans)

data <- read_csv("../ALL_A.csv")
data$`...1` = NULL
head(data)
str(data)

#DD.data <- read.csv("../Itasca_summary_code/degree_days_OCT_v2.csv")
#head(DD.data)

dailymeans.df <- mutate(data, date.time = as.POSIXct(date.time, format = "%Y-%m-%d %H:%M")) %>% 
  group_by(name, date(date.time)) %>% 
  summarise(meantemp = mean(value))
colnames(dailymeans.df)[2]<- "date" #rename grouping column

head(dailymeans.df)

dailymeans.df <- dailymeans.df %>% filter(meantemp > 0)


#setting up site,rep,position (SRP) names.

SRP.df <- as_tibble(str_split_fixed(dailymeans.df$name, "_", 5))
colnames(SRP.df) <- c("site", "rep", "position", "buttonID", "season")
head(SRP.df)
SRP.df$SRP.name <- paste(SRP.df$site, SRP.df$rep, SRP.df$position, sep = "_")
head(SRP.df)

dailymeans2.df <- cbind(SRP.df$SRP.name,dailymeans.df)
colnames(dailymeans2.df)[1] <- c("SRP.name")
view(dailymeans2.df)

#RENAME DF

DD.df <- dailymeans2.df

#code for remembering
#DD.df <- dailymeans.df %>% 
#  group_by(SRP.name) %>% 
#  summarise(SRP.name = SRP.name,
#            degree.days = sum(meantemp)) %>%  #create degree.days column by outputting the sum of meantemp for each position. (Not sensor, but position)
#  distinct(SRP.name, .keep_all = TRUE) %>% 
#  ungroup()
#view(DD.df)  


#Helpful code for remembering.

#  C2A_R1_m10_dailymean <- mutate(C2A_R1_m10,'date.time' = mdy_hm(date.time)) %>% 
#  separate('date.time',
#           into = c('longdate', 'time'),
#           sep = ' ') %>% 
#  separate('longdate',
#           into = c('year', 'month', 'day'),
#           sep = '-',
#           remove = FALSE) %>% 
#  group_by(year, month, day) %>% 
#  summarise(meantemp = mean(value)) 
  

### TIME BREAKS ###

time.start <- as.Date("2020-04-01")
time.break.one <-
time.break.two <- 
time.end <- as.Date("2020-09-30")
print(time.start)
print(time.end)

### subsetting the dataframe

DD.df.cut <- DD.df[(DD.df$date >= time.start) & (DD.df$date <= time.end),]

view(DD.df.cut)

DDsums.df <- DD.df.cut %>% 
    group_by(SRP.name) %>% 
    summarise(SRP.name = SRP.name,
              degree.days = sum(meantemp)) %>%  #create degree.days column by outputting the sum of meantemp for each position. (Not sensor, but position)
    distinct(SRP.name, .keep_all = TRUE) %>% 
    ungroup()
  view(DDsums.df)  
