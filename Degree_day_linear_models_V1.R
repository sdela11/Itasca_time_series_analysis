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
head(data)
str(data)

DD.data <- read.csv("../Itasca_summary_code/degree_days_OCT_v2.csv")
head(DD.data)

dailymeans.df <- mutate(data, date.time = as.POSIXct(date.time, format = "%Y-%m-%d %H:%M")) %>% 
  group_by(name, date(date.time)) %>% 
  summarise(meantemp = mean(value))

head(dailymeans.df)

dailymeans.df <- dailymeans.df %>% filter(meantemp > 0)


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
  
  

time.start <- 
time.break.one <-
time.break.two <- 
time.end <- 
