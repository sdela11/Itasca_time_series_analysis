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

DD.data <- read.csv("../Itasca_summary_code/degree_days_OCT_v2.csv")
head(DD.data)

dailymeans.df <- 


time.start <- 
time.break.one <-
time.break.two <- 
time.end <- 
