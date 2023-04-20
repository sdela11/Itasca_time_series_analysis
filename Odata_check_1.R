library(tidyverse)
library(lubridate)
library(stringr)
library(broom)
library(dplyr)
library(glue)
library(lme4)
library(lmerTest)

data <- read_csv("../ALL_A.csv")
data$`...1` = NULL #remove strange column that was created upon read-in.
head(data)

data <- data[!(data$site == "D5B"),] #remove all D5B sensors.
D5B.chk <- data[data$site == "D5B",]
D5B.chk

