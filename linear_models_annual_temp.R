#title: linear models annual temp
#created: 2022-04-19

library(tidyverse)
library(lubridate)
library(dplyr)
library(stringr)
library(nlme)

#manually set working directory to Itasca_project_19-21 using the Files tab in the sidebar.

#Read-in data

all <- read.csv("ALL.csv")
all$date.time <- as.POSIXct(all$date.time)
class(all$date.time)


head(all)

