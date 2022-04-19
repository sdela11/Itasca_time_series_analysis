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

year.OCT <- read.csv("annual_sums_OCT_1.csv")

head(year.OCT)
head(all)


#attempt a linear model

annual.lme <- lme(meantemp~O.thk, data=WholeSeason_19.20) 

site	plot	2yr_avg_O
C2A	R1	6
C2A	R2	4.5
C2A	R3	4
C2B	R1	6
C2B	R2	7
C2B	R3	7
C5A	R1	0
C5A	R2	0.5
C5A	R3	0
C5B	R1	5
C5B	R2	3
C5B	R3	0
D2A	R1	6
D2A	R2	6
D2A	R3	5.5
D2B	R1	5
D2B	R2	4.5
D2B	R3	3
D5A	R1	0
D5A	R2	0
D5A	R3	0.5

## FOR INPUT INTO THIS MODEL, I NEED: ANNUAL TEMPERATURE (MAT), 