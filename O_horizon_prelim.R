# Title: O-horizon processing
# By: Sara DeLaurentis
# Date created: 2023-03-25
# Purpose: Check O and A horizon data sheet: 

library(tidyverse)
library(dplyr)
library(stringr)
library(glue)
library(ggplot2)

# read in data

OA.dat <- read.csv("../OA_thickness_2019_21_clean_20230325.csv")
head(OA.dat)
str(OA.dat)

# set classes for columns
OA.dat$inv_lvl <- as.character(OA.dat$inv_lvl)
OA.dat$Oi_thk <- as.numeric(OA.dat$Oi_thk)
OA.dat$Oe_thk <- as.numeric(OA.dat$Oe_thk)
OA.dat$Oa_thk <- as.numeric(OA.dat$Oa_thk)
OA.dat$O.A_thk <- as.numeric(OA.dat$O.A_thk)
OA.dat$bulk_density <- as.numeric(OA.dat$bulk_density)

# create objects of each group, check the number of observations

C2A.Othk <- OA.dat %>% filter(site == "C2A") %>% select(date:totalO_thk)
sum(!is.na(C2A.Othk$totalO_thk))
view(C2A.Othk)

C2B.Othk <- OA.dat %>% filter(site == "C2B") %>% select(date:totalO_thk)
sum(!is.na(C2B.Othk$totalO_thk))
view(C2B.Othk)

D2A.Othk <- OA.dat %>% filter(site == "D2A") %>% select(date:totalO_thk)
sum(!is.na(D2A.Othk$totalO_thk))
view(D2A.Othk)

D2B.Othk <- OA.dat %>% filter(site == "D2B") %>% select(date:totalO_thk)
sum(!is.na(D2B.Othk$totalO_thk))

C5A.Othk <- OA.dat %>% filter(site == "C5A") %>% select(date:totalO_thk)
sum(!is.na(C5A.Othk$totalO_thk))

C5B.Othk <- OA.dat %>% filter(site == "C5B") %>% select(date:totalO_thk)
sum(!is.na(C5B.Othk$totalO_thk))

D5A.Othk <- OA.dat %>% filter(site == "D5A") %>% select(date:totalO_thk)
sum(!is.na(D5A.Othk$totalO_thk))
view(D5A.Othk)

D5B.Othk <- OA.dat %>% filter(site == "D5B") %>% select(date:totalO_thk)
sum(!is.na(D5B.Othk$totalO_thk))


site.stats.df <- OA.dat %>% 
  filter(!is.na(totalO_thk)) %>% 
  group_by(site) %>% 
  summarise(mean = mean(totalO_thk), 
            var = var(totalO_thk), 
            sd = sd(totalO_thk),
            n = length(totalO_thk))

site.stats.df
