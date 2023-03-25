# Title: O-horizon processing
# By: Sara DeLaurentis
# Date created: 2023-03-25
# Purpose: Check O and A horizon data sheet: 

library(tidyverse)
library(dplyr)
library(stringr)
library(glue)
library(ggplot2)



OA.dat <- read.csv("../OA_thickness_2019_21_clean_20230325.csv")
head(OA.dat)
str(OA.dat)

#set classes for columns
OA.dat$inv_lvl <- as.character(OA.dat$inv_lvl)
OA.dat$Oi_thk <- as.numeric(OA.dat$Oi_thk)
OA.dat$Oe_thk <- as.numeric(OA.dat$Oe_thk)
OA.dat$Oa_thk <- as.numeric(OA.dat$Oa_thk)
OA.dat$O.A_thk <- as.numeric(OA.dat$O.A_thk)
OA.dat$bulk_density <- as.numeric(OA.dat$bulk_density)
