#Degree_day_linear_models_V1.R
#Degree Day Statistical Analysis with linear models V1
#Created by: Sara DeLaurentis
#Date: 2023-02-22
#Purpose: Final coding for Degree Day data analysis outputs

## LIBRARIES

library(tidyverse)
library(lubridate)
library(stringr)
library(dplyr)
library(glue)
library(lme4)
library(lmerTest)
library(emmeans)

## Read in data

#Read in ALL_A.csv from the directory above (extra dot .). ALL_A.csv is the version with outliers and compromised data removed.

data <- read_csv("../ALL_A.csv")
data$`...1` = NULL #remove strange column that was created upon read-in.
head(data)

## Summarize mean temps for each date

#Create a new df:
# mutate date.time to POSIXct object, then summarize temperature measurements into one meantemp measurement for each date.
# rename the grouping column to "date", from the automatically created 'date(date.time)'
dailymeans.df <- mutate(data, date.time = as.POSIXct(date.time, format = "%Y-%m-%d %H:%M")) %>% 
  group_by(name, date(date.time)) %>% 
  summarise(meantemp = mean(value))
colnames(dailymeans.df)[2]<- "date" #rename grouping column

head(dailymeans.df)

## Problem-solving
C2A_R2_m10.means <- dailymeans.df %>% filter(grepl("C2A_R2_m10",name) == TRUE)
view(C2A_R2_m10.means)

C2A_R3_m10.means <- dailymeans.df %>% filter(grepl("C2A_R3_m10", name) == TRUE)
view(C2A_R3_m10.means)

## Remove all values less than or equal to 0
dailymeans.df <- dailymeans.df %>% filter(meantemp > 0)


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
  

## TIME BREAKS ##

time.start <- as.Date("2020-04-01")
#time.break.one <-
#time.break.two <- 
time.end <- as.Date("2020-08-27")
print(time.start)
print(time.end)


## subsetting the dataframe using the time breaks.
#Selects data that is greater than time.start AND less than time.end
DD.df.cut <- dailymeans.df[(dailymeans.df$date >= time.start) & (dailymeans.df$date <= time.end),]

view(DD.df.cut)



## Add in metadata columns for final dataframe

#Split the name column into 5 separate columns
meta.df <- str_split_fixed(DD.df.cut$name, "_", n = 5)
head(meta.df)

# Name and attach the columns from meta.df
DD.df.cut <- DD.df.cut %>% add_column(site = meta.df[,1], rep = meta.df[,2], position = meta.df[,3], buttonID = meta.df[,4], season = meta.df[,5],
                         .before = "date")
head(DD.df.cut)

## Cutting out incomplete datasets. Removes the group if the length is less than 50 days.

DD.df.cut <- DD.df.cut %>% group_by(name) %>% filter((length(name) > 50) == TRUE)
view(DD.df.cut)

     
## Summarise, or reframe. Final step for response variable df ##

# Group by site, rep, and position, and sum the mean temperatures in the cut dataframe.
# This gives you the response variable: degree days tabulated over the desired timeframe.
DDsums.df.pre <- DD.df.cut %>% 
    group_by(site, rep, position) %>% 
    reframe(site = site, position = position,
              degree.days = sum(meantemp)) %>%  #create degree.days column by outputting the sum of meantemp for each position. (Not sensor, but position)
    distinct(site, rep, position, .keep_all = TRUE) #%>% 
    #ungroup()
  view(DDsums.df.pre)


## Creating full (expected) dataframe, to merge with the current dataframe.  

#  site <- c(rep("C2A", 14), rep("C2B", 14), rep("C5A", 14), rep("C5B", 14),
#          rep("D2A", 14), rep("D2B", 14), rep("D5A", 14), rep("D5B", 14))
#print(site)

#rep <- c("R0", rep("R1",5), rep("R2", 4), rep("R3",4))  %>% 
#  rep(8)
#print(rep)

sensor.list <- as_tibble(read_csv("../sensor_list.csv"))
view(sensor.list)


#Add column to DDsums.df and sensor.list to give a unique column to merge with.
DDsums.df.A <- DDsums.df.pre %>% 
  mutate(srp.name = paste(site, rep, position, sep = "_"))
head(DDsums.df.A)

sensor.list.A <- sensor.list %>% 
  mutate(srp.name = paste(site, rep, position, sep = "_"))
head(sensor.list.A)

#DDsums.df.new <- merge(x = sensor.list.A, y = DDsums.df.A, by = "srp.name", all.x = TRUE)
DDsums.df <- merge(x = sensor.list.A, y = DDsums.df.A, all.x = TRUE)


view(DDsums.df)


## Adding treatment columns:
  
#Worm invasion level (worm_lvl, LOW or HIGH)

DDsums.df.a <- DDsums.df %>% mutate(worm_lvl = if_else(grepl("2", DDsums.df$site), "LOW", "HIGH"))
head(DDsums.df.a)
#view(DDsums.df.a) 

#Vegetation (Veg, Coniferous or Deciduous)

DDsums.df.b <- DDsums.df.a %>% mutate(Veg = if_else(grepl("C", DDsums.df$site), "Coniferous", "Deciduous"))
head(DDsums.df.b)
view(DDsums.df.b)  

DDSUMS.df <- DDsums.df.b # rename df
head(DDSUMS.df)

# ------------------------------------------ #


### LINEAR MODEL TIME ##


## DD vegxworm lsurf mixed (DDSUMS.lsurf)
#This model has degree.days as a function of vegetation (Veg) and worm invasion intensity (worm_lvl), as well as the interaction as slopes, with site as a random effect.
#PARAMETERS: Timeframe: 2020-04-01 - 2020-09-30, position: lsurf

#generate lsurf df for analysis
DDSUMS.lsurf <- DDSUMS.df %>% filter(position == "lsurf" | position == "m01surf")
view(DDSUMS.lsurf)

mod1.lsurf <- lmer(data = DDSUMS.lsurf, formula = degree.days ~ Veg + worm_lvl + Veg*worm_lvl + (1|site))

#View object
mod1.lsurf

#summary of object
summary(mod1.lsurf)

#Error:
#" boundary (singular) fit: see help('isSingular') "
#2023-03-16: After filling in data gaps with NAs, no more singular fit error.


##What happens if we remove the random effect for site?

## lm DD vegxworm lsurf (mod2.lsurf)
# linear model with no random effect:
# PARAMETERS: timeframe = 2020-04-01 - 2020-09-30, position: lsurf

mod2.lsurf <- lm(data = DDSUMS.lsurf, formula = degree.days ~ Veg + worm_lvl + Veg*worm_lvl)
summary(mod2.lsurf)
#non-significant slopes for lsurf, which makes sense


##m0surf models

#creation of m0surf object
DDSUMS.m0surf <- DDSUMS.df %>% filter(position == "m0surf" | position == "m02surf")
view(DDSUMS.m0surf)

##m0surf lmer
mod1.m0surf <- lmer(data = DDSUMS.m0surf, formula = degree.days ~ Veg + worm_lvl + Veg*worm_lvl + (1|site))
summary(mod1.m0surf)

##m0surf lm
mod2.m0surf <- lm(data = DDSUMS.m0surf, formula = degree.days ~ Veg + worm_lvl + Veg*worm_lvl)
summary(mod2.m0surf)

## mixed-effects DD vegxworm m10
#lmer for m10. Note: for the regular linear models, this is not taking into account the natural variation by site, that may be due to soil type and solar radiation, slope, etc.
#PARAMETERS: timeframe = 2020-04-01 - 2020-09-30, position: m10


##m10 models

#create m10 object:
DDSUMS.m10 <- DDSUMS.df %>% filter(position == "m10")
print(DDSUMS.m10)
#view(DDSUMS.m10)

##m10 lmer
mod1.m10 <- lmer(data = DDSUMS.m10, formula = degree.days ~ Veg + worm_lvl + Veg*worm_lvl + (1|site))
summary(mod1.m10)

##m10 lm
mod2.m10 <- lm(data = DDSUMS.m10, formula = degree.days ~ Veg + worm_lvl + Veg*worm_lvl)
summary(mod2.m10)
#significant effect for worm_lvl, no significant effect for vegetation type, or interaction.


##m30 models

#create m30 object:
DDSUMS.m30 <- DDSUMS.df %>% filter(position == "m30")
print(DDSUMS.m30)
#view(DDSUMS.m30)

##m30 lmer
mod1.m30 <- lmer(data = DDSUMS.m30, formula = degree.days ~ Veg + worm_lvl + Veg*worm_lvl + (1|site))
summary(mod1.m30)

##m30 lm
mod2.m30 <- lm(data = DDSUMS.m30, formula = degree.days ~ Veg + worm_lvl + Veg*worm_lvl)
summary(mod2.m30)


##air sensor DD data

#create air object:
DDSUMS.air <- DDSUMS.df %>% filter(position == "air")
print(DDSUMS.air)

#summary statistics for air DD
#create df for air data at each site:
air.DD.df <- data_frame(site = DDSUMS.air$site, degree.days = DDSUMS.air$degree.days)
print(air.DD.df)
summary(DDSUMS.air$degree.days)

## Subtract replicate measurements from Air temp at sites with AT.

