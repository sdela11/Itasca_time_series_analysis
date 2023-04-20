library(tidyverse)
library(lubridate)
library(stringr)
library(broom)
library(dplyr)
library(glue)
library(lme4)
library(lmerTest)

Odata <- read_csv("../2YR_replicate_PIT_depths_OA.csv")
view(Odata)


data <- read_csv("../ALL_A.csv")
data$`...1` = NULL #remove strange column that was created upon read-in.
head(data)

data <- data[!(data$site == "D5B"),] #remove all D5B sensors.
D5B.chk <- data[data$site == "D5B",]
D5B.chk


#```{r summarize.daily, echo = TRUE}

dailymeans.df <- mutate(data, date.time = as.POSIXct(date.time, format = "%Y-%m-%d %H:%M")) %>% 
  group_by(name, date(date.time)) %>% 
  summarise(meantemp = mean(value))
colnames(dailymeans.df)[2]<- "date" #rename grouping column

head(dailymeans.df)

#```

#```{r summarize.daily, echo = TRUE}

dailymeans.df <- mutate(data, date.time = as.POSIXct(date.time, format = "%Y-%m-%d %H:%M")) %>% 
  group_by(name, date(date.time)) %>% 
  summarise(meantemp = mean(value))
colnames(dailymeans.df)[2]<- "date" #rename grouping column

#head(dailymeans.df)

#```

#### Remove all values less than or equal to 0

#```{r filter.zero, echo = TRUE}

dailymeans.df <- dailymeans.df %>% filter(meantemp > 0)

#```

### TIME BREAKS

#```{r TIME BREAKS, echo = TRUE}

time.start <- as.Date("2020-04-01")
#time.break.one <-
#time.break.two <- 
time.end <- as.Date("2020-08-27")
print(time.start)
print(time.end)


## subsetting the dataframe using the time breaks.
#Selects data that is greater than time.start AND less than time.end
DD.df.cut <- dailymeans.df[(dailymeans.df$date >= time.start) & (dailymeans.df$date <= time.end),]

#view(DD.df.cut)

#```


#### Add in metadata columns for final dataframe

#```{r attach meta, echo = TRUE}

#Split the name column into 5 separate columns
meta.df <- str_split_fixed(DD.df.cut$name, "_", n = 5)
#head(meta.df)

# Name and attach the columns from meta.df
DD.df.cut <- DD.df.cut %>% add_column(site = meta.df[,1], rep = meta.df[,2], position = meta.df[,3], buttonID = meta.df[,4], season = meta.df[,5],
                                      .before = "date")
#head(DD.df.cut)

#```


#### Cutting out incomplete datasets. Removes the group if the length is less than 50 days.

#```{r rm.incomplete, echo = TRUE}

DD.df.cut <- DD.df.cut %>% group_by(name) %>% filter((length(name) > 50) == TRUE)
#view(DD.df.cut)

#```

#### Summarise, or reframe. Final step for response variable df 

#Group by site, rep, and position, and sum the mean temperatures in the cut dataframe.
#This gives you the response variable: degree days tabulated over the desired timeframe.

#```{r merge complete table, echo = TRUE}

DDsums.df.pre <- DD.df.cut %>% 
  group_by(site, rep, position, season) %>% 
  reframe(site = site, position = position,
          degree.days = sum(meantemp)) %>%  #create degree.days column by outputting the sum of meantemp for each position. (Not sensor, but position)
  distinct(site, rep, position, season, .keep_all = TRUE) #%>% 
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
  mutate(srps.name = paste(site, rep, position, season, sep = "_"))
#head(DDsums.df.A)

sensor.list.A <- sensor.list %>% 
  mutate(srp.name = paste(site, rep, position, sep = "_"))
#head(sensor.list.A)

#DDsums.df.new <- merge(x = sensor.list.A, y = DDsums.df.A, by = "srp.name", all.x = TRUE)
DDsums.df <- merge(x = sensor.list.A, y = DDsums.df.A, all.x = TRUE)


view(DDsums.df)

#```



#### Adding treatment columns:

#Worm invasion level (worm_lvl, LOW or HIGH)

#```{r trt cols, echo = TRUE}

DDsums.df.a <- DDsums.df %>% mutate(worm_lvl = if_else(grepl("2", DDsums.df$site), "LOW", "HIGH"))
#head(DDsums.df.a)
#view(DDsums.df.a) 

#Vegetation (Veg, Coniferous or Deciduous)

DDsums.df.b <- DDsums.df.a %>% mutate(Veg = if_else(grepl("C", DDsums.df$site), "Coniferous", "Deciduous"))
#head(DDsums.df.b)
#view(DDsums.df.b)  

DDSUMS.df <- DDsums.df.b # rename df
head(DDSUMS.df)

#```

## Bring in and attach OA horizon data ##

#See O_horizon_prelim.R to view summary statistics


#```{r OA data}

#OA.dat <- read.csv("../OA_thickness_2019_21_clean_20230325.csv")
Odata <- read_csv("../2YR_replicate_PIT_depths_OA.csv")


# set classes for columns
OA.dat$inv_lvl <- as.character(OA.dat$inv_lvl)
OA.dat$Oi_thk <- as.numeric(OA.dat$Oi_thk)
OA.dat$Oe_thk <- as.numeric(OA.dat$Oe_thk)
OA.dat$Oa_thk <- as.numeric(OA.dat$Oa_thk)
OA.dat$O.A_thk <- as.numeric(OA.dat$O.A_thk)
OA.dat$bulk_density <- as.numeric(OA.dat$bulk_density)

# extract mean O thickness, attach to DD data

OA.means.df <- OA.dat %>% 
  filter(!is.na(totalO_thk)) %>% 
  group_by(site) %>% 
  summarise(Omean = round(mean(totalO_thk), digits = 2))
#OA.means.df
# attach
DDSUMS.ALL <- DDSUMS.df %>% 
  left_join(OA.means.df)
DDSUMS.ALL  


#```
