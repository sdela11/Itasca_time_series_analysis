---
  title: "Itasca_dailes_temps"
author: "Sara"
date: "01/08/2021"
note:  "see also Itasca mean temps"
output: html_document

---
  
  
library(lubridate)
library(stringr)
library(tidyverse)
library(cowplot)
library(stringr)
library(dplyr)
library(glue) #from dplyr

install.packages("cowplot")

## If necessary, see "Itasca_plot_maker_20210106.Rmd" for instructions on setting up a file list dataframe, as well as how to select files to read by treatment, rep, etc.

name.data <- file.names %>%  
  str_replace(".csv", "")%>%
  str_split_fixed("_", n=5)


full.file.names <- list.files("./CLEAN_DATA", full.names = TRUE)
head(full.file.names)

name.data <- cbind(full.file.names, name.data)
str(full.file.names)
head(name.data)
tail(name.data)


file.names <- list.files("./CLEAN_DATA", full.names = FALSE)
head(file.names)
tail(file.names)

head(name.data)
tail(name.data)

#class(name.data)

name.df <- as.data.frame(name.data, stringsAsFactors = FALSE)
?as.data.frame
head(name.df)
tail(name.df)

colnames(name.df) <- c("file.names", "treatment", "rep", "position", "buttonID", "year")
head(name.df)
tail(name.df)
str(name.df)



########################## HERE IS THE GOOD CODE ###########################
##Trying read in for one file (another example):

#In the process of creating a function that will be able to process all the code.
#AS OF 04/21/2021: The code works for creating .csv files with extra metadata: Labels, Site, 
#Rep, Position, buttonID, year.
#WARNING: If you separate out another year column for date data, keep in mind that you already
#have one (as noted above). 

#.csv files with metadata were printed 04/21/2021, 6:41pm
#05/14/2021: Created "difference" column in addition to amplitude. "difference" is max-min.
#05/14/2021: Added metadata column


C2A_R1_m10 <- read.csv("./CLEAN_DATA/C2A_R1_m10_i36_2020.csv")

#checking if it was read in properly
head(C2A_R1_m10)

#Function walkthrough so far:
#mutate the date.time column to proper date format
#separate date.time into longdate and time
#separate longdate into year, month, day
#group by year, month, day
#summarise (creates a new column called "meantemp", equal to the mean value of the group)

C2A_R1_m10_dailymean <- mutate(C2A_R1_m10,'date.time' = mdy_hm(date.time)) %>% 
  separate('date.time',
           into = c('longdate', 'time'),
           sep = ' ') %>% 
  separate('longdate',
                into = c('year', 'month', 'day'),
                sep = '-',
                remove = FALSE) %>% 
  group_by(year, month, day) %>% 
  summarise(meantemp = mean(value)) 

view(C2A_R1_m10_dailymean)
class(C2A_R1_m10_dailymean)

#try to change the wierd tibble thing to a dataframe
as_data_frame(C2A_R1_m10_dailymean)
class(C2A_R1_m10_dailymean)


#setting up a function to handle create ALL csv "daily" files: "create_csv_daily"

create_csv_daily <- function(full_paths, labels){
  temps <- read_csv(full_paths) %>% 
    mutate(full_paths,'date.time' = mdy_hm(date.time)) %>% 
    separate('date.time',
             into = c('longdate', 'time'),
             sep = ' ') %>% 
    separate('longdate',
             into = c('year', 'month', 'day'),
             sep = '-',
             remove = FALSE)  
   # print(temps)
      avg <- temps %>% group_by(longdate) %>% 
        summarise(meantemp = mean(value))
   # print(avg)
      max <- temps %>% group_by(longdate) %>% 
        summarise(max = max(value))
   # print(max)
      min <- temps %>% group_by(longdate) %>% 
        summarise(min = min(value))
   # print(min)
      
      
      
      #create intermediate: bind together avg + just the values for max and min. avg carries the longdate column.
      temp_intA <- cbind(avg, max = max$max, min = min$min)
    head(temp_intA)
    
      #add other necessary calculated variable columns:
    
    
    temp_intB <- temp_intA %>% 
      mutate(amplitude = ((max - min)/2), difference = (max - min))                                                                                                                              
    
    
    head(temp_intB)
    
    
    #Creating the metadata dataframe:
    
    #separate the label name in order to add metadata
    fileNM <- labels %>%  
      str_split_fixed("_", n=5)
    
    treatment <- fileNM[,1]
    rep <- fileNM[,2]
    position <- fileNM[,3]
    buttonID <- fileNM[,4]
    year <- fileNM[,5]
    
    #new dataframe with metadata, setting length equal to the number of rows in the temp_intA, longdate column:
    metadata_df <- tibble(labels, treatment, rep, position, buttonID, year, .rows = length(temp_intA$longdate))
    #print(metadata_df)
    
    #bind the metadata_df and temperature data for the final csv output.
    temp_df <- cbind(metadata_df, temp_intB)
    
      
  #write the .csv. CHANGE TO dailytest folder IF YOU'RE TESTING
      
  write_csv(temp_df, glue("./dailytest/Daily_{labels}.csv"))
}


#Setting up inputs for the function. final input is df_daily_input
file.names <- list.files("./CLEAN_DATA", full.names = FALSE)
name.data.dailies <- file.names %>%  
  str_replace(".csv", "") %>% 
  as_data_frame()
head(name.data.dailies)
tail(name.data.dailies)

full.file.names <- list.files("./CLEAN_DATA", full.names = TRUE) %>% 
  as_data_frame()

full.file.names <- list.files("./CLEAN_DATA", full.names = TRUE)
head(full.file.names)

df_daily_input <- cbind(full.file.names, name.data.dailies)
colnames(df_daily_input) <- c("full_paths", "labels")
head(df_daily_input)
tail(df_daily_input)

#03/31/2021: created a working function that summarizes daily data (mean, max, min, amplitude)
#If you mess things up, display: create_csv_daily_BU

##04/14/2021: A whole set of .csv files has been created and stored in "dailies" folder. Delete or move to a new location before creating another set. ##

##The whole kitten-kaboodle. Do not press "go" unless you're ready:
pwalk(df_daily_input, create_csv_daily)

##To TEST just ONE .csv file:
create_csv_daily("./CLEAN_DATA/C2A_R1_m10_i36_2020.csv", "C2A_R1_m10_i36_2020")




###########  Code for plotting, 1st attempts:  ########### needs lots of editing and cleaning
# For the 2nd attempt using ggplot, see "Itasca_plotting_20210427.R"


#HERE'S A ROUGH TEMPLATE FOR PLOTTING MEANS

plot(C2A_R1_m10_fullmean$longdate, C2A_R1_m10_fullmean$meantemp, main="C2A R1 m10", xlab = "date", ylab = "Degrees (C)",
     ylim = c(-5,22))
#points(C2A_R1_m10_fullmean$longdate ~ C2A_R1_lsurf_fullmean$meantemp, pch = 20, col = "blue")
grid(col = "lightgray", lty = "dotted", )



# EXAMPLE SELECTION OF C2A R1 

name.df[name.df$treatment == "C2A" & name.df$rep == "R1",]


#sets the window for graphing multiple plots together. This one sets up two rows, one column.
par(mfrow=c(2,1))

plot(C2A_R1_m10_monthlymean$month, C2A_R1_m10_monthlymean$meantemp, main="C2A R1 m10 & surface (blue)", xlab = "month", ylab = "Degrees (C)",
     ylim = c(-5,22))
points(C2A_R1_lsurf_monthlymean$meantemp ~ C2A_R1_lsurf_monthlymean$month, pch = 20, col = "blue")
grid(col = "lightgray", lty = "dotted", )
#legend(C2A_R1_m10_monthlymean$month)
#?legend

plot(C5B_R1_m10_monthlymean$month, C5B_R1_m10_monthlymean$meantemp, main="C5B R1 m10 & surface (blue)", xlab = "month", ylab = "Degrees (C)",
     ylim = c(-5,22))
points(C5B_R1_lsurf_monthlymean$month, C5B_R1_lsurf_monthlymean$meantemp, pch = 20, col = "blue")
grid(col = "lightgray", lty = "dotted", )



#Seriously trying out a hilarious plotting function (to plot everything on one .png).
#04/14/2021: this function to plot all is not working.

#creating the inputs
file.names2 <- list.files("./dailies", full.names = FALSE)
names <- file.names2 %>%  
  str_replace(".csv", "") %>% 
  as_data_frame()
head(names)

full.file.names2 <- list.files("./dailies", full.names = TRUE) %>% 
  as_data_frame()
  head(full.file.names2)

df_daily_input2 <- cbind(full.file.names2, names)
colnames(df_daily_input2) <- c("full_paths", "labels")
head(df_daily_input2)

#Creating the plot_all function

plot_all <- function(full_paths){
  data <- read_csv(full_paths)
  plot(as.Date(data$longdate, format = "%m/%d/%Y"), data$meantemp, main = "PLOT ALL :)", xlab = "date", ylab = "Daily Average (C)", xlim = as.Date(c("2019-10-05", "2019-12-31")),
       ylim = c(-5, 22))
  head(data$longdate)
}

?as.Date
?png()

df_daily_input2a <- select(df_daily_input2, full_paths)
head(df_daily_input2a)

png("All.png", width = 4000, height = 1000) 
#my_plot = plot_all("./dailies/Daily_C2A_R2_m10_i32_2020.csv")
my_plot = pwalk(df_daily_input2a, plot_all)
print(my_plot)
dev.off()
#this did not work. All I see is a blank png.
?pwalk
#For C2A_R2_m10_monthlymean 

name.df[name.df$treatment == "C2A",]
C2A_R2_m10 <- read.csv("./CLEAN_DATA/C2A_R2_m10_i32_2020.csv")

#checking if it was read in properly
head(C2A_R2_m10)

#transforming date.time column into separate coluns for time and date. Then check if it worked.

C2A_R2_m10_full <- C2A_R2_m10_longdate %>%
  tidyr::separate('date.time',
                  into = c('month', 'day', 'year'),
                  sep = '/',
                  remove = FALSE)

#New name:
C5B_R1_lsurf_monthlymean <- data_monthlymean


?do.call

###   Creation of large csv. binding all rows of dailies files together.  ###
## 04/21/2021 : Successfully printed merged_dailies, with one sensor missing due to change in date.time format. I believe that file has seconds in the format.


csv_files <- list.files(path = "./dailies", full.names = TRUE)
head(csv_files)

merged_dailies <- map_dfr(csv_files, read_csv)

head(merged_dailies)

write.csv(merged_dailies, file = "merged_dailies.csv")
?write.csv

#it can read one file successfully, so there's something going on within the function.



