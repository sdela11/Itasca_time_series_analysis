#Title: ggplot_function_dailies.R

#Description: Creation of "weeklyFUN.mean" function
#Date created: 05/02/2022
#By: Sara DeLaurentis 
#Original code copied from ggplot_function_dDIFF.R
#05/22/2024: I'm not sure if this code works to plot weekly means. It might...


#Notes:
#Code examples are from Lukemiller.org, 
#r-graph-gallery.com/line-chart-several-groups-ggplot2.html
#changes from ggplotFUN: function name, "y =" param, scale_y_continuous, coord_cartesian, coordinates for legend placement, coordinates for annotations (note1, note2)
# png length (2500 from 2000)

library(tidyverse)
library(viridis)
#library(hrbrthemes)
library(stringr)
library(glue)
library(lubridate)
library(dplyr)
#install.packages("viridis")
#install.packages("hrbrthemes")
#hrbrthemes::import_roboto_condensed()
#install.packages("wesanderson")
library(wesanderson)

names(wes_palettes)
getAnywhere(wes_palettes)


#TO-DO:  

#Summarize temperatures by week (see weeklies rebuild)
#Summarize temperatures by rolling weeklies (see weeklies rebuild)




#Series selection component:

data <- read.csv("ALL_A.csv")
data$date.time <- data$date.time %>% as.POSIXlt(tz = "") #set date/time class to POSIXlt for greater ease in parsing date elements.
head(data$date.time)
head(data)

# -- subset by date, add week-counting columns, calculate weekly means --

START <- as.POSIXlt("2020-10-01 00:00:00", tz = "")
#BREAK <- as.POSIXlt("2020-08-25", tz = "")
#RESUME <- as.POSIXlt("2020-09-30", tz = "")
END <- as.POSIXlt("2021-10-01 00:00:00", tz = "")

data <- data %>% 
  subset.data.frame(date.time >= START & date.time <= END) %>% #subset the whole df                         
  filter(!is.na(value))  # Apply filter & !is.na

# WEEKLY.MEANS.DF STARTING AT "data"

data.2 <- data %>%
  group_by(site, rep, position) %>% 
  mutate(length = length(date.time)) %>% 
  filter(length > 500)

#```


##Code for means at set weeks. See next chunk for rolling means.

#```{r }

week.START <- as.POSIXlt("2020-10-01 00:00:00", tz = "") #set the starting date of the week calculations.
wday.no <- as.POSIXlt(week.START)$wday #store weekday integer
data.2$week.begin <- floor_date(as.POSIXlt(data.2$date.time), unit = "weeks", week_start = wday.no) #create week.begin column
data.2$week.no <- (1 + difftime(data.2$week.begin, week.START, units = "weeks")) %>% 
  round()
data[1:25,]

#Set weeks
weekly.means.df <- data.2 %>%
  group_by(site, rep, position, week.no) %>% 
  mutate(meantemp = round(mean(value), 2))

weekly.means.df <- distinct(weekly.means.df, site, rep, position, week.begin, .keep_all = TRUE)
length(weekly.means.df$week.begin)

print(weekly.means.df)

#selection examples and setup:

#C2A_R0_air <- filter(dailydata, labels == "C2A_R0_air_i106_2020")
#C2A_R1_lsurf <- filter(dailydata, labels == "C2A_R1_lsurf_i34")

#use this code to create your "set"
set <- dailydata %>% 
  filter(labels %in% c("C2A_R0_air_i106_2020", "C2A_R1_lsurf_i34_2020"))


print(set)


#05/03/2021: Code for ggplotFUN is functional for plotting with colors defined by viridis and assigned to each group automatically.

#ggplotFUN:
#set = selected data
#title = "graph title"
#pngname = "_____.png"

#create and run the set
set.list <- as.list(c("C2A_R1", "C2A_R2", "C2A_R3", 
                      "C2B_R1", "C2B_R2", "C2B_R3",
                      "C5A_R1", "C5A_R2", "C5A_R3",
                      "C5B_R1", "C5B_R2", "C5B_R3",
                      "D2A_R1", "D2A_R2", "D2A_R3", 
                      "D2B_R1", "D2B_R2", "D2B_R3",
                      "D5A_R1", "D5A_R2", "D5A_R3"))
#print(set)

lapply(set.list, ggplotFUN.wMEAN)




##### FUNCTION START #####

ggplotFUN.wMEAN <- function(set){
  
  #selection from set name
  
  air.name <- glue("{substr(set, 1, 3)}_R0") #create the air.name to use in grepl
  
  data.df <- weekly.means.df[(grepl(set, weekly.means.df$name) | grepl(air.name, weekly.means.df$name)),] #create dataframe from selected rows.
  data_names <- data.df[,1]#use 1 as long as full file.names is the first one
  print(data_names)
  print(set)
  
  #SNOW DATA
  #NOAA <- read.csv("./NOAA_data_RAW.csv")
  #NOAA$DATE <- as.Date(NOAA$DATE)
  #UMN1 <- NOAA %>% 
  #  filter(STATION == "USC00214106")
  
  #COLOR LIST  
  positions.present <- as.character(unique(data.df$position))
  print(positions.present)
  class(positions.present)
  
  group.colors <- c(air = "gray74", lsurf = "#e9967a", m01surf = "#e9967a", m0surf = "#668d4e", m02surf = "#668d4e", m10 = "#3B9AB2", m30 = "#663a82", m50 = "#b491c8")
  group.colors <- group.colors[names(group.colors) %in% positions.present]
  print(group.colors)
 
  #class(group.colors)
 # str(group.colors)
   #group.colors <- c(air = "gray74", lsurf = "#3B9AB2", m01surf = "#3B9AB2", m0surf = "#99d4e9", m02surf = "#99d4e9", m10 = "#EBCC2A", m30 = "#ff8b3d", m50 = "#F21A00")
  #group.colors <- c(air = "gray74", lsurf = "#8d4e85", m01surf = "#8d4e85", m0surf = "#8d624e", m02surf = "#8d624e", m10 = "#6c8d4e", m30 = "#4e8d7b", m50 = "#4e538d")
  
  #PLOT START AND STOP DATES
  start <- as.Date("2020-10-01 00:00:00") #set up start and end dates
  end <- as.Date("2021-10-01 00:00:00")
  
  break.vec <- c(as.Date("2020-10-01 00:00:00"),
                 seq(from = as.Date("2020-10-01 00:00:00"), to = as.Date("2021-10-01 00:00:00"),
                     by = "2 weeks"))
  
  #PNG FILE DIMENSIONS
  png(file = glue("{set}_weekly_mean_2021.png"), width = 2500, height = 1000, units = 'px')
  
  
  #GGPLOT INPUTS AND PARAMETERS
  mygraph = ggplot() +
    geom_line(data = data.df, aes(x = as.Date(week.begin), y = meantemp, group = position, colour = position), size = 1.1) 
 # mygraph = mygraph + geom_line(data = UMN1, aes(x = as.Date(DATE), y = SNWD), colour = "light blue", alpha = 0.5, lwd = (2.2))  #for UMN snow data
  
  mygraph = mygraph + geom_hline(yintercept = 0, colour = "black", lwd = 1, alpha = 0.4)
  
  #?geom_line
  #  geom_line(data = UMN1, aes(x = as.Date(DATE), y = SNWD), colour = "light blue", alpha = 0.25, lwd = 2)
  
  mygraph = mygraph + scale_colour_manual(values = group.colors) +
    #theme_ipsum()+
    ggtitle(glue("{set} Weekly Mean Temp 2021")) +
    ylab("Degrees C") +
    xlab("Date")
  
  #axes adjustments and background adjustments
  
  
  mygraph = mygraph + scale_x_date(breaks = break.vec, date_labels = "%m/%d/%y", expand = expansion(mult = 0.01)) + 
    scale_y_continuous(breaks = seq(-30,30,5), minor_breaks = NULL, expand = c(0,0))
  
  mygraph = mygraph + 
    theme(axis.text.x = element_text(angle = 45, vjust = 0.9,
                                     hjust = 0.9, size = rel(2))) +
    theme(axis.text.y = element_text(size = rel(2)))+
    coord_cartesian(xlim = c(start, end), ylim = c(-30, 30)) +
    theme(legend.position = c(0.9, 0.3)) 
  #theme(legend.justification = c("bottom", "right")) 
  
  
  
  #ANNOTATION PRE-WORK
  
  #create note.df
  note.df <- read_csv("Replicate_annotations.csv") 
  note.df$O_2yr_avg <- as.character(note.df$O_2yr_avg)
  head(note.df)
  
  annotations <- note.df[note.df$site_rep == set,] #select the rows in the note.df dataframe based on the elements in set.name
  print(annotations)
  view(annotations)
  # for later? select(O_thk, description, note1, note2)
  
  
  
  #ANNOTATIONS
  #includes subtitle
  
  mygraph = mygraph + annotate(geom = "text", x=as.Date("2021-04-27"), y=-5, #this creates an annotation positioned at specific x,y coordinates on the plotting area.
                               label = glue("{annotations$note1}
            {annotations$note2}"), 
                               color="black", size = rel(6), hjust = 0)
  #annotate(geom = "text", x = as.Date("2021-04-27"), y=-17)
  #mygraph = mygraph + geom_text(data = UMN1, color = "black")
  
  
  mygraph = mygraph + theme(plot.title = element_text(hjust = 0.5, size = rel(2.5), face = "bold")) +
    theme(axis.title = element_text(size = rel(2.75))) +
    labs(subtitle = glue("{annotations$description}
                         Avg. O thickness (2-year): {annotations$O_2yr_avg} cm")) +
    labs(legend.title = "Sensor Position") +  
    theme(plot.subtitle = element_text(size = rel(1.75), face = "italic", hjust = 0.5)) +
    theme(legend.text=element_text(size= rel(2.5)))+
    theme(legend.title=element_text(size = rel(2.8), face = "bold"))
  mygraph = mygraph + theme(plot.caption = element_text(size = rel(2))) + 
    theme(plot.margin = unit(c(20,45,5,20), "pt"))
  #Don't know what I'm doing with these numbers, but it works for now.
  
  print(mygraph)
  
  dev.off()
  
  }


##### FUNCTION END #####

#Testing the function:
ggplotFUN.wMEAN("C2A_R1")
dev.off()








### ---  extra, if needed  ---- ###


#for automatical set selection 

air.name <- glue("{substr(set, 1, 3)}_R0") #create the air.name to use in grepl

data.df <- name.df[grepl(set, name.df$name) | grepl(air.name, name.df$name),]
data_names <- data.df[,1]#use 1 as long as full file.names is the first one
print(data_names)

plot_names <- c(data.df$position) #create legend elements
print(plot_names)

legelist <- vector() #create the list that you will append your colors into


#for Airtemp sets (old):
set <- dailydata %>% 
  filter(position %in% c("air"))

head(set)
set[1000,]

ggplotFUN(set, "Avg Daily Air Temps", "Air_daily_average.png")
dev.off()
