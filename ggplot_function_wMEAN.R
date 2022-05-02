#Title: ggplot_function_dailies.R

#Description: Creation of "weeklyFUN.mean" function
#Date created: 05/02/2022
#By: Sara DeLaurentis 
#Original code copied from ggplot_function_dDIFF.R

#Notes:
#Code examples are from Lukemiller.org, 
#r-graph-gallery.com/line-chart-several-groups-ggplot2.html
#changes from ggplotFUN: function name, "y =" param, scale_y_continuous, coord_cartesian, coordinates for legend placement, coordinates for annotation (note1, note2)
# png length (2500 from 2000)

library(tidyverse)
library(viridis)
library(hrbrthemes)
library(stringr)
#install.packages("viridis")
#install.packages("hrbrthemes")
#hrbrthemes::import_roboto_condensed()
#install.packages("wesanderson")
library(wesanderson)

names(wes_palettes)
getAnywhere(wes_palettes)

#Re-create "merged_dailies.csv" as needed:

csv_files <- list.files(path = "./dailies", full.names = TRUE)
head(csv_files)

#05/19/2021: you need to find a different way to do this now because the annotations have been manually added to merged_dailies.csv
#merged_dailies <- map_dfr(csv_files, read_csv) #glue all of the csv files together. map_dfr() specifically returns a data frame created by row-binding.
?map_dfr

merged_dailies <- read.csv("./merged_dailies.csv")

head(merged_dailies) #check
tail(merged_dailies) #check

#write.csv(merged_dailies, file = "merged_dailies.csv") #write the csv file


#Series selection component:

dailydata <- read.csv("merged_dailies.csv")
dailydata$longdate <- as.Date(dailydata$longdate) #change longdate column to correct format

head(dailydata)
class(dailydata$longdate)

#selection examples and setup:
C2A_R0_air <- filter(dailydata, labels == "C2A_R0_air_i106_2020")
C2A_R1_lsurf <- filter(dailydata, labels == "C2A_R1_lsurf_i34")

#use this code to create your "set"
set <- dailydata %>% 
  filter(labels %in% c("C2A_R0_air_i106_2020", "C2A_R1_lsurf_i34_2020"))


print(set)


#05/03/2021: Code for ggplotFUN is functional for plotting with colors defined by viridis and assigned to each group automatically.

#ggplotFUN:
#set = selected data
#title = "graph title"
#pngname = "_____.png"



##### FUNCTION START #####

ggplotFUN.dDIFF <- function(set, title, pngname){
  
  #SNOW DATA
  NOAA <- read.csv("./NOAA_data_RAW.csv")
  NOAA$DATE <- as.Date(NOAA$DATE)
  UMN1 <- NOAA %>% 
    filter(STATION == "USC00214106")
  
  #COLOR LIST  
  group.colors <- c(air = "gray74", lsurf = "#e9967a", m01surf = "#e9967a", m0surf = "#668d4e", m02surf = "#668d4e", m10 = "#3B9AB2", m30 = "#663a82", m50 = "#b491c8")  
  #group.colors <- c(air = "gray74", lsurf = "#3B9AB2", m01surf = "#3B9AB2", m0surf = "#99d4e9", m02surf = "#99d4e9", m10 = "#EBCC2A", m30 = "#ff8b3d", m50 = "#F21A00")
  #group.colors <- c(air = "gray74", lsurf = "#8d4e85", m01surf = "#8d4e85", m0surf = "#8d624e", m02surf = "#8d624e", m10 = "#6c8d4e", m30 = "#4e8d7b", m50 = "#4e538d")
  print(group.colors)
  
  #PLOT START AND STOP DATES
  start <- as.Date("2019-11-01") #set up start and end dates
  end <- as.Date("2020-08-01")
  
  #PNG FILE DIMENSIONS
  png(file = pngname, width = 2500, height = 1000, units = 'px')
  
  #GGPLOT INPUTS AND PARAMETERS
  mygraph = ggplot() +
    geom_line(data = set, aes(x = as.Date(longdate), y = difference, group = position, colour = position), size = 1.1) 
  mygraph = mygraph + geom_line(data = UMN1, aes(x = as.Date(DATE), y = SNWD), colour = "light blue", alpha = 0.5, lwd = (2.2))
  
  
  
  #?geom_line
  #  geom_line(data = UMN1, aes(x = as.Date(DATE), y = SNWD), colour = "light blue", alpha = 0.25, lwd = 2)
  
  mygraph = mygraph + scale_colour_manual(values = group.colors) +
    #theme_ipsum()+
    ggtitle(title) +
    ylab("Degrees C") +
    xlab("Date")
  
  #axes adjustments and background adjustments
  
  
  mygraph = mygraph + scale_x_date(date_breaks = "2 weeks", date_labels = "%m/%d/%y") + 
    scale_y_continuous(breaks = seq(0,35,5), minor_breaks = NULL)
  
  mygraph = mygraph + 
    theme(axis.text.x = element_text(angle = 45, vjust = 0.9,
                                     hjust = 0.9, size = rel(2))) +
    theme(axis.text.y = element_text(size = rel(2)))+
    coord_cartesian(xlim = c(start, end), ylim = c(-2, 35)) +
    theme(legend.position = c(0.9, 0.7)) 
  #theme(legend.justification = c("bottom", "right")) 
  
  
  
  #ANNOTATION PRE-WORK
  
  #create note.df
  note.df <- merged_dailies %>% #note.df is created using merged_dailies, grouped by treatment and rep, then multiple columns are created by pasting the unique values of each
    #column
    group_by(treatment, rep) %>% 
    summarise(O_thk = paste(unique(O_thk)), description = paste(unique(description)), note1 = paste(unique(note1)), note2 = paste(unique(note2)))
  head(note.df)
  
  #selecting annotation. deparsing the set name, separating and using the name elements to select the correct row in the annotation dataframe (note.df).
  
  set.name <- deparse(substitute(set)) %>% #this selects just the name of the set, ex: C2A_R1
    str_split_fixed("_", n = 2) #this splits it into two parts, separated by "_"
  print(set.name)
  annotation <- filter(note.df, treatment == set.name[,1] & rep == set.name[,2]) #select the rows in the note.df dataframe based on the elements in set.name
  print(annotation)
  # for later? select(O_thk, description, note1, note2)
  
  
  
  #ANNOTATIONS
  #includes subtitle
  
  mygraph = mygraph + annotate(geom = "text", x=as.Date("2020-04-27"), y=-2, #this creates an annotation positioned at specific x,y coordinates on the plotting area.
                               label = glue("{annotation$note1}
            {annotation$note2}"), 
                               color="black", size = rel(6), hjust = 0)
  #annotate(geom = "text", x = as.Date("2020-04-27"), y=-17, label)
  #mygraph = mygraph + geom_text(data = UMN1, color = "black")
  
  
  mygraph = mygraph + theme(plot.title = element_text(hjust = 0.5, size = rel(2.5), face = "bold")) +
    theme(axis.title = element_text(size = rel(2.75))) +
    labs(subtitle = glue("{annotation$description}
                       Avg. O thickness: {annotation$O_thk} cm"), legend.title = "Sensor Position") +  
    theme(plot.subtitle = element_text(size = rel(1.75), face = "italic", hjust = 0.5)) +
    theme(legend.text=element_text(size= rel(1.75)))+
    theme(legend.title=element_text(size = rel(1.9), face = "bold"))
  mygraph = mygraph + theme(plot.caption = element_text(size = rel(2))) + 
    theme(plot.margin = unit(c(20,45,5,20), "pt"))
  #Don't know what I'm doing with these numbers, but it works for now.
  
  print(mygraph)}


##### FUNCTION END #####


#Testing the function:
ggplotFUN.dDIFF(D5B_R1, "function test", "338.png")
dev.off()


#Airtemp sets:
set <- dailydata %>% 
  filter(position %in% c("air"))

head(set)
set[1000,]

ggplotFUN(set, "Avg Daily Air Temps", "Air_daily_average.png")
dev.off()


set <- dailydata %>% 
  filter(treatment %in% c("C2A")) %>% 
  filter(rep %in% c("R2"))
head(set)

ggplotFUN(set, "C2A R2 Avg Daily Temps", "C2A_R2_daily_average.png")
dev.off()


#What's going on with the color list?
group.colors <- c(air = "black", lsurf = "darksalmon", m01surf = "darksalmon", m0surf = "green3", m02surf = "green3", m10 = "royalblue2", m30 = "orchid", m50 = "pink")  
print(group.colors)
class(group.colors)





