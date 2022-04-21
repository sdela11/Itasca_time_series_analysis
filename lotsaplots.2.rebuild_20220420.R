---
#title: "lotsaplots.2.rebuild"
#author: "Sara DeLaurentis"
#date: "02/10/2021"
#output: html_document

#NOTE: Much code lost during theft on 2/4/2021. This is the rebuild that includes .png plotting parameters.
---
  

##SETUP##  

  

#If necessary, install the following packages by uncommenting and running the following code:
#install.packages("lubridate")
#install.packages("stringr")
#install.packages("tidyverse")


#Call the packages:

library(lubridate)
library(stringr)
library(tidyverse)
#install.packages("plotrix")
library(plotrix)
library(glue)

#Check your working directory. This is where your files are stored:
getwd()
setwd("C:/Users/sbaue/Documents/R TEMPRY/Itasca_project_19-21")

#UGGGHHHHH. Set wd manually using console.
getwd()

#Amazing code for reading in file names, separating the file name elements into 
#a dataframe (adding a filename column), and selecting rows by values within a 
#given column. Useful for selecting specific files that you want to analyze together.


#Step 1: create a list of files, assigning to a variable called "file.names". 
#We don't want the full file names, so full.names = FALSE.


#Step 2: Using pipes, remove ".csv" using str_replace, then split the file name 
#data by your desired separator ("_" in this case). str_split_fixed can be used 
#to split the strings into a specified number of elements. It will pad strings 
#with less elements with extra values.  


#Step 3: Add a file.names column for easy reading of .csv files using cbind. 
#Upon checking the output class, I discovered it was a matrix, so I changed it 
#to data.frame and named it accordingly:  

#name.df <- as.data.frame(name.data)


#Step 4: column names.
#head(name.df) to check the order of the columns
#colnames(name.df) <- c("name1", "name2", "name3", ....)
#head(name.df) to check




# Step 1:

getwd()

file.names <- list.files("./ibuttons", full.names = FALSE)
#class(file.names)
head(file.names)
class(file.names)
#tail(file.names)

name.full <- file.names %>% 
  str_replace(".csv", "")  #just the full name
head(name.full)
#class(name.full)

name.split <- name.full %>%
  str_split_fixed("_", n=5)  #split the full name into 5 parts
head(name.split)

full.file.names <- list.files("./ibuttons", full.names = TRUE) #add column of full file names.
#head(full.file.names)

name.data <- data.frame(full.file.names, name.full, name.split)
 
cbind(full.file.names, name.data)
#str(full.file.names)
#head(name.data)
#tail(name.data)

#class(name.data)
name.df <- as.data.frame(name.data, stringsAsFactors = FALSE)
colnames(name.df) <- c("file.names", "name", "site", "rep", "position", "buttonID", "season")

name.df <- name.df[name.df$season == "2021",] #select only 2021
head(name.df)
str(name.df)

#class(name.df)
#str(name.df)



## --------   lotsaplots FUNCTION!!!   ---------


#lotsaplots <- function(set, png_name, graph_title, plot_names, annotate) {
lotsaplots.sites <- function(set) {

  #automated file name selection!!!
  air.name <- glue("{substr(set, 1, 3)}_R0") #create the air.name to use in grepl
  
  data.df <- name.df[grepl(set, name.df$name) | grepl(air.name, name.df$name),]
  data_names <- data.df[,1]#use 1 as long as full file.names is the first one
  print(data_names)
  
  plot_names <- c(data.df$position) #create legend elements
  print(plot_names)
  
  legelist <- vector() #create the list that you will append your colors into
  
  #Create label objects for limits and axes:
  xlims <- as.POSIXct(c("2020-08-01 00:00", "2021-10-31 00:00"))
  xlab <- seq(as.POSIXct("2020-08-01"), as.POSIXct("2021-10-31"),"2 weeks")
  

  
  ##PLOT CODE
  # Rotate x-axis tick labels so they fit better.
  
  png(filename = glue("{set}_2021_TALL.png"), width = 6000, height = 1000)# 2000 Spring or Fall, >4000 for whole year
  par(mar = c(6,10,6,6)) # expand figure margins to fit the large axis titles (bottom, left, top, right)
  plot(x = "",
       y = "",
       type = "n",
       xlim = xlims,
       ylim = c(-30,90),
       #cex.axis = 2.5, # expand axis tick labels,
       ylab = '',  # blank y axis label
       xlab = '',  # blank x axis label
       xaxt = 'n', # blank axis tick marks
       yaxt = 'n',
       xaxs = "i",
       yaxs = "i") #plz do not add 4% to the x axis window. "r" if you want it back.
  
  
  mytitle = glue("{set} 2021")
  mtext(mytitle, side = 3, line = 2, cex = 3) #print as margin text
  
  myxlabel = "Date" #create x axis title
  mtext(myxlabel, side = 1, line = 4, cex = 2.5) #print as margin text
  
  myYlabel = "Degrees C" # create y label
  mtext(myYlabel, side = 2, line = 6, cex = 2.2) #print as margin text
  
  
  axis.POSIXct(side = 1, at = xlab, labels = FALSE,
            cex.axis = 2) # print x axis tick marks, but leave labels blank
  axis(side = 2, at = seq(-30,90,5), cex = 3, labels = FALSE) #print y axis tick marks, leave labels blank
  axis(side = 4, at = seq(-30,90,5), cex = 3, labels = FALSE) #print far vert axis tick marks.
  
  ## X-axis labels:
  # We'll use the text() function to print the rotated labels, but first we need
  # to figure out where the lower limit of the y-axis is so that we can draw
  # text below it
  ylow = par()$usr[3] # call: parameters, usr parameter, element 3. 3rd value is minimum y value. usr is a vector of the form: c(x1,x2,y1,y2)
  print(ylow)
  op = par(xpd = NA) # turn off clipping
  text(x = xlab, # specify coordinate location of labels relative to x
       y = ylow, # specify location of labels relative to y-axis
       labels = format(xlab, "%m/%d/%Y"),
       pos = 1,
       offset = 1, #place labels below the y coordinate.
 #      srt = 0, # rotate text 45 degrees
       cex = 1.7, # enlarge labels
       #adj = c(1.1,1.2)
       ) 
  # move label position to line up under tick marks
  # Using the adj argument to move rotated tick labels is weird. If the value is
  # (0,0), the base of the first letter/number will sit just above the tick
  # mark. Adjusting the first value to adj = c(1,0) will move the label so the
  # end of the last character of the label is to the left and below the tick mark.
  # You'll still want to move the label down a bit further, and probably move it
  # to the right in the process, so play with both values of adj = c(1.1,1.2)
  
  
  ylabs = seq(-30,45,10) #create ylabs object
  xlow = par()$usr[1] #get outer bounds for the plot window
  xhigh = par()$usr[2]
  print(as.POSIXct(xlow, origin = "1970-01-01", tz=""))
  print(as.POSIXct(xhigh, origin = "1970-01-01", tz=""))
  
  #Y-AXIS labels: same thing as with x-labels, but copied to be on BOTH sides of the plot.
  
  #near side (left)
  text(x = xlow, 
       y = seq(-30,45,10), 
       labels = ylabs,
       cex = 2.5,
       pos = 2)
  #far side (right)
  text(x = xhigh,
       y = seq(-30,45,10), 
       labels = ylabs,
       cex = 2.5,
       pos = 4)
  
  
  #abline(). #Put in some lines. Find a way to clip the lines at the axes. Currently 
  #experimenting with ablineclip but encountering errors.
  
  ablineclip(h = c(-30,-20,-10,0,10,20,30,90), x1=as.POSIXct("2020-08-01 00:00"), x2=as.POSIXct("2021-10-31 00:00"),  lty = 1, lwd = 1.5, col = "gray")
  ablineclip(h = c(-25,-15,-5,0,5,15,25,35), x1=as.POSIXct("2020-08-01 00:00"), x2=as.POSIXct("2020-08-01 00:00"), col = "gray")
  ablineclip(v = xlab, y1 = -30, y2 = 90)
  par = op # reset plotting options to turn on masking
  
 
   ### THE FOR LOOP ###
  #The "money" part of the function. We fed the function a group of character strings (file names), and for every entry (every "i") in the list, R will execute the following:
  #Read the csv file given by the filepath.
  #convert the date.time column to the proper format
  #subset the dataframe to a set date range
  #figure out what color the plotted line should be, based on a series of "if/if else/else" statements. For example, if it finds the string "air" in the filename, "gray74" is added to a list of colors called "legend_color".
    #plots a line, using "value" (temperature) as a function of "date.time", and the color determined by the if statements above. 
  
  for(i in data_names){
    #if(skipper != 0){
      #if(skipper >= 8){skipper = 1}
      #color_no = colorlist[skipper]
      #print(color_no)
      #print(i)
      temp_data_next <- read.csv(i)
      temp_data_next$date.time <- mdy_hms(temp_data_next$date.time)
      # temp_data_next$date.time <- as.POSIXct(temp_data_next[,1], format = "%m/%d/%Y %H:%M")
      temp_data_next <- subset(temp_data_next, date.time>= "2020-08-01 03:00" & date.time<= "2021-10-31 00:00") #changing the date range
      #str(temp_data_next)
      
      
      if(grepl("air", i)){
        legend_color <- "gray74"
   #     legelist <- c(legelist, legend_color)
      }
      else if(grepl("lsurf", i, fixed = TRUE)){
        legend_color <- c("darksalmon")
     #   legelist <- c(legelist, legend_color)
      }
      else if(grepl("m01surf", i, fixed = TRUE)){
        legend_color <- c("darksalmon")
  #      legelist <- c(legelist, legend_color)
      }
      else if(grepl("m0surf", i, fixed = TRUE)){
        legend_color <- c("green3")
#        legelist <- c(legelist, legend_color)
      }
      else if(grepl("m02surf", i, fixed = TRUE)){
        legend_color <- c("green3")
  #     legelist <- c(legelist, legend_color)
      }
      else if(grepl("m10", i, fixed = TRUE)){
        legend_color <- c("royalblue2")
  #     legelist <- c(legelist, legend_color)
      }
      else if(grepl("m30", i, fixed = TRUE)){
        legend_color <- c("orchid")
  #    legelist <- c(legelist, legend_color)
      }
      else if(grepl("m50", i, fixed = TRUE)){
        legend_color <- c("pink")
  #     legelist <- c(legelist, legend_color)
      }
      else{
        legend_color <- c("yellow")
#        legelist <- c(legelist, legend_color)
      }
      
legelist <- c(legelist, legend_color)
      
   lines(temp_data_next$date.time, temp_data_next$value, type = "l", lwd = 2, col = as.character(legend_color))
    
  }
  
  legend("topleft", legend = plot_names, col = legelist, lty = 1, cex = 2, lwd = 3, title="Position (cm)", text.font=4)
  #return()
  dev.off()
  
}

#create and run the set
set <- as.list(c("C2A_R1", "C2A_R2", "C2A_R3", 
                 "C2B_R1", "C2B_R2", "C2B_R3",
                 "C5A_R1", "C5A_R2", "C5A_R3",
                 "C5B_R1", "C5B_R2", "C5B_R3",
                 "D2A_R1", "D2A_R2", "D2A_R3", 
                 "D2B_R1", "D2B_R2", "D2B_R3",
                 "D5A_R1", "D5A_R2", "D5A_R3"))
#print(set)

lapply(set, lotsaplots.sites)


set <- as.list(c("C5A_R1", "C5A_R2"))

#for testing
lotsaplots("C2A_R1", "annotation here")
dev.off()


## 

#setwd("C:/Users/sbaue/Documents/R TEMPRY/Itasca_2020_Fall")

#my_data <- list.files("./CLEAN_DATA", full.names = TRUE)
#print(my_data)
#iterate_csv(C2A_R1_filenames, "Low Invasion C2A R1", c("air temp", "surface", "0", "-10", "-30"))

#lotsaplots(sorted_data[c(104,5:9)], "High Invasion C5B R2", c("air temp", "surface", "0", "-10", "-30", "-50"))
#lotsaplots(sorted_data[c(104,10:13)], "High Invasion C5B R3", c("air temp", "2", "-2", "-10", "-30"))


```

