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
install.packages("plotrix")
library(plotrix)


#Check your working directory. This is where your files are stored:
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



file.names <- list.files("./CLEAN_DATA", full.names = FALSE)
#class(file.names)
head(file.names)
#tail(file.names)

name.data <- file.names %>%  
  str_replace(".csv", "")%>%
  str_split_fixed("_", n=5)

head(name.data)

full.file.names <- list.files("./CLEAN_DATA", full.names = TRUE)
head(full.file.names)

name.data <- cbind(full.file.names, name.data)
str(full.file.names)
head(name.data)
tail(name.data)

#class(name.data)
name.df <- as.data.frame(name.data, stringsAsFactors = FALSE)
?as.data.frame

colnames(name.df) <- c("file.names", "treatment", "rep", "position", "buttonID", "year")
head(name.df)
str(name.df)

class(name.df$treatment)

#class(name.df)
#str(name.df)



#Selecting groups of .csv files from the filenames data frame using BASE R 
#selection tools. For dplyr use of filter() and select(), see next section.

#First example is extracting all rows from the data frame that were part of 
#treatment C2A. Select rows of data that have "C2A" in the treatment column. 
#Since the column is unspecified (no entry after the comma), it will select all 
#columns.



C2A <- name.df[name.df$treatment == "C2A",]
C2A_file_names <- C2A[, "file.names"]
C2A_file_names


?subset
C2A_R1_rows <- name.df[name.df$treatment == "C2A" & (name.df$rep == "R1" | name.df$rep == "R0"),]

C2A_R1_filenames <- C2A_R1_rows$file.names
C2A_R1_filenames


######### Best .csv File Selection Code, as of 02/08/2021 ###########


C2A_R1_rows <- name.df[name.df$treatment == "C2A" & (name.df$rep == "R1" | name.df$rep == "R0"),]
C2A_R1_filenames<- C2A_R1_rows$file.names
print(C2A_R1_filenames_exp)
#print(C2_trt)


C5A_R1_rows <- name.df[name.df$treatment == "C5A" & (name.df$rep == "R1" | name.df$rep == "R0"),]
C5A_R1_filenames <- C5A_R1_rows$file.names
print(C5A_R1_filenames)



#Using dplyr package (of the tidyverse)

#filter() selects rows
#select() selects columns
#I found that putting them in separate functions was the most helpful. This doesn't seem to return the correct output after selection.




#C2A_filtered <- filter(name.df, treatment == "C2A")
#print(C2A_filtered)

#C2_trt_filtered <- filter(name.df, treatment == "C2A" | treatment == "C2B")
#print(C2_trt_filtered)

#C2_trt_air <- filter(name.df, (treatment =="C2A" | treatment == "C2B") & position == "air")
print(C2_trt_air)

##for input into read.csv functions:

#C2_trt_filenames <- select(C2_trt_filtered, file.names)

#print(C2_trt_filenames)


#C2A_R1_filt <- filter(name.df, treatment == "C2A" & rep == "R1")
#C2A_R1_filt

# <- select(C2A_R1_filt, file.names) 
#C2A_R1_filenames

#C2A_R1_filenames <- as.list(C2A_R1_filenames)
#C2A_R1_filenames <- as.matrix(C2A_R1_filenames)
#print(C2A_R1_filenames)





lotsaplots <- function(data_names, png_name, graph_title, plot_names, annotate) {
  
  legelist <- vector()
  
  #Create label objects for limits and axes:
  xlims <- as.POSIXct(c("2019-11-05 00:00", "2019-12-31 00:00"))
  xlab <- seq(as.POSIXct("2019-11-05 00:00"),as.POSIXct("2019-12-31 00:00"),"2 weeks")

    
#To DO: Get rid of temp_data_1 stuff.

  #Put in the if statements for color
  #Create legendcolor list
  #Add graphing parameters and create blank graph
  #
  
    #print(length(data_names))  
  #temp_data_1 <- read.csv(data_names[1])
  #temp_data_1$date.time <- as.POSIXct(temp_data_1[,1], format = "%m/%d/%Y %H:%M")
  #temp_data_1 <- subset(temp_data_1, date.time>= "2020-04-18 03:00" & date.time<= "2020-06-25 00:00")
  #date.time>= "2019-11-07 03:00" & date.time<= "2020-08-27 00:00"  #full range
  
  #legend_color <- c("gray74")
  
  ##PLOT CODE
  # Rotate x-axis tick labels so they fit better.
  
  png(file = "png_name")# Spring or Fall
  par(mar = c(8,10,10,6)) # expand figure margins to fit the large axis titles
  plot(x = "",
       y = "",
       type = "n",
       xlim = xlims,
       ylim = c(-20,32),
       #cex.axis = 2.5, # expand axis tick labels,
       ylab = '',  # blank y axis label
       xlab = '',  # blank x axis label
       xaxt = 'n') # blank x axis tick marks
  # create y label
  myYlabel = "Degrees C"
  # print y axis title on current plot
  mtext(myYlabel, side = 2, line = 7, cex = 3)
  # print x axis tick marks, but leave labels blank
  axis.POSIXct(side = 1, at = xlab, labels = FALSE,
            cex.axis = 2)
  axis(side = 2, at = seq(-20,30,5), cex = 3)
  # We'll use the text() function to print the rotated labels, but first we need
  # to figure out where the lower limit of the y-axis is so that we can draw
  # text below it
  xlow = par()$usr[3] # 3rd value is minimum y value
  op = par(xpd = NA) # turn off clipping
  text(x = xlab, # specify location of labels relative to x
       y = xlow, # specify location of labels relative to y-axis
       labels = xlab,
       srt = 0, # rotate text 45 degrees
       cex = 2, # enlarge labels
       #adj = c(1.1,1.2)
       ) 
  # move label position to line up under tick marks
  # Using the adj argument to move rotated tick labels is weird. If the value is
  # (0,0), the base of the first letter/number will sit just above the tick
  # mark. Adjusting the first value to adj = c(1,0) will move the label so the
  # end of the last character of the label is to the left and below the tick mark.
  # You'll still want to move the label down a bit further, and probably move it
  # to the right in the process, so play with both values of adj = c(1.1,1.2)
  
  #abline(). #Put in some lines. Find a way to clip the lines at the axes. Currently 
  
  #experimenting with ablineclip but encountering errors.
  
  ablineclip(h = c(-20,-10,0,10,20,30), x1=as.POSIXct("2019-11-01 00:00"), x2=as.POSIXct("2019-11-01 00:00"),  lty = 1, lwd = 1.5, col = "gray")
  ablineclip(h = c(-15,-5,0,5,15,25), x1=as.POSIXct("2019-11-01 00:00"), x2=as.POSIXct("2019-11-01 00:00"), col = "gray")
  ablineclip(v = xlab, y1 = -20, y2 = 30)
  par = op # reset plotting options to turn on masking
  # Place an x-axis title
  mtext('Time', side = 1, line = 6.5, cex = 2.5)
  
  ?abline
  ?ablineclip
  
  ### THE FOR LOOP ###
  
  for(i in data_names){
    #if(skipper != 0){
      #if(skipper >= 8){skipper = 1}
      #color_no = colorlist[skipper]
      #print(color_no)
      #print(i)
      temp_data_next <- read.csv(i)
      temp_data_next$date.time <- as.POSIXct(temp_data_next[,1], format = "%m/%d/%Y %H:%M")
      temp_data_next <- subset(temp_data_next, date.time>= "2019-11-05 03:00" & date.time<= "2019-12-31 00:00")
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
      
   lines(temp_data_next$date.time, temp_data_next$value, type = "l", col = as.character(legend_color))
      
   # }
    
  }
  
  legend("topleft", legend = plot_names, col = legelist, lty = 1, cex = 0.8, title="Position (cm)", text.font=4)
  #return()
}


## 


lotsaplots(C2A_R1_filenames, "Low Invasion C2A R1", c("air", "lsurf", "msurf", "m10", "m30", "m50"))
dev.off()

#str(C2A_R1_filenames_exp)

lotsaplots(C5A_R1_filenames, "DUMMY High Invasion C5A R1", c("lsurf", "msurf", "m10", "m30", "m50"))
dev.off()


C5A_R1_filenames
str(C5A_R1_filenames)

## 



my_data <- list.files("./CLEAN_DATA", full.names = TRUE)
print(my_data)
iterate_csv(C2A_R1_filenames, "Low Invasion C2A R1", c("air temp", "surface", "0", "-10", "-30"))

lotsaplots(sorted_data[c(104,5:9)], "High Invasion C5B R2", c("air temp", "surface", "0", "-10", "-30", "-50"))

lotsaplots(sorted_data[c(104,10:13)], "High Invasion C5B R3", c("air temp", "2", "-2", "-10", "-30"))


```

