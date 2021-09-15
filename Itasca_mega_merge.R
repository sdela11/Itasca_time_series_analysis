---
  title: "Itasca_mega_merge"
author: "Sara DeLaurentis"
date: "09/15/2021"
note: 
output: html_document
purpose: "to attempt merging all of the iButton outputs into one long .csv."

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

setwd("C:/Users/sbaue/Documents/R TEMPRY/Itasca_2020_Fall")

file.names <- list.files("./CLEAN_DATA")
head(file.names)

name.data <- file.names %>%  
  str_replace(".csv", "")%>%
  str_split_fixed("_", n=5)


full.file.names <- list.files("./CLEAN_DATA", full.names = TRUE)
head(full.file.names)

name.data <- cbind(full.file.names, name.data)
str(full.file.names)
head(name.data)
tail(name.data)

#class(name.data)

name.df <- as.data.frame(name.data, stringsAsFactors = FALSE) #convert to dataframe


colnames(name.df) <- c("file.names", "treatment", "rep", "position", "buttonID", "season")
head(name.df)
str(name.df)


sorted.name.df <- name.df[order(name.df$file.names),] #sort the files in ascending order. 
view(sorted.name.df)



########################## HERE IS THE GOOD CODE ###########################


   ### FUNCTION START ###


#setting up a function to handle create ALL iButton csv files: "merge_iButtons"
#1. Inputs are full_paths (self-explanatory) and labels (treatment, site, rep, position, button, season)
#2. Need to change date into proper format
#3.

add_meta <- function(full_paths, labels){
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
     
  
    #Creating the metadata dataframe:
    
    #separate the label name in order to add metadata
    fileNM <- labels %>%  
      str_split_fixed("_", n=5)
    
    treatment <- fileNM[,1] #for example, this line means the treatment object consists of the 1st column of fileNM
    rep <- fileNM[,2]
    position <- fileNM[,3]
    buttonID <- fileNM[,4]
    season <- fileNM[,5]
    
    #new dataframe with metadata, setting length equal to the number of rows in the temp_intA, longdate column:
    metadata_df <- tibble(labels, treatment, rep, position, buttonID, season, .rows = length(temps$longdate))
    #print(metadata_df)
    
    #bind the metadata_df and temperature data for the final csv output.
    temp_df <- cbind(metadata_df, temps)
    view(temp_df)
      
  #write the .csv. CHANGE TO a TEST folder IF YOU'RE TESTING
      
  write_csv(temp_df, glue("./CLEAN_DATA_with_meta/{labels}.csv"))
}


#Setting up inputs for the function. final input is df_ibutton_input
file.names <- list.files("./CLEAN_DATA", full.names = FALSE)
name.data.ibutton <- file.names %>%  
  str_replace(".csv", "") %>% 
  as_data_frame()
head(name.data.ibutton)
tail(name.data.ibutton)

full.file.names <- list.files("./CLEAN_DATA", full.names = TRUE) %>% 
  as_data_frame()
head(full.file.names)

df_ibutton_input <- cbind(full.file.names, name.data.ibutton)
colnames(df_ibutton_input) <- c("full_paths", "labels")
head(df_ibutton_input)
tail(df_ibutton_input)


#In case you mess things up, display: create_csv_ibutton_BU

##To TEST just ONE .csv file:
add_meta("./CLEAN_DATA/C2A_R1_m10_i36_2020.csv", "C2A_R1_m10_i36_2020")

##The whole kitten-kaboodle. Do not press "go" unless you're ready:
pwalk(df_ibutton_input, add_meta)



###   Creation of large csv. binding all rows of ibutton files together.  ###
## 09/15/2021 : Successfully printed merged_ibuttons, with one sensor missing due to change in date.time format. I believe that file has seconds in the format.


csv_files <- list.files(path = "./CLEAN_DATA_with_meta", full.names = TRUE)
head(csv_files)

merged_ibuttons <- map_dfr(csv_files, read_csv) #map_dfr reads csv files and joins them by rowbind.
head(merged_ibuttons)

write.csv(merged_ibuttons, file = "merged_ibuttons.csv")

