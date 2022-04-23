#2022-04-22 Outlier Identification

#Created by: Sara DeLaurentis for the purpose of locating outliers (based on visual
#analysis of graphs)

library(tidyverse)
library(stringr)
library(dplyr)
library(lubridate)

##

# Manually set working directory to Itasca_project_19-21 using console (Files tab)

# ---------- Create file name list ----------- #

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


#Select files and regions of interest:

#4/22/2022, 10pm: Meh, not quite working how I want it to.
find.read <- function(string){
  row <- name.df[grepl(as.character(string), name.df$file.names),]
  print(row)
  file <- row$file.names
  data <- read.csv(as.character(file), header = TRUE)

  view(data)
}

find.read("C2B_R3_lsurf")

