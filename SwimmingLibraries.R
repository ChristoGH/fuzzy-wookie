library(plyr)
library(RODBC)
library(ggplot2)
library(ggthemes)
library(outliers)
# install.packages("ggplot2")
# install.packages("devtools")
library("devtools")
#install_github("ggthemes", "jrnold")
# store the current directory
initial.dir<-getwd()

FootNoteALL <-"South Africa Short Course Swimming Champions Pietermaritzburg 7-10 August 2014"

# change to the new directory
setwd("C:/rProgramming/SwimStats")

# change back to the original directory
setwd(initial.dir)
