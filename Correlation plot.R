#My working directory
setwd("D:/Research/Lightning strike study")

library(readxl)
library(dplyr)
library(pander)
library(magick)
library(psych)

#Upload data
data<-read_excel("data_AHC.xlsx", sheet="lightning strike deaths data")

#correlation
corr.data<-data%>%
  select(deaths,max_temp, min_temp, mean_temp, humid, prep)

pairs.panels(corr.data,          
             labels= c("deaths", "max_temp", "min_temp","mean_temp","humid","prep"),
             smooth = TRUE,      
             scale = FALSE,      
             density = TRUE,    
             ellipses = TRUE,    
             method = "pearson", 
             pch = 21,           
             col= "yellowgreen",
             bg= "yellowgreen",  
             lm= FALSE,          
             digits= 2,          
             cor = TRUE,         
             jiggle = FALSE,     
             factor = 2,         
             hist.col = "yellowgreen",
             show.points = TRUE, 
             rug = TRUE,         
             breaks = 10,        
             smoother = FALSE,   
             stars = TRUE,       
             ci= FALSE)    
