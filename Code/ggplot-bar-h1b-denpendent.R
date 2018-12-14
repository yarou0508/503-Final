library(ggplot2)
library(dplyr)
library(stringr)
library(ggthemes)
setwd("C:/Users/47532/Desktop/503Visualization/Final Exam")
# Read in dataset
h1b <- read.csv('H-1B_Disclosure_Data_FY17.csv')

# Remove NAN lat and lng from sub_h1b dataset
h1b<-h1b[!h1b$H1B_DEPENDENT%in%c(""),]

theme_set(theme_classic())
g <- ggplot(h1b, aes(CASE_STATUS)) 

g + scale_fill_brewer(palette = "Spectral")+
  geom_histogram(aes(fill=H1B_DEPENDENT), 
                   stat="count", bins=4, col="black", size=.1) +
  labs(title="H1B records for H1B dependent and independent company")  
