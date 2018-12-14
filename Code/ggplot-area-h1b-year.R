library(ggplot2)
library(lubridate)
setwd("C:/Users/47532/Desktop/503Visualization/Final Exam")
# Read in dataset
h1b <- read.csv('H1B Petitions FY 2007 Through 2017.csv')
brks <- h1b$Year[seq(1, length(h1b$Year), 1)]
lbls <- lubridate::year(brks)
theme_set(theme_bw())
ggplot(h1b, aes(x=Year)) +
  geom_area(aes(y = Receipts, fill="Receipts")) + 
  geom_area(aes(y=Approvals, fill="Approvals")) + 
  labs(title="H1B Petitions From 2007 To 2017",
       caption="Source: U.S. Citisenship and Immigration Services", 
       y="Petition number")+
  scale_x_continuous(~Year, breaks= c(2007,2008,2009,2010,2011,2012,2013,2014,2015,2016,2017))
  theme(panel.grid.minor = element_blank())  
