# set work dirction
setwd("C:/Users/47532/Desktop/503Visualization/Final Exam")
library(htmlwidgets)
library(threejs)

# Read in dataset
h1b <- read.csv('h1b_2016_computer.csv')
h1b <- h1b[h1b$class == 'Data',]
MyJ3 = scatterplot3js(h1b$PREVAILING_WAGE, h1b$ACTUAL_ANNUAL_SALARY, h1b$duration,
                      color = c('grey','red','blue','orange')[as.factor(h1b$CASE_STATUS)],
                      axisLabels = c('PrevailingWage','ProcessTime', 'ActualSalary'))
saveWidget(MyJ3, file = "h1b_salary_3D.html",selfcontained = TRUE,  background = "white")
