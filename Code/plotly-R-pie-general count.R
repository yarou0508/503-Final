# set work dirction
setwd("C:/Users/47532/Desktop/503Visualization/Final Exam")
library(plotly)
library(lubridate)

# Read in dataset
h1b <- read.csv('H-1B_Disclosure_Data_FY17.csv')

# Clean dataset
#Remove columns with useless information in Crime data.
colnames <- colnames(h1b)
sub_h1b <- h1b[,c('CASE_STATUS', 'EMPLOYER_NAME', 'CASE_SUBMITTED', 'DECISION_DATE', 'VISA_CLASS', 'EMPLOYER_CITY',
                  'EMPLOYER_STATE', 'EMPLOYER_POSTAL_CODE', 'PW_UNIT_OF_PAY', 'PREVAILING_WAGE',
                  'WAGE_RATE_OF_PAY_FROM', 'WORKSITE_CITY', 'WORKSITE_STATE',
                  'JOB_TITLE', 'SOC_NAME')]
# Remove the row that with no 'EMPLOYER_STATE'
sub_h1b <- subset(sub_h1b, EMPLOYER_STATE!= '')
# Select only h1b visa class
sub_h1b <- sub_h1b[sub_h1b$VISA_CLASS=='H-1B',]
# Compute the duration between submitted date and dicision date
# Divide 'DECISION_DATE' column into year, month, day
sub_h1b$duration <- as.Date(sub_h1b$DECISION_DATE, format="%m/%d/%Y") - 
  as.Date(sub_h1b$CASE_SUBMITTED, format="%m/%d/%Y")

# Save the cleaned h1b dataset
write.csv(sub_h1b, file = "cleaned_h1b_2016.csv")
# Check whether there is NAN
sum(is.na(sub_h1b$duration))

# Do some computation
h1b_status <-  sub_h1b %>% group_by(CASE_STATUS) %>% summarise(number = n())

# Draw a plotly pie chart
x <- h1b_status$CASE_STATUS
count <- h1b_status$number
data <- data.frame(x, count)

Sys.setenv("plotly_username"="yx160")
Sys.setenv("plotly_api_key"="AcbKobOFUHf7UYvCtsPH")
colors <- c('rgb(138,43,226)', 'rgb(255,215,0)', 'rgb(124,252,0)', 'rgb(255,105,180)')
p <- plot_ly(data, labels= ~x, values = ~count, type = 'pie',
              textinfo = 'label+percent', insidetextfont = list(color = '#FFFFFF', size = 20),
              marker = list(colors = colors, line = list(color = '#FFFFFF', width = 1))) %>%
          layout(title = '2016 H1B application status',
                 xaxis = list(showgrid = FALSE, zeroline = FALSE, showticklabels = FALSE),
                 yaxis = list(showgrid = FALSE, zeroline = FALSE, showticklabels = FALSE))


api_create(p, filename="pie-h1b-status")
htmlwidgets::saveWidget(as_widget(p), 'pie-h1b-status.html')
