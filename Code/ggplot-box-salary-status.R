library(ggplot2)
library(dplyr)
library(stringr)
library(ggthemes)
setwd("C:/Users/47532/Desktop/503Visualization/Final Exam")
# Read in dataset
h1b <- read.csv('H-1B_Disclosure_Data_FY17.csv')

# Clean dataset
#Remove columns with useless information in Crime data.
colnames <- colnames(h1b)
sub_h1b <- h1b[,c('CASE_STATUS', 'EMPLOYER_NAME', 'CASE_SUBMITTED', 'DECISION_DATE', 'VISA_CLASS', 'EMPLOYER_CITY',
                  'EMPLOYER_STATE', 'EMPLOYER_POSTAL_CODE', 'PW_UNIT_OF_PAY', 'PREVAILING_WAGE',
                  'WAGE_UNIT_OF_PAY', 'WAGE_RATE_OF_PAY_FROM', 'WORKSITE_CITY', 'WORKSITE_STATE',
                  'JOB_TITLE', 'SOC_NAME')]
# Remove the row that with no 'EMPLOYER_STATE'
sub_h1b <- subset(sub_h1b, EMPLOYER_STATE!= '')
# Select only h1b visa class
sub_h1b <- sub_h1b[sub_h1b$VISA_CLASS=='H-1B',]
# Convert all wage types into annual salary
unique(sub_h1b$PW_UNIT_OF_PAY)
unique(sub_h1b$WAGE_RATE_OF_PAY_FROM)
r1 <- with(sub_h1b, PW_UNIT_OF_PAY=='Hour')
sub_h1b$PREVAILING_WAGE <- replace(sub_h1b$PREVAILING_WAGE, r1, sub_h1b$PREVAILING_WAGE[r1]*40*52)
r1 <- with(sub_h1b, WAGE_UNIT_OF_PAY=='Hour')
sub_h1b$WAGE_RATE_OF_PAY_FROM <- replace(sub_h1b$WAGE_RATE_OF_PAY_FROM, r1, sub_h1b$WAGE_RATE_OF_PAY_FROM[r1]*40*52)

r2 <- with(sub_h1b, PW_UNIT_OF_PAY=='Month')
sub_h1b$PREVAILING_WAGE <- replace(sub_h1b$PREVAILING_WAGE, r2, sub_h1b$PREVAILING_WAGE[r2]*12)
r2 <- with(sub_h1b, WAGE_UNIT_OF_PAY=='Month')
sub_h1b$WAGE_RATE_OF_PAY_FROM <- replace(sub_h1b$WAGE_RATE_OF_PAY_FROM, r2, sub_h1b$WAGE_RATE_OF_PAY_FROM[r2]*12)

r3 <- with(sub_h1b, PW_UNIT_OF_PAY=='Week')
sub_h1b$PREVAILING_WAGE <- replace(sub_h1b$PREVAILING_WAGE, r3, sub_h1b$PREVAILING_WAGE[r3]*52)
r3 <- with(sub_h1b, WAGE_UNIT_OF_PAY=='Week')
sub_h1b$WAGE_RATE_OF_PAY_FROM <- replace(sub_h1b$WAGE_RATE_OF_PAY_FROM, r3, sub_h1b$WAGE_RATE_OF_PAY_FROM[r3]*52)

r4 <- with(sub_h1b, PW_UNIT_OF_PAY=='Bi-Weekly')
sub_h1b$PREVAILING_WAGE <- replace(sub_h1b$PREVAILING_WAGE, r4, sub_h1b$PREVAILING_WAGE[r4]*26)
r4 <- with(sub_h1b, WAGE_UNIT_OF_PAY=='Bi-Weekly')
sub_h1b$WAGE_RATE_OF_PAY_FROM <- replace(sub_h1b$WAGE_RATE_OF_PAY_FROM, r4, sub_h1b$WAGE_RATE_OF_PAY_FROM[r4]*26)

# Compute the duration between submitted date and dicision date
sub_h1b$duration <- as.Date(sub_h1b$DECISION_DATE, format="%m/%d/%Y") - 
  as.Date(sub_h1b$CASE_SUBMITTED, format="%m/%d/%Y")

# Remove NAN data
sub_h1b <- sub_h1b[!sub_h1b$PREVAILING_WAGE%in%c("NA","<NA",NA,"na"),]

h1b_data_computer <- sub_h1b%>% filter(str_detect(sub_h1b$SOC_NAME, c('COMPUTER'))|
                                str_detect(sub_h1b$JOB_TITLE, c('COMPUTER'))|
                                str_detect(sub_h1b$JOB_TITLE, c('DATA'))|
                                str_detect(sub_h1b$SOC_NAME, c('DATA')))

# Creat a new column by dividing job into computer and data
h1b_data_computer$class <- 'Computer'
h1b_data_computer[str_detect(h1b_data_computer$JOB_TITLE, c('DATA'))|
                           str_detect(h1b_data_computer$SOC_NAME, c('DATA')),]$class <- 'Data'
# Save the cleaned h1b dataset
write.csv(h1b_data_computer , file = "h1b_2016_computer.csv")

h1b_computer <- read.csv('h1b_2016_computer.csv')
unique(h1b_computer$class)
#h1b_data_computer <- h1b_C%>% filter(str_detect(h1b_C$JOB_TITLE, 'ANALYST'))
p1 <- ggplot(data = h1b_computer, aes(x = CASE_STATUS, y = PREVAILING_WAGE))+ 
  geom_boxplot(aes(fill = CASE_STATUS))+
  facet_grid(~class, scales = 'free')+
  scale_x_discrete(labels = c('C','CW','D','W'))+
  ggtitle("H1B annual prevailing wage per status")+
  theme_economist() 

p2 <- ggplot(data = h1b_computer, aes(x = CASE_STATUS, y = ACTUAL_ANNUAL_SALARY))+ 
  geom_boxplot(aes(fill = CASE_STATUS))+
  facet_grid(~class, scales = 'free')+
  scale_x_discrete(labels = c('C','CW','D','W'))+
  ggtitle("H1B actual annual salary per status") +
  theme_economist()
grid.arrange(p1, p2, nrow = 2)
library(gridExtra)

