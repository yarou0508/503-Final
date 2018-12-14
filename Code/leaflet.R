# set work dirction
setwd("C:/Users/47532/Desktop/503Visualization/Final Exam")
library(rgdal)
library(dplyr)
library(leaflet)
library(stringr)
library(htmlwidgets)

# Read in dataset
h1b <- read.csv('H-1B_Disclosure_Data_FY17.csv')
zip_codes <-read.csv('zip_codes_states.csv')

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

# Save the cleaned h1b dataset
write.csv(sub_h1b , file = "h1b_2016.csv")

# Convert postal code to 5 digits
sub_h1b$EMPLOYER_POSTAL_CODE <- substr(sub_h1b$EMPLOYER_POSTAL_CODE, 1, 5)
#formatC is from C code formatting - creates a 5 digit int
sub_h1b$EMPLOYER_POSTAL_CODE <- formatC(sub_h1b$EMPLOYER_POSTAL_CODE, width = 5, format = "d", flag = "0")
zip_codes$zip_code <- formatC(zip_codes$zip_code, width = 5, format = "d", flag = "0")

#Add two new columns called "lat" and "lng" in df "sub_h1b".
sub_h1b$lat<-zip_codes$latitude[match(sub_h1b$EMPLOYER_POSTAL_CODE,zip_codes$zip_code)]
sub_h1b$lng<-zip_codes$longitude[match(sub_h1b$EMPLOYER_POSTAL_CODE,zip_codes$zip_code)]


# Check whether there is NAN
sum(is.na(sub_h1b$lat))


# Load in us map data
us.map <- readOGR(dsn = "./cb_2016_us_state_20m", layer = "cb_2016_us_state_20m", stringsAsFactors = FALSE)

# Divid the h1b dataset into four according to the case status
h1b_C <- sub_h1b[sub_h1b$CASE_STATUS == 'CERTIFIED',]
# Save the cleaned h1b dataset
write.csv(h1b_C , file = "h1b_2016_certified.csv")
h1b_CW <- sub_h1b[sub_h1b$CASE_STATUS == 'CERTIFIED-WITHDRAWN',]
h1b_W <- sub_h1b[sub_h1b$CASE_STATUS == 'WITHDRAWN',]
h1b_D <- sub_h1b[sub_h1b$CASE_STATUS == 'DENIED',]

# Compute the number of applications per state
h1b_C_count <- h1b_C %>% group_by(EMPLOYER_STATE) %>% summarise(number = n())
h1b_CW_count <- h1b_CW %>% group_by(EMPLOYER_STATE) %>% summarise(number = n())
h1b_W_count <- h1b_W %>% group_by(EMPLOYER_STATE) %>% summarise(number = n())
h1b_D_count <- h1b_D %>% group_by(EMPLOYER_STATE) %>% summarise(number = n())
h1b_D_count<-rbind(h1b_D_count, data.frame(EMPLOYER_STATE='WY', number=0))


# Merge spatial df with county shape data "us map.
h1bmap_C <- merge(us.map, h1b_C_count, by.x='STUSPS', by.y='EMPLOYER_STATE')
h1bmap_CW <- merge(us.map, h1b_CW_count, by.x='STUSPS', by.y='EMPLOYER_STATE')
h1bmap_W <- merge(us.map, h1b_W_count, by.x='STUSPS', by.y='EMPLOYER_STATE')
h1bmap_D <- merge(us.map, h1b_D_count, by.x='STUSPS', by.y='EMPLOYER_STATE')


# Remove NAN lat and lng from sub_h1b dataset
h1b_C<-h1b_C[!h1b_C$lat%in%c("NA","<NA",NA,"na"),]
h1b_C<-h1b_C[!h1b_C$lng%in%c("NA","<NA",NA,"na"),]
h1b_D<-h1b_C[!h1b_D$lat%in%c("NA","<NA",NA,"na"),]
h1b_D<-h1b_C[!h1b_D$lng%in%c("NA","<NA",NA,"na"),]

# Find out how many kind of work titles
h1b_C_data <- h1b_C%>% filter(str_detect(h1b_C$JOB_TITLE, 'DATA'))
h1b_D_data <- h1b_D%>% filter(str_detect(h1b_D$JOB_TITLE, 'DATA'))

#Format popup data for leaflet map.
popup_C<-paste0("<strong>State: </strong>", 
                h1bmap_C$NAME, 
                "<br><strong>Certified number: </strong>", 
                h1bmap_C$number)

popup_CW<-paste0("<strong>State: </strong>", 
                h1bmap_CW$NAME, 
                "<br><strong>Certificated-Withdraw number: </strong>", 
                h1bmap_CW$number)

popup_W<-paste0("<strong>State: </strong>", 
                h1bmap_W$NAME, 
                "<br><strong>Withdraw number: </strong>", 
                h1bmap_W$number)

popup_D<-paste0("<strong>State: </strong>", 
                h1bmap_D$NAME, 
                "<br><strong>Denied number: </strong>", 
                h1bmap_D$number)


popup_C_data<-paste0("<strong>Employer name: </strong>", 
                        h1b_C_data$EMPLOYER_NAME,
                     "<br><strong>Employer city: </strong>",
                     h1b_C_data$EMPLOYER_CITY,
                     "<br><strong>Employer state: </strong>",
                     h1b_C_data$EMPLOYER_STATE,
                     "<br><strong>Job title: </strong>",
                      h1b_C_data$JOB_TITLE,
                        "<br><strong>Occupational name: </strong>", 
                      h1b_C_data$SOC_NAME,
                     "<br><strong>Prevaling wage: </strong>", 
                     h1b_C_data$PREVAILING_WAGE,
                        "<br><strong>Annual salary: </strong>", 
                      h1b_C_data$WAGE_RATE_OF_PAY_FROM,
                      "<br><strong>Processing time: </strong>", 
                      h1b_C_data$duration)

popup_D_data<-paste0("<strong>Employer name: </strong>", 
                     h1b_D_data$EMPLOYER_NAME,
                     "<br><strong>Employer city: </strong>",
                     h1b_D_data$EMPLOYER_CITY,
                     "<br><strong>Employer state: </strong>",
                     h1b_D_data$EMPLOYER_STATE,
                     "<br><strong>Job title: </strong>",
                     h1b_D_data$JOB_TITLE,
                     "<br><strong>Occupational name</strong>",    
                     h1b_D_data$SOC_NAME,
                     "<br><strong>Prevaling wage: </strong>", 
                     h1b_D_data$PREVAILING_WAGE,
                        "<br><strong>Annual salary: </strong>", 
                     h1b_D_data$WAGE_RATE_OF_PAY_FROM,
                        "<br><strong>Processing time: </strong>", 
                     h1b_D_data$duration)



pal <- colorQuantile("YlOrRd", NULL, n = 9)
H1B_records <- leaflet(data = h1bmap_C) %>%
  # Base groups
  addTiles() %>%
  setView(lng = -105, lat = 40, zoom = 3) %>% 
  addPolygons(fillColor = ~pal(number), 
              fillOpacity = 0.8, 
              color = "#BDBDC3", 
              weight = 1,
              popup = popup_C,
              group="H1B certified") %>% 
  addPolygons(fillColor = pal(h1bmap_CW$number), 
              fillOpacity = 0.8, 
              color = "#BDBDC3", 
              weight = 1,
              popup = popup_CW,
              group="H1B certified-withdraw")%>% 
  addPolygons(fillColor = pal(h1bmap_W$number), 
              fillOpacity = 0.8, 
              color = "#BDBDC3", 
              weight = 1,
              popup = popup_W,
              group="H1B withdraw")%>% 
  addPolygons(fillColor = pal(h1bmap_D$number), 
              fillOpacity = 0.8, 
              color = "#BDBDC3", 
              weight = 1,
              popup = popup_D,
              group="H1B denied")%>%
  addMarkers(h1b_C_data$lng, h1b_C_data$lat, popup=popup_C_data
    ,clusterOptions = markerClusterOptions(), group="Certified records for data related job"
  )%>%
  addMarkers(h1b_D_data$lng, h1b_D_data$lat, popup=popup_D_data
             ,clusterOptions = markerClusterOptions(), group="Denied records for data related job"
  )%>%
  # Layers control
  addLayersControl(
    baseGroups = c("H1B certified"),
    overlayGroups = c("H1B certified-withdraw", "H1B withdraw", "H1B denied",
                      "Certified records for data related job","Denied records for data related job"),
    options = layersControlOptions(collapsed = FALSE)
  )

H1B_records

saveWidget(H1B_records, file="leaflet-H1B-map.html", selfcontained = TRUE)  
