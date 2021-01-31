# import libraries
# provides a lot of useful functions for working with date variables
library(lubridate)
# includes packages like dplyr and ggplot2
# dplyr provides a consistent set of verbs for common data manipulation
# Ggplot2 is another amazing package used for plotting graphs and charts 
library(tidyverse)


# loading data
data <- read.csv("https://opendata.ecdc.europa.eu/covid19/casedistribution/csv", na.strings = "", fileEncoding = "UTF-8-BOM", stringsAsFactors = F)

# convert date format
# data$date_reported <- mdy(paste0(data$month,"-",data$day,"-",data$year))

data$date_reported <- dmy(paste0(data$dateRep))

# data processing
# total cases worldwide to date
cases= sum(data$cases)# total cases and max single day by country

data %>% 
  group_by(countriesAndTerritories) %>% 
  summarise(cases_sum = sum(cases), cases_max = max(cases)) %>% 
  arrange(desc(cases_sum))# total deaths worldwide to date

deaths = sum(data$deaths)# total deaths and max single day by country

data %>% 
  group_by(countriesAndTerritories) %>% 
  summarise(deaths_sum = sum(deaths), deaths_max = max(deaths)) %>% 
  arrange(desc(deaths_sum))

# plotting daily cases and deaths
us <- data[data$countriesAndTerritories == "United_States_of_America",]

US_cases <- ggplot(us, aes(date_reported, as.numeric(cases))) +
  geom_col(fill = "blue", alpha = 0.6) + 
  theme_minimal(base_size = 14) +
  xlab(NULL) + ylab(NULL) + 
  scale_x_date(date_labels = "%Y/%m/%d")
  
US_cases + labs(title="Daily COVID-19 Cases in US")

US_deaths <- ggplot(us, 
 aes(date_reported, as.numeric(deaths))) +
 geom_col(fill = "purple", alpha = 0.6) + 
 theme_minimal(base_size = 14) +
 xlab(NULL) + ylab(NULL) + 
 scale_x_date(date_labels = "%Y/%m/%d")

US_deaths + labs(title="Daily COVID-19 Deaths in US")

# Now lets add in a few more countries
china <- data[data$countriesAndTerritories == "China",]
spain <- data[data$countriesAndTerritories == "Spain",]
italy <- data[data$countriesAndTerritories == "Italy",]

USplot <- ggplot(us, 
 aes(date_reported, as.numeric(notification_rate_per_100000_population_14.days))) +
 geom_col(fill = "blue", alpha = 0.6) + 
 theme_minimal(base_size = 14) +
 xlab(NULL) + ylab(NULL) + 
 scale_x_date(date_labels = "%Y/%m/%d")
 
China_US <- USplot + geom_col(data=china, 
 aes(date_reported, as.numeric(notification_rate_per_100000_population_14.days)),
 fill="red",
 alpha = 0.5)
 
Ch_US_Sp <- China_US + geom_col(data=spain, 
 aes(date_reported, as.numeric(notification_rate_per_100000_population_14.days)),
 fill="#E69F00",
 alpha = 0.4)
 
Chn_US_Sp_It <- Ch_US_Sp + geom_col(data=italy, 
 aes(date_reported, as.numeric(notification_rate_per_100000_population_14.days)),
 fill="#009E73",
 alpha = 0.9)
 
Chn_US_Sp_It + labs(title="China, US, Italy, & Spain")

