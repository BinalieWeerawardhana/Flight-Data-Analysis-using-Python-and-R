
## (b) Evaluate whether older planes suffer more delays on a year-to-year basis ##

#importing the required datasets
library(readr)

df2006 <- read_csv("C:/Users/DELL/Documents/python codes/2006.csv")
head(df2006)

df2007 <- read_csv("C:/Users/DELL/Documents/python codes/2007.csv")
head(df2007)

cleaned_dataset <- read.csv("C:/Users/DELL/Documents/python codes/clean_dataset.csv")
cleaned_dataset

planes <- read.csv("C:/Users/DELL/Documents/python codes/plane-data (1).csv")
planes

# Removing rows with missing values
planes <- na.omit(planes)
planes

#dropping duplicated rows
planes <- unique(planes)
planes

#renaming the column "tailnum" to "TailNum"
colnames(planes)[colnames(planes) == "tailnum"] <- "TailNum"

colnames(planes)

# Left merging the plane data with the cleaned dataset
merged_with_planes <- merge(cleaned_dataset, planes, by = "TailNum", all.x = TRUE)
merged_with_planes

# Saving the merged data
write.csv(merged_with_planes, "merged_with_planes.csv", row.names = FALSE)

colnames(merged_with_planes)

# Changing 'year' to 'YearOfManufacture'
colnames(merged_with_planes)[colnames(merged_with_planes) == "year"] <- "YearOfManufacture"

sapply(merged_with_planes, class)

#extracting the desired columns for question 2
planes_b <- merged_with_planes[, c("TailNum", "Year", "YearOfManufacture", "ArrDelay", "DepDelay", "CarrierDelay", "WeatherDelay", "NASDelay", "SecurityDelay", "LateAircraftDelay")]
planes_b

# Removing the missing values
planes_b <- na.omit(planes_b)

#checking for null values
missing_counts <- colSums(is.na(planes_b))

planes_b$YearOfManufacture <- as.integer(planes_b$YearOfManufacture)
sapply(planes_b, class)

#Sorting the planes_b by YearOfManufacture in ascending order
planes_b <- planes_b[order(planes_b$YearOfManufacture), ]

planes_b <- planes_b[planes_b$YearOfManufacture != 0, ]

#Calculating Plane Age
planes_b$PlaneAge <- planes_b$Year - planes_b$YearOfManufacture
planes_b

sapply(planes_b, class)

planes_b$PlaneAge <- as.integer(planes_b$PlaneAge)
class(planes_b$PlaneAge)

#making plane age <0 a null value
planes_b$PlaneAge[planes_b$PlaneAge < 0] <- NA

# dropping null values
planes_b <- na.omit(planes_b)

# Removing early arrivals
planes_b <- planes_b[planes_b$ArrDelay >= 0, ]

# Removing early departures
planes_b <- planes_b[planes_b$DepDelay >= 0, ]
planes_b

#Extracting the 2006 data from the cleaned dataset merged with plane data
delays2_2006 <- planes_b[planes_b$Year == 2006, ]
delays2_2006

#Extracting the desired columns for arrival delays in 2006
arrival_delays2006b <- delays2_2006[, c("TailNum", "Year", "YearOfManufacture", "ArrDelay", "PlaneAge")]
arrival_delays2006b

#Extracting the desired columns for departure delays in 2006
departure_delays2006b <- delays2_2006[, c("TailNum", "Year", "YearOfManufacture", "DepDelay", "PlaneAge")]
departure_delays2006b

#Extracting the 2007 data from the cleaned dataset merged with plane data
delays2_2007 <- planes_b[planes_b$Year == 2007, ]
delays2_2007

#Extracting the desired columns for arrival delays in 2007
arrival_delays2007b <- delays2_2007[, c("TailNum", "Year", "YearOfManufacture", "ArrDelay", "PlaneAge")]
arrival_delays2007b

#Extracting the desired columns for departure delays in 2007
departure_delays2007b <- delays2_2007[, c("TailNum", "Year", "YearOfManufacture", "DepDelay", "PlaneAge")]
departure_delays2007b

sapply(departure_delays2007b, class)

# Converting PlaneAge into integers
arrival_delays2006b$PlaneAge <- as.integer(arrival_delays2006b$PlaneAge)
departure_delays2006b$PlaneAge <- as.integer(departure_delays2006b$PlaneAge)

arrival_delays2007b$PlaneAge <- as.integer(arrival_delays2007b$PlaneAge)
departure_delays2007b$PlaneAge <- as.integer(departure_delays2007b$PlaneAge)

## Relationship between Mean Arrival Delays, Mean Departure Delays and Total Delays vs Plane Age in 2006 and 2007##

# Calculating mean delays for 2006
library(dplyr)

#mean arrival delays in 2006
mean_arrivaldelays_2006 <- arrival_delays2006b %>%
  group_by(PlaneAge) %>%
  summarize(mean_arrival_delay_2006 = mean(ArrDelay, na.rm = TRUE))

#mean departure delays in 2006
mean_departuredelays_2006 <- departure_delays2006b %>%
  group_by(PlaneAge) %>%
  summarize(mean_departure_delay_2006 = mean(DepDelay, na.rm = TRUE))

#total mean delays in 2006
total_delays_2006 <- inner_join(mean_arrivaldelays_2006, mean_departuredelays_2006, by = "PlaneAge") %>%
  mutate(total_delay_2006 = mean_arrival_delay_2006 + mean_departure_delay_2006) %>%
  select(PlaneAge, total_delay_2006)

# Calculating mean delays for 2007

#mean arrival delays in 2007
mean_arrivaldelays_2007 <- arrival_delays2007b %>%
  group_by(PlaneAge) %>%
  summarize(mean_arrival_delay_2007 = mean(ArrDelay, na.rm = TRUE))

#mean departure delays in 2007
mean_departuredelays_2007 <- departure_delays2007b %>%
  group_by(PlaneAge) %>%
  summarize(mean_departure_delay_2007 = mean(DepDelay, na.rm = TRUE))

#total mean delays in 2007
total_delays_2007 <- inner_join(mean_arrivaldelays_2007, mean_departuredelays_2007, by = "PlaneAge") %>%
  mutate(total_delay_2007 = mean_arrival_delay_2007 + mean_departure_delay_2007) %>%
  select(PlaneAge, total_delay_2007)

# Scatter Plot for Mean Arrival delays vs Plane Age
library(ggplot2)
ggplot() +
  geom_point(data = total_delays_2006, aes(x = PlaneAge, y = total_delay_2006), color = "brown", size = 3) + # Adjust size here
  geom_smooth(data = total_delays_2006, aes(x = PlaneAge, y = total_delay_2006), method = "lm", color = "brown", size = 1.5, se = FALSE, linetype = "solid") +
  geom_point(data = total_delays_2007, aes(x = PlaneAge, y = total_delay_2007), color = "orangered", size = 3) + # Adjust size here
  geom_smooth(data = total_delays_2007, aes(x = PlaneAge, y = total_delay_2007), method = "lm", color = "orangered", size = 1.5, se = FALSE, linetype = "solid") +
  labs(x = "Plane Age", y = "Total Mean Delay (minutes)", title = "Relationship Between Total Mean Delays and Plane Age", color = "Year") +
  scale_color_manual(values = c("2006" = "brown", "2007" = "orangered")) +
  theme_minimal() +
  theme(legend.position = "top")

#Calculating the correlation
correlation_2006 <- cor.test(mean_arrivaldelays_2006$ArrDelay, mean_arrivaldelays_2006$PlaneAge)
print(paste("Mean Arrival Delay and Plane Age correlation in 2006:", correlation_2006$estimate))

correlation_2007 <- cor.test(mean_arrivaldelays_2007$ArrDelay, mean_arrivaldelays_2007$PlaneAge)
print(paste("Mean Arrival Delay and Plane Age correlation in 2007:", correlation_2007$estimate))

# Scatter Plot for Mean Departure delays vs Plane Age
ggplot() +
  geom_point(data = total_delays_2006, aes(x = PlaneAge, y = total_delay_2006, color = "2006"), size = 3, show.legend = TRUE) +
  geom_smooth(data = total_delays_2006, aes(x = PlaneAge, y = total_delay_2006, color = "2006"), method = "lm", se = FALSE, size = 1.5,linetype = "solid", show.legend = TRUE) +
  geom_point(data = total_delays_2007, aes(x = PlaneAge, y = total_delay_2007, color = "2007"), size = 3, show.legend = TRUE) +
  geom_smooth(data = total_delays_2007, aes(x = PlaneAge, y = total_delay_2007, color = "2007"), method = "lm", se = FALSE, size = 1.5,linetype = "solid", show.legend = TRUE) +
  labs(x = "Plane Age", y = "Total Mean Departure Delay (minutes)", title = "Relationship Between Total Mean Departure Delays and Plane Age", color = "Year") +
  scale_color_manual(values = c("2006" = "forestgreen", "2007" = "#008080")) +
  theme_minimal() +
  theme(legend.position = "top")

# Calculating the correlation
correlation_2006 <- cor.test(mean_departuredelays_2006$DepDelay, mean_departuredelays_2006$PlaneAge)
print(paste("Mean Departure Delay and Plane Age correlation in 2006:", correlation_2006$estimate))

correlation_2007 <- cor.test(mean_departuredelays_2007$DepDelay, mean_departuredelays_2007$PlaneAge)
print(paste("Mean Departure Delay and Plane Age correlation in 2007:", correlation_2007$estimate))

# Creating Line Graph to show the trend in Total Delays in 2006 and 2007

# Total mean arrival delays

# Merge the data frames
total_mean_arrival_delays <- merge(mean_arrivaldelays_2006, mean_arrivaldelays_2007, by = "PlaneAge", all = TRUE)

# Rename columns to avoid conflict
names(total_mean_arrival_delays)[names(total_mean_arrival_delays) == "ArrDelay.x"] <- "ArrDelay_2006"
names(total_mean_arrival_delays)[names(total_mean_arrival_delays) == "ArrDelay.y"] <- "ArrDelay_2007"

# Calculate total mean arrival delays
total_mean_arrival_delays$TotalMeanArrivalDelay <- total_mean_arrival_delays$ArrDelay_2006 + total_mean_arrival_delays$ArrDelay_2007

# Select required columns
total_mean_arrival_delays <- total_mean_arrival_delays[, c("PlaneAge", "TotalMeanArrivalDelay")]


# Total mean departure delays

# Merge the data frames
total_mean_departure_delays <- merge(mean_departuredelays_2006, mean_departuredelays_2007, by = "PlaneAge", all = TRUE)

# Rename columns to avoid conflict
names(total_mean_departure_delays)[names(total_mean_departure_delays) == "DepDelay.x"] <- "DepDelay_2006"
names(total_mean_departure_delays)[names(total_mean_departure_delays) == "DepDelay.y"] <- "DepDelay_2007"

# Merge the data frames
total_mean_departure_delays <- merge(mean_departuredelays_2006, mean_departuredelays_2007, by = "PlaneAge", all = TRUE)

# Check the column names and structure of the merged data frame
str(total_mean_departure_delays)

total_mean_departure_delays$TotalMeanDepartureDelay <- total_mean_departure_delays$mean_departure_delay_2006 + total_mean_departure_delays$mean_departure_delay_2007

# Creating Line Graph to show the trend in Total Delays in 2006 and 2007

# Create a data frame for the line graph
total_mean_delays_line <- rbind(
  data.frame(PlaneAge = total_delays_2006$PlaneAge, TotalMeanDelay = total_delays_2006$total_delay_2006, Year = 2006),
  data.frame(PlaneAge = total_delays_2007$PlaneAge, TotalMeanDelay = total_delays_2007$total_delay_2007, Year = 2007)
)

# Plotting line graph
ggplot(total_mean_delays_line, aes(x = PlaneAge, y = TotalMeanDelay, color = factor(Year))) +
  geom_line(size = 1.5) +
  labs(x = "Plane Age", y = "Mean Delay (minutes)", title = "Relationship Between Plane Age and Mean Delays") +
  scale_color_manual(values = c("darkblue", "red"), labels = c("2006", "2007")) +
  theme_minimal()


## Overall relationship between Types of Delays and Plane Age ##

#creating data set with Year Of Manufacture and type of delays
delay_types <- planes_b[, c("TailNum", "Year", "YearOfManufacture", "CarrierDelay", "WeatherDelay", "NASDelay", "SecurityDelay", "LateAircraftDelay", "PlaneAge")]

#making plane age <0 a null value
arrival_delays2006b$PlaneAge <- ifelse(arrival_delays2006b$PlaneAge < 0, NA, arrival_delays2006b$PlaneAge)

# Dropping rows with NA values
delay_types <- na.omit(delay_types)
# Rechecking for NA values
colSums(is.na(delay_types))

#creating arrays for each delay type
carrier_delays <- delay_types[c("CarrierDelay", "PlaneAge")]
carrier_delays <- subset(delay_types, select = c("CarrierDelay", "PlaneAge"))

weather_delays <- delay_types[c("WeatherDelay", "PlaneAge")]
weather_delays <- subset(delay_types, select = c("WeatherDelay", "PlaneAge"))

NAS_delays <- delay_types[c("NASDelay", "PlaneAge")]
NAS_delays <- subset(delay_types, select = c("NASDelay", "PlaneAge"))

security_delays <- delay_types[c("SecurityDelay", "PlaneAge")]
security_delays <- subset(delay_types, select = c("SecurityDelay", "PlaneAge"))

late_aircraft_delays <- delay_types[c("LateAircraftDelay", "PlaneAge")]
late_aircraft_delays <- subset(delay_types, select = c("LateAircraftDelay", "PlaneAge"))

# Making delays<1 null values
carrier_delays$CarrierDelay <- ifelse(carrier_delays$CarrierDelay < 1, NA, carrier_delays$CarrierDelay)
weather_delays$WeatherDelay <- ifelse(weather_delays$WeatherDelay < 1, NA, weather_delays$WeatherDelay)
NAS_delays$NASDelay <- ifelse(NAS_delays$NASDelay < 1, NA, NAS_delays$NASDelay)
security_delays$SecurityDelay <- ifelse(security_delays$SecurityDelay < 1, NA, security_delays$SecurityDelay)
late_aircraft_delays$LateAircraftDelay <- ifelse(late_aircraft_delays$LateAircraftDelay < 1, NA, late_aircraft_delays$LateAircraftDelay)

#dropping null values
carrier_delays <- na.omit(carrier_delays)
weather_delays <- na.omit(weather_delays)
NAS_delays <- na.omit(NAS_delays)
security_delays <- na.omit(security_delays)
late_aircraft_delays <- na.omit(late_aircraft_delays)


#creating boxplots to see its distributions

carrierdelay <- carrier_delays$CarrierDelay
weatherdelay <- weather_delays$WeatherDelay
NASdelay <- NAS_delays$NASDelay
securitydelay <- security_delays$SecurityDelay
lateaircraftdelay <- late_aircraft_delays$LateAircraftDelay

delay_data <- list(CarrierDelay = carrierdelay,
                   WeatherDelay = weatherdelay,
                   NASDelay = NASdelay,
                   SecurityDelay = securitydelay,
                   LateAircraftDelay = lateaircraftdelay)
boxplot(delay_data, 
        names = c('Carrier Delay', 'Weather Delay', 'NAS Delay', 'Security Delay', 'Late Aircraft Delay'),
        ylab = 'Delay Time (minutes)',
        col = 'darkturquoise',
        outline = FALSE)


#calculating mean for each delay type for each plane age

# Calculate average carrier delay for each plane age
planeage_avgcarrierdelay <- carrier_delays %>%
  group_by(PlaneAge) %>%
  summarise(avg_CarrierDelay = mean(CarrierDelay, na.rm = TRUE))

# Calculate average weather delay for each plane age
planeage_avgweatherdelay <- weather_delays %>%
  group_by(PlaneAge) %>%
  summarise(avg_WeatherDelay = mean(WeatherDelay, na.rm = TRUE))

# Calculate average NAS delay for each plane age
planeage_avgnasdelay <- NAS_delays %>%
  group_by(PlaneAge) %>%
  summarise(avg_NASDelay = mean(NASDelay, na.rm = TRUE))

# Calculate average security delay for each plane age
planeage_avgsecuritydelay <- security_delays %>%
  group_by(PlaneAge) %>%
  summarise(avg_SecurityDelay = mean(SecurityDelay, na.rm = TRUE))

# Calculate average late aircraft delay for each plane age
planeage_avglateaircraftdelay <- late_aircraft_delays %>%
  group_by(PlaneAge) %>%
  summarise(avg_LateAircraftDelay = mean(LateAircraftDelay, na.rm = TRUE))

#merging the mean for each delay types

merged_delaytypes <- merge(planeage_avgcarrierdelay, planeage_avgweatherdelay, by = "PlaneAge")
# Merge NAS delay data frame with the merged delay types data frame
merged_delaytypes <- merge(merged_delaytypes, planeage_avgnasdelay, by = "PlaneAge")

# Merge security delay data frame with the merged delay types data frame
merged_delaytypes <- merge(merged_delaytypes, planeage_avgsecuritydelay, by = "PlaneAge")

# Merge late aircraft delay data frame with the merged delay types data frame
merged_delaytypes <- merge(merged_delaytypes, planeage_avglateaircraftdelay, by = "PlaneAge")

head(merged_delaytypes)

names(merged_delaytypes)

# Creating scatter plots to display relationship with delay types and plane age
theme_set(theme_minimal())

# Create separate plots for each delay type
carrier_delay_plot <- ggplot(merged_delaytypes, aes(x = PlaneAge, y = avg_CarrierDelay)) +
  geom_point(color = 'darkmagenta') +
  geom_smooth(method = 'lm', se = FALSE, color = 'darkmagenta') +
  labs(x = 'Plane Age', y = 'Mean Carrier Delay (minutes)') +
  ggtitle('Relationship Between Mean Carrier Delay and Plane Age') +
  theme(plot.title = element_text(size = 12))  

weather_delay_plot <- ggplot(merged_delaytypes, aes(x = PlaneAge, y = avg_WeatherDelay)) +
  geom_point(color = 'darkorchid') +
  geom_smooth(method = 'lm', se = FALSE, color = 'darkorchid') +
  labs(x = 'Plane Age', y = 'Mean Weather Delay (minutes)') +
  ggtitle('Relationship Between Mean Weather Delay and Plane Age')+
  theme(plot.title = element_text(size = 12))  

nas_delay_plot <- ggplot(merged_delaytypes, aes(x = PlaneAge, y = avg_NASDelay)) +
  geom_point(color = 'purple') +
  geom_smooth(method = 'lm', se = FALSE, color = 'purple') +
  labs(x = 'Plane Age', y = 'Mean NAS Delay (minutes)') +
  ggtitle('Relationship Between Mean NAS Delay and Plane Age')+
  theme(plot.title = element_text(size = 12))  

security_delay_plot <- ggplot(merged_delaytypes, aes(x = PlaneAge, y = avg_SecurityDelay)) +
  geom_point(color = 'deeppink') +
  geom_smooth(method = 'lm', se = FALSE, color = 'deeppink') +
  labs(x = 'Plane Age', y = 'Mean Security Delay (minutes)') +
  ggtitle('Relationship Between Mean Security Delay and Plane Age')+
  theme(plot.title = element_text(size = 12))  

late_aircraft_delay_plot <- ggplot(merged_delaytypes, aes(x = PlaneAge, y = avg_LateAircraftDelay)) +
  geom_point(color = 'hotpink') +
  geom_smooth(method = 'lm', se = FALSE, color = 'hotpink') +
  labs(x = 'Plane Age', y = 'Mean Late Aircraft Delay (minutes)') +
  ggtitle('Relationship Between Mean Late Aircraft Delay and Plane Age')+
  theme(plot.title = element_text(size = 12))  

# Arrange the plots in a grid
grid.arrange(carrier_delay_plot, weather_delay_plot, nas_delay_plot,
             security_delay_plot, late_aircraft_delay_plot,
             ncol = 3, nrow = 2)

# Checking the Correlation between Delay Types and Plane Age
cor_carrier_delay <- cor(merged_delaytypes$CarrierDelay, merged_delaytypes$PlaneAge)
cat("Carrier Delay and Plane Age correlation:", cor_carrier_delay, "\n")

cor_weather_delay <- cor(merged_delaytypes$WeatherDelay, merged_delaytypes$PlaneAge)
cat("Weather Delay and Plane Age correlation:", cor_weather_delay, "\n")

cor_nas_delay <- cor(merged_delaytypes$NASDelay, merged_delaytypes$PlaneAge)
cat("NAS Delay and Plane Age correlation:", cor_nas_delay, "\n")

cor_security_delay <- cor(merged_delaytypes$SecurityDelay, merged_delaytypes$PlaneAge)
cat("Security Delay and Plane Age correlation:", cor_security_delay, "\n")

cor_late_aircraft_delay <- cor(merged_delaytypes$LateAircraftDelay, merged_delaytypes$PlaneAge)
cat("Late Aircraft Delay and Plane Age correlation:", cor_late_aircraft_delay, "\n")
