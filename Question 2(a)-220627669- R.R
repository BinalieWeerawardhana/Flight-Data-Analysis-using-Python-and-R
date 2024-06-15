
## PART 2 ##

## (a) What are the best times and days of the week to minimise delays each year? ##

#Cleaning the Data#
  
df2006 <- read_csv("C:/Users/DELL/Documents/python codes/2006.csv")
head(df2006)

df2007 <- read_csv("C:/Users/DELL/Documents/python codes/2007.csv")
head(df2007)

# Check if the column order is the same
same_order <- all.equal(names(df2006), names(df2007))
print(same_order)

# Merging
mergeddataset <- rbind(df2006, df2007)
mergeddataset

# Total data set after combining
shape <- dim(mergeddataset)
print(shape)

missing_counts <- colSums(is.na(mergeddataset))
print(missing_counts)

# Dropping the Cancellation Code column as majority of values are null
mergeddataset <- mergeddataset[, !names(mergeddataset) %in% "CancellationCode"]

# Create a new column Total_Delay by adding ArrDelay and DepDelay
mergeddataset$Total_Delay <- mergeddataset$ArrDelay + mergeddataset$DepDelay

# Impute missing values for remaining columns
median_value <- median(mergeddataset$CRSElapsedTime, na.rm = TRUE)
mergeddataset$CRSElapsedTime <- ifelse(is.na(mergeddataset$CRSElapsedTime), median_value, mergeddataset$CRSElapsedTime)

library(dplyr)
mode_value <- mergeddataset$TailNum %>% table() %>% names() %>% as.character() %>% .[which.max(mergeddataset$TailNum %>% table())]
mergeddataset$TailNum[is.na(mergeddataset$TailNum)] <- mode_value

missing_counts <- colSums(is.na(mergeddataset))
print(missing_counts)

# Saving the cleaned data for future requirements
write.csv(mergeddataset, file = "clean_dataset.csv", row.names = FALSE)

cleaned_dataset <- read.csv("C:/Users/DELL/Documents/python codes/clean_dataset.csv")

cleaned_dataset <- mergeddataset
cleaned_dataset

 
## BEST TIMES OF DAY TO MINIMISE DELAYS##

#Extracting the cleaned 2006 data
delays2_2006 <- cleaned_dataset[cleaned_dataset$Year == 2006, ]
delays2_2006

#extracting the desired columns for arrival delays in 2006
arrival_delays2006 <- delays2_2006[, c("Year", "Month", "DayofMonth", "DayOfWeek", "CRSArrTime", "ArrTime", "ArrDelay", "CRSDepTime")]
arrival_delays2006

#checking null values
missing_counts <- colSums(is.na(arrival_delays2006))
print(missing_counts)

#removing early arrivals
arrival_delays2006 <- arrival_delays2006[arrival_delays2006$ArrDelay >= 0, ]

#extracting the desired columns for departure delays in 2006
departure_delays2006 <- delays2_2006[, c("Year", "Month", "DayofMonth", "DayOfWeek", "CRSDepTime", "DepTime", "DepDelay")]
departure_delays2006

#checking null values
missing_counts <- colSums(is.na(departure_delays2006))
print(missing_counts)

#dropping null values 
departure_delays2006 <- na.omit(departure_delays2006)
#rechecking null values
missing_counts <- colSums(is.na(departure_delays2006))
print(missing_counts)

#removing early departures
departure_delays2006 <- departure_delays2006[departure_delays2006$DepDelay >= 0, ]

#Extracting the cleaned 2007 data
delays2_2007 <- cleaned_dataset[cleaned_dataset$Year == 2007, ]
delays2_2007

#extracting the desired columns for arrival delays in 2007
arrival_delays2007 <- delays2_2007[, c("Year", "Month", "DayofMonth", "DayOfWeek", "CRSDepTime", "CRSArrTime", "ArrTime", "ArrDelay")]
arrival_delays2007

#checking null values
missing_counts <- colSums(is.na(arrival_delays2007))
print(missing_counts)

#dropping null values
arrival_delays2007 <- na.omit(arrival_delays2007)
#rechecking null values
missing_counts <- colSums(is.na(arrival_delays2007))
print(missing_counts)

#removing early arrivals
arrival_delays2007 <- arrival_delays2007[arrival_delays2007$ArrDelay >= 0, ]

#extracting the desired columns for departure delays in 2007
departure_delays2007 <- delays2_2007[, c("Year", "Month", "DayofMonth", "DayOfWeek", "CRSDepTime", "DepTime", "DepDelay")]

#checking null values
missing_counts <- colSums(is.na(departure_delays2007))
print(missing_counts)

#dropping null values 
departure_delays2007 <- na.omit(departure_delays2007)
#rechecking null values
missing_counts <- colSums(is.na(departure_delays2007))
print(missing_counts)

#removing early departures
departure_delays2007 <- departure_delays2007[departure_delays2007$DepDelay >= 0, ]

# binning the hours of scheduled arrival time for arrival delays dataset
arrival_delays2006$Hours_Binned <- floor(arrival_delays2006$CRSDepTime / 100)

departure_delays2006$Hours_Binned <- floor(departure_delays2006$CRSDepTime / 100)

# giving a range and labels for the plot
my_range <- c(0,100,200,300,400,500,600,700,800,900,1000,1100,1200,1300,1400,1500,1600,1700,1800,1900,2000,2100,2200,2300,2400)
my_labels <- c('0-1','1-2','2-3','3-4','4-5','5-6','6-7','7-8','8-9','9-10','10-11','11-12','12-13','13-14','14-15','15-16','16-17','17-18','18-19','19-20','20-21','21-22','22-23','23-24')

# changing the type of the variable CRSDepTime to category
arrival_delays2006$CRSDepTime <- cut(arrival_delays2006$CRSDepTime, breaks = my_range, labels = my_labels)
departure_delays2006$CRSDepTime <- cut(departure_delays2006$CRSDepTime, breaks = my_range, labels = my_labels)

# calculating average arrival and departure delays by scheduled Departure Time
avg_arrival_delay2006_byday <- aggregate(ArrDelay ~ CRSDepTime, arrival_delays2006, mean)
avg_departure_delay2006_byday <- aggregate(DepDelay ~ CRSDepTime, departure_delays2006, mean)

#creating a line graph
library(ggplot2)

# Plotting the graph
str(avg_arrival_delay2006_byday)
str(avg_departure_delay2006_byday)
str(index_numeric)

# Extracting relevant columns for plotting
arrival_delay <- avg_arrival_delay2006_byday$ArrDelay
departure_delay <- avg_departure_delay2006_byday$DepDelay

# Convert the index to a numeric vector for plotting
index_numeric <- 1:length(avg_arrival_delay2006_byday$CRSDepTime)

plot(index_numeric, arrival_delay, type="o", col="lightblue", pch=19,ylim=c(0, max(arrival_delay, departure_delay) + 60), xlab="Time", ylab="Delay Time (minutes)", main="Average Delays in 2006",lwd=2.5)
points(index_numeric, departure_delay, type="o", col="deepskyblue", pch=19, lw=2.5)
total_delay_byday <- arrival_delay + departure_delay
points(index_numeric, total_delay_byday, type="o", col="royalblue", pch=19,lw=2.5)

# Adding data labels
text(index_numeric, total_delay_byday + 5, labels=round(total_delay_byday, 1), pos=3)
axis(1, at=index_numeric, labels=avg_arrival_delay2006_byday$CRSDepTime, las=2)
legend("topright", legend=c("Average Arrival Delay", "Average Departure Delay", "Total Delay"), col=c("lightblue", "deepskyblue", "royalblue"), pch=19, cex=0.8)
par(las=2)

# binning the hours of scheduled arrival time for arrival delays dataset
arrival_delays2007$Hours_Binned <- floor(arrival_delays2007$CRSDepTime / 100)
departure_delays2007$Hours_Binned <- floor(departure_delays2007$CRSDepTime / 100)

# giving a range and labels for the plot
my_range <- c(0,100,200,300,400,500,600,700,800,900,1000,1100,1200,1300,1400,1500,1600,1700,1800,1900,2000,2100,2200,2300,2400)
my_labels <- c('0-1','1-2','2-3','3-4','4-5','5-6','6-7','7-8','8-9','9-10','10-11','11-12','12-13','13-14','14-15','15-16','16-17','17-18','18-19','19-20','20-21','21-22','22-23','23-24')

# changing the type of the variable CRSDepTime to category
arrival_delays2007$CRSDepTime <- cut(arrival_delays2007$CRSDepTime, breaks = my_range, labels = my_labels)
departure_delays2007$CRSDepTime <- cut(departure_delays2007$CRSDepTime, breaks = my_range, labels = my_labels)

#calculating average arrival and departure delays by scheduled Departure Time
avg_arrival_delay2007_byday <- aggregate(ArrDelay ~ CRSDepTime, arrival_delays2007, mean)
avg_departure_delay2007_byday <- aggregate(DepDelay ~ CRSDepTime, departure_delays2007, mean)

# Plotting the graph

# Extracting relevant columns for plotting from 2007 data
arrival_delay <- avg_arrival_delay2007_byday$ArrDelay
departure_delay <- avg_departure_delay2007_byday$DepDelay

# Convert the index to a numeric vector for plotting
index_numeric <- 1:length(avg_arrival_delay2007_byday$CRSDepTime)

# Plotting the graph for 2007
plot(index_numeric, arrival_delay, type="o", col="limegreen", pch=19, ylim=c(0, max(arrival_delay, departure_delay) + 60), xlab="Time", ylab="Delay Time (minutes)", main="Average Delays in 2007",lwd=2.5)
points(index_numeric, departure_delay, type="o", col="lawngreen", pch=19, lwd=2.5)
total_delay_byday <- arrival_delay + departure_delay
points(index_numeric, total_delay_byday, type="o", col="green", pch=19, lwd=2.5)

text(index_numeric, total_delay_byday + 5, labels=round(total_delay_byday, 1), pos=3)
axis(1, at=index_numeric, labels=avg_arrival_delay2007_byday$CRSDepTime, las=2)
legend("topright", legend=c("Average Arrival Delay", "Average Departure Delay", "Total Delay"), col=c("limegreen", "lawngreen", "green"), pch=19, cex=0.8)
par(las=2)

# Calculating the Overall Average Delay for Departure and Arrival in both 2006 and 2007
delays <- cleaned_dataset[, c('Year', 'Month', 'DayOfWeek', 'DepTime', 'DepDelay', 'ArrTime', 'ArrDelay', 'Total_Delay')]

# Drop rows with missing values
delays <- delays[complete.cases(delays$ArrDelay, delays$DepDelay, delays$Total_Delay),]

#  Add a new column to the dataframe that categorizes the departure time into time slots
bins <- c(0, 600, 1200, 1800, 2400)
labels <- c('Night', 'Morning', 'Afternoon', 'Evening')
delays$TimeOfDay <- cut(delays$DepTime, breaks = bins, labels = labels, include.lowest = TRUE)

# Group the data by time slot and find the average of ArrDelay, DepDelay, and Total_Delay
avg_arr_delay <- c()
avg_dep_delay <- c()
avg_total_delay <- c()
for (time in labels) {
  avg_arr_delay <- c(avg_arr_delay, mean(delays$ArrDelay[delays$TimeOfDay == time], na.rm = TRUE))
  
  avg_dep_delay <- c(avg_dep_delay, mean(delays$DepDelay[delays$TimeOfDay == time], na.rm = TRUE))
  
  avg_total_delay <- c(avg_total_delay, mean(delays$Total_Delay[delays$TimeOfDay == time], na.rm = TRUE))
}

print(avg_arr_delay)
print(avg_dep_delay)
print(avg_total_delay)

# Create a table with time slot, average arrival delay, average departure delay, and average delay
delay_dataset <- data.frame(TimeOfDay = labels,
                            AvgArrivalDelay = avg_arr_delay,
                            AvgDepartureDelay = avg_dep_delay,
                            AvgTotalDelay = avg_total_delay)

print(delay_dataset)

# Plot a bar chart to visualize the average delay by time of day
barplot(delay_dataset$AvgTotalDelay, 
        names.arg = delay_dataset$TimeOfDay, 
        col = 'lightpink', 
        border = 'black',
        xlab = 'Time of Day',
        ylab = 'Average Delay (min)',
        main = 'Average Flight Delays by Time of Day',
        las = 1) 

##BEST DAYS OF THE WEEK TO MINIMISE DELAYS##
 
# Convert the DayOfWeek column to a string for easier readability
  weekday_names <- c('Monday', 'Tuesday', 'Wednesday', 'Thursday', 'Friday', 'Saturday', 'Sunday')

# Replace numeric values with weekday names
delays$DayOfWeek <- weekday_names[delays$DayOfWeek]
head(delays)  

#calculating average arrival and departure delays by Day Of Week in 2006
avg_arrival_delay2006_byweek <- aggregate(ArrDelay ~ DayOfWeek, arrival_delays2006, mean)
avg_departure_delay2006_byweek <- aggregate(DepDelay ~ DayOfWeek, departure_delays2006, mean)

#Creating a line graph for day of week against arrival and departure delay times

par(mfrow = c(1, 1)) 
plot(avg_arrival_delay2006_byweek$DayOfWeek, avg_arrival_delay2006_byweek$ArrDelay, 
     type = "o", col = "red", pch = 19,
     xlab = "Day of the Week", ylab = "Delay Time (minutes)",
     main = "Average Delays by Week in 2006", lwd = 2, ylim = c(0, max(total_delay2006_byweek) + 10))  # Adjust line width

lines(avg_departure_delay2006_byweek$DayOfWeek, avg_departure_delay2006_byweek$DepDelay, 
      type = "o", col = "tomato", pch = 19, lwd = 2)  # Adjust line width

lines(avg_arrival_delay2006_byweek$DayOfWeek, total_delay2006_byweek, 
      type = "o", col = "darkred", pch = 19, lwd = 2)  # Adjust line width

text(avg_arrival_delay2006_byweek$DayOfWeek, avg_arrival_delay2006_byweek$ArrDelay,
     labels = round(avg_arrival_delay2006_byweek$ArrDelay, 1), pos = 3)

text(avg_departure_delay2006_byweek$DayOfWeek, avg_departure_delay2006_byweek$DepDelay,
     labels = round(avg_departure_delay2006_byweek$DepDelay, 1), pos = 3)

text(avg_arrival_delay2006_byweek$DayOfWeek, total_delay2006_byweek,
     labels = round(total_delay2006_byweek, 1), pos = 3)

axis(1, at = 1:7, labels = c("MONDAY", "TUESDAY", "WEDNESDAY", "THURSDAY", "FRIDAY", "SATURDAY", "SUNDAY"), las = 1, cex.axis = 0.8)  # Adjust label size and orientation
legend("topright", legend = c("Average Arrival Delay", "Average Departure Delay", "Total Delay"),
       col = c("red", "tomato", "darkred"), pch = 19, cex = 0.8)  


#calculating average arrival and departure delays by Day Of Week in 2007
avg_arrival_delay2007_byweek <- aggregate(ArrDelay ~ DayOfWeek, data = arrival_delays2007, FUN = mean)
avg_departure_delay2007_byweek <- aggregate(DepDelay ~ DayOfWeek, data = departure_delays2007, FUN = mean)

#Creating a line graph for day of week against arrival and departure delay times

# Calculate total delay for 2007 by adding arrival and departure delays
total_delay2007_byweek <- avg_arrival_delay2007_byweek$ArrDelay + avg_departure_delay2007_byweek$DepDelay

plot(avg_arrival_delay2007_byweek$DayOfWeek, avg_arrival_delay2007_byweek$ArrDelay, 
     type = "o", col = "mediumvioletred", pch = 19,
     xlab = "Day of the Week", ylab = "Delay Time (minutes)",
     main = "Average Delays by Week in 2007", lwd = 2, ylim = c(0, max(total_delay2007_byweek) + 10))  # Adjust line width

lines(avg_departure_delay2007_byweek$DayOfWeek, avg_departure_delay2007_byweek$DepDelay, 
      type = "o", col = "hotpink", pch = 19, lwd = 2)  # Adjust line width

lines(avg_arrival_delay2007_byweek$DayOfWeek, total_delay2007_byweek, 
      type = "o", col = "darkmagenta", pch = 19, lwd = 2)  # Adjust line width

text(avg_arrival_delay2007_byweek$DayOfWeek, avg_arrival_delay2007_byweek$ArrDelay,
     labels = round(avg_arrival_delay2007_byweek$ArrDelay, 1), pos = 3)

text(avg_departure_delay2007_byweek$DayOfWeek, avg_departure_delay2007_byweek$DepDelay,
     labels = round(avg_departure_delay2007_byweek$DepDelay, 1), pos = 3)

text(avg_arrival_delay2007_byweek$DayOfWeek, total_delay2007_byweek,
     labels = round(total_delay2007_byweek, 1), pos = 3)

axis(1, at = 1:7, labels = c("MONDAY", "TUESDAY", "WEDNESDAY", "THURSDAY", "FRIDAY", "SATURDAY", "SUNDAY"), las = 1, cex.axis = 0.8)  # Adjust label size and orientation
legend("topright", legend = c("Average Arrival Delay", "Average Departure Delay", "Total Delay"),
       col = c("mediumvioletred", "hotpink", "darkmagenta"), pch = 19, cex = 0.8)

# Calculating the overall average delay by week, for departure and arrival in both 2006 and 2007

# Fill missing values in 'DepDelay' and 'ArrDelay' with 0
delays$DepDelay[is.na(delays$DepDelay)] <- 0
delays$ArrDelay[is.na(delays$ArrDelay)] <- 0

delays <- delays %>%
  group_by(DayOfWeek) %>%
  mutate(`Avg Departure Delay` = mean(DepDelay, na.rm = TRUE)) %>%
  ungroup()

delays <- delays %>%
  group_by(DayOfWeek) %>%
  mutate(`Avg Arrival Delay` = mean(ArrDelay, na.rm = TRUE)) %>%
  ungroup()

delays <- delays %>%
  group_by(DayOfWeek) %>%
  mutate(`Avg Total Delay` = mean(Total_Delay, na.rm = TRUE)) %>%
  ungroup()

# Create a table with the day of the week, average arrival delay, average departure delay, and average delay
library(dplyr)
delay_table <- delays %>%
  group_by(DayOfWeek) %>%
  summarize(`Avg Arrival Delay` = mean(`Avg Arrival Delay`),
            `Avg Departure Delay` = mean(`Avg Departure Delay`),
            `Avg Total Delay` = mean(`Avg Total Delay`))

# Create a DataFrame with all days of the week
days_of_week <- c('Monday', 'Tuesday', 'Wednesday', 'Thursday', 'Friday', 'Saturday', 'Sunday')
all_days <- data.frame(DayOfWeek = days_of_week)

# Merge the all_days DataFrame with the Delay_Table
delay_table <- merge(all_days, delay_table, by = 'DayOfWeek', all.x = TRUE)
print(delay_table)

# Creating the bar plot
delay_vector <- as.numeric(delay_table$Avg_Total_Delay)
barplot(delay_vector, names.arg = delay_table$DayOfWeek, 
        col = 'navajowhite', border = 'black',
        xlab = 'Day of the Week', ylab = 'Average Delay (min)',
        main = 'Average Flight Delays by Day of the Week')


##BEST TIMES OF THE WEEK TO MINIMISE DELAYS##

#For Delays in 2006
delays2_2006 <- delays2_2006

delays2_2006$Hours_Binned <- arrival_delays2006$Hours_Binned
head(delays2_2006)

#Renaming "DayofMonth" column to "Day"
delays2_2006 <- delays2_2006 %>%
  rename(Day = DayofMonth)
head(delays2_2006)

#Extracting day of the week and hour of the day
delays2_2006 <- delays2_2006 %>%
  mutate(Date = as.Date(paste(Year, Month, Day, sep = "-")),
         DayOfWeek = weekdays(Date))
delays2_2006 <- delays2_2006 %>%
  mutate(Hours_Binned = CRSDepTime %/% 100)

# Calculating average arrival and departure delay for each hour of the day, Monday to Sunday
avg_arrival_delay2006 <- delays2_2006 %>%
  group_by(DayOfWeek, Hours_Binned) %>%
  summarize(Avg_ArrDelay = mean(ArrDelay, na.rm = TRUE)) %>%
  ungroup()
avg_departure_delay2006 <- delays2_2006 %>%
  group_by(DayOfWeek, Hours_Binned) %>%
  summarize(Avg_DepDelay = mean(DepDelay, na.rm = TRUE)) %>%
  ungroup()

# Calculate total average delay for each hour of the day, Monday to Sunday
total_delay2006 <- delays2_2006 %>%
  group_by(DayOfWeek, Hours_Binned) %>%
  summarize(Avg_TotalDelay = mean(Total_Delay, na.rm = TRUE)) %>%
  ungroup()

day_labels <- c('Monday', 'Tuesday', 'Wednesday', 'Thursday', 'Friday', 'Saturday', 'Sunday')

# Plot average arrival delay
ggplot(data = avg_arrival_delay2006, aes(x = Hours_Binned, y = ArrDelay, color = factor(DayOfWeek))) +
  geom_line() +
  facet_wrap(~DayOfWeek, nrow = 1, scales = 'free') +
  labs(title = 'Average Arrival Delays for each day of the Week - 2006',
       x = 'Time',
       y = 'Average Arrival Delays') +
  scale_color_manual(values = rainbow(length(day_labels))) +
  theme_minimal()

# Plot average departure delay
ggplot(data = avg_departure_delay2006, aes(x = Hours_Binned, y = DepDelay, color = factor(DayOfWeek))) +
  geom_line() +
  facet_wrap(~DayOfWeek, nrow = 1, scales = 'free') +
  labs(title = 'Average Departure Delay for each day of the Week - 2006',
       x = 'Time',
       y = 'Average Departure Delays') +
  scale_color_manual(values = rainbow(length(day_labels))) +
  theme_minimal()

#For delays in 2007
delays2_2007 <- delays2_2007
delays2_2007$Hours_Binned <- arrival_delays2007$Hours_Binned
head(delays2_2007)

#Renaming "DayofMonth" column to "Day"
names(delays2_2007)[names(delays2_2007) == 'DayofMonth'] <- 'Day'
head(delays2_2007)

# Calculating average arrival delay for each hour of the day, Monday to Sunday
avg_arrival_delay2007 <- delays2_2007 %>%
  group_by(DayOfWeek, Hours_Binned) %>%
  summarize(ArrDelay = mean(ArrDelay)) %>%
  ungroup()
avg_departure_delay2007 <- delays2_2007 %>%
  group_by(DayOfWeek, Hours_Binned) %>%
  summarize(DepDelay = mean(DepDelay)) %>%
  ungroup()

# Calculate total average delay for each hour of the day, Monday to Sunday
total_delay2007 <- delays2_2007 %>%
  group_by(DayOfWeek, Hours_Binned) %>%
  summarize(Total_Delay = mean(Total_Delay)) %>%
  ungroup()

day_labels <- c('Monday', 'Tuesday', 'Wednesday', 'Thursday', 'Friday', 'Saturday', 'Sunday')

# Plot average arrival delay
plot_arrival <- ggplot(data = avg_arrival_delay2007, aes(x = Hours_Binned, y = ArrDelay, color = factor(DayOfWeek))) +
  geom_line() +
  labs(title = 'Average Arrival Delays for each day of the Week - 2007',
       x = 'Time', y = 'Average Arrival Delays') +
  theme_minimal() +
  theme(legend.position = 'right') +
  scale_color_manual(values = rainbow(length(day_labels)), labels = day_labels)

# Plot average departure delay
plot_departure <- ggplot(data = avg_departure_delay2007, aes(x = Hours_Binned, y = DepDelay, color = factor(DayOfWeek))) +
  geom_line() +
  labs(title = 'Average Departure Delay for each day of the Week - 2007',
       x = 'Time', y = 'Average Departure Delays') +
  theme_minimal() +
  theme(legend.position = 'right') +
  scale_color_manual(values = rainbow(length(day_labels)), labels = day_labels)

# Arrange plots side by side
grid.arrange(plot_arrival, plot_departure, nrow = 1)

#Total Delays
day_labels <- c('Monday', 'Tuesday', 'Wednesday', 'Thursday', 'Friday', 'Saturday', 'Sunday')

# Create a function to plot the total delays
plot_total_delays <- function(data, title) {
  ggplot(data, aes(x = Hours_Binned, y = Total_Delay, color = DayOfWeek)) +
    geom_line() +
    labs(title = title, x = 'Time', y = 'Average Delays') +
    theme_minimal() +
    theme(legend.position = 'right') +
    scale_color_manual(values = rainbow(length(day_labels), start = 0.2, end = 0.9)) +
    guides(color = guide_legend(title = 'Day of the Week'))
}
plot_2006 <- plot_total_delays(total_delay2006, 'Total Delays for each Day of the Week (2006)')
plot_2007 <- plot_total_delays(total_delay2007, 'Total Delays for each Day of the Week (2007)')
plot_grid(plot_2006, plot_2007, ncol = 1)
  