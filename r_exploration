# Load tidyverse package

library("tidyverse")
library("skimr")
library("here")
library("janitor")
library("lubridate")


# Import dataframes

daily_activity <- read_csv("/kaggle/input/fitbit/Fitabase Data 4.12.16-5.12.16/dailyActivity_merged.csv")
daily_calories <- read_csv("/kaggle/input/fitbit/Fitabase Data 4.12.16-5.12.16/dailyCalories_merged.csv")
daily_intensities <- read_csv("/kaggle/input/fitbit/Fitabase Data 4.12.16-5.12.16/dailyIntensities_merged.csv")
daily_steps <- read_csv("/kaggle/input/fitbit/Fitabase Data 4.12.16-5.12.16/dailySteps_merged.csv")
daily_sleep <- read_csv("/kaggle/input/fitbit/Fitabase Data 4.12.16-5.12.16/sleepDay_merged.csv")
hourly_calories <- read_csv("/kaggle/input/fitbit/Fitabase Data 4.12.16-5.12.16/hourlyCalories_merged.csv")
hourly_intensities <- read_csv("/kaggle/input/fitbit/Fitabase Data 4.12.16-5.12.16/hourlyIntensities_merged.csv")
hourly_steps <- read_csv("/kaggle/input/fitbit/Fitabase Data 4.12.16-5.12.16/hourlySteps_merged.csv")
weight <- read_csv("/kaggle/input/fitbit/Fitabase Data 4.12.16-5.12.16/weightLogInfo_merged.csv")
heartrate <- read_csv("/kaggle/input/fitbit/Fitabase Data 4.12.16-5.12.16/heartrate_seconds_merged.csv")


# Get summaries from the dataframes

glimpse(daily_activity)
glimpse(daily_calories)
glimpse(daily_intensities)
glimpse(daily_steps)
glimpse(daily_sleep)
glimpse(hourly_calories)
glimpse(hourly_intensities)
glimpse(hourly_steps)
glimpse(weight)
glimpse(heartrate)


# Checking the number of unique IDs

n_distinct(daily_activity$Id)
n_distinct(daily_sleep$Id)
n_distinct(hourly_calories$Id)
n_distinct(hourly_intensities$Id)
n_distinct(hourly_steps$Id)
n_distinct(weight$Id)
n_distinct(heartrate$Id)


# Check the unique ID strings

unique_ids <- unique(daily_activity$Id)
print(unique_ids)


# Check the lenght of the unique IDs

str_length(unique_ids)


# Convert character format to date format

daily_activity$ActivityDate <- mdy(daily_activity$ActivityDate)

hourly_calories$ActivityHour <- mdy_hms(hourly_calories$ActivityHour)
# Split date and time into two different columns
hourly_calories$ActivityDay <- as.Date(hourly_calories$ActivityHour)
hourly_calories$ActivityTime <- format(as.POSIXct(hourly_calories$ActivityHour), format = "%H:%M:%S")

hourly_intensities$ActivityHour <- mdy_hms(hourly_intensities$ActivityHour)
# Split date and time into two different columns
hourly_intensities$ActivityDay <- as.Date(hourly_intensities$ActivityHour)
hourly_intensities$ActivityTime <- format(as.POSIXct(hourly_intensities$ActivityHour), format = "%H:%M:%S")

hourly_steps$ActivityHour <- mdy_hms(hourly_steps$ActivityHour)
# Split date and time into two different columns
hourly_steps$ActivityDay <- as.Date(hourly_steps$ActivityHour)
hourly_steps$ActivityTime <- format(as.POSIXct(hourly_steps$ActivityHour), format = "%H:%M:%S")


# Making sure that the columns names are consistent and unique

daily_activity_clean <- clean_names(daily_activity)
hourly_calories_clean <- clean_names(hourly_calories)
hourly_intensities_clean <- clean_names(hourly_intensities)
hourly_steps_clean <- clean_names(hourly_steps)


# Check columns names

colnames(daily_activity_clean)
colnames(hourly_calories_clean)
colnames(hourly_intensities_clean)
colnames(hourly_steps_clean)


# Checking for NA values

sum(is.na(daily_activity_clean)) 
sum(is.na(hourly_calories_clean))
sum(is.na(hourly_intensities_clean))
sum(is.na(hourly_steps_clean))


# Checking for duplicated values

sum(duplicated(daily_activity_clean))
sum(duplicated(hourly_calories_clean))
sum(duplicated(hourly_intensities_clean))   
sum(duplicated(hourly_steps_clean))


# Comprehensive summary of the data

skim_tee(daily_activity_clean, skim_fun = skim_without_charts)
skim_tee(hourly_calories_clean, skim_fun = skim_without_charts) 
skim_tee(hourly_intensities_clean, skim_fun = skim_without_charts)   
skim_tee(hourly_steps_clean, skim_fun = skim_without_charts)


# Preview of the data

head(daily_activity_clean)
head(hourly_calories_clean)
head(hourly_intensities_clean)
head(hourly_steps_clean)


# What is the percentage of users who follow the recommended minutes activity per week?
# Very active minutes and fairly active minutes in a month (four weeks)

activity_minutes <- daily_activity_clean %>%
group_by(id) %>%
summarize(sum_very_active_min = sum(very_active_minutes), sum_fairly_active_min = sum(fairly_active_minutes)) %>%
arrange(-sum_fairly_active_min)

View(activity_minutes)


# Considering that users are consistent with their exercise routine, I will consider 600 minutes of moderate activity or 300 minutes of vigorous activity per month

activity_minutes %>%
count(follow_recommendation = sum_fairly_active_min >= 600 | sum_very_active_min >= 300) 


# How are the different intensity minutes distributed in an average day?
# Average of different intensity minutes in a day

avg_intensity_minutes <- daily_activity_clean %>%
summarize(
    avg_very_active_minutes = mean(very_active_minutes),
    avg_fairly_active_minutes = mean(fairly_active_minutes),
    avg_lightly_active_minutes = mean(lightly_active_minutes),
    avg_sedentary_minutes = mean(sedentary_minutes)
    )

View(avg_intensity_minutes)


# Transpose dataframe to plot

avg_intensity_minutes_final <- data.frame(
  type_intensity = c("avg_very_active_minutes", "avg_fairly_active_minutes", "avg_lightly_active_minutes", "avg_sedentary_minutes"),
  minutes = c(round(21.16489, digits = 2), round(13.56489, digits = 2), round(192.8128, digits = 2), round(991.2106, digits = 2))
  )

# Plot theme 1

p1 <- theme(
    plot.title = element_text(size = 20, face = "bold"),
    axis.text.x = element_text(size = 15),
    axis.text.y = element_text(size = 15),
    axis.title = element_text(size = 15),
    legend.title = element_text(size = 20), 
    legend.text = element_text(size = 12)
     )

# Plot into a treemap

library(treemapify)
ggplot(avg_intensity_minutes_final, aes(area = minutes, fill = type_intensity, label = minutes)) +
  geom_treemap() +
  geom_treemap_text(size = 20) +
  p1 +
  labs(
      title = "Distribution of intensity minutes in a day",
      fill = "Intensity types"
      )


# Which days of the week are people most active?
# Average intensity minutes per day

hourly_intensities_clean$weekday <- wday(hourly_intensities_clean$activity_day, label=TRUE)

daily_avg_intensity_min <- hourly_intensities_clean %>%
group_by(weekday) %>%
summarize(avg_intensity = mean(total_intensity))

ggplot(data = daily_avg_intensity_min, aes(x = weekday, y = avg_intensity)) +
geom_bar(stat = "identity") +
p1 +
  labs(
      title = "Average intensity minutes per day",
      x = "Days of the week",
      y = "Minutes"
      )


# Which hours of the day are users most active?
# Average intensity minutes per hour

hourly_intensities_mean <- hourly_intensities_clean %>%
group_by(activity_time) %>%
summarize(
    avg_intensity_minutes = mean(total_intensity)
    ) 

ggplot(data = hourly_intensities_mean, 
       aes(x = activity_time, y = avg_intensity_minutes)
      ) +
geom_bar(stat = "identity") +
theme(
    plot.title = element_text(size = 20, face = "bold"),
    axis.text.x = element_text(angle = 90, vjust = 0.5, hjust = 1, size = 12),
    axis.text.y = element_text(size = 12),
    axis.title = element_text(size = 15),
    legend.title = element_text(size = 20), 
    legend.text = element_text(size = 20)
     ) +
labs(
    title = "Average intensity minutes per hour",
    x = "Activity time",
    y = "Minutes"
    )


# How do the different active distances and active minutes correlate?
# Plot theme 2
p2 <- theme(
    plot.title = element_text(size = 20, face = "bold"),
    axis.text.x = element_text(size = 12),
    axis.text.y = element_text(size = 12),
    axis.title = element_text(size = 15),
    legend.title = element_text(size = 20), 
    legend.text = element_text(size = 20)
     )

# Correlation between light active distance and lightly active minutes

ggplot(data = daily_activity_clean) +
geom_point(mapping = aes(x = light_active_distance, y = lightly_active_minutes)) +
geom_smooth(mapping = aes(x = light_active_distance, y = lightly_active_minutes)) +
p2 +
labs(
    title = "Relation between 'light active' distance and 'lightly active' minutes",
    x = "'Light active' distance",
    y = "'Lightly active' minutes"
    )


# How do the different active distances and active minutes correlate?
# Correlation between moderately active distance and fairly active minutes

ggplot(data = daily_activity_clean) +
geom_point(mapping = aes(x = moderately_active_distance, y = fairly_active_minutes)) +
geom_smooth(mapping = aes(x = moderately_active_distance, y = fairly_active_minutes)) +
p2 +
labs(
    title = "Relation between moderately active distance and fairly active minutes",
    x = "'Moderately active' distance",
    y = "'Fairly active' minutes"
    )


# Correlation between very active distance and very active minutes

ggplot(data = daily_activity_clean) +
geom_point(mapping = aes(x = very_active_distance, y = very_active_minutes)) +
geom_smooth(mapping = aes(x = very_active_distance, y = very_active_minutes)) +
p2 +
labs(
    title = "Relation between very active distance and very active minutes",
    x = "'Very active' distance",
    y = "'Very active' minutes"
    )


# How manys users do the recommended amount of steps in a day?
# Average, maximum and minimum steps in a day per user

avg_steps <- daily_activity_clean %>%
group_by(id)%>%
summarize(avg_steps_day = mean(total_steps), max_steps = max(total_steps), min_steps = min(total_steps)) %>%
arrange(-avg_steps_day)

View(avg_steps)


# Recommended minimum of steps in a day

sum(avg_steps$avg_steps_day >= 10000)


# How many days did each user actually take 10000 steps during the surveyed 31 day period?

recommended_steps <-  daily_activity_clean %>%
group_by(id)%>%
summarize(num_days = sum(total_steps >= 10000)) %>%
arrange(-num_days)

View(recommended_steps)


# How do steps and calories correlate?

ggplot(data = daily_activity_clean) +
geom_point(mapping = aes(x = total_steps, y = calories)) +
geom_smooth(mapping = aes(x = total_steps, y = calories)) +
p2 +
labs(
    title = "Relation between calories and steps",
    x = "Steps",
    y = "Calories"
    )





