# Bellabeat Case Study

*This is my capstone project for the Google Data Analytics Professional Certificate, that consists in a case-study to develop data-informed recommendations for a company called Bellabeat*

# Summary

1. Business task
2. Data source
3. Cleaning and manipulation of data
4. Analysis and visualizations
5. Key findings and recommendations

# 1. Business task  

Bellabeat, a high-tech manufacturer of health-focused products for women, wants to identify trends in the market of smart devices usage in order to get insights on how these trends can be applied to how Bellabeat customers use their smart devices, explore users’ habits, and develop data-informed marketing strategies.  

The **Bellabeat app**  connects to their line of smart wellness products, like the **Leaf wellness tracker**, and provides users with health data related to their activity, sleep, stress, menstrual cycle, and mindfulness habits. This data can help users better understand their current habits and make healthy decisions.

The **key stakeholders** are  
- Urška Sršen, co-founder and chief of creative officer;
- Sando Mur, cofounder and key member of the Bellabeat executive team; and
- Bellabeat marketing analytics team.

### Questions to be answered
- What are some trends in smart device usage?
- How could these trends apply to Bellabeat customers?
- How could these trends help inform Bellabeat marketing strategies?  

In other words:  
- **How can the user's habits found in the analysis of the Fitbit inform the marketing team's strategies?**

# 2. Data source 

**FitBit Fitness Tracker Data** (CC0: Public Domain, dataset made available through Mobius) contains information from thirty fitbit users who consented to the submission of personal tracker data. The data includes information about daily activities, heart rate, steps, sleep monitoring, minute-level output for physical activity. This dataset was generated by respondents to a distributed survey via Amazon Mechanical Turk for 31 days – between April, 12th 2016 and May, 12th 2016.

# 3. Cleaning and manipulation of data 

### Import and view dataframes

Out of the 18 dataframes available, I decided to import 10. Eight of the imported dataframes have the information on each participant's **activities, calories, intensities, steps** and **sleep** input in daily and hourly format; and the other two are **heartrate** per second (the only one available) and **weight**.
The eight dataframes leftout are the ones registring data by the minute. Since the focus is on the trends and not on specific usage, I decided to leave those out.

After taking a good look at the summaries, I notice that the **daily_activity** dataframe includes the data from the **daily_calories, daily_intensities** and **daily_steps** dataframes. For this reason, I will keep the **daily_activity** dataframe and drop the other three.

Before cleaning, I will check the number of unique IDs to confirm the reliability of the data: **does it have 30 unique IDs?**

### Number of distinct IDs per dataframe:  
daily_activity: 33  
daily_sleep: 24  
hourly_calories: 33  
hourly_intensities: 33   
hourly_steps: 33  
weight: 8  
heartrate: 14 

The dataframes 'daily_sleep', 'weight'and 'heartrate' will be dropped because they do not have the minimum sample size of 30, as recommended by the Central Limit Theorem (CLT).  

### Dataframes that will used in this analysis:
- daily_activity  
- hourly_calories   
- hourly_intensities   
- hourly_steps  

The number of unique IDs returned is 33. But the metadata informed that it was 30 participants.  
**Are there more participants or is the data dirty?**

There is no apparent error with the unique ID strings. There are 33 of them and they are all 10 characters long. So the dataframes have information from **33 people**. 

**TBC**

# 5. Key findings and recommendations  

### Summary of trends identified on fitbit users:

- **54%** of the users (18 people) follow the recommendation of 150 minutes of moderate activity (600 or more minutes per month) or 75 (300 of more minutes per month) minutes of vigorous activity per week.

- The sum of all the daily intensity hours equals **20,31 hours** instead of a full day (24 hours). This means that, on average, 3,70 hours a day were not tracked. Further research is necessary to find out as to why this happens.

- The users are in average most active on **Saturdays** followed by **Tuesdays**.

- People are most active afterwork hours/dinner time (**between 17h and 19h**) followed by lunch time (**between 12h and 14h**).

- The **correlation** between light active distance and lightly active minutes and between moderately active distance and fairly active minutes are **positive**. This means that when the active minutes increase, the active distance also increses and when one decreases, the other also decreases.

- The **correlation** between very active distance and very active minutes is **non-linear or curvilinear**. This means that the ratio os change between the very active distance and very active minutes is not constant. Some users did many very active minutes but it didn't translate into longer distances.

- **21%** (7 people) of the users follow *in average* the recommendation of 10000 a day. It is important to point out that none of the users actually take 10000 everyday, as showed in the *min_steps* (minumum steps) column.

- The **correlation** between steps and calories is **non-linear or curvilinear**. This means that the ratio os change between the very active distance and very active minutes is not constant, although it is **positive up until 20000 steps**, which is over the average number of daily steps.


### How could these trends apply to Bellabeat customers?

The data collected from fitbit can help Bellabeat better understand their customers' habits and help them make healthier decisions through the **Bellabeat app**.
The app connects to their line of smart wellness products and provides users with health data related to their activity, sleep, stress, menstrual cycle, and mindfulness habits. **The activity trends from fitbit will inform the Bellabeat marketing strategy for the Leaf tracker**.


### Recommendations for the Bellabeat marketing team

- **Encourage day-to-day activities** like walking. Nearly half of the users do not reach the recommendend weekly activity. The positive correletation between active distance and active minutes shows that walking is a good way to exercise – but the intensity and frequency should be taken into consideration.

- **Promote alternative exercises** for people who do not like going to the gym, eg. jogging, dancing,cycling, hiking, yoga.

- Encourage users to **be mindful about the most active hours of the day 17h–19h**. **Ways of skipping the rush hour**: eg. go to a gym close to work and exercise while you wait; jog/walk home; cycle home; play football after work.

- The marketing campaigns should focus on **people who are interested in a healthier life style and sel-awareness**.

- **Partnerships** with fitness clubs, gyms and athletes, to promote the unique features of tracker. 

- Bellabeat is high-tech manufacturer of **health-focused** products for **women**. The feature for tracking the changes in menstrual cycle can also be campaigned for **other people who have menstrual cycles but don't identify as a woman**, such as trans men and intersex people.


### A few questions that need more data:

- Which times of the day people have more moderate activity minutes or vigorous activity minutes?

- How do sleep hours and steps taken correlate?

- How do sleep hours and heartrate correlate?

- How do weight and intensity minutes correlate?

- How do the menstrual cycle affects sleep?

- What is the relation between stress and activity? 






