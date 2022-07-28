---
title: "Google Data Analytics Capstone"
author: "Mohamed Inzamam"
date: '2022-07-28'
output: html_document
---

```{r setup, include=FALSE}
knitr::opts_chunk$set(echo = TRUE)
```

# Case Study 1 - How Does a Bike-Share Navigate Speedy Success

## Scenario
You are a junior data analyst working in the marketing analyst team at Cyclistic, a bike-share company in Chicago. The director of marketing believes the company’s future success depends on maximizing the number of annual memberships. Therefore,your team wants to understand how casual riders and annual members use Cyclistic bikes differently. From these insights, your team will design a new marketing strategy to convert casual riders into annual members. But first, Cyclistic executives must approve your recommendations, so they must be backed up with compelling data insights and professional data visualizations.

## Methodology Used
In order to approach this business task, I have utilized the **Ask, Prepare, Process, Analyze, Share and Act** methodology. Each step is detailed below along with the crucial guiding questions and answers as we progress through the analysis.

## Ask Stage
This is the first stage in the data analysis process where an analyst is required to ask a comprehensive list of questions about the project, its goals and the stakeholder expectations. Fortuantely, for this case study, the main stakeholder is the Director of Marketing and has given us a clear goal: "Design marketing strategies aimed at converting casual riders into annual members". In order to accomplish this, we need to first understand how the two categories of riders differ, why they differ and how can digital media affect their marketing tactics. 

We will be focusing on the first question: **How do annual members and casual riders use Cyclistic bikes differently?**

Based on the project goals and expectations, we know that we need to **learn any insights** on how the data differs between the two categories of Cyclistic Riders and how we can **use those insights to make recommendations** to the Marketing team.

## Prepare
In this stage, we need to focus on collecting the data and answer some important questions about the data we will be using. 

**1. Where is your data located?**

The data is located on Cyclistic internal servers and is collected by the app.

**2. How is the data organized?**

Upon inspection, the data is structured data tabulated under each month and has very straight forward data types such as text, numeric and date-time.

**3. Are there issues with bias or credibility in this data?**

Since the data is collected and stored by Cyclistic's internal data collection teams, we are confident that the data is unbiased and with a large sample set, we can be certain that there will be enough data points after data cleaning to yield relevant and accurate insights. I adopted the ROCCC methodology which stands for **Reliable, Original, Comprehensive, Cited and Current.** Our data fits these all of these requirements. 

**4. How are you addressing licensing, privacy, security, and accessibility?**

For this case study, we will be using historic trip data provided to us under this [license](https://ride.divvybikes.com/data-license-agreement).

**5. How does it help you answer your question?**

Analysts need to sift through the data, in what I like to call a preliminary overview in order get a basic understanding of the data, its strengths and limitations and any obvious outliers that would pop out. This is vital as it helps guide the analyst in either cleaning, removing or transforming the data into a more useful form.


## Process
This step will begin our data cleaning, validation and transformation actions. In the previous step, we realized that the data collected is mostly valid but needs some cleaning and transformation. 

**1. What tools are we going to use to perform our analysis?**

I used Microsoft Excel to get an overview of the data with some basic sorting and filtering but given the size of the data sets, I chose to use R to perform my analysis. R is incredibly robust when working with large data sets and built-in functions and packages are incredibly easy to configure and use. I import the main packages that I will be using for the analysis.

```{r Packages Used, eval=TRUE, include=TRUE, results='hide', message=FALSE}
library(tidyverse)
library(lubridate)
library(ggplot2)
library(psych)
```

Since the data repository contained trip data by each month stored as .CSV files, I downloaded and extracted the last 12 months of data and saved them in my analysis directory. I opted to combine all the data into one file before commencing any cleaning and validation. All the datasets had the same column order so I did not need to rearrange them to combine them. The below code was used for that.

```{r Combination of Files, eval=FALSE, include=TRUE, results='hide', message=FALSE}
# Save files names to a variable.
filenames <- list.files(path="Insert your folder path here", 
                        pattern=".csv", 
                        all.files=TRUE, 
                        full.names=TRUE)

#Use the lapply and bind_rows function to combine the rows of data.
all_data <- filenames[1:12] %>%
    lapply(read_csv) %>%
    bind_rows

write.csv(all_data,"YourFolderPath/all_data.csv",row.names = FALSE)
```


```{r Process Files, eval=TRUE, include=TRUE, results='asis', message=FALSE}
#Import the compiled data set
all_data = read_csv("C:/Users/moham/Documents/Google Analytics Data Sheets/Case Study 1/csv_files/all_data.csv")

# Remove all NA values from the data set by using the complete_cases() function
#This function checks each for completeness and only rows will all complete values will be  returned.
all_data_no_na <- all_data[complete.cases(all_data),]

```

**2. What steps did you take to clean the data?**

 Upon inspection of the variable data, these were some of my observations:
 
- There were lots of blank data points (NA values) that would need to be cleaned.
- Station IDs were a mix of numeric and alphanumeric, however the variables were formatted as text. I decided against converting them to a standard format because there was no way to discern a pattern in order for me to split and concatenate the information.
- Datetime data was measured as per the UNIX Standard which is ideal for numeric calculations that I will utilize in the next step.

The following chunk of code was used to inspect and clean the data. Finally, it was sorted and filtered to remove any discrepancies in the data.

```{r Data Wrangling, eval=TRUE, include=TRUE, results='markup'}
# Rename the ride_id column as trip_id
all_data_no_na <- all_data_no_na %>% rename(trip_id = ride_id)

# Remove any duplicates based on trip_id
all_data_split_time <- all_data_no_na %>% distinct(trip_id, .keep_all = TRUE)

# Removed non-essential columns
all_data_split_time <- subset(all_data_split_time,select = -c(start_lat,start_lng,end_lat,end_lng))

# Use mutate() and difftime() to add a new column showing the ride duration in mins
all_data_split_time$ride_length <- as.numeric(difftime(all_data_split_time$ended_at,
                                                       all_data_split_time$started_at, 
                                                       units = "mins"))

# Create new columns splitting the date and time for efficient analysis.
all_data_split_time$date_of_trip <- as_date(all_data_split_time$started_at)
all_data_split_time$day <- day(all_data_split_time$date_of_trip)
all_data_split_time$month <- month(all_data_split_time$date_of_trip,label = TRUE,abbr = TRUE)
all_data_split_time$year <- year(all_data_split_time$date_of_trip)
all_data_split_time$day_label <- wday(all_data_split_time$date_of_trip, label = TRUE, abbr = FALSE)

# During the data cleaning phase, I noticed that there were negative times in the ride_length variable.
# This means that some of the end times were not accurate. Ideally, I would have to raise this with data collection team and the backend developers to identify the cause. Due to small frequency, I filtered them out of the dataset.
all_data_split_time <- all_data_split_time %>% filter(ride_length > 0)


#Sorted the data by data in ascending order
all_data_split_time <- all_data_split_time %>% arrange(date_of_trip)

```

**3. How can you verify that your data is clean and ready to analyze?**

I export the cleaned data file in to its own csv file and then run the same exploratory checks that were done previously to verify that the data is ready, the variables are of the suitable type and that there are no discrepencies.

**4. Have you documented your cleaning process so you can review and share those results?**

I documented the entire cleaning process in the original script using comments in the code and this markdown file compiles that in an orderly manner to replicate the same cleaning process. This file can be accessed [here].

## Analyze

Once the data was cleaned, sorted and filtered to remove discrepancies, I moved to analyze the data using descriptive statistics. This is the stage where we dig deep into the data to search for trends and relationships between variables. The original set of compiled data showed 5,900,385 records. After cleaning the data, we end up with 4,678,621 records to analyze which is a substantial sample to identify any insights with strong statistical confidence.

```{r Analysis}
# Summary of Ride Length for the whole data set
summary(all_data_split_time$ride_length)

# Basic count and percentage difference of the difference in member vs casual trips
sum_all <- nrow(all_data_split_time)
member_count <- all_data_split_time %>% count(member_casual)
all_data_split_time %>% count(member_casual) %>% summarise(member_casual, 
                                                           percent = (n/sum_all)*100,)

# Count of rideable types
ride_type_count <- all_data_split_time %>% count(rideable_type)
all_data_split_time %>% count(rideable_type) %>% summarise(rideable_type, 
                                                           percent = (n/sum_all)*100,)
```

```{r Plotly Pie Charts}
library(plotly)

fig1 <- plot_ly(member_count, labels = ~member_casual, values = ~n,
                type = 'pie',
                textposition = 'inside',
                textinfo = 'label+percent',
                hoverinfo = 'text',
                text = ~paste('Count: ',n),
                showlegend = FALSE)
fig1 <- fig1 %>% layout(title = 'Comparison of Cyclistic Member Types',
         xaxis = list(showgrid = FALSE, zeroline = FALSE, showticklabels = FALSE),
         yaxis = list(showgrid = FALSE, zeroline = FALSE, showticklabels = FALSE))
fig1

fig2 <- plot_ly(ride_type_count, labels = ~rideable_type, values = ~n, type = 'pie')
fig2 <- fig2 %>% layout(title = 'Comparison of Cyclistic Bike Categories',
         xaxis = list(showgrid = FALSE, zeroline = FALSE, showticklabels = FALSE),
         yaxis = list(showgrid = FALSE, zeroline = FALSE, showticklabels = FALSE))
fig2
```


```{r Further Analysis}

# Grouped Data by Month
data_by_month <- all_data_split_time %>% 
    group_by(month) %>% 
    summarise(count = n(), mean_ride = mean(ride_length)) %>% 
    arrange(desc(mean_ride))

# Grouped Data by Member
data_by_member <- all_data_split_time %>% 
    group_by(member_casual) %>% 
    summarise(count = n(), mean_ride = mean(ride_length)) %>% 
    arrange(desc(mean_ride))

# Grouped Data by Month and Member Type and calculate the Average Duration of trips
data_by_month_member <- all_data_split_time %>% 
    group_by(month,member_casual) %>% 
    summarise(count = n(), mean_ride = mean(ride_length)) %>% 
    arrange(desc(count))
data_by_month_member
```
```{r Aggregrate Analysis of Data}

#Number of Rides per Day and by descending Average Duration
data_by_day_member <- all_data_split_time %>% 
    group_by(day_label,member_casual) %>% 
    summarise(count=n(), average_duration = mean(ride_length)) %>%
    arrange(desc(average_duration))

#Number of Rides per Day and by descending Count
data_by_day_member <- all_data_split_time %>% 
    group_by(day_label,member_casual) %>% 
    summarise(count=n(), average_duration = mean(ride_length)) %>%
    arrange(desc(count))

#Ride Type and Member_casual with count and average duration
data_by_ridetype_member <- all_data_split_time %>% 
    group_by(rideable_type,member_casual) %>% 
    summarise(count=n(), average_duration = mean(ride_length)) %>% 
    arrange(desc(average_duration))

# Start Station Name that is sorted by descending Count
data_by_station_member <- all_data_split_time %>% 
    group_by(start_station_id,member_casual) %>% 
    summarise(count=n(), average_duration = mean(ride_length)) %>% 
    arrange(desc(count))
```
```{r Mode of Certain Variables}
find_mode <- function(x) {
    u <- unique(x)
    tab <- tabulate(match(x, u))
    u[tab == max(tab)]
}

# Mode of the Day with most rides
find_mode(all_data_split_time$day_label)
find_mode(all_data_split_time$month)
find_mode(all_data_split_time$ride_length)

```
```{r Visualizations}
# Prior to plotting the data, we need to order the days of the week in the order that we want it.
data_by_day_member$day_label <- factor(data_by_day_member$day_label, levels= c("Sunday", "Monday", 
                                         "Tuesday", "Wednesday", "Thursday", "Friday", "Saturday"))

data_by_day_member[order(data_by_day_member$day_label), ]

# The first plot is a bar chart comparing the number of rides for each day of the week
plot1 <- data_by_day_member %>% 
    ggplot(aes(x = day_label, y = count, fill = member_casual)) + 
    geom_col(position = "dodge") +
    labs(x = "Day of the Week", 
         y = "Number of Rides", 
         title = "Number of Rides vs. Day of the Week",
         subtitle = "By Member Type",
         fill = "Member Type")
ggplotly(plot1)

# The second plot repeats the same plot but now for average duration.
plot2 <- data_by_day_member %>% 
    ggplot(aes(x = day_label, y = average_duration, fill = member_casual)) + 
    geom_col(position = "dodge") +
    labs(x = "Day of the Week", 
         y = "Average Duration", 
         title = "Average Duration vs. Day of the Week",
         subtitle = "By Member Type",
         fill = "Member Type")
ggplotly(plot2)

# The third plot is the average duration of trips for each month.
plot3 <- data_by_month_member %>% 
    ggplot(aes(x = month, y = count, fill = member_casual)) + 
    geom_col(position = "dodge") +
    labs(x = "Month of the Year", 
         y = "Number of Trips", 
         title = "Total Number of Trips per Month",
         fill = "Member Type")

ggplotly(plot3)


# Additionally, I wanted to visualize data that showed the station with the most number of casual riders.
data_by_station_member_top_15 <- data_by_station_member %>% 
    filter(member_casual=='casual')  %>% 
    arrange(desc(count))
    

data_by_station_member_top_15 <- data_by_station_member_top_15[1:15,1:4]

plot4 <- data_by_station_member_top_15 %>% 
    ggplot(aes(x = start_station_id, y = count, fill = member_casual)) + 
    geom_col(position = "dodge") +
    labs(x = "Station ID", 
         y = "Total Number of Trips", 
         title = "Total 15 Starting Stations vs. Number of Trips",
         fill = "Member Type") + 
    theme(axis.text.x = element_text(angle = 45))
ggplotly(plot4)

```

## Share & Act

Once the analysis is completed and we have obtained the insights, we now have to share this information to the relevant stakeholders. In this case, a presentation will be made for the Director of Marketing outlining the business task, assumptions made using the data we had and translating the insights from the analysis to actionable recommendations. 


Based on my analysis, these were my observations:

* A significant amount of casual riders use Cyclistic during the weekend, notably Saturday and Sunday and the data shows a higher average trip duration for Sunday.
* Most number of casual rider trips were shown to be during the summer months of June through September and then there is decline during the winter months.
* Certain stations have a higher than average number of casual riders but we cannot be certain if they are the same user or not. It is still worth noting.

**How could your team and business apply your insights?**

I have 3 recommendations that I would give:

1. Target most of the marketing budget towards casual riders during the summer months as this would lead to optimal exposure to the benefits of the Cyclistic Annual membership. 
2. Furthermore, marketing campaigns can be further focused towards better offers and promotions during the weekends since that is when most casual riders use the service.
3. We can push marketing media towards the top 10 starting stations that yield the greatest number of rides and then based on the outcome, we can expand and cover the top 50 stations.

**Is there additional data you could use to expand on your findings?**

One of the major concerns for consumers would be cost of the annual memberships so for any further analysis, it would be ideal to possess a sample of anonymous customer_ids to observe how much the average user spends in rides per year. This could yield valuable information and aid the marketing in their pricing strategies.
