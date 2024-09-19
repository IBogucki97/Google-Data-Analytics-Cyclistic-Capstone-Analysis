Google Data Analytics Cyclistic Capstone
Ian Bogucki
2024-08-26
Introduction
For the Google Data Analytics Professional Certificate, I have chosen the Cyclistic bike-share case study for my capstone project. In this scenario, I am a junior data analyst working for Cyclistic, a fictional bike-share company based out of Chicago. Lily Moreno, the director of marketing, has tasked me with analyzing data on historical bike trips used by casual and subscription riders.

Cyclistic’s financial analysts have concluded that annual member riders are more profitable compared to casual riders. Moreno wants to maximize the amount of subscription riders, however, they want to first understand how annual members and casual riders use their service differently. Insights from this data would be used in marketing strategies to increase the amount of annual member riders.

Three questions will guide the future marketing program: 1. How do annual members and casual riders use Cyclistic bikes differently? 2. Why would casual riders buy Cyclistic annual memberships? 3. How can Cyclistic use digital media to influence casual riders to become members?

In this fictional scenario, I have been tasked with the first question.

Objective: Clean, visualize, and analyze the data set to observe how annual members use Cyclistic bikes compared to casual members.

Data
The data I will be using is a public data from Motivate International Inc. (Divvy Bicycle Sharing Service from Chicago) under this license. (https://divvybikes.com/data-license-agreement)

The data is stored in a .csv format containing info on bike rides occurring in Q1 of 2019 and 2020. The dataset includes the bike ride start & end time, station information, and member type (casual/member). There is no personable identifiable information within this dataset. Meaning, that there is no way to connect pass purchases to credit card numbers to determine if casual riders live in the Cyclistic service area, or if they have purchased multiple single passes.

Process
For this capstone I am using RStudio for data analysis, cleaning, and transformation. I am using this program due to variety of libraries that allow for an in-depth analysis of this data. In addition to that, due to the size of the dataset, there will be issues processing it through Excel.

library("conflicted") # Manages conflicts
library("lubridate") # works with date-times and time-spans
library("tidyverse")  #helps wrangle data
## Warning: replacing previous import 'lifecycle::last_warnings' by
## 'rlang::last_warnings' when loading 'pillar'
## Warning: replacing previous import 'lifecycle::last_warnings' by
## 'rlang::last_warnings' when loading 'tibble'
## Warning: replacing previous import 'lifecycle::last_warnings' by
## 'rlang::last_warnings' when loading 'hms'
## ── Attaching packages ─────────────────────────────────────── tidyverse 1.3.1 ──
## ✔ ggplot2 3.3.5     ✔ purrr   0.3.4
## ✔ tibble  3.1.4     ✔ dplyr   1.0.7
## ✔ tidyr   1.1.3     ✔ stringr 1.4.0
## ✔ readr   2.0.1     ✔ forcats 0.5.1
library("tis") #identifying holidays

# Set dplyr::filter and dplyr::lag as the default choices
conflict_prefer("filter", "dplyr")
## [conflicted] Will prefer dplyr::filter over any other package.
conflict_prefer("lag", "dplyr")
## [conflicted] Will prefer dplyr::lag over any other package.
Importing data
The datasets for Q1 of 2019 and 2020 are imported separately as .csv files.

## Rows: 365069 Columns: 12── Column specification ────────────────────────────────────────────────────────
## Delimiter: ","
## chr  (4): from_station_name, to_station_name, usertype, gender
## dbl  (5): trip_id, bikeid, from_station_id, to_station_id, birthyear
## dttm (2): start_time, end_time
## ℹ Use `spec()` to retrieve the full column specification for this data.
## ℹ Specify the column types or set `show_col_types = FALSE` to quiet this message.Rows: 426887 Columns: 13── Column specification ────────────────────────────────────────────────────────
## Delimiter: ","
## chr  (5): ride_id, rideable_type, start_station_name, end_station_name, memb...
## dbl  (6): start_station_id, end_station_id, start_lat, start_lng, end_lat, e...
## dttm (2): started_at, ended_at
## ℹ Use `spec()` to retrieve the full column specification for this data.
## ℹ Specify the column types or set `show_col_types = FALSE` to quiet this message.
After importing these two .csv files, I can see that the number of columns is different for both. In order to combine both of these datasets, we need make sure the data types and names of the columns match up. This is to make sure the data is consistent and concise. Here I compared the column names for both .csv files.

##  [1] "trip_id"           "start_time"        "end_time"         
##  [4] "bikeid"            "tripduration"      "from_station_id"  
##  [7] "from_station_name" "to_station_id"     "to_station_name"  
## [10] "usertype"          "gender"            "birthyear"
##  [1] "ride_id"            "rideable_type"      "started_at"        
##  [4] "ended_at"           "start_station_name" "start_station_id"  
##  [7] "end_station_name"   "end_station_id"     "start_lat"         
## [10] "start_lng"          "end_lat"            "end_lng"           
## [13] "member_casual"
Since there is quite a bit of disparity between the column names of both datasets, we will need to rename them. The 2019 dataset column names will be changed to match the 2020 dataset, since it is the more recent one.

q1_2019 <- rename(q1_2019
                   ,ride_id = trip_id
                   ,rideable_type = bikeid
                   ,started_at = start_time
                   ,ended_at = end_time
                   ,start_station_name = from_station_name
                   ,start_station_id = from_station_id
                   ,end_station_name = to_station_name
                   ,end_station_id = to_station_id
                   ,member_casual = usertype
)
Now that the column names match up, let’s analyze the data types for all the columns for both datasets.

str(q1_2019)
## spec_tbl_df [365,069 × 12] (S3: spec_tbl_df/tbl_df/tbl/data.frame)
##  $ ride_id           : num [1:365069] 21742443 21742444 21742445 21742446 21742447 ...
##  $ started_at        : POSIXct[1:365069], format: "2019-01-01 00:04:37" "2019-01-01 00:08:13" ...
##  $ ended_at          : POSIXct[1:365069], format: "2019-01-01 00:11:07" "2019-01-01 00:15:34" ...
##  $ rideable_type     : num [1:365069] 2167 4386 1524 252 1170 ...
##  $ tripduration      : num [1:365069] 390 441 829 1783 364 ...
##  $ start_station_id  : num [1:365069] 199 44 15 123 173 98 98 211 150 268 ...
##  $ start_station_name: chr [1:365069] "Wabash Ave & Grand Ave" "State St & Randolph St" "Racine Ave & 18th St" "California Ave & Milwaukee Ave" ...
##  $ end_station_id    : num [1:365069] 84 624 644 176 35 49 49 142 148 141 ...
##  $ end_station_name  : chr [1:365069] "Milwaukee Ave & Grand Ave" "Dearborn St & Van Buren St (*)" "Western Ave & Fillmore St (*)" "Clark St & Elm St" ...
##  $ member_casual     : chr [1:365069] "Subscriber" "Subscriber" "Subscriber" "Subscriber" ...
##  $ gender            : chr [1:365069] "Male" "Female" "Female" "Male" ...
##  $ birthyear         : num [1:365069] 1989 1990 1994 1993 1994 ...
##  - attr(*, "spec")=
##   .. cols(
##   ..   trip_id = col_double(),
##   ..   start_time = col_datetime(format = ""),
##   ..   end_time = col_datetime(format = ""),
##   ..   bikeid = col_double(),
##   ..   tripduration = col_number(),
##   ..   from_station_id = col_double(),
##   ..   from_station_name = col_character(),
##   ..   to_station_id = col_double(),
##   ..   to_station_name = col_character(),
##   ..   usertype = col_character(),
##   ..   gender = col_character(),
##   ..   birthyear = col_double()
##   .. )
##  - attr(*, "problems")=<externalptr>
str(q1_2020)
## spec_tbl_df [426,887 × 13] (S3: spec_tbl_df/tbl_df/tbl/data.frame)
##  $ ride_id           : chr [1:426887] "EACB19130B0CDA4A" "8FED874C809DC021" "789F3C21E472CA96" "C9A388DAC6ABF313" ...
##  $ rideable_type     : chr [1:426887] "docked_bike" "docked_bike" "docked_bike" "docked_bike" ...
##  $ started_at        : POSIXct[1:426887], format: "2020-01-21 20:06:59" "2020-01-30 14:22:39" ...
##  $ ended_at          : POSIXct[1:426887], format: "2020-01-21 20:14:30" "2020-01-30 14:26:22" ...
##  $ start_station_name: chr [1:426887] "Western Ave & Leland Ave" "Clark St & Montrose Ave" "Broadway & Belmont Ave" "Clark St & Randolph St" ...
##  $ start_station_id  : num [1:426887] 239 234 296 51 66 212 96 96 212 38 ...
##  $ end_station_name  : chr [1:426887] "Clark St & Leland Ave" "Southport Ave & Irving Park Rd" "Wilton Ave & Belmont Ave" "Fairbanks Ct & Grand Ave" ...
##  $ end_station_id    : num [1:426887] 326 318 117 24 212 96 212 212 96 100 ...
##  $ start_lat         : num [1:426887] 42 42 41.9 41.9 41.9 ...
##  $ start_lng         : num [1:426887] -87.7 -87.7 -87.6 -87.6 -87.6 ...
##  $ end_lat           : num [1:426887] 42 42 41.9 41.9 41.9 ...
##  $ end_lng           : num [1:426887] -87.7 -87.7 -87.7 -87.6 -87.6 ...
##  $ member_casual     : chr [1:426887] "member" "member" "member" "member" ...
##  - attr(*, "spec")=
##   .. cols(
##   ..   ride_id = col_character(),
##   ..   rideable_type = col_character(),
##   ..   started_at = col_datetime(format = ""),
##   ..   ended_at = col_datetime(format = ""),
##   ..   start_station_name = col_character(),
##   ..   start_station_id = col_double(),
##   ..   end_station_name = col_character(),
##   ..   end_station_id = col_double(),
##   ..   start_lat = col_double(),
##   ..   start_lng = col_double(),
##   ..   end_lat = col_double(),
##   ..   end_lng = col_double(),
##   ..   member_casual = col_character()
##   .. )
##  - attr(*, "problems")=<externalptr>
In the 2019 dataset it was found that the ride_id and rideable_type is stored as a double, while in the 2020 dataset, it is stored as a character. To make sure the datasets stack correctly, both column data types will be changed to a character type. Both datasets will now be combined. Since 2020 includes latitude and longitude data, while 2019 does not, we will omit these columns. Gender, birth year, and trip duration are also only included in the 2019 dataset, these will be omitted from the final combined dataset.

q1_2019 <-  mutate(q1_2019, ride_id = as.character(ride_id)
                   ,rideable_type = as.character(rideable_type)) 

all_rides <- bind_rows(q1_2019, q1_2020)

all_rides <- all_rides %>%  
  select(-c(start_lat, start_lng, end_lat, end_lng, birthyear, gender,  "tripduration"))


head(all_rides)
## # A tibble: 6 × 9
##   ride_id  started_at          ended_at            rideable_type start_station_id
##   <chr>    <dttm>              <dttm>              <chr>                    <dbl>
## 1 21742443 2019-01-01 00:04:37 2019-01-01 00:11:07 2167                       199
## 2 21742444 2019-01-01 00:08:13 2019-01-01 00:15:34 4386                        44
## 3 21742445 2019-01-01 00:13:23 2019-01-01 00:27:12 1524                        15
## 4 21742446 2019-01-01 00:13:45 2019-01-01 00:43:28 252                        123
## 5 21742447 2019-01-01 00:14:52 2019-01-01 00:20:56 1170                       173
## 6 21742448 2019-01-01 00:15:33 2019-01-01 00:19:09 2437                        98
## # … with 4 more variables: start_station_name <chr>, end_station_id <dbl>,
## #   end_station_name <chr>, member_casual <chr>
While all the column data types match and the .csv files were successfully merged. In the member_casual column, there are two names for members and two names for casual users. Here, we can show this with the table() function.

table(all_rides$member_casual)
## 
##     casual   Customer     member Subscriber 
##      48480      23163     378407     341906
The all_rides dataset will need to be mutated to show only two names for the member_casual column.

all_rides <-  all_rides %>% 
  mutate(member_casual = recode(member_casual
                                ,"Subscriber" = "member"
                                ,"Customer" = "casual"))

table(all_rides$member_casual)
## 
## casual member 
##  71643 720313
Since the trip duration had to be removed due to it only being in the 2020 Q1 dataset, I added a column to the combined dataset. I believe that the trip duration column will be the most important for insights on how annual members and casual riders use Cyclistic bikes differently. In addition to that, a column for the day, month, and year of each ride will be added to allow for better aggregation of data. The as.Date() function will be used to create the new rows for the days, months, and weekdays.

For the trip duration column, the difftime() function will be used, with the unit being minutes.

## Adding columns for dates
all_rides$date <- as.Date(all_rides$started_at)
all_rides$month <- format(as.Date(all_rides$date), "%b")
all_rides$day <- format(as.Date(all_rides$date), "%d")
all_rides$year <- format(as.Date(all_rides$date), "%Y")
all_rides$day_of_week <- format(as.Date(all_rides$date), "%A")

## Adding ride length column, rounding to two decimals for simplicity
all_rides$ride_length <- round(difftime(all_rides$ended_at,all_rides$started_at, units = ("mins")), 2)

all_rides$ride_length <- as.numeric(as.character(all_rides$ride_length))

str(all_rides)
## tibble [791,956 × 15] (S3: tbl_df/tbl/data.frame)
##  $ ride_id           : chr [1:791956] "21742443" "21742444" "21742445" "21742446" ...
##  $ started_at        : POSIXct[1:791956], format: "2019-01-01 00:04:37" "2019-01-01 00:08:13" ...
##  $ ended_at          : POSIXct[1:791956], format: "2019-01-01 00:11:07" "2019-01-01 00:15:34" ...
##  $ rideable_type     : chr [1:791956] "2167" "4386" "1524" "252" ...
##  $ start_station_id  : num [1:791956] 199 44 15 123 173 98 98 211 150 268 ...
##  $ start_station_name: chr [1:791956] "Wabash Ave & Grand Ave" "State St & Randolph St" "Racine Ave & 18th St" "California Ave & Milwaukee Ave" ...
##  $ end_station_id    : num [1:791956] 84 624 644 176 35 49 49 142 148 141 ...
##  $ end_station_name  : chr [1:791956] "Milwaukee Ave & Grand Ave" "Dearborn St & Van Buren St (*)" "Western Ave & Fillmore St (*)" "Clark St & Elm St" ...
##  $ member_casual     : chr [1:791956] "member" "member" "member" "member" ...
##  $ date              : Date[1:791956], format: "2019-01-01" "2019-01-01" ...
##  $ month             : chr [1:791956] "Jan" "Jan" "Jan" "Jan" ...
##  $ day               : chr [1:791956] "01" "01" "01" "01" ...
##  $ year              : chr [1:791956] "2019" "2019" "2019" "2019" ...
##  $ day_of_week       : chr [1:791956] "Tuesday" "Tuesday" "Tuesday" "Tuesday" ...
##  $ ride_length       : num [1:791956] 6.5 7.35 13.82 29.72 6.07 ...
Now let’s get rid of some bad data. There are quite a few entries where bikes were used by Cyclistic for quality testing purposes. These rows will need to be omitted from the .csv as they do not pertain to casual or member bike riders. The station name coinciding with these quality checks is “HQ QR”. I will also filter out any bike rides that are less than 5 minutes, as that data would not provide much insight.

all_rides <- all_rides %>% 
  filter(start_station_name != "HQ QR")

all_rides <- all_rides %>% 
  filter(ride_length > 5)
Descriptive Analysis
Using the summary() and aggregate() functions, I was able to gain some insight on ride lengths based on member type, day of week, and other variables.

summary(all_rides$ride_length)
##      Min.   1st Qu.    Median      Mean   3rd Qu.      Max. 
##      5.02      7.55     10.97     24.12     17.63 177200.37
Aggregated based on the average ride length for both rider types.

aggregate(all_rides$ride_length ~ all_rides$member_casual, FUN = mean)
##   all_rides$member_casual all_rides$ride_length
## 1                  casual              92.84174
## 2                  member              16.07071
Aggregated based on the median ride length for both rider types.

aggregate(all_rides$ride_length ~ all_rides$member_casual, FUN = median)
##   all_rides$member_casual all_rides$ride_length
## 1                  casual                 23.95
## 2                  member                 10.35
Aggregated based on maximum ride length for both rider types.

aggregate(all_rides$ride_length ~ all_rides$member_casual, FUN = max)
##   all_rides$member_casual all_rides$ride_length
## 1                  casual              177200.4
## 2                  member              101607.1
Aggregated based on minimum ride length for both rider types.

aggregate(all_rides$ride_length ~ all_rides$member_casual, FUN = min)
##   all_rides$member_casual all_rides$ride_length
## 1                  casual                  5.02
## 2                  member                  5.02
Aggregated the average bike ride length based on the day of the week for both casual and member riders

all_rides$day_of_week <- ordered(all_rides$day_of_week, levels=c("Sunday", "Monday", "Tuesday", "Wednesday", "Thursday", "Friday", "Saturday"))

aggregate(all_rides$ride_length ~ all_rides$member_casual + all_rides$day_of_week, FUN = mean)
##    all_rides$member_casual all_rides$day_of_week all_rides$ride_length
## 1                   casual                Sunday              86.28539
## 2                   member                Sunday              19.74710
## 3                   casual                Monday              83.39414
## 4                   member                Monday              16.65150
## 5                   casual               Tuesday              80.02957
## 6                   member               Tuesday              15.48003
## 7                   casual             Wednesday              77.67607
## 8                   member             Wednesday              14.25502
## 9                   casual              Thursday             148.28570
## 10                  member              Thursday              14.18257
## 11                  casual                Friday             106.12076
## 12                  member                Friday              16.22794
## 13                  casual              Saturday              84.65716
## 14                  member              Saturday              19.90735
Overall, the average bike ride length is 24.12 minutes, which accounts for both rider types. However, that changes when accounting for days of the week, and the rider types. Surprisingly, the casual bike riders have a much higher average ride length of 92 minutes, compared to the 16 minutes from member bike riders.

Next, I visualized the mode for each weekday for both members and casual riders.

plot1 <- all_rides %>% 
  mutate(weekday = wday(started_at, label = TRUE)) %>% 
  group_by(member_casual, weekday) %>% 
  summarise(number_of_rides = n()
            ,average_duration = mean(ride_length)) %>% 
  arrange(member_casual, weekday)  %>% 
  ggplot(aes(x = weekday, y = number_of_rides, fill = member_casual)) +
  geom_col(position = "dodge") +
  ggtitle("Number of Bike Rides per Weekday") +
  geom_text(aes(label = number_of_rides), position = position_dodge(width=0.9),  size=3, vjust= -.5) +
  labs(x = "Day of the week", y = "Number of Bike Rides", fill = "Rider Type") +
  scale_color_discrete(labels = c('Casual', 'Member'))+
  theme_minimal()
## `summarise()` has grouped output by 'member_casual'. You can override using the
## `.groups` argument.
plot1

![image](https://github.com/user-attachments/assets/330cc615-6f9c-4700-a0f9-051b5663b303)



This graph shows that member riders use Cyclistic’s bikes much more during the weekday than the weekend. The opposite is true for casual riders, with the most rides happening during the weekend.

Next, a plot on the average duration of each ride based on weekday and member status.

plot2 <- all_rides %>% 
  mutate(weekday = wday(started_at, label = TRUE)) %>% 
  group_by(member_casual, weekday) %>% 
  summarise(number_of_rides = n()
            ,average_duration = round(mean(ride_length), 2)) %>% 
  arrange(member_casual, weekday)  %>% 
  ggplot(aes(x = weekday, y = average_duration, fill = member_casual)) +
  geom_col(position = "dodge") +
  geom_text(aes(label = average_duration), position = position_dodge(width=0.9),  size=2.7, vjust = -.5) +
  ggtitle("Average Duration of Bike Rides per Weekday") +
  labs(x = "Day of the Week", y = "Average Duration of Bike Ride (minutes)")
## `summarise()` has grouped output by 'member_casual'. You can override using the
## `.groups` argument.
plot2


This plot shows that the average duration of each bike ride is much greater for casual riders compared to member riders. Members have a steady bike ride duration throughout the week, with slight increases during the weekend.

Now let’s make a plot with the same variables, however, I decided to use the tis package to filter out any dates that are not holidays. Only federally recognized holidays are recognized with this package. Since we are only looking at dates during Q1, the only applicable holidays are MLK Jr. Day, President’s Day, and New Years Day.

I also created a dataset that had only regular business days to compare the differences in metrics.

holidays <- all_rides %>% 
  filter(isHoliday(all_rides$date))

bizDays <- all_rides %>% 
  filter(!isHoliday(all_rides$date))
plot3 <- holidays %>% 
  group_by(member_casual, date) %>% 
  summarise(number_of_rides = n()
            ,average_duration = mean(ride_length)) %>% 
  arrange(member_casual, date)  %>% 
  ggplot(aes(x = factor(date), y = number_of_rides, fill = member_casual)) +
  geom_col(position = "dodge") +
  ggtitle("Number of Bike Rides per Holiday") +
  geom_text(aes(label = number_of_rides), position = position_dodge(width=0.9),  size=2.7, vjust = -.5) +
  labs(x = "Holiday Dates", y = "Number of bike rides") +
  theme_minimal()
## `summarise()` has grouped output by 'member_casual'. You can override using the
## `.groups` argument.
plot3


This plot shows that for federal holidays, member users are much more likely to use the bikes. There seems to be a much higher amount of rides for President’s Day compared to MLK day and New Year’s Day.

Now let’s check the average ride duration for these federal holidays.

plot4 <- holidays %>% 
  group_by(member_casual, date) %>% 
  summarise(number_of_rides = n()
            ,average_duration = round(mean(ride_length), 2)) %>% 
  arrange(member_casual, date)  %>% 
  ggplot(aes(x = factor(date), y = average_duration, fill = member_casual)) +
  geom_col(position = "dodge") +
  geom_text(aes(label = average_duration), position = position_dodge(width=0.9),  size=2.7, vjust= -.5) +
  ggtitle("Average Duration of Bike Rides per Holiday") +
  labs(x = "Holiday Dates", y = "Average Duration of Bike Ride (minutes)")
## `summarise()` has grouped output by 'member_casual'. You can override using the
## `.groups` argument.
plot4


Looking at this graph, we can see that casual riders usually more likely to have longer bike rides on federal holidays. However, it appears to be inconsistent. There was a large outlier with MLK day on 2020, with an average of 766 minutes, which equates to about 12 hours. Now let’s use this same graph, however, instead of the holidays as the x-axis, the weekdays of these holidays will be used.

plot5 <- holidays %>% 
  mutate(weekday = wday(started_at, label = TRUE)) %>% 
  group_by(member_casual, weekday) %>% 
  summarise(number_of_rides = n()
            ,average_duration = round(mean(ride_length), 2)) %>% 
  arrange(member_casual, weekday)  %>% 
  ggplot(aes(x = weekday, y = average_duration, fill = member_casual)) +
  geom_col(position = "dodge") +
  geom_text(aes(label = average_duration), position = position_dodge(width=0.9),  size=2.7, vjust = -.5) +
  ggtitle("Average Duration of Bike Rides per Weekday on Holidays") +
  labs(x = "Days of the week for Holidays", y = "Average Duration of Bike Ride (minutes)")
## `summarise()` has grouped output by 'member_casual'. You can override using the
## `.groups` argument.
plot5


This graph is a little more insightful. We can see that casual members are using bikes for much longer than member riders on holidays regardless of the day of the week. In addition to that, there is a downward trend in trip duration if the holiday is later in the week. For member riders, the ride duration stays almost exactly the same. During Q1 for both years, the only holidays that occur on a Monday are MLK Jr. Day and President’s Day.

Now let’s make a graph of the opposite, where we only look at business days.

## `summarise()` has grouped output by 'member_casual'. You can override using the
## `.groups` argument.


Share
There are a few things to conclude from this case study.

Casual bike riders are much more likely to use bike during the weekend, compared to the member bike riders, whose number of rides decreased during the weekends. (Plot 1)

While member riders rode bikes more frequently, when it came to average duration, casual riders came out on top regardless of the day of the week. (Plot 2)

Looking into the behavior for bikers during federal holidays, not much changes from the two previous findings. Members rode bikes more frequently and casual riders rode the bikes for longer durations. One insight I found, was that was a stark difference in the average duration of bike rides for rider types on holidays that were on a Monday compared to Mondays that were a regular business day. (Plot 3-5)

On business days we can see that the average duration disparity is about the same if we were to include federal holidays. However, I was able to notice that the duration of bike rides on Thursdays for casual riders was distinctly greater compared to members. I am not sure why this is the case, but Cyclistic could focus on some sort of discount or deal on Thursdays to appeal to these casual bike riders. (Plot 6)

Recommendations
Based on the information I have found, I would offer a view recommendations for Cyclistic.

There is a potential to maximize memberships by marketing towards casual riders during holiday weekends. This can done with discounts on initial membership fees for a limited time during these three day weekends.

Since, on average, casual bike riders are on rides for much longer than existing members. I would recommend some type of incentive for casual riders if they were to bike for a certain amount of time. For example, if a casual rider were to sign up for a holiday weekend membership, and bike for a certain amount of time. Cyclistic could provide a discount if the same person were to sign up at a later date.

Member riders are using Cyclistic bikes more frequently than casual riders, regardless of the day of the week. I think that some type of referral program, where current members get discounts for referring a new member, could motivate more casual members to join our membership program.
