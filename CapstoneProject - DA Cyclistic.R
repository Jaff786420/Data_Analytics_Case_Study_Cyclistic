#installing packages and loading them
install.packages("tidyverse")
install.packages("janitor")
install.packages("scales")

library(tidyverse)
library(janitor)
library(scales)

#loading the CSV files into Data Frames and saving all in one Data Frame
df1 <- read_csv("Capstone Project - DA Cyclistic/202101-divvy-tripdata.csv")
df2 <- read_csv("Capstone Project - DA Cyclistic/202102-divvy-tripdata.csv")
df3 <- read_csv("Capstone Project - DA Cyclistic/202103-divvy-tripdata.csv")
df4 <- read_csv("Capstone Project - DA Cyclistic/202104-divvy-tripdata.csv")
df5 <- read_csv("Capstone Project - DA Cyclistic/202105-divvy-tripdata.csv")
df6 <- read_csv("Capstone Project - DA Cyclistic/202106-divvy-tripdata.csv")
df7 <- read_csv("Capstone Project - DA Cyclistic/202107-divvy-tripdata.csv")
df8 <- read_csv("Capstone Project - DA Cyclistic/202108-divvy-tripdata.csv")
df9 <- read_csv("Capstone Project - DA Cyclistic/202109-divvy-tripdata.csv")
df10 <- read_csv("Capstone Project - DA Cyclistic/202110-divvy-tripdata.csv")
df11 <- read_csv("Capstone Project - DA Cyclistic/202111-divvy-tripdata.csv")
df12 <- read_csv("Capstone Project - DA Cyclistic/202112-divvy-tripdata.csv")

cyclistic_df <- rbind(df1, df2, df3, df4, df5, df6, df7, df8, df9, df10, df11, df12)

#removing empty rows/columns
cyclistic_df_rem_empty <- remove_empty(cyclistic_df, which = c("rows","cols"))
count(cyclistic_df)
count(cyclistic_df_rem_empty)
count(filter(cyclistic_df_rem_empty, start_station_name==''),start_station_name, member_casual,sort=TRUE)

#count to check the number of unique variables
cyclistic_df %>% 
  count(rideable_type)

#removing data containing NA
cyclistic_df_rem_empty <- na.omit(cyclistic_df)
count(cyclistic_df_rem_empty)

#removing duplicated data
cyclistic_df_no_dup <- cyclistic_df_rem_empty[!duplicated(cyclistic_df_rem_empty$ride_id), ]


#Starting with Analysis

#riding_time
cyclistic_clean_df <- cyclistic_df_no_dup
cyclistic_clean_df <- cyclistic_clean_df %>% 
  mutate(riding_time = as.numeric(ended_at-started_at)/60)
cyclistic_clean_df

#year_month
cyclistic_clean_df <- cyclistic_clean_df %>% 
  mutate(year_month = paste(strftime(cyclistic_clean_df$started_at, "%Y"), "-",
                            strftime(cyclistic_clean_df$started_at, "%m"), "-",
                            strftime(cyclistic_clean_df$started_at, "%b")))
cyclistic_clean_df

#removing year '2022' from the data frame, as we are focused on 2021 data
cyclistic_clean_df <- filter(cyclistic_clean_df, year_month != "2022%")
cyclistic_clean_df

#weekday
cyclistic_clean_df <- cyclistic_clean_df %>% 
  mutate(weekday = weekdays(cyclistic_clean_df$ended_at))
cyclistic_clean_df

#start_hour
cyclistic_clean_df <- cyclistic_clean_df %>% 
  mutate(start_hour = strftime(cyclistic_clean_df$ended_at, format = "%H", tz = "UTC"))
cyclistic_clean_df


#Plotting

#comparing number of 'members' and 'casual' riders
cyclistic_vis_df <- cyclistic_clean_df

cyclistic_vis_df %>% 
  group_by(member_casual) %>% 
  summarise(count = length(ride_id),
            percentage_of_total = (length(ride_id)/nrow(cyclistic_vis_df))*100)

#plotting above results
ggplot(cyclistic_vis_df, aes(x = member_casual, fill = member_casual)) + 
  geom_bar() + 
  labs(title = "Chart - 1: Member vs Casual Riders") + 
  scale_y_continuous(labels = scales::number_format(accuracy = 0.01, decimal.mark = '.')) + 
  annotate("text",x = 1, y = 2275555, label = "Member > Casual", fontface = "bold", size = 5.0)

#percentage of casual vs annual riders per month
cyclistic_vis_df %>% 
  group_by(year_month) %>% 
  summarise(count = length(ride_id),
            percentage_of_total = (length(ride_id)/nrow(cyclistic_vis_df))*100,
            member_count = sum(member_casual == "member"),
            member_percentage = (sum(member_casual == "member")/length(ride_id))*100,
            casual_count = sum(member_casual == "casual"),
            casual_percentage = (sum(member_casual == "casual")/length(ride_id))*100) %>% 
  arrange(year_month)

#plotting above results
ggplot(cyclistic_vis_df, aes(x = year_month, fill = member_casual)) + 
  geom_bar() + 
  coord_flip() + 
  labs(title = "Chart - 2: Member vs Casual Riders - Based on Month") + 
  scale_y_continuous(labels = scales::number_format(accuracy = 0.01, decimal.mark = '.'))

#based on hours of day
cyclistic_vis_hours_df <- cyclistic_vis_df %>% 
  group_by(start_hour) %>% 
  summarise(count = length(ride_id),
            percentage_of_total = (length(ride_id)/nrow(cyclistic_vis_df))*100,
            member_count = sum(member_casual == "member"),
            member_percentage = (sum(member_casual == "member")/length(ride_id))*100,
            casual_count = sum(member_casual == "casual"),
            casual_percentage = (sum(member_casual == "casual")/length(ride_id))*100) %>% 
  arrange(start_hour)

#plotting above results
ggplot(cyclistic_vis_df, aes(x = start_hour, fill = member_casual)) + 
  geom_bar() + 
  labs(title = "Chart - 3: Member vs Casual Riders - Based on Hours") + 
  scale_y_continuous(labels = scales::number_format(accuracy = 0.01, decimal.mark = '.'))

#plotting above scenario based on each day of the week
ggplot(cyclistic_vis_df, aes(x = start_hour, fill = member_casual)) + 
  geom_bar() + 
  facet_wrap(~weekday)
  labs(title = "Chart - 4: Member vs Casual Riders - Based on Each Day of the Week") + 
  scale_y_continuous(labels = scales::number_format(accuracy = 0.01, decimal.mark = '.')) + 
  theme(axis.text.x = element_text(size=6, angle=45))
  
#sub-dividing hours based on Morning, Afternoon and Evening
cyclistic_vis_df <- mutate(cyclistic_vis_df,
                           hour_of_the_day = ifelse(cyclistic_vis_df$start_hour<12, "Morning",
                                                    ifelse(cyclistic_vis_df$start_hour>=12 & cyclistic_vis_df$start_hour<18, "Afternoon", "Evening")))

#finding the percentage based on Morning, Afternoon and Evening
cyclistic_vis_MAE_df <- cyclistic_vis_df %>% 
  group_by(hour_of_the_day) %>% 
  summarise(count = length(ride_id),
            percentage_of_total = (length(ride_id)/nrow(cyclistic_vis_df))*100,
            member_count = sum(member_casual == "member"),
            member_percentage = (sum(member_casual == "member")/length(ride_id))*100,
            casual_count = sum(member_casual == "casual"),
            casual_percentage = (sum(member_casual == "casual")/length(ride_id))*100)
cyclistic_vis_MAE_df

#plotting for above result
ggplot(cyclistic_vis_df, aes(x = hour_of_the_day, fill = member_casual)) + 
  geom_bar() + 
  labs(title = "Chart - 5: Member vs Casual Riders - Based on hours in a day") + 
  scale_y_continuous(labels = scales::number_format(accuracy = 0.01, decimal.mark = '.'))

#number of riders per week of the day
cyclistic_vis_df %>% 
  group_by(weekday) %>% 
  summarise(count = length(ride_id),
            percentage_of_total = (length(ride_id)/nrow(cyclistic_vis_df))*100,
            member_count = sum(member_casual == "member"),
            member_percentage = (sum(member_casual == "member")/length(ride_id))*100,
            casual_count = sum(member_casual == "casual"),
            casual_percentage = (sum(member_casual == "casual")/length(ride_id))*100)

#plotting for above result
ggplot(cyclistic_vis_df, aes(x = weekday, fill = member_casual)) + 
  geom_bar() + 
  labs(title = "Chart - 6: Member vs Casual Riders - Based on riders per week of day") + 
  scale_y_continuous(labels = scales::number_format(accuracy = 0.01, decimal.mark = '.'))

#types of bikes riders prefer
cyclistic_vis_df %>% 
  group_by(rideable_type) %>% 
  summarise(count = length(ride_id),
            percentage_of_total = (length(ride_id)/nrow(cyclistic_vis_df))*100,
            member_count = sum(member_casual == "member"),
            member_percentage = (sum(member_casual == "member")/length(ride_id))*100,
            casual_count = sum(member_casual == "casual"),
            casual_percentage = (sum(member_casual == "casual")/length(ride_id))*100)

#plotting for above results
ggplot(cyclistic_vis_df, aes(x = rideable_type, fill = member_casual)) + 
  geom_bar() + 
  facet_wrap(~weekday) + #check this once if needed or not
  labs(title = "Chart - 7: Member vs Casual Riders - Based on Bikes used") + 
  scale_y_continuous(labels = scales::number_format(accuracy = 0.01, decimal.mark = '.'))

#checking with the riding_time which we created
summary(cyclistic_vis_df$riding_time)

#printing values in each quantiles with 5% difference
quantiles <- quantile(cyclistic_vis_df$riding_time, seq(0,1,by = 0.05))
quantiles

#considering only (5-95)% interval
cyclistic_vis_no_outlier_df <- cyclistic_vis_df %>% 
  filter(riding_time > as.numeric(quantiles['5%'])) %>% 
  filter(riding_time < as.numeric(quantiles['95%']))

cyclistic_vis_final_df <- cyclistic_vis_no_outlier_df

#check riding_time for both members and casuals
cyclistic_vis_final_df %>% 
  group_by(member_casual) %>% 
  summarise(mean = mean(riding_time),
            first_quarter = quantile(riding_time, 0.25),
            median = median(riding_time),
            third_quarter = quantile(riding_time, 0.75),
            IQR = third_quarter-first_quarter)

#plotting for above results
ggplot(cyclistic_vis_final_df, aes(x = member_casual, y = riding_time, fill = member_casual)) + 
  geom_boxplot() + 
  labs(title = "Chart - 8: Member vs Casual Riders - Based on Riding Time")

#checking riding_time for each of the week
cyclistic_vis_final_df %>% 
  group_by(weekday) %>% 
  summarize(mean = mean(riding_time),
            first_quarter = quantile(riding_time, 0.25),
            median = median(riding_time),
            third_quarter = quantile(riding_time, 0.75),
            IQR = third_quarter-first_quarter)

#plotting for above results
ggplot(cyclistic_vis_final_df, aes(x = weekday, y = riding_time, fill = member_casual)) + 
  geom_boxplot() + 
  labs(title = "Chart - 9: Member vs Casual Riders - Based on Riding Time per week")

#checking how the above times differs on each month
cyclistic_vis_final_df %>% 
  group_by(year_month) %>% 
  summarize(mean = mean(riding_time),
            first_quarter = quantile(riding_time, 0.25),
            median = median(riding_time),
            third_quarter = quantile(riding_time, 0.75),
            IQR = third_quarter-first_quarter)

#plotting for above results
ggplot(cyclistic_vis_final_df, aes(x = year_month, y = riding_time, fill = member_casual)) + 
  geom_boxplot() + 
  labs(title = "Chart - 9: Member vs Casual Riders - Based on Riding Time per week")