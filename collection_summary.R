#Summary and Descriptive stats for ACC Vol 2
#Now with more KnowHow™

library("tidyverse")
library("lubridate")

#this should be a function
per_day_stats <- file.path("All Comics Considered-2.csv") %>% 
  read.csv(stringsAsFactors = FALSE)

per_day_stats$metric <- ymd(per_day_stats$metric) #one year of downloads per day. Requests is downloads

year_stats <- file.path("show_daily-totals_allcomicsconsidered_2019-01-11.csv") %>%
  read.csv(stringsAsFactors = FALSE)

show_stats <- file.path("ACC_year_stats.csv") %>%
  read.csv(stringsAsFactors = FALSE)

show_stats$Released <- mdy(show_stats$Released)
glimpse(show_stats)

year(show_stats$Released)

show_stats_2018 <- show_stats %>%
  filter(year(Released) == 2018)

show_stats_2018 <- show_stats_2018 %>%
  select(-1)

by_month <- show_stats_2018  %>%
  mutate(Month = month(Released, label = TRUE)) %>%
  group_by(Month) %>%
    mutate(`Downloads by Month` = sum(Downloads),
           `Shows by Month` = n(),
           `Proportion by Month` = Downloads/`Downloads by Month`)

monthly_avg <- show_stats_2018  %>% 
  mutate(Month = month(Released, label = TRUE)) %>%
  group_by(Month) %>%
  summarise(Avg = mean(Downloads))


show_and_per_day_2018 <- show_stats_2018 %>%
  left_join(per_day_stats, by = c("Released" = "metric")) %>% 
  select(`Episode Title` = Title, Released, `Episode Downloads` = Downloads,
         `Daily Downloads` = requests) %>%
  gather(key =`Download Type`, value =  "Downloads", `Episode Downloads`, `Daily Downloads` )

episode_daily <- show_and_per_day_2018 %>%
  ggplot(aes(x = Released, y = Downloads, col = `Download Type`)) + geom_point()

quarterly_plot <- show_and_per_day_2018 %>%
ggplot(aes(x = quarter(Released), y = Downloads, fill = `Download Type`)) +
  geom_bar(stat = "identity", position = "dodge")

monthy_plot <- show_and_per_day_2018 %>%
  ggplot(aes(x = as.factor(month(Released, label = TRUE)), y = Downloads, fill = `Download Type`)) +
  geom_bar(stat = "identity", position = "dodge") + 
  labs(x = "Month")

glimpse(show_and_per_day_2018)
nrow(show_and_per_day_2018)
