# Load the packages
library("dplyr")
library("tidyverse")
library("ggplot2")
library("stringr")
library('scales')

# Load the dataset
df <- read.csv("~/Desktop/2022-2023-All-Checkouts-SPL-Data.csv", stringsAsFactors = FALSE)

# Total checkouts per month
filtered_df <- df %>% 
  filter(CheckoutYear == "2022") %>% 
  group_by(CheckoutMonth) %>% 
  mutate(Date = paste0(CheckoutYear, "-", CheckoutMonth, "-01"))
filtered_df$Date <- as.Date(filtered_df$Date, format = "%Y-%m-%d")

total_per_month <- filtered_df %>% 
  group_by(Date, UsageClass) %>% 
  summarize(total_each_month = sum(Checkouts))

ggplot(total_per_month, aes(x = Date, y = total_each_month, 
                          fill = UsageClass, label = total_each_month)) +
  geom_bar(stat = "identity") + geom_text(
    size = 3, position = position_stack(vjust = 0.5), angle = 90, color = "white") +
  scale_x_date(date_labels = "%b", date_breaks = "1 month") +
  scale_y_continuous(labels = comma) +
  labs(title = "Total Checkouts Per Month in 2022",
       x = "Month",
       y = "Total Checkouts",
       fill = "Usage Class")
  
