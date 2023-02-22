# Load the packages
library("dplyr")
library("tidyverse")
library("ggplot2")
library("stringr")

# Load the dataset
df <- read.csv("~/Desktop/2022-2023-All-Checkouts-SPL-Data.csv", stringsAsFactors = FALSE)

# Top Five Material Types
my_df <- df %>% 
  select(-c(ISBN, Subjects, Publisher, PublicationYear, Creator, Title)) %>% 
  mutate(Date = paste0(CheckoutYear, "-", CheckoutMonth, "-01"))
my_df$Date <- as.Date(my_df$Date, format = "%Y-%m-%d")

selected_df <- my_df %>% 
  group_by(Date, MaterialType) %>% 
  summarize(total = sum(Checkouts)) %>% 
  filter(total > 10000)

ggplot(data = selected_df, aes(x = Date, y = total)) +
  geom_line(aes(color = MaterialType)) +
  labs(title = "Checkouts of Different Material Types",
       x = "Month",
       y = "Total Checkouts",
       color = "Material Types") +
  scale_y_continuous(breaks = seq(10000, 270000, 10000))

