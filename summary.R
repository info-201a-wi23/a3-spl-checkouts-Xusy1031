# Load the packages
library("dplyr")
library("tidyverse")
library("stringr")

# Load the dataset
df <- read.csv("~/Desktop/2022-2023-All-Checkouts-SPL-Data.csv", stringsAsFactors = FALSE)

# What is the month with the most checkouts for ebooks from 2022 to 2023?
most_ebooks_month <- df %>% 
  filter(MaterialType == "EBOOK") %>% 
  group_by(CheckoutMonth) %>% 
  summarize(most = max(Checkouts)) %>% 
  filter(most == max(most)) %>% 
  pull(CheckoutMonth)

# What is the average number of checkouts for each usage class each month from 2022 to 2023?
avg_checkouts <- df %>% 
  group_by(UsageClass) %>% 
  summarize(avg = (sum(Checkouts) / 13)) %>% 
  pull(avg)
avg_checkouts <- round(avg_checkouts, digits = 0)

# What is the month with the most checkouts for “Mrs. Dalloway"?
most_dalloway <- df %>% 
  filter(Title == "Mrs. Dalloway") %>% 
  filter(Checkouts == max(Checkouts)) %>% 
  pull(CheckoutMonth)

# What is the most frequent material type from 2022 to 2023?
most_frequent_material_df <- df %>% 
  group_by(MaterialType) %>% 
  count() %>% 
  arrange(-n)
most_frequent_material <- as.vector(t(most_frequent_material_df[1, ]))
most_frequent_material

# Which item has the highest number of checkouts in January 2023?
highest_item <- df %>% 
  filter(CheckoutYear == "2023") %>% 
  filter(CheckoutMonth == "1") %>% 
  filter(Checkouts == max(Checkouts)) %>% 
  pull(Title)
  
