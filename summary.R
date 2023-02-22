# Load the packages
library("dplyr")
library("tidyverse")
library("stringr")

# Load the dataset
df <- read.csv("~/Desktop/2022-2023-All-Checkouts-SPL-Data.csv", stringsAsFactors = FALSE)

# What is the month with the most checkouts for ebooks?
most_ebooks_month <- df %>% 
  filter(MaterialType == "EBOOK") %>% 
  group_by(CheckoutMonth) %>% 
  summarize(most = max(Checkouts)) %>% 
  filter(most == max(most)) %>% 
  pull(CheckoutMonth)

# What is the average number of checkouts for each usage class?
avg_checkouts <- df %>% 
  group_by(UsageClass) %>% 
  summarize(avg = mean(Checkouts)) %>% 
  pull(avg)
avg_checkouts <- round(avg_checkouts, digits = 0)

# What is the month with the most checkouts for “Mrs. Dalloway"?
most_dalloway <- df %>% 
  filter(Title == "Mrs. Dalloway") %>% 
  filter(Checkouts == max(Checkouts)) %>% 
  pull(CheckoutMonth)
