# Load the packages
library("dplyr")
library("tidyverse")
library("stringr")

# Load the dataset
df <- read.csv("~/Desktop/2022-2023-All-Checkouts-SPL-Data.csv", stringsAsFactors = FALSE)

# What is the month with the most checkouts for ebooks from 2022 to 2023?
df_most_ebooks_month <- df %>% 
  filter(MaterialType == "EBOOK") %>% 
  mutate(Date = paste0(CheckoutYear, "-", CheckoutMonth, "-01"))
df_most_ebooks_month$Date <- as.Date(df_most_ebooks_month$Date, format = "%Y-%m-%d")
ebook_month <- df_most_ebooks_month %>% 
  group_by(Date) %>% 
  summarize(most = max(Checkouts)) %>% 
  filter(most == max(most)) %>% 
  pull(Date)
  
# What is the average number of checkouts for each usage class each month from 2022 to 2023?
avg_checkouts <- df %>% 
  group_by(UsageClass) %>% 
  summarize(avg = (sum(Checkouts) / 13)) %>% 
  pull(avg)
avg_checkouts <- c(round(avg_checkouts, digits = 0))
digital <- avg_checkouts[1]
physical <- avg_checkouts[2]

# What is the month with the most checkouts for “Mrs. Dalloway"?
df_dalloway <- df %>% 
  filter(Title == "Mrs. Dalloway") %>% 
  mutate(Date = paste0(CheckoutYear, "-", CheckoutMonth, "-01"))
df_dalloway$Date <- as.Date(df_dalloway$Date, format = "%Y-%m-%d")
dalloway_month <- df_dalloway %>% 
  filter(Checkouts == max(Checkouts)) %>% 
  pull(Date)

# What is the most frequent material type from 2022 to 2023?
most_frequent_material_df <- df %>% 
  group_by(MaterialType) %>% 
  count() %>% 
  arrange(-n)
vector_most_frequent_material <- as.vector(t(most_frequent_material_df[1, ]))
most_frequent_material <- vector_most_frequent_material[1]
material_number <- vector_most_frequent_material[2]

# Which item has the highest number of checkouts in January 2023?
highest_item <- df %>% 
  filter(CheckoutYear == "2023") %>% 
  filter(CheckoutMonth == "1") %>% 
  filter(Checkouts == max(Checkouts)) %>% 
  pull(Title)
  
