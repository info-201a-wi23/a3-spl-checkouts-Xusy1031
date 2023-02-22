# Load the packages
library("dplyr")
library("tidyverse")
library("ggplot2")
library("stringr")

# Load the dataset
df <- read.csv("~/Desktop/2022-2023-All-Checkouts-SPL-Data.csv", stringsAsFactors = FALSE)

# The Years vs Mrs. Dalloway
new_df <- df %>% 
  filter(Title %in% c("The Years", "Mrs. Dalloway")) %>% 
  select(-c(ISBN, Subjects, Publisher, PublicationYear)) %>% 
  mutate(Date = paste0(CheckoutYear, "-", CheckoutMonth, "-01"))
new_df$Date <- as.Date(new_df$Date, format = "%Y-%m-%d")

checkouts_per_month <- new_df %>% 
  group_by(Date, Title) %>% 
  summarize(sum = sum(Checkouts, na.rm = TRUE))

ggplot(data = checkouts_per_month, aes(x = Date)) +
  geom_line(aes(y = sum, color = Title)) +
  labs(title = "Checkouts of Two Books",
       x = "Month",
       y = "Total Checkouts",
       color = "Book Title")

