## Making line graphs over time.

# Import all relevant libraries
#install.packages("tidyverse") # Uncomment this (delete the first hashtag) to install, if your machine hasn't yet.
#install.packages("lubridate") # ^ same here
#install.packages("tidytext") # ^^ same here
library("lubridate")
library(tidytext)
library(ggplot2)
library(dplyr)

# Data will be drawn from Trump Twitter Archive v2
trump_data <- read.csv("trump-tweets.csv", stringsAsFactors = FALSE)

# Split the text column into one word per row 
# Save as trump_words
trump_words <- trump_data %>% unnest_tokens(word, text, token = "ngrams", n = 1)

# Use lubridate to simplify the dates
trump_words <- trump_words %>% mutate(date = ymd_hms(date))

# Extract just the year from the "date" column and make a new column "year"
# Save as trump_words
trump_words <- trump_words %>% mutate(year = year(date))

# Filter for a few different words. I'm specifically interested in him talking about Obama and his two opponents in 2016 and 2020.
trump_says_1 <- trump_words %>% filter(word == "obama") %>%
  group_by(year) %>% summarize(count = n())

trump_says_2 <- trump_words %>% filter(word == "biden") %>%
  group_by(year) %>% summarize(count = n())

trump_says_3 <- trump_words %>% filter(word == "hillary") %>% # I considered "clinton" but this might be better because it could only represent her rather than Bill.
  group_by(year) %>% summarize(count = n())

# Make individual line plots of word frequency over time
ggplot(trump_says_1) +
  geom_line(mapping = aes(x = year, y = count, color = "#0000FF")) +
  geom_point(mapping = aes(x = year, y = count, fill = "#000000"))
  labs(x = "Year", y = "Frequency", title = "Is Trump Stuck in the Past? His Usage of \"Obama\" Over Time on Twitter") 

ggplot(trump_says_2) +
  geom_line(mapping = aes(x = year, y = count, color = "#FF0000")) +
  geom_point(mapping = aes(x = year, y = count, fill = "#000000")) +
  labs(x = "Year", y = "Frequency", title = "Sleepy Joes, Order Up! Trump's Usage of \"Biden\" Over Time on Twitter") 

ggplot(trump_says_3) +
  geom_line(mapping = aes(x = year, y = count, color = "#00FF00")) +
  geom_point(mapping = aes(x = year, y = count, fill = "#000000")) +
  labs(x = "Year", y = "Frequency", title = "Is Trump Stuck in the Past? His Usage of \"Hillary\" Over Time on Twitter") 
