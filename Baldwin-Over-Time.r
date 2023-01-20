# Import all relevant libraries
library(tidyverse)
library("lubridate")
library(tidytext)
library(ggplot2)
library(dplyr)
library(plotly)

baldwin_data <- read.csv("https://raw.githubusercontent.com/ChessPiece21/Data-is-Culture/main/data/baldwin-tweets.csv", stringsAsFactors = FALSE)

# Split text
baldwin_split <- baldwin_data %>% unnest_tokens(word, text, token = "ngrams", n = 1)
# Check type (should be list), just to make sure
typeof(baldwin_split)

# Change date column into an actual date
baldwin_split <- baldwin_split %>% mutate(date = ymd_hms(date))

# Extract month, create new column
baldwin_months <- baldwin_split %>% mutate(month = month(date))

# Filter for specific words and count how many times each year they were mentioned
filter_words <- baldwin_months %>% filter(word == "violence") %>%
  group_by(month) %>% summarize(count = n())

# Make line plot of word frequency over time
violence_plot <- ggplot(filter_words) +
  geom_line(mapping = aes(x = month, y = count, color = "#FF0000")) +
  geom_point(mapping = aes(x = month, y = count, fill = "#000000")) +
  labs(x = "Month", y = "Count", title = "Monthly Usage of \"Violence\" in Tweets Quoting James Baldwin, 2014")  +
  scale_x_log10()

ggplotly(violence_plot)

# Another word
filter_words_2 <- baldwin_months %>% filter(word == "black") %>%
  group_by(month) %>% summarize(count = n())

# Make line plot of word frequency over time
black_plot <- ggplot(filter_words_2) +
  geom_line(mapping = aes(x = month, y = count, color = "#FF0000")) +
  geom_point(mapping = aes(x = month, y = count, fill = "#000000")) +
  labs(x = "Month", y = "Count", title = "Monthly Usage of \"Black\" in Tweets Quoting James Baldwin, 2014") 

ggplotly(black_plot)
