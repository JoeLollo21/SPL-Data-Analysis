# Import all relevant libraries
library(tidyverse)
library("stopwords") # Doesn't work without quotes
library(tidytext)
library(ggplot2)
library(dplyr)
library(plotly)

baldwin_data <- read.csv("https://raw.githubusercontent.com/ChessPiece21/Data-is-Culture/main/data/baldwin-tweets.csv", stringsAsFactors = FALSE)

# Split text into ngrams, bigrams, trigrams
baldwin_ngrams <- baldwin_data %>% unnest_tokens(word, text, token = "ngrams", n = 1)

# Count how often each ngram appears, save as ngram_count, do the same for bigrams and trigrams
ngram_count <- baldwin_ngrams %>% count(word, sort = TRUE) %>% rename(total = n)

# Make stopwords data frame and add custom words that would've been common in lots of tweets (rt, james, baldwin)
en_stopwords <- data.frame(word = stopwords(language = "en", source = "snowball"))
en_stopwords <- en_stopwords %>% add_row(word = c("said", "can", "rt", "james", "baldwin", "http", "t.co", "retweet", "mikebrown", "jamesbaldwin", "freddiegray", "rahielt", "blacklivesmatter", "baldwin's", "blm", "michael", "brown")) # These were gathered from looking at the top ngram data

# Remove stopwords from the ngram data frame
ngram_count_no_stops <- ngram_count %>% anti_join(en_stopwords)

# Filter for top 50 ngrams
top_ngrams <- ngram_count_no_stops %>% slice_max(order_by = total, n = 50)

# Make horizontal bar plot of the top ngrams, re-ordered from most to least frequent
# Made interactive
ngram_plot <- ggplot(top_ngrams) +
  geom_col(aes(y = reorder(word, total), x = total)) +
  labs(x = "Word Frequency", y = "Word", title = "Most Used Words in Tweets Quoting James Baldwin, 2014-15")

ggplotly(ngram_plot)
