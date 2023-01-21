# Import all relevant libraries
library(tidyverse)
library("stopwords") # Doesn't work without quotes
library(tidytext)
library(ggplot2)
library(dplyr)
library(plotly)

baldwin_data <- read.csv("https://raw.githubusercontent.com/ChessPiece21/Data-is-Culture/main/data/baldwin-tweets.csv", stringsAsFactors = FALSE)

# Split text into ngrams
baldwin_ngrams <- baldwin_data %>% unnest_tokens(word, text, token = "ngrams", n = 1)

# Count how often each ngram appears, save as ngram_count, do the same for bigrams and trigrams
ngram_count <- baldwin_ngrams %>% count(word, sort = TRUE) %>% rename(total = n)

# Make stopwords data frame and add custom words that would've been common in lots of tweets (rt, james, baldwin)
en_stopwords <- data.frame(word = stopwords(language = "en", source = "snowball"))
en_stopwords <- en_stopwords %>% add_row(word = c("said", "can", "rt", "james", "baldwin", "https", "http", "t.co", "retweet", "mikebrown", "jamesbaldwin", "freddiegray", "rahielt", "blacklivesmatter", "baldwin's", "blm", "michael", "ferg", "rkbaycd34s", "zellieimani", "quote", "courteroy_", "ericgarner", "standup", "walterscott", "54lfpfksuv", "fergusontheroot", "joseiswriting", "occupywallstnyc", "brown", "danteb4u")) 
# These stopwords were gathered from looking at the ngrams, and include usernames, links, and "rt"/"retweet" to normalize the data, 
#even if keeping usernames and retweets could offer interesting questions and perspectives on the status of BLM Twitter users' popularity.

# Remove stopwords from the ngram data frame
ngram_count_no_stops <- ngram_count %>% anti_join(en_stopwords)

# Filter for top 50 ngrams
top_ngrams <- ngram_count_no_stops %>% slice_max(order_by = total, n = 50)

# Make horizontal bar plot of the top ngrams, re-ordered from most to least frequent
# Made interactive with Plotly
ngram_plot <- ggplot(top_ngrams) +
  geom_col(aes(y = reorder(word, total), x = total, fill = "#FF0000")) +
  labs(x = "Frequency", y = "Word", title = "Most Used Frequent in Tweets Quoting James Baldwin, 2014-15")

ggplotly(ngram_plot)

# Do the same with the original data, only with bigrams and trigrams:
baldwin_bigrams <- baldwin_data %>% unnest_tokens(bigram, text, token = "ngrams", n = 2)

baldwin_trigrams <- baldwin_data %>% unnest_tokens(trigram, text, token = "ngrams", n = 3)

# Count how often each bigram and trigram appears, save as bigram_count and trigram_count
bigram_count <- baldwin_bigrams %>% count(bigram, sort = TRUE) %>% rename(total = n)

trigram_count <- baldwin_trigrams %>% count(trigram, sort = TRUE) %>% rename(total = n)

# Filter for top 50 bigrams and trigrams
top_bigrams <- bigram_count %>% slice_max(order_by = total, n = 50)

top_trigrams <- trigram_count %>% slice_max(order_by = total, n = 50)

# Visualize the top bigrams and trigrams in the same way that the ngrams were:
bigram_plot <- ggplot(top_bigrams) +
  geom_col(aes(y = reorder(bigram, total), x = total, fill = "#FF0000")) +
  labs(x = "Frequency", y = "Bigram", title = "Most Frequent Bigrams in Tweets Quoting James Baldwin, 2014-15")

ggplotly(bigram_plot)

trigram_plot <- ggplot(top_trigrams) +
  geom_col(aes(y = reorder(trigram, total), x = total, fill = "#FF0000")) +
  labs(x = "Frequency", y = "Trigram", title = "Most Frequent Trigrams in Tweets Quoting James Baldwin, 2014-15")

ggplotly(trigram_plot)
