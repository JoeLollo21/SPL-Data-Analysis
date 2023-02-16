## James Baldwin's presence in public library checkouts:
# Install relevant packages -- once per machine.
#install.packages("tidyverse")
#install.packages("plotly")

# Load libraries -- once per script.
library("tidyverse")
library("stopwords") # Doesn't work without quotes
library("ggplot2")
library("plotly")
library("stringr")

# Load the James Baldwin checkout data as a data frame.
# Note: this data was cleaned to make all of them say the same Creator and 
#make the Title easier to analyze. This was all done with OpenRefine.
spl_df <- read.csv("https://raw.githubusercontent.com/ChessPiece21/Data-is-Culture/main/data/Checkouts-By-Title-Refined.csv", stringsAsFactors = FALSE)

# Visualization 1: Material Type Checkouts Over Time
# Group the data by material type and year, create new data frame from it.
spl_materials <- spl_df %>% group_by(MaterialType, CheckoutYear) %>% summarize(TotalCheckouts = sum(Checkouts))

# Graph the checkouts over time by material type.
material_plot <- ggplot(spl_materials) + geom_line(aes(x = CheckoutYear, y = TotalCheckouts, color = MaterialType)) + 
  geom_point(aes(x = CheckoutYear, y = TotalCheckouts, color = MaterialType)) +
  labs(x = "Checkout Year", y = "Checkouts", title= "Checkouts of James Baldwin's Work at SPL, 2005-2022")

ggplotly(material_plot)

# Visualization 2: Top Title Checkouts
# Group the data by (general) title, create new data frame from it.
spl_gen_titles <- spl_df %>% group_by(Gen.Title) %>%
  summarize(TotalCheckouts = sum(Checkouts))

# Graph each title's total checkouts.
title_plot <- ggplot(spl_gen_titles) + geom_col(aes(x = TotalCheckouts, y = reorder(Gen.Title, +TotalCheckouts)), fill = "#FF0000") +
  labs(x = "Checkouts", y = "", title = "Number of Checkouts by Title, 2005-2022")

ggplotly(title_plot)

# Visualization 3: Subjects
# Create new data frame of just the subjects.
spl_subjects <- spl_df %>% separate_rows(Subjects, sep = ",")

# Group subjects.
subject_group <- spl_subjects %>% group_by(Subjects) %>%
  summarize(count = n())

# Create stopwords. Add blank to stopwords.
en_stopwords <- data.frame(word = stopwords(language = "en", source = "snowball"))
en_stopwords <- en_stopwords %>% add_row(word = "")

# Remove stopwords from top subjects.
top_subjects <- subject_group %>% filter(Subjects != "") %>% filter(count > 250)

# Graph the top subjects.
subject_plot <- ggplot(top_subjects) + geom_col(aes(x = count, y = reorder(Subjects, -count)), fill = "#FF0000") +
  labs(x = "Count", y = "Subjects", title = "Most Common Library Subject Headings for James Baldwin's Work)")

ggplotly(subject_plot)

# Idea: Look at top subject headings over time

# Visualization 4: Top Checkouts and Trends - Time Series Analysis
# Filter for the top 6 titles and create new data frame.
spl_titles <- spl_df %>% group_by(Gen.Title, CheckoutYear) %>% summarize(TotalCheckouts = sum(Checkouts))

spl_top_titles <- spl_titles %>% filter(TotalCheckouts > 200)
# This one took lots of trial and error. There is probably a MUCH better way
#to do it.

# Make visualization of the top titles.
top_titles_plot <- ggplot(spl_top_titles) + geom_line(aes(x = CheckoutYear, y = TotalCheckouts, color = Gen.Title)) +
  labs(x = "Checkout Year", y = "Checkouts", title = "Top Baldwin Titles Checked Out at SPL")

ggplotly(top_titles_plot)

# See what titles are checked out the most.
# (just some thoughts for now)
# What surprises me?
## All books have a significant decrease in 2022 except Giovanni's Room, might be
##the content being relevant to the state of the world or some specific cultures?

# Significance of "The Fire Next Time"?
## Quoted significantly in BLM Tweets, Twitter activists might be making readers
##interested in checking it out.

# Search for big spikes or surprising directions.
## BIG spike in If Beale Street Could Talk checkouts in 2019 when the movie 
##came out.
## Surprising increase in Giovanni's Room in 2021-2022.
