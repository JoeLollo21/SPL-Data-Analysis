## James Baldwin's presence in public library checkouts:
# Install relevant packages -- once per machine.
#install.packages("tidyverse")
#install.packages("plotly")

# Load libraries -- once per script.
library("tidyverse")
library("ggplot2")
library("plotly")
library("stringr")

# Load the James Baldwin checkout data as a data frame.
# Note: this data was cleaned to make all of them say the same Creator and 
#make the Title easier to analyze. This was all done with OpenRefine.
spl_df <- read.csv("https://raw.githubusercontent.com/ChessPiece21/Data-is-Culture/main/Baldwin-Checkouts-Refined.csv", stringsAsFactors = FALSE)

# Visualization 1: Material Type Checkouts Over Time
# Group the data by material type and year, create new data frame from it.
spl_group_1 <- spl_df %>% group_by(MaterialType, CheckoutYear) %>% summarize(Count = n())

# Graph the checkouts over time by material type.
material_plot <- ggplot(spl_group_1) + geom_line(aes(x = CheckoutYear, y = Count, color = MaterialType)) + 
  geom_point(aes(x = CheckoutYear, y = Count, color = MaterialType)) +
  labs(x = "Checkout Year", y = "Checkouts", title= "Checkouts of James Baldwin's Work at SPL, 2018-2022")

ggplotly(material_plot)

# Visualization 2: Top Title Checkouts
# Group the data by year, create new data frame from it.
spl_group_2 <- spl_df %>% group_by(Title, CheckoutYear) %>%
  summarize(TotalCheckouts = sum(Checkouts))

# Graph each material's checkouts.
title_plot <- ggplot(spl_group_2) + geom_col(aes(x = TotalCheckouts, y = Title, fill = "#FF0000")) +
  labs(x = "Checkouts", y = "", title = "Number of Checkouts by Title")

ggplotly(title_plot)

# More coming soon.
