# Read dataset and load libraries
# Data Ingest
library(tidyverse)
library(ggplot2)
library(readxl)
billboard <- read_excel("~/Downloads/Billboard Hot 100 Number Ones Database.xlsx", sheet = "Data")

head(billboard)
str(billboard)


# Statistics
summary(billboard)
mean(billboard$`Overall Rating`)
date = as_date(billboard$Date)
year = year(billboard$Date)

label = count(billboard, `Parent Label`)
labels = arrange(label, desc(n))
top10labels = head(labels, 10)

origin = count(billboard, `Artist Place of Origin`)
origins = arrange(origin, desc(n))
top10origins = head(origins, 10)


# Visualizations

# Histogram of top 10 labels by songs
ggplot(top10labels) +
  aes(reorder(x = `Parent Label`, n), y = n) +
  geom_col() +
  scale_y_continuous(breaks = seq(0, 200, by = 10)) +
  labs(title = "Top 10 Parent Labels",
       x = "Parent Label",
       y = "Number of Songs")

ggsave("top10labels.png")

# Dot plot between the relationships of the Date and the Overall Ratings
ggplot(billboard) +
  aes(x = Date, y = `Overall Rating`) +
  geom_point(color = "blue") +
  labs(title = "Relationship between Date and Overall Rating")

ggsave("overall.png")

# Artist place of origina and their number of songs (top 10) 
ggplot(top10origins) +
  aes(reorder(x = `Artist Place of Origin`, n), y = n) + 
  geom_col() +
  labs(title = "Artist Place of Origin and Number of Songs",
       x = "Country of Origin", y = "Count")

ggsave("origin.png")

