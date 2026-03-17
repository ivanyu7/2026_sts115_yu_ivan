library(ggplot2)
library(tidyverse)
library(dplyr)
library(tidyr)

# Load data
data_dir = "~/2026-sts115-yu-ivan/data/"
maindata = paste0(data_dir, "cts_share_data_student.csv")
data_dict = paste0(data_dir, "cts_data_dictionary_student.csv")
data = read.csv(maindata)
dict = read.csv(data_dict)

# Glimpse of data
str(data)
colSums(is.na(data))
names(data)
head(data)
summary(data)


# EDA
count(data, role_primary)

table(data$theft_victim)
# Theft data
theft_data = data |>
  filter(theft_victim != "-99") |>
  count(theft_victim) |>
  mutate(pct = round(100 * n/sum(n), 2)) 
# Gender Comfort Gap
count(data, bg_gender)
count(data, attitudes_travel...I.feel.safe.biking.on.campus.)

comfort = data |>
  filter(attitudes_travel...I.feel.safe.biking.on.campus. != "-99" | attitudes_travel...I.feel.safe.biking.on.campus. != NA) |>
  count(attitudes_travel...I.feel.safe.biking.on.campus.)

comfort_and_gender = data |>
  filter((bg_gender == "Male" | bg_gender == "Female"),
         attitudes_travel...I.feel.safe.biking.on.campus. != "-99",
         !is.na(attitudes_travel...I.feel.safe.biking.on.campus.)) |>
  count(bg_gender, attitudes_travel...I.feel.safe.biking.on.campus.)
# Housing data
housing = data |>
  filter(!is.na(housing_problems...Expensive.rent.mortgage),
         housing_problems...Expensive.rent.mortgage != "-99") |>
  count(housing_problems...Expensive.rent.mortgage)

unitrans = data |>
  filter(!is.na(attitudes_unitrans...Selected.Choice),
         attitudes_unitrans...Selected.Choice != "-99")|>
  count(attitudes_unitrans...Selected.Choice)
  
# Plots and Visualizations
ggplot(data, aes(x = role_primary)) +
  geom_bar(fill = "steelblue") +
  coord_flip() +
  labs(title = "Survey Demographics",
       x = "Level of Surveyer",
       y = "Count")
ggsave("Figure3.png")


# Bike Theft
ggplot(theft_data, aes(x = theft_victim, y = pct)) +
  geom_col(fill = "steelblue") +
  coord_flip() +
  scale_x_discrete(labels = c(
   "Yes, my entire bike was stolen" = "Entire bike stolen",
   "No, I had a bike on campus in the past year but did not experience a theft or vandalism" = "No bike stolen or vandalized",
   "Yes, but only parts of my bike were stolen (seat, wheel, accessories)" = "Parts of bike stolen",
   "My bike was vandalized (damaged but not stolen)" = "Vandalized")) +
  scale_y_continuous(breaks = seq(0, 100, by = 10)) +
  labs(title = "Bike Theft or Vandilism by Percent",
       x = "Survey Answers",
       y = "Percent of Respondents")
ggsave("Figure1.png")
# Gender
ggplot(gender, aes(x = bg_gender, y = n)) +
  geom_col(fill = "steelblue") +
  scale_y_continuous(breaks = seq(0, 3000, by = 500)) +
  labs(title = "Gender Demographics",
       x = "Gender",
       y = "Count")
ggsave("Figure2.png")
# Comfort level
ggplot(comfort_and_gender, aes(x = bg_gender, y = n, fill = attitudes_travel...I.feel.safe.biking.on.campus. )) +
  geom_col(position = "fill") +
  scale_y_continuous(labels = scales::percent) +
  labs(title = "Comfort Level of Biking on Campus",
       x = "Gender",
       y = "Percent")

ggsave("Figure4.png")
# Housing
ggplot(housing, aes(x = housing_problems...Expensive.rent.mortgage, y = n)) +
  geom_col(fill = "steelblue") +
  labs(title = "Number of respondents who said rent was expensive",
       y = "count")

ggsave("Figure5.png")

ggplot(unitrans, aes(x = attitudes_unitrans...Selected.Choice, y = n)) +
  geom_col(fill = "steelblue") +
  coord_flip() +
  labs(title = "Unitrans uncertainty",
       x = "Reason",
       y = "count")

ggsave("Figure6.png")
