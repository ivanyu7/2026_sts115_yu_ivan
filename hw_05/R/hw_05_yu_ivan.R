# Load libraries
library(rvest)
library(dplyr)
library(readr)
library(ggplot2)
library(httr)
library(jsonlite)

# API
response = GET("https://en.wikipedia.org/w/api.php", query = list(
  action = "parse",
  page = "List_of_female_Nobel_laureates",
  prop = "text",
  format = "json"
))
response$content
bdy = content(response, "text")
bdy_json = fromJSON(bdy)

html = read_html(bdy_json$parse$text[[1]])
table = html_table(html, fill = TRUE)
table[1]

# Find prize category
categories = c(
  "Physiology or Medicine",
  "Physics",
  "Chemistry",
  "Literature",
  "Peace",
  "Economic Sciences"
)

table[[2]]$prize_category = categories[1]
table[[3]]$prize_category = categories[2]
table[[4]]$prize_category = categories[3]
table[[5]]$prize_category = categories[4]
table[[6]]$prize_category = categories[5]
table[[7]]$prize_category = categories[6]


dat = bind_rows(table[2:7])
# Age they won the nobel 
birth_year = str_match(dat$Born, "(\\d{4})")[,2]
birth_year = as.numeric(birth_year)
winning_year = as.numeric(dat$Year)
age = winning_year - birth_year
age
# Country
country = str_match(dat$Born, ",\\s*([^,]+)$")[,2]
country


df1 = data.frame(
  name = dat$Name,
  year = dat$Year,
  age = age,
  country = country,
  category = dat$prize_category
)
csv1 = write_csv(df, "data/female_nobel_laureates_api.csv")

# Scrape the web
url = "https://en.wikipedia.org/wiki/List_of_female_Nobel_laureates"
doc = read_html(url)

tables = html_table(doc, fill = TRUE)

length(tables)

tables[1]
physiology = tables[2]
head(physiology)
physics = tables[3]
head(physics)
chemistry = tables[4]
head(chemistry)
literature = tables[5]
head(literature)
peace = tables[6]
head(peace)
economic = tables[7]
head(economic)

# Category
categories1 = c(
  "Physiology or Medicine",
  "Physics",
  "Chemistry",
  "Literature",
  "Peace",
  "Economic Sciences"
)

tables[[2]]$prize_category = categories1[1]
tables[[3]]$prize_category = categories1[2]
tables[[4]]$prize_category = categories1[3]
tables[[5]]$prize_category = categories1[4]
tables[[6]]$prize_category = categories1[5]
tables[[7]]$prize_category = categories1[6]

data = bind_rows(tables[2:7])

# Age
birth_year1 = str_match(data$Born, "(\\d{4})")[,2]
birth_year1 = as.numeric(birth_year1)
winning_year1 = as.numeric(data$Year)
age1 = winning_year1 - birth_year1
age1
# Country
country1 = str_match(data$Born, ",\\s*([^,]+)$")[,2]
country1

# Data frame
df = data.frame(
  name = data$Name,
  year = data$Year,
  age = age1,
  country = country1,
  category = data$prize_category
)
csv = write_csv(df, "data/female_nobel_laureates_scraping.csv")


# Visualizations
# Female Nobel Laureates over the years
ggplot(df1, aes(x = year)) +
  geom_histogram(binwidth = 5) +
  labs(title = "Female Nobel Laureates Over The Years",
    x = "Year",
    y = "Number of Laureates")

ggplot(df, aes(x = year)) +
  geom_histogram(binwidth = 5) +
  labs(title = "Female Nobel Laureates Over The Years",
    x = "Year",
    y = "Number of Laureates")
# Female Nobel Laureates sorted by category
ggplot(df1, aes(x = category)) +
  geom_bar(binwidth = 5) +
  labs(title = "Female Nobel Laureates Sorted By Category",
       x = "Prize Category",
       y = "Number of Laureates")
ggplot(df, aes(x = category)) +
  geom_bar(binwidth = 5) +
  labs(title = "Female Nobel Laureates Sorted By Category",
       x = "Prize Category",
       y = "Number of Laureates")

# Sorted by country
ggplot(df1, aes(x = country)) +
  geom_bar() +
  coord_flip() +
  labs(title = "Female Nobel Laureates Sorted By Category",
       x = "Country",
       y = "Number of Laureates")
ggplot(df, aes(x = country)) +
  geom_bar() +
  coord_flip() +
  labs(title = "Female Nobel Laureates Sorted By Category",
       x = "Country",
       y = "Number of Laureates")

# Sorted by age when they won their Nobel
ggplot(df1, aes(x = age)) +
  geom_bar() +
  scale_x_continuous(breaks = seq(0, 100, 5)) +
  scale_y_continuous(breaks = seq(0, 8, 1)) +
  labs(title = "Female Nobel Laureates Sorted By Age",
       x = "Age",
       y = "Number of Laureates")
ggplot(df, aes(x = age)) +
  geom_bar() +
  scale_x_continuous(breaks = seq(0, 100, 5)) +
  scale_y_continuous(breaks = seq(0, 8, 1)) +
  labs(title = "Female Nobel Laureates Sorted By Age",
       x = "Age",
       y = "Number of Laureates")
