library(dplyr)
# Read CSV file
bears = read.csv("kids-book-animals.csv")

# Summary of the dataset (Question 1)
# Dimensions and structure of dataset
dim(bears)
summary(bears)
names(bears)
str(bears)

# Distribution of average review scores (Question 2)
summary(bears$avg_ratings)

# Distribution of number of reviews (Question 3)
summary(bears$num_reviews)

# Popular animals sorted by frequency table (Question 5)
pop_animal = table(bears$animal_group)
head(sort(pop_animal, decreasing = TRUE), 3)

# Least Popular Animals sorted by frequency table (Question 5)
leastpop_animal = table(bears$animal_group)
tail(sort(leastpop_animal, decreasing = TRUE), 3)

# Popular animals sorted by ratings (Question 5)
anipop = summarize(
  group_by(bears, animal_group),
  ratings = sum(num_ratings))

head(arrange(anipop, desc(ratings), animal_group), 3)
  
# Least Popular animals sorted by ratings (Question 5)
anipop1 = summarize(
  group_by(bears, animal_group),
  ratings = sum(num_ratings))

tail(arrange(anipop1, desc(ratings), animal_group), 3)

# Publishers favorite animal (Question 6)
pub_animals = summarize(
  group_by(bears, publisher, animal_group),
  n = n())

arrange(pub_animals, desc(n))

# Average rating among pronoun of animals (Question 8)
gen_animal = filter(bears, pronoun == "he/him")
avg = mean(gen_animal$avg_ratings)
avg

gen_animal1 = filter(bears, pronoun == "she/her")
avg1 = mean(gen_animal1$avg_ratings)
avg1

gen_animal2 = filter(bears, pronoun == "it")
avg2 = mean(gen_animal2$avg_ratings)
avg2

# Total number of ratings based on decade category (Question 8)
pre1950s = filter(bears, decade_category == "Pre-1950")
num_pre1950s = sum(pre1950s$num_ratings)
num_pre1950s

x1960s = filter(bears, decade_category == "1960s")
num_1960s = sum(x1960s$num_ratings)
num_1960s

x1980s = filter(bears, decade_category == "1980s")
num_1980s = sum(x1980s$num_ratings)
num_1980s