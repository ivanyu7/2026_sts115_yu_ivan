# Data Ingest

# Load libraries
library(tidyverse)
library(tidyr)
rail = read_csv("Railroad_Equipment_Accident_Incident_Source_Data_(Form_54)_20260224.csv")

head(rail)
tail(rail)
summary(rail)
names(rail)

# Statistics


# Rail time
rail$time = paste(rail$TIMEHR, rail$AMPM)
time_order = c("12 PM","1 PM","2 PM","3 PM","4 PM","5 PM","6 PM","7 PM","8 PM","9 PM","10 PM","11 PM",
  "12 AM","1 AM","2 AM","3 AM","4 AM","5 AM","6 AM","7 AM","8 AM","9 AM","10 AM","11 AM")
rail$time = factor(rail$time, levels = time_order)
table(rail$time)
# Weather
rail$weather = factor(rail$WEATHER,
  levels = c(1,2,3,4,5,6),
  labels = c("Clear","Cloudy","Rain","Fog","Sleet","Snow"))
table(rail$weather)
# Visbility
rail$visibility = factor(rail$VISIBLTY,
  levels = c(1,2,3,4),
  labels = c("Dawn","Day","Dusk","Dark"))
table(rail$visibility)
# Season
rail$season = factor(rail$MONTH,
  levels = 1:12,
  labels = c("Winter","Winter","Spring","Spring","Spring",
    "Summer","Summer","Summer","Fall","Fall","Fall","Winter"))
table(rail$season)
# Engineer Impairment
rail$impairment = ifelse(
  rail$ALCOHOL > 0 | rail$DRUG > 0, "Impaired", "Not Impaired")
table(rail$impairment)

# Narration relevant information
narr = pivot_longer(
  rail,
  cols = NARR1:NARR15,
  names_to = "section",
  values_to = "narration"
)
narr = narr[!is.na(narr$narration) & narr$narration != "", ]
head(narr$narration)

# Visualizations

library(ggplot2)
# Figure 1
ggplot(rail) +
  aes(x = time) + 
  geom_bar() +
  coord_flip() +
  labs(title = "Railway Accidents by Time of Day",
    x = "Time",
    y = "Number of Accidents")
ggsave("time.png")

# Figure 4
ggplot(rail) +
  aes(x = weather) +
  geom_bar() +
  labs(title = "Railway Accidents by Weather Condition",
    x = "Weather",
    y = "Number of Accidents")
ggsave("weather.png")
# Figure 3
ggplot(rail) +
  aes(x = visibility) +
  geom_bar() +
  labs(title = "Railway Accidents by Visibility Condition",
    x = "Visibility",
    y = "Number of Accidents")
ggsave("visibility.png")
# Figure 5
ggplot(rail) +
  aes(x = season) +
  geom_bar() +
  labs(title = "Railway Accidents by Season",
    x = "Season",
    y = "Number of Accidents")
ggsave("season.png")
# Figure 2
ggplot(rail) + 
  aes(x = impairment) +
  geom_bar() +
  labs(title = "Railway Accidents and Engineer Impairment 
based on Alcohol or Drugs",
    x = "Engineer Impairment",
    y = "Number of Accidents")
ggsave("impairment.png")