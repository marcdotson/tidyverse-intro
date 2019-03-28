# R and Data Manipulation -------------------------------------------------
# Load the Tidyverse
library(tidyverse)

# Importing Data
store_data <- read_csv("store_data.csv")

# Data Frames
store_data

# Filter Observations
store_data %>% 
  filter(gender == "Female")

store_data %>% 
  filter(store_spend > 100)

store_data %>% 
  filter(gender == "Female", store_spend > 100)

# Arrange Observations
store_data %>% 
  arrange(store_trans)

store_data %>% 
  arrange(desc(store_trans))

# Selecting Variables
store_data %>% 
  select(store_spend, age, gender)

# Mutating Variables
store_data %>% 
  mutate(store_spend = store_spend / 100)

# Joining Data Frames
sat_data <- read_csv("sat_data.csv")

crm_data <- store_data %>% 
  left_join(sat_data, by = "id")

crm_data

# Data Description and Visualization --------------------------------------
# Describing Discrete Data
crm_data %>% 
  count(gender)

crm_data %>% 
  count(gender, country)

# Visualizing Discrete Data
ggplot(crm_data, aes(x = gender)) +
  geom_bar()

ggplot(crm_data, aes(x = gender, fill = country)) +
  geom_bar()

ggplot(crm_data, aes(x = gender, fill = country)) +
  geom_bar(position = "fill")

# Describing Continuous Data
crm_data %>% 
  summarize(avg_store_spend = mean(store_spend))

crm_data %>% 
  summarize(
    avg_store_spend = mean(store_spend),
    avg_sat_overall = mean(sat_overall)
  )

# Visualizing Continuous Data
ggplot(crm_data, aes(x = store_spend)) +
  geom_histogram()

ggplot(crm_data, aes(x = store_spend)) +
  geom_histogram(bins = 10)

crm_data %>% 
  ggplot(aes(x = store_spend, y = sat_overall)) +
  geom_point()

ggplot(crm_data, aes(x = store_spend, y = sat_overall)) +
  geom_point(size = 2, alpha = 0.5) +
  geom_smooth(method = "lm", se = FALSE)

# Describing Discrete and Continuous Data
crm_data %>% 
  group_by(gender, country) %>% 
  summarize(
    n = n(),
    avg_store_spend = mean(store_spend),
    avg_sat_overall = mean(sat_overall)
  ) %>% 
  arrange(desc(avg_store_spend))

crm_data %>% 
  group_by(gender) %>% 
  summarize(n = n())

# Visualizing Discrete and Continuous Data
crm_data %>%   
  ggplot(aes(x = gender, y = sat_overall)) +
  geom_boxplot()

crm_data %>%   
  ggplot(aes(x = sat_overall, fill = gender)) +
  geom_density(alpha = 0.5)

crm_data %>% 
  ggplot(aes(
    x = log(store_spend + 1), 
    y = sat_overall,
    color = gender
  )) +
  geom_jitter(size = 2, alpha = 0.5) +
  geom_smooth(method = "lm", se = FALSE) +
  facet_wrap(~ country) +
  ggtitle("Store Spend by Overall Satisfaction")

# Cleaning Data and Summarization -----------------------------------------
# Gather and Spread Columns
online_data <- read_csv("online_data.csv")

online_data <- online_data %>% 
  gather(
    key = week,
    value = visits,
    week1_visit:week4_visit
  )

online_data

online_data %>% 
  spread(key = week, value = visits)

# Separate and Unite Columns
online_data <- online_data %>% 
  separate(year_mo, c("year", "month"))

online_data

online_data %>% 
  unite(year_mo, year, month)

# Wrangling <--> Summarization
online_data %>% 
  group_by(id) %>% 
  summarize(total_visits = sum(visits)) %>% 
  left_join(sat_data, by = "id") %>% 
  ggplot(aes(x = total_visits, y = sat_overall, color = country)) +
  geom_point(size = 2, alpha = 0.5) + 
  geom_smooth(method = "lm", se = FALSE)

online_data %>% 
  group_by(id) %>% 
  summarize(total_visits = sum(visits)) %>% 
  left_join(sat_data, by = "id") %>% 
  ggplot(aes(x = total_visits, y = sat_overall)) +
  geom_point(size = 2, alpha = 0.5, aes(color = country)) + 
  geom_smooth(method = "lm", se = FALSE)

