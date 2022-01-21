## Age Grouping
#Preparations
library(tidyverse)

clean_data <- read.csv(here::here("data/clean_data.csv"))

# Inspect the age variable
names(clean_data)
clean_data <- mutate(Age = 2021 - BirthYear, clean_data)

dim(clean_data)
length(clean_data$Age)

table(clean_data$Age, useNA = "always")
quantile(clean_data$Age)
quantile(clean_data$Age, probs = seq(0,1,1/3)) # With 1/3 steps

his <- ggplot(clean_data, aes(x = Age)) +
  geom_histogram()
print(his)

den <- ggplot(clean_data, aes(x=Age)) + 
  geom_density() +
  geom_vline(aes(xintercept = quantile(Age)[2])) + # Add quartiles + median
  geom_vline(aes(xintercept = quantile(Age)[3])) +
  geom_vline(aes(xintercept = quantile(Age)[4]))
print(den)

# Create the first age grouping
clean_data <- clean_data %>% mutate(Age_group = case_when(
    Age <= 30            ~ "0-30",
    Age > 30 & Age <= 45 ~ "31-45",
    Age > 45             ~ "> 45"
  ),
  Two_Age_group = case_when(
    Age <= 35 ~ "0-35",
    Age > 35 ~ "> 35"),
  # Convert to factors
  Age_group = factor(
    Age_group,
    level = c("0-30", "31-45","> 45")
  ),
  Two_Age_group = factor(
    Two_Age_group,
    level = c("0-35", "> 35")
  )
)

# Check it
head(clean_data)

table(clean_data$Age, clean_data$Age_group, useNA = "ifany")
table(clean_data$Age, clean_data$Two_Age_group, useNA = "ifany")

prop.table(table(clean_data$Age_group))
prop.table(table(clean_data$Two_Age_group)) # Worked

## Inspect further variables
#Gender
table(clean_data$Gender, useNA = "always")
class(clean_data$Gender)

clean_data <- clean_data %>% mutate(Gender = factor(Gender))

class(clean_data$Gender)
table(clean_data$Gender, useNA = "always")

table(clean_data$Gender_optional, useNA = "always") # No values at all!

# Treatment
table(clean_data$Treatment, useNA = "always")
class(clean_data$Treatment)

# Time
table(clean_data$Time, useNA = "always")
class(clean_data$Time)

his_time <- ggplot(clean_data, aes(x = Time)) +
  geom_histogram()
print(his_time)