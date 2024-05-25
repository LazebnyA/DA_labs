library(tidyverse)

diabetes_data <- read_csv("./diabetes_binary_health_indicators_BRFSS2015.csv")

binary_variables <- c(
  "Diabetes",
  "HighBP", 
  "HighChol", 
  "CholCheck", 
  "Smoker", 
  "Stroke", 
  "HeartDiseaseorAttack", 
  "PhysActivity", 
  "Fruits", 
  "Veggies",
  "HvyAlcoholConsump",
  "AnyHealthcare",
  "NoDocbcCost",
  "DiffWalk"
)

category_variables <- c("Age", "Education", "Income")

gen.hlth_categories <- c("excellent", "very good",
                         "good", "fair", "poor")

sex_categories <- c("female", "male")

age_categories <- c("18-24", "25-29", "30-34", "35-39",
                    "40-44", "45-49", "50-54", "55-59",
                    "60-64", "65-69", "70-74", "75-79",
                    "80+")

edc_categories <- c("No education", 
                    "Elementary", 
                    "Some high school", 
                    "High school graduate", 
                    "Some college or tech. school", 
                    "College graduate")

income_categories <- c("Less than $10,000",
                       "$10,000-$15,000",
                       "$15,000-$20,000",
                       "$20,000-$25,000",
                       "$25,000-$35,000",
                       "$35,000-$50,000",
                       "$50,000-$75,000",
                       "$75,000 or more")

diabetes_data <- diabetes_data %>%
  rename(Diabetes = Diabetes_binary) %>%
  mutate(
    GenHlth = gen.hlth_categories[GenHlth],
    GenHlth = factor(GenHlth, ordered = TRUE, levels = rev(gen.hlth_categories))
    ) %>%
  mutate(
    Sex = sex_categories[Sex+1],
    Sex = factor(Sex)
    ) %>%
  mutate(
    Age = age_categories[Age],
    Age = factor(Age, ordered = TRUE, levels = age_categories)
    ) %>%
  mutate(
    Education = edc_categories[Education],
    Education = factor(Education, ordered = TRUE, levels = edc_categories)
    ) %>%
  mutate(
    Income = income_categories[Income],
    Income = factor(Income, ordered = TRUE, levels = income_categories)
    ) %>%
  mutate(
    across(all_of(binary_variables), as.logical)
  )

str(diabetes_data, give.attr = FALSE)

write_csv(diabetes_data, "prepared_data.csv")


