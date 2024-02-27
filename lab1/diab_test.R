library(tidyverse)

diabetes_data <- read_csv("../diabetes_012_health_indicators_BRFSS2015.csv")

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

category_variables <- c("GenHlth", "Sex", "Age", 
                        "Education", "Income")

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
  rename(Diabetes = Diabetes_012) %>%
  mutate(GenHlth = gen.hlth_categories[GenHlth]) %>%
  mutate(Sex = sex_categories[Sex+1]) %>%
  mutate(Age = age_categories[Age]) %>%
  mutate(Education = edc_categories[Education]) %>%
  mutate(Income = income_categories[Income]) %>%
  mutate(
    across(all_of(binary_variables), as.logical),
    across(all_of(category_variables), factor)
  ) 

str(diabetes_data, give.attr = FALSE)

write_csv(diabetes_data, "test.csv")


