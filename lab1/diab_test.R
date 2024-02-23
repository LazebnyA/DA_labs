library(tidyverse)

diabetes_data <- read_csv("diabetes_012_health_indicators_BRFSS2015.csv")

binary_variables <- c(
  "Diabetes_012",
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

category_variables <- c(
  "GenHlth",
  "Sex",
  "Age",
  "Education",
  "Income"
)



diabetes_data <- diabetes_data %>%
  mutate(
    across(all_of(binary_variables), as.logical),
    across(all_of(category_variables), factor)
  )


str(diabetes_data, give.attr = FALSE)

write_csv(diabetes_data, "lab1/test.csv")


