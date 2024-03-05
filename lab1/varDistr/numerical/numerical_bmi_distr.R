library(tidyverse)

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


diabetes_data <- read_csv("prepared_data.csv") %>%
  mutate(Sex = factor(Sex)) %>%
  mutate(GenHlth = factor(GenHlth, ordered = TRUE, levels = rev(gen.hlth_categories))) %>%
  mutate(Sex = factor(Sex)) %>%
  mutate(Age = factor(Age, ordered = TRUE, levels = age_categories)) %>%
  mutate(Education = factor(Education, ordered = TRUE, levels = edc_categories)) %>%
  mutate(Income = factor(Income, ordered = TRUE, levels = income_categories)) %>%
  mutate(across(all_of(binary_variables), as.logical))



num_intervals <- ceiling(1 + log2(length(diabetes_data$BMI)))
min_bmi <- min(diabetes_data$BMI, na.rm = TRUE)
max_bmi <- max(diabetes_data$BMI, na.rm = TRUE)
interval_width <- (max_bmi - min_bmi) / num_intervals

ggplot(diabetes_data, aes(x = BMI)) +
  geom_histogram(binwidth = interval_width, fill = "skyblue", color = "black") +
  geom_density(aes(y = ..count.. * interval_width), fill = "skyblue", color = "black", alpha = 0.3, bw = 1) +
  scale_x_continuous(breaks = seq(0, max_bmi, by = 9), limits = c(0, max_bmi)) +  
  scale_y_continuous(labels = scales::comma) +
  labs(title = "BMI Distribution",
       x = "BMI",
       y = "Frequency / Density") +
  theme_minimal()


ggsave("lab1/varDistr/img/numerical_BMI_distr.jpg", plot = last_plot(), width = 8, height = 6, dpi = 300)

