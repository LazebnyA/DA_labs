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

count_by_age_chol_true <- diabetes_data %>%
  filter(CholCheck == TRUE) %>%
  group_by(Age) %>%
  summarise(count = n())

count_by_age_chol_false <- diabetes_data %>%
  filter(CholCheck == FALSE) %>%
  group_by(Age) %>%
  summarise(count = n())

combined_data <- rbind(
  count_by_age_chol_true %>% mutate(CholCheck = "TRUE"),
  count_by_age_chol_false %>% mutate(CholCheck = "FALSE")
)

combined_plot <- ggplot(combined_data, aes(x = Age, y = count, fill = CholCheck)) +
  geom_col(position = "identity", width = 0.95) +
  ggtitle("Age categories distribution") +
  xlab("Age category") +
  ylab("Count") +
  scale_y_continuous(labels = scales::comma) +
  theme(axis.text.x = element_text(angle = 90, hjust = 1, size = 8)) 

print(combined_plot)

ggsave("lab1/thirdQ/img/chol_check_age_barchart2.jpg", plot = combined_plot, width = 8, height = 6, dpi = 300)




