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


# Создаем фрейм данных с всеми возможными комбинациями значений двух бинарных переменных
binary_combinations <- expand.grid(CholCheck = c(TRUE, FALSE), AnyHealthcare = c(TRUE, FALSE))

# Присоединяем фрейм данных к вашим данным
diabetes_data_with_combinations <- merge(binary_combinations, diabetes_data, by = c("CholCheck", "AnyHealthcare"), all = TRUE)

grouped_data <- diabetes_data_with_combinations %>%
  group_by(CholCheck, AnyHealthcare) %>%
  summarise(count = n())

chol_check_any_healthcare_barplot <- ggplot(grouped_data, aes(x = paste(CholCheck, AnyHealthcare), y = count, fill = paste(CholCheck, AnyHealthcare))) +
  geom_bar(stat = "identity") +
  xlab("CholCheck and AnyHealthcare") +
  ylab("Count") +
  ggtitle("Counts of CholCheck and AnyHealthcare combinations") +
  scale_y_continuous(labels = scales::comma) +
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) 

chol_check_any_healthcare_barplot

ggsave("lab1/thirdQ/img/chol_check_any_healthcare.jpg", plot = chol_check_any_healthcare_barplot, width = 8, height = 6, dpi = 300)


