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

only_diabets <- diabetes_data %>% filter(Diabetes == TRUE)


plot_for_save <- only_diabets %>%
  group_by(Age) %>%
  summarise(MentHlthAver = mean(MentHlth),
            PhysHlthAver = mean(PhysHlth)) %>%
  ggplot(aes(x = MentHlthAver, y = PhysHlthAver, color = Age)) + 
  geom_point() +
  labs(title = "Scatter plot of Physical Health vs Mental Health with age-based colors")

plot_for_save

ggsave("lab1/sixthQ/img/phys_health_ment_age_grouped_scatter.jpg", plot = plot_for_save, width = 8, height = 6, dpi = 300)

# Можна побачити, що чим старше людина, тим гірше ментальне здоров'я (і те починаючи з 55 років)
# Інші залежності не видно, дослідимо глибше