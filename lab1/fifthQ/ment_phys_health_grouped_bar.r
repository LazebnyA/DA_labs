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


diabetes_data %>%
  mutate(
    PhysHlthCut = cut(PhysHlth, breaks = c(-1, 5, 10, 15, 20, 25, Inf),
                      labels = c("0-5", "5-10", "10-15", "15-20", "20-25", "25+"))
  )%>%
  group_by(PhysHlthCut) %>%
  summarize(MentAverHlth = mean(MentHlth), 
            n = n(),
            .groups = "drop") %>%
  drop_na() %>%
  ggplot(aes(x = PhysHlthCut, y = MentAverHlth)) +
  labs(title = "Bar plot of grouped Mental Health vs Physical Health") + 
  geom_col() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1, size=12))

ggsave("lab1/fifthQ/img/ment_phys_health_grouped_bar.jpg", plot = last_plot(), width = 8, height = 6, dpi = 300)

# Тобто залежність існує і в зворотньому напрямку