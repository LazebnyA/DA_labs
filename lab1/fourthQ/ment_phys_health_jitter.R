library(tidyverse)
library(RColorBrewer)

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


#Scatter Plot
data_summary <- diabetes_data %>%
  group_by(MentHlth, PhysHlth) %>%
  summarise(Count = n())

ggplot(data_summary, aes(x = PhysHlth, y = MentHlth, size = Count)) +
  geom_jitter() +
  labs(title = "Dependence of Mental Health on Physical Health",
       x = "Physical Health",
       y = "Mental Health") +
  scale_size_continuous(range = c(1, 10)) +
  theme_minimal()

# #Heatmap
# 
# color_palette <- brewer.pal(9, "YlOrRd")
# 
# data_summary <- diabetes_data %>%
#   group_by(MentHlth, PhysHlth) %>%
#   summarise(Count = n())
# 
# max_count <- max(data_summary$Count)
# 
# ggplot(data_summary, aes(x = PhysHlth, y = MentHlth, fill = Count)) +
#   geom_tile() +
#   labs(title = "Heatmap of Mental Health vs Physical Health", 
#        x = "Physical Health", 
#        y = "Mental Health", 
#        fill = "Count") +
#   scale_fill_gradientn(colours = color_palette,
#                        limits = c(0, max_count), 
#                        breaks = seq(0, max_count, length.out = 2)) +
#   theme_minimal() +
#   theme(panel.grid.major = element_blank(),
#         panel.grid.minor = element_blank())


ggsave("lab1/fourthQ/img/ment_phys_health_jitter.jpg", plot = last_plot(), width = 8, height = 6, dpi = 300)



