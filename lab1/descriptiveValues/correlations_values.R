library(tidyverse)
library(vcd)

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

# Функция для вычисления коэффициентов корреляции Cramer's V и Contingency Coefficient для пары переменных
compute_correlation <- function(var1, var2) {
  # Создаем таблицу сопряженности
  contingency_table <- table(diabetes_data[[var1]], diabetes_data[[var2]])
  # Вычисляем коэффициент корреляции Cramer's V
  cramer_v <- assocstats(contingency_table)$cramer
  # Вычисляем коэффициент корреляции Contingency Coefficient
  contingency_coef <- assocstats(contingency_table)$contingency
  # Создаем датафрейм с результатами
  data.frame(
    var1 = var1,
    var2 = var2,
    Cramers_V = cramer_v,
    Contingency_Coef = contingency_coef
  )
}

# Сгенерируем все возможные комбинации пар переменных
all_pairs <- combn(names(diabetes_data), 2, simplify = FALSE)

# Применим функцию compute_correlation к каждой паре переменных
correlation_results <- map_dfr(all_pairs, ~compute_correlation(.x[1], .x[2]))

# Удалим строки с NA значениями
correlation_results <- na.omit(correlation_results)

# Визуализируем результаты с помощью хитмапов
contingency_heatmap <- ggplot(correlation_results, aes(x = var1, y = var2, fill = Contingency_Coef)) +
  geom_tile() +
  scale_fill_gradient(low = "white", high = "steelblue") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 90, hjust = 1, size = 5),
        axis.text.y = element_text(size = 5)) +
  labs(title = "Contingency Coefficient Heatmap", x = "Variable 1", y = "Variable 2")

cramersv_heatmap <- ggplot(correlation_results, aes(x = var1, y = var2, fill = Cramers_V)) +
  geom_tile() +
  scale_fill_gradient(low = "white", high = "steelblue") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 90, hjust = 1)) +
  labs(title = "Cramer's V Heatmap", x = "Variable 1", y = "Variable 2")

# Выведем хитмапы
print(contingency_heatmap)
print(cramersv_heatmap)

ggsave("lab1/descriptiveValues/img/contingency_heatmap.jpg", plot = contingency_heatmap, dpi = 300)
ggsave("lab1/descriptiveValues/img/cramersv_heatmap.jpg", plot = cramersv_heatmap, dpi = 300)