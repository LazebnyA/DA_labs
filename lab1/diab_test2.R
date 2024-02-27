diabetes_data <- read_csv("test.csv")



age_categories <- c("18-24", "25-29", "30-34", "35-39",
                    "40-44", "45-49", "50-54", "55-59",
                    "60-64", "65-69", "70-74", "75-79",
                    "80+")



diabetes_data <- diabetes_data %>%
  mutate(Age = age_categories[Age])


str(diabetes_data, give.attr = FALSE)
