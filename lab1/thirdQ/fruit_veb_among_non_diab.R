library(tidyverse)

diabetes_data <- read_csv("test.csv")

nutrition_factor <- c(rep("fruits", 2), rep("vegetables", 2))
response <- rep(c("Yes", "No"), 2)

fruits_data <- diabetes_data %>%
  filter(Diabetes == FALSE) %>%
  group_by(Fruits) %>%
  summarise(count = n())

veggies_data <- diabetes_data %>%
  filter(Diabetes == FALSE) %>%
  group_by(Veggies) %>%
  summarise(count = n())


value <- c(count_fruits_true <- fruits_data %>%
             filter(Fruits == TRUE) %>%
             pull(count),
           count_fruits_false <- fruits_data %>%
             filter(Fruits == FALSE) %>%
             pull(count),
           count_veggies_true <- veggies_data %>%
             filter(Veggies == TRUE) %>%
             pull(count),
           count_veggies_true <- veggies_data %>%
             filter(Veggies == FALSE) %>%
             pull(count))

data <- data.frame(nutrition_factor,response,value)

ggplot(data, aes(fill=response, y=value, x=nutrition_factor)) + 
  geom_bar(position="dodge", stat="identity") +
  ggtitle("Consume Fruit, Vegetables 1 or more times per day? (among non-diabetics)")

ggsave("thirdQ/img/fruit_veg_among_non_diab.png", plot = last_plot(), width = 8, height = 6, dpi = 300)

