library(tidyverse)

diabetes_data <- read_csv("test.csv")

nutrition_factor <- c(rep("fruits", 2), rep("vegetables", 2))
diabetes <- rep(c("Yes", "No"), 2)

fruits_data <- diabetes_data %>%
  filter(Fruits == TRUE) %>%
  group_by(Diabetes) %>%
  summarise(count = n())

veggies_data <- diabetes_data %>%
  filter(Veggies == TRUE) %>%
  group_by(Diabetes) %>%
  summarise(count = n())


value <- c(count_fruits_true <- fruits_data %>%
             filter(Diabetes == TRUE) %>%
             pull(count),
           count_fruits_false <- fruits_data %>%
             filter(Diabetes == FALSE) %>%
             pull(count),
           count_veggies_true <- veggies_data %>%
             filter(Diabetes == TRUE) %>%
             pull(count),
           count_veggies_false <- veggies_data %>%
             filter(Diabetes == FALSE) %>%
             pull(count))

data <- data.frame(nutrition_factor,diabetes,value)

ggplot(data, aes(fill=diabetes, y=value, x=nutrition_factor)) + 
  geom_bar(position="dodge", stat="identity") +
  ggtitle("Number of diabetics among fruit and vegitables daily consumers")

#ggsave("thirdQ/img/diab_among_fruit_veg.png", plot = last_plot(), width = 8, height = 6, dpi = 300)

