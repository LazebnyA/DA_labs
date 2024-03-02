library(tidyverse)

diabetes_data <- read_csv("./test.csv")

binary_vars <- c("Fruits", "Veggies")

binary_plotting_vars <- map(binary_vars, ~rep(.x, 2)) %>% unlist()

response <- rep(c("Yes", "No"), 2)

# Function to calculate distribution for each variable
calculate_distribution <- function(variable) {
  diabetes_data %>%
    group_by(!!sym(variable)) %>%
    summarise(count = n())
}

distributions <- setNames(map(binary_vars, calculate_distribution), binary_vars)

value <- c()
for (var in binary_vars) {
  true_count <- distributions[[var]] %>%
    filter(!!sym(var) == TRUE) %>%
    pull(count)
  false_count <- distributions[[var]] %>%
    filter(!!sym(var) == FALSE) %>%
    pull(count)
  value <- c(value, true_count, false_count)
}

data <- data.frame(binary_plotting_vars, response, value)

# Plot the distribution using ggplot2
ggplot(data, aes(fill = response, y = value, x = binary_plotting_vars)) + 
  geom_bar(position = "dodge", stat = "identity") +
  ggtitle("Binary variables distribution among responders")

ggsave("lab1/varDistr/img/binary_nutrition_distr.jpg", plot = last_plot(), width = 8, height = 6, dpi = 300)
