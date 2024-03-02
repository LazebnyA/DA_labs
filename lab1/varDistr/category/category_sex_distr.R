library(tidyverse)

diabetes_data <- read_csv("prepared_data.csv") %>%
  mutate(Sex = factor(Sex)) %>%
  mutate(GenHlth = factor(GenHlth, ordered = TRUE, levels = rev(gen.hlth_categories))) %>%
  mutate(
    across(all_of(binary_variables), as.logical),
    across(all_of(category_variables), ~ factor(.x, ordered = TRUE))
  )

str(diabetes_data, give.attr = FALSE)