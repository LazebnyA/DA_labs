library(tidyverse)

# Read the data
diabetes_data <- read_csv("test.csv")

# Calculate counts of diabetic and non-diabetic individuals
count_diab <- diabetes_data %>%
  filter(Diabetes == TRUE) %>%
  nrow()

count_not_diab <- diabetes_data %>%
  filter(Diabetes == FALSE) %>%
  nrow()

# Calculate percentages
total_count <- count_diab + count_not_diab
percent_diab <- (count_diab / total_count) * 100
percent_not_diab <- (count_not_diab / total_count) * 100

# Create a data frame
data <- data.frame(
  Category = c("Diabetic", "Non-Diabetic"),
  Count = c(count_diab, count_not_diab),
  Percent = c(percent_diab, percent_not_diab)
)

# Plot pie chart with percentage labels
ggplot(data, aes(x = "", y = Count, fill = Category)) +
  geom_bar(stat = "identity", width = 1) +
  geom_text(aes(label = paste0(round(Percent), "%")), 
            position = position_stack(vjust = 0.5)) +
  coord_polar("y", start = 0) +
  labs(title = "Distribution of Diabetic Status", fill = "Category", x = NULL, y = NULL) +
  theme_void()


ggsave("img/diab_among_respondents.jpeg", plot = last_plot(), width = 8, height = 6, dpi = 300)