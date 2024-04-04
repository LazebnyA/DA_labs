library(tidyverse)
library(plotly)
library(htmlwidgets)

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


plot_data <- only_diabets %>%
  group_by(Age) %>%
  summarise(MentHlthAver = mean(MentHlth),
            PhysHlthAver = mean(PhysHlth))

scatter_plot <- plot_ly(data = plot_data, x = ~MentHlthAver, y = ~PhysHlthAver, z = ~Age, color = ~Age,
                        type = "scatter3d", mode = "markers") %>%
  layout(scene = list(xaxis = list(title = "Mental Health"),
                      yaxis = list(title = "Physical Health"),
                      zaxis = list(title = "Age")),
         title = "3D Scatter plot of Physical Health vs Mental Health by Age Grouped")

saveWidget(scatter_plot, file = "lab1/sixthQ/img/3d_scatter_plot_Ment_Phys_Age_grouped.html")

scatter_plot <- plot_ly(data = only_diabets, x = ~MentHlth, y = ~PhysHlth, z = ~Age, color = ~Age,
                        type = "scatter3d", mode = "markers") %>%
  layout(scene = list(xaxis = list(title = "Mental Health"),
                      yaxis = list(title = "Physical Health"),
                      zaxis = list(title = "Age")),
         title = "3D Scatter plot of Physical Health vs Mental Health by Age")

saveWidget(scatter_plot, file = "lab1/sixthQ/img/3d_scatter_plot_Ment_Phys_Age.html")