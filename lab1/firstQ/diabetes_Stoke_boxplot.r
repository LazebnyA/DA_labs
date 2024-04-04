library(ggplot2)
library(dplyr)
library(egg)

data <- read.csv("prepared_data.csv")

p <- ggplot(data, aes(x=factor(Age), y=log(BMI), color=Stroke)) +
  geom_boxplot() +
  facet_wrap(Sex~Diabetes) +
  labs(x="Вік", y="ІМТ (log(кг/м^2))", subtitle="Залежність наявності діабету від віку, статі, ІМТ та пережитого інсульту", caption="(a) - жінки, здорові, (b) - жінки, хворі, (c) - чоловіки, здорові, (d) - чоловіки, хворі") +
  theme(axis.title = element_text(size=14),
        axis.text.x = element_text(size=14, angle = 90),
        axis.text.y = element_text(size=14),
        plot.subtitle = element_text(size = 14)) +
        
  geom_hline(yintercept = 2.918) +
  geom_hline(yintercept = 3.215)
tag_facet(p)