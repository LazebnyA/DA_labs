library(tidyverse)
library(ggridges)
library(egg)

data <- read.csv("prepared_data.csv")

nums <- c(
  "Less than $10,000"=1,
  "$10,000-$15,000"=2,
  "$15,000-$20,000"=3,
  "$20,000-$25,000"=4,
  "$25,000-$35,000"=5,
  "$35,000-$50,000"=6,
  "$50,000-$75,000"=7,
  "$75,000 or more"=8
)
labs <- c("Less than $10,000", "$10,000-$15,000", "$15,000-$20,000", "$20,000-$25,000", "$25,000-$35,000", "$35,000-$50,000", "$50,000-$75,000", "$75,000 or more")

p <- ggplot(data,aes(x=nums[Income], y=Education, fill = AnyHealthcare),rotate = 90) +
  geom_density_ridges(stat="binline", bins=8, alpha = 0.5, position = "dodge") +
  labs(x="Дохід", y="Рівень освіти", subtitle="Залежність розподілу доходів від рівня освіти (мед. обстеження)", caption="(a) - немає діабету, (b) - діабет") +
  theme(axis.title = element_text(size=14),
        axis.text.x = element_text(size=14, angle = 90),
        axis.text.y = element_text(size=14),
        plot.subtitle = element_text(size = 14)) +
  facet_wrap(.~Diabetes)
(tag_facet(p + scale_x_continuous(breaks=c(1,2,3,4,5,6,7,8),
                     labels=labs)))