data("rock")

library(tidyverse)
library(ggimage)

image = sample(c("https://user-images.githubusercontent.com/12670730/113008567-6ebdcb80-9177-11eb-91bd-6863196d9cd3.png"))

rock %>% 
  ggplot(aes(x = shape, y = area, fill = perm)) +
  geom_point() +
  geom_image(aes(image = image), size = 0.3) +
  ggtitle(label = "rocks") + 
  theme(text = element_text(size = 30, family = "serif", angle = 15, color = "cyan3"))+
  theme(axis.title.x = element_text(angle = 300),
        plot.background = element_rect(fill = "pink"),
        panel.background = element_rect(fill = "red"),
        axis.text = element_text(angle = 170, vjust = 1, hjust = -2),
        axis.title.y = element_text(angle = 90, color = "green"))
  


  


