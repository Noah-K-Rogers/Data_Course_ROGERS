library(tidyverse)
library(palmerpenguins)
library(ggimage)


penguins %>% names()
penguins %>% 
  ggplot(aes(x=bill_length_mm+bill_depth_mm,color = species))+
  geom_bar()+
  facet_wrap(reorder(penguins$bill_depth_mm, -penguins$bill_length_mm), scales = "free")+
  labs(x="Bill Length", y="Bill depth")+
  theme_minimal()+
  theme(axis.title.x = element_text(face = "bold",
                                    colour = "hotpink",
                                    size = 20,
                                    hjust = 0.5,
                                    angle = 30,
                                    vjust = 2),
        axis.title.y  = element_text(face = "bold",
                                    colour = "yellow",
                                    size = 40,
                                    hjust = 0.5,
                                    angle = -210,
                                    vjust = 0),
        legend.title = element_text(face = "bold",
                                    colour = "lightgreen",
                                    size = 20,
                                    hjust = 1,
                                    angle = 180,
                                    vjust = 0),
        legend.background = element_rect(fill = "green",
                                         color = "limegreen"),
        legend.text = element_text(colour = "limegreen"),
        axis.text.x = element_text(colour = "yellow", angle = 180),
        axis.text.y = element_text(color = "orange"),
        plot.background = element_rect(fill ="red"),
        
        
        )



ggsave("Noah_uglyplot.png")
