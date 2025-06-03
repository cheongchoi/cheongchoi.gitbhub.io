rm(list = ls()) #clear workspace

library(usmap)
library(ggplot2)

civil.rights <-readRDS("civil.rights")
civil.rights$fips <- as.factor(civil.rights$fips)

plot_usmap(regions = "counties", data=civil.rights, values = "protest") + 
  labs(title = "Figure 1. US Counties",
       subtitle = "The Geographic Distribution of Civil Rights Protests, 1960–65.") + 
  scale_fill_manual(
    values = c(`No Protest` = "white", `Protest` = "red", 'NA'="black"),
    name = "treatment"
  ) +
  theme(panel.background = element_rect(color = "white", fill = "lightblue")) +
  theme(legend.position = "right")