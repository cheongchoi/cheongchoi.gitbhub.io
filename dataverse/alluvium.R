library(haven)
library(dplyr)
library(ggalluvial)

plotdata<- readRDS("plotdata.RDS")

plot <- ggplot(plotdata, 
               aes(y = n, axis1 = empst01, axis2 = empst25)) + 
  geom_alluvium(aes(fill=empst01)) +
  geom_stratum(width = .25, fill = "light grey") +
  geom_text(stat = "stratum", aes(label = after_stat(stratum))) +
  scale_x_discrete(limits = c("97년 종사상지위", "22년 종사상지위"), expand = c(.05, .05)) +
  theme_classic() + 
  scale_fill_brewer(palette='RdYlBu') + 
  theme(axis.title.y=element_blank(),
        legend.position='none',
        text = element_text(size=20))

plot

ggsave("figure_1.pdf", width=11, height=8)
