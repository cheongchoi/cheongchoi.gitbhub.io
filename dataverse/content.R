# The American Viewer: Political Consequences of Entertainment Media
# Eunji Kim and Shawn Patterson Jr 
# American Political Science Review 
# Replication File [Figure 4]

library(PropCIs)
library(tidyverse)
library(ggplot2)
library(reshape)

figure4 <- read.csv("figure4.csv")

# creating a dummy variable for each reason for each individual 
individual_summary <- figure4 %>%
  group_by(apprentice, Survata.Interview.ID.x, reason_value) %>%  
  drop_na(reason_value, apprentice) %>%
  dplyr::summarize(count = n())%>% 
  ungroup() %>%
  spread(key = reason_value, value = count) %>%
  select(-Survata.Interview.ID.x)

colnames(individual_summary) <- c('apprentice','economy','health_care','immigration','abortion','gun',
                                  'supreme_court','national_security','policy_misc','corruption','party',
                                  'anti_hilary','change','patriotism','background','personality','miscellaneous')

individual_summary[is.na(individual_summary)] <- 0


# collapse policy variable
individual_summary$policy <- 0
individual_summary$policy <- rowSums(individual_summary[2:10])
individual_summary$policy[individual_summary$policy > 0] <- 1


# create a variable indicating if a person listed at least one of personality or background as reason
individual_summary$either_reason <- 0
individual_summary$either_reason[individual_summary$personality + individual_summary$background > 0] <- 1

individual_collaposed <- individual_summary %>%
  select(apprentice, policy, party, anti_hilary, change, patriotism, either_reason, miscellaneous)


# find number of respondents of each apprentice viewership level
individual_n <- individual_collaposed %>%
  group_by(apprentice) %>%
  dplyr::summarise(group_n = n()) 


# sum up total number of mentions of each reason for each viewership level
individual_value <- individual_collaposed %>%
  group_by(apprentice) %>%
  dplyr::summarise_all(sum)

individual_tbl <- cbind(individual_value, individual_n) 


n_policy <- add4ci(x = individual_tbl$policy[1], n = individual_tbl$group_n[1], conf.level = 0.95)
a_policy <- add4ci(x = individual_tbl$policy[5], n = individual_tbl$group_n[5], conf.level = 0.95)

n_party <- add4ci(x = individual_tbl$party[1], n = individual_tbl$group_n[1], conf.level = 0.95)
a_party <- add4ci(x = individual_tbl$party[5], n = individual_tbl$group_n[5], conf.level = 0.95)

n_hillary <- add4ci(x = individual_tbl$anti_hilary[1], n = individual_tbl$group_n[1], conf.level = 0.95)
a_hillary <- add4ci(x = individual_tbl$anti_hilary[5], n = individual_tbl$group_n[5], conf.level = 0.95)

n_change <- add4ci(x = individual_tbl$change[1], n = individual_tbl$group_n[1], conf.level = 0.95)
a_change  <- add4ci(x = individual_tbl$change[5], n = individual_tbl$group_n[5], conf.level = 0.95)

n_patri <- add4ci(x = individual_tbl$patriotism[1], n = individual_tbl$group_n[1], conf.level = 0.95)
a_patri <- add4ci(x = individual_tbl$patriotism[5], n = individual_tbl$group_n[5], conf.level = 0.95)

n_either <- add4ci(x = individual_tbl$either_reason[1], n = individual_tbl$group_n[1], conf.level = 0.95)
a_either <- add4ci(x = individual_tbl$either_reason[5], n = individual_tbl$group_n[5], conf.level = 0.95)

n_misc <- add4ci(x = individual_tbl$miscellaneous[1], n = individual_tbl$group_n[1], conf.level = 0.95)
a_misc <- add4ci(x = individual_tbl$miscellaneous[5], n = individual_tbl$group_n[5], conf.level = 0.95)



# divide number of individuals who listed a specific reason with the total number of people in that group
individual_tbl[2:8] <- individual_tbl[2:8] / individual_tbl$group_n 
plot_tbl <- individual_tbl[1:8] 
plot_tbl <- plot_tbl[c(1,5),] # select viewership level = 1 and 5
plot_tbl$apprentice[plot_tbl$apprentice == 1] <- 'Never'
plot_tbl$apprentice[plot_tbl$apprentice == 5] <- 'Always'
colnames(plot_tbl) <- c('Apprentice','Policy','Party/Ideology','Anti-Hilary', 'Change', 'Patriotism', 'Background/\nPersonality','Miscellaneous')




plot_tbl <- melt(plot_tbl, id="Apprentice")
plot_tbl$upper <- c(n_policy$conf.int[2], a_policy$conf.int[2],
                    n_party$conf.int[2], a_party$conf.int[2],
                    n_hillary$conf.int[2], a_hillary$conf.int[2],
                    n_change$conf.int[2], a_change$conf.int[2],
                    n_patri$conf.int[2], a_patri$conf.int[2],
                    n_either$conf.int[2], a_either$conf.int[2],
                    n_misc$conf.int[2], a_misc$conf.int[2])
plot_tbl$lower <- c(n_policy$conf.int[1], a_policy$conf.int[1],
                    n_party$conf.int[1], a_party$conf.int[1],
                    n_hillary$conf.int[1], a_hillary$conf.int[1],
                    n_change$conf.int[1], a_change$conf.int[1],
                    n_patri$conf.int[1], a_patri$conf.int[1],
                    n_either$conf.int[1], a_either$conf.int[1],
                    n_misc$conf.int[1], a_misc$conf.int[1])

plot_tbl$Apprentice[plot_tbl$Apprentice=="Never"] <- "Never Watched The Apprentice"

plot_tbl$Apprentice[plot_tbl$Apprentice=="Always"] <- "Always Watched The Apprentice"


figure4 <- ggplot(plot_tbl, aes(fill=as.factor(Apprentice), y=value, x=variable)) + 
  geom_bar(width=0.7, position=position_dodge(width=0.76), stat="identity") +
  geom_errorbar(aes(ymin=lower, ymax=upper), width=.2, 
                size = 0.3, position=position_dodge(0.8), color="grey30") +
  scale_y_continuous(breaks = seq(0, 1, by = 0.1),
                     limits = c(0,0.8),
                     labels = scales::percent_format(accuracy = 1)) +
  theme_minimal() + 
  theme(panel.grid.major.x = element_blank()) +
  theme(panel.grid.minor.y = element_blank()) + 
  scale_fill_manual(values=c("#8a0707", "grey70")) +
  theme(legend.title = element_blank()) + 
  ylim(0, 0.8) +
  xlab('Reasons for Supporting Trump') +
  ylab('Proportion') +
  theme(text = element_text(size = 10, color="black"), 
        axis.text = element_text(color="black", size=10))  + 
  theme(legend.title=element_blank(), 
        legend.background=element_blank())  + 
  theme(legend.position = "top")+
  guides(color = guide_legend(nrow = 1))  



jpeg("figure4.jpg", units="in", width=7, height=4, res=300)

figure4

dev.off()
