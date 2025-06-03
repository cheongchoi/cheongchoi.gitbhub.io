## plot figures 2 and 3 using visits data
#install.packages("data.table")
#install.packages("ggplot2")
#install.packages("dplyr")
#install.packages("lubridate")

library(data.table)
library(ggplot2)
library(dplyr)
library(lubridate)


#### Set your WD
#setwd("")

#### visits data
# visits <- fread("NewsVisits.csv")
# 
# # demographic data
# 
# demos <- readRDS("Respondent_Info.rds")
# 
# # # # # # # # # # # #  plotting visits
# visits <- merge(visits, demos, by = "person_id")
# 
# visitsgrouped <- filter(visits, affiliation != "")
# visitsgrouped <- filter(visitsgrouped, day > as.Date('2019-3-1'))
# visitsgrouped <- filter(visitsgrouped, day < '2019-12-7')
# 
# partisans <- visitsgrouped %>%
#   group_by(affiliation, day) %>%
#   dplyr::summarize(
#     partisans = n(),
#     users = n_distinct(person_id)
#   ) %>%
#   ungroup()
# 
# 
# visitsgrouped <- visitsgrouped %>%
#   group_by(day, affiliation) %>%
#   dplyr::summarize(
#     visits = n(),
#     ideo_avg = mean(wilson, na.rm = TRUE)
#   ) %>%
#   ungroup()
# 
# 
# partisans$wavenumber <- paste0(as.character(partisans$affiliation),"-", as.character(partisans$day))
# visitsgrouped$wavenumber <- paste0(as.character(visitsgrouped$affiliation),"-", as.character(visitsgrouped$day))
# 
# partisans <- select(partisans, partisans, wavenumber, users)
# 
# visitsgrouped <- merge(visitsgrouped, partisans, by = "wavenumber")
# visitsgrouped$visits_avg <- visitsgrouped$visits/ visitsgrouped$users
# visitsgrouped$Affiliation <- as.factor(visitsgrouped$affiliation)
# 
# visitsgrouped$visitsRoll <- frollmean(visitsgrouped$visits_avg, 3)
# visitsgrouped$ideoRoll <- frollmean(visitsgrouped$ideo_avg, 3)
# 
# saveRDS(visitsgrouped, "visitsgrouped.RDS")
visitsgrouped <- readRDS("visitsgrouped.RDS")
#######################

ggplot(data=visitsgrouped, aes(x=day, y=visitsRoll, group=Affiliation, color=Affiliation)) +
  geom_line(size = 1.2) +
  ylab("") +
  xlab("News date")+
  geom_vline(xintercept = as.numeric(as.Date("2019-04-17")), linetype="dotted", 
             color = "black", size=1)+
  geom_vline(xintercept = as.numeric(as.Date("2019-07-24")), linetype="dotted", 
             color = "black", size=1)+
  geom_vline(xintercept = as.numeric(as.Date("2019-09-18")), linetype="dotted", 
             color = "black", size=1)+
  geom_vline(xintercept = as.numeric(as.Date("2019-12-17")), linetype="dotted", 
             color = "black", size=1)+
  scale_x_date(date_breaks = "1 month", date_minor_breaks = "1 month",
               date_labels = "%B")+
  theme_bw()+
  scale_color_manual(values = c("grey50", "grey80", "grey10"))+
  theme(axis.line = element_line(color='black'),
        plot.background = element_blank(),
        panel.grid.minor = element_blank(),
        panel.grid.major = element_blank())+
  theme(
    axis.text.x = element_text(color="black"),
    axis.text.y = element_text(color="black"),
    axis.ticks = element_line(color = "black"),
    axis.title.x = element_text(margin = margin(t = 20)),
    axis.title.y = element_text(margin = margin(r = 10)),
    legend.title = element_blank()) +  # This line removes the legend title
  theme(legend.position="bottom")
ggsave("Figure2.png", width = 9, height = 7)

ggplot(data=visitsgrouped, aes(x=day, y=ideoRoll, group=Affiliation, color=Affiliation)) +
  geom_line(size = 1.2) +
  ylab("") +
  xlab("News date")+
  geom_vline(xintercept = as.numeric(as.Date("2019-04-17")), linetype="dotted", 
             color = "black", size=1)+
  geom_vline(xintercept = as.numeric(as.Date("2019-07-24")), linetype="dotted", 
             color = "black", size=1)+
  geom_vline(xintercept = as.numeric(as.Date("2019-09-18")), linetype="dotted", 
             color = "black", size=1)+
  geom_vline(xintercept = as.numeric(as.Date("2019-12-17")), linetype="dotted", 
             color = "black", size=1)+
  scale_x_date(date_breaks = "1 month", date_minor_breaks = "1 month",
               date_labels = "%B")+
  theme_bw()+
  scale_color_manual(values = c("grey50", "grey80", "grey10"))+
  theme(axis.line = element_line(color='black'),
        plot.background = element_blank(),
        panel.grid.minor = element_blank(),
        panel.grid.major = element_blank())+
  theme(
    axis.text.x = element_text(color="black"),
    axis.text.y = element_text(color="black"),
    axis.ticks = element_line(color = "black"),
    axis.title.x = element_text(margin = margin(t = 20)),
    axis.title.y = element_text(margin = margin(r = 10)),
    legend.title = element_blank()) +  # This line removes the legend title
  theme(legend.position="bottom")
ggsave("Figure3.png", width = 9, height = 7)

