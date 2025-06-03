## Likert 그래프 그리기 Template

library("tidyverse")


# 함수
mrln_Likert <- function(x) {
  # This function plots Likert scales based on:
  # https://scisley.github.io/articles/ggplot-likert-plot/
  
  plotdaten <- x %>% mutate(
    text = paste0(formatC(100 * perc, format = "f", digits = 0), "%"),
    cs = cumsum(perc),
    offset = sum(perc[1:(floor(n()/2))]) + (n() %% 2)*0.5*(perc[ceiling(n()/2)]),
    xmax = -offset + cs,
    xmin = xmax - perc) %>%
    ungroup()
  plotdaten <- plotdaten %>%
    left_join(plotdaten %>%
                group_by(Variable) %>%
                dplyr::summarize(max.xmax = max(xmax)) %>%
                mutate(r = row_number(max.xmax)),
              by = "Variable") %>%
    arrange(desc(r)) %>%
    mutate(ymin = r - (1 - 0.2) / 2,
           ymax = r + (1 - 0.2) / 2)
  
  ggplot(plotdaten) +
    geom_rect(aes(xmin = xmin, xmax = xmax, ymin = ymin, ymax = ymax, fill = Answer)) +
    geom_vline(xintercept = 0, color = "#9a9a9a") +
    geom_text(aes(x=(xmin+xmax)/2, y=(ymin+ymax)/2, label=text), size = 3) +
    scale_y_continuous("", breaks = 1:n_distinct(plotdaten$Variable),
                       labels=rev(plotdaten %>% distinct(Variable) %>% .$Variable)) +
    scale_fill_brewer("", palette = "RdBu", 
                      breaks=c('Strongly disagree', 'Disagree', 'Neutral', 'Agree', 'Strongly agree')) +
    theme_classic() +
    labs(x = "", y = "") +
    theme(plot.title = element_text(hjust = 0.5),
          plot.subtitle = element_text(hjust = 0.5),
          axis.text = element_text(colour = "black"))
}

Likert <- readRDS("Likert.RDS")

# 함수적용 그래프
plot_1 <- Likert %>%
  filter(Variable == "Is unjust" |
           Variable == "Is widespread" |
           Variable == "Is a serious problem" |
           Variable == "Discrimination of\nnon-Western minorities...\nHas bad consequences" |
           Variable == "Is one of the main\ncauses of ethnic inequalities") %>%
  mrln_Likert() +
  labs(title = "Likert Graph Template") +
  scale_x_continuous("", limits = c(-0.6, 0.75), 
                     labels = c("-50%", "-25%", "0%", "25%", "50%", "75%"), 
                     breaks=c(-0.5, -0.25, 0, 0.25, 0.5, 0.75)) +
  theme(legend.position = "bottom")
plot_1