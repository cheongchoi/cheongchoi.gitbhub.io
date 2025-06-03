rm(list=ls())

library(PanelMatch)
library(ggplot2)
library(gridExtra)
dem <- dem

dem.sub <- dem[dem[, "wbcode2"] <= 100, ]
# create subset of data for simplicity 
# get a matched set without refinement 
sets0 <- PanelMatch(lag = 4, 
                    time.id = "year", 
                    unit.id = "wbcode2", 
                    treatment = "dem", 
                    refinement.method = "none", 
                    data = dem.sub, 
                    match.missing = FALSE, 
                    size.match = 5, 
                    qoi = "att", 
                    outcome.var = "y", 
                    lead = 0:4, 
                    forbid.treatment.reversal = FALSE) 

# get a matched set with refinement using propensity score matching, setting the 
# size of matched set to 5 

sets1 <- PanelMatch(lag = 4, 
                    time.id = "year", 
                    unit.id = "wbcode2", 
                    treatment = "dem", 
                    refinement.method = "ps.match", 
                    data = dem.sub, 
                    match.missing = FALSE, 
                    covs.formula = ~ tradewb, 
                    size.match = 5, 
                    qoi = "att", 
                    outcome.var = "y", 
                    lead = 0:4, 
                    forbid.treatment.reversal = FALSE) 

# get another matched set with refinement using propensity score weighting 
sets2 <- PanelMatch(lag = 4, 
                    time.id = "year", 
                    unit.id = "wbcode2", 
                    treatment = "dem", 
                    refinement.method = "ps.weight", 
                    data = dem.sub, 
                    match.missing = FALSE, 
                    covs.formula = ~ tradewb, 
                    size.match = 10, 
                    qoi = "att", 
                    outcome.var = "y", 
                    lead = 0:4, 
                    forbid.treatment.reversal = FALSE) 
#use the function to produce the scatter plot 
balance_scatter(matched_set_list = list(sets0$att), 
                data = dem.sub, 
                covariates = c("y", "tradewb")) 

balance_scatter(matched_set_list = list(sets0$att, sets1$att, sets2$att), 
                data = dem.sub, 
                covariates = c("y", "tradewb")) 
# add legend
legend(x = 0, y = 0.8, 
       legend = c("mahalanobis", "PS weighting"), 
       y.intersp = 0.65, 
       x.intersp = 0.3, 
       xjust = 0, 
       pch = c(1, 3), 
       pt.cex = 1, 
       bty = "n", ncol = 1, cex = 1, bg = "white")


DisplayTreatment(unit.id = "wbcode2", 
                 time.id = "year", 
                 legend.position = "none", 
                 xlab = "year", 
                 ylab = "Country Code", 
                 treatment = "dem", 
                 data = dem)


get_covariate_balance(sets1$att, 
                      dem.sub, 
                      covariates = c("tradewb"), 
                      ylim = c(-2,2))



PM.results <- PanelMatch(lag = 4, 
                         time.id = "year", 
                         unit.id = "wbcode2", 
                         treatment = "dem", 
                         refinement.method = "ps.match", 
                         data = dem.sub, 
                         match.missing = TRUE, 
                         covs.formula = ~ I(lag(tradewb, 1:4)), 
                         size.match = 5, 
                         qoi = "att", 
                         outcome.var = "y", 
                         lead = 0:4, 
                         forbid.treatment.reversal = FALSE, 
                         placebo.test = TRUE)

set.effects <- get_set_treatment_effects(pm.obj = PM.results, 
                                         data = dem.sub, 
                                         lead = 0)
summary(set.effects)

PE.results <- PanelEstimate(sets = PM.results, 
                            data = dem.sub, 
                            se.method = "unconditional")

plot(PE.results)

summary(PE.results)
placebo_test(PM.results, 
             data = dem.sub, 
             se.method = "unconditional", 
             plot = TRUE)


plot(PM.results$att)
plot(PM.results$att, include.empty.sets = TRUE)

summary(PM.results$att)
