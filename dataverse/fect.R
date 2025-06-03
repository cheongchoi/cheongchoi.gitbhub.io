rm(list=ls(all=TRUE)) ## eliminating everything in the memory; be cautious

library(gsynth)
data(gsynth)
library(fect)
library(panelview)
library(patchwork)


### replication: Xu(2017) Election Day Registration and voter turnout in the United States


## 제1단계: Visualizing the treatment and outcome variables

### 1. panelview: treatment status
panelView::panelview(turnout ~ policy_edr, data = turnout, index = c("abb","year"), 
          axis.lab = "time", xlab = "Year", ylab = "Unit", 
          background = "white", main = "Treatment Status")

### 2. panelview outcome variable 

panelView::panelview(turnout ~ policy_edr, data = turnout, index = c("abb","year"), 
                     axis.lab = "time", xlab = "Year", ylab = "Unit", 
                     theme.bw = TRUE, type = "outcome", main = "Simulated Data: Outcome")

## 제2단계: Countfactural Estimators 
### 1: FEct
out.fect <- fect(turnout ~ policy_edr  + policy_mail_in + policy_motor, data = turnout, index = c("abb","year"), 
                 method = "fe", force = "two-way")

out.fect

#### FEct Visualization

plot(out.fect, main = "Estimated ATT (FEct)", ylab = "Effect of D on Y", 
     cex.main = 0.8, cex.lab = 0.8, cex.axis = 0.8, show.point = FALSE)

#### Uncertainty Estimates. 

out.fect <- fect(turnout ~ policy_edr  + policy_mail_in + policy_motor, data = turnout, index = c("abb","year"), 
                 method = "fe", force = "two-way", se = TRUE, parallel = TRUE, nboots = 200)
plot(out.fect, main = "Estimated ATT (FEct)", ylab = "Effect of D on Y", 
     cex.main = 0.8, cex.lab = 0.8, cex.axis = 0.8, show.point = FALSE)


### result summary

print(out.fect)



### 2: IFEct
#### Choosing the number of factors

out.ife <- fect(turnout ~ policy_edr  + policy_mail_in + policy_motor, data = turnout, index = c("abb","year"), 
                 force = "two-way", method = "ife", CV = TRUE, r = c(0, 5), 
                se = TRUE, nboots = 200, parallel = TRUE)

print(out.ife)

plot(out.ife, main = "Estimated ATT (IFEct)")


### 3: Matrix Completion(MC)

out.mc <- fect(turnout ~ policy_edr  + policy_mail_in + policy_motor, data = turnout, index = c("abb","year"), 
                force = "two-way", method = "mc", CV = TRUE, 
               se = TRUE, nboots = 200, parallel = TRUE)

plot(out.mc, main = "Estimated ATT (MC)", show.point = FALSE )

### automated selection of the superior approach

out.both <- fect(turnout ~ policy_edr  + policy_mail_in + policy_motor, data = turnout, index = c("abb","year"), 
               force = "two-way", method = "both", CV = TRUE, 
                 se = TRUE, nboots = 200, parallel = TRUE)
print(out.both)


## 3단계: tests for No pre-trend
### for Fect
plot(out.fect, type = "equiv", ylim = c(-4,4), 
     cex.legend = 0.6, main = "Testing Pre-Trend (FEct)", cex.text = 0.8)

### for IFEct
plot(out.ife, type = "equiv", ylim = c(-4,4), 
     cex.legend = 0.6, main = "Testing Pre-Trend (IFEct)", cex.text = 0.8)
### for MC

plot(out.mc, type = "equiv", ylim = c(-4,4),
     cex.legend = 0.6, main = "Testing Pre-Trend (MC)", cex.text = 0.8)

### Leave-One-OUt Pretrend Test

out.fect.loo <- fect(turnout ~ policy_edr  + policy_mail_in + policy_motor, data = turnout, index = c("abb","year"), 
                     method = "fe", force = "two-way", se = TRUE, parallel = TRUE, nboots = 200, loo = TRUE)

plot(out.fect.loo, type = "equiv", ylim = c(-4,4), loo = TRUE,
     cex.legend = 0.6, main = "Testing Pre-Trend LOO (FEct)", cex.text = 0.8)

out.ife.loo <- fect(turnout ~ policy_edr  + policy_mail_in + policy_motor, data = turnout, index = c("abb","year"),
                    method = "ife", force = "two-way", se = TRUE, parallel = TRUE, nboots = 200, loo = TRUE)
plot(out.ife.loo, type = "equiv", ylim = c(-4,4), loo = TRUE,
     cex.legend = 0.6, main = "Testing Pre-Trend LOO (IFEct)", cex.text = 0.8)

out.mc.loo <- fect(turnout ~ policy_edr  + policy_mail_in + policy_motor, data = turnout, index = c("abb","year"), 
                   method = "mc", force = "two-way", se = TRUE, parallel = TRUE, nboots = 200, loo = TRUE)

plot(out.mc.loo, type = "equiv", ylim = c(-4,4), loo = TRUE, 
     cex.legend = 0.6, main = "Testing Pre-Trend LOO (MC)", cex.text = 0.8)


## 제4단계: Placebo tests

out.fect.p <- fect(turnout ~ policy_edr  + policy_mail_in + policy_motor, data = turnout, index = c("abb","year"),
                   force = "two-way", parallel = TRUE, se = TRUE, CV = 0,
                   nboots = 200, placeboTest = TRUE, placebo.period = c(-2, 0))

plot(out.fect.p, cex.text = 0.8, stats = c("placebo.p","equiv.p"), main = "Estimated ATT (FE)")

###For the placebo test, the manually hided observations are marked in cyan. We can show only a sub-group’s treatment status by specifying the option id to certain units.
plot(out.fect.p, type = 'status', axis.lab = "both", cex.axis  = 0.6)



out.ife.p <- fect(turnout ~ policy_edr  + policy_mail_in + policy_motor, data = turnout, index = c("abb","year"),
                  force = "two-way", method = "ife",  r = 2, CV = 0,
                  parallel = TRUE, se = TRUE,
                  nboots = 200, placeboTest = TRUE, placebo.period = c(-2, 0))
plot(out.ife.p, ylab = "Effect of D on Y", main = "Estimated ATT (IFE)", cex.text = 0.8, stats = c("placebo.p","equiv.p"))


out.mc.p <- fect(turnout ~ policy_edr  + policy_mail_in + policy_motor, data = turnout, index = c("abb","year"),
                 force = "two-way", method = "mc",  lambda = out.mc$lambda.cv, 
                 CV = 0, parallel = TRUE, se = TRUE,
                 nboots = 200, placeboTest = TRUE, placebo.period = c(-2, 0))
plot(out.mc.p, vis = "none", cex.text = 0.8, stats = c("placebo.p","equiv.p"),main = "Estimated ATT (MC)")

plot(out.ife, count = FALSE, show.points = FALSE)
