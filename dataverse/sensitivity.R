####################################################################################
#SOURCEe: Mazumder(2018)
#AUTHOR: Shom Mazumder
#AFFILIATION: Harvard University Dept. of Government
#DATE LAST UPDATED: March 12th, 2018
#PURPOSE: analyze effect of historical protests on contemporary public opinion 
#DETAILS: this script checks the effect of protests on individual attitudes
####################################################################################

rm(list=ls())
   
library(foreign)
library(ggplot2)
library(stargazer)
library(lmtest)
library(dplyr)
library(readr)
library(causalsens)
library(sandwich)


# 제1단계: 데이터 불러오기

# protests <- read_csv('civil_rights_protest_final.csv')
# abs.cces.white <- read_csv('abs-jop-cces-white-countydata.csv')
# merged <- inner_join(abs.cces.white,protests,by='fips')
# cols = c("dem", "affirm", "resent", "protest_indicator", "pctblack", "pcturban", "logTotPop1960", "medincome", "avg.dem.vshare.pre", "state.abb", "sample.size")
# merged2 <- merged %>%
#   select(cols) 
# merged2 <- merged2[complete.cases(merged2),]

merged2 <- readRDS("merged2.RDS")


# 제2단계: 회귀분석

ols.resent2 <- lm(resent ~ protest_indicator + pctblack + pcturban + logTotPop1960 + medincome + avg.dem.vshare.pre + state.abb,
                  data=merged2,weights = sample.size)
summary(ols.resent2)

# 제3단계: estimate probability of treatment

treat2 <- glm(protest_indicator ~ pctblack + pcturban + medincome + avg.dem.vshare.pre + state.abb,
              data = merged2,family = binomial(link = 'logit'))
summary(treat2)

# 제4단계: run the sensitivity analysis to unobserved confounding for racial resentment

alpha <- seq(-0.4,0.4,by=0.005) #set confounding parameter
sens.rr <- causalsens(ols.resent2,treat2,~pctblack + pcturban + medincome + avg.dem.vshare.pre,data = merged2,
                      confound = one.sided.att,alpha = alpha) #estimate sensitivity

# 제5단계: 민감도 분석 결과 그래프

plot(sens.rr, type = "raw", bty = "n")

plot(sens.rr, type = "r.squared", bty = "n") 

