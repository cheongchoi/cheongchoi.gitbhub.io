################################################################################
# This file contains the R code used in the mediation vignette.
################################################################################
rm(list=ls())

library("mediation")

# 제1단계: 데이터 불러오기
data("jobs")
set.seed(4646)

# This dataset is from the Job Search Intervention Study (JOBS II) [10]. In the
# JOBS II field experiment, 1,801 unemployed workers received a pre-screening
# questionnaire and were then randomly assigned to treatment and control
# groups. Those in the treatment group participated in job-skills workshops.
# Those in the control condition received a booklet describing job-search tips. In
# follow-up interviews, two key outcome variables were measured: a continuous
# measure of depressive symptoms based on the Hopkins Symptom Checklist
# (depress2), and a binary variable representing whether the respondent had
# become employed (work1). In the JOBS II data, a continuous measure of
# job-search self-efficacy represents a key mediating variable (job_seek). In
# addition to the outcome and mediators, the JOBS II data also include the
# following list of baseline covariates that were measured prior to the administration
# of the treatment: pretreatment level of depression (depress1), education
# (educ), income, race (nonwhite), marital status (marital), age, sex,
# previous occupation (occp), and the level of economic hardship (econ_hard).


# 제2단계: 매개효과(Baron-Kenny procedure)
## 2.1. 기본 모형(X->Y)
model.m <- lm(job_seek ~ treat + depress1 + econ_hard + sex + age
                  + occp + marital + nonwhite + educ + income, data = jobs)
## 2.2. 매개변수(X -> M)
model.y <- lm(depress2 ~ treat + job_seek + depress1 + econ_hard + sex + age
                  + occp + marital + nonwhite + educ + income, data = jobs)

## 2.3.Nonparametric Bootstrap Confidence Intervals with the Percentile Method
## 2.3. 비매개변수 부트스트랩(1000번의 재표본추출)
out.1 <- mediate(model.m, model.y, sims = 1000, boot = TRUE, treat = "treat",
                                mediator = "job_seek")

summary(out.1)
## 2.4. Quasi-Bayesian Confidence Intervals
## 2.4. boot = FALSE 인 경우, 추론은 알고리즘 2가 아닌 알고리즘 1을 사용하는 준베이지안 몬테카를로 근사법을 통해 진행

out.2 <- mediate(model.m, model.y, sims = 1000, treat = "treat",
                    mediator = "job_seek")

summary(out.2)
## ACME(매개효과), ADE(직접효과), Total Effect(총효과)


plot(out.2)


## 3. 상호작용항을 가진, Baron-Kenny Procedure

model.y <- lm(depress2 ~ treat + job_seek + treat:job_seek + depress1 + econ_hard
               + sex + age + occp + marital + nonwhite + educ + income, data = jobs)
out.3 <- mediate(model.m, model.y, sims = 1000, treat = "treat", 
                    mediator = "job_seek")
summary(out.3)
plot(out.3, treatment = "both")


## 4. Discrete Mediator and Outcome Data

model.m <- glm(job_dich ~ treat + depress1 + econ_hard + sex + age
                + occp + marital + nonwhite + educ + income, data = jobs, 
                family = binomial(link = "probit"))
model.y <- lm(depress2 ~ treat + job_dich + treat:job_dich + depress1
                + econ_hard + sex + age + occp + marital + nonwhite 
                + educ + income, data = jobs)
out.8 <- mediate(model.m, model.y, sims = 1000, treat = "treat",
                    mediator = "job_dich")
summary(out.8)

model.m <- polr(job_disc ~ treat + depress1 + econ_hard + sex + age 
                + occp + marital + nonwhite + educ + income, 
                data = jobs, method = "probit", Hess = TRUE)
model.y <- lm(depress2 ~ treat + job_disc + depress1 + econ_hard + sex + age 
                + occp + marital + nonwhite + educ + income, data = jobs)

out.9 <- mediate(model.m, model.y, sims = 1000, treat = "treat",
                     mediator = "job_disc")
summary(out.9)


## 5. Sensitivity Analysis
model.m <- lm(job_seek ~ treat + depress1 + econ_hard + sex + age + occp 
                + marital + nonwhite + educ + income, data = jobs)
model.y <- lm(depress2 ~ treat + job_seek + depress1 + econ_hard + sex + age + occp
                + marital + nonwhite + educ + income, data = jobs)
med.cont <- mediate(model.m, model.y, sims=1000,  treat = "treat",
                       mediator = "job_seek")
summary(med.cont)                       
sens.cont <- medsens(med.cont, rho.by = 0.05)

summary(sens.cont)

plot(sens.cont, sens.par = "rho")

plot(sens.cont, sens.par = "R2", r.type = "total", sign.prod = "negative")

model.y <- glm(work1 ~ treat + job_seek + depress1 + econ_hard + sex + age 
                + occp + marital + nonwhite + educ + income, 
                family = binomial(link = "probit"), data = jobs)
med.bout <- mediate(model.m, model.y, sims = 1000, treat = "treat",
                       mediator = "job_seek")

sens.bout <- medsens(med.bout, rho.by = 0.05, sims = 1000)

plot(sens.bout, sens.par = "rho")
plot(sens.bout, sens.par = "R2", r.type = "total", sign.prod = "positive")

plot(sens.bout, sens.par = "rho", pr.plot = TRUE)

model.m <- glm(job_dich ~ treat + depress1 + econ_hard + sex + age 
                + occp + marital + nonwhite + educ + income, 
                data = jobs, family = binomial(link = "probit"))
model.y <- lm(depress2 ~ treat + job_dich+ depress1 + econ_hard + sex + age 
                + occp + marital + nonwhite + educ + income, data = jobs)
med.bmed <- mediate(model.m, model.y, sims = 1000, treat = "treat",
                       mediator = "job_dich")
sens.bmed <- medsens(med.bmed, rho.by = 0.05, sims = 1000)

plot(sens.bmed, sens.par = "rho")

