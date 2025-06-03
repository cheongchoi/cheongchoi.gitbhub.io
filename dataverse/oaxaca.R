# install.packages("oaxaca")
rm(list=ls())
gc()
library("oaxaca")
library("dplyr")
library("fastDummies")

# master <- readRDS("polcat.RDS")
# polcat <- master%>%
#   filter((job %in% c(441, 152, 153, 313) & sps==1) | (nps==1 & compst==1 & compsize==10 & empst==1) | (nps==1 & compst==3 & empst==1)) %>%
#   mutate(jobcat = case_when(
#     job==441~"경찰",
#     job==152~"교육",
#     job==153~"교육",
#     job==313~"일반직")) %>%
#   mutate(jobcat=ifelse((nps==1 & compst==1 & compsize==10 & empst==1), "사기업", 
#                        ifelse((nps==1 & compst==3 & empst==1), "공공기관", jobcat)))
# master.long <- polcat %>%
#   mutate(type = ifelse(male==1, "남성", "여성"))  %>%
#   dplyr::select(c(type, satlifenow, empst, jobsat, jobcat, age, marital, health, edu, income, Q1, Q2, Q3, Q4, Q5, Q6, Q7, Q8, Q9, year))
# 
# 
# master.long2 <- master.long %>%
#   filter(!is.na(jobsat) & jobsat>=1)
# 
# mdat2 <- master.long2 %>%
#          mutate(female = case_when(
#                 type == "여성"~1,
#                 type == "남성"~0,
#          ),
#          marital2 = ifelse(marital==2, 1, 0),
#          marital3 = ifelse(marital==3, 1, 0),
#          marital4 = ifelse(marital==4, 1, 0),
#          marital5 = ifelse(marital==5, 1, 0))
# mdat2 <- dummy_cols(mdat2, select_columns = "year")  
# mdat2 <- mdat2 %>%
#   mutate(edu = ifelse(is.na(edu), 4, edu))
# mdat2 <- dummy_cols(mdat2, select_columns = "edu")  
# mdat2 <- dummy_cols(mdat2, select_columns = "jobcat")  
# mdat2 <- dummy_cols(mdat2, select_columns = "empst")  
# 
# # mdat2 <- master %>%
# #         filter(year==2023) %>%
# #         left_join(., masterlca, by = 'pid')
# # mdat2 <- dummy_cols(mdat2, select_columns = "class")  
# mdat2 <- mdat2 %>%
#        #   filter(!is.na(class))%>%
#       mutate(female = case_when(
#         type == "남성"~0,
#         type == "여성"~1 )) %>%
#       mutate(income = ifelse(income<0, NA, income),
#              health = ifelse(health<0, NA, health),
#              satlifenow = ifelse(satlifenow<0, NA, satlifenow) ) %>%
#     mutate(health = case_when(
#             health==1~5,
#             health==2~4,
#             health==3~3,
#             health==4~2,
#             health==5~1))
# 
# # x1무관심 x2양성평등 x3 남성우월 
# model <- lm(jobsat ~ income + health + satlifenow + female + age + jobcat_경찰 + jobcat_공공기관 + jobcat_교육 + jobcat_일반직 +  edu_2 + edu_3+ edu_4 + edu_5 + year_2003
#             + year_2004  + year_2005  + year_2006  + year_2007  + year_2008  + year_2009  + year_2010  + year_2011  + year_2012 + year_2013
#             + year_2014  + year_2015  + year_2016  + year_2017  + year_2018  + year_2019  + year_2020  + year_2021  + year_2022 + year_2023, data= mdat2)
# summary(model)
# model2 <- lm(jobsat ~ income + female + health + satlifenow +  age + factor(year) + edu + jobcat_경찰 + jobcat_공공기관 + jobcat_교육 + jobcat_일반직, data= mdat2)
# summary(model2)
# 
# results2 <- oaxaca(formula = jobsat ~ income + health + satlifenow +  age + year + edu | female , data= mdat2, R=100)
# plot(results2, components = c("endowments"))
# plot(results2, components = c("coefficients"))
# 
# results2$n
#   # n.A 남성 10546 여성 6989, 전체 17535
# # y.A 남성 3.67 여성 3.66 차이는 0.01
# results2$y
# results2$threefold$overall
# # 0.11(집단간 차이, 나이, 수입, 성역할인식)
# # -0.06은 계수에서의 차이
# # -0.03은 이 둘간의 상호작용
# 
# plot(results2, components = c("endowments","coefficients"))
# # 기여요소에서, 나이는 영향이 없다. 하지만, 
# # 수입이 크면 만족도에 긍정적 영향을 미치고, 
# # 대부분은 남녀 직무만족의 차이는 수입이큰 개인의 비율에서 집단적인 차이에서 기인함
# 
# # 성역할인식에서 남성우월 관점은 부의 영향을
# # 양성평
# 
# summary(results2$reg$reg.pooled.2)$coefficients["income",]
# results2$x$x.mean.diff["income"]
# # 남성의 수입은 여성의 수입에 비해 평균적 1913만원 높은 것으로 나타남
# results2$beta$beta.diff["age"]
# # 나이가 미치는 직무만족의 차이의 영향은 부정적이다. 즉, 나이가 커질 수록, 직무만족에 미치는 영향은
# # 남성에게 더 작아지는 것으로 나타났다.
# # 나이가 미치는 영향은 직무만족의 차이의 상당한 부분을 차지 하는 것으로 나타났다. 
# 
# results2$twofold$overall
# plot(results2, decomposition = "twofold", group.weight =-1)
# 
# 
# mdat2 <- mdat2 %>%
#         mutate(public= case_when(
#           jobcat=="경찰"~1,
#           jobcat=="공공기관"~0,
#           jobcat=="교육"~1,
#           jobcat=="사기업"~0,
#           jobcat=="일반직"~1))
# results3 <- oaxaca(formula = jobsat ~  female + income + health + satlifenow +  age + edu +  year_2004  + year_2005  + year_2006  + year_2007  + year_2008  + year_2009  + year_2010  + year_2011  + year_2012 + year_2013
#                    + year_2014  + year_2015  + year_2016  + year_2017  + year_2018  + year_2019  + year_2020  + year_2021  + year_2022 + year_2023
#                    | public | female , data= mdat2, R=100)
# plot(results3, components = c("endowments"))
# 
# results3$n
# # n.A 비공직 12472 공직 5698, 전체 18170
# # y.A 비공직 3.71 공직 3.89 차이는 -0.16
# results3$y
# results3$threefold$overall
# # 0.11(집단간 차이, 나이, 수입, 성역할인식)
# # -0.06은 계수에서의 차이
# # -0.03은 이 둘간의 상호작용
# 
# plot(results3, components = c("endowments"), variables=c("female", "income", "health", "satlifenow",  "age", "edu"))
# 
# plot(results3, components = c("coefficients"), variables=c("female", "income", "health", "satlifenow",  "age", "edu"))
# 
# plot(results3, components = c("endowments"))
# plot(results3, components = c("coefficients"))
# summary(results3$reg$reg.pooled.2)
# summary(results3$reg$reg.pooled.2)$coefficients
# 
# results3$x$x.mean.diff
# results3$beta$beta.diff
# # 기여요소에서, 나이는 영향이 없다. 하지만, 
# # 수입이 크면 만족도에 긍정적 영향을 미치고, 
# # 대부분은 남녀 직무만족의 차이는 수입이큰 개인의 비율에서 집단적인 차이에서 기인함
# 
# # 성역할인식에서 남성우월 관점은 부의 영향을
# # 양성평
# 
# summary(results3$reg$reg.pooled.2)$coefficients[c("female", "income", "health", "satlifenow",  "age", "edu"),]
# results3$x$x.mean.diff["income"]
# # 남성의 수입은 여성의 수입에 비해 평균적 1913만원 높은 것으로 나타남
# results3$beta$beta.diff["age"]
# # 나이가 미치는 직무만족의 차이의 영향은 부정적이다. 즉, 나이가 커질 수록, 직무만족에 미치는 영향은
# # 남성에게 더 작아지는 것으로 나타났다.
# # 나이가 미치는 영향은 직무만족의 차이의 상당한 부분을 차지 하는 것으로 나타났다. 
# 
# results3$twofold$overall
# plot(results3, decomposition = "twofold", group.weight =-1,variables=c("female", "income", "health", "satlifenow",  "age", "edu"))
# 
# 
# plot(results3, decomposition = "twofold", group.weight = -1,
#         unexplained.split = TRUE, 
#         components = c("unexplained A", "unexplained B"), 
#         component.labels = c("unexplained A" = "AgainstPrivate Workers", "unexplained B" = "In Favor of  the Public Employees"),
#         variables = c("female", "health", "satlifenow"), 
#         variable.labels = c("female" = "Female", "health" = "Health", "satlifenow" = "Life Satisfaction"))
# plot(results3, decomposition = "twofold", group.weight = -1,
#      unexplained.split = TRUE, 
#      component.left = TRUE,
#      components = c("unexplained A", "unexplained B"), 
#      component.labels = c("unexplained A" = "Against Private Workers", "unexplained B" = "In Favor of the Public Employees"),
#      variables = c("female", "health", "satlifenow"), 
#      variable.labels = c("female" = "Female", "health" = "Health", "satlifenow" = "Life Satisfaction"))
# 
# 
# variables <- c("female", "health", "satlifenow")
# columns <- c("group.weight", "coef(unexplained A)", "coef(unexplained B)")
# results3$twofold$variables[[5]][variables, columns]



# 2. 공공부문과 민간부문의 직무만족 차이 분석
# Differences in Job Satisfaction between public employees and private employees

mdat2 <- readRDS("oaxaca.RDS")
# polcat <- master%>%
#   filter((job %in% c(441, 152, 153, 313) & sps==1) | (nps==1 & compst==1 & compsize==10 & empst==1) | (nps==1 & compst==3 & empst==1)) %>%
#   mutate(jobcat = case_when(
#     job==441~"경찰",
#     job==152~"교육",
#     job==153~"교육",
#     job==313~"일반직")) %>%
#   mutate(jobcat=ifelse((nps==1 & compst==1 & compsize==10 & empst==1), "사기업", 
#                        ifelse((nps==1 & compst==3 & empst==1), "공공기관", jobcat)))
# master.long <- polcat %>%
#   mutate(type = ifelse(male==1, "남성", "여성"))  %>%
#   dplyr::select(c(type, satlifenow, empst, jobsat, jobcat, age, marital, health, edu, income, Q1, Q2, Q3, Q4, Q5, Q6, Q7, Q8, Q9, year))
# 
# 
# master.long2 <- master.long %>%
#   filter(!is.na(jobsat) & jobsat>=1)
# 
# mdat2 <- master.long2 %>%
#   mutate(female = case_when(
#     type == "여성"~1,
#     type == "남성"~0,
#   ),
#   spouse = ifelse(marital==2, 1, 0))
# mdat2 <- dummy_cols(mdat2, select_columns = "year")  
# mdat2 <- mdat2 %>%
#   mutate(edu = ifelse(is.na(edu), 4, edu))
# mdat2 <- dummy_cols(mdat2, select_columns = "edu")  
# mdat2 <- dummy_cols(mdat2, select_columns = "jobcat")  
# mdat2 <- dummy_cols(mdat2, select_columns = "empst")  
# 
# # mdat2 <- master %>%
# #         filter(year==2023) %>%
# #         left_join(., masterlca, by = 'pid')
# # mdat2 <- dummy_cols(mdat2, select_columns = "class")  
# mdat2 <- mdat2 %>%
#   #   filter(!is.na(class))%>%
#   mutate(female = case_when(
#     type == "남성"~0,
#     type == "여성"~1 )) %>%
#   mutate(income = ifelse(income<0, NA, income),
#          health = ifelse(health<0, NA, health),
#          satlifenow = ifelse(satlifenow<0, NA, satlifenow) ) %>%
#   mutate(health = case_when(
#     health==1~5,
#     health==2~4,
#     health==3~3,
#     health==4~2,
#     health==5~1))
# 
# 
# mdat2 <- mdat2 %>%
#   filter(jobcat!="공공기관") %>%
#   mutate(public= case_when(
#     jobcat=="경찰"~1,
#     jobcat=="교육"~1,
#     jobcat=="사기업"~0,
#     jobcat=="일반직"~1))
results3_1 <- oaxaca(formula = jobsat ~  female + income + health + satlifenow +  age + edu +  spouse + year_2004  + year_2005  + year_2006  + year_2007  + year_2008  + year_2009  + year_2010  + year_2011  + year_2012 + year_2013
                   + year_2014  + year_2015  + year_2016  + year_2017  + year_2018  + year_2019  + year_2020  + year_2021  + year_2022 + year_2023
                   | public | female , data= mdat2, R=1000)

mdat2 %>%
  group_by(public) %>%
  summarise(count=n(),
            femalem = mean(female),
            females = sd(female),
            incomem = mean(income),
            incomes = sd(income),
            healthm = mean(health),
            healths = sd(health),
            satlifenowm = mean(satlifenow),
            satlifenows = sd(satlifenow),
            agem = mean(age),
            ages = sd(age),
            edum = mean(edu),
            edus = sd(edu),
            spousem = mean(spouse),
            spouses = sd(spouse)) %>%
  mutate(proportions = count*100 / sum(count))

yrPublic <- mdat2 %>%
  group_by(year, public) %>%
  summarise(count=n(),
            femalem = mean(female),
            females = sd(female),
            incomem = mean(income),
            incomes = sd(income),
            healthm = mean(health),
            healths = sd(health),
            satlifenowm = mean(satlifenow),
            satlifenows = sd(satlifenow),
            agem = mean(age),
            ages = sd(age),
            edum = mean(edu),
            edus = sd(edu),
            spousem = mean(spouse),
            spouses = sd(spouse)) %>%
  mutate(proportions = count*100 / sum(count))

            

results3_1$n
# n.A 비공직 12472 공직 5698, 전체 18170
# y.A 비공직 3.71 공직 3.89 차이는 -0.16
results3_1$y
results3_1$threefold$overall
# 0.11(집단간 차이, 나이, 수입, 성역할인식)
# -0.06은 계수에서의 차이
# -0.03은 이 둘간의 상호작용

plot(results3_1, components = c("endowments"), variables=c("female", "income", "health", "satlifenow",  "spouse", "edu"))

plot(results3_1, components = c("coefficients"), variables=c("female", "income", "health", "satlifenow",  "spouse", "edu"))

summary(results3_1$reg$reg.pooled.2)
summary(results3_1$reg$reg.pooled.2)$coefficients

results3_1$x$x.mean.diff[1:8]
results3_1$beta$beta.diff[1:8]
# 기여요소에서, 나이는 영향이 없다. 하지만, 
# 수입이 크면 만족도에 긍정적 영향을 미치고, 
# 대부분은 남녀 직무만족의 차이는 수입이큰 개인의 비율에서 집단적인 차이에서 기인함

# 성역할인식에서 남성우월 관점은 부의 영향을
# 양성평

summary(results3_1$reg$reg.pooled.2)$coefficients[c("female", "income", "health", "satlifenow",  "spouse", "edu"),]
results3_1$x$x.mean.diff["spouse"]
# 남성의 수입은 여성의 수입에 비해 평균적 1913만원 높은 것으로 나타남
results3_1$beta$beta.diff["age"]
# 나이가 미치는 직무만족의 차이의 영향은 부정적이다. 즉, 나이가 커질 수록, 직무만족에 미치는 영향은
# 남성에게 더 작아지는 것으로 나타났다.
# 나이가 미치는 영향은 직무만족의 차이의 상당한 부분을 차지 하는 것으로 나타났다. 

results3_1$twofold$overall
plot(results3_1, decomposition = "twofold", group.weight =-1,variables=c("female", "income", "health", "satlifenow",  "age", "edu"))



plot(results3_1, decomposition = "twofold", group.weight = -1,
     unexplained.split = TRUE, 
     component.left = TRUE,
     components = c("unexplained A", "unexplained B"), 
     component.labels = c("unexplained A" = "Against Private Workers", "unexplained B" = "In Favor of the Public Employees"),
     variables = c("female", "health", "satlifenow"), 
     variable.labels = c("female" = "Female", "health" = "Health", "satlifenow" = "Life Satisfaction", "spouse" = "Spouse"))


variables <- c("female", "health", "satlifenow")
columns <- c("group.weight", "coef(unexplained A)", "coef(unexplained B)")
results3_1$twofold$variables[[5]][variables, columns]

vars = c("year", "jobsat","female", "income", "health", "satlifenow", "age", "edu",  "spouse", "year_2004", "year_2005",
         "year_2006", "year_2007", "year_2008", "year_2009",  "year_2010", "year_2011", "year_2012", "year_2013","year_2014",  
         "year_2015", "year_2016",  "year_2017",  "year_2018",  "year_2019", "year_2020", "year_2021", "year_2022", "year_2023",
         "public")
mdat2<-mdat2 %>%
  select(vars)
mdat2 <- mdat2[complete.cases(mdat2),]
# saveRDS(mdat2, "oaxaca.RDS")




# 3. 공공부문과 민간부문의 직무만족 차이 분석: 남녀
# Differences in Job Satisfaction between public employees and private employees

master <- readRDS("polcat.RDS")
polcat <- master%>%
  filter((job %in% c(441, 152, 153, 313) & sps==1) | (nps==1 & compst==1 & compsize==10 & empst==1) | (nps==1 & compst==3 & empst==1)) %>%
  mutate(jobcat = case_when(
    job==441~"경찰",
    job==152~"교육",
    job==153~"교육",
    job==313~"일반직")) %>%
  mutate(jobcat=ifelse((nps==1 & compst==1 & compsize==10 & empst==1), "사기업", 
                       ifelse((nps==1 & compst==3 & empst==1), "공공기관", jobcat)))
master.long <- polcat %>%
  mutate(type = ifelse(male==1, "남성", "여성"))  %>%
  dplyr::select(c(type, satlifenow, empst, jobsat, jobcat, age, marital, health, edu, income, Q1, Q2, Q3, Q4, Q5, Q6, Q7, Q8, Q9, year))


master.long2 <- master.long %>%
  filter(!is.na(jobsat) & jobsat>=1)

mdat2 <- master.long2 %>%
  mutate(female = case_when(
    type == "여성"~1,
    type == "남성"~0,
  ),
  spouse = ifelse(marital==2, 1, 0))
mdat2 <- dummy_cols(mdat2, select_columns = "year")  
mdat2 <- mdat2 %>%
  mutate(edu = ifelse(is.na(edu), 4, edu))
mdat2 <- dummy_cols(mdat2, select_columns = "edu")  
mdat2 <- dummy_cols(mdat2, select_columns = "jobcat")  
mdat2 <- dummy_cols(mdat2, select_columns = "empst")  

# mdat2 <- master %>%
#         filter(year==2023) %>%
#         left_join(., masterlca, by = 'pid')
# mdat2 <- dummy_cols(mdat2, select_columns = "class")  
mdat2 <- mdat2 %>%
  #   filter(!is.na(class))%>%
  mutate(female = case_when(
    type == "남성"~0,
    type == "여성"~1 )) %>%
  mutate(income = ifelse(income<0, NA, income),
         health = ifelse(health<0, NA, health),
         satlifenow = ifelse(satlifenow<0, NA, satlifenow) ) %>%
  mutate(health = case_when(
    health==1~5,
    health==2~4,
    health==3~3,
    health==4~2,
    health==5~1))


mdat2 <- mdat2 %>%
  filter(jobcat!="공공기관") %>%
  mutate(public= case_when(
    jobcat=="경찰"~1,
    jobcat=="교육"~1,
    jobcat=="사기업"~0,
    jobcat=="일반직"~1))
results3_2 <- oaxaca(formula = jobsat ~  public + income + health + satlifenow +  age + edu +  spouse + year_2004  + year_2005  + year_2006  + year_2007  + year_2008  + year_2009  + year_2010  + year_2011  + year_2012 + year_2013
                     + year_2014  + year_2015  + year_2016  + year_2017  + year_2018  + year_2019  + year_2020  + year_2021  + year_2022 + year_2023
                     | female | public , data= mdat2, R=100)

results3_2$n
# n.A 비공직 12472 공직 5698, 전체 18170
# y.A 비공직 3.71 공직 3.89 차이는 -0.16
results3_2$y
results3_2$threefold$overall
# 0.11(집단간 차이, 나이, 수입, 성역할인식)
# -0.06은 계수에서의 차이
# -0.03은 이 둘간의 상호작용

plot(results3_2, components = c("endowments"), variables=c("public", "income", "health", "satlifenow",  "spouse", "edu"))

plot(results3_2, components = c("coefficients"), variables=c("public", "income", "health", "satlifenow",  "spouse", "edu"))

summary(results3_2$reg$reg.pooled.2)
# summary(results3_1$reg$reg.pooled.2)

summary(results3_2$reg$reg.pooled.2)$coefficients

results3_2$x$x.mean.diff[1:8]
results3_2$beta$beta.diff[1:8]
# 기여요소에서, 나이는 영향이 없다. 하지만, 
# 수입이 크면 만족도에 긍정적 영향을 미치고, 
# 대부분은 남녀 직무만족의 차이는 수입이큰 개인의 비율에서 집단적인 차이에서 기인함

# 성역할인식에서 남성우월 관점은 부의 영향을
# 양성평

summary(results3_2$reg$reg.pooled.2)$coefficients[c("public", "income", "health", "satlifenow",  "spouse", "edu"),]
results3_2$x$x.mean.diff["spouse"]
# 남성의 수입은 여성의 수입에 비해 평균적 1913만원 높은 것으로 나타남
results3_2$beta$beta.diff["age"]
# 나이가 미치는 직무만족의 차이의 영향은 부정적이다. 즉, 나이가 커질 수록, 직무만족에 미치는 영향은
# 남성에게 더 작아지는 것으로 나타났다.
# 나이가 미치는 영향은 직무만족의 차이의 상당한 부분을 차지 하는 것으로 나타났다. 

results3_2$twofold$overall
plot(results3_2, decomposition = "twofold", group.weight =-1,variables=c("public", "income", "health", "satlifenow",  "age", "edu"))



plot(results3_2, decomposition = "twofold", group.weight = -1,
     unexplained.split = TRUE, 
     component.left = TRUE,
     components = c("unexplained A", "unexplained B"), 
     component.labels = c("unexplained A" = "Against Male", "unexplained B" = "In Favor of Female"),
     variables = c("public", "health", "satlifenow", "spouse"), 
     variable.labels = c("public" = "Public", "health" = "Health", "satlifenow" = "Life Satisfaction", "spouse" = "Spouse"))


variables <- c("public", "health", "satlifenow")
columns <- c("group.weight", "coef(unexplained A)", "coef(unexplained B)")
results3_2$twofold$variables[[5]][variables, columns]




# Q1

results4 <- oaxaca(formula = Q1 ~  female + income + health + satlifenow +  age + edu +  year_2004  + year_2005  + year_2006  + year_2007  + year_2008  + year_2009  + year_2010  + year_2011  + year_2012 + year_2013
                   + year_2014  + year_2015  + year_2016  + year_2017  + year_2018  + year_2019  + year_2020  + year_2021  + year_2022 + year_2023
                   | public | female , data= mdat2, R=100)
plot(results4, components = c("endowments"))

results4$n
# n.A 비공직 12472 공직 5698, 전체 18170
# y.A 비공직 3.71 공직 3.89 차이는 -0.16
results4$y
results4$threefold$overall
# 0.11(집단간 차이, 나이, 수입, 성역할인식)
# -0.06은 계수에서의 차이
# -0.03은 이 둘간의 상호작용

plot(results4, components = c("endowments"), variables=c("female", "income", "health", "satlifenow",  "age", "edu"))

plot(results4, components = c("coefficients"), variables=c("female", "income", "health", "satlifenow",  "age", "edu"))

summary(results4$reg$reg.pooled.2)
summary(results4$reg$reg.pooled.2)$coefficients

results4$x$x.mean.diff[1:7]
results4$beta$beta.diff[1:7]
# 기여요소에서, 나이는 영향이 없다. 하지만, 
# 수입이 크면 만족도에 긍정적 영향을 미치고, 
# 대부분은 남녀 직무만족의 차이는 수입이큰 개인의 비율에서 집단적인 차이에서 기인함

# 성역할인식에서 남성우월 관점은 부의 영향을
# 양성평

summary(results4$reg$reg.pooled.2)$coefficients[c("female", "income", "health", "satlifenow",  "age", "edu"),]
results4$x$x.mean.diff["income"]
# 남성의 수입은 여성의 수입에 비해 평균적 1913만원 높은 것으로 나타남
results4$beta$beta.diff["age"]
# 나이가 미치는 직무만족의 차이의 영향은 부정적이다. 즉, 나이가 커질 수록, 직무만족에 미치는 영향은
# 남성에게 더 작아지는 것으로 나타났다.
# 나이가 미치는 영향은 직무만족의 차이의 상당한 부분을 차지 하는 것으로 나타났다. 

results4$twofold$overall
plot(results4, decomposition = "twofold", group.weight =-1,variables=c("female", "income", "health", "satlifenow",  "age", "edu"))


plot(results4, decomposition = "twofold", group.weight = -1,
     unexplained.split = TRUE, 
     components = c("unexplained A", "unexplained B"), 
     component.labels = c("unexplained A" = "AgainstPrivate Workers", "unexplained B" = "In Favor of  the Public Employees"),
     variables = c("female", "health", "satlifenow"), 
     variable.labels = c("female" = "Female", "health" = "Health", "satlifenow" = "Life Satisfaction"))
plot(results4, decomposition = "twofold", group.weight = -1,
     unexplained.split = TRUE, 
     component.left = TRUE,
     components = c("unexplained A", "unexplained B"), 
     component.labels = c("unexplained A" = "Against Private Workers", "unexplained B" = "In Favor of the Public Employees"),
     variables = c("female", "health", "satlifenow"), 
     variable.labels = c("female" = "Female", "health" = "Health", "satlifenow" = "Life Satisfaction"))


variables <- c("female", "health", "satlifenow")
columns <- c("group.weight", "coef(unexplained A)", "coef(unexplained B)")
results4$twofold$variables[[5]][variables, columns]




# Q2

results5 <- oaxaca(formula = Q2 ~  female + income + health + satlifenow +  age + edu +  year_2004  + year_2005  + year_2006  + year_2007  + year_2008  + year_2009  + year_2010  + year_2011  + year_2012 + year_2013
                   + year_2014  + year_2015  + year_2016  + year_2017  + year_2018  + year_2019  + year_2020  + year_2021  + year_2022 + year_2023
                   | public | female , data= mdat2, R=100)
plot(results5, components = c("endowments"))

results5$n
# n.A 비공직 12472 공직 5698, 전체 18170
# y.A 비공직 3.71 공직 3.89 차이는 -0.16
results5$y
results5$threefold$overall
# 0.11(집단간 차이, 나이, 수입, 성역할인식)
# -0.06은 계수에서의 차이
# -0.03은 이 둘간의 상호작용

plot(results5, components = c("endowments"), variables=c("female", "income", "health", "satlifenow",  "age", "edu"))

plot(results5, components = c("coefficients"), variables=c("female", "income", "health", "satlifenow",  "age", "edu"))

summary(results5$reg$reg.pooled.2)
summary(results5$reg$reg.pooled.2)$coefficients

results5$x$x.mean.diff[1:7]
results5$beta$beta.diff[1:7]
# 기여요소에서, 나이는 영향이 없다. 하지만, 
# 수입이 크면 만족도에 긍정적 영향을 미치고, 
# 대부분은 남녀 직무만족의 차이는 수입이큰 개인의 비율에서 집단적인 차이에서 기인함

# 성역할인식에서 남성우월 관점은 부의 영향을
# 양성평

summary(results5$reg$reg.pooled.2)$coefficients[c("female", "income", "health", "satlifenow",  "age", "edu"),]
results5$x$x.mean.diff["income"]
# 남성의 수입은 여성의 수입에 비해 평균적 1913만원 높은 것으로 나타남
results5$beta$beta.diff["age"]
# 나이가 미치는 직무만족의 차이의 영향은 부정적이다. 즉, 나이가 커질 수록, 직무만족에 미치는 영향은
# 남성에게 더 작아지는 것으로 나타났다.
# 나이가 미치는 영향은 직무만족의 차이의 상당한 부분을 차지 하는 것으로 나타났다. 

results5$twofold$overall
plot(results5, decomposition = "twofold", group.weight =-1,variables=c("female", "income", "health", "satlifenow",  "age", "edu"))


plot(results5, decomposition = "twofold", group.weight = -1,
     unexplained.split = TRUE, 
     components = c("unexplained A", "unexplained B"), 
     component.labels = c("unexplained A" = "AgainstPrivate Workers", "unexplained B" = "In Favor of  the Public Employees"),
     variables = c("female", "health", "satlifenow"), 
     variable.labels = c("female" = "Female", "health" = "Health", "satlifenow" = "Life Satisfaction"))
plot(results5, decomposition = "twofold", group.weight = -1,
     unexplained.split = TRUE, 
     component.left = TRUE,
     components = c("unexplained A", "unexplained B"), 
     component.labels = c("unexplained A" = "Against Private Workers", "unexplained B" = "In Favor of the Public Employees"),
     variables = c("female", "health", "income", "satlifenow"), 
     variable.labels = c("female" = "Female", "health" = "Health", "income"="Income", "satlifenow" = "Life Satisfaction"))


variables <- c("female", "health", "satlifenow")
columns <- c("group.weight", "coef(unexplained A)", "coef(unexplained B)")
results5$twofold$variables[[5]][variables, columns]




# Q3

results6 <- oaxaca(formula = Q3 ~  female + income + health + satlifenow +  age + edu +  year_2004  + year_2005  + year_2006  + year_2007  + year_2008  + year_2009  + year_2010  + year_2011  + year_2012 + year_2013
                   + year_2014  + year_2015  + year_2016  + year_2017  + year_2018  + year_2019  + year_2020  + year_2021  + year_2022 + year_2023
                   | public | female , data= mdat2, R=100)

results6$n
# n.A 비공직 12472 공직 5698, 전체 18170
# y.A 비공직 3.71 공직 3.89 차이는 -0.16
results6$y
results6$threefold$overall
# 0.11(집단간 차이, 나이, 수입, 성역할인식)
# -0.06은 계수에서의 차이
# -0.03은 이 둘간의 상호작용

plot(results6, components = c("endowments"), variables=c("female", "income", "health", "satlifenow",  "age", "edu"))

plot(results6, components = c("coefficients"), variables=c("female", "income", "health", "satlifenow",  "age", "edu"))

summary(results6$reg$reg.pooled.2)
summary(results6$reg$reg.pooled.2)$coefficients

results6$x$x.mean.diff[1:7]
results6$beta$beta.diff[1:7]
# 기여요소에서, 나이는 영향이 없다. 하지만, 
# 수입이 크면 만족도에 긍정적 영향을 미치고, 
# 대부분은 남녀 직무만족의 차이는 수입이큰 개인의 비율에서 집단적인 차이에서 기인함

# 성역할인식에서 남성우월 관점은 부의 영향을
# 양성평

summary(results6$reg$reg.pooled.2)$coefficients[c("female", "income", "health", "satlifenow",  "age", "edu"),]
results6$x$x.mean.diff["income"]
# 남성의 수입은 여성의 수입에 비해 평균적 1913만원 높은 것으로 나타남
results6$beta$beta.diff["age"]
# 나이가 미치는 직무만족의 차이의 영향은 부정적이다. 즉, 나이가 커질 수록, 직무만족에 미치는 영향은
# 남성에게 더 작아지는 것으로 나타났다.
# 나이가 미치는 영향은 직무만족의 차이의 상당한 부분을 차지 하는 것으로 나타났다. 

results6$twofold$overall
plot(results6, decomposition = "twofold", group.weight =-1,variables=c("female", "income", "health", "satlifenow",  "age", "edu"))

plot(results6, decomposition = "twofold", group.weight = -1,
     unexplained.split = TRUE, 
     component.left = TRUE,
     components = c("unexplained A", "unexplained B"), 
     component.labels = c("unexplained A" = "Against Private Workers", "unexplained B" = "In Favor of the Public Employees"),
     variables = c("female", "health", "income", "satlifenow"), 
     variable.labels = c("female" = "Female", "health" = "Health", "income"="Income", "satlifenow" = "Life Satisfaction"))


variables <- c("female", "health", "satlifenow")
columns <- c("group.weight", "coef(unexplained A)", "coef(unexplained B)")
results6$twofold$variables[[5]][variables, columns]



# Q4

results7 <- oaxaca(formula = Q4 ~  female + income + health + satlifenow +  age + edu +  year_2004  + year_2005  + year_2006  + year_2007  + year_2008  + year_2009  + year_2010  + year_2011  + year_2012 + year_2013
                   + year_2014  + year_2015  + year_2016  + year_2017  + year_2018  + year_2019  + year_2020  + year_2021  + year_2022 + year_2023
                   | public | female , data= mdat2, R=100)

results7$n
# n.A 비공직 12472 공직 5698, 전체 18170
# y.A 비공직 3.71 공직 3.89 차이는 -0.16
results7$y
results7$threefold$overall
# 0.11(집단간 차이, 나이, 수입, 성역할인식)
# -0.06은 계수에서의 차이
# -0.03은 이 둘간의 상호작용

plot(results7, components = c("endowments"), variables=c("female", "income", "health", "satlifenow",  "age", "edu"))

plot(results7, components = c("coefficients"), variables=c("female", "income", "health", "satlifenow",  "age", "edu"))

summary(results7$reg$reg.pooled.2)
summary(results7$reg$reg.pooled.2)$coefficients

results7$x$x.mean.diff[1:7]
results7$beta$beta.diff[1:7]
# 기여요소에서, 나이는 영향이 없다. 하지만, 
# 수입이 크면 만족도에 긍정적 영향을 미치고, 
# 대부분은 남녀 직무만족의 차이는 수입이큰 개인의 비율에서 집단적인 차이에서 기인함

# 성역할인식에서 남성우월 관점은 부의 영향을
# 양성평

summary(results7$reg$reg.pooled.2)$coefficients[c("female", "income", "health", "satlifenow",  "age", "edu"),]
results7$x$x.mean.diff["income"]
# 남성의 수입은 여성의 수입에 비해 평균적 1913만원 높은 것으로 나타남
results7$beta$beta.diff["age"]
# 나이가 미치는 직무만족의 차이의 영향은 부정적이다. 즉, 나이가 커질 수록, 직무만족에 미치는 영향은
# 남성에게 더 작아지는 것으로 나타났다.
# 나이가 미치는 영향은 직무만족의 차이의 상당한 부분을 차지 하는 것으로 나타났다. 

results7$twofold$overall
plot(results7, decomposition = "twofold", group.weight =-1,variables=c("female", "income", "health", "satlifenow",  "age", "edu"))

plot(results7, decomposition = "twofold", group.weight = -1,
     unexplained.split = TRUE, 
     component.left = TRUE,
     components = c("unexplained A", "unexplained B"), 
     component.labels = c("unexplained A" = "Against Private Workers", "unexplained B" = "In Favor of the Public Employees"),
     variables = c("female", "health", "income", "satlifenow"), 
     variable.labels = c("female" = "Female", "health" = "Health", "income"="Income", "satlifenow" = "Life Satisfaction"))


variables <- c("female", "health", "satlifenow")
columns <- c("group.weight", "coef(unexplained A)", "coef(unexplained B)")
results7$twofold$variables[[5]][variables, columns]

