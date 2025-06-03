# The American Viewer: Political Consequences of Entertainment Media
# Eunji Kim and Shawn Patterson Jr 
# American Political Science Review 
# Replication File [Table 2]

library(foreign)
library(lfe)
library(haven)

# load table.2 csv file 


table2 <- read.csv("table2_censored.csv")

# removing proprietary nielsen ratings data 
#table2_censored <- table2 %>% select(-c(RTG_9PM_Nov_May, RTG_8PM_Nov_May))

## first stage results (full)

### column 1 

column1 <- felm(RTG_9PM_Nov_May ~ RTG_8PM_Nov_May +  
                  log(population_04 +1) +
                  log(medianHHI_04+1) + femaleP_04 + age65P_04 + whiteP_04 + blackP_04  +
                  collegeP_04 + foreignBP_04 + unemR_04   + samesex_census  + religion_allP + 
                  log(avg_outflow_movers + 1) + log(avg_inflow_movers + 1 ) + pop_density.x   | 
                  STATE  | 0 | 0 ,  data = table2, weights=table2$sum_CNTYTVHH) 
summary(column1)
stargazer(column1)

stargazer(column1,   
   star.cutoffs = c(.05,.01,.001),
   title="First Stage Results",
   dep.var.caption = "",
   label = "firststage",
   omit.stat=c("adj.rsq","LL","ser","f"),
   keep.stat = c("n"),
   no.space=TRUE,
   omit = c('STATE'),
   star.char = c("*", "**", "***"), 
   notes = c("* p<0.05; ** p<0.01; *** p<0.001"), 
   notes.append = F,
   notes.align="l",
   digits=3,
   type="text",
   align = TRUE)


# primary vote share, OLS column (2)

ols_pri <- felm(trumpshare_primary~  RTG_9PM_Nov_May + romneyshare12_new + 
                  log(population_04 +1) +
                  log(medianHHI_04+1) + femaleP_04 + age65P_04 + whiteP_04 + blackP_04  +
                  collegeP_04 + foreignBP_04 + unemR_04   + samesex_census  + religion_allP +
                  log(avg_outflow_movers + 1) + log(avg_inflow_movers + 1 ) + pop_density.x   | 
                  STATE  | 0 | 0 , 
                data = table2, weights=table2$sum_CNTYTVHH) 

stargazer(ols_pri, no.space=TRUE,type="text" )

# primary vote share, IV (column (3)) 

iv_pri<- felm(trumpshare_primary~  romneyshare12_new + 
                log(population_04 +1) +
                log(medianHHI_04+1) + femaleP_04 + age65P_04 + whiteP_04 + blackP_04  +
                collegeP_04 + foreignBP_04 + unemR_04    +  samesex_census  + religion_allP +
                log(avg_outflow_movers + 1) + log(avg_inflow_movers + 1 ) + pop_density.x  | 
                STATE  | (RTG_9PM_Nov_May  ~ RTG_8PM_Nov_May)| 0 , 
              data = table2, weights=table2$sum_CNTYTVHH) 

stargazer(iv_pri,  no.space=TRUE,type="text")
# F statistic 
iv_pri$stage1$iv1fstat$RTG_9PM_Nov_May["F"]


# general vote share, OLS, column (4) 
ols_prez <- felm(trumpshare16_new ~ RTG_9PM_Nov_May + romneyshare12_new + 
                   log(population_04 +1) +
                   log(medianHHI_04+1) + femaleP_04 + age65P_04 + whiteP_04 + blackP_04  +
                   collegeP_04 + foreignBP_04 + unemR_04   + samesex_census  + religion_allP +
                   log(avg_outflow_movers + 1) + log(avg_inflow_movers + 1 ) + pop_density.x   | 
                   STATE  | 0 | 0 ,  data = table2, weights=table2$sum_CNTYTVHH) 

stargazer(ols_prez,  no.space=TRUE,type="text")


# general vote share, IV (column (5)) 

iv_prez<- felm(trumpshare16_new~  romneyshare12_new + 
                 log(population_04 +1) +
                 log(medianHHI_04+1) + femaleP_04 + age65P_04 + whiteP_04 + blackP_04  +
                 collegeP_04 + foreignBP_04 + unemR_04    +  samesex_census  + religion_allP +
                 log(avg_outflow_movers + 1) + log(avg_inflow_movers + 1 ) + pop_density.x  | 
                 STATE  | (RTG_9PM_Nov_May  ~ RTG_8PM_Nov_May)| 0 , 
               data = table2, weights=table2$sum_CNTYTVHH) 

stargazer(iv_prez, no.space=TRUE,type="text")
#F statistic 
iv_prez$stage1$iv1fstat$RTG_9PM_Nov_May["F"]


## full results, columns 2 to 5

stargazer(ols_pri,  iv_pri,  ols_prez, iv_prez,  
       star.cutoffs = c(.05,.01,.001),
       title="Table 2 Columns 2-5 Full Results",
       dep.var.caption = "",
       label = "tab2",
       dep.var.labels=c('Primary - OLS',  'Primary - IV', 
                        'General - OLS', 'General - IV'),
       omit.stat=c("adj.rsq","LL","ser","f"),
       keep.stat = c("n"),
       no.space=TRUE,
       omit = c('STATE'),
       star.char = c("*", "**", "***"), 
       notes = c("* p<0.05; ** p<0.01; *** p<0.001"), 
       notes.append = F,
       notes.align="l",
       digits=3,
        type="text",
         align = TRUE)