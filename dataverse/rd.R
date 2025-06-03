# Replication Data for: "How Partisanship in Cities Influences Housing Policy"

# This file accompanies the datasets and replication code to reproduce the results in ?How Partisanship in Cities Influences Housing Policy,? by Justin de Benedictis-Kessner, Daniel Jones, and Christopher Warshaw.

## Preamble --------------------------------------------------------------------

library(tidyverse)
library(rdrobust)
library(rdlocrand)
library(rddensity)
library(foreign)
library(stargazer)
library(lfe)
library(janitor)
library(xtable)

## functions to convert rd.robust output for figures/tables:
source("rd.export.R")

# lead and lag functions that account for missing years
lag.new <- function(x, n = 1L, along_with){
  index <- match(along_with - n, along_with, incomparable = NA)
  out <- x[index]
  attributes(out) <- attributes(x)
  out
}

lead.new <- function(x, n = 1L, along_with){
  index <- match(along_with + n, along_with, incomparable = NA)
  out <- x[index]
  attributes(out) <- attributes(x)
  out
}
## for weighted RD plots:
tri <- function (x, h, c=0) pmax(0, 1 - abs((x - c) / h))

## custom colors:
vpurple = "#440154FF"
vyellow = "#FDE725FF"
vgreen = "#21908CFF"


## ------------------ ##
## Read data -------------------------------------------------------------------
## ------------------ ##
data_2p <- read_rds(file = "data_mayors_2p_post1990.rds")


data_2p <- data_2p %>%
  rowwise() %>%
  mutate(
    total_units_lead23avg = mean(c(total_units_lead2, total_units_lead3), na.rm=T),
    total_units_multi_ln_lead23avg = mean(c(total_units_multi_ln_lead2, total_units_multi_ln_lead3), na.rm=T),
    total_units_multi_pc_lead23avg = mean(c(total_units_multi_pc_lead2, total_units_multi_pc_lead3), na.rm=T)
  )




## Analysis --------------------------------------------------------------------

## ------------------ ##
### CBPS Outcomes --------------------------------------------------------------
## ------------------ ##


## Delta units outcomes:
fit_units_delta1 <- with(data_2p,
                         rdrobust(y = total_units_ln_delta1,x = demshare,
                                  c = 0.5,cluster = cluster))
summary(fit_units_delta1)

fit_units_delta2 <- with(data_2p,
                         rdrobust(y = total_units_ln_delta2,x = demshare,
                                  c = 0.5,cluster = cluster))
summary(fit_units_delta2)

fit_units_delta3 <- with(data_2p,
                         rdrobust(y = total_units_ln_delta3,x = demshare,
                                  c = 0.5,cluster = cluster))
summary(fit_units_delta3)


fit_units_delta23avg <- with(data_2p,
                         rdrobust(y = total_units_ln_delta23avg,x = demshare,
                                  c = 0.5,cluster = cluster))
summary(fit_units_delta23avg)


## Delta bldgs outcomes:
fit_bldgs_delta1 <- with(data_2p,
                         rdrobust(y = total_bldgs_ln_delta1,x = demshare,
                                  c = 0.5,cluster = cluster))

summary(fit_bldgs_delta1)

fit_bldgs_delta2 <- with(data_2p,
                         rdrobust(y = total_bldgs_ln_delta2,x = demshare,
                                  c = 0.5,cluster = cluster))

summary(fit_bldgs_delta2)

fit_bldgs_delta3 <- with(data_2p,
                         rdrobust(y = total_bldgs_ln_delta3,x = demshare,
                                  c = 0.5,cluster = cluster))

summary(fit_bldgs_delta3)


fit_bldgs_delta23avg <- with(data_2p,
                         rdrobust(y = total_bldgs_ln_delta23avg,x = demshare,
                                  c = 0.5,cluster = cluster))

summary(fit_bldgs_delta23avg)



## Delta multiunit units outcomes:
fit_units_multi_delta1 <- with(data_2p,
                               rdrobust(y = total_units_multi_ln_delta1,x = demshare,
                                        c = 0.5,cluster = cluster))

summary(fit_units_multi_delta1)

fit_units_multi_delta2 <- with(data_2p,
                               rdrobust(y = total_units_multi_ln_delta2,x = demshare,
                                        c = 0.5,cluster = cluster))

summary(fit_units_multi_delta2)

fit_units_multi_delta3 <- with(data_2p,
                               rdrobust(y = total_units_multi_ln_delta3,x = demshare,
                                        c = 0.5,cluster = cluster))

summary(fit_units_multi_delta3)
fit_units_multi_delta23avg <- with(data_2p,
                               rdrobust(y = total_units_multi_ln_delta23avg,x = demshare,
                                        c = 0.5,cluster = cluster))

summary(fit_units_multi_delta23avg)
writeLines(as.character(round(fit_units_multi_delta23avg$coef['Conventional', ], 2)),"mayors/rd_effect_units_multi_delta23avg.tex",sep = "%")



## Delta singleunit units outcomes:
fit_units_single_delta1 <- with(data_2p,
                                rdrobust(y = total_units_single_ln_delta1,x = demshare,
                                         c = 0.5,cluster = cluster))

summary(fit_units_single_delta1)

fit_units_single_delta2 <- with(data_2p,
                                rdrobust(y = total_units_single_ln_delta2,x = demshare,
                                         c = 0.5,cluster = cluster))

summary(fit_units_single_delta2)

fit_units_single_delta3 <- with(data_2p,
                                rdrobust(y = total_units_single_ln_delta3,x = demshare,
                                         c = 0.5,cluster = cluster))

summary(fit_units_single_delta3)
fit_units_single_delta23avg <- with(data_2p,
                                    rdrobust(y = total_units_single_ln_delta23avg,x = demshare,
                                             c = 0.5,cluster = cluster))
summary(fit_units_single_delta23avg)

## Delta mutiunit bldgs outcomes:
fit_bldgs_multi_delta1 <- with(data_2p,
                         rdrobust(y = total_bldgs_multi_ln_delta1,x = demshare,
                                  c = 0.5,cluster = cluster
                                  ))

summary(fit_bldgs_multi_delta1)

fit_bldgs_multi_delta2 <- with(data_2p,
                         rdrobust(y = total_bldgs_multi_ln_delta2,x = demshare,
                                  c = 0.5,cluster = cluster
                                  ))

summary(fit_bldgs_multi_delta2)

fit_bldgs_multi_delta3 <- with(data_2p,
                         rdrobust(y = total_bldgs_multi_ln_delta3,x = demshare,
                                  c = 0.5,cluster = cluster
                                  ))
summary(fit_bldgs_multi_delta3)

fit_bldgs_multi_delta23avg <- with(data_2p,
                               rdrobust(y = total_bldgs_multi_ln_delta23avg,x = demshare,
                                        c = 0.5,cluster = cluster
                                        ))
summary(fit_bldgs_multi_delta23avg)


## Delta mutiunit bldgs outcomes:
fit_bldgs_single_delta1 <- with(data_2p,
                                rdrobust(y = total_bldgs_single_ln_delta1,x = demshare,
                                         c = 0.5,cluster = cluster
                                ))
summary(fit_bldgs_single_delta1)

fit_bldgs_single_delta2 <- with(data_2p,
                                rdrobust(y = total_bldgs_single_ln_delta2,x = demshare,
                                         c = 0.5,cluster = cluster
                                ))
summary(fit_bldgs_single_delta2)



fit_bldgs_single_delta3 <- with(data_2p,
                                rdrobust(y = total_bldgs_single_ln_delta3,x = demshare,
                                         c = 0.5,cluster = cluster
                                ))
summary(fit_bldgs_single_delta3)

fit_bldgs_single_delta23avg <- with(data_2p,
                                    rdrobust(y = total_bldgs_single_ln_delta23avg,x = demshare,
                                             c = 0.5,cluster = cluster
                                    ))
summary(fit_bldgs_single_delta23avg)
fit_bldgs_single_delta23avg_90 <- with(data_2p,
                                    rdrobust(y = total_bldgs_single_ln_delta23avg,x = demshare,
                                             c = 0.5,cluster = cluster,level=90
                                    ))


## Proportion of housing:
fit_ratio_bldgs_multisingle_delta1 <- with(data_2p,
                                               rdrobust(y = ratio_bldgs_multisingle_delta1,x = demshare,
                                                        c = 0.5,cluster = cluster
                                               ))
summary(fit_ratio_bldgs_multisingle_delta1)
fit_ratio_bldgs_multisingle_delta2 <- with(data_2p,
                                           rdrobust(y = ratio_bldgs_multisingle_delta2,x = demshare,
                                                    c = 0.5,cluster = cluster
                                           ))
summary(fit_ratio_bldgs_multisingle_delta2)
fit_ratio_bldgs_multisingle_delta3 <- with(data_2p,
                                           rdrobust(y = ratio_bldgs_multisingle_delta3,x = demshare,
                                                    c = 0.5,cluster = cluster
                                           ))
summary(fit_ratio_bldgs_multisingle_delta3)

fit_ratio_bldgs_multisingle_delta23avg <- with(data_2p,
                                   rdrobust(y = ratio_bldgs_multisingle_delta23avg,x = demshare,
                                            c = 0.5,cluster = cluster
                                            ))

summary(fit_ratio_bldgs_multisingle_delta23avg)

fit_ratio_units_multisingle_delta1 <- with(data_2p,
                                           rdrobust(y = ratio_units_multisingle_delta1,x = demshare,
                                                    c = 0.5, cluster = cluster
                                           ))
summary(fit_ratio_units_multisingle_delta1)
fit_ratio_units_multisingle_delta2 <- with(data_2p,
                                               rdrobust(y = ratio_units_multisingle_delta2,x = demshare,
                                                        c = 0.5, cluster = cluster
                                               ))
summary(fit_ratio_units_multisingle_delta2)
fit_ratio_units_multisingle_delta3 <- with(data_2p,
                                           rdrobust(y = ratio_units_multisingle_delta3,x = demshare,
                                                    c = 0.5, cluster = cluster
                                           ))
summary(fit_ratio_units_multisingle_delta3)

fit_ratio_units_multisingle_delta23avg <- with(data_2p,
                                                  rdrobust(y = ratio_units_multisingle_delta23avg,x = demshare,
                                                           c = 0.5, cluster = cluster
                                                           ))
summary(fit_ratio_units_multisingle_delta23avg)
writeLines(as.character(100*round(fit_ratio_units_multisingle_delta23avg$coef['Conventional', ], 2)),"mayors/rd_effect_ratio_units_multisingle_delta23avg.tex",sep = "%")

fit_ratio_units_multisingle_delta234avg <- with(data_2p,
                                               rdrobust(y = ratio_units_multisingle_delta234avg,x = demshare,
                                                        c = 0.5, cluster = cluster
                                               ))

fit_ratio_units_multisingle_delta1234avg <- with(data_2p,
                                                rdrobust(y = ratio_units_multisingle_delta1234avg,x = demshare,
                                                         c = 0.5, cluster = cluster
                                                ))


#### term-level deltas ####
fit_units_deltaterm4avg <- with(filter(data_2p),
                                rdrobust(y = total_units_ln_deltaterm4avg,x = demshare,
                                         c = 0.5,cluster = cluster
                                ))
fit_bldgs_deltaterm4avg <- with(filter(data_2p),
                                rdrobust(y = total_bldgs_ln_deltaterm4avg,x = demshare,
                                         c = 0.5,cluster = cluster
                                ))

fit_units_single_deltaterm4avg <- with(filter(data_2p),
                                       rdrobust(y = total_units_single_ln_deltaterm4avg,x = demshare,
                                                c = 0.5,cluster = cluster
                                       ))
fit_bldgs_single_deltaterm4avg <- with(filter(data_2p),
                                       rdrobust(y = total_bldgs_single_ln_deltaterm4avg,x = demshare,
                                                c = 0.5,cluster = cluster
                                       ))

fit_units_multi_deltaterm4avg <- with(filter(data_2p),
                                      rdrobust(y = total_units_multi_ln_deltaterm4avg,x = demshare,
                                               c = 0.5,cluster = cluster
                                      ))
fit_bldgs_multi_deltaterm4avg <- with(filter(data_2p),
                                      rdrobust(y = total_bldgs_multi_ln_deltaterm4avg,x = demshare,
                                               c = 0.5,cluster = cluster
                                      ))
fit_ratio_bldgs_multisingle_deltaterm4avg <- with(filter(data_2p),
                                                  rdrobust(y = ratio_bldgs_multisingle_deltaterm4avg,x = demshare,
                                                           c = 0.5,cluster = cluster
                                                  ))

fit_ratio_units_multisingle_deltaterm4avg <- with(filter(data_2p),
                                                  rdrobust(y = ratio_units_multisingle_deltaterm4avg,x = demshare,
                                                           c = 0.5, cluster = cluster
                                                  ))
fit_ratio_units_multisingle_deltaterm234avg <- with(filter(data_2p),
                                                  rdrobust(y = ratio_units_multisingle_deltaterm234avg,x = demshare,
                                                           c = 0.5, cluster = cluster
                                                  ))




### Heterogeneity --------------------------------------------------------------

#### by FOG ####
fit_may_units_delta23avg <- with(filter(data_2p,fog==1),
                                 rdrobust(y = total_units_ln_delta23avg,x = demshare,
                                          c = 0.5,cluster = cluster
                                          ))
fit_may_bldgs_delta23avg <- with(filter(data_2p,fog==1),
                                 rdrobust(y = total_bldgs_ln_delta23avg,x = demshare,
                                          c = 0.5,cluster = cluster
                                          ))

fit_may_units_multi_delta23avg <- with(filter(data_2p,fog==1),
                                       rdrobust(y = total_units_multi_ln_delta23avg,x = demshare,
                                                c = 0.5,cluster = cluster
                                                ))
fit_may_bldgs_multi_delta23avg <- with(filter(data_2p,fog==1),
                                       rdrobust(y = total_bldgs_multi_ln_delta23avg,x = demshare,
                                                c = 0.5,cluster = cluster
                                                ))
fit_may_ratio_bldgs_multisingle_delta23avg <- with(filter(data_2p,fog==1),
                                                   rdrobust(y = ratio_bldgs_multisingle_delta23avg,x = demshare,
                                                            c = 0.5,cluster = cluster
                                                            ))

fit_may_ratio_units_multisingle_delta23avg <- with(filter(data_2p,fog==1),
                                                   rdrobust(y = ratio_units_multisingle_delta23avg,x = demshare,
                                                            c = 0.5, cluster = cluster
                                                            ))

fit_man_units_delta23avg <- with(filter(data_2p,fog==2),
                                 rdrobust(y = total_units_ln_delta23avg,x = demshare,
                                          c = 0.5,cluster = cluster
                                          ))
fit_man_bldgs_delta23avg <- with(filter(data_2p,fog==2),
                                 rdrobust(y = total_bldgs_ln_delta23avg,x = demshare,
                                          c = 0.5,cluster = cluster
                                          ))

fit_man_units_multi_delta23avg <- with(filter(data_2p,fog==2),
                                       rdrobust(y = total_units_multi_ln_delta23avg,x = demshare,
                                                c = 0.5,cluster = cluster
                                                ))
fit_man_bldgs_multi_delta23avg <- with(filter(data_2p,fog==2),
                                       rdrobust(y = total_bldgs_multi_ln_delta23avg,x = demshare,
                                                c = 0.5,cluster = cluster
                                                ))
fit_man_ratio_bldgs_multisingle_delta23avg <- with(filter(data_2p,fog==2),
                                                   rdrobust(y = ratio_bldgs_multisingle_delta23avg,x = demshare,
                                                            c = 0.5,cluster = cluster
                                                            ))

fit_man_ratio_units_multisingle_delta23avg <- with(filter(data_2p,fog==2),
                                                   rdrobust(y = ratio_units_multisingle_delta23avg,x = demshare,
                                                            c = 0.5, cluster = cluster
                                                            ))

####  by Districts vs At-large ####
fit_district_units_delta23avg <- with(filter(data_2p,districts=="districts"),
                                      rdrobust(y = total_units_ln_delta23avg,x = demshare,
                                               c = 0.5,cluster = cluster
                                      ))
fit_district_bldgs_delta23avg <- with(filter(data_2p,districts=="districts"),
                                      rdrobust(y = total_bldgs_ln_delta23avg,x = demshare,
                                               c = 0.5,cluster = cluster
                                      ))

fit_district_units_multi_delta23avg <- with(filter(data_2p,districts=="districts"),
                                            rdrobust(y = total_units_multi_ln_delta23avg,x = demshare,
                                                     c = 0.5,cluster = cluster
                                            ))
fit_district_bldgs_multi_delta23avg <- with(filter(data_2p,districts=="districts"),
                                            rdrobust(y = total_bldgs_multi_ln_delta23avg,x = demshare,
                                                     c = 0.5,cluster = cluster
                                            ))
fit_district_ratio_bldgs_multisingle_delta23avg <- with(filter(data_2p,districts=="districts"),
                                                        rdrobust(y = ratio_bldgs_multisingle_delta23avg,x = demshare,
                                                                 c = 0.5,cluster = cluster
                                                        ))

fit_district_ratio_units_multisingle_delta23avg <- with(filter(data_2p,districts=="districts"),
                                                        rdrobust(y = ratio_units_multisingle_delta23avg,x = demshare,
                                                                 c = 0.5, cluster = cluster
                                                        ))

fit_atlarge_units_delta23avg <- with(filter(data_2p,districts=="at-large"),
                                     rdrobust(y = total_units_ln_delta23avg,x = demshare,
                                              c = 0.5,cluster = cluster
                                     ))
fit_atlarge_bldgs_delta23avg <- with(filter(data_2p,districts=="at-large"),
                                     rdrobust(y = total_bldgs_ln_delta23avg,x = demshare,
                                              c = 0.5,cluster = cluster
                                     ))

fit_atlarge_units_multi_delta23avg <- with(filter(data_2p,districts=="at-large"),
                                           rdrobust(y = total_units_multi_ln_delta23avg,x = demshare,
                                                    c = 0.5,cluster = cluster
                                           ))
fit_atlarge_bldgs_multi_delta23avg <- with(filter(data_2p,districts=="at-large"),
                                           rdrobust(y = total_bldgs_multi_ln_delta23avg,x = demshare,
                                                    c = 0.5,cluster = cluster
                                           ))
fit_atlarge_ratio_bldgs_multisingle_delta23avg <- with(filter(data_2p,districts=="at-large"),
                                                       rdrobust(y = ratio_bldgs_multisingle_delta23avg,x = demshare,
                                                                c = 0.5,cluster = cluster
                                                       ))

fit_atlarge_ratio_units_multisingle_delta23avg <- with(filter(data_2p,districts=="at-large"),
                                                       rdrobust(y = ratio_units_multisingle_delta23avg,x = demshare,
                                                                c = 0.5, cluster = cluster
                                                       ))


fit_combo_units_delta23avg <- with(filter(data_2p,districts=="combo"),
                                   rdrobust(y = total_units_ln_delta23avg,x = demshare,
                                            c = 0.5,cluster = cluster
                                   ))
fit_combo_bldgs_delta23avg <- with(filter(data_2p,districts=="combo"),
                                   rdrobust(y = total_bldgs_ln_delta23avg,x = demshare,
                                            c = 0.5,cluster = cluster
                                   ))

fit_combo_units_multi_delta23avg <- with(filter(data_2p,districts=="combo"),
                                         rdrobust(y = total_units_multi_ln_delta23avg,x = demshare,
                                                  c = 0.5,cluster = cluster
                                         ))
fit_combo_bldgs_multi_delta23avg <- with(filter(data_2p,districts=="combo"),
                                         rdrobust(y = total_bldgs_multi_ln_delta23avg,x = demshare,
                                                  c = 0.5,cluster = cluster
                                         ))
fit_combo_ratio_bldgs_multisingle_delta23avg <- with(filter(data_2p,districts=="combo"),
                                                     rdrobust(y = ratio_bldgs_multisingle_delta23avg,x = demshare,
                                                              c = 0.5,cluster = cluster
                                                     ))

fit_combo_ratio_units_multisingle_delta23avg <- with(filter(data_2p,districts=="combo"),
                                                     rdrobust(y = ratio_units_multisingle_delta23avg,x = demshare,
                                                              c = 0.5, cluster = cluster
                                                     ))




#### by Partisan Elecs ####
writeLines(as.character(round(mean(data_2p$partisan==0,na.rm=T),2)*100),"mayors/perc_elecs_nonpartisan.tex",sep = "%")

fit_par_units_delta23avg <- with(filter(data_2p,partisan==1),
                                 rdrobust(y = total_units_ln_delta23avg,x = demshare,
                                          c = 0.5,cluster = cluster
                                 ))
fit_par_bldgs_delta23avg <- with(filter(data_2p,partisan==1),
                                 rdrobust(y = total_bldgs_ln_delta23avg,x = demshare,
                                          c = 0.5,cluster = cluster
                                 ))

fit_par_units_multi_delta23avg <- with(filter(data_2p,partisan==1),
                                       rdrobust(y = total_units_multi_ln_delta23avg,x = demshare,
                                                c = 0.5,cluster = cluster
                                       ))
fit_par_bldgs_multi_delta23avg <- with(filter(data_2p,partisan==1),
                                       rdrobust(y = total_bldgs_multi_ln_delta23avg,x = demshare,
                                                c = 0.5,cluster = cluster
                                       ))
fit_par_ratio_bldgs_multisingle_delta23avg <- with(filter(data_2p,partisan==1),
                                                   rdrobust(y = ratio_bldgs_multisingle_delta23avg,x = demshare,
                                                            c = 0.5,cluster = cluster
                                                   ))

fit_par_ratio_units_multisingle_delta23avg <- with(filter(data_2p,partisan==1),
                                                   rdrobust(y = ratio_units_multisingle_delta23avg,x = demshare,
                                                            c = 0.5, cluster = cluster
                                                   ))

fit_nonpar_units_delta23avg <- with(filter(data_2p,partisan==0),
                                    rdrobust(y = total_units_ln_delta23avg,x = demshare,
                                             c = 0.5,cluster = cluster
                                    ))
fit_nonpar_bldgs_delta23avg <- with(filter(data_2p,partisan==0),
                                    rdrobust(y = total_bldgs_ln_delta23avg,x = demshare,
                                             c = 0.5,cluster = cluster
                                    ))

fit_nonpar_units_multi_delta23avg <- with(filter(data_2p,partisan==0),
                                          rdrobust(y = total_units_multi_ln_delta23avg,x = demshare,
                                                   c = 0.5,cluster = cluster
                                          ))
fit_nonpar_bldgs_multi_delta23avg <- with(filter(data_2p,partisan==0),
                                          rdrobust(y = total_bldgs_multi_ln_delta23avg,x = demshare,
                                                   c = 0.5,cluster = cluster
                                          ))
fit_nonpar_ratio_bldgs_multisingle_delta23avg <- with(filter(data_2p,partisan==0),
                                                      rdrobust(y = ratio_bldgs_multisingle_delta23avg,x = demshare,
                                                               c = 0.5,cluster = cluster
                                                      ))

fit_nonpar_ratio_units_multisingle_delta23avg <- with(filter(data_2p,partisan==0),
                                                      rdrobust(y = ratio_units_multisingle_delta23avg,x = demshare,
                                                               c = 0.5, cluster = cluster
                                                      ))


#### by Local Council Approval Power ####
fit_approval_units_delta23avg <- with(filter(data_2p,council_approval_norez==1),
                                      rdrobust(y = total_units_ln_delta23avg,x = demshare,
                                               c = 0.5,cluster = cluster
                                      ))
fit_approval_bldgs_delta23avg <- with(filter(data_2p,council_approval_norez==1),
                                      rdrobust(y = total_bldgs_ln_delta23avg,x = demshare,
                                               c = 0.5,cluster = cluster
                                      ))

fit_approval_units_multi_delta23avg <- with(filter(data_2p,council_approval_norez==1),
                                            rdrobust(y = total_units_multi_ln_delta23avg,x = demshare,
                                                     c = 0.5,cluster = cluster
                                            ))
fit_approval_bldgs_multi_delta23avg <- with(filter(data_2p,council_approval_norez==1),
                                            rdrobust(y = total_bldgs_multi_ln_delta23avg,x = demshare,
                                                     c = 0.5,cluster = cluster
                                            ))
fit_approval_units_single_delta23avg <- with(filter(data_2p,council_approval_norez==1),
                                            rdrobust(y = total_units_single_ln_delta23avg,x = demshare,
                                                     c = 0.5,cluster = cluster
                                            ))
fit_approval_bldgs_single_delta23avg <- with(filter(data_2p,council_approval_norez==1),
                                            rdrobust(y = total_bldgs_single_ln_delta23avg,x = demshare,
                                                     c = 0.5,cluster = cluster
                                            ))
fit_approval_ratio_bldgs_multisingle_delta23avg <- with(filter(data_2p,council_approval_norez==1),
                                                        rdrobust(y = ratio_bldgs_multisingle_delta23avg,x = demshare,
                                                                 c = 0.5,cluster = cluster
                                                        ))

fit_approval_ratio_units_multisingle_delta23avg <- with(filter(data_2p,council_approval_norez==1),
                                                        rdrobust(y = ratio_units_multisingle_delta23avg,x = demshare,
                                                                 c = 0.5, cluster = cluster
                                                        ))

fit_noapproval_units_delta23avg <- with(filter(data_2p,council_approval_norez==0),
                                        rdrobust(y = total_units_ln_delta23avg,x = demshare,
                                                 c = 0.5,cluster = cluster
                                        ))
fit_noapproval_bldgs_delta23avg <- with(filter(data_2p,council_approval_norez==0),
                                        rdrobust(y = total_bldgs_ln_delta23avg,x = demshare,
                                                 c = 0.5,cluster = cluster
                                        ))

fit_noapproval_units_multi_delta23avg <- with(filter(data_2p,council_approval_norez==0),
                                              rdrobust(y = total_units_multi_ln_delta23avg,x = demshare,
                                                       c = 0.5,cluster = cluster
                                              ))
fit_noapproval_bldgs_multi_delta23avg <- with(filter(data_2p,council_approval_norez==0),
                                              rdrobust(y = total_bldgs_multi_ln_delta23avg,x = demshare,
                                                       c = 0.5,cluster = cluster
                                              ))
fit_noapproval_units_single_delta23avg <- with(filter(data_2p,council_approval_norez==0),
                                              rdrobust(y = total_units_single_ln_delta23avg,x = demshare,
                                                       c = 0.5,cluster = cluster
                                              ))
fit_noapproval_bldgs_single_delta23avg <- with(filter(data_2p,council_approval_norez==0),
                                              rdrobust(y = total_bldgs_single_ln_delta23avg,x = demshare,
                                                       c = 0.5,cluster = cluster
                                              ))
fit_noapproval_ratio_bldgs_multisingle_delta23avg <- with(filter(data_2p,council_approval_norez==0),
                                                          rdrobust(y = ratio_bldgs_multisingle_delta23avg,x = demshare,
                                                                   c = 0.5,cluster = cluster
                                                          ))

fit_noapproval_ratio_units_multisingle_delta23avg <- with(filter(data_2p,council_approval_norez==0),
                                                          rdrobust(y = ratio_units_multisingle_delta23avg,x = demshare,
                                                                   c = 0.5, cluster = cluster
                                                          ))


# moderation significance check:
fit_approval_ratio_units_multisingle_delta23avg
fit_noapproval_ratio_units_multisingle_delta23avg
(approval_diff <- fit_approval_ratio_units_multisingle_delta23avg$coef[1,1] - fit_noapproval_ratio_units_multisingle_delta23avg$coef[1,1]) # 0.06
Z_approval = (fit_approval_ratio_units_multisingle_delta23avg$coef[1,1] - fit_noapproval_ratio_units_multisingle_delta23avg$coef[1,1])/
  sqrt(fit_approval_ratio_units_multisingle_delta23avg$se[3,1]^2 + fit_noapproval_ratio_units_multisingle_delta23avg$se[3,1]^2)
(Pvalue_approval <- 2*pnorm(-abs(Z_approval))) # 0.58




## -------------- ##
## Tables ----------------------------------------------------------------------
## -------------- ##

## Table A8 ##
tab_cbps_deltas2 <- rd.export4(list(fit_bldgs_delta23avg,
                                    fit_bldgs_single_delta23avg,
                                    fit_bldgs_multi_delta23avg,
                                   fit_units_delta23avg,
                                   fit_units_single_delta23avg,
                                   fit_units_multi_delta23avg
)) %>% as.data.frame()

tab_cbps_deltas2$DV = c("Total buildings, T+2/3 Avg","",
                       "Single-family buildings, T+2/3 Avg","",
                       "Multi-family buildings, T+2/3 Avg","",
                       "Total units, T+2/3 Avg","",
                       "Single-family units, T+2/3 Avg","",
                       "Multi-family units, T+2/3 Avg",""
)

print(xtable(tab_cbps_deltas2),
      include.rownames = F,
      floating = F,
      file = "mayors/tab_cbps_deltas2.tex")


## Table A9 ##
tab_cbps_deltaratios <- rd.export4(list(
  fit_ratio_bldgs_multisingle_delta23avg,
  fit_ratio_units_multisingle_delta23avg
)) %>% as.data.frame()

tab_cbps_deltaratios$DV = c(
                       "Multifamily proportion of buildings, T+2/3 Avg","",
                       "Multifamily proportion of units, T+2/3 Avg",""
)

print(xtable(tab_cbps_deltaratios),
      include.rownames = F,
      floating = F,
      file = "mayors/tab_cbps_deltaratios.tex")


## -------------- ##
## Plots -----------------------------------------------------------------------
## -------------- ##

## Figure 4b ##
bin.df <- filter(data_2p) %>%
  group_by(demshare_bin) %>%
  summarize(n_total = n(),
            avg_y = mean(total_units_multi_ln_delta23avg,na.rm=T),
            mid = first(mid))

pdf("mayors/rdplot_units_multi_ln_delta23avg.pdf",width=5,height=4)
ggplot(data_2p) +
  geom_point(data=bin.df,aes(x=mid,y=avg_y, size=n_total),
             shape=1,col="black") +
  geom_smooth(data=subset(data_2p,mid<=0.5),
              aes(x = demshare, y = total_units_multi_ln_delta23avg,
              weight=tri(demshare, fit_units_multi_delta23avg$bws[1,1],c=0.5)),
              method = 'lm', formula = y ~ poly(x, 1), size=1.5,col="blue") + 
  geom_smooth(data=subset(data_2p,mid>0.5),
              aes(x = demshare, y = total_units_multi_ln_delta23avg,
              weight=tri(demshare, fit_units_multi_delta23avg$bws[1,1],c=0.5)),
              method = 'lm', formula = y ~ poly(x, 1), size=1.5,col="blue") + 
  annotate('text', x = .51, y = 1.5, 0.05, hjust=0, parse=TRUE,
           label = paste('hat(tau)==',
                         round(fit_units_multi_delta23avg$coef['Conventional', ], 2),
                         '~(list(', round(fit_units_multi_delta23avg$ci['Robust', 1], 2),
                         ',', round(fit_units_multi_delta23avg$ci['Robust', 2], 2),
                         '))')) + 
  theme_minimal() + 
  scale_x_continuous("Mayoral Democratic voteshare",
                     limits=c(0.5-fit_units_multi_delta23avg$bws[1,1],
                              0.5+fit_units_multi_delta23avg$bws[1,1])) + 
  scale_y_continuous(str_wrap("Change in log(multi-family units + 1)",40)) +
  scale_size_continuous("Number of\nelections in bin") + 
  coord_cartesian(ylim=c(-1.5,2.25),xlim=c(0.39,0.61)) + 
  theme(legend.pos="bottom",axis.line = element_line(),axis.ticks = element_line(),panel.grid = element_blank())
dev.off()



## Multiple CBPS outcomes together:

## Figure 5b ##
coefs <- rd.export.numeric.90(list(fit_bldgs_delta23avg,
                                fit_bldgs_single_delta23avg,
                                fit_bldgs_multi_delta23avg,
                                fit_units_delta23avg,
                                fit_units_single_delta23avg,
                                fit_units_multi_delta23avg
))


coefs$outcome_pretty = c("Total\nbuildings",
                         "Single-family\nbuildings",
                         "Multi-family\nbuildings",
                         "Total\nunits",
                         "Single-family\nunits",
                         "Multi-family\nunits"
)
coefs$plotorder <- 6:1


pdf("mayors/coefplot_cbps_single_delta23avg.pdf",height=3.5,width=5)
ggplot(coefs) +
  geom_hline(yintercept=0, lty=2, lwd=1, colour="grey50") +
  geom_errorbar(aes(x=plotorder,ymin=cilo, ymax=cihi), 
                colour="black", width=0, size=0.75) +
  geom_errorbar(aes(x=plotorder,ymin=cilo_90, ymax=cihi_90),
                colour="black", width=0, size=1.5) +
  geom_point(aes(x=plotorder,y=coef),size=4, pch=21, fill="black") +
  geom_text(aes(x=plotorder,y=coef+0.2,label=round(coef,2)),nudge_x=0.2, size=3) +
  scale_y_continuous("RD effect of Democratic mayor\non change in log(outcome + 1)",
                     breaks=seq(-2, 2, 0.5),
                     limits=c(min(coefs$cilo),
                              max(coefs$cihi)),
                     labels=seq(-2, 2, 0.5)) +
  scale_x_continuous("",
                     breaks=coefs$plotorder,
                     limits=c(0.7,6.3),
                     labels=coefs$outcome_pretty,position = "bottom") + 
  coord_flip() + 
  theme_minimal() + 
  theme(axis.line = element_line(),axis.ticks = element_line(),panel.grid = element_blank())
dev.off()



## Ratios:

## Figure 6b ##
coefs <- rd.export.numeric.90(list(
                                fit_ratio_bldgs_multisingle_delta23avg,
                                fit_ratio_units_multisingle_delta23avg
))

coefs$outcome_pretty = c("Multi-family proportion\nof total buildings",
                         "Multi-family proportion\nof total units"
)

pdf("mayors/coefplot_cbps_deltaratios.pdf",height=3,width=5)
ggplot(coefs) +
  geom_hline(yintercept=0, lty=2, lwd=1, colour="grey50") +
  geom_errorbar(aes(x=2:1,ymin=cilo, ymax=cihi), 
                colour="black", width=0, size=0.75) +
  geom_errorbar(aes(x=2:1,ymin=cilo_90, ymax=cihi_90),
                colour="black", width=0, size=1.5) +
  geom_point(aes(x=2:1,y=coef),size=4, pch=21, fill="black") +
  geom_text(aes(x=2:1,y=coef+0.02,label=round(coef,2)),nudge_x=0.1, size=3) +
  scale_y_continuous("RD effect of Democratic mayor\non change in outcome",
                     breaks=seq(-0.5, 0.5, 0.05),
                     limits=c(min(coefs$cilo),
                              max(coefs$cihi)),
                     labels=seq(-0.5, 0.5, 0.05)) +
  scale_x_continuous("",
                     breaks=c(2:1),
                     limits=c(0.7,2.3),
                     labels=coefs$outcome_pretty,position = "bottom") + 
  coord_flip() + 
  theme_minimal() + 
  theme(axis.line = element_line(),axis.ticks = element_line(),panel.grid = element_blank())
dev.off()




#### by FOG ####

## Figure A16b ##
coefs <- rd.export.numeric.90(list(fit_may_bldgs_delta23avg,
                                fit_may_bldgs_multi_delta23avg,
                                fit_may_units_delta23avg,
                                fit_may_units_multi_delta23avg,
                                
                                fit_man_bldgs_delta23avg,
                                fit_man_bldgs_multi_delta23avg,
                                fit_man_units_delta23avg,
                                fit_man_units_multi_delta23avg
)) %>%
  mutate(FOG = c(rep("Strong Mayor",4),rep("Council-Manager",4)))

coefs$outcome_pretty = rep(c("Total\nbuildings",
                             "Multi-family\nbuildings",
                             "Total\nunits",
                             "Multi-family\nunits"),2)

coefs$plotorder <- c(4:1,4:1)


pdf("mayors/coefplot_cbps_delta23avg_byfog.pdf",height=4,width=5)
ggplot(coefs,aes(group=FOG)) +
  geom_hline(yintercept=0, lty=2, lwd=1, colour="grey50") +
  geom_errorbar(aes(x=plotorder,ymin=cilo, ymax=cihi),position=position_dodge(width=0.8), 
                colour="black", width=0, size=0.5) +
  geom_errorbar(aes(x=plotorder,ymin=cilo_90, ymax=cihi_90),position=position_dodge(width=0.8),
                colour="black", width=0, size=1) +
  geom_point(aes(x=plotorder,y=coef,shape=FOG),position=position_dodge(width=0.8),size=4, fill="black") +
  geom_text(aes(x=plotorder+0.2,y=coef+0.3,label=round(coef,2)),position=position_dodge(width=0.8), size=3) +
  scale_y_continuous("RD effect on change in log(outcome + 1) between\nelection year and avg. of 2/3 years after election",
                     breaks=seq(-1, 2, 0.5),
                     limits=c(min(coefs$cilo),
                              max(coefs$cihi)),
                     labels=seq(-1, 2, 0.5)) +
  scale_x_continuous("Outcome",
                     breaks=c(coefs$plotorder),
                     limits=c(0.7,4.4),
                     labels=coefs$outcome_pretty,position = "bottom") + 
  scale_shape_manual("Form of Government",values = c(21,22),breaks = c("Strong Mayor","Council-Manager")) + 
  coord_flip(ylim=c(-1,2)) + 
  theme_minimal() + 
  theme(legend.position="bottom")
dev.off()


## Figure A17b ##
coefs <- rd.export.numeric.90(list(
  fit_may_ratio_bldgs_multisingle_delta23avg,
  fit_may_ratio_units_multisingle_delta23avg,
  fit_man_ratio_bldgs_multisingle_delta23avg,
  fit_man_ratio_units_multisingle_delta23avg
)) %>%
  mutate(FOG = c(rep("Strong Mayor",2),rep("Council-Manager",2)))


coefs$outcome_pretty = rep(c("Multi-family proportion\nof total buildings",
                             "Multi-family proportion\nof total units"), 2)
coefs <- coefs %>%
  mutate(plotorder = c(2:1,2:1))


pdf("mayors/coefplot_cbps_deltaratios_byfog.pdf",height=3,width=5)
ggplot(coefs,aes(group=FOG)) +
  geom_hline(yintercept=0, lty=2, lwd=1, colour="grey50") +
  geom_errorbar(aes(x=plotorder,ymin=cilo, ymax=cihi),position=position_dodge(width=0.6), 
                colour="black", width=0, size=0.5) +
  geom_errorbar(aes(x=plotorder,ymin=cilo_90, ymax=cihi_90),position=position_dodge(width=0.6),
                colour="black", width=0, size=1) +
  geom_point(aes(x=plotorder,y=coef,shape=FOG),position=position_dodge(width=0.6),size=4, fill="black") +
  geom_text(aes(x=plotorder+0.06,y=coef+0.05,label=round(coef,2)),position=position_dodge(width=0.6), size=3) +
  scale_y_continuous("RD effect on change in outcome between\nelection year and avg. of 2/3 years after election",
                     breaks=seq(-1, 1, 0.1),
                     limits=c(min(coefs$cilo),
                              max(coefs$cihi)),
                     labels=seq(-1, 1, 0.1)) +
  scale_x_continuous("Outcome",
                     breaks=c(coefs$plotorder),
                     limits=c(0.7,2.3),
                     labels=coefs$outcome_pretty,position = "bottom") + 
  scale_shape_manual("Form of Government",values = c(21,22),breaks = c("Strong Mayor","Council-Manager")) + 
  coord_flip() + 
  theme_minimal() + 
  theme(legend.position = c(0.82,0.6),legend.text = element_text(size=6),legend.title = element_text(size=7))
dev.off()


####  by Districts vs At-large ####

## Figure A18b ##
coefs <- rd.export.numeric.90(list(fit_district_bldgs_delta23avg,
                                fit_district_bldgs_multi_delta23avg,
                                fit_district_units_delta23avg,
                                fit_district_units_multi_delta23avg,
                                
                                fit_combo_bldgs_delta23avg,
                                fit_combo_bldgs_multi_delta23avg,
                                fit_combo_units_delta23avg,
                                fit_combo_units_multi_delta23avg,
                                
                                fit_atlarge_bldgs_delta23avg,
                                fit_atlarge_bldgs_multi_delta23avg,
                                fit_atlarge_units_delta23avg,
                                fit_atlarge_units_multi_delta23avg
)) %>%
  mutate(districts = c(rep("Districts",4),rep("Combo",4),rep("At-large",4)))

coefs$outcome_pretty = rep(c("Total\nbuildings",
                             "Multi-family\nbuildings",
                             "Total\nunits",
                             "Multi-family\nunits"),3)

coefs$plotorder <- c(4:1,4:1,4:1)

pdf("mayors/coefplot_cbps_delta23avg_bydistricts.pdf",height=5,width=5)
ggplot(coefs,aes(group=districts)) +
  geom_hline(yintercept=0, lty=2, lwd=1, colour="grey50") +
  geom_errorbar(aes(x=plotorder,ymin=cilo, ymax=cihi),position=position_dodge(width=0.8), 
                colour="black", width=0, size=0.5) +
  geom_errorbar(aes(x=plotorder,ymin=cilo_90, ymax=cihi_90),position=position_dodge(width=0.8),
                colour="black", width=0, size=1) +
  geom_point(aes(x=plotorder,y=coef,shape=districts),position=position_dodge(width=0.8),size=4, fill="black") +
  geom_text(aes(x=plotorder+0.1,y=coef+0.3,label=round(coef,2)),position=position_dodge(width=0.8), size=3) +
  scale_y_continuous("RD effect on change in log(outcome + 1) between\nelection year and avg. of 2/3 years after election",
                     breaks=seq(-2, 2.5, 0.5),
                     limits=c(min(coefs$cilo),
                              max(coefs$cihi)),
                     labels=seq(-2, 2.5, 0.5)) +
  scale_x_continuous("Outcome",
                     breaks=c(coefs$plotorder),
                     limits=c(0.5,4.5),
                     labels=coefs$outcome_pretty,position = "bottom") + 
  scale_shape_manual("Council\ndistricts",values = c(21,22,23),breaks = c("Districts","Combo","At-large")) + 
  coord_flip(ylim=c(-1.5,2.7)) + 
  theme_minimal() + 
  theme(legend.position="bottom")
dev.off()


## Figure A19b ##
coefs <- rd.export.numeric.90(list(
  fit_district_ratio_bldgs_multisingle_delta23avg,
  fit_district_ratio_units_multisingle_delta23avg,
  fit_combo_ratio_bldgs_multisingle_delta23avg,
  fit_combo_ratio_units_multisingle_delta23avg,
  fit_atlarge_ratio_bldgs_multisingle_delta23avg,
  fit_atlarge_ratio_units_multisingle_delta23avg
)) %>%
  mutate(districts = c(rep("Districts",2),rep("Combo",2),rep("At-large",2)))

coefs$outcome_pretty = rep(c("Multi-family proportion\nof total buildings",
                             "Multi-family proportion\nof total units"), 3)
coefs <- coefs %>%
  mutate(plotorder = c(2:1,2:1,2:1))

pdf("mayors/coefplot_cbps_deltaratios_bydistricts.pdf",height=3.5,width=5)
ggplot(coefs,aes(group=districts)) +
  geom_hline(yintercept=0, lty=2, lwd=1, colour="grey50") +
  geom_errorbar(aes(x=plotorder,ymin=cilo, ymax=cihi),position=position_dodge(width=0.6), 
                colour="black", width=0, size=0.5) +
  geom_errorbar(aes(x=plotorder,ymin=cilo_90, ymax=cihi_90),position=position_dodge(width=0.6),
                colour="black", width=0, size=1) +
  geom_point(aes(x=plotorder,y=coef,shape=districts),position=position_dodge(width=0.6),size=4, fill="black") +
  geom_text(aes(x=plotorder+0.06,y=coef+0.05,label=round(coef,2)),position=position_dodge(width=0.6), size=3) +
  scale_y_continuous("RD effect on change in outcome between\nelection year and avg. of 2/3 years after election",
                     breaks=seq(-1, 1, 0.1),
                     limits=c(min(coefs$cilo),
                              max(coefs$cihi)),
                     labels=seq(-1, 1, 0.1)) +
  scale_x_continuous("Outcome",
                     breaks=c(coefs$plotorder),
                     limits=c(0.7,2.3),
                     labels=coefs$outcome_pretty,position = "bottom") + 
  scale_shape_manual("Council\ndistricts",values = c(21,22,23),breaks = c("Districts","Combo","At-large")) + 
  coord_flip() + 
  theme_minimal() + 
  theme(legend.position = c(0.82,0.6),legend.text = element_text(size=6),legend.title = element_text(size=7))
dev.off()



#### by Partisan Elecs ####
## Figure A20b ##
coefs <- rd.export.numeric.90(list(fit_par_bldgs_delta23avg,
                                   fit_par_bldgs_multi_delta23avg,
                                   fit_par_units_delta23avg,
                                   fit_par_units_multi_delta23avg,
                                   
                                   fit_nonpar_bldgs_delta23avg,
                                   fit_nonpar_bldgs_multi_delta23avg,
                                   fit_nonpar_units_delta23avg,
                                   fit_nonpar_units_multi_delta23avg
)) %>%
  mutate(partisan = c(rep("Partisan",4),rep("Nonpartisan",4)))

coefs$outcome_pretty = rep(c("Total\nbuildings",
                             "Multi-family\nbuildings",
                             "Total\nunits",
                             "Multi-family\nunits"),2)

coefs$plotorder <- c(4:1,4:1)


pdf("mayors/coefplot_cbps_delta23avg_bypartisan.pdf",height=4,width=5)
ggplot(coefs,aes(group=partisan)) +
  geom_hline(yintercept=0, lty=2, lwd=1, colour="grey50") +
  geom_errorbar(aes(x=plotorder,ymin=cilo, ymax=cihi),position=position_dodge(width=0.8), 
                colour="black", width=0, size=0.5) +
  geom_errorbar(aes(x=plotorder,ymin=cilo_90, ymax=cihi_90),position=position_dodge(width=0.8),
                colour="black", width=0, size=1) +
  geom_point(aes(x=plotorder,y=coef,shape=partisan),position=position_dodge(width=0.8),size=4, fill="black") +
  geom_text(aes(x=plotorder+0.2,y=coef+0.3,label=round(coef,2)),position=position_dodge(width=0.8), size=3) +
  scale_y_continuous("RD effect on change in log(outcome + 1) between\nelection year and avg. of 2/3 years after election",
                     breaks=seq(-1, 2, 0.5),
                     limits=c(min(coefs$cilo),
                              max(coefs$cihi)),
                     labels=seq(-1, 2, 0.5)) +
  scale_x_continuous("Outcome",
                     breaks=c(coefs$plotorder),
                     limits=c(0.7,4.4),
                     labels=coefs$outcome_pretty,position = "bottom") + 
  scale_shape_manual("Elections",values = c(21,22),breaks = c("Partisan","Nonpartisan")) + 
  coord_flip(ylim=c(-1,2)) + 
  theme_minimal() + 
  theme(legend.position="bottom")
dev.off()


## Figure A21b ##
coefs <- rd.export.numeric.90(list(
  fit_par_ratio_bldgs_multisingle_delta23avg,
  fit_par_ratio_units_multisingle_delta23avg,
  fit_nonpar_ratio_bldgs_multisingle_delta23avg,
  fit_nonpar_ratio_units_multisingle_delta23avg
)) %>%
  mutate(partisan = c(rep("Partisan",2),rep("Nonpartisan",2)))

coefs$outcome_pretty = rep(c("Multi-family proportion\nof total buildings",
                             "Multi-family proportion\nof total units"), 2)
coefs <- coefs %>%
  mutate(plotorder = c(2:1,2:1))

pdf("mayors/coefplot_cbps_deltaratios_bypartisan.pdf",height=3,width=5)
ggplot(coefs,aes(group=partisan)) +
  geom_hline(yintercept=0, lty=2, lwd=1, colour="grey50") +
  geom_errorbar(aes(x=plotorder,ymin=cilo, ymax=cihi),position=position_dodge(width=0.6), 
                colour="black", width=0, size=0.5) +
  geom_errorbar(aes(x=plotorder,ymin=cilo_90, ymax=cihi_90),position=position_dodge(width=0.6),
                colour="black", width=0, size=1) +
  geom_point(aes(x=plotorder,y=coef,shape=partisan),position=position_dodge(width=0.6),size=4, fill="black") +
  geom_text(aes(x=plotorder+0.06,y=coef+0.05,label=round(coef,2)),position=position_dodge(width=0.6), size=3) +
  scale_y_continuous("RD effect on change in outcome between\nelection year and avg. of 2/3 years after election",
                     breaks=seq(-1, 1, 0.1),
                     limits=c(min(coefs$cilo),
                              max(coefs$cihi)),
                     labels=seq(-1, 1, 0.1)) +
  scale_x_continuous("Outcome",
                     breaks=c(coefs$plotorder),
                     limits=c(0.7,2.3),
                     labels=coefs$outcome_pretty,position = "bottom") + 
  scale_shape_manual("Elections",values = c(21,22),breaks = c("Partisan","Nonpartisan")) + 
  coord_flip() + 
  theme_minimal() + 
  theme(legend.position = c(0.82,0.6),legend.text = element_text(size=6),legend.title = element_text(size=7))
dev.off()



#### by Local Council Approval Power ####

## Figure 7b ##
coefs <- rd.export.numeric.90(list(
  fit_approval_ratio_bldgs_multisingle_delta23avg,
  fit_approval_ratio_units_multisingle_delta23avg,
  fit_noapproval_ratio_bldgs_multisingle_delta23avg,
  fit_noapproval_ratio_units_multisingle_delta23avg
)) %>%
  mutate(approval = c(rep("Yes",2),rep("No",2)))

coefs$outcome_pretty = rep(c("Multi-family proportion\nof total buildings",
                             "Multi-family proportion\nof total units"), 2)
coefs <- coefs %>%
  mutate(plotorder = c(2:1,2:1))


pdf("mayors/coefplot_cbps_deltaratios_byapproval_color.pdf",height=3,width=5)
ggplot(coefs,aes(group=approval)) +
  geom_hline(yintercept=0, lty=2, lwd=1, colour="grey50") +
  geom_errorbar(aes(x=plotorder,ymin=cilo, ymax=cihi,col=approval),position=position_dodge(width=0.6), width=0, size=0.75) +
  geom_errorbar(aes(x=plotorder,ymin=cilo_90, ymax=cihi_90,col=approval),position=position_dodge(width=0.6), width=0, size=1.5) +
  geom_point(aes(x=plotorder,y=coef,shape=approval,fill=approval),position=position_dodge(width=0.6),size=4) +
  geom_text(aes(x=plotorder+0.06,y=coef+0.05,label=round(coef,2)),position=position_dodge(width=0.6), size=3) +
  annotate(geom="text",x=2.35,y=filter(coefs,approval=="Yes"&outcome=="ratio_bldgs_multisingle_delta23avg")$coef+0.095,label="Requires majority\ncouncil approval",hjust=0,lineheight = .7,size=2.5) + 
  geom_curve(aes(x = 2.35, y = filter(coefs,approval=="Yes"&outcome=="ratio_bldgs_multisingle_delta23avg")$coef+0.09, xend = 2.2, yend = filter(coefs,approval=="Yes"&outcome=="ratio_bldgs_multisingle_delta23avg")$coef+0.01),arrow = arrow(length = unit(0.03, "npc"),type="closed"), size = 0.5,curvature = 0.25,angle=90) + 
  annotate(geom="text",x=1.65,y=filter(coefs,approval=="No"&outcome=="ratio_bldgs_multisingle_delta23avg")$coef+0.055,label="Does not require\nmajority council approval",hjust=0,lineheight = .7,size=2.5) + 
  geom_curve(aes(x = 1.65, y = filter(coefs,approval=="No"&outcome=="ratio_bldgs_multisingle_delta23avg")$coef+0.05, xend = 1.8, yend = filter(coefs,approval=="No"&outcome=="ratio_bldgs_multisingle_delta23avg")$coef+0.01),arrow = arrow(length = unit(0.03, "npc"),type="closed"), size = 0.5,curvature = -0.25,angle=90) + 
  scale_y_continuous("RD effect of Democratic mayor\non change in outcome",
                     breaks=seq(-1, 1, 0.1),
                     limits=c(min(coefs$cilo),
                              max(coefs$cihi)),
                     labels=seq(-1, 1, 0.1)) +
  scale_x_continuous("",
                     breaks=c(coefs$plotorder),
                     limits=c(0.7,2.6),
                     labels=coefs$outcome_pretty,position = "bottom") + 
  scale_shape_manual("Council approval\nneeded?",values = c(21,22),breaks = c("Yes","No"),labels=c("Requires\nmajority","Does\nnot")) + 
  scale_color_manual("Council approval\nneeded?",breaks=c("Yes","No"),labels=c("Requires\nmajority","Does\nnot"),values=c(vpurple,vgreen),aesthetics = c("col","fill")) + 
  coord_flip(xlim=c(0.7,2.35)) + 
  theme_minimal() + 
  theme(legend.position = "none",legend.text = element_text(size=6),legend.title = element_text(size=7),legend.justification = "top") + 
  theme(axis.line = element_line(),axis.ticks = element_line(),panel.grid = element_blank())
dev.off()


## Figure A22b ##
coefs <- rd.export.numeric.90(list(fit_approval_bldgs_delta23avg,
                                   fit_approval_bldgs_single_delta23avg,
                                   fit_approval_bldgs_multi_delta23avg,
                                   fit_approval_units_delta23avg,
                                   fit_approval_units_single_delta23avg,
                                   fit_approval_units_multi_delta23avg,
                                   
                                   fit_noapproval_bldgs_delta23avg,
                                   fit_noapproval_bldgs_single_delta23avg,
                                   fit_noapproval_bldgs_multi_delta23avg,
                                   fit_noapproval_units_delta23avg,
                                   fit_noapproval_units_single_delta23avg,
                                   fit_noapproval_units_multi_delta23avg
)) %>%
  mutate(approval = c(rep("Yes",6),rep("No",6)))

coefs$outcome_pretty = rep(c("Total\nbuildings",
                             "Single-family\nbuildings",
                             "Multi-family\nbuildings",
                             "Total\nunits",
                             "Single-family\nunits",
                             "Multi-family\nunits"),2)

coefs$plotorder <- c(6:1,6:1)


pdf("mayors/coefplot_cbps_delta23avg_byapproval_v2_color.pdf",height=5,width=5)
ggplot(coefs,aes(group=approval)) +
  geom_hline(yintercept=0, lty=2, lwd=1, colour="grey50") +
  geom_errorbar(aes(x=plotorder,ymin=cilo, ymax=cihi,col=approval),position=position_dodge(width=0.8), 
                width=0, size=0.75) +
  geom_errorbar(aes(x=plotorder,ymin=cilo_90, ymax=cihi_90,col=approval),position=position_dodge(width=0.8),
                width=0, size=1.5) +
  geom_point(aes(x=plotorder,y=coef,shape=approval,fill=approval),position=position_dodge(width=0.8),size=4) +
  geom_text(aes(x=plotorder+0.2,y=coef+0.3,label=round(coef,2)),position=position_dodge(width=0.8), size=3) +
  scale_y_continuous("RD effect on change in log(outcome + 1) between\nelection year and avg. of 2/3 years after election",
                     breaks=seq(-1, 2, 0.5),
                     limits=c(min(coefs$cilo),
                              max(coefs$cihi)),
                     labels=seq(-1, 2, 0.5)) +
  scale_x_continuous("Outcome",
                     breaks=c(coefs$plotorder),
                     limits=c(0.7,6.4),
                     labels=coefs$outcome_pretty,position = "bottom") + 
  scale_shape_manual("Council approval\nneeded?",values = c(21,22),breaks = c("Yes","No"),labels=c("Requires\nmajority","Does\nnot")) + 
  scale_color_manual("Council approval\nneeded?",breaks=c("Yes","No"),labels=c("Requires\nmajority","Does\nnot"),values=c(vpurple,vgreen),aesthetics = c("col","fill")) + 
  coord_flip(ylim=c(-1,2)) + 
  theme_minimal() + 
  theme(legend.position="bottom")
dev.off()


fit_missingapproval_units_delta23avg <- with(filter(data_2p,is.na(council_approval_norez)),
                                             rdrobust(y = total_units_ln_delta23avg,x = demshare,
                                                      c = 0.5,cluster = cluster
                                                      #, weights=weight
                                             ))
fit_missingapproval_bldgs_delta23avg <- with(filter(data_2p,is.na(council_approval_norez)),
                                             rdrobust(y = total_bldgs_ln_delta23avg,x = demshare,
                                                      c = 0.5,cluster = cluster
                                                      #,weights=weight
                                             ))

fit_missingapproval_units_multi_delta23avg <- with(filter(data_2p,is.na(council_approval_norez)),
                                                   rdrobust(y = total_units_multi_ln_delta23avg,x = demshare,
                                                            c = 0.5,cluster = cluster
                                                            # ,weights=weight
                                                   ))
fit_missingapproval_bldgs_multi_delta23avg <- with(filter(data_2p,is.na(council_approval_norez)),
                                                   rdrobust(y = total_bldgs_multi_ln_delta23avg,x = demshare,
                                                            c = 0.5,cluster = cluster
                                                            # ,weights=weight
                                                   ))
fit_missingapproval_units_single_delta23avg <- with(filter(data_2p,is.na(council_approval_norez)),
                                                    rdrobust(y = total_units_single_ln_delta23avg,x = demshare,
                                                             c = 0.5,cluster = cluster
                                                             # ,weights=weight
                                                    ))
fit_missingapproval_bldgs_single_delta23avg <- with(filter(data_2p,is.na(council_approval_norez)),
                                                    rdrobust(y = total_bldgs_single_ln_delta23avg,x = demshare,
                                                             c = 0.5,cluster = cluster
                                                             # ,weights=weight
                                                    ))
fit_missingapproval_ratio_bldgs_multisingle_delta23avg <- with(filter(data_2p,is.na(council_approval_norez)),
                                                               rdrobust(y = ratio_bldgs_multisingle_delta23avg,x = demshare,
                                                                        c = 0.5,cluster = cluster
                                                                        # ,weights=weight
                                                               ))

fit_missingapproval_ratio_units_multisingle_delta23avg <- with(filter(data_2p,is.na(council_approval_norez)),
                                                               rdrobust(y = ratio_units_multisingle_delta23avg,x = demshare,
                                                                        c = 0.5, cluster = cluster
                                                                        # ,weights=weight
                                                               ))

coefs <- rd.export.numeric.90(list(
  fit_approval_ratio_bldgs_multisingle_delta23avg,
  fit_approval_ratio_units_multisingle_delta23avg,
  fit_noapproval_ratio_bldgs_multisingle_delta23avg,
  fit_noapproval_ratio_units_multisingle_delta23avg,
  fit_missingapproval_ratio_bldgs_multisingle_delta23avg,
  fit_missingapproval_ratio_units_multisingle_delta23avg
)) %>%
  mutate(approval = c(rep("Yes",2),rep("No",2),rep("Missing",2)))

coefs$outcome_pretty = rep(c("Multi-family proportion\nof total buildings",
                             "Multi-family proportion\nof total units"), 3)
coefs <- coefs %>%
  mutate(plotorder = c(2:1,2:1,2:1))


pdf("mayors/coefplot_cbps_deltaratios_byapproval_color_wmissing.pdf",height=3,width=5)
ggplot(coefs,aes(group=approval)) +
  geom_hline(yintercept=0, lty=2, lwd=1, colour="grey50") +
  geom_errorbar(aes(x=plotorder,ymin=cilo, ymax=cihi,col=approval),position=position_dodge(width=0.6), width=0, size=0.75) +
  geom_errorbar(aes(x=plotorder,ymin=cilo_90, ymax=cihi_90,col=approval),position=position_dodge(width=0.6), width=0, size=1.5) +
  geom_point(aes(x=plotorder,y=coef,shape=approval,fill=approval),position=position_dodge(width=0.6),size=4) +
  geom_text(aes(x=plotorder+0.06,y=coef+0.03,label=round(coef,2)),position=position_dodge(width=0.6), size=3) +
  scale_y_continuous("RD effect on change in outcome between\nelection year and avg. of 2/3 years after election",
                     breaks=seq(-1, 1, 0.1),
                     limits=c(min(coefs$cilo),
                              max(coefs$cihi)),
                     labels=seq(-1, 1, 0.1)) +
  scale_x_continuous("Outcome",
                     breaks=c(coefs$plotorder),
                     limits=c(0.7,2.3),
                     labels=coefs$outcome_pretty,position = "bottom") + 
  scale_shape_manual("Council approval\nneeded?",values = c(21,22,23),breaks = c("Yes","No","Missing"),labels=c("Requires\nmajority","Does\nnot","Missing")) + 
  scale_color_manual("Council approval\nneeded?",breaks=c("Yes","No","Missing"),labels=c("Requires\nmajority","Does\nnot","Missing"),values=c(vpurple,vgreen,vyellow),aesthetics = c("col","fill")) + 
  coord_flip() + 
  theme_minimal() + 
  theme(legend.position = c(0.22,0.85),legend.text = element_text(size=6),legend.title = element_text(size=7),legend.justification = "top")
dev.off()


## Placebo checks --------------------------------------------------------------

#### Pre-treatment outcomes ####

fit_units_multi_deltalag1 <- with(data_2p,
                                  rdrobust(y = total_units_multi_ln_deltalag1,x = demshare,
                                           c = 0.5,cluster = cluster))
summary(fit_units_multi_deltalag1)

fit_units_multi_deltalag2 <- with(data_2p,
                                  rdrobust(y = total_units_multi_ln_deltalag2,x = demshare,
                                           c = 0.5,cluster = cluster))
summary(fit_units_multi_deltalag2)
fit_units_multi_delta1 <- with(data_2p,
                               rdrobust(y = total_units_multi_ln_delta1,x = demshare,
                                        c = 0.5,cluster = cluster))
fit_units_multi_delta2 <- with(data_2p,
                               rdrobust(y = total_units_multi_ln_delta2,x = demshare,
                                        c = 0.5,cluster = cluster))
fit_units_multi_delta3 <- with(data_2p,
                               rdrobust(y = total_units_multi_ln_delta3,x = demshare,
                                        c = 0.5,cluster = cluster))


fit_ratio_units_multisingle_deltalag1 <- with(data_2p,
                                              rdrobust(y = ratio_units_multisingle_deltalag1,x = demshare,
                                                       c = 0.5,cluster = cluster))
summary(fit_ratio_units_multisingle_deltalag1)

fit_ratio_units_multisingle_deltalag2 <- with(data_2p,
                                              rdrobust(y = ratio_units_multisingle_deltalag2,x = demshare,
                                                       c = 0.5,cluster = cluster))
summary(fit_ratio_units_multisingle_deltalag2)


fit_ratio_units_multisingle_delta1 <- with(data_2p,
                                           rdrobust(y = ratio_units_multisingle_delta1,x = demshare,
                                                    c = 0.5,cluster = cluster))
fit_ratio_units_multisingle_delta2 <- with(data_2p,
                                           rdrobust(y = ratio_units_multisingle_delta2,x = demshare,
                                                    c = 0.5,cluster = cluster))
fit_ratio_units_multisingle_delta3 <- with(data_2p,
                                           rdrobust(y = ratio_units_multisingle_delta3,x = demshare,
                                                    c = 0.5,cluster = cluster))



## Figure A5b:
coefs <- rd.export.numeric.90(list(fit_units_multi_deltalag2,
                                   fit_units_multi_deltalag1,
                                   fit_units_multi_delta1,
                                   fit_units_multi_delta2,
                                   fit_units_multi_delta3
))

coefs$outcome_pretty = rep("Multi-family units",5)

coefs$year = c("-2 years","-1 year","+1 year","+2 years","+3 years")

coefs$plotorder = c(1:5)


pdf("mayors/coefplot_cbps_delta_units_placebo.pdf",height=3,width=5)
ggplot(filter(coefs,outcome_pretty=="Multi-family units")) +
  geom_hline(yintercept=0, lty=2, lwd=1, colour="grey50") +
  geom_errorbar(aes(x=plotorder,ymin=cilo, ymax=cihi), 
                colour="black", width=0, size=0.5) +
  geom_errorbar(aes(x=plotorder,ymin=cilo_90, ymax=cihi_90), 
                colour="black", width=0, size=1) +
  geom_point(aes(x=plotorder,y=coef),size=4, pch=21, fill="black") +
  geom_text(aes(x=plotorder,y=coef+0.1,label=round(coef,2)),nudge_x=0.2, size=3) +
  geom_vline(xintercept = 2.5,col="red") + 
  scale_y_continuous("RD effect on change in log(multi-family units + 1)",
                     breaks=seq(-2, 2, 0.5),
                     limits=c(min(coefs$cilo),
                              max(coefs$cihi)),
                     labels=seq(-2, 2, 0.5)) +
  scale_x_continuous("Change relative to election year",
                     breaks=coefs$plotorder,
                     limits=c(0.7,5.3),
                     labels=coefs$year,position = "bottom") + 
  # coord_flip() + 
  theme_minimal()
dev.off()


## Figure A6b:
coefs.ratios <- rd.export.numeric.90(list(fit_ratio_units_multisingle_deltalag2,
                                          fit_ratio_units_multisingle_deltalag1,
                                          fit_ratio_units_multisingle_delta1,
                                          fit_ratio_units_multisingle_delta2,
                                          fit_ratio_units_multisingle_delta3
))
coefs.ratios$outcome_pretty = rep("Multifamily proportion\nof total units",5)
coefs.ratios$year = c("-2 years","-1 year","+1 year","+2 years","+3 years")

coefs.ratios$plotorder = c(1:5)


pdf("mayors/coefplot_cbps_deltaratio_units_placebo.pdf",height=3,width=5)
ggplot(filter(coefs.ratios,outcome_pretty=="Multifamily proportion\nof total units")) +
  geom_hline(yintercept=0, lty=2, lwd=1, colour="grey50") +
  geom_errorbar(aes(x=plotorder,ymin=cilo, ymax=cihi), 
                colour="black", width=0, size=0.5) +
  geom_errorbar(aes(x=plotorder,ymin=cilo_90, ymax=cihi_90), 
                colour="black", width=0, size=1) +
  geom_point(aes(x=plotorder,y=coef),size=4, pch=21, fill="black") +
  geom_text(aes(x=plotorder,y=coef+0.03,label=round(coef,2)),nudge_x=0.2, size=3) +
  geom_vline(xintercept = 2.5,col="red") + 
  scale_y_continuous("RD effect on change in\nmultifamily proportion of total units",
                     breaks=seq(-0.2, 0.25, 0.1),
                     limits=c(min(coefs.ratios$cilo[which(coefs.ratios$outcome_pretty=="Multifamily proportion\nof total units")]),
                              max(coefs.ratios$cihi[which(coefs.ratios$outcome_pretty=="Multifamily proportion\nof total units")])),
                     labels=seq(-0.2, 0.25, 0.1)) +
  scale_x_continuous("Change year relative to election year value",
                     breaks=coefs.ratios$plotorder[which(coefs.ratios$outcome_pretty=="Multifamily proportion\nof total units")],
                     limits=c(0.7,5.3),
                     labels=coefs.ratios$year[which(coefs.ratios$outcome_pretty=="Multifamily proportion\nof total units")],position = "bottom") + 
  # coord_flip() + 
  theme_minimal()
dev.off()


#### Lagged running variable ####
cct.rv.lag4 <- with(data_2p, rdrobust(y=demshare_lag4 , x=demshare, c=0.5 ,
                                      cluster=cluster))


bin.df <- filter(data_2p) %>%
  group_by(demshare_bin) %>%
  summarize(n_total = n(),
            avg_y = mean(demshare_lag4,na.rm=T),
            mid = first(mid))

## Figure A4b:
pdf("mayors/rdplot_demshare_lag4.pdf",width=4,height=4)
ggplot(data_2p) +
  geom_point(data=bin.df,aes(x=mid,y=avg_y, size=n_total),
             shape=1,col="black") +
  geom_smooth(data=subset(data_2p,mid<=0.5),
              aes(x = demshare, y = demshare_lag4),
              method = 'lm', formula = y ~ poly(x, 1), size=1.5,col="blue") + 
  geom_smooth(data=subset(data_2p,mid>0.5),
              aes(x = demshare, y = demshare_lag4),
              method = 'lm', formula = y ~ poly(x, 1), size=1.5,col="blue") + 
  annotate('text', x = .51, y = 0.75, 0.05, hjust=0.5, parse=TRUE,
           label = paste('hat(tau)==',
                         round(cct.rv.lag4$coef['Conventional', ], 2),
                         '~(list(', round(cct.rv.lag4$ci['Robust', 1], 2),
                         ',', round(cct.rv.lag4$ci['Robust', 2], 2),
                         '))')) + 
  theme_bw() + 
  scale_x_continuous("Democratic voteshare",limits=c(0.5-cct.rv.lag4$bws[1,1],0.5+cct.rv.lag4$bws[1,1])) + 
  scale_y_continuous("Democratic voteshare, 4 years earlier") +
  scale_size_continuous("Number of\nelections in bin") + 
  theme(legend.position = "bottom")
dev.off()



#### McCrary tests ####

# formal McCrary test:
width <- .005
data.graph <- data_2p %>%
  mutate(bin=cut(demshare, breaks=seq(0,1, width))
  )
bins <- data.frame(bin = levels(data.graph$bin),
                   mid = seq(0 + width/2, 1 - width/2, width)
)
data.graph <- left_join(data.graph,bins,by="bin")

data.graph.all <- data.graph %>% 
  select(bin, mid.y, ratio_bldgs_multisingle_delta23avg) %>%
  group_by(bin,mid.y) %>%
  summarise(bin.mean = mean(ratio_bldgs_multisingle_delta23avg,na.rm=T),
            n = sum(!is.na(ratio_bldgs_multisingle_delta23avg))) %>%
  mutate(mid_adj = mid.y - 0.5)

mc.rd.mayors <- lm(n ~ mid_adj * (mid_adj>=0), data=data.graph.all[which(data.graph.all$mid.y>=(0.5-fit_ratio_bldgs_multisingle_delta23avg$bws["h", "left"]) & data.graph.all$mid.y<=(0.5+fit_ratio_bldgs_multisingle_delta23avg$bws["h", "right"])),])
summary(mc.rd.mayors)

## Table A3b:
stargazer(mc.rd.mayors,
          out = "mayors/mccrary.tex",
          dep.var.labels = "Number of observations in bin",
          omit.stat = c("ser","f","adj.rsq"),
          covariate.labels = c("Voteshare bin","Voteshare $\\ge 0.5$","Voteshare bin $\\times$ Voteshare $\\ge$ 0.5"),
          float = F)


## Figure A3b (density histogram):
(mccrary_mayors <- ggplot(data_2p,aes(x=demshare)) +
    geom_histogram(alpha=0.8,binwidth = 0.005,boundary=0.5) +
    geom_vline(col="red",lty=2,xintercept = 0.5) +
    labs(x="Democratic Voteshare",y="Observations") +
    scale_x_continuous(breaks=seq(0,1,0.02),
                       limits=c(0.5-fit_ratio_units_multisingle_delta23avg$bws["h", "left"],0.5+fit_ratio_units_multisingle_delta23avg$bws["h", "right"]),
                       labels=scales::percent_format(accuracy = 1)) +
    theme(text = element_text(size=20)) + 
    coord_cartesian(xlim=c(0.4,0.6)) + 
    theme_minimal()
)

ggsave(mccrary_mayors,filename = "mayors/mccrary_hist.pdf",height=3,width=4)



## Using Cattaneo Jansson and Ma (2019) nonparametric method:
cjm_mayors <- with(data_2p, rddensity(X=demshare,c=0.5,massPoints = F,h = fit_ratio_units_multisingle_delta23avg$bws["h", "left"]))
summary(cjm_mayors)

names(cjm_mayors)
cjm_mayors$hat$diff # density difference on either side of cutoff
cjm_mayors$test$t_jk # t-stat with SE based on jackknife
cjm_mayors$test$p_jk # p-value for density test
cjm_mayors$N$eff_left + cjm_mayors$N$eff_right # effective N
cjm_mayors$h$left
cjm_mayors$h$right

## Table A4b:
density_tab <- data.frame(
  "t-statistic" = c(cjm_mayors$test$t_jk),
  "p-value" = c(cjm_mayors$test$p_jk),
  "Effective N" = c(cjm_mayors$N$eff_left + cjm_mayors$N$eff_right)#,
  # "Bandwidth" = c(cjm_sler$h$left + cjm_sler$h$right,
  #                 cjm_ceda$h$left + cjm_ceda$h$right,
  #                 cjm_mayors$h$left + cjm_mayors$h$right)
)

print(xtable(density_tab),file = "mayors/rddensity_cjm.tex",floating = F,include.rownames = F)


## For RDD Density test using Hartman (2020) method (null hypothesis of difference)
devtools::source_url("https://github.com/ekhartman/rdd_equivalence/blob/master/RDD_equivalence_functions.R?raw=TRUE")
fstar_mayors <- cjm_mayors$hat$left/cjm_mayors$hat$right # f-star-hat
hartman_mayors <- rdd.tost.ratio(estL = cjm_mayors$hat$left, estR = cjm_mayors$hat$right,
                                 seL = cjm_mayors$sd_jk$left, seR = cjm_mayors$sd_jk$right,
                                 eps = 1.5,
                                 alpha = 0.05)

## Table A5b:
hartman_tab <- data.frame("Observed Ratio" = c(fstar_mayors),
                          "Equivalence Confidence Interval" = c(paste0("(",round(1/hartman_mayors$inverted,2), ", ",round(hartman_mayors$inverted,2),")")),
                          "p-value" = c(hartman_mayors$p)
)
print(xtable(hartman_tab),file = "mayors/density_hartman.tex",floating = F,include.rownames = F)




## Robustness tests ------------------------------------------------------------
#### Long-run effects ####

fit_units_multi_delta1 <- with(data_2p,
                               rdrobust(y = total_units_multi_ln_delta1,x = demshare,
                                        c = 0.5,cluster = cluster))
fit_units_multi_delta2 <- with(data_2p,
                               rdrobust(y = total_units_multi_ln_delta2,x = demshare,
                                        c = 0.5,cluster = cluster))
fit_units_multi_delta3 <- with(data_2p,
                               rdrobust(y = total_units_multi_ln_delta3,x = demshare,
                                        c = 0.5,cluster = cluster))
fit_units_multi_delta4 <- with(data_2p,
                               rdrobust(y = total_units_multi_ln_delta4,x = demshare,
                                        c = 0.5,cluster = cluster))
summary(fit_units_multi_delta4)
fit_units_multi_delta5 <- with(data_2p,
                               rdrobust(y = total_units_multi_ln_delta5,x = demshare,
                                        c = 0.5,cluster = cluster))
summary(fit_units_multi_delta5)
fit_units_multi_delta6 <- with(data_2p,
                               rdrobust(y = total_units_multi_ln_delta6,x = demshare,
                                        c = 0.5,cluster = cluster))
summary(fit_units_multi_delta6)
fit_units_multi_delta23avg <- with(data_2p,
                                    rdrobust(y = total_units_multi_ln_delta23avg,x = demshare,
                                             c = 0.5,cluster = cluster))
summary(fit_units_multi_delta23avg)
fit_units_multi_delta234avg <- with(data_2p,
                               rdrobust(y = total_units_multi_ln_delta234avg,x = demshare,
                                        c = 0.5,cluster = cluster))
summary(fit_units_multi_delta234avg)
fit_units_multi_delta1234avg <- with(data_2p,
                                    rdrobust(y = total_units_multi_ln_delta1234avg,x = demshare,
                                             c = 0.5,cluster = cluster))
summary(fit_units_multi_delta1234avg)
fit_units_multi_deltaterm234avg <- with(data_2p,
                                     rdrobust(y = total_units_multi_ln_deltaterm234avg,x = demshare,
                                              c = 0.5,cluster = cluster))
summary(fit_units_multi_deltaterm234avg)
fit_units_multi_deltaterm4avg <- with(data_2p,
                                        rdrobust(y = total_units_multi_ln_deltaterm4avg,x = demshare,
                                                 c = 0.5,cluster = cluster))
summary(fit_units_multi_deltaterm4avg)



## Figure A9b ##
coefs <- rd.export.numeric.90(list(
  fit_units_multi_delta23avg,
  fit_units_multi_delta234avg,
  fit_units_multi_deltaterm234avg,
  fit_units_multi_delta1234avg,
  fit_units_multi_deltaterm4avg
))

coefs$outcome_pretty = c("Avg. of 2-3 years\npost-election\nrelative to\nelection year","Avg. of 2-4 years\npost-election\nrelative to\nelection year","Avg. of 2-4 years\npost-election\nrelative to 4-year\navg. pre-election","Avg. of 1-4 years\npost-election\nrelative to\nelection year","Avg. of 1-4 years\npost-election\nrelative to 4-year\navg. pre-election")
coefs$plotorder <- 1:5


pdf("mayors/coefplot_cbps_delta_units_longhorizon_v2.pdf",height=3.5,width=6.7)
ggplot(coefs) +
  geom_hline(yintercept=0, lty=2, lwd=1, colour="grey50") +
  geom_vline(xintercept=0.5, lty=3, lwd=1, colour="red") +
  geom_errorbar(aes(x=plotorder,ymin=cilo, ymax=cihi), 
                colour="black", width=0, size=0.5) +
  geom_errorbar(aes(x=plotorder,ymin=cilo_90, ymax=cihi_90),
                colour="black", width=0, size=1) +
  geom_point(aes(x=plotorder,y=coef),size=4, pch=21, fill="black") +
  geom_text(aes(x=plotorder,y=coef+0.02,label=round(coef,2)),nudge_x=0.2, size=3) +
  scale_y_continuous("RD effect on change in\nlog(multi-family units + 1)",
                     breaks=seq(-0.5, 1.5, 0.25),
                     limits=c(min(coefs$cilo),
                              max(coefs$cihi)),
                     labels=seq(-0.5, 1.5, 0.25)) +
  scale_x_continuous("Outcome calculation",
                     breaks=coefs$plotorder,
                     limits=c(0.5,5.3),
                     labels=coefs$outcome_pretty,position = "bottom") + 
  # coord_flip() + 
  theme_minimal()
dev.off()


fit_ratio_units_multisingle_delta1 <- with(data_2p,
                                           rdrobust(y = ratio_units_multisingle_delta1,x = demshare,
                                                    c = 0.5,cluster = cluster))
fit_ratio_units_multisingle_delta2 <- with(data_2p,
                                           rdrobust(y = ratio_units_multisingle_delta2,x = demshare,
                                                    c = 0.5,cluster = cluster))
fit_ratio_units_multisingle_delta3 <- with(data_2p,
                                           rdrobust(y = ratio_units_multisingle_delta3,x = demshare,
                                                    c = 0.5,cluster = cluster))
fit_ratio_units_multisingle_delta4 <- with(data_2p,
                                           rdrobust(y = ratio_units_multisingle_delta4,x = demshare,
                                                    c = 0.5,cluster = cluster))
summary(fit_ratio_units_multisingle_delta4)
fit_ratio_units_multisingle_delta5 <- with(data_2p,
                                           rdrobust(y = ratio_units_multisingle_delta5,x = demshare,
                                                    c = 0.5,cluster = cluster))
summary(fit_ratio_units_multisingle_delta5)
fit_ratio_units_multisingle_delta6 <- with(data_2p,
                                           rdrobust(y = ratio_units_multisingle_delta6,x = demshare,
                                                    c = 0.5,cluster = cluster))
summary(fit_ratio_units_multisingle_delta6)


## Figure A7b:
coefs <- rd.export.numeric.90(list(
                                fit_units_multi_delta1,
                                fit_units_multi_delta2,
                                fit_units_multi_delta3,
                                fit_units_multi_delta4,
                                fit_units_multi_delta5,
                                fit_units_multi_delta6
))

coefs$outcome_pretty = rep("Change in\nmultifamily units",6)
coefs$year = c("+1 year","+2 years","+3 years","+4 years","+5 years","+6 years")

coefs$plotorder = c(1:6)



pdf("mayors/coefplot_cbps_delta_units_longhorizon.pdf",height=3,width=6)
ggplot(filter(coefs,outcome_pretty=="Change in\nmultifamily units")) +
  geom_hline(yintercept=0, lty=2, lwd=1, colour="grey50") +
  geom_errorbar(aes(x=plotorder,ymin=cilo, ymax=cihi), 
                colour="black", width=0, size=0.5) +
  geom_errorbar(aes(x=plotorder,ymin=cilo_90, ymax=cihi_90), 
                colour="black", width=0, size=1) +
  geom_point(aes(x=plotorder,y=coef),size=4, pch=21, fill="black") +
  geom_text(aes(x=plotorder,y=coef+0.1,label=round(coef,2)),nudge_x=0.2, size=3) +
  geom_vline(xintercept = 0.7,col="red") + 
  scale_y_continuous("RD effect on change in\nlog(multifamily units + 1)",
                     breaks=seq(-2, 3, 0.5),
                     limits=c(min(coefs$cilo[which(coefs$outcome_pretty=="Change in\nmultifamily units")]),
                              max(coefs$cihi[which(coefs$outcome_pretty=="Change in\nmultifamily units")])),
                     labels=seq(-2, 3, 0.5)) +
  scale_x_continuous("Change relative to election year",
                     breaks=coefs$plotorder[which(coefs$outcome_pretty=="Change in\nmultifamily units")],
                     limits=c(0.5,6.3),
                     labels=coefs$year[which(coefs$outcome_pretty=="Change in\nmultifamily units")],position = "bottom") + 
  # coord_flip() + 
  theme_minimal()
dev.off()


## Figure A8b:
coefs.ratios <- rd.export.numeric.90(list(
                                       fit_ratio_units_multisingle_delta1,
                                       fit_ratio_units_multisingle_delta2,
                                       fit_ratio_units_multisingle_delta3,
                                       fit_ratio_units_multisingle_delta4,
                                       fit_ratio_units_multisingle_delta5,
                                       fit_ratio_units_multisingle_delta6
))

coefs.ratios$outcome_pretty = c(
                                rep("Multifamily proportion\nof total units",6)
)
coefs.ratios$year = c("+1 year","+2 years","+3 years","+4 years","+5 years","+6 years")

coefs.ratios$plotorder = rep(1:6)

pdf("mayors/coefplot_cbps_deltaratio_units_longhorizon.pdf",height=3,width=6)
ggplot(filter(coefs.ratios,outcome_pretty=="Multifamily proportion\nof total units")) +
  geom_hline(yintercept=0, lty=2, lwd=1, colour="grey50") +
  geom_vline(xintercept=0.5, lty=3, lwd=1, colour="red") + 
  geom_errorbar(aes(x=plotorder,ymin=cilo, ymax=cihi), 
                colour="black", width=0, size=0.5) +
  geom_errorbar(aes(x=plotorder,ymin=cilo_90, ymax=cihi_90), 
                colour="black", width=0, size=1) +
  geom_point(aes(x=plotorder,y=coef),size=4, pch=21, fill="black") +
  geom_text(aes(x=plotorder,y=coef+0.03,label=round(coef,2)),nudge_x=0.2, size=3) +
  scale_y_continuous("RD effect on change in\nmultifamily proportion of total units",
                     breaks=seq(-0.2, 0.3, 0.1),
                     limits=c(min(coefs.ratios$cilo[which(coefs.ratios$outcome_pretty=="Multifamily proportion\nof total units")]),
                              max(coefs.ratios$cihi[which(coefs.ratios$outcome_pretty=="Multifamily proportion\nof total units")])),
                     labels=seq(-0.2, 0.3, 0.1)) +
  scale_x_continuous("Change relative to election year",
                     breaks=coefs.ratios$plotorder[which(coefs.ratios$outcome_pretty=="Multifamily proportion\nof total units")],
                     limits=c(0.5,6.3),
                     labels=coefs.ratios$year[which(coefs.ratios$outcome_pretty=="Multifamily proportion\nof total units")],position = "bottom") + 
  # coord_flip() + 
  theme_minimal()
dev.off()





#### Nonlog Outcomes ####
fit_nonlog_units_delta23avg <- with(filter(data_2p),
                                    rdrobust(y = total_units_pc_delta23avg,x = demshare,
                                             c = 0.5,cluster = cluster
                                    ))
fit_nonlog_bldgs_delta23avg <- with(filter(data_2p),
                                    rdrobust(y = total_bldgs_pc_delta23avg,x = demshare,
                                             c = 0.5,cluster = cluster
                                    ))

fit_nonlog_units_single_delta23avg <- with(filter(data_2p),
                                          rdrobust(y = total_units_single_pc_delta23avg,x = demshare,
                                                   c = 0.5,cluster = cluster
                                          ))
fit_nonlog_bldgs_single_delta23avg <- with(filter(data_2p),
                                          rdrobust(y = total_bldgs_single_pc_delta23avg,x = demshare,
                                                   c = 0.5,cluster = cluster
                                          ))
fit_nonlog_units_multi_delta23avg <- with(filter(data_2p),
                                          rdrobust(y = total_units_multi_pc_delta23avg,x = demshare,
                                                   c = 0.5,cluster = cluster
                                          ))
writeLines(as.character(round(abs(fit_nonlog_units_multi_delta23avg$coef['Conventional', ]), 0)),"mayors/rd_effect_nonlog_units_multi_delta23avg.tex",sep = "%")

fit_nonlog_bldgs_multi_delta23avg <- with(filter(data_2p),
                                          rdrobust(y = total_bldgs_multi_pc_delta23avg,x = demshare,
                                                   c = 0.5,cluster = cluster
                                          ))
fit_nonlog_ratio_bldgs_multisingle_delta23avg <- with(filter(data_2p),
                                                      rdrobust(y = ratio_bldgs_multisingle_delta23avg,x = demshare,
                                                               c = 0.5,cluster = cluster
                                                      ))

fit_nonlog_ratio_units_multisingle_delta23avg <- with(filter(data_2p),
                                                      rdrobust(y = ratio_units_multisingle_delta23avg,x = demshare,
                                                               c = 0.5, cluster = cluster
                                                      ))


## Figure A29b ##
coefs <- rd.export.numeric.90(list(fit_nonlog_bldgs_delta23avg,
                                fit_nonlog_bldgs_single_delta23avg,
                                fit_nonlog_bldgs_multi_delta23avg,
                                fit_nonlog_units_delta23avg,
                                fit_nonlog_units_single_delta23avg,
                                fit_nonlog_units_multi_delta23avg
))

coefs$outcome_pretty = c("Total\nbuildings",
                         "Single-family\nbuildings",
                         "Multi-family\nbuildings",
                         "Total\nunits",
                         "Single-family\nunits",
                         "Multi-family\nunits")

coefs$plotorder <- c(6:1)


pdf("mayors/coefplot_cbps_delta23avg_nonlog.pdf",height=4,width=5)
ggplot(coefs) +
  geom_hline(yintercept=0, lty=2, lwd=1, colour="grey50") +
  geom_errorbar(aes(x=plotorder,ymin=cilo, ymax=cihi),position=position_dodge(width=0.8), 
                colour="black", width=0, size=0.5) +
  geom_errorbar(aes(x=plotorder,ymin=cilo_90, ymax=cihi_90),position=position_dodge(width=0.8),
                colour="black", width=0, size=1) +
  geom_point(aes(x=plotorder,y=coef),position=position_dodge(width=0.8),size=4, fill="black") +
  geom_text(aes(x=plotorder+0.2,y=coef+45,label=round(coef,2)),position=position_dodge(width=0.8), size=3) +
  scale_y_continuous("RD effect on change in\nnon-logged per 100k capita outcome between\nelection year and avg. of 2/3 years after election",
                     breaks=seq(-200, 400, 200),
                     limits=c(min(coefs$cilo),
                              max(coefs$cihi)),
                     labels=seq(-200, 400, 200)) +
  scale_x_continuous("Outcome",
                     breaks=c(coefs$plotorder),
                     limits=c(0.7,6.5),
                     labels=coefs$outcome_pretty,position = "bottom") + 
  coord_flip(ylim=c(-210,500)) + 
  theme_minimal() + 
  theme(legend.position="bottom")
dev.off()


## Remove leverage point cities:
# Irvine, CA
# Nashville-Davidson, TN
# Orlando, FL
# Henderson, NV
# Raleigh, NC

data_2p %>%
  arrange(desc(abs(total_units_multi_pc_delta23avg))) %>%
  select(place_fips,city,abb,year,total_units_pc_delta23avg,total_units_multi_pc_delta23avg,demshare)

data_2p_nooutliers <- data_2p %>%
  filter(!(place_fips %in% c(636770, 4752006,1253000,3231900,3755000)))

fit_nonlog_units_delta23avg <- with(filter(data_2p_nooutliers),
                                    rdrobust(y = total_units_pc_delta23avg,x = demshare,
                                             c = 0.5,cluster = cluster
                                    ))
fit_nonlog_bldgs_delta23avg <- with(filter(data_2p_nooutliers),
                                    rdrobust(y = total_bldgs_pc_delta23avg,x = demshare,
                                             c = 0.5,cluster = cluster
                                    ))

fit_nonlog_units_single_delta23avg <- with(filter(data_2p_nooutliers),
                                          rdrobust(y = total_units_single_pc_delta23avg,x = demshare,
                                                   c = 0.5,cluster = cluster
                                          ))
fit_nonlog_bldgs_single_delta23avg <- with(filter(data_2p_nooutliers),
                                          rdrobust(y = total_bldgs_single_pc_delta23avg,x = demshare,
                                                   c = 0.5,cluster = cluster
                                          ))
fit_nonlog_units_multi_delta23avg <- with(filter(data_2p_nooutliers),
                                          rdrobust(y = total_units_multi_pc_delta23avg,x = demshare,
                                                   c = 0.5,cluster = cluster
                                          ))
fit_nonlog_bldgs_multi_delta23avg <- with(filter(data_2p_nooutliers),
                                          rdrobust(y = total_bldgs_multi_pc_delta23avg,x = demshare,
                                                   c = 0.5,cluster = cluster
                                          ))
fit_nonlog_ratio_bldgs_multisingle_delta23avg <- with(filter(data_2p_nooutliers),
                                                      rdrobust(y = ratio_bldgs_multisingle_delta23avg,x = demshare,
                                                               c = 0.5,cluster = cluster
                                                      ))

fit_nonlog_ratio_units_multisingle_delta23avg <- with(filter(data_2p_nooutliers),
                                                      rdrobust(y = ratio_units_multisingle_delta23avg,x = demshare,
                                                               c = 0.5, cluster = cluster
                                                      ))



## Figure A28b ##
coefs <- rd.export.numeric.90(list(fit_nonlog_bldgs_delta23avg,
                                fit_nonlog_bldgs_single_delta23avg,
                                fit_nonlog_bldgs_multi_delta23avg,
                                fit_nonlog_units_delta23avg,
                                fit_nonlog_units_single_delta23avg,
                                fit_nonlog_units_multi_delta23avg
))

coefs$outcome_pretty = c("Total\nbuildings",
                         "Single-family\nbuildings",
                         "Multi-family\nbuildings",
                         "Total\nunits",
                         "Single-family\nunits",
                         "Multi-family\nunits")

coefs$plotorder <- c(6:1)



pdf("mayors/coefplot_cbps_delta23avg_nonlog_nooutliers.pdf",height=4,width=5)
ggplot(coefs) +
  geom_hline(yintercept=0, lty=2, lwd=1, colour="grey50") +
  geom_errorbar(aes(x=plotorder,ymin=cilo, ymax=cihi),position=position_dodge(width=0.8), 
                colour="black", width=0, size=0.5) +
  geom_errorbar(aes(x=plotorder,ymin=cilo_90, ymax=cihi_90),position=position_dodge(width=0.8),
                colour="black", width=0, size=1) +
  geom_point(aes(x=plotorder,y=coef),position=position_dodge(width=0.8),size=4, fill="black") +
  geom_text(aes(x=plotorder+0.2,y=coef+45,label=round(coef,2)),position=position_dodge(width=0.8), size=3) +
  scale_y_continuous("RD effect on change in\nnon-logged per 100k capita outcome between\nelection year and avg. of 2/3 years after election",
                     breaks=seq(-200, 400, 200),
                     limits=c(min(coefs$cilo),
                              max(coefs$cihi)),
                     labels=seq(-200, 400, 200)) +
  scale_x_continuous("Outcome",
                     breaks=c(coefs$plotorder),
                     limits=c(0.7,6.5),
                     labels=coefs$outcome_pretty,position = "bottom") + 
  coord_flip(ylim=c(-210,500)) + 
  theme_minimal() + 
  theme(legend.position="bottom")
dev.off()


#### Log+0.1 outcomes ####
fit_ln01_units_delta23avg <- with(filter(data_2p),
                                  rdrobust(y = total_units_ln01_delta23avg,x = demshare,
                                           c = 0.5,cluster = cluster
                                  ))
fit_ln01_bldgs_delta23avg <- with(filter(data_2p),
                                  rdrobust(y = total_bldgs_ln01_delta23avg,x = demshare,
                                           c = 0.5,cluster = cluster
                                  ))

fit_ln01_units_single_delta23avg <- with(filter(data_2p),
                                        rdrobust(y = total_units_single_ln01_delta23avg,x = demshare,
                                                 c = 0.5,cluster = cluster
                                        ))
fit_ln01_bldgs_single_delta23avg <- with(filter(data_2p),
                                        rdrobust(y = total_bldgs_single_ln01_delta23avg,x = demshare,
                                                 c = 0.5,cluster = cluster
                                        ))
fit_ln01_units_multi_delta23avg <- with(filter(data_2p),
                                        rdrobust(y = total_units_multi_ln01_delta23avg,x = demshare,
                                                 c = 0.5,cluster = cluster
                                        ))
fit_ln01_bldgs_multi_delta23avg <- with(filter(data_2p),
                                        rdrobust(y = total_bldgs_multi_ln01_delta23avg,x = demshare,
                                                 c = 0.5,cluster = cluster
                                        ))
fit_ln01_ratio_bldgs_multisingle_delta23avg <- with(filter(data_2p),
                                                    rdrobust(y = ratio_bldgs_multisingle_delta23avg,x = demshare,
                                                             c = 0.5,cluster = cluster
                                                    ))

fit_ln01_ratio_units_multisingle_delta23avg <- with(filter(data_2p),
                                                    rdrobust(y = ratio_units_multisingle_delta23avg,x = demshare,
                                                             c = 0.5, cluster = cluster
                                                    ))



## Figure A30b ##
coefs <- rd.export.numeric.90(list(fit_ln01_bldgs_delta23avg,
                                fit_ln01_bldgs_single_delta23avg,
                                fit_ln01_bldgs_multi_delta23avg,
                                fit_ln01_units_delta23avg,
                                fit_ln01_units_single_delta23avg,
                                fit_ln01_units_multi_delta23avg
))

coefs$outcome_pretty = c("Total\nbuildings",
                         "Single-family\nbuildings",
                         "Multi-family\nbuildings",
                         "Total\nunits",
                         "Single-family\nunits",
                         "Multi-family\nunits")

coefs$plotorder <- c(6:1)


pdf("mayors/coefplot_cbps_delta23avg_ln01.pdf",height=4,width=5)
ggplot(coefs) +
  geom_hline(yintercept=0, lty=2, lwd=1, colour="grey50") +
  geom_errorbar(aes(x=plotorder,ymin=cilo, ymax=cihi),position=position_dodge(width=0.8), 
                colour="black", width=0, size=0.5) +
  geom_errorbar(aes(x=plotorder,ymin=cilo_90, ymax=cihi_90),position=position_dodge(width=0.8),
                colour="black", width=0, size=1) +
  geom_point(aes(x=plotorder,y=coef),position=position_dodge(width=0.8),size=4, fill="black") +
  geom_text(aes(x=plotorder+0.2,y=coef+0.2,label=round(coef,2)),position=position_dodge(width=0.8), size=3) +
  scale_y_continuous("RD effect on change in log(outcome+0.1) between\nelection year and avg. of 2/3 years after election",
                     breaks=seq(-1, 2, 0.5),
                     limits=c(min(coefs$cilo),
                              max(coefs$cihi)),
                     labels=seq(-1, 2, 0.5)) +
  scale_x_continuous("Outcome",
                     breaks=c(coefs$plotorder),
                     limits=c(0.7,6.5),
                     labels=coefs$outcome_pretty,position = "bottom") + 
  coord_flip(ylim=c(-0.5,1.9)) + 
  theme_minimal() + 
  theme(legend.position="bottom")
dev.off()


#### Different bandwidths ####
coefs.total_bldgs_multi <- NULL
coefs.total_units_multi <- NULL
coefs.ratio_bldgs_multisingle <- NULL
coefs.ratio_units_multisingle <- NULL
for(i in seq(0.01,0.5,0.01)){
  fit_total_bldgs_multi_ln_delta23avg_temp <- with(data_2p,
                                                   rdrobust(y = total_bldgs_multi_ln_delta23avg,x = demshare,
                                                            c = 0.5,cluster = cluster,
                                                            h = i
                                                   ))
  coefs.total_bldgs_multi <- bind_rows(coefs.total_bldgs_multi,rd.export.numeric.90(list(fit_total_bldgs_multi_ln_delta23avg_temp)))
  
  
  fit_total_units_multi_ln_delta23avg_temp <- with(data_2p,
                                                   rdrobust(y = total_units_multi_ln_delta23avg,x = demshare,
                                                            c = 0.5,cluster = cluster,
                                                            h = i
                                                   ))
  coefs.total_units_multi <- bind_rows(coefs.total_units_multi,rd.export.numeric.90(list(fit_total_units_multi_ln_delta23avg_temp)))
  
  fit_ratio_bldgs_multisingle_delta23avg_temp <- with(data_2p,
                                                      rdrobust(y = ratio_bldgs_multisingle_delta23avg,x = demshare,
                                                               c = 0.5,
                                                               cluster = cluster,
                                                               h = i
                                                      ))
  coefs.ratio_bldgs_multisingle <- bind_rows(coefs.ratio_bldgs_multisingle,rd.export.numeric.90(list(fit_ratio_bldgs_multisingle_delta23avg_temp)))
  
  fit_ratio_units_multisingle_delta23avg_temp <- with(data_2p,
                                                      rdrobust(y = ratio_units_multisingle_delta23avg,x = demshare,
                                                               c = 0.5,cluster = cluster,
                                                               h = i
                                                      ))
  coefs.ratio_units_multisingle <- bind_rows(coefs.ratio_units_multisingle,rd.export.numeric.90(list(fit_ratio_units_multisingle_delta23avg_temp)))
}

## Figure A25b
pdf("mayors/bandwidths_total_bldgs_multi.pdf",width=7,height=5)
ggplot(data = coefs.total_bldgs_multi) +
  geom_point(aes(x=bw,y=coef,color=ifelse(pval<0.05,T,F)),size = 3)+
  geom_errorbar(aes(x = bw,color=ifelse(pval<0.05,T,F), 
                    ymin = cilo, ymax = cihi, height=.1)) +
  xlab("Bandwidth") + 
  ylab("RD effect on change in\nlog(multifamily buildings + 1) between election\nyear and avg. of 2/3 years after election") +
  scale_color_manual("Significant",values = c("red","blue"),breaks = c(T,F)) + 
  coord_cartesian(ylim=c(-0.5,2)) + 
  geom_hline(yintercept = 0,size=.5,colour="red",linetype="dotted") +
  theme_bw() +
  theme(axis.text = element_text(size=15),axis.title = element_text(size=15),
        axis.text.y = element_text(angle = 0, hjust = 0, color="black"),
        legend.position = "none")
dev.off()

## Figure A26b:
pdf("mayors/bandwidths_total_units_multi.pdf",width=7,height=5)
ggplot(data = coefs.total_units_multi) +
  geom_point(aes(x=bw,y=coef,color=ifelse(pval<0.05,T,F)),size = 3)+
  geom_errorbar(aes(x = bw,color=ifelse(pval<0.05,T,F), 
                    ymin = cilo, ymax = cihi, height=.1)) +
  xlab("Bandwidth") + 
  ylab("RD effect on change in\nlog(multifamily units + 1) between election\nyear and avg. of 2/3 years after election") +
  scale_color_manual("Significant",values = c("red","blue"),breaks = c(T,F)) + 
  coord_cartesian(ylim=c(-0.25,2)) + 
  geom_hline(yintercept = 0,size=.5,colour="red",linetype="dotted") +
  theme_bw() +
  theme(axis.text = element_text(size=15),axis.title = element_text(size=15),
        axis.text.y = element_text(angle = 0, hjust = 0, color="black"),
        legend.position = "none")
dev.off()


## Figure A27b:
pdf("mayors/bandwidths_ratio_units.pdf",width=7,height=5)
ggplot(data = coefs.ratio_units_multisingle) +
  geom_point(aes(x=bw,y=coef,color=ifelse(pval<0.05,T,F)),size = 3)+
  geom_errorbar(aes(x = bw,color=ifelse(pval<0.05,T,F), 
                    ymin = cilo, ymax = cihi, height=.1)) +
  xlab("Bandwidth") + 
  ylab("RD effect on change in multifamily proportion\nof total units between election year\nand avg. of 2/3 years after election") +
  scale_color_manual("Significant",values = c("red","blue"),breaks = c(T,F)) + 
  coord_cartesian(ylim=c(-0.3,0.6)) + 
  geom_hline(yintercept = 0,size=.5,colour="red",linetype="dotted") +
  theme_bw() +
  theme(axis.text = element_text(size=15),axis.title = element_text(size=15),
        axis.text.y = element_text(angle = 0, hjust = 0, color="black"),
        legend.position = "none")
dev.off()


#### Different order polynomials ####
fit_total_units_multi_ln_delta23avg_poly0 <- with(data_2p,
                                                  rdrobust(y = total_units_multi_ln_delta23avg,x = demshare,
                                                           c = 0.5,
                                                           cluster = cluster,
                                                           p=0
                                                  ))
fit_total_units_multi_ln_delta23avg <- with(data_2p,
                                            rdrobust(y = total_units_multi_ln_delta23avg,x = demshare,
                                                     c = 0.5,
                                                     cluster = cluster,
                                                     p=1
                                            ))
fit_total_units_multi_ln_delta23avg_poly2 <- with(data_2p,
                                                  rdrobust(y = total_units_multi_ln_delta23avg,x = demshare,
                                                           c = 0.5,cluster = cluster,
                                                           p=2
                                                  ))
fit_total_units_multi_ln_delta23avg_poly3 <- with(data_2p,
                                                  rdrobust(y = total_units_multi_ln_delta23avg,x = demshare,
                                                           c = 0.5,cluster = cluster,
                                                           p=3
                                                  ))
fit_total_units_multi_ln_delta23avg_poly4 <- with(data_2p,
                                                  rdrobust(y = total_units_multi_ln_delta23avg,x = demshare,
                                                           c = 0.5,cluster = cluster,
                                                           p=4
                                                  ))

fit_ratio_bldgs_multisingle_delta23avg_poly0 <- with(data_2p,
                                                     rdrobust(y = ratio_bldgs_multisingle_delta23avg,x = demshare,
                                                              c = 0.5,
                                                              # cluster = cluster,
                                                              p=0
                                                     ))
fit_ratio_bldgs_multisingle_delta23avg <- with(data_2p,
                                               rdrobust(y = ratio_bldgs_multisingle_delta23avg,x = demshare,
                                                        c = 0.5,
                                                        cluster = cluster,
                                                        p=1
                                               ))
fit_ratio_bldgs_multisingle_delta23avg_poly2 <- with(data_2p,
                                                     rdrobust(y = ratio_bldgs_multisingle_delta23avg,x = demshare,
                                                              c = 0.5,
                                                              cluster = cluster,
                                                              p=2
                                                     ))
fit_ratio_bldgs_multisingle_delta23avg_poly3 <- with(data_2p,
                                                     rdrobust(y = ratio_bldgs_multisingle_delta23avg,x = demshare,
                                                              c = 0.5,
                                                              cluster = cluster,
                                                              p=3
                                                     ))
fit_ratio_bldgs_multisingle_delta23avg_poly4 <- with(data_2p,
                                                     rdrobust(y = ratio_bldgs_multisingle_delta23avg,x = demshare,
                                                              c = 0.5,
                                                              cluster = cluster,
                                                              p=4
                                                     ))
fit_ratio_units_multisingle_delta23avg_poly0 <- with(data_2p,
                                                     rdrobust(y = ratio_units_multisingle_delta23avg,x = demshare,
                                                              c = 0.5,
                                                              cluster = cluster,
                                                              p=0
                                                     ))
fit_ratio_units_multisingle_delta23avg <- with(data_2p,
                                               rdrobust(y = ratio_units_multisingle_delta23avg,x = demshare,
                                                        c = 0.5,cluster = cluster,
                                                        p=1
                                               ))
fit_ratio_units_multisingle_delta23avg_poly2 <- with(data_2p,
                                                     rdrobust(y = ratio_units_multisingle_delta23avg,x = demshare,
                                                              c = 0.5,cluster = cluster,
                                                              p=2
                                                     ))
fit_ratio_units_multisingle_delta23avg_poly3 <- with(data_2p,
                                                     rdrobust(y = ratio_units_multisingle_delta23avg,x = demshare,
                                                              c = 0.5,cluster = cluster,
                                                              p=3
                                                     ))
fit_ratio_units_multisingle_delta23avg_poly4 <- with(data_2p,
                                                     rdrobust(y = ratio_units_multisingle_delta23avg,x = demshare,
                                                              c = 0.5,cluster = cluster,
                                                              p=4
                                                     ))


coefs <- rd.export.numeric.90(list(
  fit_total_units_multi_ln_delta23avg_poly0,
  fit_total_units_multi_ln_delta23avg,
  fit_total_units_multi_ln_delta23avg_poly2,
  fit_total_units_multi_ln_delta23avg_poly3,
  fit_total_units_multi_ln_delta23avg_poly4,
  
  fit_ratio_bldgs_multisingle_delta23avg_poly0,
  fit_ratio_bldgs_multisingle_delta23avg,
  fit_ratio_bldgs_multisingle_delta23avg_poly2,
  fit_ratio_bldgs_multisingle_delta23avg_poly3,
  fit_ratio_bldgs_multisingle_delta23avg_poly4,
  
  fit_ratio_units_multisingle_delta23avg_poly0,
  fit_ratio_units_multisingle_delta23avg,
  fit_ratio_units_multisingle_delta23avg_poly2,
  fit_ratio_units_multisingle_delta23avg_poly3,
  fit_ratio_units_multisingle_delta23avg_poly4
))

coefs$outcome_pretty = c(rep("Logged multifamily units",5),
                         rep("Multifamily proportion of buildings",5),
                         rep("Multifamily proportion of units",5))
coefs$poly = rep(0:4,3)


## Figure A31b:
pdf("mayors/coefplot_units_multi_poly.pdf",height=3,width=6)
ggplot(filter(coefs,outcome_pretty=="Logged multifamily units")) +
  geom_hline(yintercept=0, lty=2, lwd=1, colour="grey50") +
  geom_errorbar(aes(x=poly,ymin=cilo, ymax=cihi), 
                colour="black", width=0, size=0.5) +
  geom_errorbar(aes(x=poly,ymin=cilo_90, ymax=cihi_90), 
                colour="black", width=0, size=1) +
  geom_point(aes(x=poly,y=coef),size=4, pch=21, fill="black") +
  geom_text(aes(x=poly,y=coef+0.02,label=round(coef,2)),nudge_x=0.2, size=3) +
  scale_y_continuous(str_wrap("RD effect on change in log(multifamily units + 1) between election year and avg. of 2/3 years after election",40),
                     breaks=seq(-1, 3, 1),
                     limits=c(min(coefs$cilo[coefs$outcome_pretty=="Logged multifamily units"])-0.3,
                              max(coefs$cihi[coefs$outcome_pretty=="Logged multifamily units"])+0.1),
                     labels=round(seq(-1, 3, 1),2)) +
  scale_x_continuous("Order of polynomial",
                     breaks=coefs$poly[coefs$outcome_pretty=="Logged multifamily units"],
                     limits=c(-0.3,4.3),
                     labels=coefs$poly[coefs$outcome_pretty=="Logged multifamily units"],position = "bottom") + 
  # coord_flip() + 
  theme_minimal()
dev.off()

## Figure A32b:
pdf("mayors/coefplot_ratio_bldgs_multi_poly.pdf",height=3,width=6)
ggplot(filter(coefs,outcome_pretty=="Multifamily proportion of buildings")) +
  geom_hline(yintercept=0, lty=2, lwd=1, colour="grey50") +
  geom_errorbar(aes(x=poly,ymin=cilo, ymax=cihi), 
                colour="black", width=0, size=0.5) +
  geom_errorbar(aes(x=poly,ymin=cilo_90, ymax=cihi_90), 
                colour="black", width=0, size=1) +
  geom_point(aes(x=poly,y=coef),size=4, pch=21, fill="black") +
  geom_text(aes(x=poly,y=coef+0.02,label=round(coef,2)),nudge_x=0.2, size=3) +
  scale_y_continuous(str_wrap("RD effect on change in multifamily proportion of total buildings between election year and avg. of 2/3 years after election",40),
                     breaks=seq(-1, 1, 0.05),
                     limits=c(min(coefs$cilo[coefs$outcome_pretty=="Multifamily proportion of buildings"])-0.01,
                              max(coefs$cihi[coefs$outcome_pretty=="Multifamily proportion of buildings"])+0.01),
                     labels=round(seq(-1, 1, 0.05),2)) +
  scale_x_continuous("Order of polynomial",
                     breaks=coefs$poly[coefs$outcome_pretty=="Multifamily proportion of buildings"],
                     limits=c(-0.3,4.3),
                     labels=coefs$poly[coefs$outcome_pretty=="Multifamily proportion of buildings"],position = "bottom") + 
  # coord_flip() + 
  theme_minimal()
dev.off()

## Figure A33b:
pdf("mayors/coefplot_ratio_units_multi_poly.pdf",height=3,width=6)
ggplot(filter(coefs,outcome_pretty=="Multifamily proportion of units")) +
  geom_hline(yintercept=0, lty=2, lwd=1, colour="grey50") +
  geom_errorbar(aes(x=poly,ymin=cilo, ymax=cihi), 
                colour="black", width=0, size=0.5) +
  geom_errorbar(aes(x=poly,ymin=cilo_90, ymax=cihi_90), 
                colour="black", width=0, size=1) +
  geom_point(aes(x=poly,y=coef),size=4, pch=21, fill="black") +
  geom_text(aes(x=poly,y=coef+0.02,label=round(coef,2)),nudge_x=0.2, size=3) +
  scale_y_continuous(str_wrap("RD effect on change in multifamily proportion of total units between election year and avg. of 2/3 years after election",40),
                     breaks=seq(-1, 1, 0.2),
                     limits=c(min(coefs$cilo[coefs$outcome_pretty=="Multifamily proportion of units"])-0.03,
                              max(coefs$cihi[coefs$outcome_pretty=="Multifamily proportion of units"])+0.01),
                     labels=round(seq(-1, 1, 0.2),2)) +
  scale_x_continuous("Order of polynomial",
                     breaks=coefs$poly[coefs$outcome_pretty=="Multifamily proportion of units"],
                     limits=c(-0.3,4.3),
                     labels=coefs$poly[coefs$outcome_pretty=="Multifamily proportion of units"],position = "bottom") + 
  # coord_flip() + 
  theme_minimal()
dev.off()


#### Randomization Inference ####
fit_RI_total_units_multi_ln_delta23avg <- with(data_2p,
                                               rdrandinf(Y = total_units_multi_ln_delta23avg,
                                                         R = demshare,
                                                         wl=0.48,wr=0.52,
                                                         rep=10000,
                                                         cutoff = 0.5,
                                                         ci=0.05,
                                                         seed=02139
                                                         # cluster = cluster,weight=weight
                                               ))
fit_RI_total_units_multi_ln_delta234avg <- with(data_2p,
                                                rdrandinf(Y = total_units_multi_ln_delta234avg,
                                                          R = demshare,
                                                          wl=0.48,wr=0.52,
                                                          rep=10000,
                                                          cutoff = 0.5,
                                                          ci=0.05,
                                                          seed=02139
                                                          # cluster = cluster,weight=weight
                                                ))
fit_RI_total_units_multi_ln_deltaterm234avg <- with(data_2p,
                                                    rdrandinf(Y = total_units_multi_ln_deltaterm234avg,
                                                              R = demshare,
                                                              wl=0.48,wr=0.52,
                                                              rep=10000,
                                                              cutoff = 0.5,
                                                              ci=0.05,
                                                              seed=02139
                                                    ))
fit_RI_total_units_multi_ln_deltaterm4avg <- with(data_2p,
                                                  rdrandinf(Y = total_units_multi_ln_deltaterm4avg,
                                                            R = demshare,
                                                            wl=0.48,wr=0.52,
                                                            rep=10000,
                                                            cutoff = 0.5,
                                                            ci=0.05,
                                                            seed=02139
                                                  ))


tab_cbps_ri <- rd.export.ri(list(fit_RI_total_units_multi_ln_delta23avg,
                                 fit_RI_total_units_multi_ln_delta234avg,
                                 fit_RI_total_units_multi_ln_deltaterm234avg,
                                 fit_RI_total_units_multi_ln_deltaterm4avg
)) %>% 
  as.data.frame()

tab_cbps_ri$DV = c("Multi-family units, T+2/3 Avg","",
                   "Multi-family units, T+2-4 Avg","",
                   "Multi-family units, Avg. of 2-4 years post-election - 4-yr avg. pre-election","",
                   "Multi-family units, Avg. of 1-4 years post-election - 4-yr avg. pre-election",""
)
tab_cbps_ri <- select(tab_cbps_ri,DV,everything())

## Table A11:
print(xtable(tab_cbps_ri),
      include.rownames = F,
      floating = F,
      file = "mayors/tab_cbps_ri.tex")



## Other Outcomes -------------------------------------------------------------

#### ZHVI ####

fit_zhvi_delta1 <- with(data_2p,
                        rdrobust(y = zhvi_ln_delta1,x = demshare,
                                 c = 0.5,cluster = cluster))
summary(fit_zhvi_delta1)
fit_zhvi_delta2 <- with(data_2p,
                        rdrobust(y = zhvi_ln_delta2,x = demshare,
                                 c = 0.5,cluster = cluster))
summary(fit_zhvi_delta2)
fit_zhvi_delta3 <- with(data_2p,
                        rdrobust(y = zhvi_ln_delta3,x = demshare,
                                 c = 0.5,cluster = cluster))
summary(fit_zhvi_delta3)
fit_zhvi_delta4 <- with(data_2p,
                        rdrobust(y = zhvi_ln_delta4,x = demshare,
                                 c = 0.5,cluster = cluster))
summary(fit_zhvi_delta4)
fit_zhvi_delta5 <- with(data_2p,
                        rdrobust(y = zhvi_ln_delta5,x = demshare,
                                 c = 0.5,cluster = cluster))
summary(fit_zhvi_delta5)
fit_zhvi_delta23avg <- with(data_2p,
                            rdrobust(y = zhvi_ln_delta23avg,x = demshare,
                                     c = 0.5,cluster = cluster))
summary(fit_zhvi_delta23avg)
writeLines(as.character(100*round(abs(fit_zhvi_delta23avg$coef['Conventional', ]), 2)),"mayors/rd_effect_zhvi_delta23avg.tex",sep = "%")

fit_zhvi_deltaterm4avg <- with(data_2p,
                              rdrobust(y = zhvi_ln_deltaterm4avg,x = demshare,
                                       c = 0.5,cluster = cluster))
summary(fit_zhvi_deltaterm4avg)


## Figure A10b:
coefs <- rd.export.numeric.90(list(fit_zhvi_delta1,
                                fit_zhvi_delta2,
                                fit_zhvi_delta3,
                                fit_zhvi_delta23avg,
                                fit_zhvi_delta4,
                                fit_zhvi_deltaterm4avg
))

coefs$outcome_pretty = c("+1 year\npost-election","+2","+3","Avg. of 2-3\nyears post-election","+4","Avg. of 1-4 years\npost-election\nrelative to 4-year\navg. pre-election")
coefs$plotorder <- 1:6


pdf("mayors/coefplot_zhvi_deltas.pdf",height=3.5,width=5)
ggplot(coefs) +
  geom_hline(yintercept=0, lty=2, lwd=1, colour="grey50") +
  geom_vline(xintercept=0.5, lty=3, lwd=1, colour="red") +
  geom_errorbar(aes(x=plotorder,ymin=cilo, ymax=cihi), 
                colour="black", width=0, size=0.5) +
  geom_errorbar(aes(x=plotorder,ymin=cilo_90, ymax=cihi_90),
                colour="black", width=0, size=1) +
  geom_point(aes(x=plotorder,y=coef),size=4, pch=21, fill="black") +
  geom_text(aes(x=plotorder,y=coef+0.02,label=round(coef,2)),nudge_x=0.3, size=3) +
  scale_y_continuous("RD effect on change in log(ZHVI)\nbetween election year and...",
                     breaks=seq(-0.5, 0.5, 0.1),
                     limits=c(min(coefs$cilo),
                              max(coefs$cihi)+0.05),
                     labels=seq(-0.5, 0.5, 0.1)) +
  scale_x_continuous("Outcome year",
                     breaks=coefs$plotorder,
                     limits=c(0,6.3),
                     labels=coefs$outcome_pretty,position = "bottom") + 
  # coord_flip() + 
  theme_minimal()
dev.off()




##### ZHVI - polynomials #####
fit_zhvi_ln_delta23avg_poly0 <- with(data_2p,
                                     rdrobust(y = zhvi_ln_delta23avg,x = demshare,
                                              c = 0.5,
                                              cluster = cluster,
                                              p=0
                                     ))

fit_zhvi_ln_delta23avg_poly2 <- with(data_2p,
                                     rdrobust(y = zhvi_ln_delta23avg,x = demshare,
                                              c = 0.5,
                                              cluster = cluster,                                     
                                              p=2
                                     ))
fit_zhvi_ln_delta23avg_poly3 <- with(data_2p,
                                     rdrobust(y = zhvi_ln_delta23avg,x = demshare,
                                              c = 0.5,
                                              cluster = cluster,                                     
                                              p=3
                                     ))
fit_zhvi_ln_delta23avg_poly4 <- with(data_2p,
                                     rdrobust(y = zhvi_ln_delta23avg,x = demshare,
                                              c = 0.5,
                                              cluster = cluster,                                     
                                              p=4))

## Figure A12b:
coefs <- rd.export.numeric.90(list(fit_zhvi_ln_delta23avg_poly0,
                                fit_zhvi_delta23avg,
                                fit_zhvi_ln_delta23avg_poly2,
                                fit_zhvi_ln_delta23avg_poly3,
                                fit_zhvi_ln_delta23avg_poly4
))

coefs$outcome_pretty = "ZHVI avg. of 2-3\nyears post-election"
coefs$poly = 0:4

pdf("mayors/coefplot_zhvi_poly.pdf",height=3,width=6)
ggplot(coefs) +
  geom_hline(yintercept=0, lty=2, lwd=1, colour="grey50") +
  geom_errorbar(aes(x=poly,ymin=cilo, ymax=cihi), 
                colour="black", width=0, size=0.5) +
  geom_errorbar(aes(x=poly,ymin=cilo_90, ymax=cihi_90), 
                colour="black", width=0, size=1) +
  geom_point(aes(x=poly,y=coef),size=4, pch=21, fill="black") +
  geom_text(aes(x=poly,y=coef+0.02,label=round(coef,2)),nudge_x=0.2, size=3) +
  scale_y_continuous("RD effect on change in\nlogged ZHVI between election year\nand avg. of 2/3 years after election",
                     breaks=seq(-0.5, 0.2, 0.1),
                     limits=c(min(coefs$cilo)-0.03,
                              max(coefs$cihi)+0.01),
                     labels=round(seq(-0.5, 0.2, 0.1),2)) +
  scale_x_continuous("Order of polynomial",
                     breaks=coefs$poly,
                     limits=c(-0.3,4.3),
                     labels=coefs$poly,position = "bottom") + 
  theme_minimal()
dev.off()


##### ZHVI - diff bandwidths #####
coefs.zhvi <- NULL
for(i in seq(0.01,0.5,0.01)){
  fit_zhvi_delta23avg_temp <- with(data_2p,
                                   rdrobust(y = zhvi_ln_delta23avg,x = demshare,
                                            c = 0.5,cluster = cluster,
                                            h = i
                                   ))
  coefs.zhvi <- bind_rows(coefs.zhvi,rd.export.numeric.90(list(fit_zhvi_delta23avg_temp)))
  
}

## Figure A11b:
pdf("mayors/bandwidths_zhvi.pdf",width=7,height=5)
ggplot(data = coefs.zhvi) +
  geom_point(aes(x=bw,y=coef,color=ifelse(pval<0.05,T,F)),size = 3)+
  geom_errorbar(aes(x = bw,color=ifelse(pval<0.05,T,F), 
                    ymin = cilo, ymax = cihi, height=.1)) +
  xlab("Bandwidth") + 
  ylab("Effect on change in logged ZHVI,\navg. of 2/3 years after election") +
  scale_color_manual("Significant",values = c("red","blue"),breaks = c(T,F)) + 
  coord_cartesian(ylim=c(-0.5,0.35)) + 
  geom_hline(yintercept = 0,size=.5,colour="red",linetype="dotted") +
  theme_minimal() +
  theme(axis.text = element_text(size=15),axis.title = element_text(size=15),
        axis.text.y = element_text(angle = 0, hjust = 0, color="black"),
        legend.position = "none")
dev.off()



#### Eviction Outcomes ####
fit_evictionrate_delta1 <- with(data_2p,
                                rdrobust(y = evictionrate_delta1,x = demshare,
                                         c = 0.5,cluster = cluster))
summary(fit_evictionrate_delta1)
fit_evictionrate_delta2 <- with(data_2p,
                                rdrobust(y = evictionrate_delta2,x = demshare,
                                         c = 0.5,cluster = cluster))
summary(fit_evictionrate_delta2)
fit_evictionrate_delta3 <- with(data_2p,
                                rdrobust(y = evictionrate_delta3,x = demshare,
                                         c = 0.5,cluster = cluster))
summary(fit_evictionrate_delta3)
fit_evictionrate_delta4 <- with(data_2p,
                                rdrobust(y = evictionrate_delta4,x = demshare,
                                         c = 0.5,cluster = cluster))
summary(fit_evictionrate_delta4)
fit_evictionrate_delta5 <- with(data_2p,
                                rdrobust(y = evictionrate_delta5,x = demshare,
                                         c = 0.5,cluster = cluster))
summary(fit_evictionrate_delta5)
fit_evictionrate_delta23avg <- with(data_2p,
                                    rdrobust(y = evictionrate_delta23avg,x = demshare,
                                             c = 0.5,cluster = cluster))
summary(fit_evictionrate_delta23avg)
fit_evictionrate_deltaterm4avg <- with(data_2p,
                                       rdrobust(y = evictionrate_deltaterm4avg,x = demshare,
                                                c = 0.5,cluster = cluster))
summary(fit_evictionrate_deltaterm4avg)


## Figure A3b:
coefs <- rd.export.numeric.90(list(fit_evictionrate_delta1,
                                fit_evictionrate_delta2,
                                fit_evictionrate_delta3,
                                fit_evictionrate_delta23avg,
                                fit_evictionrate_delta4,
                                fit_evictionrate_deltaterm4avg
))

coefs$outcome_pretty = c("+1 year\npost-election","+2","+3","Avg. of 2-3\nyears post-election","+4","Avg. of 1-4 years\npost-election\nrelative to 4-year\navg. pre-election")
coefs$plotorder <- 1:6

pdf("mayors/coefplot_evictionrate_deltas.pdf",height=3.5,width=5)
ggplot(coefs) +
  geom_hline(yintercept=0, lty=2, lwd=1, colour="grey50") +
  geom_vline(xintercept=0.5, lty=3, lwd=1, colour="red") +
  geom_errorbar(aes(x=plotorder,ymin=cilo, ymax=cihi), 
                colour="black", width=0, size=0.5) +
  geom_errorbar(aes(x=plotorder,ymin=cilo_90, ymax=cihi_90),
                colour="black", width=0, size=1) +
  geom_point(aes(x=plotorder,y=coef),size=4, pch=21, fill="black") +
  geom_text(aes(x=plotorder,y=coef+0.02,label=round(coef,2)),nudge_x=0.3, size=3) +
  scale_y_continuous("RD effect on change in eviction\nrate between election year and...",
                     breaks=seq(-2, 3.5, 0.5),
                     limits=c(min(coefs$cilo),
                              max(coefs$cihi)),
                     labels=seq(-2, 3.5, 0.5)) +
  scale_x_continuous("Outcome year",
                     breaks=coefs$plotorder,
                     limits=c(0.5,6.3),
                     labels=coefs$outcome_pretty,position = "bottom") + 
  # coord_flip() + 
  theme_minimal()
dev.off()


#### LIHTC Outcomes ####

fit_lihtc_units_delta1 <- with(data_2p,
                               rdrobust(y = lihtc_units_ln_delta1,x = demshare,
                                        c = 0.5,cluster = cluster))
summary(fit_lihtc_units_delta1)
fit_lihtc_units_delta2 <- with(data_2p,
                               rdrobust(y = lihtc_units_ln_delta2,x = demshare,
                                        c = 0.5,cluster = cluster))
summary(fit_lihtc_units_delta2)
fit_lihtc_units_delta3 <- with(data_2p,
                               rdrobust(y = lihtc_units_ln_delta3,x = demshare,
                                        c = 0.5,cluster = cluster))
summary(fit_lihtc_units_delta3)
fit_lihtc_units_delta4 <- with(data_2p,
                               rdrobust(y = lihtc_units_ln_delta4,x = demshare,
                                        c = 0.5,cluster = cluster))
summary(fit_lihtc_units_delta4)
fit_lihtc_units_delta5 <- with(data_2p,
                               rdrobust(y = lihtc_units_ln_delta5,x = demshare,
                                        c = 0.5,cluster = cluster))
summary(fit_lihtc_units_delta5)
fit_lihtc_units_delta23avg <- with(data_2p,
                                   rdrobust(y = lihtc_units_ln_delta23avg,x = demshare,
                                            c = 0.5,cluster = cluster))
summary(fit_lihtc_units_delta23avg)
fit_lihtc_units_deltaterm4avg <- with(data_2p,
                                      rdrobust(y = lihtc_units_ln_deltaterm4avg,x = demshare,
                                               c = 0.5,cluster = cluster))
summary(fit_lihtc_units_deltaterm4avg)


## Figure A15b:
coefs <- rd.export.numeric.90(list(fit_lihtc_units_delta1,
                                fit_lihtc_units_delta2,
                                fit_lihtc_units_delta3,
                                fit_lihtc_units_delta23avg,
                                fit_lihtc_units_delta4,
                                fit_lihtc_units_deltaterm4avg
))


coefs$outcome_pretty = c("+1 year\npost-election","+2","+3","Avg. of 2-3\nyears post-election","+4","Avg. of 1-4 years\npost-election\nrelative to 4-year\navg. pre-election")
coefs$plotorder <- 1:6

pdf("mayors/coefplot_lihtc_units_deltas.pdf",height=3.5,width=5)
ggplot(coefs) +
  geom_hline(yintercept=0, lty=2, lwd=1, colour="grey50") +
  geom_vline(xintercept=0.5, lty=3, lwd=1, colour="red") +
  geom_errorbar(aes(x=plotorder,ymin=cilo, ymax=cihi), 
                colour="black", width=0, size=0.5) +
  geom_errorbar(aes(x=plotorder,ymin=cilo_90, ymax=cihi_90),
                colour="black", width=0, size=1) +
  geom_point(aes(x=plotorder,y=coef),size=4, pch=21, fill="black") +
  geom_text(aes(x=plotorder,y=coef+0.02,label=round(coef,2)),nudge_x=0.5, size=3) +
  scale_y_continuous("RD effect on change in\nlog(LIHTC-subsidized units + 1)\nbetween election year and...",
                     breaks=seq(-5, 5, 0.5),
                     limits=c(min(coefs$cilo),
                              max(coefs$cihi)),
                     labels=seq(-5, 5, 0.5)) +
  scale_x_continuous("Outcome year",
                     breaks=coefs$plotorder,
                     limits=c(0,6.5),
                     labels=coefs$outcome_pretty,position = "bottom") + 
  # coord_flip() + 
  theme_minimal()
dev.off()



#### HUD Outcomes ####

fit_total_spending_allhud_delta1 <- with(data_2p,
                                         rdrobust(y = total_spending_allhud_ln_delta1,x = demshare,
                                                  c = 0.5,cluster = cluster))
summary(fit_total_spending_allhud_delta1)
fit_total_spending_allhud_delta2 <- with(data_2p,
                                         rdrobust(y = total_spending_allhud_ln_delta2,x = demshare,
                                                  c = 0.5,cluster = cluster))
summary(fit_total_spending_allhud_delta2)
fit_total_spending_allhud_delta3 <- with(data_2p,
                                         rdrobust(y = total_spending_allhud_ln_delta3,x = demshare,
                                                  c = 0.5,cluster = cluster))
summary(fit_total_spending_allhud_delta3)
fit_total_spending_allhud_delta4 <- with(data_2p,
                                         rdrobust(y = total_spending_allhud_ln_delta4,x = demshare,
                                                  c = 0.5,cluster = cluster))
summary(fit_total_spending_allhud_delta4)
fit_total_spending_allhud_delta5 <- with(data_2p,
                                         rdrobust(y = total_spending_allhud_ln_delta5,x = demshare,
                                                  c = 0.5,cluster = cluster))
summary(fit_total_spending_allhud_delta5)
fit_total_spending_allhud_delta23avg <- with(data_2p,
                                             rdrobust(y = total_spending_allhud_ln_delta23avg,x = demshare,
                                                      c = 0.5,cluster = cluster))
summary(fit_total_spending_allhud_delta23avg)
fit_total_spending_allhud_deltaterm4avg <- with(data_2p,
                                                rdrobust(y = total_spending_allhud_ln_deltaterm4avg,x = demshare,
                                                         c = 0.5,cluster = cluster))
summary(fit_total_spending_allhud_deltaterm4avg)


## Figure A14b:
coefs <- rd.export.numeric.90(list(fit_total_spending_allhud_delta1,
                                fit_total_spending_allhud_delta2,
                                fit_total_spending_allhud_delta3,
                                fit_total_spending_allhud_delta23avg,
                                fit_total_spending_allhud_delta4,
                                fit_total_spending_allhud_deltaterm4avg
))

coefs$outcome_pretty = c("+1 year\npost-election","+2","+3","Avg. of 2-3\nyears post-election","+4","Avg. of 1-4 years\npost-election\nrelative to 4-year\navg. pre-election")
coefs$plotorder <- 1:6

pdf("mayors/coefplot_total_spending_allhud_deltas.pdf",height=3.5,width=5)
ggplot(coefs) +
  geom_hline(yintercept=0, lty=2, lwd=1, colour="grey50") +
  geom_vline(xintercept=0.5, lty=3, lwd=1, colour="red") +
  geom_errorbar(aes(x=plotorder,ymin=cilo, ymax=cihi), 
                colour="black", width=0, size=0.5) +
  geom_errorbar(aes(x=plotorder,ymin=cilo_90, ymax=cihi_90),
                colour="black", width=0, size=1) +
  geom_point(aes(x=plotorder,y=coef),size=4, pch=21, fill="black") +
  geom_text(aes(x=plotorder,y=coef+0.02,label=round(coef,2)),nudge_x=0.3, size=3) +
  scale_y_continuous("RD effect on change in\nlog(total HUD spending + 1)\nbetween election year and...",
                     breaks=seq(-0.5, 0.5, 0.1),
                     limits=c(min(coefs$cilo),
                              max(coefs$cihi)),
                     labels=seq(-0.5, 0.5, 0.1)) +
  scale_x_continuous("Outcome year",
                     breaks=coefs$plotorder,
                     limits=c(0,6.3),
                     labels=coefs$outcome_pretty,position = "bottom") + 
  # coord_flip() + 
  theme_minimal()
dev.off()

