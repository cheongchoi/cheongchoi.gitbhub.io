# -----------------------------------------------------------------------------------
# Paper title:  Narcissism and Political Orientations
# Authors:      Peter K Hatemi & Zoltan Fazekas
# Contact:      phatemi@gmail.com; zoltan.fazekas@gmail.com
# Journal:      American Journal of Political Science
# Date:         10 April 2018
# Description:  Script generating all results (including SI) reported in paper
# R version:    R version 3.4.3 (2017-11-30) -- "Kite-Eating Tree"
# Note:         Session Info is supplied as a separate .txt file in reproduction folder uploaded
# -----------------------------------------------------------------------------------

# -----------------------------------------------------------------------------------
# Package installation preface
# -----------------------------------------------------------------------------------
# Most packages used are available to install from CRAN
# Exception is patchwork - through devtools from github
#
# For package installation, uncomment the following lines IF needed:
# Warning: does not check whether you have any of these already installed
#          some packages have dependencies that might need compilation from source, you will be prompted
#          you can answer no (faster, and makes no difference)
# used_packages <- c("psych", "dummies", "dplyr", "reshape2", "lavaan",
#                   "semTools", "polycor", "ggplot2", "stringr", "MASS",
#                   "Zelig", "ZeligChoice", "rstan", "rstanarm", "texreg", "stargazer")
# install.packages(used_packages, dependencies = TRUE)
# install.packages("devtools")
# devtools::install_github("thomasp85/patchwork")
# -----------------------------------------------------------------------------------

# install.packages("E:/narcissism/dummies_1.5.6.tar.gz", repos = NULL, type = "source")
# load packages
library("psych")
library("dummies")
library("dplyr")
library("reshape2")
library("lavaan")
library("semTools")
library("polycor")
library("ggplot2")
library("stringr")

library("MASS")
library("Zelig")
library("ZeligChoice")
library("rstan")
library("rstanarm")

library("texreg")
library("stargazer")

# note: only used to put together figures for final display
#   sometimes generates warnings/device issues on particular machines
#   also it might require restarting R, conflict error (sometimes) happens with ggplot2 after first installing the package
#   overall, if this is an issue, plots are still created and can be manually put together in document, hence package not needed
library("patchwork")  

# helper functions
se_mean <- function(x) sqrt(var(x, na.rm = TRUE)/length(na.omit(x))) # standard error of the mean
two_sd <- function(x){
  (x - mean(x, na.rm = TRUE))/(2*sd(x, na.rm = TRUE))
} # center around mean and divide by 2 standard deviations

# import raw data from Yougov
# script in same folder as data, and that should be working directory
us <- read.csv("us16_yougov.csv", stringsAsFactors = FALSE, header = TRUE)
# dim(us) # 750 observations, we start with (full data)

# -----------------------------------------------------------------------------------
# ///
#
# PART 1: Recoding covariates and outcomes, export descriptives
#         (not including narcissism)
# ///
# -----------------------------------------------------------------------------------

# -----------------------------------------------------------------------------------
# Supplementary Information 1: Sample details (on raw data)
# -----------------------------------------------------------------------------------
us_summary <- dplyr::select(us, gender, educ, birthyr, race, faminc, pid7, ideo5) %>% 
  mutate(age     = 2016 - birthyr,
         faminc  = ifelse(faminc > 30, NA, faminc), # one entry where codebook label not meaningful (either mislabeled/wrong size)
         birthyr = NULL) %>% 
  data.frame() %>% 
  dummies::dummy.data.frame(dummy.class = "integer") # splitting up categorical variables into dummies, for proportions in summary

# note: for gender, original scale 2 = female
stargazer(us_summary, summary = TRUE, type = "text") # edited in text for labeling
rm(us_summary)
# -----------------------------------------------------------------------------------
# recode covariates
# -----------------------------------------------------------------------------------
us <- us %>% 
  mutate(female        = gender - 1, # gender
         age           = 2016 - birthyr - 18, # age; to range from 0, where 0 = 18 years old
         edu_cat       = ifelse(educ > 2, 1, 0), # dichotomous education (1 = some college)
         not_caucasian = ifelse(us$race > 1, 1, 0), # race, dichotomous
         relig         = 4 - pew_religimp, # reverse to range low to high importance
         income        = ifelse(faminc > 30, NA, (faminc - 1)/15), # income
         demrep        = ifelse(pid7 > 7, NA, pid7 - 1), # 7-point party ID, recoded to range 0-6, S Dem to S Rep
         pid3          = case_when(pid7 < 4 ~ -1,
                                   pid7 == 4 ~ 0,
                                   pid7 > 4 & pid7 < 8 ~ 1,
                                   pid7 > 7 ~ NA_real_,
                                   TRUE ~ as.numeric(pid7)) # party ID control (3 cateogry PID, Dem, Indep, Rep)
         ) 

# -----------------------------------------------------------------------------------
# recode outcome variables
# original coding described in the codebook
# recoding is guided by the goal of higher values ~ more conservative position
# and 0-1 range
# -----------------------------------------------------------------------------------
us <- us %>%
  mutate(leftright  = ifelse(ideo5 > 5, NA, (ideo5 - 1)/4), # liberal-conservative ideology (recoded to range 0 to 1, Very Liberal to Very Conservative)
         economy    = case_when(e14_deficit == 2 ~ 0,
                               e14_deficit == 1 ~ 1,
                               e14_deficit == 3 ~ 0.5,
                               e14_deficit > 3 ~ NA_real_,
                               TRUE ~ as.numeric(e14_deficit)),
         immigrants = ifelse(immigration > 3, NA, (immigration - 1)/2),
         ref        = (refugees - 1)/6,
         guns       = ifelse(us$e14_guns > 5, NA, (us$e14_guns - 1)/4),
         police     = case_when(policing == 1 ~ 0,
                                policing == 2 ~ 1,
                                policing > 2 ~ NA_real_,
                                TRUE ~ as.numeric(policing))
        )

ideo_vars <- c("economy", "immigrants", "ref", "guns", "police") # store the recoded ideo var names

# check coding by looking at correlation and reliability
# cor(us[, c("leftright", "demrep", ideo_vars)], use = "complete")
# psych::alpha(us[, ideo_vars])

# -----------------------------------------------------------------------------------
# Supplementary Information 4: Ideology correlations
# -----------------------------------------------------------------------------------
ideo_cor <- us[, c("unique_id", "leftright", ideo_vars, "pid3")]
nc <- polycor::hetcor(ideo_cor[, c("leftright", ideo_vars, "pid3")])$correlations # get correlations
colnames(nc) <- c("Self-report\nideology", "Economy", "Immigrants",
                  "Refugees", "Guns", "Police", 
                  "Party ID")
rownames(nc) <- c("Self-report\nideology", "Economy", "Immigrants",
                  "Refugees", "Guns", "Police", 
                  "Party ID")
# reshape for heatmap style plot (only upper triangle kept for display to avoid duplicated info)
upper <- nc
upper[lower.tri(upper)] <- NA
nc <- reshape2::melt(nc)
upper <- reshape2::melt(upper)

ggplot(nc, aes(x = Var1, y = Var2, 
               fill = value,
               label = round(value, 2))) +
  geom_tile() + 
  geom_text(data = upper, aes(x = Var1, y = Var2)) +
  theme_minimal(base_size = 8) +
  scale_fill_gradient(expression(rho), low = "white", high = "grey50") +
  scale_colour_manual("", values = c("grey40", "black"), guide = FALSE) +
  scale_size_manual("",   values = c(2.5, 3.5), guide = FALSE) +
  xlab("") + ylab("")
# ggsave("si4_attitude_heatmap.pdf", width = 6, height = 6) # uncomment to save

# -----------------------------------------------------------------------------------
# Summarize Table 2 content (Main text)
# -----------------------------------------------------------------------------------
# for continuous entries (mean, SD, valid obs)
# in all cases, back to the original scale (as in survey)
us %>% 
  mutate(leftright = leftright*4 + 1,
         ref       = ref*6 + 1,
         guns      = guns*4 + 1,
         demrep    = demrep + 1) %>% 
  dplyr::select(leftright, ref, guns, demrep) %>%   
  summarize_all(funs(mean(., na.rm = TRUE), sd(., na.rm = TRUE), sum(!is.na(.))))

# for categorical entries answer proportion and valid n
prop.table(table(us$economy))
sum(!is.na(us$economy))
prop.table(table(us$immigrants))
sum(!is.na(us$immigrants))
prop.table(table(us$police))
sum(!is.na(us$police))
prop.table(table(us$pid3))
sum(!is.na(us$pid3))

# -----------------------------------------------------------------------------------
# ///
#
# PART 2: Recoding, summarizing and evaluating narcissism measure
#         (including factor models; export summaries)
# ///
# -----------------------------------------------------------------------------------

# -----------------------------------------------------------------------------------
# (npi) See detailed wording and sub-facet grouping in Supplementary Information 2 of the manuscript
# and the attached codebook
# all npi related variables are named PI1-40
npi_vars <- paste0("PI", 1:40) # original coding: 1 and 2 (answer choice 1 or answer choice 2)
us[, npi_vars] <- us[, npi_vars] - 1 # recode to 0 and 1

# based on codebook (see SI 2, again), select those where "narcissistic" answer was first option
# not second (highest), recode => for all items, 1 = more narcissistic answer, 0 = less narc

# in parallel with coding, checking inter-item correlations and reliability
# lists reversals
psych::alpha(us[, npi_vars], check.keys = TRUE)

to_reverse <- c(1:3, 6, 8, 11:14, 16, 21, 24, 25, 27, 29:31, 33,
                34, 36:39) # items to be reversed
us[, npi_vars[to_reverse]] <- 1 - us[, npi_vars[to_reverse]]

psych::alpha(us[, npi_vars], check.keys = TRUE) # now all +, 0.86 alpha (reported in paper)

# when to generate a full npi score? 
# at least 12 out of 40 items have a valid answer/individual
# first, count the number of valid responses/individual in the 40 set
us$narc_valid_count <- apply(us[, npi_vars], 1,
  function (x) sum(!is.na(x)))
us$narc_valid_count[us$narc_valid_count == 0] <- 0.001 # if completely no answer on the battery, adjust so that rescaling (division) works
us$npi <- rowSums(us[, npi_vars], na.rm = TRUE)/us$narc_valid_count # sum and divide by valid count
us$npi[us$narc_valid_count < 12] <- NA # 12 answers needed
# table(us$npi, useNA = "always") # 4 missing on NPI, rule of 12/40 answered

# reliability and means (reported in paper) - SI2 Table 4: Descriptive statistics for Narcissism variables, after recoding
# mean(us$npi, na.rm = TRUE)
# sd(us$npi, na.rm = TRUE)

# following codebook, get sub-facet variables
aut_vars <- npi_vars[c(1, 8, 10, 11, 12, 32, 33, 36)] # Authority
exh_vars <- npi_vars[c(2, 3, 7, 20, 28, 30, 38)] # Exhibitionism
sup_vars <- npi_vars[c(4, 9, 26, 37, 40)] # Superiority
exp_vars <- npi_vars[c(6, 13, 16, 23, 35)] # Exploitativeness
ent_vars <- npi_vars[c(5, 14, 18, 24, 25, 27)] # Entitlement
van_vars <- npi_vars[c(15, 19, 29)] # Vanity
suf_vars <- npi_vars[c(17, 21, 22, 31, 34, 39)] # Self-sufficiency

# reliability (reported in paper) - SI2 Table 4: Descriptive statistics for Narcissism variables, after recoding
psych::alpha(us[, aut_vars]) ## 0.77 reliability
psych::alpha(us[, ent_vars]) ## 0.46 reliability
psych::alpha(us[, exp_vars]) ## 0.54 reliability
psych::alpha(us[, sup_vars]) ## 0.55 reliability
psych::alpha(us[, suf_vars]) ## 0.41 reliability
psych::alpha(us[, exh_vars]) ## 0.65 reliability
psych::alpha(us[, van_vars]) ## 0.61  reliability

# calculating values - using the number of "necessary" answers for summed score for facet
# as reported/listed in SI 4
# code as before: count valid entries and vary the necessary threshold depending on number of items
# example: 4 for authority, but less for those sub-facets that have fewer items
us$facet_valid_count <- apply(us[, aut_vars], 1,
                              function (x) sum(!is.na(x)))
us$facet_valid_count[us$facet_valid_count == 0] <- 0.001
us$authority <- rowSums(us[, aut_vars], na.rm = TRUE)/us$facet_valid_count
us$authority[us$facet_valid_count < 4] <- NA

us$facet_valid_count <- apply(us[, exh_vars], 1,
                              function (x) sum(!is.na(x)))
us$facet_valid_count[us$facet_valid_count == 0] <- 0.001
us$exhib <- rowSums(us[, exh_vars], na.rm = TRUE)/us$facet_valid_count
us$exhib[us$facet_valid_count < 4] <- NA

us$facet_valid_count <- apply(us[, sup_vars], 1,
                              function (x) sum(!is.na(x)))
table(us$facet_valid_count)
us$facet_valid_count[us$facet_valid_count == 0] <- 0.001
us$superior <- rowSums(us[, sup_vars], na.rm = TRUE)/us$facet_valid_count
us$superior[us$facet_valid_count < 3] <- NA

us$facet_valid_count <- apply(us[, exp_vars], 1,
                              function (x) sum(!is.na(x)))
table(us$facet_valid_count)
us$facet_valid_count[us$facet_valid_count == 0] <- 0.001
us$exploit <- rowSums(us[, exp_vars], na.rm = TRUE)/us$facet_valid_count
us$exploit[us$facet_valid_count < 3] <- NA

us$facet_valid_count <- apply(us[, ent_vars], 1,
                              function (x) sum(!is.na(x)))
table(us$facet_valid_count)
us$facet_valid_count[us$facet_valid_count == 0] <- 0.001
us$entitle <- rowSums(us[, ent_vars], na.rm = TRUE)/us$facet_valid_count
us$entitle[us$facet_valid_count < 4] <- NA

us$facet_valid_count <- apply(us[, van_vars], 1,
                              function (x) sum(!is.na(x)))
table(us$facet_valid_count)
us$facet_valid_count[us$facet_valid_count == 0] <- 0.001
us$vanity <- rowSums(us[, van_vars], na.rm = TRUE)/us$facet_valid_count
us$vanity[us$facet_valid_count < 2] <- NA

us$facet_valid_count <- apply(us[, suf_vars], 1,
                              function (x) sum(!is.na(x)))
table(us$facet_valid_count)
us$facet_valid_count[us$facet_valid_count == 0] <- 0.001
us$sufficient <- rowSums(us[, suf_vars], na.rm = TRUE)/us$facet_valid_count
us$sufficient[us$facet_valid_count < 4] <- NA
us$facet_valid_count <- NULL
us$narc_valid_count <- NULL

narc_vars <- c("npi", "authority", "exhib", "superior", "exploit", "entitle", "vanity", "sufficient") # store out narcissism variable names

# means/SD (reported in paper) - SI2 Table 4: Descriptive statistics for Narcissism variables, after recoding
apply(us[, narc_vars], 2, function(x) paste0(round(mean(x, na.rm = TRUE), 3),
                                             " (", round(sd(x, na.rm = TRUE), 3), ")"))

# -----------------------------------------------------------------------------------
# npi factor models
# -----------------------------------------------------------------------------------

# 7-factor Confirmatory Factor Model following the original 7-factor model
npi.factor <-  'auth =~ PI1 + PI8 + PI10 + PI11 + PI12 + PI32 + PI33
                        + PI36
               exhib2 =~ PI2 + PI3 + PI7 + PI20 + PI28 + PI30 + PI38
                 sup =~ PI4 + PI9 + PI26 + PI37 + PI40
                 exp =~ PI6 + PI13 + PI16 + PI23 + PI35
                 ent =~ PI5 + PI14 + PI18 + PI24 + PI25 + PI27
                 van =~ PI15 + PI19 + PI29
                 suf =~ PI17 + PI21 + PI22 + PI31 + PI34 + PI39'
npi.factor.fit <- cfa(npi.factor, data = us, ordered = npi_vars) # note: categorical manifest variables
summary(npi.factor.fit, fit.measures = TRUE, standardized = TRUE)

# -----------------------------------------------------------------------------------
# SI3 Table 3: Fit statistics
# source: summary call above
# -----------------------------------------------------------------------------------
## For DWLS estimation: CFI = 0.958; TFI = 0.954; RMSEA = 0.037 (0.034, 0.040, 90% CI)
## For Robust estimation: CFI = 0.913; TFI = 0.906; RMSEA = 0.034 (0.031, 0.038, 90% CI)


# -----------------------------------------------------------------------------------
# SI3 Table 1: 7-factor solution, factor loadings
# note: format edited in document (not content)
#       for presentation in SI
# -----------------------------------------------------------------------------------

fit_40 <- data.frame(parameterEstimates(npi.factor.fit))[1:40, c(1, 3:5)] %>% 
  mutate(est = round(est, 3),
         se  = round(est, 3),
         se  = ifelse(est == 0.000 & est != 1, 0.001, se), # because of rounding, we still want to display 0.001 se even if 0.000
         rhs = str_replace(rhs, "PI", "Item"),
         lhs = str_replace(lhs, "auth", "Authority"),
         lhs = str_replace(lhs, "ent", "Entitlement"),
         lhs = str_replace(lhs, "exhib", "Exhibitionism"),
         lhs = str_replace(lhs, "exp", "Exploitativeness"),
         lhs = str_replace(lhs, "suf", "Self-sufficiency"),
         lhs = str_replace(lhs, "sup", "Superiority"),
         lhs = str_replace(lhs, "van", "Vanity"),
         label = paste0(est, " (", se, ")"),
         label = ifelse(est == 1.000, "Fixed to 1", label)) %>% 
         group_by(lhs) %>% 
         arrange(lhs, desc(est)) %>% 
         dplyr::select(lhs, rhs, label)

# write.csv(fit_40, "npi40_loadings.csv",
#           row.names = FALSE) # uncomment to export

# -----------------------------------------------------------------------------------
# SI3 Table 2: 7-factor solution, factor covariances
# note: format edited in document (not content)
# -----------------------------------------------------------------------------------
fit_40_cov <- data.frame(parameterEstimates(npi.factor.fit))[121:148, c(1, 3:5)] %>% 
  mutate(est = round(est, 3),
       se  = round(est, 3),
       se  = ifelse(est == 0.000 & est != 1, 0.001, se),
       rhs = str_replace(rhs, "PI", "Item"),
       lhs = str_replace(lhs, "auth", "Authority"),
       lhs = str_replace(lhs, "ent", "Entitlement"),
       lhs = str_replace(lhs, "exhib", "Exhibitionism"),
       lhs = str_replace(lhs, "exp", "Exploitativeness"),
       lhs = str_replace(lhs, "suf", "Self-sufficiency"),
       lhs = str_replace(lhs, "sup", "Superiority"),
       lhs = str_replace(lhs, "van", "Vanity"),
       rhs = str_replace(rhs, "auth", "Authority"),
       rhs = str_replace(rhs, "ent", "Entitlement"),
       rhs = str_replace(rhs, "exhib", "Exhibitionism"),
       rhs = str_replace(rhs, "exp", "Exploitativeness"),
       rhs = str_replace(rhs, "suf", "Self-sufficiency"),
       rhs = str_replace(rhs, "sup", "Superiority"),
       rhs = str_replace(rhs, "van", "Vanity"),
       label = paste0(est, " (", se, ")")) %>%
  dplyr::select(lhs, rhs, label) %>%
  reshape(idvar='rhs', timevar='lhs', direction='wide')
names(fit_40_cov) <- stringr::str_replace(names(fit_40_cov), "label.", "")
fit_40_cov[is.na(fit_40_cov)] <- ""
# write.csv(fit_40_cov, "npi40_covariances.csv",
#           row.names = FALSE) # uncomment to export
rm(fit_40, fit_40_cov)

# -----------------------------------------------------------------------------------
# Alternative NPI factor model
# 3-factor specification, SI9 material
# -----------------------------------------------------------------------------------

# Based on: Ackerman, Robert A, Edward A Witt, M Brent Donnellan, Kali H Trzesniewski, Richard W Robins, and Deborah A Kashy.  2011.  
#           "What does the Narcissistic Personality Inventory really measure?".  Assessment 1: 67-87.
npi.3factor <-  'lead_auth =~ PI1 + PI5 + PI10 + PI11 + PI12 + PI27 +
                              PI32 + PI33 + PI34 + PI36 + PI40
                 grand_exh =~ PI4 + PI7 + PI15 + PI19 + PI20 + PI26 + PI28 +
                              PI29 + PI30 + PI38
                 ent_exp   =~ PI13 + PI14 + PI24 + PI25'
npi.3factor.fit <- cfa(npi.3factor, data = us, ordered = npi_vars)
summary(npi.3factor.fit, fit.measures = TRUE, standardized = TRUE)
# -----------------------------------------------------------------------------------
# SI 9 Table 2: 3-factor solution, covariances and fit statistics (second part)
# -----------------------------------------------------------------------------------
## For DWLS estimation: CFI = 0.960; TFI = 0.956; RMSEA = 0.044 (0.039, 0.049, 90% CI)
## For Robust estimation: CFI = 0.924; TFI = 0.916; RMSEA = 0.044 (0.040, 0.049, 90% CI)

# -----------------------------------------------------------------------------------
# SI 9 Table 1: 3-factor solution, factor loadings
# -----------------------------------------------------------------------------------
fit_25 <- data.frame(parameterEstimates(npi.3factor.fit))[1:25, c(1, 3:5)]
fit_25[, 3:4] <- round(fit_25[, 3:4], 3)
fit_25$se[fit_25$se == 0.000 & fit_25$est != 1] <- 0.001
fit_25$rhs <- stringr::str_replace(fit_25$rhs, "PI", "Item")

fit_25$lhs <- stringr::str_replace(fit_25$lhs, "lead_auth", "Leadership/Authority")
fit_25$lhs <- stringr::str_replace(fit_25$lhs, "ent_exp", "Entitlement/Exploitativeness")
fit_25$lhs <- stringr::str_replace(fit_25$lhs, "grand_exh", "Grandiose/Exhibitionism")

fit_25 <- mutate(fit_25, label = paste0(est, " (", se, ")"))
fit_25$label[fit_25$est == 1.000] <- "Fixed to 1"

fit_25 <- fit_25 %>% group_by(lhs) %>% 
  arrange(lhs, desc(est)) %>% dplyr::select(lhs, rhs, label)

# write.csv(fit_25, "npi3fact_loadings.csv",
#           row.names = FALSE) # uncomment to export

# -----------------------------------------------------------------------------------
# SI 9 Table 2: 3-factor solution, covariances and fit statistics (first part)
# -----------------------------------------------------------------------------------
fit_25_cov <- data.frame(parameterEstimates(npi.3factor.fit))[76:81, c(1, 3:5)]
fit_25_cov[, 3:4] <- round(fit_25_cov[, 3:4], 3)

fit_25_cov$lhs <- stringr::str_replace(fit_25_cov$lhs, "lead_auth", "Leadership/Authority")
fit_25_cov$lhs <- stringr::str_replace(fit_25_cov$lhs, "ent_exp", "Entitlement/Exploitativeness")
fit_25_cov$lhs <- stringr::str_replace(fit_25_cov$lhs, "grand_exh", "Grandiose/Exhibitionism")

fit_25_cov$rhs <- stringr::str_replace(fit_25_cov$rhs, "lead_auth", "Leadership/Authority")
fit_25_cov$rhs <- stringr::str_replace(fit_25_cov$rhs, "ent_exp", "Entitlement/Exploitativeness")
fit_25_cov$rhs <- stringr::str_replace(fit_25_cov$rhs, "grand_exh", "Grandiose/Exhibitionism")

fit_25_cov <- mutate(fit_25_cov, label = paste0(est, " (", se, ")"))
fit_25_cov <- fit_25_cov[, c(1, 2, 5)]
fit_25_cov <- reshape(fit_25_cov, idvar='rhs', timevar='lhs', direction='wide')
names(fit_25_cov) <- stringr::str_replace(names(fit_25_cov), "label.", "")
fit_25_cov[is.na(fit_25_cov)] <- ""

# write.csv(fit_25_cov, "npi3fact_covariances.csv",
#           row.names = FALSE) # uncomment to export

rm(fit_25, fit_25_cov)

# -----------------------------------------------------------------------------------
# SI 9 Table 3: Standardized loadings
# Entitlement and Exploitativeness standardized item loading (7 factor model)
#   together with Entitle/Exploit standardized item loading (3 factor model)
# -----------------------------------------------------------------------------------
summary(npi.3factor.fit, standardized = TRUE)
summary(npi.factor.fit, standardized = TRUE)

fit_ent7 <- data.frame(standardizedSolution(npi.factor.fit))[26:31, c(1, 3, 4)]
fit_exp7 <- data.frame(standardizedSolution(npi.factor.fit))[21:25, c(1, 3, 4)]
fit7 <- rbind(fit_ent7, fit_exp7) 
# reorganize content
fit7$est.std <- round(fit7$est.std, 3)
fit7$exploit <- fit7$est.std
names(fit7)[3] <- "entitle"
fit7$entitle[fit7$lhs == "exp"] <- ""
fit7$exploit[fit7$lhs == "ent"] <- ""

fit_entexp3 <- data.frame(standardizedSolution(npi.3factor.fit))[22:25, c(1, 3, 4)]
fit_entexp3$entexp <- round(fit_entexp3$est.std, 3)
fit_all <- left_join(fit7, fit_entexp3[, c("rhs", "entexp")], by = "rhs")
fit_all$entexp[is.na(fit_all$entexp)] <- ""
fit_all$rhs <- gsub("PI", "Item", fit_all$rhs)

# write.csv(fit_all[, 2:5], "si9_table3.csv",
#           row.names = FALSE) # uncomment to export

rm(fit_ent7, fit_exp7, fit7, fit_entexp3, fit_all)

# add to data 3-factor sums
lead_auth   <- c("PI1", "PI5", "PI10", "PI11", "PI12", "PI27", "PI32", "PI33", "PI34", "PI36", "PI40")
grand_exhib <- c("PI4", "PI7", "PI15", "PI19", "PI20", "PI26", "PI28", "PI29", "PI30", "PI38")
ent_exp     <- c("PI13", "PI14", "PI24", "PI25")

# follow same principles of "valid answer count"
us$facet_valid_count <- apply(us[, lead_auth], 1,
                              function (x) sum(!is.na(x)))
us$facet_valid_count[us$facet_valid_count == 0] <- 0.001
us$leadauth <- rowSums(us[, lead_auth], na.rm = TRUE)/us$facet_valid_count
us$leadauth[us$facet_valid_count < 4] <- NA

us$facet_valid_count <- apply(us[, grand_exhib], 1,
                              function (x) sum(!is.na(x)))
us$facet_valid_count[us$facet_valid_count == 0] <- 0.001
us$grdexhib <- rowSums(us[, grand_exhib], na.rm = TRUE)/us$facet_valid_count
us$grdexhib[us$facet_valid_count < 4] <- NA

us$facet_valid_count <- apply(us[, ent_exp], 1,
                              function (x) sum(!is.na(x)))
us$facet_valid_count[us$facet_valid_count == 0] <- 0.001
us$entexp <- rowSums(us[, ent_exp], na.rm = TRUE)/us$facet_valid_count
us$entexp[us$facet_valid_count < 2] <- NA

us$facet_valid_count <- NULL

# -----------------------------------------------------------------------------------
# SI2 Figure 1: Pearson’s correlation coefficients between Narcissism variables 
# -----------------------------------------------------------------------------------
narc_cor <- us[, c("unique_id", narc_vars)]
nc <- polycor::hetcor(narc_cor[, narc_vars])$correlations
colnames(nc) <- c("Full NPI", "Authority", "Exhibitionism",
                  "Superiority", "Exploitativeness", "Entitlement", 
                  "Vanity", "Self-sufficiency")
rownames(nc) <- c("Full NPI", "Authority", "Exhibitionism",
                  "Superiority", "Exploitativeness", "Entitlement", 
                  "Vanity", "Self-sufficiency")
upper <- nc
upper[lower.tri(upper)] <- NA
nc <- reshape2::melt(nc)
upper <- reshape2::melt(upper)

ggplot(nc, aes(x = Var1, y = Var2, 
               fill = value,
               label = round(value, 2))) +
  geom_tile() + 
  geom_text(data = upper, aes(x = Var1, y = Var2)) +
  theme_minimal(base_size = 8) +
  scale_fill_gradient(expression(rho), low = "white", high = "grey50") +
  scale_colour_manual("", values = c("grey40", "black"), guide = FALSE) +
  scale_size_manual("",   values = c(2.5, 3.5), guide = FALSE) +
  xlab("") + ylab("")
# ggsave("si2_narc_heatmap.pdf", width = 6, height = 6) # uncomment to save

# -----------------------------------------------------------------------------------
# Supplementary Information 5: Narcissism and Ideology correlations
# -----------------------------------------------------------------------------------
for_cor <- us[, c("unique_id", "leftright", "pid3", ideo_vars, narc_vars)]
for_cor <- reshape2::melt(for_cor, id = c("unique_id", "leftright", "pid3", ideo_vars))
names(for_cor)[9:10] <- c("narc_id", "narc_score") 
for_id <- names(for_cor)[c(1, 9:10)]
for_cor <- reshape2::melt(for_cor, id = for_id)
names(for_cor)[4:5] <- c("ideo_id", "ideo_score")
for_test <- for_cor

cor_sum <- for_cor %>% 
  group_by(ideo_id, narc_id) %>%
  summarise(rho  = round(cor.test(ideo_score, 
                                  narc_score)$estimate, 2),
            pval = round(cor.test(ideo_score, 
                                  narc_score)$p.value, 2) 
  )

cor_sum$pval_s <- as.character(cor_sum$pval)
cor_sum$pval_s[cor_sum$pval == 0] <- "p < 0.001"
cor_sum$label <- paste0(cor_sum$rho, "\n(", cor_sum$pval_s, ")")
cor_sum$is_sig <- ifelse(cor_sum$pval < 0.049, 1, 0)

cor_sum$ideo_lab <- "Self-report\nideology"
cor_sum$ideo_lab[cor_sum$ideo_id == "economy"] <- "Economy"
cor_sum$ideo_lab[cor_sum$ideo_id == "immigrants"] <- "Immigrants"
cor_sum$ideo_lab[cor_sum$ideo_id == "ref"] <- "Refugees"
cor_sum$ideo_lab[cor_sum$ideo_id == "guns"] <- "Guns"
cor_sum$ideo_lab[cor_sum$ideo_id == "police"] <- "Police"
cor_sum$ideo_lab[cor_sum$ideo_id == "pid3"] <- "Party ID"

cor_sum$narc_lab <- "Full NPI"
cor_sum$narc_lab[cor_sum$narc_id == "authority"] <- "Authority"
cor_sum$narc_lab[cor_sum$narc_id == "entitle"] <- "Entitlement"
cor_sum$narc_lab[cor_sum$narc_id == "exploit"] <- "Exploitativeness"
cor_sum$narc_lab[cor_sum$narc_id == "superior"] <- "Superiority"
cor_sum$narc_lab[cor_sum$narc_id == "sufficient"] <- "Self-sufficiency"
cor_sum$narc_lab[cor_sum$narc_id == "exhib"] <- "Exhibitionism"
cor_sum$narc_lab[cor_sum$narc_id == "vanity"] <- "Vanity"


cor_sum$ideo_lab <- factor(cor_sum$ideo_lab, levels = c("Self-report\nideology",
                                                        "Economy",
                                                        "Immigrants",
                                                        "Refugees",
                                                        "Guns",
                                                        "Police",
                                                        "Party ID"))

cor_sum$narc_lab <- factor(cor_sum$narc_lab, levels = c("Full NPI",
                                                        "Authority",
                                                        "Entitlement",
                                                        "Exploitativeness",
                                                        "Superiority",
                                                        "Self-sufficiency",
                                                        "Exhibitionism",
                                                        "Vanity"))

ggplot(cor_sum, aes(x = ideo_lab, y = narc_lab, 
                    fill = rho,
                    label = label)) +
  geom_tile() + geom_text(aes(colour = factor(is_sig),
                              size = factor(is_sig))) +
  theme_minimal() +
  scale_fill_gradient(expression(rho), low = "white", high = "grey50") +
  scale_colour_manual("", values = c("grey40", "black"), guide = FALSE) +
  scale_size_manual("",   values = c(2.5, 3.5), guide = FALSE) +
  xlab("") + ylab("")
# ggsave("si5_narcattitude_heatmap.pdf", width = 8, height = 8) # uncomment to save

# -----------------------------------------------------------------------------------
# Supplementary Information 8: Self-report Ideology extremity checks
# SI 8 Figure 1: Mean (with 95% confidence intervals) values of Narcissism and 
#                sub-facets for each response category on Self-report Ideology.
# -----------------------------------------------------------------------------------
mean_diff <- for_test %>%
  filter(ideo_id == "leftright") %>%
  filter(!is.na(ideo_score)) %>%
  group_by(narc_id, ideo_score) %>%
  summarise(mu_cat = mean(narc_score, na.rm = TRUE),
            se_cat = se_mean(narc_score))

mean_diff$narc_lab <- "Full NPI"
mean_diff$narc_lab[mean_diff$narc_id == "authority"] <- "Authority"
mean_diff$narc_lab[mean_diff$narc_id == "entitle"] <- "Entitlement"
mean_diff$narc_lab[mean_diff$narc_id == "exploit"] <- "Exploitativeness"
mean_diff$narc_lab[mean_diff$narc_id == "superior"] <- "Superiority"
mean_diff$narc_lab[mean_diff$narc_id == "sufficient"] <- "Self-sufficiency"
mean_diff$narc_lab[mean_diff$narc_id == "exhib"] <- "Exhibitionism"
mean_diff$narc_lab[mean_diff$narc_id == "vanity"] <- "Vanity"

mean_diff$ideo_lab <- "Moderate"
mean_diff$ideo_lab[mean_diff$ideo_score == 0] <- "Very\nliberal"
mean_diff$ideo_lab[mean_diff$ideo_score == 0.25] <- "Lib"
mean_diff$ideo_lab[mean_diff$ideo_score == 0.75] <- "Cons"
mean_diff$ideo_lab[mean_diff$ideo_score == 1] <- "Very\nconservative"

mean_diff$ideo_lab <- factor(mean_diff$ideo_lab, levels = c("Very\nliberal",
                                                            "Lib",
                                                            "Moderate",
                                                            "Cons",
                                                            "Very\nconservative"))

mean_diff$narc_lab <- factor(mean_diff$narc_lab, levels = c("Full NPI",
                                                            "Authority",
                                                            "Entitlement",
                                                            "Exploitativeness",
                                                            "Superiority",
                                                            "Self-sufficiency",
                                                            "Exhibitionism",
                                                            "Vanity"))

ggplot(mean_diff,
       aes(x = ideo_lab, y = mu_cat, 
           ymin = mu_cat - 1.96*se_cat, 
           ymax = mu_cat + 1.96*se_cat)) +
  geom_pointrange() + 
  facet_wrap(~narc_lab, ncol = 4) +
  theme_minimal() + xlab("") + ylab("") +
  theme(plot.margin       = unit(c(1, 1, 1, 1), "cm")) +
  theme(panel.spacing      = unit(1, "cm"), panel.grid = element_blank())
# ggsave("si8_ideosplit_narc.pdf", width = 12, height = 6) # uncomment to save

# -----------------------------------------------------------------------------------
# SI 8 Figure 2: Mean (with 95% confidence intervals) values of Narcissism and sub-facets for “Extreme” (either 1 or 5 on Self-report Ideology [“very”]) 
# respondents compared to only leaning ideologues (grey) and leaning ideologues + moderates (black).
# -----------------------------------------------------------------------------------
for_test$ideo_fold_all <- ifelse(for_test$ideo_score == 0 | for_test$ideo_score == 1, 1, 0)
for_test$ideo_fold_ideo <- ifelse(for_test$ideo_score == 0 | for_test$ideo_score == 1, 1, 0)
for_test$ideo_fold_ideo[for_test$ideo_score == 0.5] <- NA # no Idenpendents included

mean_diff_1 <- for_test %>%
  filter(ideo_id == "leftright") %>%
  group_by(narc_id, ideo_fold_all) %>%
  summarise(mu_cat = mean(narc_score, na.rm = TRUE),
            se_cat = se_mean(narc_score))
mean_diff_1 <- na.omit(mean_diff_1)
head(mean_diff_1)
mean_diff_2 <- for_test %>%
  filter(ideo_id == "leftright") %>%
  group_by(narc_id, ideo_fold_ideo) %>%
  summarise(mu_cat = mean(narc_score, na.rm = TRUE),
            se_cat = se_mean(narc_score))
mean_diff_2 <- na.omit(mean_diff_2)
mean_diff_2 <- dplyr::filter(mean_diff_2, ideo_fold_ideo == 0)

mean_diff_1$group <- "with Moderates"
mean_diff_2$group <- "without Moderates"
names(mean_diff_1)[2] <- "ideo_fold_ideo"
mean_diff <- bind_rows(mean_diff_1, mean_diff_2)

mean_diff$narc_lab <- "Full NPI"
mean_diff$narc_lab[mean_diff$narc_id == "authority"] <- "Authority"
mean_diff$narc_lab[mean_diff$narc_id == "entitle"] <- "Entitlement"
mean_diff$narc_lab[mean_diff$narc_id == "exploit"] <- "Exploitativeness"
mean_diff$narc_lab[mean_diff$narc_id == "superior"] <- "Superiority"
mean_diff$narc_lab[mean_diff$narc_id == "sufficient"] <- "Self-sufficiency"
mean_diff$narc_lab[mean_diff$narc_id == "exhib"] <- "Exhibitionism"
mean_diff$narc_lab[mean_diff$narc_id == "vanity"] <- "Vanity"

mean_diff$ideo_lab <- ifelse(mean_diff$ideo_fold_ideo == 0, "Moderate", "Extreme")

mean_diff$ideo_lab <- factor(mean_diff$ideo_lab, levels = c("Moderate", "Extreme"))

mean_diff$narc_lab <- factor(mean_diff$narc_lab, levels = c("Full NPI",
                                                            "Authority",
                                                            "Entitlement",
                                                            "Exploitativeness",
                                                            "Superiority",
                                                            "Self-sufficiency",
                                                            "Exhibitionism",
                                                            "Vanity"))

ggplot(mean_diff,
       aes(x = ideo_lab, y = mu_cat, 
           ymin = mu_cat - 1.96*se_cat, 
           ymax = mu_cat + 1.96*se_cat, 
           colour = group, group = group)) +
  geom_pointrange(position = position_dodge(width = 0.5)) + 
  facet_wrap(~narc_lab, ncol = 4) +
  scale_colour_manual("", values = c("black", "grey50")) +
  theme_minimal() + xlab("") + ylab("") +
  theme(plot.margin       = unit(c(1, 1, 1, 1), "cm")) +
  theme(panel.spacing      = unit(1, "cm"), panel.grid = element_blank())
# ggsave("si8_extremeplit_narc.pdf", width = 12, height = 6) # uncomment to save

# -----------------------------------------------------------------------------------
# ///
#
# PART 3: Multivariate analyses
#         (fitting models, exporting summary figures [main text] and 
#         SI regression table results)
#         Includes SI materials on 3-facet NPI results (part 3.4)
# ///
# -----------------------------------------------------------------------------------

# -----------------------------------------------------------------------------------
# centering and standardizing (continuous) variables
narc_3comp <- c("leadauth", "grdexhib", "entexp") # 3 factors
pol_vars  <- c("income", "age", "relig")
to_recode <- c(narc_vars, narc_3comp, pol_vars)
rec_vars  <- paste0(to_recode, "_sd") ## new variable names with two_sd standardization

us[, rec_vars] <- apply(us[, to_recode], 2, function (x) two_sd(x))
# head(us)

# -----------------------------------------------------------------------------------
# Model fitting and extract relevant summary
#    Separate regression models for each outcom
#    mdem = demographic controls (first part of analysis)
#    mall = all controls (first part of analysis)
#    _second_ = marks predictor of interest (full npi or subfacet model)
#    _third_  = marks outcome
# -----------------------------------------------------------------------------------

# -----------------------------------------------------------------------------------
# 3.1 Full NPI models (as predictor)
# -----------------------------------------------------------------------------------

# liberal-conservative as outcome
mdem_npi_lr <- lm(leftright ~ npi_sd +
                    female + age_sd + edu_cat + not_caucasian + income_sd,
                  data = us)
summary(mdem_npi_lr)
coef_res <- cbind("Self-report\nideology", "Full NPI", "Socio-dem",
                  coef(mdem_npi_lr)[2], 
                  confint(mdem_npi_lr, level = 1 - (0.05/1))[2,1],
                  confint(mdem_npi_lr, level = 1 - (0.05/1))[2,2])
mall_npi_lr <- lm(leftright ~ npi_sd +
  female + age_sd + edu_cat + not_caucasian + income_sd +
  relig_sd + factor(pid3),
  data = us)
coef_res <- rbind(coef_res, cbind("Self-report\nideology", "Full NPI", "All",
                  coef(mall_npi_lr)[2], 
                  confint(mall_npi_lr, level = 1 - (0.05/1))[2,1],
                  confint(mall_npi_lr, level = 1 - (0.05/1))[2,2]))

# economy as outcome
mdem_npi_ec <- polr(factor(economy) ~ npi_sd +
  female + age_sd + edu_cat + not_caucasian + income_sd,
  data = us, method = "logistic")
coef_res <- rbind(coef_res, cbind("Economy", "Full NPI", "Socio-dem",
                  coef(mdem_npi_ec)[1], 
                  confint(mdem_npi_ec, level = 1 - (0.05/1))[1,1],
                  confint(mdem_npi_ec, level = 1 - (0.05/1))[1,2]))
mall_npi_ec <- polr(factor(economy) ~ npi_sd +
  female + age_sd + edu_cat + not_caucasian + income_sd +
  relig_sd + factor(pid3),
  data = us, method = "logistic")
coef_res <- rbind(coef_res, cbind("Economy", "Full NPI", "All",
                  coef(mall_npi_ec)[1], 
                  confint(mall_npi_ec, level = 1 - (0.05/1))[1,1],
                  confint(mall_npi_ec, level = 1 - (0.05/1))[1,2]))

# immigration as outcome
mdem_npi_imm <- polr(factor(immigrants) ~ npi_sd +
  female + age_sd + edu_cat + not_caucasian + income_sd,
  data = us, method = "logistic")
coef_res <- rbind(coef_res, cbind("Immigrants", "Full NPI", "Socio-dem",
                  coef(mdem_npi_imm)[1], 
                  confint(mdem_npi_imm, level = 1 - (0.05/1))[1,1],
                  confint(mdem_npi_imm, level = 1 - (0.05/1))[1,2]))
mall_npi_imm <- polr(factor(immigrants) ~ npi_sd +
  female + age_sd + edu_cat + not_caucasian + income_sd +
  relig_sd + factor(pid3),
  data = us, method = "logistic")
coef_res <- rbind(coef_res, cbind("Immigrants", "Full NPI", "All",
                  coef(mall_npi_imm)[1], 
                  confint(mall_npi_imm, level = 1 - (0.05/1))[1,1],
                  confint(mall_npi_imm, level = 1 - (0.05/1))[1,2]))

# refugees as outcome
mdem_npi_ref <- lm(ref ~ npi_sd +
  female + age_sd + edu_cat + not_caucasian + income_sd,
  data = us)
coef_res <- rbind(coef_res, cbind("Refugees", "Full NPI", "Socio-dem",
                  coef(mdem_npi_ref)[2], 
                  confint(mdem_npi_ref, level = 1 - (0.05/1))[2,1],
                  confint(mdem_npi_ref, level = 1 - (0.05/1))[2,2]))
mall_npi_ref <- lm(ref ~ npi_sd +
  female + age_sd + edu_cat + not_caucasian + income_sd +
  relig_sd + factor(pid3),
  data = us)
coef_res <- rbind(coef_res, cbind("Refugees", "Full NPI", "All",
                  coef(mall_npi_ref)[2], 
                  confint(mall_npi_ref, level = 1 - (0.05/1))[2,1],
                  confint(mall_npi_ref, level = 1 - (0.05/1))[2,2]))
mdem_npi_guns <- lm(guns ~ npi_sd +
  female + age_sd + edu_cat + not_caucasian + income_sd,
  data = us)
coef_res <- rbind(coef_res, cbind("Guns", "Full NPI", "Socio-dem",
                  coef(mdem_npi_guns)[2], 
                  confint(mdem_npi_guns, level = 1 - (0.05/1))[2,1],
                  confint(mdem_npi_guns, level = 1 - (0.05/1))[2,2]))

# gun ownership as outcome
mall_npi_guns <- lm(guns ~ npi_sd +
  female + age_sd + edu_cat + not_caucasian + income_sd +
  relig_sd + factor(pid3),
  data = us)
coef_res <- rbind(coef_res, cbind("Guns", "Full NPI", "All",
                  coef(mall_npi_guns)[2], 
                  confint(mall_npi_guns, level = 1 - (0.05/1))[2,1],
                  confint(mall_npi_guns, level = 1 - (0.05/1))[2,2]))

# policing as outcome
mdem_npi_pol <- glm(police ~ npi_sd +
  female + age_sd + edu_cat + not_caucasian + income_sd,
  data = us, family = binomial)
coef_res <- rbind(coef_res, cbind("Police", "Full NPI", "Socio-dem",
                  coef(mdem_npi_pol)[2], 
                  confint(mdem_npi_pol, level = 1 - (0.05/1))[2,1],
                  confint(mdem_npi_pol, level = 1 - (0.05/1))[2,2]))
mall_npi_pol <- glm(police ~ npi_sd +
  female + age_sd + edu_cat + not_caucasian + income_sd +
  relig_sd + factor(pid3),
  data = us, family = binomial)
coef_res <- rbind(coef_res, cbind("Police", "Full NPI", "All",
                  coef(mall_npi_pol)[2], 
                  confint(mall_npi_pol, level = 1 - (0.05/1))[2,1],
                  confint(mall_npi_pol, level = 1 - (0.05/1))[2,2]))

# 3-category party identification as outcome
mdem_npi_pid <- polr(factor(pid3) ~ npi_sd +
  female + age_sd + edu_cat + not_caucasian + income_sd,
  data = us, method = "logistic")
coef_res <- rbind(coef_res, cbind("Party ID", "Full NPI", "Socio-dem",
                  coef(mdem_npi_pid)[1], 
                  confint(mdem_npi_pid, level = 1 - (0.05/1))[1,1],
                  confint(mdem_npi_pid, level = 1 - (0.05/1))[1,2]))
mall_npi_pid <- polr(factor(pid3) ~ npi_sd +
  female + age_sd + edu_cat + not_caucasian + income_sd +
  relig_sd + leftright,
  data = us, method = "logistic")
coef_res <- rbind(coef_res, cbind("Party ID", "Full NPI", "All",
                  coef(mall_npi_pid)[1], 
                  confint(mall_npi_pid, level = 1 - (0.05/1))[1,1],
                  confint(mall_npi_pid, level = 1 - (0.05/1))[1,2]))
# 7-category party identification as outcome (treated as continuous, scaled to range 0 to 1)
mdem_npi_pid7 <- lm(I(demrep/6) ~ npi_sd +
                       female + age_sd + edu_cat + not_caucasian + income_sd,
                    data = us)
coef_res <- rbind(coef_res, cbind("Party ID-7", "Full NPI", "Socio-dem",
                                  coef(mdem_npi_pid7)[2], 
                                  confint(mdem_npi_pid7, level = 1 - (0.05/1))[2,1],
                                  confint(mdem_npi_pid7, level = 1 - (0.05/1))[2,2]))
mall_npi_pid7 <- lm(I(demrep/6) ~ npi_sd +
                       female + age_sd + edu_cat + not_caucasian + income_sd +
                       relig_sd + leftright,
                     data = us)
coef_res <- rbind(coef_res, cbind("Party ID-7", "Full NPI", "All",
                                  coef(mall_npi_pid7)[2], 
                                  confint(mall_npi_pid7, level = 1 - (0.05/1))[2,1],
                                  confint(mall_npi_pid7, level = 1 - (0.05/1))[2,2]))

# prepare results summary dataset for plotting
coef_res <- data.frame(coef_res, stringsAsFactors = FALSE)
coef_res[, 4:6] <- apply(coef_res[, 4:6], 2, function (x) as.numeric(x))

coef_res$X1 <- factor(coef_res$X1, levels = c("Self-report\nideology",
                                                        "Economy",
                                                        "Immigrants",
                                                        "Party ID-7",
                                                        "Refugees",
                                                        "Guns",
                                                        "Police", 
                                                        "Party ID"))
# -----------------------------------------------------------------------------------
# Supplementary Information 6: Detailed model results, demographic controls
# Full Narcissism and Ideology/Party ID, demographic controls
# -----------------------------------------------------------------------------------

# htmlreg(l = list(mdem_npi_lr,
#                    mdem_npi_ec,
#                    mdem_npi_imm,
#                    mdem_npi_ref,
#                    mdem_npi_guns,
#                    mdem_npi_pol,
#                    mdem_npi_pid,
#                    mdem_npi_pid7),
#           custom.model.names = c("Self-report\nideo.",
#             "Econ\n(ordered)",
#             "Immigrants\n(ordered)",
#             "Refugees",
#             "Gun control",
#             "Police\n(logit)",
#             "Party ID\n(ordered)",
#             "7-Party ID"),
#           custom.coef.names = c("Intercept",
#                                 "Full NPI", "Gender", "Age",
#                                 "Education", "Race", "Income"),
#           caption = "Full Narcissism and Ideology/Party ID, demographic controls",
#           file = "npi_ideo_dem.html")
summary(mdem_npi_lr)
summary(mdem_npi_ec)
summary(mdem_npi_imm)
summary(mdem_npi_ref)
summary(mdem_npi_guns)
summary(mdem_npi_pol)
summary(mdem_npi_pid)
summary(mdem_npi_pid7)


# screenreg(l = list(mdem_npi_lr,
#                    mdem_npi_ec,
#                    mdem_npi_imm,
#                    mdem_npi_ref,
#                    mdem_npi_guns,
#                    mdem_npi_pol,
#                    mdem_npi_pid,
#                    mdem_npi_pid7), digits = 3,
#           custom.model.names = as.character(c(1:8)),
#           custom.coef.names = c("Intercept", 
#                                 "Full NPI", "Gender", "Age", 
#                                 "Education", "Race", "Income"),
#           caption = "Full Narcissism and Ideology/Party ID, demographic controls")


# -----------------------------------------------------------------------------------
# Supplementary Information 7: Detailed model results, all controls
# Full Narcissism and Ideology/Party ID, all controls
# -----------------------------------------------------------------------------------

# htmlreg(l = list(mall_npi_lr,
#                    mall_npi_ec,
#                    mall_npi_imm,
#                    mall_npi_ref,
#                    mall_npi_guns,
#                    mall_npi_pol,
#                    mall_npi_pid,
#                    mall_npi_pid7),
#           custom.model.names = c("Self-report\nideo.",
#             "Econ\n(ordered)",
#             "Immigrants\n(ordered)",
#             "Refugees",
#             "Gun control",
#             "Police\n(logit)",
#             "Party ID\n(ordered)",
#             "7-Party ID"),
#           custom.coef.names = c("Intercept",
#                                 "Full NPI", "Gender", "Age",
#                                 "Education", "Race", "Income",
#                                 "Religiosity", "Idependent", "Republican",
#           "Self-report ideology"),
#           caption = "Full Narcissism and Ideology/Party ID, all controls",
#           file = "npi_ideo_all.html")
summary(mall_npi_lr)
summary(mall_npi_lr)
summary(mall_npi_ec)
summary(mall_npi_imm)
summary(mall_npi_ref)
summary(mall_npi_guns)
summary(mall_npi_pol)
summary(mall_npi_pid)
summary(mall_npi_pid7)

# screenreg(l = list(mall_npi_lr,
#                    mall_npi_ec,
#                    mall_npi_imm,
#                    mall_npi_ref,
#                    mall_npi_guns,
#                    mall_npi_pol,
#                    mall_npi_pid,
#                    mall_npi_pid7),
#           digits = 3,
#           custom.model.names = as.character(c(1:8)),
#           custom.coef.names = c("Intercept", 
#                                 "Full NPI", "Gender", "Age", 
#                                 "Education", "Race", "Income", 
#                                 "Religiosity", "Idependent", "Republican",
#           "Self-report ideology"),
#           caption = "Full Narcissism and Ideology/Party ID, all controls")

coef_npi <- coef_res # save out narcissism coefs from full NPI, for later
# -----------------------------------------------------------------------------------
# 3.2 Sub-facet models (as predictor, 7 facets)
# -----------------------------------------------------------------------------------

# liberal-conservative as outcome
mdem_facets_lr <- lm(leftright ~ authority_sd + exploit_sd + entitle_sd + 
  superior_sd + sufficient_sd + exhib_sd + vanity_sd  +
  female + age_sd + edu_cat + not_caucasian + income_sd,
  data = us)
coef_res <- cbind("Self-report\nideology", "Socio-dem",
                  coef(mdem_facets_lr)[2:8], 
                  confint(mdem_facets_lr, level = 1 - (0.05/1))[2:8,1],
                  confint(mdem_facets_lr, level = 1 - (0.05/1))[2:8,2])
mall_facets_lr <- lm(leftright ~ authority_sd + exploit_sd + entitle_sd + 
  superior_sd + sufficient_sd + exhib_sd + vanity_sd  +
  female + age_sd + edu_cat + not_caucasian + income_sd +
  relig_sd + factor(pid3),
  data = us)
coef_res <- rbind(coef_res, cbind("Self-report\nideology", "All",
                  coef(mall_facets_lr)[2:8], 
                  confint(mall_facets_lr, level = 1 - (0.05/1))[2:8,1],
                  confint(mall_facets_lr, level = 1 - (0.05/1))[2:8,2]))

# economy as outcome
mdem_facets_ec <- polr(factor(economy) ~ authority_sd + exploit_sd + entitle_sd + 
  superior_sd + sufficient_sd + exhib_sd + vanity_sd  +
  female + age_sd + edu_cat + not_caucasian + income_sd,
  data = us, method = "logistic")
coef_res <- rbind(coef_res, cbind("Economy", "Socio-dem",
                  coef(mdem_facets_ec)[1:7], 
                  confint(mdem_facets_ec, level = 1 - (0.05/1))[1:7,1],
                  confint(mdem_facets_ec, level = 1 - (0.05/1))[1:7,2]))
mall_facets_ec <- polr(factor(economy) ~ authority_sd + exploit_sd + entitle_sd + 
  superior_sd + sufficient_sd + exhib_sd + vanity_sd  +
  female + age_sd + edu_cat + not_caucasian + income_sd +
  relig_sd + factor(pid3),
  data = us, method = "logistic")
coef_res <- rbind(coef_res, cbind("Economy", "All",
                  coef(mall_facets_ec)[1:7], 
                  confint(mall_facets_ec, level = 1 - (0.05/1))[1:7,1],
                  confint(mall_facets_ec, level = 1 - (0.05/1))[1:7,2]))
# immigration as outcome
mdem_facets_imm <- polr(factor(immigrants) ~ authority_sd + exploit_sd + entitle_sd + 
  superior_sd + sufficient_sd + exhib_sd + vanity_sd  +
  female + age_sd + edu_cat + not_caucasian + income_sd,
  data = us, method = "logistic")
coef_res <- rbind(coef_res, cbind("Immigrants", "Socio-dem",
                  coef(mdem_facets_imm)[1:7], 
                  confint(mdem_facets_imm, level = 1 - (0.05/1))[1:7,1],
                  confint(mdem_facets_imm, level = 1 - (0.05/1))[1:7,2]))
mall_facets_imm <- polr(factor(immigrants) ~ authority_sd + exploit_sd + entitle_sd + 
  superior_sd + sufficient_sd + exhib_sd + vanity_sd  +
  female + age_sd + edu_cat + not_caucasian + income_sd +
  relig_sd + factor(pid3),
  data = us, method = "logistic")
coef_res <- rbind(coef_res, cbind("Immigrants", "All",
                  coef(mall_facets_imm)[1:7], 
                  confint(mall_facets_imm, level = 1 - (0.05/1))[1:7,1],
                  confint(mall_facets_imm, level = 1 - (0.05/1))[1:7,2]))

# refugees as outcome
mdem_facets_ref <- lm(ref ~ authority_sd + exploit_sd + entitle_sd + 
  superior_sd + sufficient_sd + exhib_sd + vanity_sd  +
  female + age_sd + edu_cat + not_caucasian + income_sd,
  data = us)

coef_res <- rbind(coef_res, cbind("Refugees", "Socio-dem",
                  coef(mdem_facets_ref)[2:8], 
                  confint(mdem_facets_ref, level = 1 - (0.05/1))[2:8,1],
                  confint(mdem_facets_ref, level = 1 - (0.05/1))[2:8,2]))
mall_facets_ref <- lm(ref ~ authority_sd + exploit_sd + entitle_sd + 
  superior_sd + sufficient_sd + exhib_sd + vanity_sd  +
  female + age_sd + edu_cat + not_caucasian + income_sd +
  relig_sd + factor(pid3),
  data = us)
coef_res <- rbind(coef_res, cbind("Refugees", "All",
                  coef(mall_facets_ref)[2:8], 
                  confint(mall_facets_ref, level = 1 - (0.05/1))[2:8,1],
                  confint(mall_facets_ref, level = 1 - (0.05/1))[2:8,2]))

# gun ownership as outcome
mdem_facets_guns <- lm(guns ~ authority_sd + exploit_sd + entitle_sd + 
  superior_sd + sufficient_sd + exhib_sd + vanity_sd  +
  female + age_sd + edu_cat + not_caucasian + income_sd,
  data = us)
coef_res <- rbind(coef_res, cbind("Guns", "Socio-dem",
                  coef(mdem_facets_guns)[2:8], 
                  confint(mdem_facets_guns, level = 1 - (0.05/1))[2:8,1],
                  confint(mdem_facets_guns, level = 1 - (0.05/1))[2:8,2]))
mall_facets_guns <- lm(guns ~ authority_sd + exploit_sd + entitle_sd + 
  superior_sd + sufficient_sd + exhib_sd + vanity_sd  +
  female + age_sd + edu_cat + not_caucasian + income_sd +
  relig_sd + factor(pid3),
  data = us)
coef_res <- rbind(coef_res, cbind("Guns", "All",
                  coef(mall_facets_guns)[2:8], 
                  confint(mall_facets_guns, level = 1 - (0.05/1))[2:8,1],
                  confint(mall_facets_guns, level = 1 - (0.05/1))[2:8,2]))

# policing as outcome
mdem_facets_pol <- glm(police ~ authority_sd + exploit_sd + entitle_sd + 
  superior_sd + sufficient_sd + exhib_sd + vanity_sd  +
  female + age_sd + edu_cat + not_caucasian + income_sd,
  data = us, family = binomial)
coef_res <- rbind(coef_res, cbind("Police", "Socio-dem",
                  coef(mdem_facets_pol)[2:8], 
                  confint(mdem_facets_pol, level = 1 - (0.05/1))[2:8,1],
                  confint(mdem_facets_pol, level = 1 - (0.05/1))[2:8,2]))
mall_facets_pol <- glm(police ~ authority_sd + exploit_sd + entitle_sd + 
  superior_sd + sufficient_sd + exhib_sd + vanity_sd  +
  female + age_sd + edu_cat + not_caucasian + income_sd +
  relig_sd + factor(pid3),
  data = us, family = binomial)
coef_res <- rbind(coef_res, cbind("Police", "All",
                  coef(mall_facets_pol)[2:8], 
                  confint(mall_facets_pol, level = 1 - (0.05/1))[2:8,1],
                  confint(mall_facets_pol, level = 1 - (0.05/1))[2:8,2]))

# 3-category party identification as outcome
mdem_facets_pid <- polr(factor(pid3) ~ authority_sd + exploit_sd + entitle_sd + 
  superior_sd + sufficient_sd + exhib_sd + vanity_sd  +
  female + age_sd + edu_cat + not_caucasian + income_sd,
  data = us, method = "logistic")
coef_res <- rbind(coef_res, cbind("Party ID", "Socio-dem",
                  coef(mdem_facets_pid)[1:7], 
                  confint(mdem_facets_pid, level = 1 - (0.05/1))[1:7,1],
                  confint(mdem_facets_pid, level = 1 - (0.05/1))[1:7,2]))
mall_facets_pid <- polr(factor(pid3) ~ authority_sd + exploit_sd + entitle_sd + 
  superior_sd + sufficient_sd + exhib_sd + vanity_sd  +
  female + age_sd + edu_cat + not_caucasian + income_sd +
  relig_sd + leftright,
  data = us, method = "logistic")
coef_res <- rbind(coef_res, cbind("Party ID", "All",
                  coef(mall_facets_pid)[1:7], 
                  confint(mall_facets_pid, level = 1 - (0.05/1))[1:7,1],
                  confint(mall_facets_pid, level = 1 - (0.05/1))[1:7,2]))

# 7-category party identification as outcome (treated as continuous, scaled to range 0 to 1)
mdem_facets_pid7 <- lm(I(demrep/6) ~ authority_sd + exploit_sd + entitle_sd + 
                          superior_sd + sufficient_sd + exhib_sd + vanity_sd  +
                          female + age_sd + edu_cat + not_caucasian + income_sd,
                        data = us)
coef_res <- rbind(coef_res, cbind("Party ID-7", "Socio-dem",
                                  coef(mdem_facets_pid7)[2:8], 
                                  confint(mdem_facets_pid7, level = 1 - (0.05/1))[2:8,1],
                                  confint(mdem_facets_pid7, level = 1 - (0.05/1))[2:8,2]))
mall_facets_pid7 <- lm(I(demrep/6) ~ authority_sd + exploit_sd + entitle_sd + 
                          superior_sd + sufficient_sd + exhib_sd + vanity_sd  +
                          female + age_sd + edu_cat + not_caucasian + income_sd +
                          relig_sd + leftright,
                        data = us)
coef_res <- rbind(coef_res, cbind("Party ID-7", "All",
                                  coef(mall_facets_pid7)[2:8], 
                                  confint(mall_facets_pid7, level = 1 - (0.05/1))[2:8,1],
                                  confint(mall_facets_pid7, level = 1 - (0.05/1))[2:8,2]))

coef_res <- cbind(coef_res, rownames(coef_res))

coef_res <- data.frame(coef_res, stringsAsFactors = FALSE)
coef_res[, 3:5] <- apply(coef_res[, 3:5], 2, function (x) as.numeric(x))

coef_res$X1 <- factor(coef_res$X1, levels = c("Self-report\nideology",
                                                        "Economy",
                                                        "Immigrants",
                                                        "Party ID-7",
                                                        "Refugees",
                                                        "Guns",
                                                        "Police",
                                                        "Party ID"))

coef_res$narc_lab[coef_res$X6 == "authority_sd"] <- "Authority"
coef_res$narc_lab[coef_res$X6 == "entitle_sd"] <- "Entitlement"
coef_res$narc_lab[coef_res$X6 == "exploit_sd"] <- "Exploitativeness"
coef_res$narc_lab[coef_res$X6 == "superior_sd"] <- "Superiority"
coef_res$narc_lab[coef_res$X6 == "sufficient_sd"] <- "Self-sufficiency"
coef_res$narc_lab[coef_res$X6 == "exhib_sd"] <- "Exhibitionism"
coef_res$narc_lab[coef_res$X6 == "vanity_sd"] <- "Vanity"


coef_res$narc_lab <- factor(coef_res$narc_lab, levels = c(
                                                        "Vanity",
                                                        "Exhibitionism",
                                                        "Self-sufficiency",
                                                        "Superiority",
                                                        "Exploitativeness",
                                                        "Entitlement",
                                                        "Authority"                                                        
                                                        ))

# -----------------------------------------------------------------------------------
# Supplementary Information 6: Detailed model results, demographic controls
# Narcissism Sub-Facets and Ideology/Party ID, demographic controls
# -----------------------------------------------------------------------------------

# htmlreg(l = list(mdem_facets_lr,
#                    mdem_facets_ec,
#                    mdem_facets_imm,
#                    mdem_facets_ref,
#                    mdem_facets_guns,
#                    mdem_facets_pol,
#                    mdem_facets_pid,
#                    mdem_facets_pid7),
#           custom.model.names = c("Self-report\nideo.",
#             "Econ\n(ordered)",
#             "Immigrants\n(ordered)",
#             "Refugees",
#             "Gun control",
#             "Police\n(logit)",
#             "Party ID\n(ordered)",
#             "7-Party ID"),
#           custom.coef.names = c("Intercept",
#                                 "Authority", "Exploitativeness",
#                                 "Entitlement", "Superiority",
#                                 "Self-sufficiency", "Exhibitionism",
#                                 "Vanity", "Gender", "Age",
#                                 "Education", "Race", "Income"),
#           caption = "Narcissism Sub-Facets and Ideology/Party ID, demographic controls",
#           file = "facets_ideo_dem.html")
summary(mdem_facets_lr)



# screenreg(l = list(mdem_facets_lr,
#                    mdem_facets_ec,
#                    mdem_facets_imm,
#                    mdem_facets_ref,
#                    mdem_facets_guns,
#                    mdem_facets_pol,
#                    mdem_facets_pid,
#                    mdem_facets_pid7),
#           digits = 3,
#           custom.model.names = as.character(c(1:8)),
#           custom.coef.names = c("Intercept",
#                                 "A", "E",
#                                 "Ent", "Sup",
#                                 "Ss", "Exh",
#                                 "Vanity", "Gender", "Age",
#                                 "Education", "Race", "Income"),
#           caption = "Narcissism Sub-Facets and Ideology/Party ID, demographic controls")

# -----------------------------------------------------------------------------------
# Supplementary Information 7: Detailed model results, all controls
# Narcissism Sub-Facets and Ideology/Party ID, all controls
# -----------------------------------------------------------------------------------

# htmlreg(l = list(mall_facets_lr,
#                    mall_facets_ec,
#                    mall_facets_imm,
#                    mall_facets_ref,
#                    mall_facets_guns,
#                    mall_facets_pol,
#                    mall_facets_pid,
#                    mall_facets_pid7),
#           custom.model.names = c("Self-report\nideo.",
#             "Econ\n(ordered)",
#             "Immigrants\n(ordered)",
#             "Refugees",
#             "Gun control",
#             "Police\n(logit)",
#             "Party ID\n(ordered)",
#             "7-Party ID"),
#           custom.coef.names = c("Intercept",
#                                 "Authority", "Exploitativeness",
#                                 "Entitlement", "Superiority",
#                                 "Self-sufficiency", "Exhibitionism",
#                                 "Vanity", "Gender", "Age",
#                                 "Education", "Race", "Income",
#                                 "Religiosity", "Idependent", "Republican",
#           "Self-report ideology"),
#           caption = "Narcissism Sub-Facets and Ideology/Party ID, all controls",
#           file = "facets_ideo_all.html")
summary(mall_facets_lr)
# screenreg(l = list(mall_facets_lr,
#                    mall_facets_ec,
#                    mall_facets_imm,
#                    mall_facets_ref,
#                    mall_facets_guns,
#                    mall_facets_pol,
#                    mall_facets_pid,
#                    mall_facets_pid7),
#           digits = 3,
#           custom.model.names = as.character(c(1:8)),
#           custom.coef.names = c("Intercept",
#                                 "A", "E",
#                                 "Ent", "Sup",
#                                 "Ss", "Exh",
#                                 "Vanity", "Gender", "Age",
#                                 "Education", "Race", "Income", "Rel", "Ind", "Rep",
#                                 "Ideo"),
#           caption = "Narcissism Sub-Facets and Ideology/Party ID, demographic controls")


# -----------------------------------------------------------------------------------
# 3.3 Create combined regression result summary plots for Main Text
# -----------------------------------------------------------------------------------
coef_npi <- coef_npi[, c(1, 3, 2, 4:6)]
coef_res <- coef_res[, c(1, 2, 7, 3:5)]
names(coef_npi) <- c("ideo_lab", "model", "narc_lab", "est", "lower", "upper")
names(coef_res) <- c("ideo_lab", "model", "narc_lab", "est", "lower", "upper")

coef_all <- bind_rows(coef_res, coef_npi)

coef_all$ideo_lab <- factor(coef_all$ideo_lab, levels = c("Self-report\nideology",
                                                        "Economy",
                                                        "Immigrants",
                                                        "Party ID-7",
                                                        "Refugees",
                                                        "Guns",
                                                        "Police",
                                                        "Party ID"))

coef_all$narc_lab <- factor(coef_all$narc_lab, levels = c(
                                                        "Vanity",
                                                        "Exhibitionism",
                                                        "Self-sufficiency",
                                                        "Superiority",
                                                        "Exploitativeness",
                                                        "Entitlement",
                                                        "Authority",
                                                        "Full NPI"                                                        
                                                        ))

coef_all$is_sig <- ifelse(coef_all$lower * coef_all$upper > 0, 1, 0)

# upper panel
p_dem_1 <- ggplot(filter(coef_all,
  ideo_lab != "Economy" &
  ideo_lab != "Immigrants" &
  ideo_lab != "Police" &
  ideo_lab != "Party ID" &
  model == "Socio-dem"), aes(x = narc_lab, y = est, 
                     ymin = lower, ymax = upper, colour = factor(is_sig))) +
  geom_hline(yintercept = 0, linetype = 2, alpha = 0.25) +
  geom_pointrange(size = 1.25) + 
  facet_grid(.~ideo_lab) +
  scale_colour_manual("", values = c("grey80", "black"), guide = FALSE) +
  theme_minimal(base_size = 16) + coord_flip() + xlab("") + ylab("") +
  theme(plot.margin       = unit(c(1, 1, 1, 1), "cm")) +
  theme(panel.spacing      = unit(1, "cm"), panel.grid = element_blank()) +
  scale_y_continuous("", breaks = c(-0.2, -0.1, 0, 0.1, 0.2),
    labels = c(-0.2, -0.1, 0, 0.1, 0.2))

# lower panel
p_dem_2 <- ggplot(filter(coef_all,
  (ideo_lab == "Economy" |
  ideo_lab == "Immigrants" |
  ideo_lab == "Police" |
  ideo_lab == "Party ID") & model == "Socio-dem"), aes(x = narc_lab, y = est, 
                     ymin = lower, ymax = upper,
                     colour = factor(is_sig))) +
  geom_hline(yintercept = 0, linetype = 2, alpha = 0.25) +
  geom_pointrange(size = 1.25) + 
  facet_grid(.~ideo_lab) +
  scale_colour_manual("", values = c("grey80", "black"), guide = FALSE) +  
  theme_minimal(base_size = 16) + coord_flip() + 
  ylab("Effect size for 2SD change in Narcissism and sub-facets 
        Separate models for each outcome and Full NPI vs sub-facets with 95% confidence intervals 
        First row of panels OLS estimates from linear regression, second row panels maximum likelihood estimates of logit coefficients 
        A positive effect reflects more conservative attitudes and a negative effect reflects more liberal attitudes") + xlab("") +
  theme(plot.margin       = unit(c(1, 1, 1, 1), "cm")) +
  theme(panel.spacing      = unit(1, "cm"), panel.grid = element_blank()) +
  scale_y_continuous(breaks = c(-1, -0.5, 0, 0.5, 1),
    labels = c(-1, -0.5, 0, 0.5, 1)) +
  theme(axis.title.x =element_text(margin = margin(b = 10, t = 30), hjust = 1, size = 10))

# -----------------------------------------------------------------------------------
# Figure 1: Narcissism coefficient plot
# -----------------------------------------------------------------------------------
p_dem_1 + p_dem_2 + plot_layout(ncol = 1) # needs patchwork library, otherwise comment out
# figures can be saved from device (use of cairo recommended)


# -----------------------------------------------------------------------------------
p_all_1 <- ggplot(filter(coef_all,
  ideo_lab != "Economy" &
  ideo_lab != "Immigrants" &
  ideo_lab != "Police" &
  ideo_lab != "Party ID" &
  model == "All"), aes(x = narc_lab, y = est, 
                     ymin = lower, ymax = upper, colour = factor(is_sig))) +
  geom_hline(yintercept = 0, linetype = 2, alpha = 0.25) +
  geom_pointrange(size = 1.25) + 
  facet_grid(.~ideo_lab) +
  scale_colour_manual("", values = c("grey80", "black"), guide = FALSE) +
  theme_minimal(base_size = 16) + coord_flip() + xlab("") + ylab("") +
  theme(plot.margin       = unit(c(1, 1, 1, 1), "cm")) +
  theme(panel.spacing      = unit(1, "cm"), panel.grid = element_blank()) +
  scale_y_continuous("", breaks = c(-0.2, -0.1, 0, 0.1, 0.2),
    labels = c(-0.2, -0.1, 0, 0.1, 0.2))

p_all_2 <- ggplot(filter(coef_all,
  (ideo_lab == "Economy" |
  ideo_lab == "Immigrants" |
  ideo_lab == "Police" |
  ideo_lab == "Party ID") & model == "All"), aes(x = narc_lab, y = est, 
                     ymin = lower, ymax = upper, colour = factor(is_sig))) +
  geom_hline(yintercept = 0, linetype = 2, alpha = 0.25) +
  geom_pointrange(size = 1.25) + 
  facet_grid(.~ideo_lab) +
  scale_colour_manual("", values = c("grey80", "black"), guide = FALSE) +  
  theme_minimal(base_size = 16) + coord_flip() +  
  ylab("Effect size for 2SD change in Narcissism and sub-facets 
        Separate models for each outcome and Full NPI vs sub-facets with 95% confidence intervals 
     First row of panels OLS estimates from linear regression, second row panels maximum likelihood estimates of logit coefficients 
     A positive effect reflects more conservative attitudes and a negative effect reflects more liberal attitudes") + xlab("") +
  theme(plot.margin       = unit(c(1, 1, 1, 1), "cm")) +
  theme(panel.spacing      = unit(1, "cm"), panel.grid = element_blank()) +
  scale_y_continuous(breaks = c(-1, -0.5, 0, 0.5, 1),
                     labels = c(-1, -0.5, 0, 0.5, 1)) +
  theme(axis.title.x = element_text(margin = margin(b = 10, t = 30), hjust = 1, size = 10))

# -----------------------------------------------------------------------------------
# Figure 3: Narcissism coefficient plot with control for Party ID for the 
#           Ideology outcome, and Self-report Ideology for the Party-ID model
# -----------------------------------------------------------------------------------
p_all_1 + p_all_2 + plot_layout(ncol = 1) # needs patchwork library, otherwise comment out
# figures can be saved from device (use of cairo recommended)

# -----------------------------------------------------------------------------------
# 3.4 Re-fitting sub-facet models with 3-facet operationalization
#     SI9 materials
# -----------------------------------------------------------------------------------
mdem_facets_lr <- lm(leftright ~ leadauth_sd + grdexhib_sd + entexp_sd  +
                       female + age_sd + edu_cat + not_caucasian + income_sd,
                     data = us)
coef_res <- cbind("Self-report\nideology", "Socio-dem",
                  coef(mdem_facets_lr)[2:4], 
                  confint(mdem_facets_lr, level = 1 - (0.05/1))[2:4,1],
                  confint(mdem_facets_lr, level = 1 - (0.05/1))[2:4,2])
mall_facets_lr <- lm(leftright ~ leadauth_sd + grdexhib_sd + entexp_sd  +
                       female + age_sd + edu_cat + not_caucasian + income_sd +
                       relig_sd + factor(pid3),
                     data = us)
coef_res <- rbind(coef_res, cbind("Self-report\nideology", "All",
                                  coef(mall_facets_lr)[2:4], 
                                  confint(mall_facets_lr, level = 1 - (0.05/1))[2:4,1],
                                  confint(mall_facets_lr, level = 1 - (0.05/1))[2:4,2]))
mdem_facets_ec <- polr(factor(economy) ~ leadauth_sd + grdexhib_sd + entexp_sd  +
                         female + age_sd + edu_cat + not_caucasian + income_sd,
                       data = us, method = "logistic")
coef_res <- rbind(coef_res, cbind("Economy", "Socio-dem",
                                  coef(mdem_facets_ec)[1:3], 
                                  confint(mdem_facets_ec, level = 1 - (0.05/1))[1:3,1],
                                  confint(mdem_facets_ec, level = 1 - (0.05/1))[1:3,2]))
mall_facets_ec <- polr(factor(economy) ~ leadauth_sd + grdexhib_sd + entexp_sd  +
                         female + age_sd + edu_cat + not_caucasian + income_sd +
                         relig_sd + factor(pid3),
                       data = us, method = "logistic")
coef_res <- rbind(coef_res, cbind("Economy", "All",
                                  coef(mall_facets_ec)[1:3], 
                                  confint(mall_facets_ec, level = 1 - (0.05/1))[1:3,1],
                                  confint(mall_facets_ec, level = 1 - (0.05/1))[1:3,2]))
mdem_facets_imm <- polr(factor(immigrants) ~ leadauth_sd + grdexhib_sd + entexp_sd  +
                          female + age_sd + edu_cat + not_caucasian + income_sd,
                        data = us, method = "logistic")
coef_res <- rbind(coef_res, cbind("Immigrants", "Socio-dem",
                                  coef(mdem_facets_imm)[1:3], 
                                  confint(mdem_facets_imm, level = 1 - (0.05/1))[1:3,1],
                                  confint(mdem_facets_imm, level = 1 - (0.05/1))[1:3,2]))
mall_facets_imm <- polr(factor(immigrants) ~ leadauth_sd + grdexhib_sd + entexp_sd  +
                          female + age_sd + edu_cat + not_caucasian + income_sd +
                          relig_sd + factor(pid3),
                        data = us, method = "logistic")
coef_res <- rbind(coef_res, cbind("Immigrants", "All",
                                  coef(mall_facets_imm)[1:3], 
                                  confint(mall_facets_imm, level = 1 - (0.05/1))[1:3,1],
                                  confint(mall_facets_imm, level = 1 - (0.05/1))[1:3,2]))
mdem_facets_ref <- lm(ref ~ leadauth_sd + grdexhib_sd + entexp_sd  +
                        female + age_sd + edu_cat + not_caucasian + income_sd,
                      data = us)


coef_res <- rbind(coef_res, cbind("Refugees", "Socio-dem",
                                  coef(mdem_facets_ref)[2:4], 
                                  confint(mdem_facets_ref, level = 1 - (0.05/1))[2:4,1],
                                  confint(mdem_facets_ref, level = 1 - (0.05/1))[2:4,2]))
mall_facets_ref <- lm(ref ~ leadauth_sd + grdexhib_sd + entexp_sd  +
                        female + age_sd + edu_cat + not_caucasian + income_sd +
                        relig_sd + factor(pid3),
                      data = us)
coef_res <- rbind(coef_res, cbind("Refugees", "All",
                                  coef(mall_facets_ref)[2:4], 
                                  confint(mall_facets_ref, level = 1 - (0.05/1))[2:4,1],
                                  confint(mall_facets_ref, level = 1 - (0.05/1))[2:4,2]))
mdem_facets_guns <- lm(guns ~ leadauth_sd + grdexhib_sd + entexp_sd  +
                         female + age_sd + edu_cat + not_caucasian + income_sd,
                       data = us)
coef_res <- rbind(coef_res, cbind("Guns", "Socio-dem",
                                  coef(mdem_facets_guns)[2:4], 
                                  confint(mdem_facets_guns, level = 1 - (0.05/1))[2:4,1],
                                  confint(mdem_facets_guns, level = 1 - (0.05/1))[2:4,2]))
mall_facets_guns <- lm(guns ~ leadauth_sd + grdexhib_sd + entexp_sd  +
                         female + age_sd + edu_cat + not_caucasian + income_sd +
                         relig_sd + factor(pid3),
                       data = us)
coef_res <- rbind(coef_res, cbind("Guns", "All",
                                  coef(mall_facets_guns)[2:4], 
                                  confint(mall_facets_guns, level = 1 - (0.05/1))[2:4,1],
                                  confint(mall_facets_guns, level = 1 - (0.05/1))[2:4,2]))

mdem_facets_pol <- glm(police ~ leadauth_sd + grdexhib_sd + entexp_sd  +
                         female + age_sd + edu_cat + not_caucasian + income_sd,
                       data = us, family = binomial)
coef_res <- rbind(coef_res, cbind("Police", "Socio-dem",
                                  coef(mdem_facets_pol)[2:4], 
                                  confint(mdem_facets_pol, level = 1 - (0.05/1))[2:4,1],
                                  confint(mdem_facets_pol, level = 1 - (0.05/1))[2:4,2]))
mall_facets_pol <- glm(police ~ leadauth_sd + grdexhib_sd + entexp_sd  +
                         female + age_sd + edu_cat + not_caucasian + income_sd +
                         relig_sd + factor(pid3),
                       data = us, family = binomial)
coef_res <- rbind(coef_res, cbind("Police", "All",
                                  coef(mall_facets_pol)[2:4], 
                                  confint(mall_facets_pol, level = 1 - (0.05/1))[2:4,1],
                                  confint(mall_facets_pol, level = 1 - (0.05/1))[2:4,2]))
mdem_facets_pid <- polr(factor(pid3) ~ leadauth_sd + grdexhib_sd + entexp_sd  +
                          female + age_sd + edu_cat + not_caucasian + income_sd,
                        data = us, method = "logistic")
coef_res <- rbind(coef_res, cbind("Party ID", "Socio-dem",
                                  coef(mdem_facets_pid)[1:3], 
                                  confint(mdem_facets_pid, level = 1 - (0.05/1))[1:3,1],
                                  confint(mdem_facets_pid, level = 1 - (0.05/1))[1:3,2]))
mall_facets_pid <- polr(factor(pid3) ~ leadauth_sd + grdexhib_sd + entexp_sd  +
                          female + age_sd + edu_cat + not_caucasian + income_sd +
                          relig_sd + leftright,
                        data = us, method = "logistic")

coef_res <- rbind(coef_res, cbind("Party ID", "All",
                                  coef(mall_facets_pid)[1:3], 
                                  confint(mall_facets_pid, level = 1 - (0.05/1))[1:3,1],
                                  confint(mall_facets_pid, level = 1 - (0.05/1))[1:3,2]))
mdem_facets_pid7 <- lm(I(demrep/6) ~ leadauth_sd + grdexhib_sd + entexp_sd  +
                           female + age_sd + edu_cat + not_caucasian + income_sd,
                         data = us)
coef_res <- rbind(coef_res, cbind("Party ID-7", "Socio-dem",
                                  coef(mdem_facets_pid7)[2:4], 
                                  confint(mdem_facets_pid7, level = 1 - (0.05/1))[2:4,1],
                                  confint(mdem_facets_pid7, level = 1 - (0.05/1))[2:4,2]))
mall_facets_pid7 <- lm(I(demrep/6) ~ leadauth_sd + grdexhib_sd + entexp_sd  +
                           female + age_sd + edu_cat + not_caucasian + income_sd +
                           relig_sd + leftright,
                         data = us)

coef_res <- rbind(coef_res, cbind("Party ID-7", "All",
                                  coef(mall_facets_pid7)[2:4], 
                                  confint(mall_facets_pid7, level = 1 - (0.05/1))[2:4,1],
                                  confint(mall_facets_pid7, level = 1 - (0.05/1))[2:4,2]))

coef_res <- cbind(coef_res, rownames(coef_res))

coef_res <- data.frame(coef_res, stringsAsFactors = FALSE)
coef_res[, 3:5] <- apply(coef_res[, 3:5], 2, function (x) as.numeric(x))

coef_res$X1 <- factor(coef_res$X1, levels = c("Self-report",
                                              "Economy",
                                              "Immigrants",
                                              "Party ID-7",
                                              "Refugees",
                                              "Guns",
                                              "Police",
                                              "Party ID"))

coef_res$narc_lab[coef_res$X6 == "leadauth_sd"] <- "Leadership/Authority"
coef_res$narc_lab[coef_res$X6 == "grdexhib_sd"] <- "Grandiose Exhibitionism"
coef_res$narc_lab[coef_res$X6 == "entexp_sd"] <- "Entitlement/Exploitativeness"


coef_res$narc_lab <- factor(coef_res$narc_lab, levels = c("Entitlement/Exploitativeness",
                                                          "Grandiose Exhibitionism", 
                                                          "Leadership/Authority"))

# -----------------------------------------------------------------------------------
# Supplementary Information 9: 3-factor Narcissism
# Narcissism 3-Sub-Facets and Ideology/Party ID, demographic controls
# -----------------------------------------------------------------------------------
# htmlreg(l = list(mdem_facets_lr,
#                    mdem_facets_ec,
#                    mdem_facets_imm,
#                    mdem_facets_ref,
#                    mdem_facets_guns,
#                    mdem_facets_pol,
#                    mdem_facets_pid,
#                    mdem_facets_pid7),
#           custom.model.names = c("Self-report\nideo.",
#             "Econ\n(ordered)",
#             "Immigrants\n(ordered)",
#             "Refugees",
#             "Gun control",
#             "Police\n(logit)",
#             "Party ID\n(ordered)",
#             "7-Party ID"),
#           custom.coef.names = c("Intercept",
#                                 "Leadership-Authority", "Grandiose Exhibitionism", "Entitlement-Exploitativeness", "Gender", "Age",
#                                 "Education", "Race", "Income"),
#           caption = "Narcissism 3-Sub-Facets and Ideology/Party ID, demographic controls",
#           file = "si9_3facets_ideo_dem.html")

summary(mdem_facets_lr)
# screenreg(l = list(mdem_facets_lr,
#                    mdem_facets_ec,
#                    mdem_facets_imm,
#                    mdem_facets_ref,
#                    mdem_facets_guns,
#                    mdem_facets_pol,
#                    mdem_facets_pid,
#                    mdem_facets_pid7),
#           digits = 3,
#           custom.model.names = as.character(c(1:8)),
#           custom.coef.names = c("Intercept",
#                                 "Lead", "Grand", "Ent", "Gender", "Age",
#                                 "Education", "Race", "Income"),
#           caption = "Narcissism 3-Sub-Facets and Ideology/Party ID, demographic controls")

# -----------------------------------------------------------------------------------
# Supplementary Information 9: 3-factor Narcissism
# Narcissism 3-Sub-Facets and Ideology/Party ID, all controls
# -----------------------------------------------------------------------------------
# htmlreg(l = list(mall_facets_lr,
#                    mall_facets_ec,
#                    mall_facets_imm,
#                    mall_facets_ref,
#                    mall_facets_guns,
#                    mall_facets_pol,
#                    mall_facets_pid,
#                    mall_facets_pid7),
#           custom.model.names = c("Self-report\nideo.",
#             "Econ\n(ordered)",
#             "Immigrants\n(ordered)",
#             "Refugees",
#             "Gun control",
#             "Police\n(logit)",
#             "Party ID\n(ordered)",
#             "7-Party ID"),
#           custom.coef.names = c("Intercept",
#                                 "Leadership-Authority", "Grandiose Exhibitionism", "Entitlement-Exploitativeness", "Gender", "Age",
#                                 "Education", "Race", "Income",
#                                 "Religiosity", "Idependent", "Republican",
#           "Self-report ideology"),
#           caption = "Narcissism 3-Sub-Facets and Ideology/Party ID, all controls",
#           file = "si9_3facets_ideo_all.html")
summary(mall_facets_lr)
# screenreg(l = list(mall_facets_lr,
#                    mall_facets_ec,
#                    mall_facets_imm,
#                    mall_facets_ref,
#                    mall_facets_guns,
#                    mall_facets_pol,
#                    mall_facets_pid,
#                    mall_facets_pid7),
#           digits = 3,
#           custom.model.names = as.character(c(1:8)),
#           custom.coef.names = c("Intercept",
#                                 "Lead", "Grand", "Ent", "Gender", "Age",
#                                 "Education", "Race", "Income", "Rel", "Ind", "Rep",
#                                 "Ideo"),
#           caption = "Narcissism 3-Sub-Facets and Ideology/Party ID, demographic controls")


# -----------------------------------------------------------------------------------
# Supplementary Information 9: 3-factor Narcissism
# SI9 Figure 1: Narcissism coefficient plot for three 
#               sub-facets with 95% confidence intervals. 
# -----------------------------------------------------------------------------------
names(coef_res) <- c("ideo_lab", "model", "est", "lower", "upper", "narc_orig", "narc_lab")
coef_res$is_sig <- ifelse(coef_res$lower * coef_res$upper > 0, 1, 0)
p_3comb_1 <- ggplot(filter(coef_res,
                         ideo_lab != "Economy" &
                           ideo_lab != "Immigrants" &
                           ideo_lab != "Police" &
                           ideo_lab != "Party ID"), aes(x = narc_lab, y = est, 
                                                ymin = lower, ymax = upper, 
                                                colour = factor(is_sig),
                                                shape = as.factor(model))) +
  geom_hline(yintercept = 0, linetype = 2, alpha = 0.25) +
  geom_pointrange(position = position_dodge(width = 0.6)) + 
  facet_grid(.~ideo_lab) +
  scale_colour_manual("", values = c("grey80", "black"), guide = FALSE) +
  theme_minimal() + coord_flip() + xlab("") + ylab("") +
  scale_shape_discrete("") +
  theme(plot.margin       = unit(c(1, 1, 1, 1), "cm")) +
  theme(panel.spacing      = unit(1, "cm"), panel.grid = element_blank()) +
  scale_y_continuous("", breaks = c(-0.2, -0.1, 0, 0.1, 0.2),
                     labels = c(-0.2, -0.1, 0, 0.1, 0.2))

p_3comb_3 <- ggplot(filter(coef_res,
                            ideo_lab == "Economy" |
                            ideo_lab == "Immigrants" |
                            ideo_lab == "Police" |
                            ideo_lab == "Party ID"), 
                  aes(x = narc_lab, y = est, 
                      ymin = lower, ymax = upper, 
                      colour = factor(is_sig),
                      shape = as.factor(model))) +
  geom_hline(yintercept = 0, linetype = 2, alpha = 0.25) +
  geom_pointrange(position = position_dodge(width = 0.6)) + 
  facet_grid(.~ideo_lab) +
  scale_shape_discrete(guide = FALSE) +
  scale_colour_manual("", values = c("grey80", "black"), guide = FALSE) +  
  theme_minimal() + coord_flip() + xlab("") + ylab("") +
  theme(plot.margin       = unit(c(1, 1, 1, 1), "cm")) +
  theme(panel.spacing      = unit(1, "cm"), panel.grid = element_blank()) +
  scale_y_continuous("", breaks = c(-1, -0.5, 0, 0.5, 1),
                     labels = c(-1, -0.5, 0, 0.5, 1))

p_3comb_1 + p_3comb_3 + plot_layout(ncol = 1) # needs patchwork library, otherwise comment out
# figures can be saved from device (use of cairo recommended)


# -----------------------------------------------------------------------------------
# ///
#
# PART 4: Entitlement as predictor form three outcomes, where outcomes are treated un-ordered
#         Figure 5: Multinomial logistic regressions of Entitlement and political attitudes
#
# ///
# -----------------------------------------------------------------------------------
# immigration as outcome
mlogit_imm <- zelig(factor(immigrants) ~ entitle_sd + 
                      female + age_sd + edu_cat + not_caucasian + income_sd +
                      relig_sd + factor(pid3),
                    data = us, model = "mlogit")
# summary(mlogit_imm)
x.weak   <- setx(mlogit_imm, entitle_sd = -1) # 2SD below the mean
x.strong <- setx(mlogit_imm, entitle_sd = 1) # 2SD above the mean
diff_imm <- sim(mlogit_imm, x = x.weak, x1 = x.strong)

m_imm <- data.frame(fit   = apply(data.frame(diff_imm$sim.out[["x1"]][["fd"]]), 2,
                                  function(x) quantile(x, 0.5)),
                    lower = apply(data.frame(diff_imm$sim.out[["x1"]][["fd"]]), 2,
                                  function(x) quantile(x, 0.025)),
                    upper = apply(data.frame(diff_imm$sim.out[["x1"]][["fd"]]), 2,
                                  function(x) quantile(x, 0.975)))
m_imm$what <- "Immigrants"
m_imm$prob <- c("Stay & apply",
                "Stay & not apply",
                "Leave")

m_imm$prob <- factor(m_imm$prob, levels = c("Stay & apply",
                                            "Stay & not apply",
                                            "Leave"))


# refugees as outcome
mlogit_ref <- zelig(factor(ref) ~ entitle_sd + 
                      female + age_sd + edu_cat + not_caucasian + income_sd +
                      relig_sd + factor(pid3),
                    data = us, model = "mlogit")
summary(mlogit_ref)
x.weak   <- setx(mlogit_ref, entitle_sd = -1)
x.strong <- setx(mlogit_ref, entitle_sd = 1)
diff_ref <- sim(mlogit_ref, x = x.weak, x1 = x.strong)

m_ref <- data.frame(fit   = apply(data.frame(diff_ref$sim.out[["x1"]][["fd"]]), 2,
                                  function(x) quantile(x, 0.5)),
                    lower = apply(data.frame(diff_ref$sim.out[["x1"]][["fd"]]), 2,
                                  function(x) quantile(x, 0.025)),
                    upper = apply(data.frame(diff_ref$sim.out[["x1"]][["fd"]]), 2,
                                  function(x) quantile(x, 0.975)))
m_ref$what <- "Refugees"
m_ref$prob <- c("Fav\ngr deal", "Fav\nmod", 
                "Fav\nlittle", "Neither",
                "Opp\nlittle", "Opp\nmod", "Opp\ngr deal")
m_ref$prob <- factor(m_ref$prob, levels = c("Fav\ngr deal", "Fav\nmod", 
                                            "Fav\nlittle", "Neither",
                                            "Opp\nlittle", "Opp\nmod", "Opp\ngr deal"))

# 3-point party id as outcome
mlogit_pid <- zelig(factor(pid3) ~ entitle_sd + 
                      female + age_sd + edu_cat + not_caucasian + income_sd +
                      relig_sd + leftright,
                    data = us, model = "mlogit")
summary(mlogit_pid)
x.weak   <- setx(mlogit_pid, entitle_sd = -1)
x.strong <- setx(mlogit_pid, entitle_sd = 1)
diff_pid <- sim(mlogit_pid, x = x.weak, x1 = x.strong)

m_pid <- data.frame(fit   = apply(data.frame(diff_pid$sim.out[["x1"]][["fd"]]), 2,
                                  function(x) quantile(x, 0.5)),
                    lower = apply(data.frame(diff_pid$sim.out[["x1"]][["fd"]]), 2,
                                  function(x) quantile(x, 0.025)),
                    upper = apply(data.frame(diff_pid$sim.out[["x1"]][["fd"]]), 2,
                                  function(x) quantile(x, 0.975)))
m_pid$what <- "Party ID"
m_pid$prob <- c("Democrat", "Independent", "Republican")


m_all <- rbind(m_ref, m_imm, m_pid) # combine results in one dataframe
m_all$is_sig <- ifelse(m_all$lower * m_all$upper > 0, 1, 0)

# -----------------------------------------------------------------------------------
# Figure 5: Multinomial logistic regressions of Entitlement and political attitudes
# -----------------------------------------------------------------------------------
ggplot(m_all, aes(x = prob, y = fit, 
                  ymin = lower, 
                  ymax = upper, colour = factor(is_sig))) +
  geom_hline(yintercept = 0, linetype = 2, colour = "grey50") +
  geom_pointrange(size = 1.25) +
  scale_colour_manual("", values = c("grey80", "black"), guide = FALSE) +
  facet_wrap(~what, ncol = 3, scales = "free_y") +
  coord_flip() +
  theme_minimal(base_size = 16) + xlab("") + 
  ylab("First difference: from 2 SD below the mean to 2 SD above mean in Entitlement") +
  theme(plot.margin       = unit(c(1, 1, 1, 1), "cm")) +
  theme(panel.spacing      = unit(1, "cm"),
        panel.grid = element_blank()) +
  theme(strip.text = element_text(hjust = 0)) +
  scale_size_continuous("") +
  theme(axis.title.x = element_text(margin = margin(b = 10, t = 30), hjust = 1, size = 10))
# ggsave("fig5_entitle_mnl.pdf", width = 12, height = 6) # uncomment to save

# -----------------------------------------------------------------------------------
# ///
#
# PART 5: Hierarchical models for Entitlement and Exhibitionism
#         Figure 2: Figure 2: Hierarchical models of Ideology-Party ID with varying effects of 
#                   Entitlement and Exhibitionism. The solid (red) vertical line is the overall (average) 
#                   effect across the different Ideology-Party ID measures with 95% credible intervals shaded.
#         Figure 4: Figure 4: Hierarchical models of Ideology-Party ID with varying effects of 
#                   Entitlement and Exhibitionism. Includes control for Party ID (Ideology outcomes) and 
#                   Ideology (Party ID outcome). 
#
# ///
# -----------------------------------------------------------------------------------

# -----------------------------------------------------------------------------------
# 5.1 Create dataframe for hierarchical model
# -----------------------------------------------------------------------------------
# create a "stacked" dataframe, long format
# important: extra control for ideo cases should be based on PID, but for PID model should be leftright
# y is outcome value, outcome identifies which outcome we are looking at

controls <- c("female", "age_sd", "relig_sd", "edu_cat",
              "not_caucasian", "income_sd")

ideo_vars <- c("leftright", ideo_vars)

pid_data <- us[, c("pid3", "exhib_sd", 
                   "entitle_sd", "leftright",
                   controls)]
names(pid_data)[c(1, 4)] <- c("y", 
                              "extra_control")

pid_data$outcome <- "pid"
pid_data$y <- (pid_data$y + 1)/2 # recode PID to be 0, 0.5, 1 (rather than -1, 0, 1)

ideo_data <- us[, c(ideo_vars, "pid3", "exhib_sd", "entitle_sd",
                   controls)]
ideo_data <- reshape2::melt(ideo_data, id = c("pid3", "exhib_sd", "entitle_sd", controls))
ideo_data <- ideo_data[, c(11, 2:3, 1, 4:9, 10)] # re-order variables (same structure as for PID)
names(ideo_data)[c(1, 4, 11)] <- c("y", 
                                  "extra_control", "outcome")

# bind
hlm_data <- rbind(ideo_data, pid_data)
rm(ideo_data, pid_data) # clean

# -----------------------------------------------------------------------------------
# 5.2 Fit hierarchical models reported
# -----------------------------------------------------------------------------------
# Note = coffee time
# Around 10-15 minutes run time/model
  # 2.8 GHz Intel Core i5 (2014), 16GB RAM
# Example (from one model, one chain)
#   Elapsed Time: 576.741 seconds (Warm-up)
#   214.22 seconds (Sampling)
#   790.962 seconds (Total)
# -----------------------------------------------------------------------------------
rstan_options(auto_write = TRUE)
n_cores  <- 3 # modify, if needed
options(mc.cores = n_cores)
seed_val <- 12345
n_iter  <- 2000
n_chain <- 3 
wi_prior <- normal(0, 2.5)

# entitlement as predictor, socio-dem controls
# note that no correlation between varying intercept and varying slope is estimated
bayes_ent_1_sd <- stan_lmer(y ~ entitle_sd +
                              female + age_sd + edu_cat + not_caucasian + income_sd +
                              (1 | outcome) + (0 + entitle_sd | outcome),
                            prior  = wi_prior,
                            data   = hlm_data,
                            chains = n_chain,
                            iter   = n_iter,
                            adapt_delta = 0.99,
                            seed   = seed_val)
# entitlement as predictor, all controls
bayes_ent_1 <- stan_lmer(y ~ entitle_sd +
                       female + age_sd + edu_cat + not_caucasian + income_sd +
                       relig_sd + extra_control + (1 | outcome) + (0 + entitle_sd | outcome),
                       prior  = wi_prior,
                       data   = hlm_data,
                       chains = n_chain,
                       iter   = n_iter,
                       adapt_delta = 0.99,
                       seed   = seed_val)

# exhibitionism as predictor, socio-dem controls
# note that no correlation between varying intercept and varying slope is estimated
bayes_exhib_1_sd <- stan_lmer(y ~ exhib_sd +
                                female + age_sd + edu_cat + not_caucasian + income_sd +
                                (1 | outcome) +
                                (0 + exhib_sd | outcome),
                              prior  = wi_prior,
                              data   = hlm_data,
                              chains = n_chain,
                              iter   = n_iter,
                              adapt_delta = 0.99,
                              seed   = seed_val)
# exhibitionism as predictor, all controls
bayes_exhib_1 <- stan_lmer(y ~ exhib_sd +
                       female + age_sd + edu_cat + not_caucasian + income_sd +
                       relig_sd + extra_control + (1 | outcome) +
                       (0 + exhib_sd | outcome),
                       prior  = wi_prior,
                       data   = hlm_data,
                       chains = n_chain,
                       iter   = n_iter,
                       adapt_delta = 0.99,
                       seed   = seed_val)


# inspect results
# summary(bayes_exhib_1)
# summary(bayes_exhib_1_sd)
# 
# summary(bayes_ent_1)
# summary(bayes_ent_1_sd)

# -----------------------------------------------------------------------------------
# 5.3 Extract relevant information and put together dataframe for plotting
# -----------------------------------------------------------------------------------

# -----------------------------------------------------------------------------------
# For soc-dem figure
# -----------------------------------------------------------------------------------
# Figure 2: Hierarchical models of Ideology-Party ID with varying effects of 
#           Entitlement and Exhibitionism. The solid (red) vertical line is the overall (average) 
#           effect across the different Ideology-Party ID measures with 95% credible intervals shaded.
# -----------------------------------------------------------------------------------
coefs <- data.frame(bayes_ent_1_sd$coefficients)
coefs$what <- rownames(coefs)
coefs <- coefs[grep("entitle_sd", rownames(coefs)), ] # all entitlement related coefs

# note: first is average effect, rest is deviation from that average effect
#       i.e. coefficient for leftright would be 0.07+(-0.011)

# separating these
ent_mean <- coefs[1, ]
coefs <- coefs[-1, ]

# get CIs
ci95 <- data.frame(posterior_interval(bayes_ent_1_sd, prob = 0.95))
ci95$what <- rownames(ci95)
ci95 <- ci95[grep("entitle_sd", rownames(ci95)), ]
ent_mean <- cbind(ent_mean, ci95[1, ]) # again, CI for average effect
ci95 <- ci95[c(-1, -9), ] # CIs for the deviation
coefs <- cbind(coefs, ci95)

# same steps for exhibitionism
coef_exhib <- data.frame(bayes_exhib_1_sd$coefficients)
coef_exhib$what <- rownames(coef_exhib)
coef_exhib <- coef_exhib[grep("exhib_sd", rownames(coef_exhib)), ]

exhib_mean <- coef_exhib[1, ]
coef_exhib <- coef_exhib[-1, ]

ci95 <- data.frame(posterior_interval(bayes_exhib_1_sd, prob = 0.95))
ci95$what <- rownames(ci95)
ci95 <- ci95[grep("exhib_sd", rownames(ci95)), ]
exhib_mean <- cbind(exhib_mean, ci95[1, ])
ci95 <- ci95[c(-1, -9), ]
coef_exhib <- cbind(coef_exhib, ci95)
coefs <- coefs[, c(2, 1, 3, 4), ]
coef_exhib <- coef_exhib[, c(2, 1, 3, 4), ]
names(coefs) <- c("what", "med", "lwr", "upr")
names(coef_exhib) <- c("what", "med", "lwr", "upr")
coefs$pred <- "Entitlement"
coef_exhib$pred <- "Exhibitionism"

coefs$lwr <- fixef(bayes_ent_1_sd)[2] + coefs$lwr # lower CI
coefs$upr <- fixef(bayes_ent_1_sd)[2] + coefs$upr # upper CI
coefs$med <- fixef(bayes_ent_1_sd)[2] + coefs$med # effect

coef_exhib$lwr <- fixef(bayes_exhib_1_sd)[2] + coef_exhib$lwr
coef_exhib$upr <- fixef(bayes_exhib_1_sd)[2] + coef_exhib$upr
coef_exhib$med <- fixef(bayes_exhib_1_sd)[2] + coef_exhib$med

coefs <- rbind(coefs, coef_exhib)

coefs$id <- matrix(unlist(stringr::str_split(coefs$what, ":", 2)), 
                   ncol = 2, byrow = TRUE)[, 2]
coefs$id <- gsub("]", "", coefs$id)


coefs$id_lab <- "Self-report ideology"
coefs$id_lab[coefs$id == "economy"] <- "Economy"
coefs$id_lab[coefs$id == "immigrants"] <- "Immigrants"
coefs$id_lab[coefs$id == "ref"] <- "Refugees"
coefs$id_lab[coefs$id == "guns"] <- "Guns"
coefs$id_lab[coefs$id == "police"] <- "Police"
coefs$id_lab[coefs$id == "pid"] <- "Party ID"


coefs$id_lab <- factor(coefs$id_lab, levels = c("Self-report ideology",
                                                "Economy",
                                                "Immigrants",
                                                "Refugees",
                                                "Guns",
                                                "Police",
                                                "Party ID"))

ent_mean <- ent_mean[, c(1:4)]
exhib_mean <- exhib_mean[, c(1:4)]
names(ent_mean) <- c("med", "pred", "lwr", "upr")
ent_mean[, 2] <- "Entitlement"
names(exhib_mean) <- c("med", "pred", "lwr", "upr")
exhib_mean[, 2] <- "Exhibitionism"
means <- rbind(ent_mean, exhib_mean)

tmp <- coefs[, c("pred", "id_lab")]
tmp$med <- NA
tmp$lwr <- NA
tmp$upr <- NA
tmp$med[1:7] <- ent_mean$med
tmp$med[8:14] <- exhib_mean$med

tmp$lwr[1:7] <- ent_mean$lwr
tmp$lwr[8:14] <- exhib_mean$lwr

tmp$upr[1:7] <- ent_mean$upr
tmp$upr[8:14] <- exhib_mean$upr

lablist <- coefs[1:7, "id_lab"]
coefs$id_num <- as.numeric(coefs$id_lab)
tmp$id_num <- as.numeric(tmp$id_lab)

# -----------------------------------------------------------------------------------
# Figure 2: Hierarchical models of Ideology-Party ID with varying effects of 
#           Entitlement and Exhibitionism. The solid (red) vertical line is the overall (average) 
#           effect across the different Ideology-Party ID measures with 95% credible intervals shaded.
# -----------------------------------------------------------------------------------

ggplot(coefs, aes(x = id_num, y = med, ymin = lwr, ymax = upr)) +
  facet_wrap(~pred, ncol = 2) +
  geom_rect(data = tmp, aes(ymin = lwr, ymax = upr,
                            xmin = 0.5, 
                            xmax = 7.5), alpha = 0.25, fill = "grey95") +
  geom_hline(data = tmp, aes(yintercept = med), colour = "darkgrey", alpha = 0.5) +
  geom_hline(yintercept = 0, alpha = 0.25, linetype = 2) +
  scale_x_continuous("", breaks = seq(1:7),
                     labels = lablist) +
  geom_pointrange(size = 1.25) + 
  theme_minimal(base_size = 16) + coord_flip() + xlab("") + 
  scale_y_continuous(breaks = c(-0.2, -0.1, 0, 0.1, 0.2),
                     labels = c(-0.2, -0.1, 0, 0.1, 0.2)) +
  ylab("Hierarchical models of Ideology-Party ID with varying effects of Entitlement and Exhibitionism. 
        The solid vertical line is the overall (average) effect across the different Ideology-Party ID measures with 95% credible intervals shaded.") +
  theme(plot.margin       = unit(c(1, 1, 1, 1), "cm")) +
  theme(panel.spacing      = unit(1, "cm"),
        panel.grid = element_blank()) +
  theme(strip.text = element_text(hjust = 0, size = 12)) +
  theme(axis.title.x =element_text(margin = margin(b = 10, t = 30), hjust = 1, size = 10))
# ggsave("fig2_hierarchical_sd.pdf", width = 12, height = 6) # uncomment to save


# -----------------------------------------------------------------------------------
# All controls
# -----------------------------------------------------------------------------------
# Figure 4: Hierarchical models of Ideology-Party ID with varying effects of 
#           Entitlement and Exhibitionism. Includes control for Party ID (Ideology outcomes) and 
#           Ideology (Party ID outcome). 
# -----------------------------------------------------------------------------------
# note: same steps as before

coefs <- data.frame(bayes_ent_1$coefficients)
coefs$what <- rownames(coefs)
coefs <- coefs[grep("entitle_sd", rownames(coefs)), ]

ent_mean <- coefs[1, ]
coefs <- coefs[-1, ]

ci95 <- data.frame(posterior_interval(bayes_ent_1, prob = 0.95))
ci95$what <- rownames(ci95)
ci95 <- ci95[grep("entitle_sd", rownames(ci95)), ]
ent_mean <- cbind(ent_mean, ci95[1, ])
ci95 <- ci95[c(-1, -9), ]
coefs <- cbind(coefs, ci95)


coef_exhib <- data.frame(bayes_exhib_1$coefficients)
coef_exhib$what <- rownames(coef_exhib)
coef_exhib <- coef_exhib[grep("exhib_sd", rownames(coef_exhib)), ]

exhib_mean <- coef_exhib[1, ]
coef_exhib <- coef_exhib[-1, ]

ci95 <- data.frame(posterior_interval(bayes_exhib_1, prob = 0.95))
ci95$what <- rownames(ci95)
ci95 <- ci95[grep("exhib_sd", rownames(ci95)), ]
exhib_mean <- cbind(exhib_mean, ci95[1, ])
ci95 <- ci95[c(-1, -9), ]
coef_exhib <- cbind(coef_exhib, ci95)
coefs <- coefs[, c(2, 1, 3, 4), ]
coef_exhib <- coef_exhib[, c(2, 1, 3, 4), ]
names(coefs) <- c("what", "med", "lwr", "upr")
names(coef_exhib) <- c("what", "med", "lwr", "upr")
coefs$pred <- "Entitlement"
coef_exhib$pred <- "Exhibitionism"

coefs$lwr <- fixef(bayes_ent_1)[2] + coefs$lwr
coefs$upr <- fixef(bayes_ent_1)[2] + coefs$upr
coefs$med <- fixef(bayes_ent_1)[2] + coefs$med

coef_exhib$lwr <- fixef(bayes_exhib_1)[2] + coef_exhib$lwr
coef_exhib$upr <- fixef(bayes_exhib_1)[2] + coef_exhib$upr
coef_exhib$med <- fixef(bayes_exhib_1)[2] + coef_exhib$med

coefs <- rbind(coefs, coef_exhib)

coefs$id <- matrix(unlist(stringr::str_split(coefs$what, ":", 2)), 
  ncol = 2, byrow = TRUE)[, 2]
coefs$id <- gsub("]", "", coefs$id)
head(coefs)

coefs$id_lab <- "Self-report ideology"
coefs$id_lab[coefs$id == "economy"] <- "Economy"
coefs$id_lab[coefs$id == "immigrants"] <- "Immigrants"
coefs$id_lab[coefs$id == "ref"] <- "Refugees"
coefs$id_lab[coefs$id == "guns"] <- "Guns"
coefs$id_lab[coefs$id == "police"] <- "Police"
coefs$id_lab[coefs$id == "pid"] <- "Party ID"


coefs$id_lab <- factor(coefs$id_lab, levels = c("Self-report ideology",
                                                        "Economy",
                                                        "Immigrants",
                                                        "Refugees",
                                                        "Guns",
                                                        "Police",
                                                        "Party ID"))

ent_mean <- ent_mean[, c(1:4)]
exhib_mean <- exhib_mean[, c(1:4)]
names(ent_mean) <- c("med", "pred", "lwr", "upr")
ent_mean[, 2] <- "Entitlement"
names(exhib_mean) <- c("med", "pred", "lwr", "upr")
exhib_mean[, 2] <- "Exhibitionism"
means <- rbind(ent_mean, exhib_mean)
tmp <- coefs[, c("pred", "id_lab")]
tmp$med <- NA
tmp$lwr <- NA
tmp$upr <- NA
tmp$med[1:7] <- ent_mean$med
tmp$med[8:14] <- exhib_mean$med

tmp$lwr[1:7] <- ent_mean$lwr
tmp$lwr[8:14] <- exhib_mean$lwr

tmp$upr[1:7] <- ent_mean$upr
tmp$upr[8:14] <- exhib_mean$upr

lablist <- coefs[1:7, "id_lab"]
coefs$id_num <- as.numeric(coefs$id_lab)
tmp$id_num <- as.numeric(tmp$id_lab)

ggplot(coefs, aes(x = id_num, y = med, ymin = lwr, ymax = upr)) +
  facet_wrap(~pred, ncol = 2) +
  geom_rect(data = tmp, aes(ymin = lwr, ymax = upr,
                            xmin = 0.5, 
                            xmax = 7.5), alpha = 0.25, fill = "grey95") +
  geom_hline(data = tmp, aes(yintercept = med), colour = "darkgrey", alpha = 0.5) +
  geom_hline(yintercept = 0, alpha = 0.25, linetype = 2) +
  scale_x_continuous("", breaks = seq(1:7),
                     labels = lablist) +
  geom_pointrange(size = 1.25) + 
  theme_minimal(base_size = 16) + coord_flip() + xlab("") + 
  scale_y_continuous(breaks = c(-0.2, -0.1, 0, 0.1, 0.2),
                     labels = c(-0.2, -0.1, 0, 0.1, 0.2)) +
  ylab("Hierarchical models of Ideology-Party ID with varying effects of Entitlement and Exhibitionism, all controls. 
       The solid vertical line is the overall (average) effect across the different Ideology-Party ID measures with 95% credible intervals shaded.") +
  theme(plot.margin       = unit(c(1, 1, 1, 1), "cm")) +
  theme(panel.spacing      = unit(1, "cm"),
        panel.grid = element_blank()) +
  theme(strip.text = element_text(hjust = 0, size = 12)) +
  theme(axis.title.x =element_text(margin = margin(b = 10, t = 30), hjust = 1, size = 10))
# ggsave("fig4_hierarchical_all.pdf", width = 12, height = 6) # uncomment to save

# sessionInfo()
