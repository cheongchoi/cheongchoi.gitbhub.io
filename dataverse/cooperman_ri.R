rm(list=ls(all=TRUE))
dev.off()
# Created using R version 3.3.2 under OS X Yosemite 10.10.5
library(plm) # Created using version 1.6-5
library(dplyr) # Created using version 0.5.0
library(tidyr) # Created using version 0.6.1
library(lme4) # Created using version 1.1-12
library(lfe) # Created using version 2.5-1998
library(reshape2) # Created using version 1.4.2
library(Matrix) # Created using version 1.2-7.1
library(Formula) # Created using version 1.2-1

# NOTE: Set working directory
setwd("~/Downloads/rep")

# NOTE: Most of the code conducts the RI procedure
# The RI procedure generates 1000 permutations of the treatment assignment vector (ie. 1000 potential weather assignments ) for each specification and then calculates the ATE.
# The procedure is time-consuming and creates large .Rdata files, but I recommend saving the .Rdata files to avoid repeating the procedure in the future.
# Uncomment the #save and #load lines to save and then load these files.
# The output files of estimated ATEs and p-values under different numbers of simulations are included in Replication folder.

# Load Data --------
load("cooperman_dataset.Rdata") 
names(dataset)
# Subset data to drop 7 counties in Gomez et al. (2007) dataset that do not have out-of-sample rainfall data
data2 <- dataset[!is.na(dataset$Rainfall12),]
data2 <- data2 %>% arrange(FIPS.County,Year)

nsims = 10 # Use nsims = 10 to run RI efficiently on 10 permutations of treatment assignment.
# nsims = 1000 # Paper uses n of 1000. To replicate full results, uncomment beginning of line. 

# 1. FUNCTIONS ----------
##################################################################

# These functions generate permutations of the treatment assignment vector. They assume all counties are independent.
assign_independently_inch <- function(df){
  df_sim<-
    df %>%
    group_by(FIPS.County) %>%
    mutate(Z_sim = sample(Rainfall12, n(), replace=T)) %>%
    arrange(FIPS.County)
  return(df_sim$Z_sim)
}
assign_independently_index <- function(df){
  df_sim<-
    df %>%
    group_by(FIPS.County) %>%
    mutate(Z_sim = sample(Rainfall12.index, n(), replace=T)) %>%
    arrange(FIPS.County)
  return(df_sim$Z_sim)
}
assign_independently_spi <- function(df){
  df_sim<-
    df %>%
    group_by(FIPS.County) %>%
    mutate(Z_sim = sample(Rainfall12.spi, n(), replace=T)) %>%
    arrange(FIPS.County)
  return(df_sim$Z_sim)
}

# This function generates permutations of the treatment assignment vector. It assumes all CWAs are independent.
assign_cwa_year <- function(df,rainmeasure){
  wide_df <- dcast(df, CWA_short + FIPS.County~ Year , value.var = rainmeasure)
  unique_cwas <- unique(wide_df$CWA_short)
  internal_fun <- function(){
    Z <- rep(NA, nrow(wide_df))
    for(i in 1:length(unique_cwas)){
      local_df <- filter(wide_df, CWA_short==unique_cwas[i])
      Z[wide_df$CWA_short==unique_cwas[i]] <- local_df[,sample(3:ncol(wide_df), 1)]
    }
    return(Z)
  }
  return(as.vector(replicate(length(unique(df$Year)),internal_fun())))
}

# This function generates permutations of the treatment assignment vector. It assumes all states are independent.
assign_state_year <- function(df,rainmeasure){
  wide_df <- dcast(df, FIPS.County + State ~ Year , value.var = rainmeasure)
  unique_states <- unique(wide_df$State)
  internal_fun <- function(){
    Z <- rep(NA, nrow(wide_df))
    for(i in 1:length(unique_states)){
      local_df <- filter(wide_df, State==unique_states[i])
      Z[wide_df$State==unique_states[i]] <- local_df[,sample(3:ncol(wide_df), 1)]
    }
    return(Z)
  }
  return(as.vector(replicate(length(unique(df$Year)),internal_fun())))
}

# This function generates permutations of the treatment assignment vector. It assumes all weather regions are independent.
assign_reg_year <- function(df,rainmeasure){
  wide_df <- dcast(df, REG + FIPS.County~ Year , value.var = rainmeasure)
  unique_regs <- unique(wide_df$REG)
  internal_fun <- function(){
    Z <- rep(NA, nrow(wide_df))
    for(i in 1:length(unique_regs)){
      local_df <- filter(wide_df, REG==unique_regs[i])
      Z[wide_df$REG==unique_regs[i]] <- local_df[,sample(3:ncol(wide_df), 1)]
    }
    return(Z)
  }
  return(as.vector(replicate(length(unique(df$Year)),internal_fun())))
}

# This function generates permutations of the treatment assignment vector. It assumes that US-years are independent of each other. It draws counties from the same year.
assign_country_year <- function(df,rainmeasure){
  wide_df <- dcast(df, FIPS.County + State ~ Year , value.var = rainmeasure)
  internal_fun <- function(){
    Z <- wide_df[,sample(3:ncol(wide_df), 1)]
    return(Z)
  }
  return(as.vector(replicate(length(unique(df$Year)),internal_fun())))
}

# 2. Output needed for Table 3 and Figure 4 -----
##################################################################

# Conduct RI procedure for Rainfall (inches)
# COUNTY ---------
# 1. Create potential weather assignments using out-of-sample rainfall data, assume counties independent
data2 <- data2 %>% arrange(FIPS.County,Year)
set.seed(1234)

# zindep is a matrix in which the columns correspond to different permutations of the treatment assignment vector generated by the function assign_independently_inch()
zindep <- matrix(NA, nrow=nrow(data2), ncol=nsims)
colnames(zindep) <- paste0("V", 1:nsims)
for (i in 1:nsims){
  zindep[,i] <- assign_independently_inch(data2)
  print(i)
}
data.county <- cbind(data2, zindep)
# save(data.county, file="Data_county.Rdata")

# 2. Calculate estimates for ATE under different permutations of the treatment assignment vector
#load("Data_county.Rdata")
zsims.cols <- colnames(data.county)[grep("V",colnames(data.county),fixed=TRUE)]

# This loop estimates the random effects model from Gomez et al. (2007) utilizing each permutation of the treatment assignment vector. 
# It returns the estimated ATE under each permutation.
ate.sims.indep <- rep(NA, nsims)
for (i in 1:nsims){
  model <- lmer(Turnout ~ get(zsims.cols[i]) + Snow  + ZPcntHSGrad + AdjIncome + PcntBlack + FarmsPerCap + Closing + Motor + Property + Literacy + PollTax  + GubElection + SenElection + Turnout.Lag + (1|FIPS.County) + as.factor(Year), data=data.county)
  ate.sims.indep[i] <- fixef(model)["get(zsims.cols[i])"]
  print(i)
}
save(ate.sims.indep, file="ate_sims_indep.Rdata")

# CWA ---------
data2 <- data2 %>% arrange(Year,CWA_short, FIPS.County)
set.seed(1234)
zcwa <- matrix(NA, nrow=nrow(data2), ncol=nsims)
colnames(zcwa) <- paste0("V", 1:nsims)
for (i in 1:nsims){
  zcwa[,i] <- assign_cwa_year(data2,"Rainfall12")
  print(i)
}
data2 <- data2 %>% arrange(Year,CWA_short, FIPS.County)
data.cwa <- cbind(data2,zcwa)
#save(data.cwa, file="Data_cwa.Rdata")
#load("Data_cwa.Rdata")
zsims.cols <- colnames(data.cwa)[grep("V",colnames(data.cwa),fixed=TRUE)]
ate.sims.cwa <- rep(NA, nsims)
for (i in 1:nsims){
  model <- lmer(Turnout ~ get(zsims.cols[i]) + Snow  + ZPcntHSGrad + AdjIncome + PcntBlack + FarmsPerCap + Closing + Motor + Property + Literacy + PollTax  + GubElection + SenElection + Turnout.Lag + (1|FIPS.County) + as.factor(Year), data=data.cwa)
  ate.sims.cwa[i] <- fixef(model)["get(zsims.cols[i])"]
  print(i)
}
save(ate.sims.cwa, file="ate_sims_cwa.Rdata")

# STATE ---------
data2 <- data2 %>% arrange(Year,State, FIPS.County)
set.seed(1234)
zstate <- matrix(NA, nrow=nrow(data2), ncol=nsims)
colnames(zstate) <- paste0("V", 1:nsims)
for (i in 1:nsims){
  zstate[,i] <- assign_state_year(data2, "Rainfall12")
  print(i)
}
data2 <- data2 %>% arrange(Year,State, FIPS.County)
data.state <- cbind(data2,zstate)
#save(data.state, file="Data_state.Rdata")
#load("Data_state.Rdata")
zsims.cols <- colnames(data.state)[grep("V",colnames(data.state),fixed=TRUE)]
ate.sims.state <- rep(NA, nsims)
for (i in 1:nsims){
  model <- lmer(Turnout ~ get(zsims.cols[i]) + Snow  + ZPcntHSGrad + AdjIncome + PcntBlack + FarmsPerCap + Closing + Motor + Property + Literacy + PollTax  + GubElection + SenElection + Turnout.Lag + (1|FIPS.County) + as.factor(Year), data=data.state)
  ate.sims.state[i] <- fixef(model)["get(zsims.cols[i])"]
  print(i)
}
save(ate.sims.state, file="ate_sims_state.Rdata")

# REGION ---------
data2 <- data2 %>% arrange(Year,REG, FIPS.County)
set.seed(1234)
zreg <- matrix(NA, nrow=nrow(data2), ncol=nsims)
colnames(zreg) <- paste0("V", 1:nsims)
for (i in 1:nsims){
  zreg[,i] <- assign_reg_year(data2, "Rainfall12")
  print(i)
}
data2 <- data2 %>% arrange(Year,REG, FIPS.County)
data.reg <- cbind(data2,zreg)
#save(data.reg, file="Data_reg.Rdata")
#load("Data_reg.Rdata")
zsims.cols <- colnames(data.reg)[grep("V",colnames(data.reg),fixed=TRUE)]
ate.sims.reg <- rep(NA, nsims)
for (i in 1:nsims){
  model <- lmer(Turnout ~ get(zsims.cols[i]) + Snow  + ZPcntHSGrad + AdjIncome + PcntBlack + FarmsPerCap + Closing + Motor + Property + Literacy + PollTax  + GubElection + SenElection + Turnout.Lag + (1|FIPS.County) + as.factor(Year), data=data.reg)
  ate.sims.reg[i] <- fixef(model)["get(zsims.cols[i])"]
  print(i)
}
save(ate.sims.reg, file="ate_sims_reg.Rdata")

# US ---------
data2 <- data2 %>% arrange(Year, State, FIPS.County)
set.seed(1234)
zUS <- matrix(NA, nrow=nrow(data2), ncol=nsims)
colnames(zUS) <- paste0("V", 1:nsims)
for (i in 1:nsims){
  zUS[,i] <- assign_country_year(data2,"Rainfall12")
  print(i)
}
data2 <- data2 %>% arrange(Year, State, FIPS.County)
data.US <- cbind(data2,zUS)
save(data.US, file="Data_US.Rdata")
#load("Data_US.Rdata")
zsims.cols <- colnames(data.US)[grep("V",colnames(data.US),fixed=TRUE)]
ate.sims.US <- rep(NA, nsims)
for (i in 1:nsims){
  model <- lmer(Turnout ~ get(zsims.cols[i]) + Snow  + ZPcntHSGrad + AdjIncome + PcntBlack + FarmsPerCap + Closing + Motor + Property + Literacy + PollTax  + GubElection + SenElection + Turnout.Lag + (1|FIPS.County) + as.factor(Year), data=data.US)
  ate.sims.US[i] <- fixef(model)["get(zsims.cols[i])"]
  print(i)
}
save(ate.sims.US, file="ate_sims_US.Rdata")

###################################################################
# Conduct RI procedure for Rainfall (index)
# COUNTY INDEX -------------------------------------------------------------------
data2 <- data2 %>% arrange(Year,State, FIPS.County)
set.seed(1234)
zindep.index <- matrix(NA, nrow=nrow(data2), ncol=nsims)
colnames(zindep.index) <- paste0("V", 1:nsims)
for (i in 1:nsims){
  zindep.index[,i] <- assign_independently_index(data2)
  print(i)
}
data.county.index <- cbind(data2, zindep.index)
#save(data.county.index, file="Data_county_index.Rdata")
#load("Data_county_index.Rdata")
zsims.cols <- colnames(data.county.index)[grep("V",colnames(data.county.index),fixed=TRUE)]
ate.sims.indep.index <- rep(NA, nsims)
for (i in 1:nsims){
  model <- lmer(Turnout ~ get(zsims.cols[i]) + Snow  + ZPcntHSGrad + AdjIncome + PcntBlack + FarmsPerCap + Closing + Motor + Property + Literacy + PollTax  + GubElection + SenElection + Turnout.Lag + (1|FIPS.County) + as.factor(Year), data=data.county.index)
  ate.sims.indep.index[i] <- fixef(model)["get(zsims.cols[i])"]
  print(i)
}
save(ate.sims.indep.index, file="ate_sims_indep_index.Rdata")

# CWA INDEX ---------
data2 <- data2 %>% arrange(Year,CWA_short, FIPS.County)
set.seed(1234)
zcwa.index <- matrix(NA, nrow=nrow(data2), ncol=nsims)
colnames(zcwa.index) <- paste0("V", 1:nsims)
for (i in 1:nsims){
  zcwa.index[,i] <- assign_cwa_year(data2,"Rainfall12.index")
  print(i)
}
data2 <- data2 %>% arrange(Year,CWA_short, FIPS.County)
data.cwa.index <- cbind(data2,zcwa.index)
#save(data.cwa.index, file="Data_cwa_index.Rdata")
#load("Data_cwa_index.Rdata")
zsims.cols <- colnames(data.cwa.index)[grep("V",colnames(data.cwa.index),fixed=TRUE)]
ate.sims.cwa.index <- rep(NA, nsims)
for (i in 1:nsims){
  model <- lmer(Turnout ~ get(zsims.cols[i]) + Snow  + ZPcntHSGrad + AdjIncome + PcntBlack + FarmsPerCap + Closing + Motor + Property + Literacy + PollTax  + GubElection + SenElection + Turnout.Lag
                + (1|FIPS.County) + as.factor(Year), data=data.cwa.index)
  ate.sims.cwa.index[i] <- fixef(model)["get(zsims.cols[i])"]
  print(i)
}
save(ate.sims.cwa.index, file="ate_sims_cwa_index.Rdata")

# STATE INDEX ---------
data2 <- data2 %>% arrange(Year,State, FIPS.County)
set.seed(1234)
zstate.index <- matrix(NA, nrow=nrow(data2), ncol=nsims)
colnames(zstate.index) <- paste0("V", 1:nsims)
for (i in 1:nsims){
  zstate.index[,i] <- assign_state_year(data2,"Rainfall12.index")
  print(i)
}
data2 <- data2 %>% arrange(Year,State, FIPS.County)
data.state.index <- cbind(data2,zstate.index)
#save(data.state.index, file="Data_state_index.Rdata")
#load("Data_state_index.Rdata")
zsims.cols <- colnames(data.state.index)[grep("V",colnames(data.state.index),fixed=TRUE)]
ate.sims.state.index <- rep(NA, nsims)
for (i in 1:nsims){
  model <- lmer(Turnout ~ get(zsims.cols[i]) + Snow  + ZPcntHSGrad + AdjIncome + PcntBlack + FarmsPerCap + Closing + Motor + Property + Literacy + PollTax  + GubElection + SenElection + Turnout.Lag + (1|FIPS.County) + as.factor(Year), data=data.state.index)
  ate.sims.state.index[i] <- fixef(model)["get(zsims.cols[i])"]
  print(i)
}
save(ate.sims.state.index, file="ate_sims_state_index.Rdata")

# REGION INDEX ---------
data2 <- data2 %>% arrange(Year,REG, FIPS.County)
set.seed(1234)
zreg.index <- matrix(NA, nrow=nrow(data2), ncol=nsims)
colnames(zreg.index) <- paste0("V", 1:nsims)
for (i in 1:nsims){
  zreg.index[,i] <- assign_reg_year(data2,"Rainfall12.index")
  print(i)
}
data2 <- data2 %>% arrange(Year,REG, FIPS.County)
data.reg.index <- cbind(data2,zreg.index)
#save(data.reg.index, file="Data_reg_index.Rdata")
#load("Data_reg_index.Rdata")
zsims.cols <- colnames(data.reg.index)[grep("V",colnames(data.reg.index),fixed=TRUE)]
ate.sims.reg.index <- rep(NA, nsims)
for (i in 1:nsims){
  model <- lmer(Turnout ~ get(zsims.cols[i]) + Snow  + ZPcntHSGrad + AdjIncome + PcntBlack + FarmsPerCap + Closing + Motor + Property + Literacy + PollTax  + GubElection + SenElection + Turnout.Lag + (1|FIPS.County) + as.factor(Year), data=data.reg.index)
  ate.sims.reg.index[i] <- fixef(model)["get(zsims.cols[i])"]
  print(i)
}
save(ate.sims.reg.index, file="ate_sims_reg_index.Rdata")

# US INDEX ---------
data2 <- data2 %>% arrange(Year, State, FIPS.County)
zUS.index <- matrix(NA, nrow=nrow(data2), ncol=nsims)
colnames(zUS.index) <- paste0("V", 1:nsims)
set.seed(1234)
for (i in 1:nsims){
  zUS.index[,i] <- assign_country_year(data2,"Rainfall12.index")
  print(i)
}
data2 <- data2 %>% arrange(Year, State, FIPS.County)
data.US.index <- cbind(data2,zUS.index)
#save(data.US.index, file="Data_US_index.Rdata")
#load("Data_US_index.Rdata")
zsims.cols <- colnames(data.US.index)[grep("V",colnames(data.US.index),fixed=TRUE)]
ate.sims.US.index <- rep(NA, nsims)
for (i in 1:nsims){
  model <- lmer(Turnout ~ get(zsims.cols[i]) + Snow  + ZPcntHSGrad + AdjIncome + PcntBlack + FarmsPerCap + Closing + Motor + Property + Literacy + PollTax  + GubElection + SenElection + Turnout.Lag
                + (1|FIPS.County) + as.factor(Year), data=data.US.index)
  ate.sims.US.index[i] <- fixef(model)["get(zsims.cols[i])"]
  print(i)
}
save(ate.sims.US.index, file="ate_sims_US_index.Rdata")

###################################################################
# Conduct RI procedure for Rainfall (SPI)
# COUNTY SPI ---------
data2 <- data2 %>% arrange(Year,State, FIPS.County)
set.seed(1234)
zindep.spi <- matrix(NA, nrow=nrow(data2), ncol=nsims)
colnames(zindep.spi) <- paste0("V", 1:nsims)
for (i in 1:nsims){
  zindep.spi[,i] <- assign_independently_spi(data2)
  print(i)
}
data.county.spi <- cbind(data2, zindep.spi)
#save(data.county.spi, file="Data_county_spi.Rdata")
#load("Data_county_spi.Rdata")
zsims.cols <- colnames(data.county.spi)[grep("V",colnames(data.county.spi),fixed=TRUE)]
ate.sims.indep.spi <- rep(NA, nsims)
for (i in 1:nsims){
  model <- lmer(Turnout ~ get(zsims.cols[i]) + Snow  + ZPcntHSGrad + AdjIncome + PcntBlack + FarmsPerCap + Closing + Motor + Property + Literacy + PollTax  + GubElection + SenElection + Turnout.Lag + (1|FIPS.County) + as.factor(Year), data=data.county.spi)
  ate.sims.indep.spi[i] <- fixef(model)["get(zsims.cols[i])"]
  print(i)
}
save(ate.sims.indep.spi, file="ate_sims_indep_spi.Rdata")

# CWA SPI ---------
data2 <- data2 %>% arrange(Year,CWA_short, FIPS.County)
set.seed(1234)
zcwa.spi <- matrix(NA, nrow=nrow(data2), ncol=nsims)
colnames(zcwa.spi) <- paste0("V", 1:nsims)
for (i in 1:nsims){
  zcwa.spi[,i] <- assign_cwa_year(data2,"Rainfall12.spi")
  print(i)
}
data2 <- data2 %>% arrange(Year,CWA_short, FIPS.County)
data.cwa.spi <- cbind(data2,zcwa.spi)
#save(data.cwa.spi, file="Data_cwa_spi.Rdata")
#load("Data_cwa_spi.Rdata")
zsims.cols <- colnames(data.cwa.spi)[grep("V",colnames(data.cwa.spi),fixed=TRUE)]
ate.sims.cwa.spi <- rep(NA, nsims)
for (i in 1:nsims){
  model <- lmer(Turnout ~ get(zsims.cols[i]) + Snow  + ZPcntHSGrad + AdjIncome + PcntBlack + FarmsPerCap + Closing + Motor + Property + Literacy + PollTax  + GubElection + SenElection + Turnout.Lag + (1|FIPS.County) + as.factor(Year), data=data.cwa.spi)
  ate.sims.cwa.spi[i] <- fixef(model)["get(zsims.cols[i])"]
  print(i)
}
save(ate.sims.cwa.spi, file="ate_sims_cwa_spi.Rdata")

# STATE SPI ---------
data2 <- data2 %>% arrange(Year,State, FIPS.County)
set.seed(1234)
zstate.spi <- matrix(NA, nrow=nrow(data2), ncol=nsims)
colnames(zstate.spi) <- paste0("V", 1:nsims)
for (i in 1:nsims){
  zstate.spi[,i] <- assign_state_year(data2,"Rainfall12.spi")
  print(i)
}
data2 <- data2 %>% arrange(Year,State, FIPS.County)
data.state.spi <- cbind(data2,zstate.spi)
#save(data.state.spi, file="Data_state_spi.Rdata")
#load("Data_state_spi.Rdata")
zsims.cols <- colnames(data.state.spi)[grep("V",colnames(data.state.spi),fixed=TRUE)]
ate.sims.state.spi <- rep(NA, nsims)
for (i in 1:nsims){
  model <- lmer(Turnout ~ get(zsims.cols[i]) + Snow  + ZPcntHSGrad + AdjIncome + PcntBlack + FarmsPerCap + Closing + Motor + Property + Literacy + PollTax  + GubElection + SenElection + Turnout.Lag + (1|FIPS.County) + as.factor(Year), data=data.state.spi)
  ate.sims.state.spi[i] <- fixef(model)["get(zsims.cols[i])"]
  print(i)
}
save(ate.sims.state.spi, file="ate_sims_state_spi.Rdata")

# REGION SPI ---------
data2 <- data2 %>% arrange(Year,REG, FIPS.County)
set.seed(1234)
zreg.spi <- matrix(NA, nrow=nrow(data2), ncol=nsims)
colnames(zreg.spi) <- paste0("V", 1:nsims)
for (i in 1:nsims){
  zreg.spi[,i] <- assign_reg_year(data2,"Rainfall12.spi")
  print(i)
}
data2 <- data2 %>% arrange(Year,REG, FIPS.County)
data.reg.spi <- cbind(data2,zreg.spi)
#save(data.reg.spi, file="Data_reg_spi.Rdata")
#load("Data_reg_spi.Rdata")
zsims.cols <- colnames(data.reg.spi)[grep("V",colnames(data.reg.spi),fixed=TRUE)]
ate.sims.reg.spi <- rep(NA, nsims)
for (i in 1:nsims){
  model <- lmer(Turnout ~ get(zsims.cols[i]) + Snow  + ZPcntHSGrad + AdjIncome + PcntBlack + FarmsPerCap + Closing + Motor + Property + Literacy + PollTax  + GubElection + SenElection + Turnout.Lag + (1|FIPS.County) + as.factor(Year), data=data.reg.spi)
  ate.sims.reg.spi[i] <- fixef(model)["get(zsims.cols[i])"]
  print(i)
}
save(ate.sims.reg.spi, file="ate_sims_reg_spi.Rdata")

# US SPI ---------
data2 <- data2 %>% arrange(Year, State, FIPS.County)
set.seed(1234)
zUS.spi <- matrix(NA, nrow=nrow(data2), ncol=nsims)
colnames(zUS.spi) <- paste0("V", 1:nsims)
for (i in 1:nsims){
  zUS.spi[,i] <- assign_country_year(data2,"Rainfall12.spi")
  print(i)
}
data2 <- data2 %>% arrange(Year, State, FIPS.County)
data.US.spi <- cbind(data2,zUS.spi)
save(data.US.spi, file="Data_US_spi.Rdata")
#load("Data_US_spi.Rdata")
zsims.cols <- colnames(data.US.spi)[grep("V",colnames(data.US.spi),fixed=TRUE)]
ate.sims.US.spi <- rep(NA, nsims)
for (i in 1:nsims){
  model <- lmer(Turnout ~ get(zsims.cols[i]) + Snow  + ZPcntHSGrad + AdjIncome + PcntBlack + FarmsPerCap + Closing + Motor + Property + Literacy + PollTax  + GubElection + SenElection + Turnout.Lag + (1|FIPS.County) + as.factor(Year), data=data.US.spi)
  ate.sims.US.spi[i] <- fixef(model)["get(zsims.cols[i])"]
  print(i)
}
save(ate.sims.US.spi, file="ate_sims_US_spi.Rdata")



####################################
# 3. Output for Appendix Tables 2 and 3 ------

# Calculate estimated ATE under sharp null of no effect -- FE Model for Inches
# NOTE: Requires creation of "Data_US.Rdata" above. 

#load("Data_US.Rdata") # Created earlier in the code
zsims.cols <- colnames(data.US)[grep("V",colnames(data.US),fixed=TRUE)]
ate.sims.US.FE <- rep(NA, nsims)
for (i in 1:nsims){
  model <- felm(Turnout ~ get(zsims.cols[i]) + Snow + ZPcntHSGrad + AdjIncome + PcntBlack + FarmsPerCap + Closing + Motor + Property + Literacy + PollTax + GubElection + SenElection + Turnout.Lag | FIPS.County + Year, data=data.US)
  ate.sims.US.FE[i] <- model$coefficients[1]
  print(i)
}
save(ate.sims.US.FE, file="ate_sims_US_FE.Rdata")

# Calculate estimated ATE under sharp null of no effect -- FE Model for SPI
#load("Data_US_spi.Rdata") # Created earlier in the code
zsims.cols <- colnames(data.US.spi)[grep("V",colnames(data.US.spi),fixed=TRUE)]
ate.sims.US.spi.FE <- rep(NA, nsims)
for (i in 1:nsims){
  model <- felm(Turnout ~ get(zsims.cols[i]) + Snow + ZPcntHSGrad + AdjIncome + PcntBlack + FarmsPerCap + Closing + Motor + Property + Literacy + PollTax + GubElection + SenElection + Turnout.Lag | FIPS.County + Year, data=data.US.spi)
  ate.sims.US.spi.FE[i] <- model$coefficients[1]
  print(i)
}
save(ate.sims.US.spi.FE, file="ate_sims_US_spi_FE.Rdata")

###################################################################
# 4. Output for Appendix Table 4 ----- 
# Robustness without post-2000 data 

# Inches Pre-2001
data2 <- data2 %>% arrange(Year, State, FIPS.County)
data2.pre2001 <- subset(data2, Year<2001)
set.seed(1234)
zUS.pre2001 <- matrix(NA, nrow=nrow(data2.pre2001), ncol=nsims)
colnames(zUS.pre2001) <- paste0("V", 1:nsims)
for (i in 1:nsims){
  zUS.pre2001[,i] <- assign_country_year(data2.pre2001,"Rainfall12")
  print(i)
}
data2.pre2001 <- data2.pre2001 %>% arrange(Year, State, FIPS.County)
data.US.pre2001 <- cbind(data2.pre2001,zUS.pre2001)
#save(data.US.pre2001, file="Data_US_pre2001.Rdata")
#load("Data_US_pre2001.Rdata")
zsims.cols <- colnames(data.US.pre2001)[grep("V",colnames(data.US.pre2001),fixed=TRUE)]
ate.sims.US.pre2001 <- rep(NA, nsims)
for (i in 1:nsims){
  model <- lmer(Turnout ~ get(zsims.cols[i]) + Snow  + ZPcntHSGrad + AdjIncome + PcntBlack + FarmsPerCap + Closing + Motor + Property + Literacy + PollTax  + GubElection + SenElection + Turnout.Lag + (1|FIPS.County) + as.factor(Year), data=data.US.pre2001)
  ate.sims.US.pre2001[i] <- fixef(model)["get(zsims.cols[i])"]
  print(i)
}
save(ate.sims.US.pre2001, file="ate_sims_US_pre2001.Rdata")

# Index Pre-2001
data2 <- data2 %>% arrange(Year, State, FIPS.County)
data2.pre2001 <- subset(data2, Year<2001)
set.seed(1234)
zUS.index.pre2001 <- matrix(NA, nrow=nrow(data2.pre2001), ncol=nsims)
colnames(zUS.index.pre2001) <- paste0("V", 1:nsims)
for (i in 1:nsims){
  zUS.index.pre2001[,i] <- assign_country_year(data2.pre2001,"Rainfall12.index")
  print(i)
}
data2.pre2001 <- data2.pre2001 %>% arrange(Year, State, FIPS.County)
data.US.index.pre2001 <- cbind(data2.pre2001,zUS.index.pre2001)
#save(data.US.index.pre2001, file="Data_US_index_pre2001.Rdata")
#load("Data_US_index_pre2001.Rdata")
zsims.cols <- colnames(data.US.index.pre2001)[grep("V",colnames(data.US.index.pre2001),fixed=TRUE)]
ate.sims.US.index.pre2001 <- rep(NA, nsims)
for (i in 1:nsims){
  model <- lmer(Turnout ~ get(zsims.cols[i]) + Snow  + ZPcntHSGrad + AdjIncome + PcntBlack + FarmsPerCap + Closing + Motor + Property + Literacy + PollTax  + GubElection + SenElection + Turnout.Lag + (1|FIPS.County) + as.factor(Year), data=data.US.index.pre2001)
  ate.sims.US.index.pre2001[i] <- fixef(model)["get(zsims.cols[i])"]
  print(i)
}
save(ate.sims.US.index.pre2001, file="ate_sims_US_index_pre2001.Rdata")

# SPI Pre-2001
data2 <- data2 %>% arrange(Year, State, FIPS.County)
data2.pre2001 <- subset(data2, Year<2001)
set.seed(1234)
zUS.spi.pre2001 <- matrix(NA, nrow=nrow(data2.pre2001), ncol=nsims)
colnames(zUS.spi.pre2001) <- paste0("V", 1:nsims)
for (i in 1:nsims){
  zUS.spi.pre2001[,i] <- assign_country_year(data2.pre2001,"Rainfall12.spi")
  print(i)
}
data2.pre2001 <- data2.pre2001 %>% arrange(Year, State, FIPS.County)
data.US.spi.pre2001 <- cbind(data2.pre2001,zUS.spi.pre2001)
#save(data.US.spi.pre2001, file="Data_US_spi_pre2001.Rdata")
#load("Data_US_spi_pre2001.Rdata")
zsims.cols <- colnames(data.US.spi.pre2001)[grep("V",colnames(data.US.spi.pre2001),fixed=TRUE)]
ate.sims.US.spi.pre2001 <- rep(NA, nsims)
for (i in 1:nsims){
  model <- lmer(Turnout ~ get(zsims.cols[i]) + Snow  + ZPcntHSGrad + AdjIncome + PcntBlack + FarmsPerCap + Closing + Motor + Property + Literacy + PollTax  + GubElection + SenElection + Turnout.Lag + (1|FIPS.County) + as.factor(Year), data=data.US.spi.pre2001)
  ate.sims.US.spi.pre2001[i] <- fixef(model)["get(zsims.cols[i])"]
  print(i)
}
save(ate.sims.US.spi.pre2001, file="ate_sims_US_spi_pre2001.Rdata")

#################################################################
# 5. Save list of ATEs

ate <- list(ate.sims.US=ate.sims.US, ate.sims.US.index=ate.sims.US.index, ate.sims.US.spi=ate.sims.US.spi,
            ate.sims.reg=ate.sims.reg, ate.sims.reg.index=ate.sims.reg.index, ate.sims.reg.spi=ate.sims.reg.spi,
            ate.sims.state=ate.sims.state, ate.sims.state.index=ate.sims.state.index, ate.sims.state.spi=ate.sims.state.spi,
            ate.sims.cwa=ate.sims.cwa, ate.sims.cwa.index=ate.sims.cwa.index, ate.sims.cwa.spi=ate.sims.cwa.spi,
            ate.sims.indep=ate.sims.indep, ate.sims.indep.index=ate.sims.indep.index, ate.sims.indep.spi=ate.sims.indep.spi,
            ate.sims.US.FE=ate.sims.US.FE, ate.sims.US.spi.FE=ate.sims.US.spi.FE,
            ate.sims.US.pre2001=ate.sims.US.pre2001, ate.sims.US.index.pre2001=ate.sims.US.index.pre2001, ate.sims.US.spi.pre2001=ate.sims.US.spi.pre2001)
save(ate, file="ate_sims_rep.Rdata")
#save(ate, file="ate_sims.Rdata") # If uncomment, will save over the downloaded file.

###################################################################
# 6. Output for Appendix Table 7 and Figure 1 -------

# NOTE: Time-consuming procedure. 
# Precision of US-year Rainfall (inch) estimates under different numbers of simulations
# Requires creation of "Data_US.Rdata" above using nsims = 1000. 

gomez.model <- lmer(Turnout ~ Rainfall12 + Snow  + ZPcntHSGrad + AdjIncome + PcntBlack + FarmsPerCap + Closing + Motor + Property + Literacy + PollTax  + GubElection + SenElection + Turnout.Lag + (1|FIPS.County) + as.factor(Year), data=data2)
ate.obs <- fixef(gomez.model)["Rainfall12"]

#load("Data_US.Rdata") # Created using nsims=1000.
nsims.iter=c(50,100,200,500) # Number of permutations used. 
# Columns of permutations are randomly sampled with replacement from the 1000 permutations created above.
# Ie. if nsims.iter=50, then 50 permutations are used to estimate 50 ATEs. These 50 ATEs are used to calculate the p-value.
reps = 50 # The calculation of the p-value is repeated 50 times.

pvals.by.nsims <- matrix(NA, nrow=reps, ncol=length(nsims.iter))
colnames(pvals.by.nsims) <- paste0("iter",nsims.iter)
seeds <- matrix(1:200, nrow=reps, ncol=length(nsims.iter))

for (n in 1:length(nsims.iter)){
  niter <- nsims.iter[n]
  for (r in 1:reps){
    set.seed(seeds[r,n])
    zsims.cols <- sample(colnames(data.US)[grep("V",colnames(data.US),fixed=TRUE)],size = niter,replace = T)
    ate.sims.US.iter <- rep(NA, niter)
    for (i in 1:niter){
      model <- lmer(Turnout ~ get(zsims.cols[i]) + Snow  + ZPcntHSGrad + AdjIncome + PcntBlack + FarmsPerCap + Closing + Motor + Property + Literacy + PollTax  + GubElection + SenElection + Turnout.Lag + (1|FIPS.County) + as.factor(Year), data=data.US)
      ate.sims.US.iter[i] <- fixef(model)["get(zsims.cols[i])"]
    }
    pvals.by.nsims[r,n] <- sum(abs(ate.sims.US.iter) >= abs(ate.obs))/length(ate.sims.US.iter)
  }
}
save(pvals.by.nsims, file="pvals_by_nsims_rep.Rdata") 
#save(pvals.by.nsims, file="pvals_by_nsims.Rdata") # If uncomment, will save over the downloaded file.
