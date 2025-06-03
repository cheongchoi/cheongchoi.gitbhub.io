## Reproduction script for Menon, Aravind and Daniel J. Mallinson. 
## "Policy Diffusion Speed: A Replication Study Using the State Policy Innovation and Diffusion Database"
## Political Studies Review

rm(list = ls(all = TRUE))

########################################################################################
## The first portion of this script was used to extract adoption data from SPID
## and produce Mallinson 2016 and Nicholson-Crotty 2009 speed measures.
########################################################################################

########################################################################################
######################### Create speed measures ########################################
########################################################################################

#install.packages(c("NetworkInference", "eha", "gplots", "lmtest", "survival", "dplyr", "psych", "survey))

library(foreign)
library(NetworkInference)
library(eha)
library(gplots)
library(lmtest)
library(survival)
library(dplyr)
library(psych)
library(survey)

# Load SPID Policy Adoption data
#Creates two data.frame objects:
#1. "policies" contains adoption events
#2 "policies_metadata" contains additional info

# setwd("E:/Diffusion")
data("policies") 

head(policies)
head(policies_metadata)

## Correct Nevada gambling error
policies_metadata$first_year[policies_metadata$policy=="gambling_casino"] <- 1931

## Add major topic numbers
policies_metadata$majornum[policies_metadata$majortopic=="Macroeconomics"] <- 1
policies_metadata$majornum[policies_metadata$majortopic=="Civil Rights"] <- 2
policies_metadata$majornum[policies_metadata$majortopic=="Health"] <- 3
policies_metadata$majornum[policies_metadata$majortopic=="Agriculture"] <- 4
policies_metadata$majornum[policies_metadata$majortopic=="Labor"] <- 5
policies_metadata$majornum[policies_metadata$majortopic=="Education"] <- 6
policies_metadata$majornum[policies_metadata$majortopic=="Environment"] <- 7
policies_metadata$majornum[policies_metadata$majortopic=="Energy"] <- 8
policies_metadata$majornum[policies_metadata$majortopic=="Immigration"] <- 9
policies_metadata$majornum[policies_metadata$majortopic=="Transportation"] <- 10
policies_metadata$majornum[policies_metadata$majortopic=="Law and Crime"] <- 12
policies_metadata$majornum[policies_metadata$majortopic=="Social Welfare"] <- 13
policies_metadata$majornum[policies_metadata$majortopic=="Housing"] <- 14
policies_metadata$majornum[policies_metadata$majortopic=="Domestic Commerce"] <- 15
policies_metadata$majornum[policies_metadata$majortopic=="Defense"] <- 16
policies_metadata$majornum[policies_metadata$majortopic=="Technology"] <- 17
policies_metadata$majornum[policies_metadata$majortopic=="Foreign Trade"] <- 18
policies_metadata$majornum[policies_metadata$majortopic=="International Affairs"] <- 19
policies_metadata$majornum[policies_metadata$majortopic=="Government Operations"] <- 20
policies_metadata$majornum[policies_metadata$majortopic=="Public Lands"] <- 21


#Read in state abbreviations
states <- read.csv("states.csv")

all.states <- as.character(states$state) #create vector of state abbreviations

## Drop policies that were all adopted in the same year
#Meta data
delete <- policies_metadata[which(policies_metadata$first_year==policies_metadata$last_year),]
deleterow <- as.numeric(rownames(delete))
policies_metadata <- policies_metadata[-deleterow,]
#check, should be 0 rows
nrow(policies_metadata[which(policies_metadata$first_year==policies_metadata$last_year),])

#Adoption data
delete2 <- policies[policies$policy %in% delete$policy,]
deleterow2 <- as.numeric(rownames(delete2))
policies <- policies[-deleterow2,]
#check, should be 0 rows
nrow(policies[which(policies$first_year==policies$last_year),])

all.policies <- as.character(unique(policies$policy))

for(i in 1:length(all.policies)){
  usepolicy <- policies[which(policies$policy==all.policies[i]),]
  usepolicy$adopt <- 1
  usepolicy <- merge(usepolicy, states, by="statenam", all.y=TRUE)
  usepolicy$policy <- all.policies[i]
  usepolicy$adopt_year[is.na(usepolicy$adopt_year)] <- max(usepolicy$adopt_year, na.rm=TRUE)
  usepolicy$adopt[is.na(usepolicy$adopt)] <- 0 
  if(i == 1){
    long <- usepolicy
  }else{
    long <- rbind(long, usepolicy)
  }
}

long <- merge(long, policies_metadata, by="policy")

long$duration <- long$adopt_year - long$first_year + 1 #must start at 1 for count for survival modelling

############## Calculate N-C 2009 Indicator ###################

#The dependent variable used here is a dichotomous measure coded 1 for 
## those policies where over 50% of adopters do so within the first third 
## of the temporal distribution and 0 otherwise. (Nicholson-Crotty, Sean. 2009.
## "The Politics of Diffusion: Public Policy in the American States. 
##Journal of Politics. 71(1): 192-205). Page 197.

## Instead of determining the trail off in adoptions,
## I use the entire span of adoptions to calculate Nicholson-Crotty's 
## speed measure. Essentially, I measure the total span of time for 
## observed adoptions and calculate one-third of it. I then measure 
## the percentage of adopters that did so before that one-third cut off. 
## If more than 50 percent adopted in that first third of the adoption time, 
## the policy is marked as a rapid adoption. 

for(i in 1:length(all.policies)){
  usepolicy <- long[which(long$policy==all.policies[i]),]
  if(max(usepolicy$duration) > 2){
    cut <- round(max(usepolicy$duration)*.333333333, 0)
    tot.adopt <- sum(usepolicy$adopt[usepolicy$duration<=cut])/sum(usepolicy$adopt)
    usepolicy$percent.below.cut <- tot.adopt
    if(tot.adopt>.5){
      usepolicy$nc_fast <- 1
    }else{usepolicy$nc_fast <- 0}
  }else{
    usepolicy$nc_fast <- 1
    usepolicy$percent.below.cut <- 1
  }
  if(i==1){
    long2 <- usepolicy
  }else{
    long2 <- rbind(long2, usepolicy)
  }
}

## Drop policies whose duration = 1
long2[long2$first_year==long2$last_year] <- NULL

### Add salience and complexity measures

hearings <- read.csv("US-Legislative-congressional_hearings-18.2.csv")

hearings <- hearings[c("id", "year", "majortopic")] #keep needed variables
names(hearings)[2] <- "adopt_year"
names(hearings)[3] <- "majornum"

hearings$counter <- 1 #Add a counter for calculating percent coverage

for(i in 1946:2016){
  r <- hearings[which(hearings$adopt_year==i),]
  majors <- unique(r$majornum)
  #subs <- unique(r$subtopic)
  total <- nrow(r)
  for(j in 1:length(majors)){
    m <- r[which(r$majornum==majors[j]),]
    m$congress_majortopic <- nrow(m)/total
    m$counter <- NULL
    if(j==1){
      hearings.major <- m[1,]
    }else{
      hearings.major <- rbind(hearings.major,m[1,])
    }
  }
  if(i == 1946){
    major.pct <- hearings.major
  }else{
    major.pct <- rbind(major.pct, hearings.major)
  }
}

major.pct$subtopic <- major.pct$id <- NULL #delete unnecessary columns
nrow(major.pct) #1424

long2 <- merge(x=long2, y=major.pct, by=c("adopt_year", "majornum"), all.x=TRUE)
long2$congress_majortopic <- long2$congress_majortopic*100
long2$congress_majortopic[is.na(long2$congress_majortopic)] <- 0
nrow(long2) #34100

## Add NYT Coverage from Policy Agendas Project
nyt <- read.csv("US-Media-new_york_times_index-15_1.csv")
nyt <- nyt[which(nyt$majortopic!=-9),] #remove observations with no majortopic
nyt <- nyt[which(nyt$majortopic!=33),] #remove error major topic code

nyt <- nyt[c("id", "year", "majortopic")] #keep needed variables
names(nyt)[2:3] <- c("adopt_year", "majornum")

nyt$counter <- 1 #Add a counter for calculating percent coverage

for(i in 1946:2014){
  t <- nyt[which(nyt$adopt_year==i),]
  majors <- unique(t$majornum)
  total <- nrow(t)
  for(j in 1:length(majors)){
    n <- t[which(t$majornum==majors[j]),]
    n$nyt <- nrow(n)/total
    n$counter <- NULL
    if(j==1){
      nyt.major <- n[1,]
    }else{
      nyt.major <- rbind(nyt.major,n[1,])
    }
  }
  if(i == 1946){
    nyt.pct <- nyt.major
  }else{
    nyt.pct <- rbind(nyt.pct, nyt.major)
  }
}

nyt.pct$nyt <- nyt.pct$nyt*100
nrow(nyt.pct) #1858

nyt.pct$id <- NULL #delete unnecessary columns

#merge in New York Times coverage percentages
long2 <- merge(x=long2, y=nyt.pct, by=c("adopt_year", "majornum"), all.x=TRUE)

# Replace NAs with 0s (NA produced when a given major topic is not covered in the NYT in a year
long2$nyt[is.na(long2$nyt)] <- 0
nrow(long2) #34100

## Add Most Important Problem (Gallup)

mip <- read.csv("US-Public-Gallups_Most_Important_Problem-18.1.csv") #load data
mip$id <- mip$Congress <- NULL #drop unnecessary variables
colnames(mip) <- c("adopt_year", "mip", "majornum")

long2 <- merge(x=long2, y=mip, by=c("adopt_year", "majornum"), all.x=TRUE)
long2$mip <- long2$mip*100
long2$mip[is.na(long2$mip)] <- 0

nrow(long2) #34100

## Add complexity based on Nicholson-Crotty (2009)
long2$complexity_topic <- 0
long2$complexity_topic[long2$majornum==1] <- 1
long2$complexity_topic[long2$majornum==3] <- 1
long2$complexity_topic[long2$majornum==7] <- 1
long2$complexity_topic[long2$majornum==8] <- 1
long2$complexity_topic[long2$majornum==18] <- 1
long2$complexity_topic[long2$majornum==21] <- 1

#check
unique(long2[c("complexity_topic", "majortopic")])

nrow(long2) #34100

###############################################################
############# Calculate speed measures for ####################
#############   cross-sectional analysis   ####################
###############################################################

################# weibull #####################

for(i in 1:length(all.policies)){
  usepolicy <- long2[which(long2$policy==all.policies[i]),]
  output <- survreg(Surv(time=usepolicy$duration, event=usepolicy$adopt, type="right")~1, dist="weibull", data=usepolicy)
  const <- as.numeric(coef(output)[1])
  logp <- log(1/(output$scale))
  se <- as.matrix(sqrt(diag(output$var)))
  se.const <- as.numeric(se[1,1])
  se.logp <- as.numeric(se[2,1])
  keep <- as.data.frame(cbind(all.policies[i], const, se.const, logp, se.logp))
  names(keep) <- c("policy", "weib.speed", "weib.speed.se", "weib.scale", "weib.scale.se")
  if(i == 1){
    weib <- keep
  }else{
    weib <- rbind(weib, keep)
  }
}

speeddata <- weib

################# exponential #####################

for(i in 1:length(all.policies)){
  usepolicy <- long2[which(long2$policy==all.policies[i]),]
  output <- survreg(Surv(time=usepolicy$duration, event=usepolicy$adopt, type="right")~1, dist="exponential", data=usepolicy)
  const <- as.numeric(coef(output)[1])
  se.const <- as.numeric(sqrt(diag(output$var)))
  keep <- as.data.frame(cbind(all.policies[i], const, se.const))
  names(keep) <- c("policy", "exp.speed", "exp.speed.se")
  if(i == 1){
    exp <- keep
  }else{
    exp <- rbind(exp, keep)
  }
}

speeddata <- merge(speeddata, exp, by="policy")

################# lognormal #####################

for(i in 1:length(all.policies)){
  usepolicy <- long2[which(long2$policy==all.policies[i]),]
  output <- survreg(Surv(time=usepolicy$duration, event=usepolicy$adopt, type="right")~1, dist="lognormal", data=usepolicy)
  const <- as.numeric(coef(output)[1])
  logp <- log(1/(output$scale))
  se <- as.matrix(sqrt(diag(output$var)))
  se.const <- as.numeric(se[1,1])
  se.logp <- as.numeric(se[2,1])
  keep <- as.data.frame(cbind(all.policies[i], const, se.const, logp, se.logp))
  names(keep) <- c("policy", "lognorm.speed", "lognorm.speed.se", "lognorm.scale", "lognorm.scale.se")
  if(i == 1){
    lognorm <- keep
  }else{
    lognorm <- rbind(lognorm, keep)
  }
}

speeddata <- merge(speeddata, lognorm, by="policy")

################# loglogistic #####################

for(i in 1:length(all.policies)){
  usepolicy <- long2[which(long2$policy==all.policies[i]),]
  output <- survreg(Surv(time=usepolicy$duration, event=usepolicy$adopt, type="right")~1, dist="loglogistic", data=usepolicy)
  const <- as.numeric(coef(output)[1])
  logp <- log(1/(output$scale))
  se <- as.matrix(sqrt(diag(output$var)))
  se.const <- as.numeric(se[1,1])
  se.logp <- as.numeric(se[2,1])
  keep <- as.data.frame(cbind(all.policies[i], const, se.const, logp, se.logp))
  names(keep) <- c("policy", "loglog.speed", "loglog.speed.se", "loglog.scale", "loglog.scale.se")
  if(i == 1){
    loglog <- keep
  }else{
    loglog <- rbind(loglog, keep)
  }
}

speeddata <- merge(speeddata, loglog, by="policy")

speeddata <- cbind(speeddata$policy, mutate_all(speeddata[2:ncol(speeddata)], function(x) as.numeric(as.character(x))))
names(speeddata)[1] <- "policy"

speeddata$policy <- as.character(speeddata$policy) #Convert factor to character

#Export Raw speed coefficients
write.csv(speeddata, file="all_raw_speed_spid_2021-02-23.csv")

##Rescale speed coefficient to range from zero to one
#First, subtract all speed values from 1 so they range from slowest to fastest

weib.rescale.speed <- 1-speeddata$weib.speed
weib.rescale.speed <- as.data.frame((weib.rescale.speed - min(weib.rescale.speed))/(max(weib.rescale.speed) - min(weib.rescale.speed)))
names(weib.rescale.speed) <- "weib.rescale.speed"

weib.rescale.robust <- as.data.frame(((1-speeddata$weib.speed) - median(1-speeddata$weib.speed))/(quantile(1-speeddata$weib.speed)[4]-quantile(1-speeddata$weib.speed)[2]))
names(weib.rescale.robust) <- "weib.rescale.robust"

exp.rescale.speed <- 1-speeddata$exp.speed
exp.rescale.speed <- as.data.frame((exp.rescale.speed - min(exp.rescale.speed))/(max(exp.rescale.speed) - min(exp.rescale.speed)))
names(exp.rescale.speed) <- "exp.rescale.speed"

lognorm.rescale.speed <- 1-speeddata$lognorm.speed
lognorm.rescale.speed <- as.data.frame((lognorm.rescale.speed - min(lognorm.rescale.speed))/(max(lognorm.rescale.speed) - min(lognorm.rescale.speed)))
names(lognorm.rescale.speed) <- "lognorm.rescale.speed"

loglog.rescale.speed <- 1-speeddata$loglog.speed
loglog.rescale.speed <- as.data.frame((loglog.rescale.speed - min(loglog.rescale.speed))/(max(loglog.rescale.speed) - min(loglog.rescale.speed)))
names(loglog.rescale.speed) <- "loglog.rescale.speed"

speeddata <- cbind(speeddata, weib.rescale.speed, weib.rescale.robust, exp.rescale.speed, lognorm.rescale.speed, loglog.rescale.speed)

##Add Nicholson-Crotty 2009 Speed Measure

nc_fast <- long2[c("policy", "nc_fast", "percent.below.cut")]
nc_fast <- distinct(nc_fast)

speeddata <- merge(speeddata, nc_fast, by="policy", all.y=FALSE)

## Merge in policy details

speeddata <- merge(speeddata, policies_metadata, all.y=FALSE)

## Tag Mallinson 2016 Policies

mallinson.data <- read.csv("mallinson_2016_speedanalysis.csv")
mallinson.data <- mallinson.data[c("policy", "first", "last", "totalyears", "totalstates", "weib.speed", "weib.rescale.speed")]
names(mallinson.data) <- c("policy", "mfirst", "mlast", "mtotalyears", "mtotalstates", "mweib.speed", "mweib.rescale.speed")
mallinson.data$mallinson16 <- 1

speeddata <- merge(speeddata, mallinson.data, by=c("policy"), all.x=TRUE)

speeddata$mallinson16[is.na(speeddata$mallinson16)] <- 0

mallinson_ind <- mallinson.data[c("policy", "mallinson16")]
long2 <- merge(long2, mallinson_ind, by="policy", all.x=TRUE)
long2$mallinson16[is.na(long2$mallinson16)] <- 0

##################### Export speed coefficients and adoptions #######################

write.csv(speeddata, "speedanalysis_spid_2021-02-23.csv", row.names=FALSE) # Export speed coefficients to csv

################ Export Pooled Weibull Data #######################

write.csv(long2, "pooled_weibull_spid_2021-02-23.csv")

#####################################################################################
######################## Analysis for manuscript ####################################
#####################################################################################

################### Import Speed Coefficients and covariates ########################

speeddata <- read.csv("speedanalysis_spid_2021-02-23.csv") #This dataset includes speed measures and covariates

## Remove antimis (first adopt =1691)
speeddata <- speeddata[which(speeddata$policy!="antimis"),]

#################################################################################
######################### Initial Analysis of Speed Measure #####################
#################################################################################

### Comparison of Mallinson and SPID speed measures

mallcheck <- speeddata[which(speeddata$mallinson16==1),]

cor(speeddata$weib.speed[speeddata$mallinson16==1],speeddata$mweib.speed[speeddata$mallinson16==1])
#.93 correlation
cor(speeddata$weib.rescale.speed[speeddata$mallinson16==1], speeddata$mweib.rescale.speed[speeddata$mallinson16==1])
#.93 correlation

mallcheck$weib.rescale.diff <- mallcheck$weib.rescale.speed - mallcheck$mweib.rescale.speed
mean(mallcheck$weib.rescale.diff)

mallyears <- mallcheck[c("policy", "first_year", "last_year", "adopt_count", "mfirst", "mlast", "mtotalstates", "weib.speed", "mweib.speed", "weib.rescale.speed", "mweib.rescale.speed")]

#Create Robust measure for Mallinson data
mweib.rescale.robust <- as.data.frame(((1-mallcheck$weib.speed) - median(1-mallcheck$weib.speed))/(quantile(1-mallcheck$weib.speed)[4]-quantile(1-mallcheck$weib.speed)[2]))
names(mweib.rescale.robust) <- "mweib.rescale.robust"
mallcheck <- cbind(mallcheck,mweib.rescale.robust)

merge <- mallcheck[c("policy", "mweib.rescale.robust")]

speeddata <- merge(speeddata, merge, all.x=TRUE)

speeddata$weib.robust.diff[speeddata$mallinson16==1] <- mallcheck$weib.rescale.robust[speeddata$mallinson16==1] - mallcheck$mweib.rescale.robust[speeddata$mallinson16==1]
mean(speeddata$weib.robust.diff, na.rm=TRUE)

### Correlations between speed measures ###
#All are compared with the chosen Weibull

cor.test(speeddata$weib.rescale.speed, speeddata$exp.rescale.speed) # r(680) = 0.91, p < 0.001
cor.test(speeddata$weib.rescale.speed, speeddata$lognorm.rescale.speed) # r(680) = 0.98, p < 0.001
cor.test(speeddata$weib.rescale.speed, speeddata$loglog.rescale.speed) # r(680) = 0.99, p < 0.001

###### Plot comparison of Mallinson Rescaled and NC Measures (Figure 1)  #######

tiff("figure1new.tiff", width=11, height=8, res=600, units="in")
plot.new()
par(mar=c(5,5,1,2), fig=c(0,0.5,0,1))
plot.window(xlim=c(0,1), ylim=c(-.05,1))

fast <- speeddata[which(speeddata$nc_fast==1 & speeddata$mallinson16==1),]
slow <- speeddata[which(speeddata$nc_fast==0 & speeddata$mallinson16==1),]

fast_correct <- fast[which(fast$mweib.rescale.speed>=0.5),]
fast_incorrect <- fast[which(fast$mweib.rescale.speed<0.5),]
slow_correct <- slow[which(slow$mweib.rescale.speed<=0.5),]
slow_incorrect <- slow[which(slow$mweib.rescale.speed>0.5),]

points(x=fast_correct$percent.below.cut, y=fast_correct$mweib.rescale.speed, col="black", pch=19)
points(x=slow_correct$percent.below.cut, y=slow_correct$mweib.rescale.speed, col="black", pch=19)
points(x=fast_incorrect$percent.below.cut, y=fast_incorrect$mweib.rescale.speed, col="red", pch=17)
points(x=slow_incorrect$percent.below.cut, y=slow_incorrect$mweib.rescale.speed, col="red", pch=17)
axis(1, at=seq(0,1,.1), labels=c(0,10,20,30,40,50,60,70,80,90,100))
axis(2, at=c(0,0.1,0.2,0.3,0.4,0.5,0.6,0.7,0.8,0.9,1.0), labels=c(0,0.1,0.2,0.3,0.4,0.5,0.6,0.7,0.8,0.9,1.0))
segments(.5,0,.5,1, lty=1, col="black")
segments(0,0.5,1,0.5, lty=1, col="black")
text(x=.25, y=-.05, c("Slow Adoption (0)"), col="black")
text(x=.75, y=-.05, c("Fast Adoption (1)"), col="black")
title(main="", xlab="Percentage of Adoptions Below 33% Cut Off", ylab="Speed Measure", sub="(a) Mallinson 2016")

par(mar=c(5,5,1,2), fig=c(0.5,1,0,1))
plot.window(xlim=c(0,1), ylim=c(-.05,1))

fast <- speeddata[which(speeddata$nc_fast==1),]
slow <- speeddata[which(speeddata$nc_fast==0),]

fast_correct <- fast[which(fast$weib.rescale.speed>=0.5),]
fast_incorrect <- fast[which(fast$weib.rescale.speed<0.5),]
slow_correct <- slow[which(slow$weib.rescale.speed<=0.5),]
slow_incorrect <- slow[which(slow$weib.rescale.speed>0.5),]

points(x=fast_correct$percent.below.cut, y=fast_correct$weib.rescale.speed, col="black", pch=19)
points(x=slow_correct$percent.below.cut, y=slow_correct$weib.rescale.speed, col="black", pch=19)
points(x=fast_incorrect$percent.below.cut, y=fast_incorrect$weib.rescale.speed, col="red", pch=17)
points(x=slow_incorrect$percent.below.cut, y=slow_incorrect$weib.rescale.speed, col="red", pch=17)
axis(1, at=seq(0,1,.1), labels=c(0,10,20,30,40,50,60,70,80,90,100))
axis(2, at=c(0,0.1,0.2,0.3,0.4,0.5,0.6,0.7,0.8,0.9,1.0), labels=c(0,0.1,0.2,0.3,0.4,0.5,0.6,0.7,0.8,0.9,1.0))
segments(.5,0,.5,1, lty=1, col="black")
segments(0,0.5,1,0.5, lty=1, col="black")
text(x=.25, y=-.05, c("Slow Adoption (0)"), col="black")
text(x=.75, y=-.05, c("Fast Adoption (1)"), col="black")
title(main="", xlab="Percentage of Adoptions Below 33% Cut Off", ylab="Speed Measure", sub="(b) SPID")



dev.off()

###### Plot comparison of Mallinson Robust Rescaled and NC Measures (Figure 2)  #######
tiff("figure2new.tiff", width=11, height=8, res=600, units="in")
plot.new()
par(mar=c(5,5,1,2), fig=c(0,0.5,0,1))
plot.window(xlim=c(0,1), ylim=c(-5,2.5))

fast <- speeddata[which(speeddata$nc_fast==1 & speeddata$mallinson16==1),]
slow <- speeddata[which(speeddata$nc_fast==0 & speeddata$mallinson16==1),]

fast_correct <- fast[which(fast$mweib.rescale.robust>=0),]
fast_incorrect <- fast[which(fast$mweib.rescale.robust<0),]
slow_correct <- slow[which(slow$mweib.rescale.robust<=0),]
slow_incorrect <- slow[which(slow$mweib.rescale.robust>0),]

points(x=fast_correct$percent.below.cut, y=fast_correct$mweib.rescale.robust, col="black", pch=19)
points(x=slow_correct$percent.below.cut, y=slow_correct$mweib.rescale.robust, col="black", pch=19)
points(x=fast_incorrect$percent.below.cut, y=fast_incorrect$mweib.rescale.robust, col="red", pch=17)
points(x=slow_incorrect$percent.below.cut, y=slow_incorrect$mweib.rescale.robust, col="red", pch=17)
axis(1, at=seq(0,1,.1), labels=c(0,10,20,30,40,50,60,70,80,90,100))
axis(2, at=seq(-5,2,1), labels=seq(-5,2,1), las=2)
segments(.5,-5,.5,2.5, lty=1, col="black")
segments(0,0,1,0, lty=1, col="black")
text(x=.25, y=-5, c("Slow Adoption (0)"), col="black")
text(x=.75, y=-5, c("Fast Adoption (1)"), col="black")
title(main="", xlab="Percentage of Adoptions Below 33% Cut Off", ylab="Speed Measure", sub="(a) Mallinson 2016")

par(mar=c(5,5,1,2), fig=c(0.5,1,0,1))
plot.window(xlim=c(0,1), ylim=c(-5,2.5))

fast <- speeddata[which(speeddata$nc_fast==1),]
slow <- speeddata[which(speeddata$nc_fast==0),]

fast_correct <- fast[which(fast$weib.rescale.robust>=0),]
fast_incorrect <- fast[which(fast$weib.rescale.robust<0),]
slow_correct <- slow[which(slow$weib.rescale.robust<=0),]
slow_incorrect <- slow[which(slow$weib.rescale.robust>0),]

points(x=fast_correct$percent.below.cut, y=fast_correct$weib.rescale.robust, col="black", pch=19)
points(x=slow_correct$percent.below.cut, y=slow_correct$weib.rescale.robust, col="black", pch=19)
points(x=fast_incorrect$percent.below.cut, y=fast_incorrect$weib.rescale.robust, col="red", pch=17)
points(x=slow_incorrect$percent.below.cut, y=slow_incorrect$weib.rescale.robust, col="red", pch=17)
axis(1, at=seq(0,1,.1), labels=c(0,10,20,30,40,50,60,70,80,90,100))
axis(2, at=seq(-5,2,1), labels=seq(-5,2,1), las=2)
segments(.5,-5,.5,2.5, lty=1, col="black")
segments(0,0,1,0, lty=1, col="black")
text(x=.25, y=-5, c("Slow Adoption (0)"), col="black")
text(x=.75, y=-5, c("Fast Adoption (1)"), col="black")
title(main="", xlab="Percentage of Adoptions Below 33% Cut Off", ylab="Speed Measure", sub="(b) SPID")
dev.off()

################## Speed changes over time (Figure 3) #########################

tiff("figure3.tiff", height=8.5, width=11, units="in", res=600)
scatter.smooth(xlim=c(1800,2020), ylim=c(-5,2.5), speeddata$first, speeddata$weib.rescale.robust, ylab="Adoption Speed", xlab="Year of First Adoption", main="",pch=20, degree=2, evaluation=10, lpars=list(lwd=2, col="red"), axes=FALSE, font.lab=2)
abline(v=1965, col="red", lwd=2, lty=2)
text(1965, -5, "1965", cex=1, pos=4, col="red", font=2)
axis(1, at=seq(1800,2000, 50), labels=seq(1800,2000,50), font.lab=2, cex=2)
axis(2, at=seq(-5,2,1), labels=seq(-5,2,1), las=2)
dev.off()

## T-test for average scope of policy adoption pre-1990 compared to post-1990
pre90 <- speeddata[which(speeddata$first_year<1990),]
post90 <- speeddata[which(speeddata$first_year>=1990),]
mean(pre90$adopt_count) #28.26
sd(pre90$adopt_count) #14.97
mean(post90$adopt_count) #21.51
sd(post90$adopt_count)#14.74
t.test(pre90$adopt_count, post90$adopt_count) # t(681) = 5.66, p < 0.001

## T-test for average scope of policy adoption pre-1965 and post-1965
pre65 <- speeddata[which(speeddata$first<1965),]
post65 <- speeddata[which(speeddata$first>=1965),]
mean(pre65$adopt_count) #34.28
sd(pre65$adopt_count) #13.65
mean(post65$adopt_count) #22.94
sd(post65$adopt_count)#14.64
t.test(pre65$adopt_count, post65$adopt_count) # t(681) = 9.36, p < 0.001

#################### Mean and SD from each MajorTopic (not reported)  #################

ag <- speeddata[c("majornum", "weib.rescale.robust")]
ag <- aggregate(ag$weib.rescale.robust ~ ag$majornum, FUN=mean)

describe <- describeBy(speeddata$weib.rescale.robust, group = speeddata$majornum)

means <- do.call(rbind.data.frame, describe)
means[is.na(means)] <- 0
means <- means[,1:5]

means <- cbind(rownames(means), seq(0,nrow(means)-1,1), means, means$mean-1.96*means$sd, means$mean+1.96*means$sd)

names(means)[1:2] <- c("majornum", "plotnum")
names(means)[8:9] <- c("ll", "ul")

plot.new()
plot.window(xlim=c(0,nrow(means)), ylim=c(-5, 3))
points(x=means$plotnum, y=means$mean, pch=19)
segments(means$plotnum, means$ll, means$plotnum, means$ul)
axis(1, at=c(0:19), labels=FALSE)
axis(2, at=seq(-5,3,1), labels=seq(-5,3,1), las=0)
text(c(0:19)-.4, par("usr")[3]-0.15, labels=legend, srt=45, pos=1, xpd=TRUE, cex=.9)
speed.mean <- mean(speeddata$weib.rescale.speed)
legend <- c("Macroeconomics","Civil Rights", "Health", "Agriculture", "Labor","Education", "Environment", "Energy", "Immigration", "Transportation", "Law and Crime", 
            "Social Welfare", "Housing", "Domestic Commerce", "Defense", "Technology", "Foreign Trade", "International Affairs", "Government Operations", "Public Lands")
abline(h=speed.mean)
abline(h=0, lty=2)
text(means$plotnum, 3, means$n)

#################### Pooled Weibull Analysis ############################

pooled.raw <- read.csv("pooled_weibull_spid_2021-02-23.csv") #Includes covariates

pooled <- subset(pooled.raw, first_year>1946)  #Cut observations prior to 1946
pooled <- subset(pooled, adopt_year<2014) #Cut observations after 2014

## Need to separately calculate the interaction term, so as to suppress the constituent term of complexity in the fixed effects models
pooled$interaction <- pooled$complexity_topic*pooled$mip

####################################### Table 1 #############################################

#Base model with single scale parameter (mip interaction)
base.pooled <- survreg(Surv(time=pooled$duration, event=pooled$adopt, type="right") ~ nyt + mip + complexity_topic + mip*complexity_topic + cluster(policy), dist="weibull", data=pooled)
summary(base.pooled)
AIC(base.pooled)

#NYT Interaction
nyt.pooled <- survreg(Surv(time=pooled$duration, event=pooled$adopt, type="right") ~ nyt + mip + complexity_topic + nyt*complexity_topic + cluster(policy), dist="weibull", data=pooled)
summary(nyt.pooled)
AIC(nyt.pooled)
lrtest(base.pooled, nyt.pooled) 

#Base plus FE for policy in regression equation 
base.pooled.fe <- survreg(Surv(time=pooled$duration, event=pooled$adopt, type="right") ~ nyt + mip + interaction + as.factor(policy) + cluster(policy) -1, dist="weibull", data=pooled)
summary(base.pooled.fe)
round(summary(base.pooled.fe)$table[1:4,],3)
AIC(base.pooled.fe) 
lrtest(base.pooled, base.pooled.fe)

#Base plus all FE (not reported)
base.pooled.allfe <- survreg(Surv(time=pooled$duration, event=pooled$adopt, type="right") ~ nyt + mip + interaction + as.factor(policy) + strata(policy) + (cluster(policy) - 1), dist="weibull", data=pooled, maxiter=1000)
summary(base.pooled.allfe)
round(summary(base.pooled.allfe)$table[1:4,],3)
AIC(base.pooled.allfe) 
lrtest(base.pooled, base.pooled.allfe)
lrtest(base.pooled.fe, base.pooled.allfe)

#Include measure of year
pooled$year_count <- pooled$adopt_year - min(pooled$first_year)
year.pooled.out <- survreg(Surv(time=pooled$duration, event=pooled$adopt, type="right") ~ nyt + mip + complexity_topic + mip*complexity_topic + cluster(policy) + year_count, dist="weibull", data=pooled)
summary(year.pooled.out)
AIC(year.pooled.out)
lrtest(base.pooled, year.pooled.out)

year.pooled.fe <- survreg(Surv(time=pooled$duration, event=pooled$adopt, type="right") ~ nyt + mip + interaction + as.factor(policy) + strata(policy) + year_count + cluster(policy) -1, dist="weibull", data=pooled, maxiter=1000)
summary(year.pooled.fe)

#Include only Mallinson 2016 policies
mall.pooled.fe <- survreg(Surv(time=pooled$duration[pooled$mallinson16==1], event=pooled$adopt[pooled$mallinson16==1], type="right") ~ nyt + mip + interaction + as.factor(policy) + cluster(policy) -1, dist="weibull", data=pooled[which(pooled$mallinson16==1),])
#summary(mall.pooled.fe)
round(summary(mall.pooled.fe)$table[1:4,],3)

#Weighted to assume identical selection probability in the population across major policy topics

# https://rpubs.com/mhanauer/268281

uniform.dat <- pooled[which(pooled$majornum!=4),] #Remove Agriculture policies
uniform.dat <- uniform.dat[which(uniform.dat$majornum!=9),] #Remove Immigration policies
uniform.dat <- uniform.dat[which(uniform.dat$majornum!=16),] #Remove Defense policies
uniform.dat <- uniform.dat[which(uniform.dat$majornum!=17),] #Remove Space policies
uniform.dat <- uniform.dat[which(uniform.dat$majornum!=18),] #Remove Foreign Trade policies
uniform.dat <- uniform.dat[which(uniform.dat$majornum!=19),] #Remove International policies

data.svy.unweighted <- svydesign(ids=~1, data=uniform.dat)
policy.dist <- data.frame(majornum=c("1", "2", "3", "5", "6", "7", "8", "10", "12", "13", "14", "15", "20", "21"),
                          Freq = nrow(pooled) * c(0.07142857, 0.07142857, 0.07142857, 0.07142857, 0.07142857, 0.07142857, 0.07142857, 0.07142857, 0.07142857, 0.07142857, 0.07142857, 0.07142857, 0.07142857, 0.07142857))

data.svy.rake <- rake(design = data.svy.unweighted,
                      sample.margins = list(~majornum),
                      population.margins = list(policy.dist))

#data.svy.rake.trim <- trimWeights(data.svy.rake, lower=0.3, upper=3,
# strict=TRUE)

uniform.dat <- cbind(uniform.dat, data.svy.rake$prob)
names(uniform.dat)[ncol(uniform.dat)] <- "prob"
uniform.dat$invprob <- 1/uniform.dat$prob

wbase.pooled.fe <- survreg(Surv(time=uniform.dat$duration, event=uniform.dat$adopt, type="right") ~ nyt + mip + interaction + as.factor(policy) + cluster(policy) -1, dist="weibull", data=uniform.dat, weights=uniform.dat$invprob)
round(summary(wbase.pooled.fe)$table[1:4,],3)
summary(wbase.pooled.fe)

firstobs <- read.csv("firstobs.csv")

nc <- glm(nc_fast ~ nyt + mip + complexity_topic + mip*complexity_topic, data=firstobs, family = binomial())
summary(nc)



###################### Table 2 #########################

base.pooled.fe <- survreg(Surv(time=pooled$duration, event=pooled$adopt, type="right") ~ nyt + mip + interaction + as.factor(policy) + cluster(policy) -1, dist="weibull", data=pooled)
#summary(base.pooled.fe)
round(summary(base.pooled.fe)$table[1:4,],3)

bs.pooled.fe <- survreg(Surv(time=pooled$duration[pooled$source=="Boehmke-Skinner"], event=pooled$adopt[pooled$source=="Boehmke-Skinner"], type="right") ~ nyt + mip + interaction + as.factor(policy) + cluster(policy) -1, dist="weibull", data=pooled[which(pooled$source=="Boehmke-Skinner"),])
#summary(bs.pooled.fe)
round(summary(bs.pooled.fe)$table[1:4,],3)

b.pooled.fe <- survreg(Surv(time=pooled$duration[pooled$source=="Boushey"], event=pooled$adopt[pooled$source=="Boushey"], type="right") ~ nyt + mip + interaction + as.factor(policy) + cluster(policy) -1, dist="weibull", data=pooled[which(pooled$source=="Boushey"),])
#summary(b.pooled.fe)
round(summary(b.pooled.fe)$table[1:4,],3)

cw.pooled.fe <- survreg(Surv(time=pooled$duration[pooled$source=="Caughey-Warshaw"], event=pooled$adopt[pooled$source=="Caughey-Warshaw"], type="right") ~ nyt + mip + interaction + as.factor(policy) + cluster(policy) -1, dist="weibull", data=pooled[which(pooled$source=="Caughey-Warshaw"),])
#summary(cw.pooled.fe)
round(summary(cw.pooled.fe)$table[1:4,],3)

kar.pooled.fe <- survreg(Surv(time=pooled$duration[pooled$source=="Karch"], event=pooled$adopt[pooled$source=="Karch"], type="right") ~ nyt + mip + interaction + as.factor(policy) + cluster(policy) -1, dist="weibull", data=pooled[which(pooled$source=="Karch"),])
#summary(kar.pooled.fe)
round(summary(kar.pooled.fe)$table[1:4,],3)

k.pooled.fe <- survreg(Surv(time=pooled$duration[pooled$source=="Kreitzer"], event=pooled$adopt[pooled$source=="Kreitzer"], type="right") ~ nyt + mip + interaction + as.factor(policy) + cluster(policy) -1, dist="weibull", data=pooled[which(pooled$source=="Kreitzer"),])
#summary(k.pooled.fe)
round(summary(k.pooled.fe)$table[1:4,],3)

ul.pooled.fe <- survreg(Surv(time=pooled$duration[pooled$source=="Uniform Law"], event=pooled$adopt[pooled$source=="Uniform Law"], type="right") ~ nyt + mip + interaction + as.factor(policy) + cluster(policy) -1, dist="weibull", data=pooled[which(pooled$source=="Uniform Law"),])
#summary(ul.pooled.fe)
round(summary(ul.pooled.fe)$table[1:4,],3)

#################### Marginal Effects for SPID and Mallinson (Figure 4) ########################

tiff("figure4.tiff", height=8, width=11, res=600, units="in")
plot.new()
par(mar=c(3,5,2,2), fig=c(0,0.5,0,1))

sal.complex.est <- base.pooled.fe$coeff[3] + 1*base.pooled.fe$coeff[4]
sal.complex.se <- sqrt(vcov(base.pooled.fe)[3,3] + (1)^2*vcov(base.pooled.fe)[4,4] + 2*(1)*vcov(base.pooled.fe)[3,4])

sal.nocomplex.est <- base.pooled.fe$coeff[3] + 0*base.pooled.fe$coeff[4]
sal.nocomplex.se <- sqrt(vcov(base.pooled.fe)[3,3] + (0)^2*vcov(base.pooled.fe)[4,4] + 2*(0)*vcov(base.pooled.fe)[3,4])

plot.window(xlim=c(0,2), ylim=c(-.1,.1))
points(x=0.5, y=sal.nocomplex.est, pch=20, cex=2)
lines(x=c(0.5,0.5), y=c((sal.nocomplex.est+(1.96*sal.nocomplex.se)), (sal.nocomplex.est-(1.96*sal.nocomplex.se))), lwd=1.5)
lines(x=c(0.45, 0.55), y=c((sal.nocomplex.est+(1.96*sal.nocomplex.se)), (sal.nocomplex.est+(1.96*sal.nocomplex.se))), lwd=1.5)
lines(x=c(0.45, 0.55), y=c((sal.nocomplex.est-(1.96*sal.nocomplex.se)), (sal.nocomplex.est-(1.96*sal.nocomplex.se))), lwd=1.5)
points(x=1.5, y=sal.complex.est, pch=20, cex=2)
lines(x=c(1.5,1.5), y=c((sal.complex.est+(1.96*sal.complex.se)), (sal.complex.est-(1.96*sal.complex.se))), lwd=1.5)
lines(x=c(1.45,1.55), y=c((sal.complex.est+(1.96*sal.complex.se)), (sal.complex.est+(1.96*sal.complex.se))), lwd=1.5)
lines(x=c(1.45,1.55), y=c((sal.complex.est-(1.96*sal.complex.se)), (sal.complex.est-(1.96*sal.complex.se))), lwd=1.5)
abline(h=0)
axis(1, at=c(0.5,1.5), label=c("Not Complex", "Complex"), lty=0)
axis(2, at=c(-.1,.1), label=c(-.1,.1))
title(main="(a) All SPID Policies", xlab="", ylab="Salience (MIP)")

par(mar=c(3,5,2,2), fig=c(0.5,1,0,1))

sal.complex.est <- mall.pooled.fe$coeff[3] + 1*mall.pooled.fe$coeff[4]
sal.complex.se <- sqrt(vcov(mall.pooled.fe)[3,3] + (1)^2*vcov(mall.pooled.fe)[4,4] + 2*(1)*vcov(mall.pooled.fe)[3,4])

sal.nocomplex.est <- mall.pooled.fe$coeff[3] + 0*mall.pooled.fe$coeff[4]
sal.nocomplex.se <- sqrt(vcov(mall.pooled.fe)[3,3] + (0)^2*vcov(mall.pooled.fe)[4,4] + 2*(0)*vcov(mall.pooled.fe)[3,4])

plot.window(xlim=c(0,2), ylim=c(-.1,.1))
points(x=0.5, y=sal.nocomplex.est, pch=20, cex=2)
lines(x=c(0.5,0.5), y=c((sal.nocomplex.est+(1.96*sal.nocomplex.se)), (sal.nocomplex.est-(1.96*sal.nocomplex.se))), lwd=1.5)
lines(x=c(0.45, 0.55), y=c((sal.nocomplex.est+(1.96*sal.nocomplex.se)), (sal.nocomplex.est+(1.96*sal.nocomplex.se))), lwd=1.5)
lines(x=c(0.45, 0.55), y=c((sal.nocomplex.est-(1.96*sal.nocomplex.se)), (sal.nocomplex.est-(1.96*sal.nocomplex.se))), lwd=1.5)
points(x=1.5, y=sal.complex.est, pch=20, cex=2)
lines(x=c(1.5,1.5), y=c((sal.complex.est+(1.96*sal.complex.se)), (sal.complex.est-(1.96*sal.complex.se))), lwd=1.5)
lines(x=c(1.45,1.55), y=c((sal.complex.est+(1.96*sal.complex.se)), (sal.complex.est+(1.96*sal.complex.se))), lwd=1.5)
lines(x=c(1.45,1.55), y=c((sal.complex.est-(1.96*sal.complex.se)), (sal.complex.est-(1.96*sal.complex.se))), lwd=1.5)
abline(h=0)
axis(1, at=c(0.5,1.5), label=c("Not Complex", "Complex"), lty=0)
axis(2, at=c(-.1,.1), label=c(-.1,.1))
title(main="(b) Mallinson Policies", xlab="", ylab="Salience (MIP)")
dev.off()

## Interaction plot for weighted SPID results
sal.complex.est <- wbase.pooled.fe$coeff[3] + 1*wbase.pooled.fe$coeff[4]
sal.complex.se <- sqrt(vcov(wbase.pooled.fe)[3,3] + (1)^2*vcov(wbase.pooled.fe)[4,4] + 2*(1)*vcov(wbase.pooled.fe)[3,4])

sal.nocomplex.est <- wbase.pooled.fe$coeff[3] + 0*wbase.pooled.fe$coeff[4]
sal.nocomplex.se <- sqrt(vcov(wbase.pooled.fe)[3,3] + (0)^2*vcov(wbase.pooled.fe)[4,4] + 2*(0)*vcov(wbase.pooled.fe)[3,4])

plot.window(xlim=c(0,2), ylim=c(-.1,.1))
points(x=0.5, y=sal.nocomplex.est, pch=20, cex=2)
lines(x=c(0.5,0.5), y=c((sal.nocomplex.est+(1.96*sal.nocomplex.se)), (sal.nocomplex.est-(1.96*sal.nocomplex.se))), lwd=1.5)
lines(x=c(0.45, 0.55), y=c((sal.nocomplex.est+(1.96*sal.nocomplex.se)), (sal.nocomplex.est+(1.96*sal.nocomplex.se))), lwd=1.5)
lines(x=c(0.45, 0.55), y=c((sal.nocomplex.est-(1.96*sal.nocomplex.se)), (sal.nocomplex.est-(1.96*sal.nocomplex.se))), lwd=1.5)
points(x=1.5, y=sal.complex.est, pch=20, cex=2)
lines(x=c(1.5,1.5), y=c((sal.complex.est+(1.96*sal.complex.se)), (sal.complex.est-(1.96*sal.complex.se))), lwd=1.5)
lines(x=c(1.45,1.55), y=c((sal.complex.est+(1.96*sal.complex.se)), (sal.complex.est+(1.96*sal.complex.se))), lwd=1.5)
lines(x=c(1.45,1.55), y=c((sal.complex.est-(1.96*sal.complex.se)), (sal.complex.est-(1.96*sal.complex.se))), lwd=1.5)
abline(h=0)
axis(1, at=c(0.5,1.5), label=c("Not Complex", "Complex"), lty=0)
axis(2, at=c(-.1,.1), label=c(-.1,.1))
title(main="Weighted SPID Results", xlab="", ylab="Salience (MIP)")

#Interaction plot for Nicholson-Crotty
sal.complex.est <- nc$coeff[3] + 1*nc$coeff[5]
sal.complex.se <- sqrt(vcov(nc)[3,3] + (1)^2*vcov(nc)[5,5] + 2*(1)*vcov(nc)[3,5])

sal.nocomplex.est <- nc$coeff[3] + 0*nc$coeff[4]
sal.nocomplex.se <- sqrt(vcov(nc)[3,3] + (0)^2*vcov(nc)[5,5] + 2*(0)*vcov(nc)[3,5])

plot.window(xlim=c(0,2), ylim=c(-.1,.1))
points(x=0.5, y=sal.nocomplex.est, pch=20, cex=2)
lines(x=c(0.5,0.5), y=c((sal.nocomplex.est+(1.96*sal.nocomplex.se)), (sal.nocomplex.est-(1.96*sal.nocomplex.se))), lwd=1.5)
lines(x=c(0.45, 0.55), y=c((sal.nocomplex.est+(1.96*sal.nocomplex.se)), (sal.nocomplex.est+(1.96*sal.nocomplex.se))), lwd=1.5)
lines(x=c(0.45, 0.55), y=c((sal.nocomplex.est-(1.96*sal.nocomplex.se)), (sal.nocomplex.est-(1.96*sal.nocomplex.se))), lwd=1.5)
points(x=1.5, y=sal.complex.est, pch=20, cex=2)
lines(x=c(1.5,1.5), y=c((sal.complex.est+(1.96*sal.complex.se)), (sal.complex.est-(1.96*sal.complex.se))), lwd=1.5)
lines(x=c(1.45,1.55), y=c((sal.complex.est+(1.96*sal.complex.se)), (sal.complex.est+(1.96*sal.complex.se))), lwd=1.5)
lines(x=c(1.45,1.55), y=c((sal.complex.est-(1.96*sal.complex.se)), (sal.complex.est-(1.96*sal.complex.se))), lwd=1.5)
abline(h=0)
axis(1, at=c(0.5,1.5), label=c("Not Complex", "Complex"), lty=0)
axis(2, at=c(-.1,.1), label=c(-.1,.1))
title(main="Weighted SPID Results", xlab="", ylab="Salience (MIP)")

#################### Marginal Effects for Constituent Datasets (Figure 5) ########################

tiff("figure5.tiff", height=11, width=8.5, res=600, units="in")
plot.new()
par(mar=c(3,5,2,2), fig=c(0,0.5,.75,1))

sal.complex.est <- base.pooled.fe$coeff[3] + 1*base.pooled.fe$coeff[4]
sal.complex.se <- sqrt(vcov(base.pooled.fe)[3,3] + (1)^2*vcov(base.pooled.fe)[4,4] + 2*(1)*vcov(base.pooled.fe)[3,4])

sal.nocomplex.est <- base.pooled.fe$coeff[3] + 0*base.pooled.fe$coeff[4]
sal.nocomplex.se <- sqrt(vcov(base.pooled.fe)[3,3] + (0)^2*vcov(base.pooled.fe)[4,4] + 2*(0)*vcov(base.pooled.fe)[3,4])

plot.window(xlim=c(0,2), ylim=c(-.1,.1))
points(x=0.5, y=sal.nocomplex.est, pch=20, cex=2)
lines(x=c(0.5,0.5), y=c((sal.nocomplex.est+(1.96*sal.nocomplex.se)), (sal.nocomplex.est-(1.96*sal.nocomplex.se))), lwd=1.5)
lines(x=c(0.45, 0.55), y=c((sal.nocomplex.est+(1.96*sal.nocomplex.se)), (sal.nocomplex.est+(1.96*sal.nocomplex.se))), lwd=1.5)
lines(x=c(0.45, 0.55), y=c((sal.nocomplex.est-(1.96*sal.nocomplex.se)), (sal.nocomplex.est-(1.96*sal.nocomplex.se))), lwd=1.5)
points(x=1.5, y=sal.complex.est, pch=20, cex=2)
lines(x=c(1.5,1.5), y=c((sal.complex.est+(1.96*sal.complex.se)), (sal.complex.est-(1.96*sal.complex.se))), lwd=1.5)
lines(x=c(1.45,1.55), y=c((sal.complex.est+(1.96*sal.complex.se)), (sal.complex.est+(1.96*sal.complex.se))), lwd=1.5)
lines(x=c(1.45,1.55), y=c((sal.complex.est-(1.96*sal.complex.se)), (sal.complex.est-(1.96*sal.complex.se))), lwd=1.5)
abline(h=0)
axis(1, at=c(0.5,1.5), label=c("Not Complex", "Complex"), lty=0)
axis(2, at=c(-.1,.1), label=c(-.1,.1))
title(main="(a) All SPID Policies", xlab="", ylab="Salience (MIP)")

par(mar=c(3,5,2,2), fig=c(0.5,1,.75,1))

sal.complex.est <- bs.pooled.fe$coeff[3] + 1*bs.pooled.fe$coeff[4]
sal.complex.se <- sqrt(vcov(bs.pooled.fe)[3,3] + (1)^2*vcov(bs.pooled.fe)[4,4] + 2*(1)*vcov(bs.pooled.fe)[3,4])

sal.nocomplex.est <- bs.pooled.fe$coeff[3] + 0*bs.pooled.fe$coeff[4]
sal.nocomplex.se <- sqrt(vcov(bs.pooled.fe)[3,3] + (0)^2*vcov(bs.pooled.fe)[4,4] + 2*(0)*vcov(bs.pooled.fe)[3,4])

plot.window(xlim=c(0,2), ylim=c(-.2,.1))
points(x=0.5, y=sal.nocomplex.est, pch=20, cex=2)
lines(x=c(0.5,0.5), y=c((sal.nocomplex.est+(1.96*sal.nocomplex.se)), (sal.nocomplex.est-(1.96*sal.nocomplex.se))), lwd=1.5)
lines(x=c(0.45, 0.55), y=c((sal.nocomplex.est+(1.96*sal.nocomplex.se)), (sal.nocomplex.est+(1.96*sal.nocomplex.se))), lwd=1.5)
lines(x=c(0.45, 0.55), y=c((sal.nocomplex.est-(1.96*sal.nocomplex.se)), (sal.nocomplex.est-(1.96*sal.nocomplex.se))), lwd=1.5)
points(x=1.5, y=sal.complex.est, pch=20, cex=2)
lines(x=c(1.5,1.5), y=c((sal.complex.est+(1.96*sal.complex.se)), (sal.complex.est-(1.96*sal.complex.se))), lwd=1.5)
lines(x=c(1.45,1.55), y=c((sal.complex.est+(1.96*sal.complex.se)), (sal.complex.est+(1.96*sal.complex.se))), lwd=1.5)
lines(x=c(1.45,1.55), y=c((sal.complex.est-(1.96*sal.complex.se)), (sal.complex.est-(1.96*sal.complex.se))), lwd=1.5)
abline(h=0)
axis(1, at=c(0.5,1.5), label=c("Not Complex", "Complex"), lty=0)
axis(2, at=c(-.2,.1), label=c(-.2,.1))
title(main="(b) Boehmke and Skinner Policies", xlab="", ylab="Salience (MIP)")

par(mar=c(3,5,2,2), fig=c(0,0.5,.5,.75))

sal.complex.est <- b.pooled.fe$coeff[3] + 1*b.pooled.fe$coeff[4]
sal.complex.se <- sqrt(vcov(b.pooled.fe)[3,3] + (1)^2*vcov(b.pooled.fe)[4,4] + 2*(1)*vcov(b.pooled.fe)[3,4])

sal.nocomplex.est <- b.pooled.fe$coeff[3] + 0*b.pooled.fe$coeff[4]
sal.nocomplex.se <- sqrt(vcov(b.pooled.fe)[3,3] + (0)^2*vcov(b.pooled.fe)[4,4] + 2*(0)*vcov(b.pooled.fe)[3,4])

plot.window(xlim=c(0,2), ylim=c(-.11,.11))
points(x=0.5, y=sal.nocomplex.est, pch=20, cex=2)
lines(x=c(0.5,0.5), y=c((sal.nocomplex.est+(1.96*sal.nocomplex.se)), (sal.nocomplex.est-(1.96*sal.nocomplex.se))), lwd=1.5)
lines(x=c(0.45, 0.55), y=c((sal.nocomplex.est+(1.96*sal.nocomplex.se)), (sal.nocomplex.est+(1.96*sal.nocomplex.se))), lwd=1.5)
lines(x=c(0.45, 0.55), y=c((sal.nocomplex.est-(1.96*sal.nocomplex.se)), (sal.nocomplex.est-(1.96*sal.nocomplex.se))), lwd=1.5)
points(x=1.5, y=sal.complex.est, pch=20, cex=2)
lines(x=c(1.5,1.5), y=c((sal.complex.est+(1.96*sal.complex.se)), (sal.complex.est-(1.96*sal.complex.se))), lwd=1.5)
lines(x=c(1.45,1.55), y=c((sal.complex.est+(1.96*sal.complex.se)), (sal.complex.est+(1.96*sal.complex.se))), lwd=1.5)
lines(x=c(1.45,1.55), y=c((sal.complex.est-(1.96*sal.complex.se)), (sal.complex.est-(1.96*sal.complex.se))), lwd=1.5)
abline(h=0)
axis(1, at=c(0.5,1.5), label=c("Not Complex", "Complex"), lty=0)
axis(2, at=c(-.1,0,.1), label=c(-.1,0,.1))
title(main="(c) Boushey Policies", xlab="", ylab="Salience (MIP)")

par(mar=c(3,5,2,2), fig=c(0.5,1,.5,.75))

sal.complex.est <- cw.pooled.fe$coeff[3] + 1*cw.pooled.fe$coeff[4]
sal.complex.se <- sqrt(vcov(cw.pooled.fe)[3,3] + (1)^2*vcov(cw.pooled.fe)[4,4] + 2*(1)*vcov(cw.pooled.fe)[3,4])

sal.nocomplex.est <- cw.pooled.fe$coeff[3] + 0*cw.pooled.fe$coeff[4]
sal.nocomplex.se <- sqrt(vcov(cw.pooled.fe)[3,3] + (0)^2*vcov(cw.pooled.fe)[4,4] + 2*(0)*vcov(cw.pooled.fe)[3,4])

plot.window(xlim=c(0,2), ylim=c(-.2,.1))
points(x=0.5, y=sal.nocomplex.est, pch=20, cex=2)
lines(x=c(0.5,0.5), y=c((sal.nocomplex.est+(1.96*sal.nocomplex.se)), (sal.nocomplex.est-(1.96*sal.nocomplex.se))), lwd=1.5)
lines(x=c(0.45, 0.55), y=c((sal.nocomplex.est+(1.96*sal.nocomplex.se)), (sal.nocomplex.est+(1.96*sal.nocomplex.se))), lwd=1.5)
lines(x=c(0.45, 0.55), y=c((sal.nocomplex.est-(1.96*sal.nocomplex.se)), (sal.nocomplex.est-(1.96*sal.nocomplex.se))), lwd=1.5)
points(x=1.5, y=sal.complex.est, pch=20, cex=2)
lines(x=c(1.5,1.5), y=c((sal.complex.est+(1.96*sal.complex.se)), (sal.complex.est-(1.96*sal.complex.se))), lwd=1.5)
lines(x=c(1.45,1.55), y=c((sal.complex.est+(1.96*sal.complex.se)), (sal.complex.est+(1.96*sal.complex.se))), lwd=1.5)
lines(x=c(1.45,1.55), y=c((sal.complex.est-(1.96*sal.complex.se)), (sal.complex.est-(1.96*sal.complex.se))), lwd=1.5)
abline(h=0)
axis(1, at=c(0.5,1.5), label=c("Not Complex", "Complex"), lty=0)
axis(2, at=c(-.2,.1), label=c(-.2,.1))
title(main="(d) Caughey and Warshaw Policies", xlab="", ylab="Salience (MIP)")

par(mar=c(3,5,2,2), fig=c(0,0.5,.25,.5))

sal.complex.est <- kar.pooled.fe$coeff[3] + 1*kar.pooled.fe$coeff[4]
sal.complex.se <- sqrt(vcov(kar.pooled.fe)[3,3] + (1)^2*vcov(kar.pooled.fe)[4,4] + 2*(1)*vcov(kar.pooled.fe)[3,4])

sal.nocomplex.est <- kar.pooled.fe$coeff[3] + 0*kar.pooled.fe$coeff[4]
sal.nocomplex.se <- sqrt(vcov(kar.pooled.fe)[3,3] + (0)^2*vcov(kar.pooled.fe)[4,4] + 2*(0)*vcov(kar.pooled.fe)[3,4])

plot.window(xlim=c(0,2), ylim=c(-.1,.1))
points(x=0.5, y=sal.nocomplex.est, pch=20, cex=2)
lines(x=c(0.5,0.5), y=c((sal.nocomplex.est+(1.96*sal.nocomplex.se)), (sal.nocomplex.est-(1.96*sal.nocomplex.se))), lwd=1.5)
lines(x=c(0.45, 0.55), y=c((sal.nocomplex.est+(1.96*sal.nocomplex.se)), (sal.nocomplex.est+(1.96*sal.nocomplex.se))), lwd=1.5)
lines(x=c(0.45, 0.55), y=c((sal.nocomplex.est-(1.96*sal.nocomplex.se)), (sal.nocomplex.est-(1.96*sal.nocomplex.se))), lwd=1.5)
points(x=1.5, y=sal.complex.est, pch=20, cex=2)
lines(x=c(1.5,1.5), y=c((sal.complex.est+(1.96*sal.complex.se)), (sal.complex.est-(1.96*sal.complex.se))), lwd=1.5)
lines(x=c(1.45,1.55), y=c((sal.complex.est+(1.96*sal.complex.se)), (sal.complex.est+(1.96*sal.complex.se))), lwd=1.5)
lines(x=c(1.45,1.55), y=c((sal.complex.est-(1.96*sal.complex.se)), (sal.complex.est-(1.96*sal.complex.se))), lwd=1.5)
abline(h=0)
axis(1, at=c(0.5,1.5), label=c("Not Complex", "Complex"), lty=0)
axis(2, at=c(-.1,.1), label=c(-.1,.1))
title(main="(e) Karch Policies", xlab="", ylab="Salience (MIP)")

par(mar=c(3,5,2,2), fig=c(0.5,1,.25,.5))

sal.complex.est <- k.pooled.fe$coeff[3] + 1*k.pooled.fe$coeff[4]
sal.complex.se <- sqrt(vcov(k.pooled.fe)[3,3] + (1)^2*vcov(k.pooled.fe)[4,4] + 2*(1)*vcov(k.pooled.fe)[3,4])

sal.nocomplex.est <- k.pooled.fe$coeff[3] + 0*k.pooled.fe$coeff[4]
sal.nocomplex.se <- sqrt(vcov(k.pooled.fe)[3,3] + (0)^2*vcov(k.pooled.fe)[4,4] + 2*(0)*vcov(k.pooled.fe)[3,4])

plot.window(xlim=c(0,2), ylim=c(-.3,.1))
points(x=0.5, y=sal.nocomplex.est, pch=20, cex=2)
lines(x=c(0.5,0.5), y=c((sal.nocomplex.est+(1.96*sal.nocomplex.se)), (sal.nocomplex.est-(1.96*sal.nocomplex.se))), lwd=1.5)
lines(x=c(0.45, 0.55), y=c((sal.nocomplex.est+(1.96*sal.nocomplex.se)), (sal.nocomplex.est+(1.96*sal.nocomplex.se))), lwd=1.5)
lines(x=c(0.45, 0.55), y=c((sal.nocomplex.est-(1.96*sal.nocomplex.se)), (sal.nocomplex.est-(1.96*sal.nocomplex.se))), lwd=1.5)
points(x=1.5, y=sal.complex.est, pch=20, cex=2)
lines(x=c(1.5,1.5), y=c((sal.complex.est+(1.96*sal.complex.se)), (sal.complex.est-(1.96*sal.complex.se))), lwd=1.5)
lines(x=c(1.45,1.55), y=c((sal.complex.est+(1.96*sal.complex.se)), (sal.complex.est+(1.96*sal.complex.se))), lwd=1.5)
lines(x=c(1.45,1.55), y=c((sal.complex.est-(1.96*sal.complex.se)), (sal.complex.est-(1.96*sal.complex.se))), lwd=1.5)
abline(h=0)
axis(1, at=c(0.5,1.5), label=c("Not Complex", "Complex"), lty=0)
axis(2, at=c(-.3,.1), label=c(-.3,.1))
title(main="(f) Kreitzer Policies", xlab="", ylab="Salience (MIP)")

par(mar=c(3,5,2,2), fig=c(.25,.75,0,.25))

sal.complex.est <- ul.pooled.fe$coeff[3] + 1*ul.pooled.fe$coeff[4]
sal.complex.se <- sqrt(vcov(ul.pooled.fe)[3,3] + (1)^2*vcov(ul.pooled.fe)[4,4] + 2*(1)*vcov(ul.pooled.fe)[3,4])

sal.nocomplex.est <- ul.pooled.fe$coeff[3] + 0*ul.pooled.fe$coeff[4]
sal.nocomplex.se <- sqrt(vcov(ul.pooled.fe)[3,3] + (0)^2*vcov(ul.pooled.fe)[4,4] + 2*(0)*vcov(ul.pooled.fe)[3,4])

plot.window(xlim=c(0,2), ylim=c(-.1,.1))
points(x=0.5, y=sal.nocomplex.est, pch=20, cex=2)
lines(x=c(0.5,0.5), y=c((sal.nocomplex.est+(1.96*sal.nocomplex.se)), (sal.nocomplex.est-(1.96*sal.nocomplex.se))), lwd=1.5)
lines(x=c(0.45, 0.55), y=c((sal.nocomplex.est+(1.96*sal.nocomplex.se)), (sal.nocomplex.est+(1.96*sal.nocomplex.se))), lwd=1.5)
lines(x=c(0.45, 0.55), y=c((sal.nocomplex.est-(1.96*sal.nocomplex.se)), (sal.nocomplex.est-(1.96*sal.nocomplex.se))), lwd=1.5)
points(x=1.5, y=sal.complex.est, pch=20, cex=2)
lines(x=c(1.5,1.5), y=c((sal.complex.est+(1.96*sal.complex.se)), (sal.complex.est-(1.96*sal.complex.se))), lwd=1.5)
lines(x=c(1.45,1.55), y=c((sal.complex.est+(1.96*sal.complex.se)), (sal.complex.est+(1.96*sal.complex.se))), lwd=1.5)
lines(x=c(1.45,1.55), y=c((sal.complex.est-(1.96*sal.complex.se)), (sal.complex.est-(1.96*sal.complex.se))), lwd=1.5)
abline(h=0)
axis(1, at=c(0.5,1.5), label=c("Not Complex", "Complex"), lty=0)
axis(2, at=c(-.1,.1), label=c(-.1,.1))
title(main="(g) Uniform Law Policies", xlab="", ylab="Salience (MIP)")

dev.off()
