####################################################################################
#AUTHOR: Shom Mazumder
#AFFILIATION: Harvard University Dept. of Government
#DATE LAST UPDATED: March 12th, 2018
#PURPOSE: Estimates heterogenous effects
#DETAILS: this script estimates het effects using sparsereg
####################################################################################
rm(list = setdiff(ls(),"root")) #clear workspace

############PACKAGES############
library(foreign)
library(ggplot2)
library(stargazer)
library(lmtest)
library(dplyr)
library(readr)
library(causalsens)
library(sparsereg)

####FUNCTIONS####


####SETUP####

#set working directory
setwd(root)

#read data
# protests <- read_csv('civil_rights_protest_final.csv')
# abs.cces.white <- read_csv('abs-jop-cces-white-countydata.csv')

#merge Acharya, Blackwell, and Sen's CCES data with protests data
# merged <- inner_join(abs.cces.white,protests,by='fips')
# cols <- c("resent", "protest_indicator", "pctblack", "pcturban", "logTotPop1960",
#          "medincome", "avg.dem.vshare.pre", "south")
# merged3<- merged%>%
#          dplyr::select(cols)
# saveRDS(merged3, "merged3.RDS")

merged3 <- readRDS("merged3.RDS")


## 제1단계: create vector of interactions
interactions_df <- model.matrix(~(resent + protest_indicator + pctblack + pcturban + logTotPop1960 + 
                                    medincome + avg.dem.vshare.pre + south)^2 - 1,
                                merged)
interactions_df <- scale(interactions_df)
X.cols <- c("protest_indicator","pcturban","pctblack","medincome",
            "avg.dem.vshare.pre","protest_indicator:pcturban",
            "protest_indicator:pctblack","protest_indicator:medincome",
            "protest_indicator:avg.dem.vshare.pre","protest_indicator:south")

## 제3단계: sparsereg()함수 실행
sparse.mod <- sparsereg(y = interactions_df[,"resent"],
                        X = interactions_df[,X.cols],burnin = 400,gibbs = 1000)

## 제4단계: 그래프 그리기
cairo_pdf(filename = "figures/sparsereg.pdf",family = "MinionPro")
plot(sparse.mod)
dev.off()
