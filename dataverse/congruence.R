# REPLICATION CODE FOR: Lupu, Noam, Lucia Selios, and Zach Warner. 2016. "A
# New Measure of Congruence: The Earth Mover's Distance." Political Analysis
# (forthcoming).
#
# Last edited 10-Nov-2016 by ZW. For questions please contact me at
# zwarner@wisc.edu.

################################################################################
#                                                                              #
#                                 SETUP                                        #
#                                                                              #
################################################################################
rm(list=ls())
library(ggplot2); library(grid); library(sfsmisc); library(emdist)
library(clusterGeneration); library(foreign); library(plyr); library(sandwich) 
library(lmtest); library(reshape)
setwd("/your/path/to/this/replication")

### version control
sessionInfo()
# R version 3.3.0 (2016-05-03)
# Platform: x86_64-apple-darwin13.4.0 (64-bit)
# Running under: OS X 10.11.6 (El Capitan)
# 
# locale:
# [1] en_US.UTF-8/en_US.UTF-8/en_US.UTF-8/C/en_US.UTF-8/en_US.UTF-8
# 
# attached base packages:
# [1] grid      stats     graphics  grDevices utils     datasets  methods  
# [8] base     
# 
# other attached packages:
# [1] reshape_0.8.6           lmtest_0.9-34           zoo_1.7-13             
# [4] sandwich_2.3-4          plyr_1.8.4              foreign_0.8-67         
# [7] clusterGeneration_1.3.4 MASS_7.3-45             emdist_0.3-2           
# [10] sfsmisc_1.1-0           ggplot2_2.1.0          
# 
# loaded via a namespace (and not attached):
# [1] Rcpp_0.12.7      lattice_0.20-34  gtable_0.2.0     scales_0.4.0    
# [5] tools_3.3.0      munsell_0.4.3    rsconnect_0.5    colorspace_1.2-7  

################################################################################
#                                                                              #
#                                 UTILITIES                                    #
#                                                                              #
################################################################################

##### Helper functions #####
# for multiple ggplot objects
multiplot <- function(..., plotlist=NULL, file, cols=1, layout=NULL) {
  plots <- c(list(...), plotlist)
  numPlots = length(plots)
  if (is.null(layout)) {
    layout <- matrix(seq(1, cols * ceiling(numPlots/cols)),
                     ncol = cols, nrow = ceiling(numPlots/cols))
  }
  if (numPlots==1) {
    print(plots[[1]])
  } else {
    grid.newpage()
    pushViewport(viewport(layout = grid.layout(nrow(layout), ncol(layout))))
    for (i in 1:numPlots) {
      matchidx <- as.data.frame(which(layout == i, arr.ind = TRUE))
      print(plots[[i]], vp = viewport(layout.pos.row = matchidx$row,
                                      layout.pos.col = matchidx$col))
    }
  }
}
# to simulate from a bimodal distribution
bimodalDistFunc <- function (n,cpct, mu1, mu2, sig1, sig2) {
  y0 <- rnorm(n,mean=mu1, sd = sig1)
  y1 <- rnorm(n,mean=mu2, sd = sig2)
  
  flag <- rbinom(n,size=1,prob=cpct)
  y <- y0*(1 - flag) + y1*flag 
}
# R^2 approximation from glm objects
r2.corr.mer <- function(m) {
  lmfit <-  lm(model.response(model.frame(m)) ~ fitted(m))
  summary(lmfit)$r.squared
} 
# for clustered SEs
cl <- function(dat,fm, cluster){
  M <- length(unique(cluster))
  N <- length(cluster)
  K <- fm$rank
  dfc <- (M/(M-1))*((N-1)/(N-K))
  uj  <- apply(estfun(fm),2, function(x) tapply(x, cluster, sum));
  vcovCL <- dfc*sandwich(fm, meat=crossprod(uj)/N)
  coeftest(fm, vcovCL) 
}

##### Helper functions for computing congruence #####
# Difference in means
dmeans <- function(data){
  df <- data
  x <- as.matrix(df$samps[which(df$pop == "x")])
  y <- as.matrix(df$samps[which(df$pop == "y")])
  abs(mean(x,na.rm=T)-mean(y,na.rm=T))
}
# CDF overlap
cdf.overlap <- function(data, q = 512) { 
  # no default length for sequences, but we'll use density's 512
  df <- data
  x <- as.matrix(na.omit(df$samps[which(df$pop == "x")]))
  y <- as.matrix(na.omit(df$samps[which(df$pop == "y")]))
  Fx <- ecdf(x); Fy <- ecdf(y)
  lower <- min(x,y) 
  upper <- max(x,y)
  z <- seq(lower,upper,((upper-lower)/q))
  Fxz <- Fx(z); Fyz <- Fy(z) 
  sum(abs(Fxz-Fyz))
}
# PDF overlap
pdf.overlap <- function(data, q = 512){ # q = 512 is the default for density()
  df <- data
  fx <- as.vector(na.omit(df$samps[which(df$pop == "x")]))
  fy <- as.vector(na.omit(df$samps[which(df$pop == "y")]))
  lower <- min(c(fx, fy))
  upper <- max(c(fx, fy))
  dx <- density(fx, from=lower, to=upper, n = q)
  dy <- density(fy, from=lower, to=upper, n = q)
  d <- data.frame(location=dx$x, x.den=dx$y, y.den=dy$y)
  d$intersect <- pmin(d$x.den, d$y.den)
  integrate.xy(d$location, d$intersect)
}
# EMD
emd.dis <- function(data, metric = "manhattan", iterations=100000){
  df <- data
  x <- as.matrix(df$samps[which(df$pop == "x")])
  y <- as.matrix(df$samps[which(df$pop == "y")])
  weight.x <- rep(1/nrow(x),nrow(x))
  weight.y <- rep(1/nrow(y),nrow(y))
  emdw(x,weight.x,y,weight.y,dist=metric,max.iter=iterations)
}

################################################################################
#                                                                              #
#                               INTRODUCTION                                   #
#                                                                              #
################################################################################

##### SIMULATED COMPUTATION TIME (FN 3) #####
# generate the data
x <- genRandomClust(numClust=7,numNonNoisy = 10, numNoisy = 5, numOutlier = .02,
                    rangeN=c(150,500), numReplicate = 1,fileName="x", 
                    outputLogFlag=F, outputEmpirical=F, outputInfo = F, 
                    outputDatFlag = F)
y <- genRandomClust(numClust=3,numNonNoisy = 10, numNoisy = 5, numOutlier = .08, 
                    rangeN=c(150,500), numReplicate = 1,fileName="y", 
                    outputLogFlag=F, outputEmpirical=F, outputInfo = F, 
                    outputDatFlag = F)
x <- x$datList$x_1
y <- y$datList$y_1
w.x <- rep(1/nrow(x),nrow(x))
w.y <- rep(1/nrow(y),nrow(y))
# time it
system.time(
  emd.bigprob <- emdw(x,w.x,y,w.y,max.iter=100000,dist="manhattan")
)
# Results for my machine (mid-2012 MacBook Air, 2 GHz Intel Core i7 processor, 
# 8 GB 1600 MHz DDR3 memory)
# user      system    elapsed 
# 500.493   4.536     511.266 

### clean up
rm(list=ls()[!(ls() %in% c("dmeans","cdf.overlap","pdf.overlap","emd.dis",
                           "r2.corr.mer","multiplot","bimodalDistFunc","cl"))])

################################################################################
#                                                                              #
#                            MEASURING CONGRUENCE                              #
#                                                                              #
################################################################################

### Create the data
set.seed(1234567)
dat.x.y1 <- data.frame(samps = c(rnorm(1000, mean=0, sd=1), 
                                 rnorm(1000, mean=3, sd=1)), 
                       pop = rep(c("x", "y"), each = 1000))
dat.x.y3 <- dat.x.y2 <- dat.x.y1
dat.x.y2$samps[which(dat.x.y2$pop == "y")] <- rnorm(1000,0,8)
dat.x.y3$samps[which(dat.x.y3$pop == "y")] <- bimodalDistFunc(1000,.35,2.5,
                                                              13.1,1,2)

##### FIGURE 1 #####
fig1.1 <- ggplot(dat.x.y1, aes(x = samps, fill = pop)) +
  theme_bw(base_size = 16) +
  theme(panel.grid.minor = element_blank(), axis.text.x = element_blank(),
        panel.border = element_rect(fill=NA,size=1,linetype="solid")) +
  geom_histogram(aes(y = ..density.. ), alpha = 0.5,pos="identity",binwidth=1,
                 show.legend=F) +
  geom_line(aes(y = ..density..), stat = 'density') + 
  scale_fill_manual(values=c("grey20", "grey20")) +
  coord_cartesian(ylim=c(-.01,.44),xlim=c(-11,26)) +
  scale_x_continuous(breaks=seq(-10,25,by=5)) +
  scale_y_continuous(breaks=seq(0,.40,by=.1)) +
  annotate("text",x=-3,y=.20,label="X",family="Times",size=5) +
  annotate("text",x=6.2,y=.10,label="Y[1]",family="Times", parse=T,size=5) +
  labs(title=NULL,x=NULL,y="Density") +
  theme(text=element_text(family="Times"))
fig1.1
fig1.2 <- ggplot(dat.x.y2, aes(x = samps, fill = pop)) +
  theme_bw(base_size = 16) +
  theme(panel.grid.minor = element_blank(), axis.text.x = element_blank(),
        panel.border = element_rect(fill=NA,size=1,linetype="solid")) +
  geom_histogram(aes(y = ..density.. ), alpha = 0.5,pos="identity",binwidth=1,
                 show.legend=F) +
  geom_line(aes(y = ..density..), stat = 'density') + 
  scale_fill_manual(values=c("grey20", "grey20")) +
  coord_cartesian(ylim=c(-.01,.44),xlim=c(-11,26)) +
  scale_x_continuous(breaks=seq(-10,25,by=5)) +
  scale_y_continuous(breaks=seq(0,.40,by=.1)) +
  annotate("text",x=-3,y=.20,label="X",family="Times",size=5) +
  annotate("text",x=6.2,y=.10,label="Y[2]",family="Times", parse=T,size=5) +
  labs(title=NULL,x=NULL,y="Density") +
  theme(text=element_text(family="Times"))
fig1.2
fig1.3 <- ggplot(dat.x.y3, aes(x = samps, fill = pop)) +
  theme_bw(base_size = 16) +
  theme(panel.grid.minor = element_blank(),
        panel.border = element_rect(fill=NA,size=1,linetype="solid")) +
  geom_histogram(aes(y = ..density.. ), alpha = 0.5,pos="identity",binwidth=1,
                 show.legend=F) +
  geom_line(aes(y = ..density..), stat = 'density') + 
  scale_fill_manual(values=c("grey20", "grey20")) +
  coord_cartesian(ylim=c(-.01,.44),xlim=c(-11,26)) +
  scale_x_continuous(breaks=seq(-10,25,by=5)) +
  scale_y_continuous(breaks=seq(0,.40,by=.1)) +
  annotate("text",x=-3,y=.20,label="X",family="Times",size=5) +
  annotate("text",x=6.2,y=.10,label="Y[3]",family="Times", parse=T,size=5) +
  labs(title=NULL,x="z",y="Density") +
  theme(text=element_text(family="Times"))
fig1.3
pdf("figure1.pdf",
    width=6.5,height=9)
grid.newpage()
grid.draw(rbind(ggplotGrob(fig1.1), 
                ggplotGrob(fig1.2), 
                ggplotGrob(fig1.3),size="last"))
dev.off()

################################################################################
#                                                                              #
#                             SIMULATION EVIDENCE                              #
#                                                                              #
################################################################################

##### TABLE 1 #####
### Difference of means distance
o2o.1 <- dmeans(dat.x.y1)
o2o.2 <- dmeans(dat.x.y2)
o2o.3 <- dmeans(dat.x.y3)

### CDF overlap distance
cdf.1 <- cdf.overlap(dat.x.y1)
cdf.2 <- cdf.overlap(dat.x.y2)
cdf.3 <- cdf.overlap(dat.x.y3)

### PDF overlap distance
pdf.1 <- pdf.overlap(dat.x.y1)
pdf.2 <- pdf.overlap(dat.x.y2)
pdf.3 <- pdf.overlap(dat.x.y3)

### EMD
emd.1 <- emd.dis(dat.x.y1)
emd.2 <- emd.dis(dat.x.y2)
emd.3 <- emd.dis(dat.x.y3)

### print the table
tab <- matrix(c(o2o.1,o2o.2,o2o.3,
                cdf.1,cdf.2,cdf.3,
                pdf.1,pdf.2,pdf.3,
                emd.1,emd.2,emd.3),ncol=4)
dimnames(tab) <- list(c("Top","Middle","Bottom"),
                      c("Diff in Means","CDF overlap","PDF Overlap","EMD"))
round(tab, 2)

##### FIGURE 3 #####
### size of each sample to compare
sizedist = 100 
### number of samples to compare
numsamps = 1000 
### create target distribution, a standard-normal
target <- data.frame(samps =  rnorm(sizedist,0,1), 
                     pop = rep("x", sizedist))

### generate means
means <-rnorm(numsamps,10,10)
# create dataframes to store results
df.meanshift <- data.frame(matrix(ncol=0,nrow=numsamps))
df.meanshift$means <- means
df.meanshift$emd <- df.meanshift$pdf <- df.meanshift$cdf <- 
  df.meanshift$diffmeans <-rep(NA,numsamps)
samps.list <- lapply(1:length(means),  
                     function(i) data.frame(matrix(nrow=sizedist,ncol=0)))
samps.df.means <- data.frame(matrix(nrow=sizedist,ncol=numsamps))
# create samples from the random means, then calculate distances and store them
for(i in 1:numsamps){
  print(i)
  # sample from the simulated mean
  samps.list[[i]]$samps <- rnorm(n=sizedist, mean = means[i], sd = 1)
  samps.df.means[,i] <- samps.list[[i]]$samps
  samps.list[[i]]$pop <- rep("y",sizedist)
  # bind the target dist
  samps.list[[i]] <- rbind(samps.list[[i]],target)
  # calculate congruence in various ways
  df.meanshift$diffmeans[i] <- dmeans(samps.list[[i]])
  df.meanshift$cdf[i] <- cdf.overlap(samps.list[[i]])
  df.meanshift$pdf[i] <- pdf.overlap(samps.list[[i]])
  df.meanshift$emd[i] <- emd.dis(samps.list[[i]],iterations = 100000)
}

### generate variances
variances <-runif(numsamps,1,10)
# create dataframes to store results
df.variances <- data.frame(matrix(ncol=0,nrow=numsamps))
df.variances$variances <- variances
df.variances$emd <- df.variances$pdf <- df.variances$cdf <- 
  df.variances$diffmeans <-rep(NA,numsamps)
samps.list <- lapply(1:length(variances),  
                     function(i) data.frame(matrix(nrow=sizedist,ncol=0)))
samps.df.vars <- data.frame(matrix(nrow=sizedist,ncol=numsamps))
# create samples from the random means, then calculate distances and store them
for(i in 1:numsamps){
  print(i)
  # sample from the simulated mean
  samps.list[[i]]$samps <- rnorm(n=sizedist, mean = 0, sd = sqrt(variances[i]))
  samps.df.vars[,i] <- samps.list[[i]]$samps
  samps.list[[i]]$pop <- rep("y",sizedist)
  # bind the target dist
  samps.list[[i]] <- rbind(samps.list[[i]],target)
  # calculate congruence in various ways
  df.variances$diffmeans[i] <- dmeans(samps.list[[i]])
  df.variances$cdf[i] <- cdf.overlap(samps.list[[i]])
  df.variances$pdf[i] <- pdf.overlap(samps.list[[i]])
  df.variances$emd[i] <- emd.dis(samps.list[[i]],iterations = 100000)
}

### Get the means data into shape for plotting
# diff-means
d1 <- as.data.frame(cbind(abs(df.meanshift$means),abs(df.meanshift$diffmeans)))
colnames(d1) <- c("x","y")
d1$x <- d1$x/max(d1$x); d1$y <- d1$y/max(d1$y)
d1$true.rank <-  NA; d1$true.rank[order(d1$x)] <- 1:nrow(d1)
d1$calc.rank <-  NA; d1$calc.rank[order(d1$y)] <- 1:nrow(d1)
# CDF overlap
d2 <- as.data.frame(cbind(abs(df.meanshift$means),abs(df.meanshift$cdf)))
colnames(d2) <- c("x","y")
d2$x <- d2$x/max(d2$x); d2$y <- d2$y/max(d2$y)
d2$true.rank <-  NA; d2$true.rank[order(d2$x)] <- 1:nrow(d2)
d2$calc.rank <-  NA; d2$calc.rank[order(d2$y)] <- 1:nrow(d2)
# PDf overlap (slightly different)
d3 <- as.data.frame(cbind(abs(df.meanshift$means),abs(df.meanshift$pdf)))
colnames(d3) <- c("x","y")
d3$x <- d3$x/max(d3$x)
# the next line inverts distance so closer to 0 means "more similar,"
# like other measures
d3$y <- 1/d3$y
d3$y <- d3$y/max(d3$y) # scales it
d3$true.rank <-  NA; d3$true.rank[order(d3$x)] <- 1:nrow(d3)
d3$calc.rank <-  NA; d3$calc.rank[order(d3$y)] <- 1:nrow(d3)
# EMD
d4 <- as.data.frame(cbind(abs(df.meanshift$means),abs(df.meanshift$emd)))
colnames(d4) <- c("x","y")
d4$x <- d4$x/max(d4$x); d4$y <- d4$y/max(d4$y)
d4$true.rank <-  NA; d4$true.rank[order(d4$x)] <- 1:nrow(d4)
d4$calc.rank <-  NA; d4$calc.rank[order(d4$y)] <- 1:nrow(d4)

### Get the variances data ready for plotting
# subtract 1 off variance to account for target variance = 1, as in text
# diff-means
d5 <- as.data.frame(cbind(abs(df.variances$variances-1)
                          ,abs(df.variances$diffmeans)))
colnames(d5) <- c("x","y")
d5$x <- d5$x/max(d5$x); d5$y <- d5$y/max(d5$y)
d5$true.rank <-  NA; d5$true.rank[order(d5$x)] <- 1:nrow(d5)
d5$calc.rank <-  NA; d5$calc.rank[order(d5$y)] <- 1:nrow(d5)
# CDF overlap
d6 <- as.data.frame(cbind(abs(df.variances$variances-1),abs(df.variances$cdf)))
colnames(d6) <- c("x","y")
d6$x <- d6$x/max(d6$x); d6$y <- d6$y/max(d6$y)
d6$true.rank <-  NA; d6$true.rank[order(d6$x)] <- 1:nrow(d6)
d6$calc.rank <-  NA; d6$calc.rank[order(d6$y)] <- 1:nrow(d6)
# PDf overlap (slightly different)
d7 <- as.data.frame(cbind(abs(df.variances$variances-1),abs(df.variances$pdf)))
colnames(d7) <- c("x","y")
d7$x <- d7$x/max(d7$x)
# the next line inverts distance so closer to 0 means "more similar,"
# like other measures
d7$y <- 1/d7$y 
d7$y <- d7$y/max(d7$y) # scales it
d7$true.rank <-  NA; d7$true.rank[order(d7$x)] <- 1:nrow(d7)
d7$calc.rank <-  NA; d7$calc.rank[order(d7$y)] <- 1:nrow(d7)
# EMD
d8 <- as.data.frame(cbind(abs(df.variances$variances-1),abs(df.variances$emd)))
colnames(d8) <- c("x","y")
d8$x <- d8$x/max(d8$x); d8$y <- d8$y/max(d8$y)
d8$true.rank <-  NA; d8$true.rank[order(d8$x)] <- 1:nrow(d8)
d8$calc.rank <-  NA; d8$calc.rank[order(d8$y)] <- 1:nrow(d8)

### plot
fig3.1 <- ggplot(d1) + 
  theme_bw(base_size = 20) +
  theme(panel.grid.minor = element_blank(),panel.grid.major = element_blank(),
        panel.border = element_rect(fill=NA,size=1,linetype="solid"),
        axis.text = element_text(size=12)) +
  geom_point(aes(x=true.rank,y=calc.rank)) +
  coord_cartesian(xlim=c(0,1000),ylim=c(0,1000)) +
  geom_abline(intercept = 0, slope = 1, linetype = "dashed", color = "gray20") + 
  labs(title="Difference in means",
       x="True mean rank",
       y="Calculated distance rank",
       size=16) +
  theme(text=element_text(family="Times"))

fig3.2 <- ggplot(d2) + 
  theme_bw(base_size = 20) +
  theme(panel.grid.minor = element_blank(),panel.grid.major = element_blank(),
        panel.border = element_rect(fill=NA,size=1,linetype="solid"),
        axis.text = element_text(size=12)) +
  geom_point(aes(x=true.rank,y=calc.rank)) +
  coord_cartesian(xlim=c(0,1000),ylim=c(0,1000)) +
  geom_abline(intercept = 0, slope = 1, linetype = "dashed", color = "gray20") + 
  labs(title="CDF Overlap",
       x="True mean rank",
       y="Calculated distance rank",
       size=16) +
  theme(text=element_text(family="Times"))

fig3.3 <- ggplot(d3) + 
  theme_bw(base_size = 20) +
  theme(panel.grid.minor = element_blank(),panel.grid.major = element_blank(),
        panel.border = element_rect(fill=NA,size=1,linetype="solid"),
        axis.text = element_text(size=12)) +
  geom_point(aes(x=true.rank,y=calc.rank)) +
  coord_cartesian(xlim=c(0,1000),ylim=c(0,1000)) +
  geom_abline(intercept = 0, slope = 1, linetype = "dashed", color = "gray20") + 
  labs(title="PDF Overlap",
       x="True mean rank",
       y="Calculated distance rank",size=16) +
  theme(text=element_text(family="Times"))

fig3.4 <- ggplot(d4) + 
  theme_bw(base_size = 20) +
  theme(panel.grid.minor = element_blank(),panel.grid.major = element_blank(),
        panel.border = element_rect(fill=NA,size=1,linetype="solid"),
        axis.text = element_text(size=12)) +
  geom_point(aes(x=true.rank,y=calc.rank)) +
  coord_cartesian(xlim=c(0,1000),ylim=c(0,1000)) +
  geom_abline(intercept = 0, slope = 1, linetype = "dashed", color = "gray20") + 
  labs(title="EMD",
       x="True mean rank",
       y="Calculated distance rank",
       size=16) +
  theme(text=element_text(family="Times"))

fig3.5 <- ggplot(d5) + 
  theme_bw(base_size = 20) +
  theme(panel.grid.minor = element_blank(),panel.grid.major = element_blank(),
        panel.border = element_rect(fill=NA,size=1,linetype="solid"),
        axis.text = element_text(size=12)) +
  geom_point(aes(x=true.rank,y=calc.rank)) +
  coord_cartesian(xlim=c(0,1000),ylim=c(0,1000)) +
  geom_abline(intercept = 0, slope = 1, linetype = "dashed", color = "gray20") + 
  labs(title="Difference in means",
       x="True variance rank",
       y="Calculated distance rank",
       size=16) +
  theme(text=element_text(family="Times"))

fig3.6 <- ggplot(d6) + 
  theme_bw(base_size = 20) +
  theme(panel.grid.minor = element_blank(),panel.grid.major = element_blank(),
        panel.border = element_rect(fill=NA,size=1,linetype="solid"),
        axis.text = element_text(size=12)) +
  geom_point(aes(x=true.rank,y=calc.rank)) +
  coord_cartesian(xlim=c(0,1000),ylim=c(0,1000)) +
  geom_abline(intercept = 0, slope = 1, linetype = "dashed", color = "gray20") + 
  labs(title="CDF Overlap",
       x="True variance rank",
       y="Calculated distance rank",
       size=16) +
  theme(text=element_text(family="Times"))

fig3.7 <- ggplot(d7) + 
  theme_bw(base_size = 20) +
  theme(panel.grid.minor = element_blank(),panel.grid.major = element_blank(),
        panel.border = element_rect(fill=NA,size=1,linetype="solid"),
        axis.text = element_text(size=12)) +
  geom_point(aes(x=true.rank,y=calc.rank)) +
  coord_cartesian(xlim=c(0,1000),ylim=c(0,1000)) +
  geom_abline(intercept = 0, slope = 1, linetype = "dashed", color = "gray20") + 
  labs(title="PDF Overlap",
       x="True variance rank",
       y="Calculated distance rank",
       size=16) +
  theme(text=element_text(family="Times"))

fig3.8 <- ggplot(d8) + 
  theme_bw(base_size = 20) +
  theme(panel.grid.minor = element_blank(),panel.grid.major = element_blank(),
        panel.border = element_rect(fill=NA,size=1,linetype="solid"),
        axis.text = element_text(size=12)) +
  geom_point(aes(x=true.rank,y=calc.rank)) +
  coord_cartesian(xlim=c(0,1000),ylim=c(0,1000)) +
  geom_abline(intercept = 0, slope = 1, linetype = "dashed", color = "gray20") + 
  labs(title="EMD",
       x="True variance rank",
       y="Calculated distance rank",
       size=16) +
  theme(text=element_text(family="Times"))

pdf("figure3.pdf",width=16,height=8)
multiplot(fig3.1,fig3.5,fig3.2,fig3.6,fig3.3,fig3.7,fig3.4,fig3.8, cols=4)
dev.off()

##### TABLE 2 #####
### calculate RMSE for ordinality
rmse.ord <- vector()
for(i in 1:8){
  tmp <- eval(parse(text=paste("d",i,sep="")))
  tmp$sqerr <- (tmp$true.rank - tmp$calc.rank)^2
  rmse.ord[i] <- sqrt(sum(tmp$sqerr)/nrow(tmp))
}
### calculate RMSE for cardinality
rmse.card <- vector()
for(i in 1:8){
  tmp <- eval(parse(text=paste("d",i,sep="")))
  tmp$sqerr <- (tmp$x - tmp$y)^2
  rmse.card[i] <- sqrt(sum(tmp$sqerr)/nrow(tmp))
}

### print the table
rmses <- c(rmse.ord,rmse.card)
rmses <- matrix(rmses,ncol=4,byrow=F)
dimnames(rmses) <- list(c("Diff in Means","CDF overlap","PDF Overlap","EMD"),
                        c("Ord: Mean","Ord: Variance", "Card: Mean",
                          "Card: Variance"))
round(rmses, 2)

### clean up
rm(list=ls()[!(ls() %in% c("dmeans","cdf.overlap","pdf.overlap","emd.dis",
                           "r2.corr.mer","multiplot","bimodalDistFunc","cl",
                           "samps.df.means","samps.df.vars","d1","d2","d3",
                           "d4","d5","d6","d7","d8","target"))])
