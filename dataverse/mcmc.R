library(dplyr)
library(MCMCpack)

df<- readRDS("df.RDS")
id <- 1:9
variable <- c("salary", "safety", "work", "environ", "time", "develop", "personal", "fair",  "welfare")
variables <- tibble(id, variable)

mcmcmodel <- MCMCfactanal(as.matrix(df[,names(df) %in% variables$variable]),
                          factors=1, 
                          lambda.constraints = list(salary=list(1,"+"),
                                                    safety=list(1,"+"),
                                                    work=list(1, "+"),
                                                    environ=list(1,"+"),
                                                    time=list(1,"+"),
                                                    develop=list(1,"+"),
                                                    peronal=list(1,"+"),
                                                    fair=list(1,"+"),
                                                    welfare=list(1,"+")
                          ),     
                          burnin = 1000, mcmc = 1000,
                          thin=1,
                          store.scores=T, verbose=1
)

saveRDS(mcmcmodel, file="mcmcmodel.RDS")
mcmcmodel <- readRDS(file="mcmcmodel.RDS")
mcmc <- data.frame(summary(mcmcmodel)$statistics)
mcmc_scores <- mcmc[grepl("phi.", row.names(mcmc)), ]

#Loadings
mcmc_summary <- data.frame(summary(mcmcmodel)[[1]])
mcmc_summary$variable <- rownames(mcmc_summary)
mcmc_summary <- mcmc_summary[grepl("phi_",mcmc_summary$variable)==F,]
mcmc_summary$parameter[grepl("Lambda",mcmc_summary$variable)] <- "discrimination"
mcmc_summary$parameter[grepl("Lambda",mcmc_summary$variable)==F] <- "difficulty"
mcmc_summary$variable <- gsub("Lambda","",mcmc_summary$variable)
mcmc_summary$variable <- gsub("Psi","",mcmc_summary$variable)
mcmc_summary$variable <- gsub("_1","",mcmc_summary$variable)
mcmc_summary$cilow <- mcmc_summary$Mean - 1.96*mcmc_summary$Naive.SE
mcmc_summary$cihigh <- mcmc_summary$Mean + 1.96*mcmc_summary$Naive.SE

mcmc_summary$variable_full <- factor(mcmc_summary$variable, 
                                     levels=unique(mcmc_summary$variable[order(mcmc_summary$Mean)]))


id <- 1:9
variable <- c("salary", "safety", "work", "environ", "time", "develop", "personal", "fair",  "welfare")
description <- c("salary", "safety", "work", "environ", "time", "develop", "personal", "fair",  "welfare")
variables <- tibble(id, variable, description)

mcmc_summary <- left_join(mcmc_summary, variables[,c("variable","description")])
mcmc_summary$description_full <- factor(mcmc_summary$description, 
                                        levels=unique(mcmc_summary$description[order(mcmc_summary$Mean)]))


# mcmc 그래프 그리기
ggplot(mcmc_summary[mcmc_summary$parameter=="discrimination",], 
       aes(x=variable, y=Mean)) +
  geom_point()  +
  geom_errorbar(aes(ymin=(Mean - 1.96*SD), 
                    ymax=(Mean + 1.96*SD)), width=0) +
  xlab("Variable") +
  ylab("Discrimination Parameter") +
  coord_flip() +
  geom_hline(yintercept=0, linetype=2) +
  theme_bw()


data <- cbind(df, mcmc_scores)


data <- data %>%
  mutate(companycat = as.character(companycat))

table(data$companycat)

plotdata <- data
plotdata$company <- NA
plotdata$company[plotdata$companycat=="1"] <- "Private"
plotdata$company[plotdata$companycat=="2"] <- "Public"
plotdata$company[plotdata$companycat=="3"] <- "Others"

plotdata2 <- plotdata %>%
  dplyr::group_by(year, company) %>%
  dplyr::summarise(job_sd = mean(SD, na.rm = TRUE), 
                   job_mean = mean(Mean, na.rm = TRUE))





### FIGURE. 직무만족 소속별 변화추이(1999-2022)
pdf("NC_highlight.pdf", h=4, w=8)
ggplot(plotdata2[!is.na(plotdata2$year),], aes(x=year, y=job_mean, size=company, color=company, 
                                               linetype=company)) +
  geom_ribbon(aes(ymin=job_mean - job_sd,
                  ymax=job_mean + job_sd), alpha = .3, color=NA) +
  geom_line(aes(group=company)) +
  # scale_linetype_manual(values=c(1,2,3)) +
  scale_color_manual(values=c("blue","black","red")) +
  scale_size_manual(values=c(1.2, 1.2, 1.2, 0.2), guide=F) +
  #geom_smooth(aes(group=component), se=F) +
  xlab("Year") +
  ylab("Job Satisfaction Score") +
  scale_x_continuous(breaks=seq(1999, 2022, by=2)) +
  theme_classic()
dev.off()

