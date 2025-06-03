library(tidyverse)
getSei <- function(df="seidf.RData") {
  filename <- paste0(df)
  load(df)
  df <- df %>%
    mutate(ages = scale(age),
           edus = scale(edu),
           occup = factor(occup),
           incomes = scale(income))
  summary(df)
  df$sei <- 0.5
  # step 1
  ## regress income on age and sei
  
  res1 <- lm(formula = incomes~sei + ages -1, data=df)
  summary(res1)
  
  b41<- res1$coefficients['ages']
  b43<- res1$coefficients[-length(res1$coefficients)]
  df$b41 <- b41
  df$b43 <- b43
  ## regress sei on edu and age
  
  res2 <- lm(formula = sei ~ ages + edus -1, data=df)
  summary(res2)
  b31<- res2$coefficients[1]
  b32<- res2$coefficients[2]
  df$b31 <- b31
  df$b32 <- b32
  
  ## regress Edu on Age
  res3 <- lm(formula = edus ~ ages -1, data=df)
  summary(res3)
  b21<-res3$coefficients[1]
  df$b21 <- b21
  
  
  # Step 3 
  ## compte SEI
  df <- df %>%
    mutate(seih = b43*(incomes - b41*ages) + b32*edus + b31*ages)%>%
    mutate(seih = scale(seih))
  ## standardize sei
  
  dfseihsh<- df %>%
    group_by(occup) %>%
    summarise(seihsh = mean(seih)) 
  
  df <- left_join(df, dfseihsh, by= "occup")
  # regress inc on age, edu, sei
  
  
  (res4fmla <- as.formula(paste("incomes ~ ages + edus + seihsh -1")))
  res4 <-lm(formula = res4fmla, data= df)
  summary(res4)
  
  b42<-res4$coefficients['edus']
  
  dfb41<-as.data.frame(b41)
  dfb43<-as.data.frame(b43)
  dfb31<-as.data.frame(b31)
  dfb32<-as.data.frame(b32)
  dfdfseihsh<- dfseihsh
  dfb42<-as.data.frame(b42)
  
  
  itr.n <- 100
  for (i in 1:itr.n) {
    ## regress income on age and sei
    df$seih <- df$seihsh
    (res1fmla <- as.formula(paste("incomes ~ seih + ages", "-1")))
    
    res1 <- lm(formula = res1fmla, data=df)
    
    b41<- res1$coefficients['ages']
    dfb41<-rbind(dfb41, b41)
    df$b41<-b41
    b43<- res1$coefficients[-length(res1$coefficients)]
    df$b43<-b43
    
    dfb43<-rbind(dfb43, b43)
    
    
    ## regress sei on edu and age
    
    res2 <- lm(formula = seih ~ ages + edus -1, data=df)
    summary(res2)
    
    b31<- res2$coefficients[1]
    dfb31<-rbind(dfb31, b31)
    df$b31 <- b31
    b32<- res2$coefficients[2]
    dfb32<-rbind(dfb32, b32)
    df$b32 <- b32
    
    # Step 3 
    ## compte SEI
    df <- df %>%
      mutate(seih = b43*(incomes - b41*ages) + b32*edus + b31*ages) %>%
      mutate(seih = scale(seih))
    ## standardize sei
    
    
    dfseihsh<- df %>%
      group_by(occup) %>%
      summarise(seihsh = mean(seih)) 
    
    
    df <- df %>%
      select(-c(seihsh))
    df <- left_join(df, dfseihsh, by= "occup")
    dfseihsh <- dfseihsh[-1]
    names(dfseihsh)[names(dfseihsh)=="seihsh"] <- paste0("seihsh", i)
    dfdfseihsh<-cbind(dfdfseihsh, dfseihsh)
    
    # regress inc on age, edu, sei
    res4 <-lm(formula = res4fmla, data= df)
    b42<-res4$coefficients['edus']
    dfb42<-rbind(dfb42, b42)
  }
  return(list(df, dfb43, dfb42, dfb41, dfb32, dfb31, dfdfseihsh))
}  