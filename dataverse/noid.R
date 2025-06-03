rm(list=ls())

# Set seed for replications purposes
set.seed(2024)

result <- as.data.frame(matrix(NA, nrow=1000, ncol=3))
names(result) <- c("MaxDupl", "Correct", "FalsePos")

for (i in 1:1000) 
{

# ------ Simulation of dataset ------
# Source: whuber (2018), Generating correlated binomial random variables. Accessed last on 02.04.2023 from https://stats.stackexchange.com/q/285008

# Compute Pr(0,0) from rho, p=Pr(X=1), and q=Pr(Y=1).
a <- function(rho, p, q) {
  rho * sqrt(p*q*(1-p)*(1-q)) + (1-p)*(1-q)
}

# Specify number of observations and variables in dataset (prior to adding duplicates)
n.sim <- 10500 # Number of observations
n.var <- 11  # Number of variables
data <- matrix(NA, nrow=n.sim, ncol=n.var)

# Specify parameters for the FIRST two variables (replicates "state of birth" and "state of employment")
n.options <- 50 # Number of variable categories or 'answer options'
p <- 1/3
q <- 1/4
rho <- 0.4985

# Compute the four probabilities for the joint distribution.
a.0 <- a(rho, p, q)
prob <- c(`(0,0)`=a.0, `(1,0)`=1-q-a.0, `(0,1)`=1-p-a.0, `(1,1)`=a.0+p+q-1)
if (min(prob) < 0) {
  print(prob)
  stop("Error: a probability is negative.")
}

# Generation of correlated Binomial variables.
round <- 1 # Reflecting the first, second, ... set of two variables

u <- sample.int(4, n.sim * n.options, replace=TRUE, prob=prob)
y <- floor((u-1)/2)
x <- 1 - u %% 2
data[,round*2-1] <- colSums(matrix(x, nrow=n.options)) # Sum in groups of `n`
data[,round*2] <- colSums(matrix(y, nrow=n.options)) # Sum in groups of `n`


# Specify parameters for the SECOND two variables (replicates "gender" and "race")
n.options <- 2 # Number of variable categories or 'answer options'
p <- 1/3
q <- 1/4
rho <- 0.0536

# Compute the four probabilities for the joint distribution.
a.0 <- a(rho, p, q)
prob <- c(`(0,0)`=a.0, `(1,0)`=1-q-a.0, `(0,1)`=1-p-a.0, `(1,1)`=a.0+p+q-1)
if (min(prob) < 0) {
  print(prob)
  stop("Error: a probability is negative.")
}

# Generation of correlated Binomial variables.
round <- 2 # variable reflecting the first, second, ... set of two variables

u <- sample.int(4, n.sim * n.options, replace=TRUE, prob=prob)
y <- floor((u-1)/2)
x <- 1 - u %% 2
data[,round*2-1] <- colSums(matrix(x, nrow=n.options)) # Sum in groups of `n`
data[,round*2] <- colSums(matrix(y, nrow=n.options)) # Sum in groups of `n`


# Specify parameters for the THIRD two variables (replicates "Employment Father" and "Employment Mother")
n.options <- 4 # Number of variable categories or 'answer options'
p <- 1/3
q <- 1/4
rho <- 0.2765

# Compute the four probabilities for the joint distribution.
a.0 <- a(rho, p, q)
prob <- c(`(0,0)`=a.0, `(1,0)`=1-q-a.0, `(0,1)`=1-p-a.0, `(1,1)`=a.0+p+q-1)
if (min(prob) < 0) {
  print(prob)
  stop("Error: a probability is negative.")
}

# Generation of correlated Binomial variables.
round <- 3 # variable reflecting the first, second, ... set of two variables

u <- sample.int(4, n.sim * n.options, replace=TRUE, prob=prob)
y <- floor((u-1)/2)
x <- 1 - u %% 2
data[,round*2-1] <- colSums(matrix(x, nrow=n.options)) # Sum in groups of `n`
data[,round*2] <- colSums(matrix(y, nrow=n.options)) # Sum in groups of `n`


# Specify parameters for the FOURTH two variables (replicates "Education Father" and "Education Mother")
n.options <- 5 # Number of variable categories or 'answer options'
p <- 1/3
q <- 1/4
rho <- 0.5493

# Compute the four probabilities for the joint distribution.
a.0 <- a(rho, p, q)
prob <- c(`(0,0)`=a.0, `(1,0)`=1-q-a.0, `(0,1)`=1-p-a.0, `(1,1)`=a.0+p+q-1)
if (min(prob) < 0) {
  print(prob)
  stop("Error: a probability is negative.")
}

# Generation of correlated Binomial variables.
round <- 4 # variable reflecting the first, second, ... set of two variables

u <- sample.int(4, n.sim * n.options, replace=TRUE, prob=prob)
y <- floor((u-1)/2)
x <- 1 - u %% 2
data[,round*2-1] <- colSums(matrix(x, nrow=n.options)) # Sum in groups of `n`
data[,round*2] <- colSums(matrix(y, nrow=n.options)) # Sum in groups of `n`


# Specify parameters for the FIFTH two variables (replicates "Age" and "Length in agency")
n.options <- 45 # Number of variable categories or 'answer options'
p <- 1/3
q <- 1/4
rho <- 0.3589

# Compute the four probabilities for the joint distribution.
a.0 <- a(rho, p, q)
prob <- c(`(0,0)`=a.0, `(1,0)`=1-q-a.0, `(0,1)`=1-p-a.0, `(1,1)`=a.0+p+q-1)
if (min(prob) < 0) {
  print(prob)
  stop("Error: a probability is negative.")
}

# Generation of correlated Binomial variables.
round <- 5 # variable reflecting the first, second, ... set of two variables

u <- sample.int(4, n.sim * n.options, replace=TRUE, prob=prob)
y <- floor((u-1)/2)
x <- 1 - u %% 2
data[,round*2-1] <- colSums(matrix(x, nrow=n.options)) # Sum in groups of `n`
data[,round*2] <- colSums(matrix(y, nrow=n.options)) # Sum in groups of `n`


# Add a final random variable  (replicates "education")
n.options <- 5 # Number of variable categories or 'answer options'
data[, n.var] <- sample(1:n.options, n.sim, replace = TRUE)

data <- as.data.frame(data)
cor(data)


# ------ Duplicate 950 observations and split sample over two "time periods" ------

Matched <- sample(1:n.sim, 950, replace=FALSE) # Randomly select 950 observations as matches
data$time <- 0
data$match <- 0
data.temp1 <- data[-Matched,]
temp <- sample(1:nrow(data.temp1), nrow(data.temp1)/2, replace=FALSE)
data.temp1$time[temp] <- 1
data.temp2 <- data[Matched,]
data.temp2$time <- 1
data.temp2$match <- 1
data.temp3 <- data[Matched,]
data.temp3$match <- 1

data <- rbind(data.temp1,data.temp2,data.temp3)
#summary(data)

# ------ Apply matching methodology ------

# Step 1: Set up function that amalgamates and filters all at once

panelmatch <- function(data, vars, at_least) {
  
  store <- paste0(paste(vars, collapse=", "))
  
  data %>%
    group_by(!!!syms(vars)) %>%
    dplyr::mutate(FULLSET = cur_group_id()) %>%
    group_by(FULLSET) %>%
    arrange(FULLSET, time) %>%
    filter(n() > (at_least - 1)) %>% # what frequency do we want to select
    mutate(count = n()) %>% # record how many times it occurs 
    ungroup()
}

## Instructions: 
# data = name of the dataset 
# vars = a character vector of the variables you want to match on
# at_least = the minimum number of matches you want to filter on. 
#            e.g. at_least = 2 means filter for where DUP occurs at least twice

library(dplyr)
characteristics <- c("V1", "V2", "V3", "V4", "V5", "V6", "V7", "V8", "V9", "V10", "V11")

Step4 <- panelmatch(data=data, vars=characteristics, at_least=2)
Step4 <- Step4[order(-Step4$count,Step4$FULLSET,Step4$time), ]

# ------ Calculate key characteristics of matched dataset ------
#max(Step4$count) # Maximum duplicates
#nrow(Step4[(Step4$match == 1 & Step4$count == 2), ]) / 2 # Correct matches
#nrow(Step4[(Step4$match == 0 & Step4$count == 2), ]) / 2 # False positives

result[i,1] <- max(Step4$count)
result[i,2] <- nrow(Step4[(Step4$match == 1 & Step4$count == 2), ]) / 2
result[i,3] <- nrow(Step4[(Step4$match == 0 & Step4$count == 2), ]) / 2

}

par(mfrow = c(1, 2))
hist(result[,2], main = "Correct Matches", xlab = "Number of correct matches", freq=TRUE, ylim = c(0,1000), breaks = c(940, 941, 942, 943, 944, 945, 946, 947, 948, 949, 950))
hist(result[,3], main = "False Positives", xlab = "Number of false positives", freq=TRUE, ylim = c(0,1000), breaks = c(0, 0.99, 1.99, 2.99, 3.99, 4.99, 5.99, 6.99, 7.99, 8.99, 9.99))
par(mfrow = c(1, 1))

table(result[,2])
table(result[,3])
