library(magrittr) #for pipes
library(MASS)     #for the cats and bacteria data
library(mice)     #for the boys data
library(plyr)
library(dplyr)
library(magrittr)
library(ggplot2)

#set random seed, make things reproducible
set.seed(123)

#1
studydesign <- matrix(NA, nrow = 7, ncol = 4) #make an empty matrix
rownames(studydesign) <- paste("manipulation", 1:7, sep=" ") #name row names
colnames(studydesign) <- paste("condition", 1:4, sep=" ") #name column names

#place the numbers 1-7 in each column in a random order
for (i in 1:ncol(studydesign)) {
  studydesign[ ,i] = sample(1:7, replace = FALSE)
}

#2 generate a vector of 100 random standard normal numbers
randvec <- rnorm(n = 100, mean = 0, sd = 1)

#3
meanvec <- mean(randvec)
sdvec <- sd(randvec)

#4
#option 1
# Repeat the sampling of numbers 25 times, each time getting the mean.
av <- numeric(25)
for(i in 1:25) {
  av[i] <- mean(rnorm(100))
}

# Standard error of sample mean.
sd(av) 

#option 2
# rlply evaluates an expression n times and combines the results in a list.
samples <- rlply(.n = 25, .expr = rnorm(n = 100, mean = 0, sd = 1))
av2 <- sapply(X = samples, FUN = mean)
sd(av2)

#5
#create a function that returns the sd for the sample means
mean.av <- function(n = 100, reps = 25) {
  #start with making an empty variable with the length of reps
  av <- numeric(reps)
  #then draw a sample of 100 from a normal distribution and store the mean of these values in av
  for (i in 1:reps){
    av[i] <- mean(rnorm(n = n, mean = 0, sd = 1))
  }
  #return the vector of means at the end of the function
  return(av)
}

sd(mean.av())

#option 2
mean.av <- function(n = 100, reps = 25) {
  rlply(.n = reps, rnorm(n, mean = 0, sd = 1)) %>%
    sapply(mean)
}

sd(mean.av())


#6
#create a function that returns the sd for the sample means
mean.av <- function(n = 100, reps = 25, DensPlot = TRUE) {
  #start with making an empty variable with the length of reps
  av <- numeric(reps)
  #then draw a sample of 100 from a normal distribution and store the mean of these values in av
  for (i in 1:reps){
    av[i] <- mean(rnorm(n = n, mean = 0, sd = 1))
  }
  
  if (DensPlot) {
    plot(density(av), main = "Sampling distribution of the mean.")
  }
  
  #return the vector of means at the end of the function
  return(av)
}

sd(mean.av())

#option 2
mean.av <- function(n = 100, reps = 25, plotDens = TRUE) {
  av <- 
    rlply(.n = reps, rnorm(n, mean = 0, sd = 1)) %>%
    sapply(mean) 
  if (plotDens) {
    density(av) %>%
      plot(main = "Sampling distribution of the mean.")
  }
  return(av)
}

sd(mean.av())

#7 Generate a random sample of size 20 from a normal population with mean 100 and standard deviation 10.
randsample <- rnorm(n = 20, mean = 100, sd = 10)

#8
par(mfrow=c(3,4))
for (i in 1:3) {
  for (j in 1:4){
    qqnorm(rnorm(n = 10^i))
  }
}

#9 sample from uniform distribution (uniform = equal chance of being sampled)
par(mfrow=c(3,4))
for (i in 1:3) {
  for (j in 1:4){
    qqnorm(runif(n = 10^i))
  }
}
par(mfrow=c(1, 1))


#10
#simulate 100 exponential numbers
sample <- rexp(n = 100, rate = 0.2)

#density plot
plot(density(sample, from = 0), main = "Exponential with rate = 0.2")

#compare sample with population mean
c("sample mean" = mean(sample), "population mean" = 1/0.2)

#11
#linear model 1
fit1 <- anscombe %$%
  lm(y1 ~ x1)
#linear model 2
fit2 <- anscombe %$%
  lm(y2 ~ x2)
#linear model 3
fit3 <- anscombe %$%
  lm(y3 ~ x3)
#linear model 4
fit4 <- anscombe %$%
  lm(y4 ~ x4)


#12
coeffs <- data.frame(fit1 = coef(fit1),
                     fit2 = coef(fit2),
                     fit3 = coef(fit3),
                     fit4 = coef(fit4))
row.names(coeffs) <- names(coef(fit1))
coeffs

#13 plot all points in 1 plotting model!
par(mfrow = c(1, 1))
plot(y1 ~ x1, col = "blue", data = anscombe)
points(y2 ~ x2, col = "gray", data = anscombe)
points(y3 ~ x3, col = "orange", data = anscombe)
points(y4 ~ x4, col = "purple", data = anscombe)

#14
par(mfrow = c(2, 2))
plot(y1 ~ x1, data = anscombe)
plot(y2 ~ x2, data = anscombe)
plot(y3 ~ x3, data = anscombe)
plot(y4 ~ x4, data = anscombe)
#14