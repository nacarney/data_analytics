### DATA ANALYTICS ASSIGNMENT 5 ### 

## LOADING IN PACKAGES 
library(plyr)
library(readr)
library(dplyr)
library(caret)
library(ggplot2)
library(repr)
library("VIM")
library(MASS)
library(stats4)
library(expss)

set.seed = 123

setwd("/Users/nathancarney/Documents/College/4th Year/Data Analytics/Labs") 

## PART 1 ##

xi <- c(12.8, 10.5, 13.2, 13.0, 7.0, 11.0, 13.4, 13.2, 9.5, 11.0, 10.9, 4.6, 5.8, 3.2, 9.8, 0.2, 11.2,
        7.2, 14.7, 5.9, 9.7, 17.6, 8.5, 6.8, 7.2, 12.2, 16.7, 10.4, 14.2, 5.7)

mu_zero = 8

sigma_squared_zero = 4

alpha_zero = 5

beta_zero = 1

n = length(xi)

xbar = mean(xi)

tao_zero = 1/sigma_squared_zero

# my own initial estimate for tao
tao = 0.5

## PART 2 + PART 3 ## 

mews <- c()
sigma_squares <- c()
iterations <- 1:500

for(i in 1:500){
  
  mew = ((n*xbar*tao) + (mu_zero*tao_zero)) / (n*tao + tao_zero)
  
  tao_xi = ((n* tao) + tao_zero )^-1
  
  mu_samples <- rnorm(1000, mew, tao_xi)
  
  mu <- mean(mu_samples)
  
  mews <- c(mews, mu)
  
  shape <- alpha_zero + n/2
  
  S = sum((xi - mu)^2)
  rate <- beta_zero + S/2
  
  tao_samples <- rgamma(1000, shape = alpha_zero + n/2, rate = beta_zero + S/2)
  
  tao <- mean(tao_samples)
  
  sigma_square <- (n*tao + tao_zero)^-1
  
  sigma_squares <- c(sigma_squares, sigma_square)
  
}

mean(sigma_squares)

tao

# zoomed out plot shows mu's converging around the value 9.7
#plot(iterations, mews,  xlim=c(0, 500), ylim=c(0, 11))

#plot(iterations, sigma_squares)

# Final estimates for mu and sigma squared
#mean(mews)
#mean(sigma_squares)
