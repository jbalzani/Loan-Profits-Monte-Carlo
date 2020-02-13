#Modeling loan profits
#scope - make a general model for loan expvalues, and how they vary as economic activity
#changes the default rate

#out of scope - money earned from foreclosure of houses or recovery of assets, other expenses
#current market interest or default rates, specific home values, monthly payments

library(tidyverse)
#Parameters
n <- 10000  # number of loans in sample
B <- 10000 #number of repetitions of Monte Carlo simulation
i <- .025 #interest rate
p <- .02 # probability of default
l <- -200000 #average loan amount, loss per loan in case of default
g <- -l*i #profit per loan at the end of yr 1

####interest rate calculation for 1% chance of losing money####
set.seed(1)
z <- qnorm(0.01)

#formula & explanation given in harvardx big short interest rate video
a <- -l*( n*p - z*sqrt(n*p*(1 - p))) / ( n*(1 - p) + z*sqrt(n*p*(1 - p))) 
a    # required profit when loan is not a default
ir <- a/-l 

#check with mc
mc <- replicate(B, {
  loan <- sample(c(l, a), n, replace = TRUE, prob = c(p, 1 - p))
  sum(loan)
})
mean(mc)
mean(mc < 0)

####number of loans needed for a 1% chance of losing money####
#formula doesn't work for interest rates that result in losing money
#formula & explanation given in harvardx big short video
n2 <- ceiling((z^2*(g-l)^2*p*(1-p)) / (l*p + g*(1-p))^2)
n2    # number of loans required
n2*(l*p + g*(1-p))    # expected profit over n loans

mc_2 <- replicate(B, {
  loan <- sample(c(l, g), n2, replace = TRUE, prob = c(p, 1-p))
  sum(loan)
})
mean(mc_2 < 0)

####simulation of case where economic activity changes default rate####
set.seed(1)
#monte carlo
loan_profits <- replicate (B, { #vector B columns long, each col is the sum of n loans
  new_p <- p + sample(seq(0, .01, length = 100), 1)
  expvalues <- sample(c(l, a), n, replace = TRUE, prob = c(new_p, 1 - new_p)) 
  sum(expvalues) #sum of n loans
})

#visualize the results
loan_profits_in_millions <- loan_profits/10e6
hist(loan_profits_in_millions, main = "Histogram of Loan Profits - Variable Default Rate",
     xlab = "Loan Profits in Millions") 

mean(loan_profits) #on average, how much $ you make from n loans, simulated B times
sd(loan_profits)    #standard deviation of monte carlo sim
expvalue_per_loan <- mean(loan_profits)/n #expvalue per loan
expvalue_per_loan
mean(loan_profits < 0)
