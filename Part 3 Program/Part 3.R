### PART 3 ####
library(magrittr)

### Functions

# Excercises

# 1.
# TRUE is the value of the argument within range. If na.rm was FALSE and x
# contained an NA value then the Range would indicate NA NA since the max and 
# min are unknown

# 2.
x <- c(1:10, Inf, -Inf)

rescale01 <- function(x) {
  rng <- range(x, na.rm = TRUE, finite = T)
  (x - rng[1]) / (rng[2] - rng[1])
  for (i in 1:length(x)){
    if (x[i] == Inf){
      x[i] = 1
    }
    else if (x[i] == -Inf){
      x[i] = 0
    }
  }
  x
}

# 3.

mean_x <- function(x){
  y <- is.na(x)
  sum(y)/length(y)
}

x_norm <- function(x){
  x / sum(x, na.rm = T)
}

x_sd <- function(x, na.rm = TRUE){
  sd(x, na.rm = na.rm) / mean(x, na.rm = na.rm)
}

# 4.
#Variance function

my_var <- function(x){
  n <- length(x)
  m <- mean(x)
  sum((x-m)^2)/(n-1)
}

my_skew <- function(x){
  n <- length(x)
  m <- mean(x)
  num <- sum((x-m)^3)/n
  denom <- (my_var(x))^(3/2)
  num/denom
}

skewness <- function(x) {
  n <- length(x)
  v <- var(x)
  m <- mean(x)
  third.moment <- (1/(n - 2)) * sum((x - m)^3)
  third.moment/(var(x)^(3/2))
}

# 5.