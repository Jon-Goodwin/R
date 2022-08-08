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

both_na <- function(x,y){
  sum(is.na(x) &is.na(y))
}

# 6.

is_readable <- function(x) file.access(x, 4) == 0
# Checks if a file has read permission

is_directory <- function(x) file.info(x)$isdir
# checks if the path is a directory

# 7.

# #verse_n <- function(n){
#   foo_foo %>%
#   hop(through = forest) %>%
#   scoop( up = fieldmouse) %>%
#   bop(on = head) %>%
#   fair %>%
#   down %>%
#   little(bunny = foo_foo) %>%
#   dont(scoop = fieldmice) %>%
#   and(bop = head) %>%
#   give(chances = n) %>%
#   and(behave = dont) %>%
#   turn(into = goon)
# }
# 
# #fairy %>%
#   gave(chances = 3) %>%
#   now(turn = goon) %>%
#   POOF

### Functions are for humans and computers

# Excercises

# 1.
f1 <- function(string, prefix) {
  substr(string, 1, nchar(prefix)) == prefix
}
# returns whether the prefix of the string, matches the given prefix.
# better name would be has_prefix

f2 <- function(x) {
  if (length(x) <= 1) return(NULL)
  x[-length(x)]
}

#removes the last element of x, better anme might be remove_last

f3 <- function(x, y) {
  rep(y, length.out = length(x))
}
# replicates y to a vector the same length of x

# 2.
#

# 3.
# mvrnorm takes the covariate matrix Sigma instead of the standard deviation, which
# in a univariate case is the standard deviation.

# 4.
#norm_r() and norm_d() make it clear first off that you are using the normal
# distribution and the _ makes them easier to read the desired sub function

#rnorm and dnorm are shorter.

### Conditional Execution
has_name <- function(x) {
  nms <- names(x)
  if (is.null(nms)) {
    rep(FALSE, length(x))
  } else {
    !is.na(nms) & nms != ""
  }
}

# Excercises

# 1.
#ifelse() can take logical vectors and return a vector or values for each element.

# 2.
greeting <-
  if (hour(now()) <= 11){
  "Good Morning"
} else if (hour(now()) <= 18){
  "Good Afternoon"
} else {
  "Good Evening"
}

# 3.
fizzbuss <- function(x){
  if (near(x%% 15, 0)){
    "fizzbuzz"
  } else if (near(x%%3,0)){
    "fizz"
  } else if (near(x%% 5, 0)){
    "buzz"
  } else {
    x
  }
}

# 4.
if (temp <= 0) {
  "freezing"
} else if (temp <= 10) {
  "cold"
} else if (temp <= 20) {
  "cool"
} else if (temp <= 30) {
  "warm"
} else {
  "hot"
}

x <- seq(-10, 45, by = 10)
cut(x, breaks = c(-Inf,0,10,20,30, Inf),
    labels = c("freezing", "cold", "cool", "warm", "hot"))

# 5.
# with an integer the first argument is switched with the corresponding integer
# argument + 1

# 6.
switch(x,
       a = ,
       b = "ab",
       c = ,
       d = "cd"
)

# a and b evaluate to ab, c and d evaluate to cd. If there is no match returns
# nothing

switch("e",
       a = ,
       b = "ab",
       c = ,
       d = "cd"
)
# doesn't return anything.

### Function Arguments

# Excercises

