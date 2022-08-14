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

# 1.
# commas(letters, collapse = "-") causes an error, since the "..." argument passes
# both letters and collapse = "-" to str_c(..., collapse = ", ")

# 2.
rule("Title", pad = "-+")
# the problem is the length of pad has increased but the function 

rule <- function(..., pad = "-") {
  title <- paste0(...)
  width <- (getOption("width") - nchar(title) - 5)/str_length(pad)
  cat(title, " ", stringr::str_dup(pad, width), "\n", sep = "")
}

# implicitly assumes the length of pad to be 1, here I've divided the original
# width by the sstr_length of pad.

# 3.
x <- c(0,2,5,7,8,10,100,1000)

mean(x, trim = 0.5)

(5+7+8+10)/4
# trim, removes a proportion of the input vector from each end of the range. So
# at trim = 0.5, it removes the top 0.25 and bottom 0.25.

# 4.
# the method argument chooses the method to compute the correlation coefficient
# Pearson is the default computes the linearity of responses, while spearman 
# calculatures monotonicity

### Return Values

## Excercises

# 1.
# By the documentation is.finite() returns elements which are 
# (not infinite and not missing), where is.infinite() returns True only for Inf 
# and -Inf thus NA returns TRUE for !is.infinite(NA) and is.finite(NA) returns
# FALSE since NA means missing is not considered finite and is not infinite.

# 2.
# .Machine$double.eps^0.5 is the smallest possible floating point number that 
# 1 + x != 1, thus abs(x-y) <.Machine$double.eps^0.5 returns TRUE only if the
# distance from x to y is smaller then this precision. So near() essentially tells
# you that x and y are at least this close.

# 3.
# Largest number of possible distinct values is 2^32 since R uses 32-bit 
# representation for integers. One of these is NA_integer and one is used for the
# sign of the integer, +/-
# for dbls R has 64-bit representation and so has 2^64 different values possible
# one of them is NA_real_

# 4.

f <- function(x){
  as.integer(x)
}
# I'm not sure how else you could do this, some have interpreted this simply as 
# rounding a double to it's nearest integer value

# 5.
#parse_logical, parse_integer and parse_double

### Using Atomic Vectors

# Excercises

# 1.
# mean(is.na(x)) gives the proportion of NA values in x
mean(is.na(x))
# Gives the number of non finite members of x.
sum(!is.finite(x))

# 2.
# is.vector(x) will return FALSE if x has attributes other then names.
# is.atomic(x) is TRUE if x is any of the atomic types or NULL
is.atomic(NULL)

# 3.
#set_names() is equivalent to setNames() for most purposes but has some extended features
# for example, it will supply characters for names if no name argument is provided
x <- c(1,2,3)
setNames(x)
set_names(x)

# 4.
x <- c(1,2,3)
x <- set_names(x)
#a.
last_value <- function(x){
  x[[length(x)]]
}
# using [ will produce the name and the element contained, we only
# want the elemnt.

#b.
x <- 1:10
even_elements <- function(x){
  x[1:length(x) %% 2 == 0]
}

#c.
x <- 1:10
all_but_last <- function(X){
  x[1:length(x)-1]
}

#d.
even_numbers <- function(x){
  x[x %% 2 == 0]
}

# 5.
x[-which(x > 0)] # drops the indices corresponding to elements > 0.

x[x <= 0] # subsets the values which correspond to elements <= 0

# 6.
x <- -5:10
x[1000]
x <- set_names(x)
x["name"]
# subsetting with an index that doesnt exist results in NA and so does subsetting
# using a name that is not in the vector.

### Recursive Vectors(Lists)

# Excercises

# 1.
# Drawing

# 2.
x <- tibble(x = c(1,2,3), y = c(1,2,3))
y <- list(x = c(1,2,3), y= c(1,2,3))
# a list doesn't have dimension so subsetting such as y[1,1] doesn't have meaning
# and returns an error
x[1,1]
y[1,1]
# the main difference is that tibbles must have columns of the same length where
# lists can have elements of differing length, tibbles can however have elements
# that have different lengths within their own elements, like x_diff
x <- tibble(x = c(1,2,3), y = c(1,2))
x_diff <- tibble(x = c(1,2,3), y = list(list(1,2),2,3))
y <- list(x = c(1,2,3), y= c(1,2))
typeof(x) # shows a tibble is actually a list.
### Attributes

### Augmented Vectors

# Excercises

# 1.
x <- hms::hms(3600)
# prints as 01:00:00
typeof(x)
# type iss a double
attributes(x)
# > attributes(x)
# $units
# [1] "secs"
# 
# $class
# [1] "hms"      "difftime"

# 2.
y <- tibble(x = c(1,2,3), y = c(1,2))
# > y <- tibble(x = c(1,2,3), y = c(1,2))
# Error:
#   ! Tibble columns must have compatible sizes.
# • Size 3: Existing data.
# • Size 2: Column `y`.
# ℹ Only values of size one are recycled.
# Run `rlang::last_error()` to see where the error occurred.

# 3.
y <- tibble(x = c(1,2,3), list(1,2,3))
# No there is nothing wrong with having a list as a column as long as the list is
# of the correct length, or else an error occurs as below:
# > y <- tibble(x = c(1,2,3), list(1,2,3,3))
# Error:
#   ! Tibble columns must have compatible sizes.
# • Size 3: Existing data.
# • Size 4: Column at position 2.
# ℹ Only values of size one are recycled.
# Run `rlang::last_error()` to see where the error occurred.

### Iteration with purrr

### For Loops

# Excercises

# 1.
# a.
mean_mtcars <- vector("double", ncol(mtcars))
names(mean_mtcars) <- names(mtcars)
for (i in seq_along(mtcars)){
  mean_mtcars[i] <- mean(mtcars[[i]])
}
mean_mtcars

#b.
types_flights <- vector("character", ncol(flights))
names(types_flights) <- names(flights)
for(i in seq_along(flights)){
  types_flights[i] <- class(flights[[i]])
}
types_flights

# c.
unique_values <- vector("double", ncol(iris))
names(unique_values) <- names(iris)
for (i in seq_along(iris)){
  unique_values[i] <- nrow(unique(iris[i]))
}
unique_values

# d.
n <- 10
normals <- c(-10,0,10,100)
x <- vector("double", n)
rand_normals <- list(x,x,x,x)
names(rand_normals) <- names(set_names(normals))
for (i in seq_along(normals)){
  rand_normals[[i]] <- rnorm(n, mean = normals[i])
}

# 2.
out <- ""
for (x in letters) {
  out <- stringr::str_c(out, x)
}
# alternative
str_c(letters, collapse = "")

x <- sample(100)
sd <- 0
for (i in seq_along(x)) {
  sd <- sd + (x[i] - mean(x)) ^ 2
}
sd <- sqrt(sd / (length(x) - 1))
# alternative
SSE <- sum((x-mean(x))^2)
s_dev <- sqrt(SSE/(length(x)-1))

x <- runif(100)
out <- vector("numeric", length(x))
out[1] <- x[1]
for (i in 2:length(x)) {
  out[i] <- out[i - 1] + x[i]
}
#alternative
cumsum(x)

# 3.
alice_the_camel <- function(n){
  a = c(n:1,"no")
  Go <- "So go, Alice, go!"
  Boom <- "Boom, boom, boom, boom!"
  Horse <- "'Cause Alice is a horse, of course!"
  for (i in seq_along(a)){
    replicate(3, print(str_c("Alice the camel has ", as.character(a[i]), " humps.")))
    print(Go)
    if (i != length(a)){
      print(Boom)
    } else {print(Horse)}
  }
}

ten_in_the_bed <- function(n = 10, s = "bed"){
  a <- as.character(n:1)
  Little <- "And the little one said,"
  Roll_over <- "Roll over! Roll over!"
  Falls_out <- "So they all rolled over and one fell out"
  for (i in seq_along(a)){
    print(str_c("There ", ifelse(i != length(a), "were ", "was "),
                a[i]," in the ", s))
    print(Little)
    if (i != length(a)){
      print(Roll_over)
      print(Falls_out)
    } else{
      print("Alone at last!")
      print("Good Night!")
      }
  }
}

bottles_of_beer <- function(n = 99, vessel = "bottle", liquid = "beer",
                            surface = "wall"){
  a <- c(n:1,"no")
  for (i in seq_along(a)) {
    if (i < length(a)-1){
    repeater <- str_c(a[i]," ",vessel,"s"," of ", liquid)
    cat(str_c(repeater, " on the ", surface, ", ", repeater,".\n"))
    cat(str_c("Take one down and pass it around, ", a[i+1], " ", vessel, "s",
              " of ", liquid, " on the ", surface, ".\n"))
    }
    else if (i == length(a)-1){
      repeater <- str_c(a[i]," ",vessel," of ", liquid)
      cat(str_c(repeater, " on the ", surface, ", ", repeater,".\n"))
      cat(str_c("Take one down and pass it around, no more ", vessel, "s",
                " of ", liquid, " on the ", surface, ".\n"))
    }
    else {
      cat(str_c("No more ",vessel,"s", " of ", liquid, " on the ", surface,
                " no more ", vessel, "s", " of ", liquid, ".\n"))
      cat("Go to the store and buy some more, ", a[1], vessel, "s", " of ", liquid,
          " on the ", surface)
      }
  }
}

# 4.
fun_1 <- function(n){
  x = sample(n)
  output <- vector("integer", 0)
  for (i in seq_along(x)) {
    output <- c(output, lengths(x[[i]]))
  }
  output
}
fun_2 <- function(n){
  x <- sample(n)
  output_2 <- vector("integer", length(x))
  for (i in seq_along(x)) {
    output_2[i] <- lengths(x[[i]])
}
output_2
}
microbenchmark(fun_1(10000),fun_2(10000), times = 5)

### For Loop Variations

#Excercises

# 1.
files <-
  dir(getwd(), pattern = "\\.csv$", full.names = TRUE)
data <- vector("list", length(files))
for (i in seq_along(files)){
  data[[i]] <- read_csv(files[[i]], show_col_types = FALSE)
}
bind_rows(data)

# 2.
i = 0
x <- c(1,2,3)
for (nm in names(x)) {i + i+1}
# return i = 0, nothing happens because names(x) == NULL
x <- c(a=1,b=2,NA = 3)
for (nm in names(x)) {i + i+1}
# returns 3 since names(x) gives
# > names(x)
# [1] "a" "b" "" 
# the name of the unnamed variable

# 3.
show_means <- function(x){
  y <- x[,sapply(x,is.numeric)]
  y <- sapply(y,mean)
  y <- round(y, digits = 3)
  names <- names(y)
  for (i in seq_along(y)){
    cat(str_c(str_pad(str_c(names[i],":"), 14, "right"),y[[i]]), sep = "\n")
  }
}
# I used str_pad to get the width correct

# 4.
trans <- list(
  disp = function(x) x * 0.0163871,
  am = function(x) {
    factor(x, labels = c("auto", "manual"))
  }
)
for (var in names(trans)) {
  mtcars[[var]] <- trans[[var]](mtcars[[var]])
}
# trans is a list of functions, disp and am. disp multiplies the argument by
# 0.0163871 and am creates a factor with levels "auto" and "manual"
# the loop is over the names of trans, "disp" and "am" so when at "disp"
# trans[[disp]] gives the argument mtcars[[disp]] to the function disp inside trans.
# similarly for "am". the function multiplies the displacement and converts the
# values of the am, 1 for auto, 0 for manual to characters.

### For Loops Versus Functionals

# Excercises

# 1.
# By the documentation apply generalizes the use of column and row indices as well
# as being able to select dimension names in the MARGIN arugment.

# 2.
col_summary <- function(df, fun) {
  y <- df[,apply(df,2,is.numeric)]
  out <- vector("double", length(y))
  for (i in seq_along(y)) {
    out[i] <- fun(y[[i]])
  }
  out
}

### The Map Functions

# Excercises

# 1.
map_dbl(mtcars, mean)
map_chr(flights, typeof)
map(iris,unique) %>% map_dbl(length)
c(-10,0,10,100) %>%
  map(rnorm, n=10)

# 2.
vec <- map_lgl(iris, is.factor)

# 3.
set.seed(1)
map(1:5, runif)
# randomly samples 1,2,3,4,5 from a distribution U(0,1). 1:5 is being used for the
# first argument of runif, n.
set.seed(1); map(list(rep(1,5)), runif) # alters to a list containing 1 vector of length 5.
set.seed(1); map(5, runif) # runif treats 1:5 as taking n = 5.

# 4.
# Consider
map(-2:2, ~ {
  set.seed(1)
  rnorm(n=5, mean = .x)
})
# gives the list of vectors rnorm(-2, n = 5), rnorm(-1, n= 5) where -2:2 is being
# applied to the next available argument, mean.
map_dbl(-2:2, rnorm, n = 5) #produces an error because map_dbl returns an atomic vector
map_dbl(-2:2, rnorm, n = 1) # produces an atomic vector of length 5 where each element
# corresponds to iterating over -2:2 for the mean.

# 5.
lin_m <- function(df) lm(mpg ~ wt, data = df)
x <- mtcars %>%
  split(.$cyl)
map(x, lin_m)
# Not sure if this is what was meant, the only other way would be to do what was done
# in the text using this example
models <- mtcars %>%
  split(.$cyl) %>%
  map(~lm(mpg ~ wt, data = .))

### Other Patterns of For Loops

# Excercises

# 1.
for_all <- function(x, fun, ...){
  vec <- vector("logical", length(x))
  for(i in seq_along(x)){
    vec[i] <- fun(x[[i]], ...)
  }
  all(vec)
}
# for_all seems to agree for every test I can think of.

# 2.
col_sum <- function(df, fun, ...){
  data <- df %>%
    keep(is.numeric)
  map_dbl(data,fun, ...)
}

# 3.
col_sum3 <- function(df, f) {
  is_num <- sapply(df, is.numeric)
  df_num <- df[, is_num]
  sapply(df_num, f)
}
df <- tibble(
  x = 1:3,
  y = 3:1,
  z = c("a", "b", "c")
)
col_sum3(df[1:2], mean)
col_sum3(df[1], mean)
col_sum3(df[0], mean)
# Unlear what the bugs are supposed to be

# 4.