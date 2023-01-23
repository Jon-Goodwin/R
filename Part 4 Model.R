### Part 4 Model

### Model Basics with modelr

## A Simple Model

# Excercises
model1 <- function(a, data) {
  a[1] + data$x * a[2]
}
# 1.
sim1a <- tibble(
  x = rep(1:10, each = 3),
  y = x * 1.5 + 6 + rt(length(x), df = 2)
)
sim2a <- tibble(
  x = rep(1:10, each = 3),
  y = x * 1.5 + 6 + rnorm(length(x))
)
lm(y ~ x, data = sim1a) %>% summary()
lm(y ~ x, data = sim2a) %>% summary()
# Because the t distribution is wider then the normal, it's more prone to outliers
# we see in the model the R-Squared of the model using normals is much higher

# 2.
measure_distance1 <- function(mod, data) {
  diff <- data$y - model1(mod, data)
  mean(abs(diff))
}
measure_distance <- function(mod, data) {
  diff <- data$y - model1(mod, data)
  sqrt(mean(diff^2))
}
best <- optim(c(1, 1), measure_distance1, data = sim1)
best2 <- optim(c(0,0), measure_distance, data = sim1)
best
best2
# The main difference here is the choice of minimizing the residual value.
# The first method was essentially the euclidean distance, the mean absolute value
# can never be larger then the euclidean distance. And because the euclidean distance
# is squaring the elements of the diff vector, the result is more sensitive to outliers.

# 3.
model1 <- function(a, data) {
  a[1] + data$x * a[2] + a[3]
}
# The problem would be that for a particular pair (a[1] = a, a[3] = b), the pair
# (a[1] = b, a[3] = a) gives the same result. so there isn't a unique solution
# to minimizing the distance.

### Visualizing Models
grid <- sim1 %>%
  data_grid(x)

# Excercises

# 1.
sim1_mod <- loess(y ~ x, data = sim1)
grid <- sim1 %>%
  data_grid(x)
grid <- grid %>%
  add_predictions(sim1_mod)
ggplot(sim1, aes(x)) +
  geom_point(aes(y = y)) +
  geom_line(
    aes(y = pred),
    data = grid,
    colour = "red",
    size = 1
  ) +
  geom_smooth(aes(y = y))
# geom_smooth is exactly loess as that is the default method of geom_smooth
sim1 <- sim1 %>%
  add_residuals(sim1_mod)
ggplot(sim1, aes(x, resid)) +
  geom_ref_line(h = 0) +
  geom_point()
# the plot of the residuals is similar to the original plot, much noise no clear
# pattern in the residuals.

# 2.
# spread and gather are similar to the previous spread and gather from dplyr
# add_predictions takes only 1 model in its argument and creates a single column
# of predictions. spread_predictions  creates 2 and takes 2 models, gather takes
# 2 models but instead of creating a new column for prediction of model 2
# it instead creates a new column with values m2 and m1 and a predictions column
# for both models, making the data longer where spread makes it wider.

# 3.
# geom_ref_line comes from modelr. it gives a line through the desired horizontal
# or vertical lines. It's useful in a residual plot because it allows us to see
# points which were over and under estimated by the predictions.
sim1 <- sim1 %>%
  add_residuals(sim1_mod)
ggplot(sim1, aes(x, resid)) +
  geom_ref_line(h = 0) +
  geom_point()

# 4.
ggplot(sim1, aes(abs(resid))) +
  geom_freqpoly(binwidth = 0.5)
# A frequency polygon of absolute residuals allows you to see the magnitude of
# how much the predictions deviate from the observed values.

### Formulas and Model Families.

# Excercises 

# 1.
mod2 <- lm(y ~ x, data = sim2)
lm_sim2 <- lm(y ~ x-1, data = sim2)
grid <- sim2 %>%
  data_grid(x) %>%
  add_predictions(lm_sim2)
grid2 <- sim2 %>%
  data_grid(x) %>%
  add_predictions(mod2)
grid
grid2
mod2
lm_sim2

# The predictions are the same, the coefficients are not, if we look at the
# model_matrix of both we see why.
model_matrix(sim2, y ~ x) %>% print(n=40)
model_matrix(sim2, y ~ x - 1) %>% print(n=40)
### In the model without intercept the intercept column of all 1's, is replaced
# with the column for category a, of 1's for a, and 0's everywhere else.
# in the model with intercept, the intercept is being added to the coefficients
# of each category, for example the prediction for b is 8.116=1.152+6.9639, because
# of the lack of intercept in the no intercept model, this prediction just becomes
# the coefficient.

# 2.
mod1_sim4 <- lm(y ~ x1 + x2, data = sim4)
mod2_sim4 <- lm(y ~ x1 * x2, data = sim4)
mod1_sim3 <- lm(y ~ x1 + x2, data = sim3)
mod2_sim3 <- lm(y ~ x1 * x2, data = sim3)
model_matrix(sim3, y ~ x1 + x2)
model_matrix(sim3, y ~ x1 * x2)
# in the interaction model, the effect of x1 on the result is now dependent on
# the values of x2 through multiplication.

# 3.
mod1_new <- lm(y ~ x1 + x2, data = sim3)
model_1 <- function(x){
  df <- x
  df %>%
    mutate(
      'Intercept' = 1,
      'x2b' = as.numeric(x2 == 'b'),
      'x2c' = as.numeric(x2 == 'c'),
      'x2d' = as.numeric(x2 == 'd')
    ) %>%
    select(Intercept, x1, x2b,x2c,x2d)
}

model_2 <- function(x){
  df <- x
  df %>%
    mutate(
      'Intercept' = 1,
      'x2b' = as.numeric(x2 == 'b'),
      'x2c' = as.numeric(x2 == 'c'),
      'x2d' = as.numeric(x2 == 'd'),
      'x1:x2b' = x1*as.numeric(x2 == 'b'),
      'x1:x2c' = x1*as.numeric(x2 == 'c'),
      'x1:x2d' = x1*as.numeric(x2 == 'd')
    ) %>%
    select('Intercept', 'x1', 'x2b','x2c','x2d','x1:x2b','x1:x2c','x1:x2d')
}

# 4.
mod1 <- lm(y ~ x1 + x2, data = sim4)
mod2 <- lm(y ~ x1 * x2, data = sim4)
grid <- sim4 %>%
  gather_residuals(mod1, mod2) %>%
  select(model, x1, x2, y, resid)
ggplot(grid, aes(x = resid, color = model)) +
  geom_freqpoly(binwidth = 0.5)
summary(mod1)
summary(mod2)
# R squared of mod2 is slightly better, but visually I don't
# see that mod2 is noticeably different from mod1 and they are very close
# in both r-squared and in the standard deviation of the residuals.

### Model Building

grid <- diamonds2 %>%
  data_grid(carat = seq_range(carat, 20)) %>%
  mutate(lcarat = log2(carat)) %>%
  add_predictions(mod_diamond, "lprice") %>%
  mutate(price = 2 ^ lprice)

ggplot(diamonds2, aes(carat, price)) +
  geom_hex(bins = 50) +
  geom_line(data = grid, color = "red", size = 1)


### A more Complicated Model

# Excercises

# 1.
# they seem to be nice fractions of 1 carat. For example at l(carat) of 0, that
# means a carat of 1, the next strip down is 0.75 of 1 carat, then 1/2 of a carat

# 2.
# If lprice = a + b * lcarat then price = A * carat^b

lm_1 <- lm(log(price) ~ log(carat), data = diamonds)

grid_1 <- diamonds %>%
  data_grid(carat = seq_range(carat, 20)) %>%
  add_predictions(lm_1, "lprice") %>%
  mutate(price_pred = exp(1) ^ lprice)

coef(lm_1)

# for example carat = 4.00, price = (exp(1) ^ 8.448661)*(4^1.675816)=47659.41
# Check in grid_1 is 47607.
# > grid_1[[16,1]]
# [1] 3.997368

# 3.
diamonds2 %>%
  filter(abs(lresid2) > 1.5)
# The only interesting thing seems to be that all the diamonds with large residuals
# all have positive residuals meaning they sold for much higher then expected.
# it could be this data does not capture something about these particular
# diamonds that made them more valuable, or the data could simply be innacurate.
diamonds2 %>%
  filter(lresid2 < -1)
# this table only has 2 values, one of them is an unusually large diamond at 2.46
# which could contribute to the large negative residual, the other has no real explanation.

# 4.
mod_diamond1 <- lm(lprice ~ lcarat, data = diamonds2)
summary(mod_diamond2)
summary(mod_diamond1)
# the 2nd model is a better model for price, it has a fairly low residual st error
# and a high R-squared value, although the first model based only on carat isn't bad
# this model is performing better.

### What affects the number of daily flights

daily <- flights %>%
  mutate(date = make_date(year, month, day)) %>%
  group_by(date) %>%
  summarize(n = n())
daily

# Excercises

# 1.
# These all correspond to the day before a holiday. September 2 2013 was Labor Day
# May 27 2013 was Memorial Day and January 21 2013 was martin luther king jr day/

# 2. November 28 was thanksgiving day in United States in 2013. So the larger then
# expected flights on Sat and Sun are likely due to people flying home from meeting
# family. Similary December 28 2013 is the first saturday after christmas day.

# 3.
daily2 <- daily %>%
  dplyr::select('date', 'n', 'wday') %>%
  mutate(term_sat = case_when(wday == 'Sat' ~ str_c('Sat_',term(date)),
                           TRUE ~ as.character(wday)), term = term(date))
mod2 <- lm(n ~ wday * term_sat, data = daily2)
mod1 <- lm(n ~ wday, data = daily2)
mod3 <- lm(n ~ wday * term, data = daily2)
daily2 %>%
  gather_residuals(without_term = mod1, with_term = mod2) %>%
  ggplot(aes(date, resid, color = model)) +
  geom_line(alpha = 0.75)
daily2 %>%
  gather_residuals(with_term = mod3, with_term_sat = mod2) %>%
  ggplot(aes(date, resid, color = model)) +
  geom_line(alpha = 0.75)
# The model only accounting for terms for summer isn't as good as the model
# with terms for every day.

# 4.
Holidays <- ymd(c("2013-01-01", "2013-01-21", "2013-02-13", "2013-02-18",
                  "2013-05-27", "2013-07-04", " 2013-09-02", "2013-10-14",
                  "2013-11-11", "2013-11-28", " 2013-12-25"))
daily3 <- daily2 %>%
  mutate(wday3 = case_when(date %in% Holidays ~ str_c(as.character(wday),"_Holiday"),
                           wday == 'Sat' ~ str_c('Sat_', term(date)),
                           TRUE ~ as.character(wday)))
mod3_4 <- lm(n ~ wday * term, data = daily3)
mod2_4 <- lm(n ~ wday * term_sat, data = daily3)
mod4 <- lm(n ~ wday * wday3, data = daily3)
daily3 %>%
  gather_residuals(without_holidays = mod2_4, with_holidays = mod4) %>%
  ggplot(aes(date, resid, color = model)) +
  geom_line(alpha = 0.75)
daily3 %>%
  add_residuals(mod4, var = "resid") %>%
  ggplot(aes(date, resid)) +
  geom_line(alpha = 0.75)
# Model is better then the old model only using the terms for saturdays but still
# worse then the model for terms for the entire year.

# 5.
daily4 <- daily3 %>%
  group_by(date) %>%
  mutate(month = month(date, label = T))
md_5 <- lm(n ~ wday * month, data = daily4)
mod3_4 <- lm(n ~ wday * term, data = daily4)
daily4 %>%
  gather_residuals(without_month = mod3_4, with_month = md_5) %>%
  ggplot(aes(date, resid, color = model)) +
  geom_line(alpha = 0.75)
# The porblem is that there are only 12 months and each month only has 4 weeks so
# each month only has 4 of each day. this causes the model to have a high value
# for the standard error of the residuals.
summary(md_5)

# 6.
# We already know that certain days of the week like saturdays are effective by
# time of year. So without interaction between the date and the day of the week
md_6 <- lm(n ~ wday * ns(date, 5), data = daily)
md_7 <- lm(n ~ wday + ns(date, 5), data = daily)

daily %>%
  gather_residuals(with_interaction = md_6, without_interaction = md_7) %>%
  ggplot(aes(date, resid, color = model)) + 
  geom_line(alpha = 0.75)

daily %>%
  spread_residuals(with_interaction = md_6, without_interaction = md_7) %>%
  mutate(resid_diff = without_interaction - with_interaction) %>%
  ggplot(aes(date, resid_diff)) + 
  geom_line(alpha = 0.75)

# 7.
daily <- flights %>%
  mutate(date = make_datetime(year, month, day, hour))

Time <- function(date) {
  cut(hour(date),
      breaks = c(0,6,12,18,24),
      labels = c("Night", "Morning", "Afternoon", "Evening")
  )
}
daily_7 <- daily %>%
  mutate(time = Time(date), Day = date(date), wday = wday(Day, label = T)) %>%
  group_by(wday,time) %>%
  summarize(n = n(), mean_distance = mean(distance, na.rm = T))

daily_7 %>%
  ggplot(aes(x = wday, y = mean_distance, fill = time)) +
  geom_bar(position = "dodge", stat = "identity") +
  coord_cartesian(ylim=c(900,1100))
daily_7 %>%
  ggplot(aes(x = wday, y = n, fill = time)) +
  geom_bar(position = "dodge", stat = "identity")
# Saturday evenings have the longest flights, which might indicate vacation flights
# but Sunday has the next longest flights.

# the number of flights on sunday evenings is not particularly higher then any other.

# 8.
re_order <- function(x){
  y <- factor(x, levels = c(levels(x)[-1],levels(x)[1]))
}
d_8 <- daily_7 %>%
  mutate(wday = re_order(wday))

#### Many Models with purrr and broom
## gapminder

# Excercises
library(modelr)
library(tidyverse)

# 1.
# The only change here is changing the function so that year is a polynomial
country_model <- function(df) {
  lm(lifeExp ~ poly(year - mean(year),2), data = df)
}

by_country <- gapminder %>%
  group_by(country, continent) %>%
  nest()

by_country <- by_country %>%
  mutate(model = map(data, country_model))

by_country <- by_country %>%
  mutate(
    resids = map2(data, model, add_residuals)
  )

resids <- unnest(by_country, resids)

resids %>%
  ggplot(aes(year, resid)) +
  geom_line(aes(group = country), alpha = 1 / 3) +
  geom_smooth(se = FALSE)
# This does seem better

# 2.

glance2 <- by_country %>%
  mutate(glance = map(model, broom::glance)) %>%
  unnest(glance, .drop = T)
glance2 %>%
  dplyr::select(-data,-model,-resids) %>%
  arrange(r.squared)
glance2 %>%
  ggplot(aes(x = continent, y = r.squared, color = continent)) +
  geom_quasirandom()

# 3.

bad_fit <- filter(glance2, r.squared < 0.50)
gapminder %>%
  group_by(country, continent) %>%
  nest() %>%
  mutate(model = map(data, country_model)) %>%
  mutate(resids = map2(data, model, add_residuals)) %>%
  mutate(glance = map(model, broom::glance)) %>%
  unnest(glance) %>%
  filter(r.squared < 0.7) %>%
  unnest(data) %>%
  ggplot(aes(year, lifeExp, color = country)) +
  geom_line()
# I've got less countries with small r.squared now since the model used is improved
# however the process is the same. In fact, since .drop has been deprecated this
# question is somewhat redundant.

### List Columns

### Creating List Columns #test commit
