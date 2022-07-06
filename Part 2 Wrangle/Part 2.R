library(tidyverse)

### Tibbles with Tibble

# Excercises

# 1.

# We can tell an object is a tibble since when printed a tibble, says A tibble
# also mtcars is a data.frame object and uses headers for rows, where a tibble 
# does not.

# 2.

df <- data.frame(abc = 1, xyz = "a")
df$x
df[, "xyz"]
df[, c("abc", "xyz")]

# df$x when used on a dataframe object gives the second column entry, when it 
# would probably be more expected to give a warning message as the tibble does.

# The data frame also only gives the entries to the associated column while the
# tibble treats the column object as 1x1 tibble with the header.

# 3. 
mtcars[[var]]

# 4.

# a. 
annoying$`1`

# b. 
ggplot(annoying,aes(`1`,`2`))+geom_point()

# c.
annoying <- annoying %>% mutate(`3` = `2`/`1`)

# d.
rename(annoying, one = `1`, two = `2`, three = `3`)

# 5.
