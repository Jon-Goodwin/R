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
y <- enframe(1:3)
y
x <- enframe(c(a = 5, b = 7))
x
z <- enframe(list(one = 1, two = 2:3, three = 4:6))
z

# 6.
print(flights, max_footer_lines = 1)

### Data Import with readr

# Excercises

# 1. read_delim(file, delim = |)

# 2. they take na = , quoted_na =, quote = "\""

# 3.Most important argument is col_positions, which tells read_fwf where the
# columns are

# 4. 
read_csv("x,y\n1,'a,b'", quote = "'")

# 5.
read_csv("a,b\n1,2,3\n4,5,6")

# initially only 2 headers are given, so 2 columns are created, but then 3
# entries are given for the first row and 3 for the second, the read_csv() just
# appends the last entry to the last column

read_csv("a,b,c\n1,2\n1,2,3,4")

# the first row has a non entry for column c, and the last row has 4 entries but
# only 3 column headers were given initially.

read_csv("a,b\n\"1")
# \"1 leaves an open quotation, and its unclear whether \ is intended as a delimiter
# or not.

read_csv("a,b\n1,2\na,b")
# a,b are character vectors, it's not clear that there really is a problem

read_csv("a;b\n1;3")
# It seems the intent here would have been to use read_delim()
read_delim("a;b\n1;3", delim = ";")

# Parsing a Vector

# Excercises

# 1. Most important arguments are encoding, and date_format and time_format.

# 2. 
parse_number("123,456,789",
  locale = locale(grouping_mark = ",", decimal_mark = ",")
# Gives an error message Error: `decimal_mark` and `grouping_mark` must be different
parse_number("123,456,789",locale = locale(decimal_mark = ","))
             