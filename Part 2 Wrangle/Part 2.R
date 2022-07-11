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
  locale = locale(grouping_mark = ",", decimal_mark = ","))
# Gives an error message Error: `decimal_mark` and `grouping_mark` must be different
parse_number("123,456,789",locale = locale(decimal_mark = ","))

# 3.
parse_date("01 01 2022")
parse_date("01 01 2022", locale = locale(date_format = "%D %M %Y"))
# They allow you to customize the formatting of the date or time being parsed

# 4.

# 5.
# the delim argument of read_csv() is set to "," and the delim argument of 
# read_csv2() is ";"

# 6.
# ISO-8859-1 and ISO-8859-2 is the most common encoding for European languages
# GB18030 and Big5 are the most common chinese encodings.

# 7.
d1 <- "January 1, 2010"
d2 <- "2015-Mar-07"
d3 <- "06-Jun-2017"
d4 <- c("August 19 (2015)", "July 1 (2015)")
d5 <- "12/30/14" # Dec 30, 2014
t1 <- "1705"
t2 <- "11:15:10.12 PM"

parse_date(d1, locale = locale(date_format = "%B %d, %Y"))
parse_date(d2, locale = locale(date_format = "%Y-%b-%d"))
parse_date(d3, locale = locale(date_format="%d-%b-%Y"))
parse_date(d4, locale = locale(date_format = "%B %d (%Y)"))
parse_date(d5, locale = locale(date_format = "%m/%d/%y"))
parse_time(t1, locale = locale(time_format = "%H%M"))
parse_time(t2, locale = locale(time_format = "%I:%M:%OS %p"))

### Tidy Data with tidyr

# 1. 
# Table 1, the tidy table each row entry is a country with a year, 1999 or 2000
# and the number of cases and the population of the country for that year.

# table 2, the row entries are the country, the year and the type either 
# the population or the cases, with the last entry being the number

# table 3, the row entries are the country and year with a rate of cases/population

# table 4 is 2 tables, first the cases table with country, and the number of cases
# for the years 1999 and 2000, and the second table is the same but for population.

# 2.

table2a <- table2 %>% filter(type == "cases")
table2b <- table2 %>% filter(type == "population")
table2c <- merge(x = table2a, y = table2b, by = c("country", "year"))
table2c <- table2c %>%
  rename(cases = count.x, population = count.y) %>%
  select(country, year, cases, population) %>%
  mutate(rate = cases/population * 10000)
table2c <- table2c %>%
  mutate(type = "rate") %>%
  rename(count = rate) %>%
  select(country,year,type,count)
table2 <- table2 %>%
  bind_rows(table2, table2c) %>%
  arrange(country,year,type,count)

table4c <- merge(table4a, table4b, by = "country")
table4c <- table4c %>%
  rename(cases.1999 = pop.1999, cases.2000 = '2000.x', pop.1999 = '1999.y', pop.2000 = '2000.y')
table4c <- table4c %>%
  mutate(rate.1999 = cases.1999/pop.1999 * 10000, rate.2000 = cases.2000/pop.2000 *10000)
table4d <- bind_cols(table4c$country, table4c$rate.1999, table4c$rate.2000)
table4d <- table4d %>%
  rename(country = ...1 , rate.1999 = ...2, rate.2000 = ...3)

# 3.
table2 %>%
  filter(type == "cases") %>%
  ggplot(aes(factor(year),count))+
  geom_line(aes(group = country), color = "gray50")+
  geom_point(aes(color = country))
# first you have to filter out population

### Spreading and Gathering

# 1.
stocks <- tibble(
  year = c(2015, 2015, 2016, 2016),
  half = c( 1, 2, 1, 2),
  return = c(1.88, 0.59, 0.92, 0.17)
)
stocks %>%
  spread(year, return) %>%
  gather("year", "return", `2015`:`2016`)
# originally the type of all 3 columns is a dbl, but after the transformation year
# has become a character variable. From the documentation of gather(), when
# factor_key = FALSE, the default value, then key values are stored as a character
# vector, so "year" is stored as character vector.

# type.convert() converts the type of a data object

# 2.
# missing the `` around 1999 and 2000

table4a %>%
  gather(`1999`, `2000`, key = "year", value = "cases")

# 3.
people %>%
  spread(key, value)
# This results in an error since Phillip Woods age has 2 values, and keys must be unique
people %>%
  spread(value, key)
# can be reversed but results in NA values, but logically a person can't have 2 ages.

# 4.
preg <- tribble(
  ~pregnant, ~male, ~female,
  "yes", NA, 10,
  "no", 20, 12
)
preg <- preg %>%
  gather(Gender, Count, male, female,) %>%
  rename(Pregnant = pregnant)

### Separating and Pulling

#Excercises

# 1.
tibble(x = c("a,b,c", "d,e,f,g", "h,i,j")) %>%
  separate(x, c("one", "two", "three"), extra = "merge")

tibble(x = c("a,b,c", "d,e", "f,g,i")) %>%
  separate(x, c("one", "two", "three"), fill = "warn")

# 2. 

# With separate when remove() is FALSE the separate function will not remove the
# column being separated.
tibble(x = c("a,b,c", "d,e,f,g", "h,i,j")) %>%
  separate(x, c("one", "two", "three"), remove = F)

# With remove() FALSE in unite() the united columns are not removed.
table5 %>%
  unite(new, century, year, sep = "", remove = F)

# 3.
# Unite has only one variation because there's only 1 way to combine things but many
# ways a column could be separated.


### Missing Values

