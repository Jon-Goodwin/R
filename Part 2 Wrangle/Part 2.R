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
  gather(Gender, Count, male, female) %>%
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

# Excercises

# 1. 
# Fill only has a data and direction argument.
# Complete has only the columns to expand as and argument, the fill argument
# which gives a list of values to use instead of NA and explicit which is T/F 
# depending on whether to use fill for existing NA values or not.

# 2.
# direction changes the direction of the fill, example
treatment %>%
  fill(person, .direction = "up")

### Case Study

who3 <- who %>%
  gather(code, value, new_sp_m014:newrel_f65, na.rm = TRUE) %>%
  mutate(
    code = stringr::str_replace(code, "newrel", "new_rel")
  ) %>%
  separate(code, c("new", "var", "sexage")) %>%
  select(-new, -iso2, -iso3) %>%
  separate(sexage, c("sex", "age"), sep = 1)

# 1. If there are no 0 entries it would be reasonable to assume NA is a substitute
# for 0 cases, however:

length(which(who == 0))

# returns 11080 so there are 11080, entries of 0. So it would be reasonable to assume
# NA means no data and is an explicit missing value.

dim(who)[1]

who %>% complete(year,country)%>% nrow()

# we see that there are 7446 possible pairings of countries and years in the table
# but only 7240 entries are present.

who.complete <- who %>% complete(year,country)

who.missing <- who.complete %>% 
  anti_join(who) %>%
  select(country,year)

# this tibble contains all country, year pairings that did not appear in the original
# who tibble.

# 2.
test <- who %>%
  gather(code, value, new_sp_m014:newrel_f65, na.rm = TRUE) %>%
  separate(code, c("new", "var", "sexage"))

# there are no longer 3 separators in code so separate fills the inconsistently 
# entered newrel instead the first 2 columns are filled by the separator and the
# last column "sexage" is filled as NA

which(is.na(test$sexage) == T)

# 3.
who %>%
  select(country,iso2,iso3) %>%
  group_by(country) %>%
  distinct()%>%
  filter(n() > 1)

# shows there are no countries with more then 1 combination of iso2, iso3. Thus
# the iso2 and iso3 columns are redundant.

# 4.
who3 %>%
  group_by(country,year,sex) %>%
  summarize(count = sum(value)) %>%
  unite(country_sex, country, sex, remove = F) %>%
  filter(year > 1995) %>%
  ggplot(aes(year,count, group = country_sex, color = sex)) + 
  geom_line()

who3 %>%
  group_by(country,year,sex) %>%
  summarize(count = sum(value)) %>%
  unite(country_sex, country, sex, remove = F) %>%
  filter(year > 1995) %>%
  arrange(-count) %>%
  filter(count > 100000) %>%
  ggplot(aes(year,count, group = country_sex, color = sex)) + 
  geom_line()

### Relational Data with dplyr

# Excercises

# 1. The airports table contains the lattitude and longitude of all airports.
# the flights table contains the tailnum of the plane and the origin and dest of
# each of its scheduled flights. So we would combine airports with flights
# and then take the origin, dest, longitude, lattitude, tailnum variables.

# 2. faa from airports is the origin in weather

# 3. Weather would contain the weather information for the destination

# 4. A "Special Dates" table would contain year,month,day, and maybe a name for
# day.

# Keys

# Excercises

# 1. 
flights %>%
  arrange(year,month,day,flight) %>%
  mutate(id = row_number())

# 2. 
Batting %>%
  group_by(playerID,yearID, stint) %>%
  mutate(n = n()) %>%
  filter(n>1)
# The primary key is playerID, yearID, stint

babynames %>%
  count(year,sex,name) %>%
  filter(n>1)
# The primary key is year,sex,name

atmos %>%
  count(lat,long, year, month) %>%
  filter(n>1)

vehicles %>%
  count(id) %>%
  filter(n>1)

diamonds %>%
  count(x,y,z, depth, table, color ,clarity, cut, carat, price, color) %>%
  filter(n>1)

#All variables are a primary key.

# 3.

### Mutating Joins

# 1. 
airports %>%
  semi_join(flights, c("faa" = "dest")) %>%
  ggplot(aes(lon, lat)) +
  borders("state") +
  geom_point() +
  coord_quickmap()

flights3 <-
  flights %>%
  filter(!is.na(dep_delay) & !is.na(arr_delay)) %>%
  group_by(dest) %>%
  summarize(avg_delay = mean(arr_delay, na.rm = T))

flights3 %>% left_join(airports, c("dest" = "faa")) %>%
  ggplot(aes(lon,lat)) +
  borders("state") +
  geom_point(aes(size = avg_delay)) +
  coord_quickmap()

# 2.

flights2 %>% 
  left_join(airports, c("dest" = "faa")) %>%
  rename(lat.dest = lat, lon.dest = lon) %>%
  left_join(airports, c("origin" = "faa")) %>%
  rename(lat.origin = lat, lon.origin = lon) %>%
  select(year,month,day,hour, origin, lat.origin, lon.origin, dest, lat.dest,lon.dest,
         tailnum, carrier)

# 3.
planes.age <- planes %>%
  select(year, tailnum) %>%
  rename(man.date = year)

flights.age <- flights %>%
  left_join(planes.age, "tailnum") %>%
  mutate(age = year-man.date) %>%
  select(dep_delay, age,) %>%
  filter(!is.na(dep_delay) & !is.na(age)) %>%
  group_by(age) %>%
  mutate(avg_delay = mean(dep_delay)) %>%
  ggplot(aes(x = age, y = avg_delay))+
  geom_point()

# there doesn't seem to be any strong relationship between the manufacture date
# of a plane and the delays. Early on in a planes life they seem to increase but
# then fall off.

# 4. 

flights.weather <- flights %>%
  left_join(weather, by = c("origin","year","month","day","hour")) %>%
  filter(!is.na(dep_delay) & !is.na(wind_speed)) %>%
  group_by(wind_speed) %>%
  summarize(avg_delay = mean(dep_delay)) %>%
  ggplot(aes(x = wind_speed, y = avg_delay))+
  geom_point()

#we notice a rather strong correlation between wind speed and dep delays.

# 5.

flights.june <-
  flights %>%
  filter(year == 2013, month == 6, day == 13, !is.na(arr_delay)) %>%
  group_by(dest) %>%
  summarize(avg_delay = mean(arr_delay)) %>%
  left_join(airports, c("dest" = "faa")) %>%
  ggplot(aes(lon,lat)) +
  borders("state") +
  geom_point(aes(size = avg_delay, color = avg_delay)) +
  coord_quickmap()

# 900 flights were canceled due to strong storms on the east coast.

### Filtering Joins

# 1. 
f1 <- flights %>%
  anti_join(planes, by = "tailnum") %>%
  group_by(carrier) %>%
  summarize(count = n()) %>%
  arrange(-count)

# From the documentation for plaens "American Airways (AA) and Envoy Air (MQ) 
# report fleet numbers rather than tail numbers so can't be matched."

flights %>%
  filter(carrier == 'AA' | carrier == 'MQ') %>%
  group_by(carrier) %>%
  summarize(count = n()) %>%
  arrange(-count)

# We see these airlines do have some planes with tailnumbers however, and its
# not clear why the other carriers have some planes with unregistered tailnumbers.

# 2. 
flights %>%
  filter(!is.na(tailnum)) %>%
  group_by(tailnum) %>%
  mutate(count = n()) %>%
  filter(count >= 100)

# 3. 

vehicles %>%
  semi_join(common)

# 4.

# The easiest interpretation here is finding the "worst" 48 hours of the year.
# worst being interpreted as the hour with the largest total dep_delay.

flight.48 <- 
  flights %>%
  filter(!is.na(dep_delay) & !is.na(arr_delay)) %>%
  group_by(year,month,day,hour) %>%
  summarize(delay = sum(dep_delay, na.rm = T)) %>%
  ungroup() %>%
  arrange(-delay) %>%
  head(48)

weather %>%
  summarize(avg_visib = mean(visib, na.rm = T), avg_wind_speed = mean(wind_speed,
                                                                      na.rm = T),
            avg_precip = mean(precip, na.rm = T))

# My expectation is that visibility on the most delayed hours will be lower then
# average and wind speed will be higher.

weather %>%
  semi_join(flight.48) %>%
  summarize(avg_visib = mean(visib, na.rm = T), avg_wind_speed = mean(wind_speed,
                                                                      na.rm = T),
            avg_precip = mean(precip, na.rm = T))

# We get what was expected, the weather patterns associated with higher delays were
# more prevalent during those worst delayed hours.

# 5.
anti_join(flights, airports, by = c("dest" =
                                      "faa")) %>%
  select(dest) %>%
  unique()

# this tells us the airports in flights not listed in airports. There are 4
# BQN, SJU, STT, PSE, which all appear to be in puerto rico or the virgin islands.

anti_join(airports, flights,
          by = c("faa" = "dest")) %>%
  select(faa) %>%
  unique()

# These are all the domestic airports which were not flown to from NYC in 2013.

# 6.
flights %>%
  filter(!is.na(tailnum)) %>%
  select(tailnum, carrier) %>%
  unique() %>%
  group_by(tailnum) %>%
  count() %>%
  filter(n>1)

# So the assumption that each plane has only flown with 1 carrier is not true,
# though it seems reasonable that carrier may buy and sell planes among eachother

### Strings with stringr

# Excercises:

# 1. paste() you can specify the sep argument, where paste0 just uses sep = ""
# str_c, is the equivalent stringr function, but paste() prints NA as "NA" by 
# default

# 2.
str_length(str_c(c("x", "y", "z"), collapse = ", "))

str_length(str_c(c("x", "y", "z"), sep = ", "))

# sep is a separator str_c combines vectors, collapse, collapses a vector to a
# single string.

# 3.

x <- "Number"

y <- str_length(x)/2

str_sub(x, if (str_length(x) %% 2 != 0) {ceiling(y)}
        else {str_length(x)/2},
        if (str_length(x) %% 2 != 0) {ceiling(y)}
        else {str_length(x)/2})

# 4.
str_wrap() # a line wrapping algorithm to fit a string into a specific line
# format, useful to reformat text to fit on a page

# 5. str_trim() removes the whitespace at the start and end of a string

x <- " example "
x.trim <- str_trim(x)

# str_pad() adds whitespace

str_pad("example", 9, side = c("both"))

# 6.
z <- c("eggs","lettuce", "apples", "oranges")
if (length(z) == 1) {z} else {str_c(c(str_c(z[-length(z)],
                                            collapse = ", "), z[length(z)]),
                                    collapse = ", and ")}

# Im not sure if for example:

z.2 <- c("eggs", "lettuce")
if (length(z.2) == 1) {z.2} else if (length(z.2) == 2) {str_c(z.2,
                                    collapse = " and ")} else {
                                    str_c(c(str_c(z.2[-length(z.2)],
                                    collapse = ", "), z.2[length(z.2)]),
                                    collapse = ", and ")}
# would be better

### Matching Pattersn with regular Expressions

# Excercises

# 1.
# Every \ escapes the next character so "\" expects a another character after \
# in this case it escapes " but then the string is left open

# "\\" in the regular expression this gives \ which is used as the escape character
# so effectively the string "\\" is the escape regular expression

# "\\\" \\ is a metacharacter in the regular expression, which needs to be escaped
# via a \ but then that resolves to escaping a character. thus the need for an 
# additional \, to resolve to a literal \.

# 2.
x <- "\"\'\\"
str_view(x, "\\\"\\\'\\\\")

# 3.

# The expression will match the sequence, literal ." character" literal ." character"
# literal ." character"

### Anchors

# 1. 

str_view("$^$", "\\$\\^\\$")

# 2.

str_view(stringr::words, "^y", match = T)

str_view(stringr::words, "x$", match = T)

str_view(stringr::words, "^...$", match = T)

str_view(stringr::words, ".......", match = T)

### Characater classes and alternatives

# 1.

str_view(stringr::words, "^[aeiou]", match = T)

str_view(stringr::words, "[aeiou]", match = F)

str_view(stringr::words, "[^e]ed$", match = T)

str_view(stringr::words, "ing$|ize$", match = T)

# 2.

str_view(stringr::words, "[c]ei|[^c]ie", match = T)

str_view(stringr::words, "cie|[^c]ei", match = T)

# three words violate the rule.

# 3.

str_view(stringr::words, "q[^u]", match = T)

# yes, there are no words that have a character other then u after q

# 4.

str_view(stringr::words, "ou|ise$", match = T)

# will capture any british spelling but also common words like group
# however flavour instead of flavor is british spelling

# 5.
x <- c("226-567-5678", "2222222222", "519-564-4665")

str_view(x, "\\d\\d\\d-\\d\\d\\d-\\d\\d\\d\\d", match = T)

### Repetitions

# 1.

x <- "1888 is the longest year in Roman numerals: MDCCCLXXXVIII"

str_view(x, "CC{0,1}")

str_view(x, "CC{1,}")

str_view(x, "CC{0,}")
str_view(x, "CC*")

# 2.

# ^.*$ any character repeated 0 or more times, regular expression for entire line

# "\\{.+\\}" any string that contains literal { and } with any characters in between
# string that defines a reg exp

# \d{4}-\d{2}-\d{2} any digit 4 times - any digit 2 times - any digit 2 times

# "\\\\{4}" literal \ repeated 4 times, string that defines a regular exp

str_view("\\\\\\\\\\", "\\\\{4}")

# 3.

str_view(stringr::words, "^[^aeiou]{3}", match = T)

str_view(stringr::words, "[aeiou]{3,}", match = T)

str_view(stringr::words, "([aeiou][^aeiou]){2,}", match = T)

# 4. yes.

### Grouping and Backreferences

# Excercises

# 1.
# (.)\1\1 regexp matching any character repeated 3 times
str_view(c(fruit, "aaaa"), "(.)\\1\\1", match = T)

# "(.)(.)\\2\\1" a string with any 2 characters, and then the second character
# repeats followed by the first.
str_view(c("abba", "cddc", "dcdc", "ccdd"), "(.)(.)\\2\\1", match = T)

# (..)\1 group of any 2 characters repeats once

str_view(stringr::words, "(..)\\1", match = T)

# "(.).\\1.\\1" any character then any character  then first character then any
# then first again

str_view("aoaka", "(.).\\1.\\1")

# "(.)(.)(.).*\\3\\2\\1" any character any character any character any character
# 0 or more times, then repeat third, repeat second, repeat first

str_view("abcooocba", "(.)(.)(.).*\\3\\2\\1")

# 2.

str_view(stringr::words, "^(.).*\\1$", match = T)

str_view(stringr::words, "(.)(.).*\\1\\2", match = T)

str_view("bcdcedc", "(.).*\\1.*\\1.*", match = T)

### Tools

# Excercise

# 1.

words[str_detect(words, "^x|x$")]

start <- str_detect(words, "^x")
end <- str_detect(words, "x$")
words[start|end] == words[str_detect(words, "^x|x$")]
#
words[str_detect(words, "^[aeiou].*[^aeiou]$")]

start_vowel <- str_detect(words, "^[aeiou]")
end_consonant <- str_detect(words, "[^aeiou]$")
words[start_vowel & end_consonant]
#
a <- str_detect(words, "a+")

e <- str_detect(words, "e+")

i <- str_detect(words, "i+")

o <- str_detect(words, "o+")

u <- str_detect(words, "u+")

words[a&e&i&o&u]
#
str_count(words, "[aeiou]")
df %>%
  mutate(vowels = str_count(words, "[aeiou]"), length = str_count(words, "."),
         prop = vowels/length) %>%
  arrange(-prop)
# a has the highest proportion of vowels.

### Extract Matches

# Excercises

# 1.
color_match <- "\\bred\\b|\\borange\\b|\\byellow\\b|\\bgreen\\b|\\bblue\\b|\\bpurple\\b"
has_color <- str_subset(sentences, color_match)
matches <- str_extract(has_color, color_match)
more <- sentences[str_count(sentences, color_match) > 1]
str_view_all(more, color_match)
#
str_extract(sentences, "[A-Z][a-z]*")
str_extract(str_subset(sentences, "[A-Za-z][a-z]*ing"), "[A-Za-z][a-z]*ing")
#
str_subset(sentences, "[A-Za-z][a-z]{2,}s\\b")
str_extract(str_subset(sentences, "[A-Za-z][a-z]{2,}s\\b"), "[A-Za-z][a-z]{2,}s\\b")
# identifies some but many false positives.

