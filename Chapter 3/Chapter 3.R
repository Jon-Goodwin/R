### Data Transformation in dplyr

# filter() allows you to subset observations based on their values.
filter(flights, month == 1, day == 1)

#Excercises
# 1. filter(flights, near(arr_delay, 120))
# b. filter(flights, dest == "IAH" | dest == "HOU")
# c. filter(flights, carrier %in% c("UA","AA","DL"))
# d. filter(flights, month %in% c(7,8,9))
# e. filter(flights, arr_delay >= 120 & !dep_delay>0)
# f. filter(flights, dep_delay>= 60 & (sched_arr_time-sched_dep_time)-air_time>=30)
# g. filter(flights, 0 <= dep_time & dep_time <= 600)

# 2. filter(flights, between(dep_time,0,600))
# acts as an interval

# 3. count(filter(flights, is.na(dep_time)))
# 8255, likely represents cancelled flights based on no arrival or dep times.

# 4. NA ^ 0 = 1 since every number raised to the power 0 is 1, so it doesn't
# matter what value NA is. Similarly anything | TRUE always evaluates to TRUE,
# Anything & FALSE will always evaluate as FALSE. NA*0, Inf is a valid value in
# this case. Inf*Any=Inf except if Any=0, then Inf*0 returns NA, this is why NA*0
# is NA.

### Arrange()

#Excercises

# 1. arrange(flights, desc(is.na(dep_time)),dep_time)

# 2.arrange(flights, dep_delay), arrange(flights, desc(dep_delay))

# 3. arrange(flights, air_time)

# 4. arrange(flights, desc(distance)), arrange(flights, distance)

### Select columns with select()

# 1. select(flights, dep_time, dep_delay,arr_time, arr_delay)

# 2. select() ignores duplication

# 3. one_of() selects any variable that is present in the vector given.

# 4. By defaults contains is not case sensitive

### Add new variables with mutate()

# Excercises

# 1. mutate(f.1, new_dep_time = (((dep_time %/% 100)*60)+(dep_time %% 100)),+
# new_sched_dep_time = ((sched_dep_time %/% 100)*60)+(sched_dep_time %% 100))

# 2. mutate(f.2, diff = ((arr_time %/% 100 *60 +arr_time %% 100)%%1440)-(dep_+
#time %/% 100 *60 +dep_time %% 100)%%1440)
  #We would expect that arr_time-dep_time would be the same as air_time
  # after the conversion to minutes but it is not, From the documentation,
  # ?flights, we see that time of arrival anbd departure is in local time,
  # so this accounts for cases where a flight traveled west of NY and so the
  # arr_time and dep_time difference was shortened. Cases where the difference
  # longer are most likely attributed to possible delays on the runway like
  # waiting to unload passengers.

# 3. We would expect that dep_delay is the difference between the actual dep_time
# and the scheduled departure time.

# 4. mutate(flights, ranking = omit.na(min_rank(dep_delay)))

# 5. 1:3+1:10 returns an error because the 3 length sequence cannot evenly be
# added to the 10 length sequence.

# 6. cos,sin,tan,acos,asin,atan,atan2,cospi,sinpi,tanpi.
# a'x' gives arc'tan'x'

### Grouped summaries with summarize()

# Excercises

# 1. Arrival delay is likely more important most of the time, since most
# passengers likely schedule their days based on their arrival times. More
# so however would be whether factors affecting arrival delays or factors
# affecting departure delay contribute more towards "total delays", since clearly
# a plane that left late is more likely to not arrive on scheduled time then a plane
# that left on time, likewise a plane arriving late with likely depart later for
# its next flight. This depends on how airlines manage multiple flights for
# individual planes.

# for example p <- not_cancelled %>% group_by(tailnum,origin,dest) %>%
#summarize(count = n(), arr_delay_10 = min(arr_delay >= 10))

#p %>% filter(count>5, arr_delay_10>0)

# shows that there are exactly 6 flights which have flown more then 5 times
# and been delayed on arrival more then 10 minutes every time. While it may seem
# that variance in delays like 50% of the time being early or 50% of the time being
# late, it seems that consistent delays, point to a procedural problem and airline
# could actually solve, whereas variant delays, may be more inclined to be due to things
# outside of an airlines control, like areas with highly variant whether patterns.

# a plane that almost always is 10min delayed, either means the expected times 
# in the schedule need to be altered, or something very wrong is happening

# 2. not_cancelled %>% group_by(dest) %>% summarize(Flights = n())
# not_cancelled %>% group_by(tailnum) %>% summarize( n = sum(distance))

# 3. A flight that needs to make emergency landing in the middle of flight would
# not be considered cancelled in this case because of the "or" statement. The 
# flight would have departed but never arrived. Instead it probably should only
# include flights which never depart.

# 4. cancelled <- flights %>% 
# mutate(cancelled = (is.na(dep_delay)) | is.na(arr_delay)) %>%
# group_by(year, month, day) %>%
# summarize(Cancelled_total = sum(cancelled)+
# ,Total = n(), avg_delay = sum(arr_delay, na.rm = T)/n())

# We end up seeing a positive correlation between the avg_delay and the 
# proportion of cancelled flights. This intuitively makes sense since as the 
# average delay climbs some flights will approach a cut off where an airline
# must start cancelling flights which can no longer reasonably depart on time.

# 5. Depending on how you define the worst delays

#not_cancelled %>%
#group_by(carrier) %>%
#summarize(avg_delay = sum(arr_delay)/n()+
#, largest = max(arr_delay), smallest = min(arr_delay))

# this gives the avg_delay and the worst delays as well as the smallest or most
# ahead of schedule.

# by this is seems on average the worst carrier is F9, Frontier Airlines, Inc
# but the largest delay was from HA, Hawaiian Airlines Inc. 

# The problem with trying to distinguish carrier from airport is that not all 
# carriers fly out of every airport, and even more not all carriers go to every 
# destination

#flights %>%
#group_by(carrier) %>%
#summarize(unique_destinations = length(unique(dest)))

# So you can't really compare them, the best you can do is filter the ones that
# go to the same destinations, flying out of the same locations.
# but Alaska airlines for example, only flies to Seattle in this dataset, so it
# can't realistically be compared to anyone else except in that narrow flight path

# 6. 
#y <- not_cancelled %>%
  #group_by(tailnum) %>%
  #arrange(tailnum,year,month,day,dep_time,dep_delay) %>%
  #select(tailnum, year,month,day,dep_time,dep_delay) %>%
  #filter(cumall(dep_delay >= 60)) %>%
  #tally()

# 7. The sort argument sorts the groups by largest to smallest count. Say you 
# want to count the number of flights per day and then find the busiest flying days
# not_cancelled %>% group_by(year,month,day)%>% count(sort =T)

### Grouped Mutates

#Excercises
# 1. The arithmetic functions like +, - , /, are unaffected. But summary functions
# like mean, and median, operate within the group. Similary, ranking is ranked 
# relative to the group

# 2.
not_cancelled %>%
  group_by(tailnum) %>%
  mutate(worst_performer = sum(arr_delay)/n()) %>%
  select(tailnum, worst_performer)%>%
  arrange(desc(worst_performer))%>%
  filter(n()>10)%>%distinct(tailnum, .keep_all = T)

# returns the average arrival delay of planes that have flown more then 10 times.
# sorted by largest to smallest average delay times

# 3. 
not_cancelled %>%
  group_by(hour) %>%
  mutate(avg_delay = sum(arr_delay)/n()) %>%
  select(hour,avg_delay)%>% distinct(hour, .keep_all = T)%>%
  arrange(desc(avg_delay))

# shows that 7am is the hour with the lowest average delay.

# 4. 
not_cancelled %>%
  filter(arr_delay > 0) %>%
  group_by(dest) %>%
  summarize(delay_total = sum(arr_delay), proportion_delay = arr_delay/sum(arr_delay))

# 5.
p <- not_cancelled %>%
  arrange(origin,year,month,day,dep_time) %>%
  group_by(origin) %>%
  mutate(lagged_dep_delay = lag(dep_delay)) %>%
  select(origin,year,month,day,dep_time,dep_delay,lagged_dep_delay,dest) %>%
  filter(origin == "EWR")

ggplot(p)+geom_point(mapping = aes(x = lagged_dep_delay, y = dep_delay))

# 6.
flights %>%
  filter(!is.na(dep_delay) & !is.na(arr_delay)) %>%
  group_by(dest,origin) %>%
  mutate(avg_air_time = mean(air_time),number_flights = n())%>%
  select(tailnum,origin,dest,air_time,avg_air_time, number_flights)%>%
  mutate(rank = air_time/avg_air_time)%>%
  filter(rank<=0.8)%>%
  arrange(rank)

# probably multiple ways to identify what is suspicious, this will rank flights
# by how much air_time they had relative to the average flights from that origin
# to the same destination. Listed are flights whose air_time was less then 0.8 
# of the average flight time. A suspicious flight could also be one whose 
# air_time is significantly shorter then sched_dep_time and sched_arr_time, however
# it's likely that carriers anticipate some amount of delay, and adjust those
# schedules based on that, and so a flight that encounters no delays may seem 
# suspicious when it is in fact not.

p<-flights %>%
  filter(!is.na(dep_delay) & !is.na(arr_delay)) %>%
  group_by(dest,origin) %>%
  mutate(relative_delay = air_time-min(air_time), min_air_time = min(air_time)) %>%
  select(tailnum,origin,dest,air_time,min_air_time,relative_delay)%>%
  arrange(desc(relative_delay))%>%
  ungroup%>%
  slice(1:10)
# the top 10 most delayed flights in the air relative to the shorters air_time flight

# 7.
flights %>%
  filter(!is.na(dep_delay) & !is.na(arr_delay)) %>%
  group_by(dest)%>%
  filter(length(unique(carrier))>= 2)%>%
  group_by(carrier)%>%
  summarize(num_of_dest = length(unique(dest)))%>%
  arrange(desc(num_of_dest))
#Assuming it meant to rank them according to which carrier visits the most
# destinations.

### Workflow:Scripts

### Exploratory Data Analysis

#Excercises

# 1. After removing the outliers the data is 
p <- diamonds%>% filter(y<30,z<30)
ggplot(p)+
  geom_freqpoly(mapping = aes(x=x),color = "blue", binwidth = 0.1)+
  geom_freqpoly(mapping = aes(x=y),color = "red", binwidth = 0.1)+
  geom_freqpoly(mapping = aes(x=z),color = "green", binwidth = 0.1)
# x,y,z are all highly correlated with one another
# > cor(diamonds$x,diamonds$y)
#[1] 0.9747015
#> cor(diamonds$z,diamonds$y)
#[1] 0.9520057
#

# 2. The relation between the length of the diamond and the price, is seemingly
# what would be expected

ggplot(diamonds)+geom_point(mapping = aes(x = x, y = price))

# as length increases price seemingly increases exponentially, this could be 
# because larger cuts of diamonds require larger raw diamonds, which are rarer.

ggplot(diamonds)+geom_histogram(mapping = aes(x = price), binwidth=50)+coord_cartesian(xlim = c(0,10000))
# the distribution is similar to exponential distribution

# 3.
sum(diamonds$carat == 0.99)
sum(diamonds$carat == 1)
# its likely that most diamonds get rounded to common fractions of a whole diamond
# which explains why there are 1558 1 carat diamonds but only 23 0.99 carat diamonds

# 4. Most noticeable the difference between these options,
ggplot(diamonds)+geom_smooth(mapping = aes(x = x, y = price))+
  coord_cartesian(ylim = c(0,10000))

ggplot(diamonds)+geom_smooth(mapping = aes(x = x, y = price))+ylim(0,10000)

# in the second the ylim removes all data above 10000, whereas the first with
# coord_cartesian() is a true zoom.
# default binwidth is 30 when left undefined.

# Missing Values

#Excercises

# 1. In geom_histogram, the NA values are removed with the warning 
# Warning message:
#Removed 2 rows containing non-finite values (stat_count).
# in geom_bar, NA will be treated as a separate category

# 2. na.rm removes NA values from mean() and sum()

### Covariation

# Excercises

# 1. 
flights %>% mutate(cancelled = is.na(dep_time)) %>%
  ggplot(mapping = aes(sched_dep_time))+
  geom_freqpoly(mapping = aes(color = cancelled, y = ..density..), bindwidth = 1/4)+
  coord_cartesian(xlim = c(0,2400))
# we see using the density argument for y in geom_freqpoly and geom_density gives
# a kernel density estimator of the variable, a smoothed version of the histogram
flights %>% mutate(cancelled = is.na(dep_time)) %>%
  ggplot(mapping = aes(sched_dep_time, color = cancelled))+
  geom_density()+
  coord_cartesian(xlim = c(0,2400))

# 2. Lower cut quality diamonds have higher median carat then higher quality diamonds.
# higher carat is the most highly correlated variable with price
cor.test(diamonds$carat,diamonds$price)
diamonds %>%ggplot(aes(cut,price))+geom_boxplot()
diamonds %>%
  ggplot(aes(cut,carat))+
  geom_boxplot()

# 3. 
ggplot(diamonds)+geom_boxploth(mapping = aes(y= cut, x = price))
ggplot(diamonds)+geom_boxplot(mapping = aes(x= cut, y = price))+coord_flip()
# both plots are the same

# 4. geom_lv is better for visualizing data sets with large amounts of data
# since those data are more likely to have long tails of outliers
ggplot(diamonds)+geom_lv(mapping = aes(x= cut, y = price), k = 2)

# 5. 
ggplot(diamonds)+geom_freqpoly(mapping = aes(x = price, color = cut))
ggplot(diamonds)+
  geom_histogram(mapping = aes(x = price))+facet_grid(~cut, scale = "free_y")
ggplot(diamonds)+geom_violin(mapping = aes(x = cut, y = price))
# The geom_freqpoly method it becomes difficult to see any differences between
# the groups at the tails. The geom_histogram method makes it easier to compare but
# even with 5 groups it compresses the tables quite a bit. The geom_violin
# method seems the best in this scenario though it becomes somewhat more difficult
# to distinguish the distributions

# 6. 

### Two categorical variables

