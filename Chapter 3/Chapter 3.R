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