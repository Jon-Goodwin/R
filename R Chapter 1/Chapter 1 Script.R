### Chapter 1 Explore

### Load Packages
library(tidyverse)
library(gapminder)
library(Lahman)
library(nycflights13)

# Creating ggplot's


# First example using mpg dataframe

ggplot(data = mpg)+
  geom_point(mapping = aes(x=displ, y = hwy))

#ggplot(data = <DATA>) +
#  <GEOM_FUNCTION>(mapping = aes(<MAPPINGS>))

# Excercises Chapter 1

#First Steps

# 1. Run ggplot(data = mpg). What do you see?
ggplot(data =mpg)
# returns blank graph

# 2. How many rows are in mtcars? How many columns?
dim(mtcars)
# 32 rows, 11 columns

# 3. What does the drv variable describ? Read the help for ?mpg to find out.
# drv is the type of drivetrain, f = FWD, r = RWD, 4 = 4wd

# 4. Make a scatterplot of hwy versus cyl.
ggplot(data=mpg)+
  geom_point(mapping = aes(x = cyl, y = hwy))

# 5. What happens if you make a scatterplot of class versus drv?
#Why is the plot not useful? Variables are independent
ggplot(data=mpg)+
  geom_point(mapping = aes(x = displ, y= hwy, color = displ<5))

## Facets

ggplot(data = mpg) +
  geom_point(mapping = aes(x = displ, y = hwy)) +
  facet_grid(drv~cyl)

ggplot(data = mpg) +
  geom_point(mapping = aes(x = displ, y = hwy)) +
  facet_grid(.~drv)

ggplot(data = mpg) +
  geom_point(mapping = aes(x = displ, y = hwy)) +
  facet_grid(~cyl, nrow=1)

# 1. What happens if you facet on a continuous variable?
# Faceting a continuous 
#variable just creates many distinct panels with likely 1 point

# 2. The cells are empty because no cars have that combination of cylinders
  #and drivetrain

# 3.What plots does the following code make?
# First makes a facet grid with drivetrain in the rows
# Second makes a grid with cylinders in the columns

# 4.
ggplot(data = mpg) +
  geom_point(mapping = aes(x = displ, y = hwy)) +
  facet_wrap(~ class, nrow = 2)
# With a low number of classes it's much easier to see how the relationship 
# between displacement and highway mileage within the vehicle classes.
# with a large number of vehicle classes however the number of panels would
# become too large.

# 5. nrow determines the number of rows of panels, ncol determines number of
# columns. Grid doesn't have these options because it is determined by the
# number of points in the group, where wrap is using only a single group.

# 6. 
#Adding more rows squishes the panels making them more difficult to read.

## Geometric Objects

#NOTES

#ggplot(data = mpg, mapping = aes(x = displ, y = hwy)) +
  #geom_point() +
  #geom_smooth()

# Using global mapping to produce multiple geoms

# Excercises
# 1. geom_line. geom_boxplot. geom_histogram. geom_area

# 2. A scatterplot with a line of best fit, without confint, with displacement
# as independent variable and hwy as dependent, color by drive train

# 3. Remove the legend, giving a color aesthetic defaults to displaying a
# legend for the colors

# 4. se argument takes values of TRUE or FALSE as to whether to display the
# confidence interval