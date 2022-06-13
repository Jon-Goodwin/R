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

# 5. The graphs will not look different they both use the same data and the 
# global aesthetic in the first graph is the same as both local aesthetics 
# in the 2nd.

# 6.

#First
ggplot(data = mpg, mapping = aes(x = displ, y = hwy))+
  geom_point(size=5)+
  geom_smooth(se=F, size = 2)

#Second
ggplot(data = mpg, mapping = aes(x = displ, y = hwy))+
  geom_point(size=5)+
  geom_smooth(mapping = aes(group = drv), se=F, size = 2)

#Third
ggplot(data = mpg, mapping = aes(x = displ, y = hwy))+
  geom_point(mapping = aes(color = drv),size=5)+
  geom_smooth(mapping = aes(group = drv, color = drv), se=F, size = 2)

#Fourth
ggplot(data = mpg, mapping = aes(x = displ, y = hwy))+
  geom_point(mapping = aes(color = drv),size=5)+
  geom_smooth(se=F, size = 2)

#Fifth
ggplot(data = mpg, mapping = aes(x = displ, y = hwy))+
  geom_point(mapping = aes(color = drv),size=5)+
  geom_smooth(mapping = aes(linetype = drv),se=F, size = 2)

#Sixth
ggplot(data = mpg, mapping = aes(x = displ, y = hwy))+
  geom_point(mapping = aes(color = drv),size=5)


### Statistical transformations

#Excercises

# 1. Default geom of stat_summary is geom_pointrange
ggplot(data = diamonds) +
  geom_pointrange(mapping = aes(x = cut, y = depth),
                stat = "summary",
                fun.ymin = min,
                fun.ymax = max,
                fun.y = median)

# 2.
ggplot(data=mpg)+
  geom_col(mapping = aes(x = displ, y = hwy))
#geom_col takes both an x and y argument whereas geom_bar builds a plot with
# y values proportioned by counts of x.

# 3.
# geom_histogram,stat_ecdf
# geom_path, stat_ellipse
# stat_function, geom_function
# stat_identity, geom_point
# stat_summary, geom_pointrange
# geom_tile, stat_summary_2d
# stat_unique, geom_point
# stat_sf_coordinates, geom_sf_label

# 4.stat_smooth computes the confidence interval and 

# 5. geom_bar by default will group the x values by the space of other similar x values

### Position Adjustments

#Excercises

# 1. Much of the data is overlapping, the geom_jitter() geom would reveal
# the data is much noiser then it appears in this plot.

# 2. width and height control the amount of vertical and horizontal jitter

# 3.geom_count() increases the area covered by points where there is overlap
# where as jitter displaces the overlapping points

# 4.position default is "dodge2"
ggplot(data = mpg, mapping = aes(x = class, y = hwy , fill = trans))+geom_boxplot(position = "identity")

### Coordinate Systems

# Excercises

# 1.
ggplot(data = diamonds)+geom_bar(mapping = aes(x = "", fill = cut))+coord_polar(theta = "y")

# 2. Adds labels, like titles, x label, y labe,...

# 3. coord_quickmap() preverse straight lines where as coord_map() does not and coord_map()
# is more computationally expensive.

# 4. The following plot visualizes the linear relationship between hwy and cty
# gas mileage. the geom_abline() geom adds a line with default intercept at 0
# and default slope of 1. Which would imply a 1-1 relation between cty and hwy
# coord_fixed() fixes the ratio of the plot so that distance in the x direction
# is the same as the distance in the y direction, to ensure the abline is not
# distorted by the aspect ratio of the image.

### The Layered Grammar of Graphics

#Current code template.

#ggplot(data = <DATA>) +
#<GEOM_FUNCTION>(
#  mapping = aes(<MAPPINGS>),
#  stat = <STAT>,
#  position = <POSITION>
#) +
#  <COORDINATE_FUNCTION> +
#  <FACET_FUNCTION>
