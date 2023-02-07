### Graphics for Communication
library(tidyverse)

## Label

ggplot(mpg, aes(displ, hwy)) +
  geom_point(aes(color = class)) +
  geom_smooth(se = F) +
  labs(
    title = paste(
      "Fuel efficiency generally decreases with engine size"
    ))

ggplot(mpg, aes(displ, hwy)) +
  geom_point(aes(color = class)) +
  geom_smooth(se = FALSE) +
  labs(
    title = paste(
      "Fuel efficiency generally decreases with
      engine size"
    ),
    subtitle = paste(
      "Two seaters (sports cars) are an exception
      because of their light weight"
    ),
    caption = "Data from fueleconomy.gov"
  )

ggplot(mpg, aes(displ, hwy)) +
  geom_point(aes(color = class)) +
  geom_smooth(se = FALSE) +
  labs(
    x = "Engine displacement (L)",
    y = "Highway fuel economy (mpg)",
    colour = "Car type"
  )

df <- tibble(
  x = runif(10),
  y = runif(10)
)
ggplot(df, aes(x, y)) +
  geom_point() +
  labs(
    x = quote(sum(x[i] ^ 2, i == 1, n)),
    y = quote(alpha + beta + frac(delta, theta))
  )


## Excercises

# 1.

mpg %>%
  select(cyl,cty, hwy) %>%
  pivot_longer(cols = c(cty,hwy), names_to = "Road", values_to = "MPG") %>%
  ggplot(aes(x = as.factor(cyl), y = MPG, fill = as.factor(Road))) +
  geom_bar(position = "dodge", stat = "identity") +
  labs(
    x = "Number of Cylinders",
    y = "Miles per Gallon",
    title = "Fuel efficiency is generally better on the highway"
  )

# 2.
mpg %>%
  ggplot(aes(displ, hwy, color = class)) +
  geom_point() + 
  geom_smooth(method = "lm", se = F)

mod <- lm(hwy ~ class, data = mpg)

mpg %>%
  add_residuals(mod) %>%
  ggplot(aes(x = displ, y = resid))+
  geom_point() +
  geom_smooth(method = "lm", se = F) +
  labs(
    x = "Engine Displacement(L)",
    y = "Highway MPG relative to the Class Average",
    title = "Engine size has a small negative effect on Fuel Efficiency"
  )


# 3.

### Annotations

best_in_class <- mpg %>%
  group_by(class) %>%
  filter(row_number(desc(hwy)) == 1)

ggplot(mpg, aes(displ, hwy)) +
  geom_point(aes(color = class)) +
  geom_text(aes(label = model), data = best_in_class)

ggplot(mpg, aes(displ, hwy)) +
  geom_point(aes(color = class)) +
  geom_label(
    aes(label = model),
    data = best_in_class,
    nudge_y = 2,
    alpha = 0.5
  )

library(ggrepel)

ggplot(mpg, aes(displ, hwy)) +
  geom_point(aes(color = class)) +
  geom_point(size = 3, shape = 1, data = best_in_class) +
  ggrepel::geom_label_repel(
    aes(label = model),
    data = best_in_class
  )

class_avg <- mpg %>%
  group_by(class) %>%
  summarize(
    displ = median(displ),
    hwy = median(hwy)
  )

ggplot(mpg, aes(displ, hwy, color = class)) +
  ggrepel::geom_label_repel(aes(label = class),
                            data = class_avg,
                            size = 6,
                            label.size = 0,
                            segment.color = NA
  ) +
  geom_point() +
  theme(legend.position = "none")

label <- mpg %>%
  summarize(
    displ = min(displ),
    hwy = max(hwy),
    label = paste(
      "Increasing engine size is \nrelated to decreasing fuel economy."
    )
  )

ggplot(mpg, aes(displ, hwy)) +
  geom_point() +
  geom_text(
    aes(label = label),
    data = label,
    vjust = "top",
    hjust = "right"
  )


### Excercises

# 1.
label <- mpg %>%
  summarize(
  displ = min(displ),
  hwy = max(hwy),
  label = paste(
    "Increasing engine size is \nrelated to decreasing fuel economy."
  )
)

ggplot(mpg, aes(displ, hwy)) +
  geom_point() +
  geom_text(
    aes(label = label),
    data = label,
    vjust = "top",
    hjust = 'left'
  )

# 2.
?annotate

# 3.

label <- mpg %>%
  summarize(
    displ = max(displ),
    hwy = max(hwy),
    label = paste(
      "text."
    )
  )

ggplot(mpg, aes(displ, hwy)) +
  geom_point() +
  geom_text(aes(label = label,vjust = "center", hjust = "right"),
  data = label) +
  facet_wrap(~cyl)

# 4.
# Fill aesthetic controls background box color.

ggplot(mpg, aes(displ, hwy)) +
  geom_point(aes(color = class)) +
  geom_label(
    aes(label = model),
    data = best_in_class,
    nudge_y = 2,
    alpha = 0.5,
    fill = "grey"
  )

# 5.
?arrow
# angle, length, ends, type
mpg %>%
  ggplot(aes(x = hwy, y = displ)) +
  geom_point() +
  geom_segment(aes(x = 20, y = 5.5, xend = 30, yend = 3), 
                           arrow = arrow(length = unit(0.5, 'cm')))

### Scales

ggplot(mpg, aes(displ, hwy)) +
  geom_point(aes(color = class))

ggplot(mpg, aes(displ, hwy)) +
  geom_point(aes(color = class)) +
  scale_x_continuous() +
  scale_y_continuous() +
  scale_color_discrete()

ggplot(mpg, aes(displ, hwy)) +
  geom_point() +
  scale_x_continuous(labels = NULL) +
  scale_y_continuous(labels = NULL)

presidential %>%
  mutate(id = 33 + row_number()) %>%
  ggplot(aes(start, id)) +
  geom_point() +
  geom_segment(aes(xend = end, yend = id)) +
  scale_x_date(
    NULL,
    breaks = presidential$start,
    date_labels = "'%y"
  )

ggplot(mpg, aes(displ, hwy)) +
  geom_point(aes(color = drv))

ggplot(mpg, aes(displ, hwy)) +
  geom_point(aes(color = drv)) +
  scale_color_brewer(palette = "Set1")

ggplot(mpg, aes(displ, hwy)) +
  geom_point(aes(color = drv, shape = drv)) +
  scale_color_brewer(palette = "Set1")

presidential %>%
  mutate(id = 33 + row_number()) %>%
  ggplot(aes(start, id, color = party)) +
  geom_point() +
  geom_segment(aes(xend = end, yend = id)) +
  scale_colour_manual(
    values = c(Republican = "red", Democratic = "blue")
  )


## Excercises

# 1.
df <- tibble(
  x = rnorm(10000),
  y = rnorm(10000)
)
ggplot(df, aes(x, y)) +
  geom_hex() +
  scale_color_gradient(low = "white", high = "red") +
  coord_fixed()

#the color in the plot is using fill not color.

ggplot(df, aes(x, y)) +
  geom_hex() +
  scale_fill_gradient(low = "white", high = "red") +
  coord_fixed()

# 2.
ggplot(mpg, aes(displ, hwy)) +
  geom_point() +
  scale_x_continuous(name = 'displacement',
                     breaks = seq(15, 40, by = 5))

# name is the first argument and functions the same as adding a label.

# 3.
presidential %>%
  mutate(id = 33 + row_number()) %>%
  ggplot(aes(start, id, color = party)) +
  geom_point() +
  geom_segment(aes(xend = end, yend = id)) +
  scale_colour_manual(
    values = c(Republican = "red", Democratic = "blue")
  )

presidential %>%
  mutate(id = 33 + row_number(),
         name_num = fct_inorder(str_c(name, ",",id))) %>%
  arrange(-id) %>%
  ggplot(aes(start, name_num, color = party)) +
  geom_point() +
  geom_segment(aes(xend = end, yend = name_num)) +
  scale_y_discrete(NULL) +
  scale_x_date(NULL,
               breaks = presidential$start,
               date_labels = "'%y",
               minor_breaks = make_date(seq(year(min(presidential$start)),
                                            year(max(presidential$end)),
                                            by = 4),1,1)) +
  scale_colour_manual(
    values = c(Republican = "red", Democratic = "blue")
  ) +
  labs(title = "Presidential Terms",
       subtitle = "Eisenhower(34th) to Trump(45th)") + 
  theme(
    panel.grid.minor = element_blank(),
    axis.ticks.y = element_blank()
  )

# 4.
ggplot(diamonds, aes(carat, price)) +
  geom_point(aes(color = cut), alpha = 1/20) + 
  guides(
    color = guide_legend(
      override.aes = list(size = 4, alpha = 1)
    )
  )

### Zooming

# Generally coord_cartesian() is best for zooming

ggplot(mpg, mapping = aes(displ, hwy)) +
  geom_point(aes(color = class)) +
  geom_smooth() +
  coord_cartesian(xlim = c(5, 7), ylim = c(10, 30))

mpg %>%
  filter(displ >= 5, displ <= 7, hwy >= 10, hwy <= 30) %>%
  ggplot(aes(displ, hwy)) +
  geom_point(aes(color = class)) +
  geom_smooth()

suv <- mpg %>% filter(class == "suv")
compact <- mpg %>% filter(class == "compact")
ggplot(suv, aes(displ, hwy, color = drv)) +
  geom_point()
ggplot(compact, aes(displ, hwy, color = drv)) +
  geom_point()

x_scale <- scale_x_continuous(limits = range(mpg$displ))
y_scale <- scale_y_continuous(limits = range(mpg$hwy))
col_scale <- scale_color_discrete(limits = unique(mpg$drv))

ggplot(suv, aes(displ, hwy, color = drv)) +
  geom_point() +
  x_scale +
  y_scale +
  col_scale
ggplot(compact, aes(displ, hwy, color = drv)) +
  geom_point() +
  x_scale +
  y_scale +
  col_scale

### Themes

ggplot(mpg, aes(displ, hwy)) +
  geom_point(aes(color = class)) +
  geom_smooth(se = FALSE) +
  theme_bw()

### Saving Plots

ggplot(mpg, aes(displ, hwy)) + geom_point()
ggsave("my-plot.pdf")

### Figure Sizing

# Five main options for R markdown figure dimensions
# fig.width, fig.height, fig.asp, out.width and out.height

# fig.show = "hold" renders plots after code blocks.

# dev = "png" can make pltos load faster but slightly lower
# in quality

