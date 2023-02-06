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
ggplot(mpg, aes(displ, hwy)) +
  geom_point() +
  annotate("text", x = 6, y = 42,
           label = "Increasing engine size is \nrelated to decreasing fuel economy.")

# 3.
