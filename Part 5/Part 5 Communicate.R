### Communicate

### R Markdown

## Excercises

# 3.
# Changing the YAML header changes a notebook to a markdown document and
# vice-verse. The main difference between them is the existence of the 
# preview option in notebook

# 4.
# Outputs are different file types, the input change is what the YAML header
# contains being output: pdf_document to html_document, word_document.

# cached = TRUE will cache the output of a code chunk to avoid rerunning

# dependson = "name" will not an onbject the code chunk depends on and will
# update cache if that object changes.

# knitr::clean_cache() to clear cache

### Excercises 

# 1.
diamonds %>%
  mutate(volume = x*y*z) %>%
  filter(volume < 1000) %>%
  ggplot(aes(x = as.factor(cut), y = volume))+geom_boxplot()

# 3.

diamonds %>%
  mutate(volume = x*y*z) %>%
  arrange(-volume) %>%
  slice(1:20)

comma <- function(x) format(x, digits = 2, big.mark = ",")