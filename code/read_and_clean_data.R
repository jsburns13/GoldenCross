library(tidyverse)
library(fixest)
library(vtable)

df <- read_csv("data/daily_stonks.csv")

### clean data; pick only USD stocks

df <- df %>%
  filter(curcdd == "USD")

df %>%
  group_by(exchg, curcdd) %>%
  summarize(mean(prccd))