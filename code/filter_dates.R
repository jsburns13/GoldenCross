library(tidyverse)

df_forfilter <- read_csv("data/daily_stonks.csv")

### Clean data; pick only USD stocks on NYSE; filter to [X, Y]
# This code section was created to pare down a larger dataset; it is no longer
# needed (unless you query the full dataset again)

df_forfilter <- df_forfilter %>%
  filter(curcdd == "USD") %>%
  filter(exchg == 11) %>%
  mutate(datadate = as.character(datadate)) %>%
  mutate(date = as.Date(datadate, "%Y%m%d")) %>%
  filter(date >= ISOdate(2000,01,01)) %>%
  filter(date < ISOdate(2010,01,01))

### Save as .csv for easier file size sharing - commented out for reasons

write.csv(df_forfilter,"data/daily_stonks_NYSE_2000_2009.csv")