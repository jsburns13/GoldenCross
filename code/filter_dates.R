library(tidyverse)

df_forfilter <- read_csv("data/daily_stonks.csv")

### Clean data; pick only USD stocks on NYSE; filter to [X, Y]
# This code section was created to pare down a larger dataset; it is no longer
# needed (unless you query the full dataset again)

# Select dates and exchange
X = ISOdate(2000, 01, 01)
Y = ISOdate(2009, 12, 31)
exchange = 11 # 11 is the WRDS code for NYSE

df_forfilter <- df_forfilter %>%
  filter(curcdd == "USD") %>%
  filter(exchg == exchange) %>%
  mutate(datadate = as.character(datadate)) %>%
  mutate(date = as.Date(datadate, "%Y%m%d")) %>%
  filter(date >= X) %>%
  filter(date <= Y)

### Let's also deal with market cap restrictions

cut_mkt = "Micro" # This designation will be filtered out below

df_forfilter <- df_forfilter %>%
  group_by(tic) %>%
  mutate(min_mkt = case_when(
    min(cschoc * prccd) < 250000000 ~ "Micro",
    min(cschoc * prccd) < 2000000000 ~ "Small",
    min(cschoc * prccd) < 10000000000 ~ "Mid",
    TRUE ~ "Large"
  )) %>%
  filter(min_mkt != cut_mkt)

### Save as .csv for easier file size sharing - commented out for reasons

write.csv(df_forfilter,paste0("data/daily_stonks_NYSE_",format(X,format="%Y"),"_",format(Y,format="%Y"),".csv"))

rm(df_forfilter)