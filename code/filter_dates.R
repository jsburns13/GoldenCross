library(tidyverse)

#df_forfilter <- read_csv("data/daily_stonks.csv")
df_forfilter <- read_csv("data/daily_stonks_NYSE_2010_2019.csv")

### Clean data; pick only USD stocks on NYSE; filter to [X, Y]
# This code section was created to pare down a larger dataset; it is no longer
# needed (unless you query the full dataset again)

# Select dates and exchange
X = ISOdate(2010, 01, 01)
Y = ISOdate(2019, 12, 31)
exchange = 11 # 11 is the WRDS code for NYSE

df_forfilter <- df_forfilter %>%
  filter(curcdd == "USD") %>%
  filter(exchg == exchange) %>%
  mutate(datadate = as.character(datadate)) %>%
  mutate(date = as.Date(datadate, "%Y%m%d")) %>%
  filter(date >= X) %>%
  filter(date <= Y)

### Let's also deal with market cap restrictions

keep_mkt = "Large" # This designation will be filtered to below

df_forfilter <- df_forfilter %>%
  group_by(tic) %>%
  mutate(mkt = case_when(
    min(cshoc * prccd) < 250000000 ~ "Micro",
    min(cshoc * prccd) < 2000000000 ~ "Small",
    min(cshoc * prccd) < 10000000000 ~ "Mid",
    TRUE ~ "Large"
  )) %>%
  filter(mkt == keep_mkt)

### Save as .csv for easier file size sharing

write.csv(df_forfilter,paste0("data/daily_stonks_NYSE_",keep_mkt,"_",format(X,format="%Y"),"_",format(Y,format="%Y"),".csv"))

rm(df_forfilter)