library(tidyverse)
library(fixest)
library(vtable)
library(zoo)

df <- read_csv("data/daily_stonks.csv")

### Clean data; pick only USD stocks on NYSE; filter to [2010, 2019]

df <- df %>%
  filter(curcdd == "USD") %>%
  filter(exchg == 11) %>%
  mutate(datadate = as.character(datadate)) %>%
  mutate(date = as.Date(datadate, "%Y%m%d")) %>%
  filter(date >= ISOdate(2010,01,01)) %>%
  filter(date < ISOdate(2020,01,01))

### Create rolling averages

df <- df %>%
  arrange(date) %>%
  group_by(tic) %>%
  mutate(ma_50 = rollmeanr(prccd, k=50, fill=NA)) %>%
  mutate(ma_200 = rollmeanr(prccd, k=200, fill=NA))

### Create indicators

df <- df %>%
  mutate(gc_50_over_200 = case_when(
    ma_50 <= ma_200 ~ -1,
    ma_50 > ma_200 ~ 1
  ))

### Create Golden Cross indicator and find dates

df <- df %>%
  arrange(date) %>%
  group_by(tic) %>%
  mutate(gci = rollmeanr(gc_50_over_200, k=2, fill=NA)) %>%
  mutate(gc = case_when(
    gci == 0 & gc_50_over_200 == 1 ~ 1,
    TRUE ~ 0
  )) %>%
  mutate(dc = case_when(
    gci == 0 & gc_50_over_200 == -1 ~ 1,
    TRUE ~ 0
  )) %>%
  mutate(gc_date = case_when(
    gc == 1 ~ date
  )) %>%
  mutate(dc_date = case_when(
    dc ==1 ~ date
  ))

### Find first gc date and then join to main df

df2 <- df %>%
  filter(is.na(gc_date) == FALSE) %>%
  group_by(tic) %>%
  filter(gc_date == min(gc_date)) %>%
  select(tic, gc_date) %>%
  rename(first_gc = gc_date)

df <- df %>%
  inner_join(df2)

### Find +/- 120 days from gc

df3 <- df %>%
  mutate(time_to_gc = difftime(first_gc, date, units = "days")) %>%
  filter(time_to_gc >= -120 & time_to_gc <= 120) %>%
  mutate(time_to_gc = as.numeric(time_to_gc, units="days"))

### Create normalized stock prices

df3 <- df3 %>%
  mutate(logprice = log(prccd)) %>%
  arrange(date) %>%
  group_by(tic) %>%
  mutate(pct_change=(prccd/lag(prccd)-1)*100) %>%
  mutate(log_change=(logprice/lag(logprice)-1)*100)

df2 %>%
  group_by(tic) %>%
  summarize(length(unique(GVKEY)), mean(prccd), min(gc_date), max(gc_date))
