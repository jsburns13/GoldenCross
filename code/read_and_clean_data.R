library(tidyverse)
library(fixest)
library(vtable)
library(zoo)

### Run filter_dates.R first to filter down to desired dates if .csv files
### aren't ready to go in the /data/ folder

filename = "daily_stonks_NYSE_Mid_2000_2009.csv"

df <- read_csv(paste0("data/", filename))
  
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

### Find max gc date and then join to main df
### Choosing max to make sure the stocks have been listed for some time
### and avoid situations where something crosses the golden threshold
### very early; might be a source of variability

max_df <- df %>%
  filter(is.na(gc_date) == FALSE) %>%
  group_by(tic) %>%
  filter(gc_date == max(gc_date)) %>%
  select(tic, gc_date) %>%
  rename(last_gc = gc_date)

### We'll also find the min (first) gc to compare as well

min_df <- df %>%
  filter(is.na(gc_date) == FALSE) %>%
  group_by(tic) %>%
  filter(gc_date == min(gc_date)) %>%
  select(tic, gc_date) %>%
  rename(first_gc = gc_date)

df <- df %>%
  inner_join(max_df) %>%
  inner_join(min_df)

### Summary table for info

df %>%
  group_by(exchg) %>%
  summarise(
    ticker_count = n_distinct(tic)
    ,mean(first_gc)
    ,mean(last_gc)
    ,min(prccd)
    ,mean(prccd)
    ,max(prccd)
            )

### Find +/- 120 days from last gc

max_df_full <- df %>%
  mutate(time_to_maxgc = -difftime(last_gc, date, units = "days")) %>%
  filter(time_to_maxgc >= -120 & time_to_maxgc <= 120) %>%
  mutate(time_to_maxgc = as.numeric(time_to_maxgc, units="days"))

### Create normalized stock prices

max_df_full <- max_df_full %>%
  mutate(logprice = log(prccd)) %>%
  arrange(date) %>%
  group_by(tic) %>%
  mutate(pct_change=(prccd/lag(prccd)-1)*100) %>%
  replace_na(list(pct_change = 1)) %>%
  mutate(log_change=(logprice/lag(logprice)-1)*100) %>%
  mutate(gc_pre_post = case_when(
    time_to_maxgc >= 0 ~ 1,
    time_to_maxgc < 0 ~ 0
  )) %>%
  mutate(normalized=case_when(
    time_to_maxgc == 0 ~ 1,
    time_to_maxgc > 0 ~ cumprod(case_when(
      time_to_maxgc > 0 ~ 1 + (pct_change/100),
      TRUE ~ 1
    ))
  )) %>%
  arrange(desc(date)) %>%
  group_by(tic) %>%
  mutate(normalized = case_when(
    is.na(normalized) & time_to_maxgc < 0 ~ 1/(
      cumprod(
        case_when(
          time_to_maxgc <= 0 ~ 1 + (
            pct_change/100
            ),
          TRUE ~ 1
          )
        )/(
          1 + (pct_change/100)
          )
      ),
    TRUE ~ normalized
    )
    ) %>%
  arrange(date) %>%
  group_by(tic) %>%
  mutate(normalized_2 = case_when(
    time_to_maxgc == min(time_to_maxgc) ~ 1,
    TRUE ~ cumprod(1+(pct_change/100))
  )) %>%
  rename(time_to_gc = time_to_maxgc)

### Find +/- 120 days from first gc

min_df_full <- df %>%
  mutate(time_to_mingc = -difftime(first_gc, date, units = "days")) %>%
  filter(time_to_mingc >= -120 & time_to_mingc <= 120) %>%
  mutate(time_to_mingc = as.numeric(time_to_mingc, units="days"))

### Create normalized stock prices

min_df_full <- min_df_full %>%
  mutate(logprice = log(prccd)) %>%
  arrange(date) %>%
  group_by(tic) %>%
  mutate(pct_change=(prccd/lag(prccd)-1)*100) %>%
  replace_na(list(pct_change = 1)) %>%
  mutate(log_change=(logprice/lag(logprice)-1)*100) %>%
  mutate(gc_pre_post = case_when(
    time_to_mingc >= 0 ~ 1,
    time_to_mingc < 0 ~ 0
  )) %>%
  mutate(normalized=case_when(
    time_to_mingc == 0 ~ 1,
    time_to_mingc > 0 ~ cumprod(case_when(
      time_to_mingc > 0 ~ 1 + (pct_change/100),
      TRUE ~ 1
    ))
  )) %>%
  arrange(desc(date)) %>%
  group_by(tic) %>%
  mutate(normalized = case_when(
    is.na(normalized) & time_to_mingc < 0 ~ 1/(
      cumprod(
        case_when(
          time_to_mingc <= 0 ~ 1 + (
            pct_change/100
          ),
          TRUE ~ 1
        )
      )/(
        1 + (pct_change/100)
      )
    ),
    TRUE ~ normalized
  )
  ) %>%
  arrange(date) %>%
  group_by(tic) %>%
  mutate(normalized_2 = case_when(
    time_to_mingc == min(time_to_mingc) ~ 1,
    TRUE ~ cumprod(1+(pct_change/100))
  )) %>%
  rename(time_to_gc = time_to_mingc)

rm(df)

### Join both min and max data sets

trigger <- exists('full_df')

if (trigger) {
  full_df <- full_df %>%
    full_join(max_df_full) %>%
    full_join(min_df_full)
} else {
  full_df <- max_df_full %>%
    full_join(min_df_full)
}

rm(max_df, max_df_full, min_df, min_df_full)

### Adding an indicator to allow for TIC to be used as a fixed effect

#fixed_tic = model.matrix(~full_df$tic+0)

### Create a focused data frame for t +/- 70

df_70 <- full_df %>%
  filter(time_to_gc >= -70 & time_to_gc <= 70)

df_neg_70 <- full_df %>%
  filter(time_to_gc >= -120 & time_to_gc <= -20) %>%
  mutate(time_to_neg70 = time_to_gc + 70) %>%
  mutate(neg70 = case_when(
    time_to_neg70 < 0 ~ 0,
    TRUE ~ 1
  ))
