library(rdrobust)

### Create feols models and Regression Discontinuity models for min and max events

max_model1 <- feols(normalized ~ date * gc_pre_post, data = max_df_full) %>%
  etable()

max_model_1rd <- rdrobust(max_df_full$normalized, max_df_full$time_to_maxgc, c=0, p=1, h=75, kernel="uniform") %>%
  summary()

max_model_2rd <- rdrobust(max_df_full$normalized_2, max_df_full$time_to_maxgc, c=0, p=1, h=75, kernel="uniform") %>%
  summary()

rdplot(max_df_full$normalized, max_df_full$time_to_maxgc, c=0, p=1, h=75, kernel="uniform", nbins=1000)

rdplot(max_df_full$normalized_2, max_df_full$time_to_maxgc, c=0, p=1, h=75, kernel="uniform", nbins=1000)

rdplot(max_df_full$pct_change, max_df_full$time_to_maxgc, c=0, p=1, h=75, kernel="uniform", nbins=1000)

min_model1 <- feols(normalized ~ date * gc_pre_post, data = min_df_full) %>%
  etable()

min_model_1rd <- rdrobust(min_df_full$normalized, min_df_full$time_to_mingc, c=0, p=1, h=75, kernel="uniform") %>%
  summary()

min_model_2rd <- rdrobust(min_df_full$normalized_2, min_df_full$time_to_mingc, c=0, p=1, h=75, kernel="uniform") %>%
  summary()

rdplot(min_df_full$normalized, min_df_full$time_to_mingc, c=0, p=1, h=75, kernel="uniform", nbins=1000)

rdplot(min_df_full$normalized_2, min_df_full$time_to_mingc, c=0, p=1, h=75, kernel="uniform", nbins=1000)

rdplot(min_df_full$pct_change, min_df_full$time_to_mingc, c=0, p=1, h=75, kernel="uniform", nbins=1000)