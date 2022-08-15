library(rdrobust)

### Create feols models and Regression Discontinuity models for min and max events

model_1 <- feols(normalized ~ time_to_gc * gc_pre_post, data = full_df)
etable(model_1)

model_2 <- feols(normalized_2 ~ time_to_gc * gc_pre_post, data = full_df)
etable(model_2)

model_1rd <- rdrobust(full_df$normalized, full_df$time_to_gc, c=0, p=1, h=70, kernel="uniform") %>%
  summary()

model_2rd <- rdrobust(full_df$normalized_2, full_df$time_to_gc, c=0, p=1, h=70, kernel="uniform") %>%
  summary()

rdplot(full_df$normalized_2, full_df$time_to_gc, c=0, p=1, h=70, kernel="uniform")
