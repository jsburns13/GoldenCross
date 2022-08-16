library(rdrobust)

### Create feols models and Regression Discontinuity models for min and max events

model_1 <- feols(normalized ~ time_to_gc * gc_pre_post + date, data = full_df)
etable(model_1)

model_2 <- feols(normalized_2 ~ time_to_gc * gc_pre_post + date, data = full_df)
etable(model_2)

model_1rd <- rdrobust(full_df$normalized, full_df$time_to_gc, c=0, p=1, h=70, kernel="uniform") %>%
  summary()

model_2rd <- rdrobust(full_df$normalized_2, full_df$time_to_gc, c=0, p=1, h=70, kernel="uniform") %>%
  summary()

rdplot(full_df$normalized_2, full_df$time_to_gc, c=0, p=1, h=70, kernel="uniform", title="Normalized to t=-120")

model_2rd_2 <- rdrobust(full_df$normalized_2, full_df$time_to_gc, c=0, p=1, h=10, kernel="uniform") %>%
  summary()

# Why are we using a normalized price? Let's go back to the natural log

rdplot(full_df$logprice, full_df$time_to_gc, c=0, p=1, h=70, kernel="uniform", title="Log Price")

# omg that looks great

model_log_rd <- rdrobust(full_df$logprice, full_df$time_to_gc, c=0, p=1, h=70, kernel="uniform") %>%
  summary()

model_log <- feols(logprice ~ time_to_gc * gc_pre_post, data = df_70)
etable(model_log)

### What's going on at t = -70?

pre_gc_log_model <- rdrobust(full_df$logprice, full_df$time_to_gc, c=-70, p=1, h=50, kernel="uniform") %>%
  summary()

rdplot(full_df$logprice, full_df$time_to_gc, c=-70, p=1, h=50, kernel="uniform", title="Log Price")

ggplot(df_70, aes(x=time_to_gc, y=logprice, color=gc_pre_post)) +
  geom_point() +
  geom_vline(xintercept=0) +
  geom_smooth(method="lm", se=FALSE)

