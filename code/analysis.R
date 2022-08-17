library(rdrobust)

### Create feols models and Regression Discontinuity models for min and max events
# Not up to date; retained for posterity

# model_1 <- feols(normalized ~ time_to_gc * gc_pre_post + date, data = full_df)
# etable(model_1)
# 
# model_2 <- feols(normalized_2 ~ time_to_gc * gc_pre_post + date, data = get(feols_tbl[1]))
# etable(model_2)
# 
# model_1rd <- rdrobust(full_df$normalized, full_df$time_to_gc, c=0, p=1, h=70, kernel="uniform") %>%
#   summary()
# 
# model_2rd <- rdrobust(full_df$normalized_2, full_df$time_to_gc, c=0, p=1, h=70, kernel="uniform") %>%
#   summary()
# 
# rdplot(full_df$normalized_2, full_df$time_to_gc, c=0, p=1, h=70, kernel="uniform", title="Normalized to t=-120")
# 
# model_2rd_2 <- rdrobust(full_df$normalized_2, full_df$time_to_gc, c=0, p=1, h=10, kernel="uniform") %>%
#   summary()

### Better log analysis here
# Why are we using a normalized price? Let's go back to the natural log

market_cap = "All" # This is just for filling in graph names; not used to pull data set
table_name = "full_df"
feols_tbl = "df_70"

rdplot(get(table_name[1])$logprice, get(table_name[1])$time_to_gc, c=0, p=1, h=70, kernel="uniform",
       title=paste0(market_cap," Cap Log Price"))

# omg that looks great

model_log_rd <- rdrobust(get(table_name[1])$logprice, get(table_name[1])$time_to_gc, c=0, p=1, h=70, kernel="uniform") %>%
  summary()

model_log <- feols(logprice ~ time_to_gc * gc_pre_post, data = get(feols_tbl[1]))
etable(model_log)

#model_log_rd <- rdrobust(get(table_name[1])$prccd, get(table_name[1])$time_to_gc, covs=fixed_tic, c=0, p=1, h=70, kernel="uniform") %>%
#  summary()

### What's going on at t = -70?

pre_gc_log_model <- rdrobust(get(table_name[1])$logprice, get(table_name[1])$time_to_gc, c=-70, p=1, h=50, kernel="uniform") %>%
  summary()

rdplot(get(table_name[1])$logprice, get(table_name[1])$time_to_gc, c=-70, p=1, h=50, kernel="uniform",
       title=paste0(market_cap," Cap Log Price"))

model_log_neg70 <- feols(logprice ~ time_to_neg70 * neg70, data = df_neg_70)
etable(model_log_neg70)

ggplot(df_70, aes(x=time_to_gc, y=logprice, color=gc_pre_post)) +
  geom_point() +
  geom_vline(xintercept=0) +
  geom_smooth(method="lm", se=FALSE)
