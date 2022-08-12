### Graph it!

ggplot(data = df3, mapping = aes(x=time_to_gc, y=log_change, group=tic)) +
  geom_line()
