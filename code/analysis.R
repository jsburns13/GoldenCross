### Graph it!

ggplot(data = df3, mapping = aes(x=time_to_gc, y=prccd, colour=tic)) +
  geom_line()
  