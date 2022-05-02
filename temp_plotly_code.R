tib <- tibble(year = 1:4, min = rnorm(4), max = 15+rnorm(4), avg_5yr = 5+rnorm(4), current = 8+rnorm(4))

plot_ly(tib, x= ~year, y=~current, name = "Current value", type = "scatter", mode="line") %>% 
  add_trace(y=~avg_5yr, name="5yr avg", type="scatter", mode="lines+markers", line = list(color = c('purple')), marker=list(color = 'purple')) %>% 
  add_ribbons(ymin = ~ min, ymax = ~ max, line = list(color = "red"), fillcolor = "rgba(7, 164, 181, 0.2)", name = "95% region")


