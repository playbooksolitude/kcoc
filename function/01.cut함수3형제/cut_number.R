#26-0418 sat 10:44

library(tidyverse)

#
diamonds |> 
  count(cut, color)

diamonds |> 
  ggplot(aes(x = carat)) +
  geom_histogram() # +
  #coord_cartesian(ylim = c(0,10))

#
diamonds |> 
  ggplot(aes(x = x)) +
  geom_histogram()

#
diamonds |> 
  ggplot(aes(x = y)) +
  geom_histogram()

#
diamonds |> 
  ggplot(aes(x = z)) +
  geom_histogram()

#
diamonds |> 
  count(carat, sort = T)

#
(seq(1,100) -> s1)
cut(s1, breaks = 2)


# cut_interval() ----
diamonds |> 
  mutate(interval_itv = cut_interval(carat, n = 10)) |> #, labels = T
  count(interval_itv)


# cut_width() ----
diamonds |> 
  mutate(interval_wd = cut_width(carat, width = .2, labels = F)) |> 
  count(interval_wd)


# cut_number() ----
diamonds |> 
  mutate(interval_nm = cut_number(carat, n = 10)) |> 
  count(interval_nm)


# ntile() ----
diamonds |> 
  mutate(nt = ntile(carat, n = 10), 
         number = row_number(), .before = 1) |> 
  ggplot(aes(x = factor(nt), y = carat)) +
  geom_point()


diamonds %>%
  mutate(interval_id = cut_number(carat, n = 10, labels = F)) %>%
  filter(interval_id == 2) # 2번째 그룹만 필터링















