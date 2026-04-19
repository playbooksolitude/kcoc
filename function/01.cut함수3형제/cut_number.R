#26-0418 sat 10:44

library(tidyverse)

#
set.seed(123)
(sample(3000, size = 4) -> s1)
(tibble(num = c(1:4), 
       s1 = s1) -> s2)

s2 |> 
  mutate(cnt = cut_number(s1, n = 2))

s1 |> 
  mutate(cnt = ntile(s1, n = 2)) |> 
  arrange(s1)

#
diamonds |> 
  count(cut, color)

#
diamonds |> 
  count(carat)

#
diamonds |> 
  mutate(num = cut_number(carat, n = 10)) |> 
  count(num)


#
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


# starwars ----
starwars |> 
  mutate(birth_num = cut_number(birth_year, 
                                n = 5, labels = F), 
         birth_num_labels = cut_number(birth_year, n = 5, labels = T)) |> 
  select(name, birth_year, birth_num, birth_num_labels)

starwars |> 
  mutate(birth_num = cut_number(birth_year, n = 5), 
         birth_interval = cut_interval(birth_year, n = 5)) |> 
  select(name, birth_year, birth_num, birth_interval)


starwars |> 
  mutate(birth_num = cut_number(birth_year, n = 5), 
         birth_interval = cut_interval(birth_year, n = 5)) |> 
  select(name, birth_year, birth_num, birth_interval) |> 
  count(birth_num)
  #count(birth_interval)


# 특수문자 제거 ----
## str_split_i
starwars |> 
  filter(!is.na(birth_year)) |> 
  mutate(range_label = cut_number(birth_year, n = 5) |> 
           as.character()) |> 
  mutate(
    # 1. 시작값: 문자열 전체에서 첫 번째 숫자를 바로 추출
    start_val = parse_number(range_label),
    
    # 2. 끝값: 쉼표(,)를 기준으로 뒷부분만 떼어낸 뒤 숫자 추출
    end_val = range_label |> 
      str_split_i(pattern = ",", i = 2) |> 
      parse_number()
  ) |> 
  select(name, birth_year, start_val, end_val)



# 다시 ----
starwars |> 
  drop_na(birth_year) |> 
  select(name, birth_year) |> 
  mutate(birth_number = cut_number(birth_year, n = 5)) |> 
  count(birth_number)
  
starwars |> 
  drop_na(birth_year) |> 
  select(name, birth_year) |> 
  mutate(birth_number = cut_number(birth_year, n = 10)) |> 
  count(birth_number)

starwars |> 
  drop_na(birth_year) |> 
  select(name, birth_year) |> 
  mutate(birth_interval = cut_interval(birth_year, n = 5)) |> 
  count(birth_interval)

starwars |> 
  drop_na(birth_year) |> 
  select(name, birth_year) |> 
  mutate(birth_interval = cut_interval(birth_year, n = 10)) |>
  count(birth_interval)

starwars |> 
  drop_na(birth_year) |> 
  select(name, birth_year) |> 
  mutate(birth_interval = cut_width(birth_year, n = 5))


starwars |> 
  drop_na(birth_year) |> 
  select(name, birth_year) |> 
  mutate(birth_interval = cut_interval(birth_year, n = 10))

# cut_width ----
starwars |> 
  drop_na(birth_year) |> 
  select(name, birth_year) |> 
  mutate(birth_width = cut_width(birth_year, 
                                 width = 100, 
                                 boundary = 0))

mpg |> 
  select(manufacturer, model, drv, class, cty) |> 
  mutate(cty_width = cut_width(cty, width = 5, boundary = 0)) |> 
  group_by(cty_width) |> 
  reframe(avg = mean(cty), n = n())


mpg |> 
  select(manufacturer, model, drv, class, cty) |> 
  mutate(cty_number = cut_number(cty, n = 10)) |> 
  group_by(cty_number) |> 
  reframe(avg = mean(cty), n = n())


mpg |> 
  select(manufacturer, model, drv, class, cty) |> 
  mutate(cty_interval = cut_interval(cty, n = 10)) |> 
  group_by(cty_interval) |> 
  reframe(avg = mean(cty), n = n())











