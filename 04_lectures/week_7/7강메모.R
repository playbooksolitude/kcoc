

koica_1_2024xlsx |> 
  separate_wider_delim(
    지역, 
    delim = " > ", 
    names = c("대륙", "지역1", "지역2"), 
    too_few = "align_start"
    #too_many = "merge"
  ) |> 
  select(사업번호, 대륙, 지역1, 지역2)



koica_1_2024xlsx |> 
  sapply(n_distinct) |> 
  enframe() |> 
  print(n = Inf)


koica_1_2024xlsx |> 
  ggplot(aes(x = 지출액)) +
  geom_histogram() +
  theme_minimal() +
  #scale_x_continuous(labels = scales::comma)  +
  facet_wrap(.~'원조구분(양자/다자간)', scales = 'free')

koica_1_2024xlsx |> 
  filter(약정액 < 80000000) |> 
  ggplot(aes(x = 약정액, y = 지출액, color = '원조구분(양자/다자간)')) +
  geom_point() +
  facet_wrap(.~'원조구분(양자/다자간)')  +
  theme(legend.position = 'top') 
  #geom_smooth(method = 'lm', se = F) +
  geom_abline(intercept = 0, slope = 1, 
              color = "grey50", linetype = "dashed", size = 0.8) +
  lims(x = c(0, 30000000), y = c(0, 30000000))

  
  koica_1_2024xlsx |> 
    count(원조유형)
  
koica_1_2024xlsx |> 
  group_by(원조유형) |> 
  reframe(사업건수 = n(),
          지출액합계 = sum(지출액),
          사업건당_지출액평균 = 지출액합계 / 사업건수) |> 
  ggplot(aes(x = 원조유형, y = 사업건당_지출액평균)) +
  geom_bar(stat = 'identity') +
  coord_flip()
  

koica_1_2024xlsx |> 
  drop_na(약정액, 지출액) |> 
  mutate(지출액비율 = 지출액 / 약정액,
         구분 = case_when(지출액비율 > 1 ~ '초과집행', 
                        지출액비율 == 1 ~ '준수', 
                        지출액비율 < 1 ~ '미달집행')
  ) |> 
  count(구분) -> koica_1_2024xlsx_table
  mutate(비율 = n / sum(n) * 100) |> 
  ggplot(aes(x = 구분, y = 비율, fill = 구분)) +
  geom_bar(stat = 'identity') +
  bbplot::bbc_style() +
  geom_text(aes(label = paste0(round(비율,1),"%")), 
            #position = position_stack(.5), 
            vjust = -.1,
            size = 6) 

  
  koica_1_2024xlsx |> 
    group_by(원조유형) |> 
    reframe(사업건수 = n(),
            지출액합계 = sum(지출액),
            사업건당_지출액평균 = 지출액합계 / 사업건수)

  
  koica_1_2024xlsx |> 
    drop_na(약정액, 지출액) |> 
    mutate(지출액비율 = 지출액 / 약정액) |> 
    select(사업번호, 사업실시기관명, 지출액비율, 지출액, 약정액) |> 
    filter(지출액비율 > 1) |> 
    arrange(desc(지출액비율)) |> 
    print(n = Inf)
  
  
  
  
  
  
  
  
  
  
  
  
  
  
    