#26-0308 sun 19:50

source("./p_load.R", echo = T)
library(ggalluvial)

oecd2023_5select

oecd2023_5select |> 
  is.na() |> 
  colSums() |> 
  data.frame()

### 
oecd2023_5select |> 
  group_by(사업구분) |> 
  reframe(총지출액 = sum(지출액_백만달러)) 







## 5 EDA 함수 ----
### 5-1 1. 분석하고 싶은 범주형 컬럼들을 벡터로 정의합니다.
target_groups <- c("대륙명_1", "대륙명_2","대륙명_3",
                   "수원국", "사업실시기관명", 
                   "사업구분", "사업분야", "원조유형")

# 2. purrr::map을 사용하여 각 컬럼별로 요약을 수행합니다.
# 결과는 각 컬럼명을 이름으로 가진 '리스트' 형태로 저장됩니다.
eda_list <- target_groups %>% 
  set_names() %>% # 결과 리스트에 컬럼 이름을 붙여줍니다 (나중에 호출하기 편함)
  map(~ {
    oecd2023_5select %>%
      group_by(across(all_of(.x))) %>% # 문자열로 된 컬럼명을 그룹화에 적용
      summarise(
        총지출액 = sum(지출액_백만달러, na.rm = TRUE),
        사업건수 = n(),
        .groups = "drop"
      ) %>%
      arrange(desc(총지출액))
  })

# --- 결과 확인 방법 ---

# 특정 결과만 보고 싶을 때 (예: 대륙명)
eda_list$대륙명_1
eda_list$대륙명_2
eda_list$대륙명_3
eda_list$수원국
eda_list$사업실시기관명
eda_list$사업구분
eda_list$사업분야


# 모든 결과를 콘솔에 한꺼번에 출력하고 싶을 때
walk(eda_list, print)

# (팁) 만약 모든 결과를 하나의 긴 테이블로 합치고 싶다면?
# '구분'이라는 컬럼이 추가되면서 하나로 합쳐집니다.
combined_eda <- eda_list %>% 
  bind_rows(.id = "분류기준")

#6. 감사 ----

oecd2023_5select |> 
  filter(사업번호 %in% c('2023110103738', '2023100103739', 
                     '2023019901707', '2023019901710')) 

oecd2023_5select |> 
  filter(
    grepl('미배분', 대륙명_2)
  )

### 6-0 대륙별 지원금 ----
oecd2023_5select |> 
  group_by(대륙명_1) |> 
  reframe(총지원금 = sum(지출액_백만달러), 
          사업건수 = n()) |> 
  ggplot(aes(x = 대륙명_1, y = 총지원금)) +
  geom_bar(stat = 'identity', show.legend = F, aes(fill = 대륙명_1)) +
  geom_label(aes(label = round(총지원금,1)), size = 7) +
  bbc_style() +
  scale_fill_brewer(palette = 'Set2') +
  ggtitle(label = '2023년 대륙별 지원금 규모', 
          subtitle = '단위: 백만달러') 
  


oecd2023_5select |> 
  group_by(사업분야) |> 
  reframe(총지원금 = sum(지출액_백만달러), 
          사업건수 = n()) |> 
  arrange(desc(총지원금),사업건수)
  # ggplot(aes(area = 총지원금, label = 사업건수, fill = 사업분야)) +
  # geom_treemap() +
  # geom_treemap_text()

  

### 6-1 waffle 원조유형 ----
oecd2023_5select |> 
  group_by(대륙명_1, 원조유형) |> 
  reframe(총지원금 = sum(지출액_백만달러), 
          사업건수 = n()) |> 
  arrange(desc(사업건수)) |> 
  ggplot(aes(fill = 원조유형, values = 총지원금)) +
  geom_waffle(flip = T, 
              n_rows = 5) +
  facet_wrap(.~대륙명_1, nrow = 1, strip.position = 'bottom')  +
  #coord_equal() +
  theme_void() +
  theme(strip.text = element_text(size = 18), 
        legend.position = 'top') +
  scale_fill_brewer(palette = 'Set3')


oecd2023_5select |> 
  filter(대륙명_1 == '그외')



### 6-2 treemap 대륙별 ----
oecd2023_5select |> 
  group_by(대륙명_1) |> 
  reframe(총지원금 = sum(지출액_백만달러), 
          사업건수 = n()) |> 
  ggplot(aes(area = 총지원금, fill = 총지원금, 
             label = 대륙명_1, subgroup2 = paste0(comma(사업건수),"건"),
             subgroup = round(총지원금,1))) +
  geom_treemap(color = 'snow', size = 4) +
  geom_treemap_text(colour = 'snow', place = "topleft", reflow = T, alpha = .7) +
  geom_treemap_subgroup_text(color = 'snow', size = 42,
                              grow = F, place = 'center', alpha = .7) +
  geom_treemap_subgroup2_text(color = 'snow', size = 22,
                             grow = F, place = 'bottomright') +
  scale_fill_viridis_c()  + #색맹 color Blindness 고려
  labs(title = "대륙별 지원 비중 (트리맵)", subtitle = '단위: 백만달러') +
  scale_y_continuous(labels = scales::comma_format(suffix = "M$")) +
  theme(strip.text = element_text(size = 28, color = 'black'), 
        strip.background = element_rect(fill = 'snow'), 
        legend.position = 'none') 




library(tidyverse)
library(treemapify)
library(scales)

oecd2023_5select |> 
  group_by(대륙명_1) |> 
  reframe(총지원금 = sum(지출액_백만달러, na.rm = TRUE), 
          사업건수 = n()) |> 
  ggplot(aes(area = 총지원금, fill = 총지원금, 
             label = 대륙명_1, # 대륙명을 메인 라벨로
             subgroup = round(총지원금, 1), # 지원금을 서브그룹으로
             subgroup2 = paste0(comma(사업건수), "건"))) + # 건수를 서브그룹2로
  geom_treemap(color = 'snow', size = 2) + # 테두리 사이즈를 약간 줄여 세련미 강조
  
  # 1. 대륙명 (가장 강조하고 싶은 텍스트 - 상단 배치)
  geom_treemap_text(colour = "snow", place = "topleft", 
                    reflow = TRUE, family = "NanumGothic", fontface = "bold") +
  
  # 2. 총지원금 (중앙에 크게 배치하여 임팩트 부여)
  geom_treemap_subgroup_text(color = 'snow', size = 42, alpha = 0.5, # 약간 투명하게
                             grow = FALSE, place = 'center', family = "NanumGothic") +
  
  # 3. 사업건수 (하단에 작게 배치)
  geom_treemap_subgroup2_text(color = 'snow', size = 22,
                              grow = FALSE, place = 'bottomright', family = "NanumGothic") +
  
  scale_fill_viridis_c() + 
  labs(title = "대륙별 지원 비중 (트리맵)", 
       subtitle = '단위: 백만달러 (M$)', # 단위 명시
       caption = "출처: OECD ODA 데이터 2023") +
  theme_minimal() + # 기본 테마를 깔끔하게 변경
  theme(plot.title = element_text(size = 20, face = "bold"),
        legend.position = 'none')
  
### 6-3 treemap 수원국 ----
oecd2023_5select |> 
  group_by(대륙명_1, 수원국) |> 
  reframe(총지원금 = sum(지출액_백만달러), 
          사업건수 = n()) |> 
  filter(대륙명_1 %in% c('아시아', '아프리카')) |> 
ggplot(aes(area = 총지원금, fill = 총지원금, label = 수원국,
           subgroup = 대륙명_1)) +
  geom_treemap(show.legend = F) +
  # geom_treemap_subgroup_text(grow = T, color = 'grey60', 
  #                            size = 6, place = 'center') +
  geom_treemap_text(colour = "snow", place = "centre", reflow = T) +
  facet_wrap(.~대륙명_1) +
  labs(title = "수원국별 지원 비중 (트리맵)") +
    theme(strip.text = element_text(size = 28, color = 'black'), 
          strip.background = element_rect(fill = 'snow'))



### 6-4 treemap 수원국 ----
oecd2023_5select |> 
  group_by(원조유형) |> 
  reframe(총지원금 = sum(지출액_백만달러), 
          사업건수 = n()) |> 
  mutate(지원금비중 = 총지원금 / sum(총지원금) * 100) |> 
  ggplot(aes(area = 총지원금, fill = 총지원금, 
             label = 원조유형,
             subgroup = 사업건수, 
             subgroup2 = paste0(round(지원금비중,1),"%"))) +
  geom_treemap() +
  geom_treemap_text(colour = "snow", place = "center", reflow = T, size = 40) +
#  geom_treemap_subgroup_text(color = 'grey90', size = 32, place = 'center') +
  #geom_treemap_subgroup2_text(colour = "snow", place = "bottom",  reflow = T, size = 40) +
  #facet_wrap(.~원조유형) +
  labs(title = "수원국별 지원 비중 (트리맵)") +
  theme(strip.text = element_text(size = 28, color = 'black'), 
        strip.background = element_rect(fill = 'snow'))


### 6-5 treemap 사업유형 ----
oecd2023_5select |> 
  group_by(원조유형) |> 
  reframe(총지원금 = sum(지출액_백만달러), 
          사업건수 = n()) |> 
  #filter(대륙명_1 %in% c('아시아', '아프리카')) |> 
  ggplot(aes(area = 총지원금, fill = 총지원금, label = 원조유형,
             subgroup = 원조유형)) +
  geom_treemap() +
  # geom_treemap_subgroup_text(grow = T, color = 'grey60', 
  #                             size = 6, place = 'center') +
  geom_treemap_text(colour = "snow", place = "centre", reflow = T) 
#facet_wrap(.~원조유형) +
labs(title = "수원국별 지원 비중 (트리맵)") +
  theme(strip.text = element_text(size = 28, color = 'black'), 
        strip.background = element_rect(fill = 'snow'))
  
  
### 6-6 상위 10개국 원조 ----
# 상위 10개 수원국 추출
top10_countries <- oecd2023_5select %>%
  group_by(대륙명_1, 수원국) %>%
  summarise(total = sum(지출액_백만달러, na.rm = TRUE)) %>%
  arrange(desc(total)) %>%
  slice(2:11) %>%
  pull(수원국)

oecd2023_5select %>%
  filter(수원국 %in% top10_countries) %>%
  ggplot(aes(x = 수원국 |> fct_reorder(지출액_백만달러), 
             y = 지출액_백만달러, 
             fill = 원조유형)) +
  geom_col() +
  coord_flip() +
  facet_wrap(.~대륙명_1, scales = 'free_y') +
  scale_fill_viridis_d(option = "plasma") +
  labs(
    title = "대륙별 지원금 상위 10개국",
    subtitle = " 원조유형비중 확인",
    x = "수원국", y = "지출액 (백만달러)",
    fill = "원조 유형"
  ) +
  theme_minimal(base_family = "NanumGothic") +
  theme(legend.position = "top", 
        axis.text = element_text(size = 12),
        legend.text = element_text(size = 10), 
        strip.text = element_text(size = 16))


oecd2023_5select |> colnames()
oecd2023_5select |> str()


### 6-7 미배분사업 ----  
oecd2023_5select |> 
  filter(str_detect(수원국, '미배분')) |> 
  count(사업실시기관명, 사업분야) |> #print(n = Inf)
  ggplot(aes(x = 사업실시기관명, y = 사업분야, fill = n)) +
  geom_tile(color = 'snow', show.legend = F) +
  geom_text(aes(label = n), color = 'snow', size = 5) +
  theme_minimal() +
  ggtitle(label = '2023년도 미배분 사업') +
  theme(axis.text.x = element_text(angle = 90, hjust = 1, vjust = .5, 
                                   size = 12), 
        axis.text.y = element_text(size = 12), 
        plot.title = element_text(size = 24)) 


### 6-8 성평등 ----
oecd2023_5select |> 
  group_by(성평등) |> 
  reframe(total = sum(지출액_백만달러), 
          n = n()) |> 
  mutate(prop = total / sum(total) * 100) |> 
  ggplot(aes(x = '성평등', y = total, fill = 성평등)) +
  geom_bar(stat = 'identity', position = 'stack') +
  coord_flip() +
  geom_text(
    aes(label = paste0(round(prop, 1), "%\n(", round(total, 1), "M$)")), 
            size = 5,
            lineheight = 0.8, # 줄 간격 조절
            position = position_stack(vjust = 0.5)) +
  theme_void() +
  ggtitle(label = '성평등 관련 지출액 규모') +
  theme(legend.position = 'top', 
        legend.margin = margin(t = 15, b = 10), 
        legend.text = element_text(size = 10)) +
  guides(fill = guide_legend(reverse = T)) #범례 일치 
  
  
### 6-9 생키 차트 ----
oecd2023_5select |>
  group_by(원조유형, 대륙명_1, 사업분야) |>
  summarise(total = sum(지출액_백만달러, na.rm = TRUE), .groups = "drop") |>
  # 상위 노드 위주로 시각화하기 위해 필터링 (선택 사항)
  filter(total > 5) |> 
  ggplot(aes(y = total, axis1 = 원조유형, axis2 = 대륙명_1, 
             axis3 = 사업분야)) +
  geom_alluvium(aes(fill = 원조유형), width = 1/12) +
  geom_stratum(width = 1/12, fill = "grey80", color = "white") +
  geom_text(stat = "stratum", aes(label = after_stat(stratum)), size = 3) +
  scale_x_discrete(limits = c("원조유형", "대륙", "분야"), 
                   expand = c(.05, .05)) +
  scale_fill_viridis_d(alpha = 0.8) +
  theme_minimal() +
  labs(title = "2023 KOICA 원조 자금 흐름도", y = "지출액 (백만달러)", 
       fill = "원조유형") +
  theme(legend.position = "bottom")


### 6-10 
library(sf)
library(rnaturalearth)
library(countrycode)

# # 1. 지도 데이터 가져오기
# world <- ne_countries(scale = "medium", returnclass = "sf")
# 
# # 2. 데이터 준비 (국가명 매칭을 위해 영문명 추가)
# map_data <- oecd2023_5select |>
#   group_by(수원국) |>
#   summarise(total = sum(지출액_백만달러, na.rm = TRUE)) |>
#   # 한국어 국가명을 ISO 영문 코드로 변환 (예: 베트남 -> VNM)
#   mutate(iso_code = countrycode(수원국, "korean.name", "iso3c"))
# 
# # 3. 지도와 결합 및 시각화
# world |>
#   left_join(map_data, by = c("iso_a3" = "iso_code")) |>
#   ggplot() +
#   geom_sf(aes(fill = total), color = "white", size = 0.1) +
#   scale_fill_distiller(palette = "YlGnBu", direction = 1, na.value = "grey90",
#                        name = "지출액(M$)") +
#   theme_void() +
#   labs(title = "2023 KOICA 전 세계 수원국별 지원 현황") +
#   theme(legend.position = "bottom")


### 6-10 SDGs ----
library(fmsb)

#### 6-10-1 data check ----
  oecd2023_5select |>
  separate_rows(SDGs, sep = "; ") |> 
  mutate(sdg_main = as.numeric(str_extract(SDGs, "^[0-9]+"))) |> 
  filter(!is.na(sdg_main)) |>
  group_by(sdg_main) |>
  summarise(total = sum(지출액_백만달러, na.rm = TRUE), n = n()) 

# 1. 데이터 정리: 공백 유무에 상관없이 분리하고 1~17번 순서 고정
sdg_summary <- oecd2023_5select |>
  # "; " 또는 ";" 모두 대응하도록 정규표현식(sep = ";\\s*") 사용
  separate_rows(SDGs, sep = ";\\s*") |> 
  mutate(sdg_main = as.numeric(str_extract(SDGs, "^[0-9]+"))) |> 
  filter(!is.na(sdg_main)) |>
  group_by(sdg_main) |>
  summarise(total = sum(지출액_백만달러, na.rm = TRUE)) |>
  complete(sdg_main = 1:17, fill = list(total = 0)) |> 
  # [핵심] 반시계 방향(1 -> 17 -> 16...) 정렬 로직
  mutate(sort_order = ifelse(sdg_main == 1, 0, 18 - sdg_main)) |> 
  arrange(sort_order) |> 
  mutate(sdg_label = paste0("G", sdg_main)) |> 
  select(sdg_label, total) |> 
  pivot_wider(names_from = sdg_label, values_from = total)

# 2. 레이더 차트용 상한/하한행 추가
# max_val을 수동으로 넉넉하게 잡거나 데이터의 최대값으로 설정
max_val <- 250 # Goal 17이 약 243이므로 250으로 고정하면 눈금 보기가 편합니다.
radar_df <- rbind(rep(max_val, 17), 
                  rep(0, 17), 
                  sdg_summary)

# 3. 차트 그리기
radarchart(radar_df,
           axistype = 1,
           # 디자인 설정
           pcol = rgb(0.1, 0.4, 0.8, 0.9), pfcol = rgb(0.1, 0.4, 0.8, 0.3), plwd = 3,
           cglcol = "grey70", cglty = 1, axislcol = "grey30", 
           # 축 눈금 표시 (0, 62.5, 125, 187.5, 250)
           caxislabels = seq(0, max_val, length.out = 5),
           title = "SDGs 기여도 (G1 기준 반시계 방향)")




  




 