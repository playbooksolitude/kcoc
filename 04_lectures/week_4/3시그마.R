#26-0608 sun 

#
library(tidyverse)
library(palmerpenguins)

#
set.seed(42)
tibble(
  id = 1:100,
  value = rnorm(100, mean = 50, sd = 15)
) -> df

# 2. 3시그마 경계값 계산 및 필터링 변수 추가
df_3sigma <- df %>% 
  mutate(
    # 전체 데이터의 평균과 표준편차 계산
    mu = mean(value),
    sigma = sd(value),
    
    # 3시그마 하한선/상한선 설정
    lower_bound = mu - (3 * sigma),
    upper_bound = mu + (3 * sigma),
    
    # 3시그마를 벗어나는 아웃라이어 판정 (TRUE/FALSE)
    is_outlier = value < lower_bound | value > upper_bound
  )

# 결과 확인
print(df_3sigma)

#
df_3sigma |> 
  ggplot(aes(x = value)) +
  geom_histogram(color = 'snow') +
  scale_x_continuous(breaks = c(10,20,30,40,50,60,70,80,90,100))

df_3sigma |> 
  filter(is_outlier == TRUE)

#
penguins |> 
  filter(species == 'Adelie') -> penguins_Adelie

#
penguins_Adelie |> 
  ggplot(aes(x = body_mass_g)) +
  geom_histogram()

penguins_Adelie |> 
  ggplot(aes(x = bill_length_mm)) +
  geom_histogram()

penguins_Adelie |> 
  ggplot(aes(x = bill_depth_mm)) +
  geom_histogram()



# 데이터 확인 및 3시그마 계산
penguins_3sigma <- penguins_Adelie %>%
  # 분석에 필요한 변수만 선택 (보기 편하게)
  select(species, island, bill_length_mm, bill_depth_mm, body_mass_g) %>% 
  mutate(
    # 1. bill_length_mm 3시그마 기준 설정
    length_mu  = mean(bill_length_mm, na.rm = TRUE),
    length_sd  = sd(bill_length_mm, na.rm = TRUE),
    length_low = length_mu - (3 * length_sd),
    length_upp = length_mu + (3 * length_sd),
    
    # 2. bill_depth_mm 3시그마 기준 설정
    depth_mu   = mean(bill_depth_mm, na.rm = TRUE),
    depth_sd   = sd(bill_depth_mm, na.rm = TRUE),
    depth_low  = depth_mu - (3 * depth_sd),
    depth_upp  = depth_mu + (3 * depth_sd),
    
    # 3. body_mass_g 3시그마 기준 설정
    mass_mu    = mean(body_mass_g, na.rm = TRUE),
    mass_sd    = sd(body_mass_g, na.rm = TRUE),
    mass_low   = mass_mu - (3 * mass_sd),
    mass_upp   = mass_mu + (3 * mass_sd),
    
    # 하나라도 3시그마 범위를 벗어나면 TRUE (결측치는 제외)
    is_outlier = (bill_length_mm < length_low | bill_length_mm > length_upp) |
      (bill_depth_mm < depth_low  | bill_depth_mm > depth_upp)  |
      (body_mass_g < mass_low     | body_mass_g > mass_upp)
  )

# 결과 확인
print(penguins_3sigma)

penguins_3sigma |> 
  count(is_outlier)

# -----
library(tidyverse)
library(palmerpenguins)

# 1. 데이터 필터링 및 각 시그마 경계값 계산
penguins_mass_multi_sigma <- penguins_Adelie %>% 
  filter(!is.na(body_mass_g)) %>% 
  mutate(
    mu = mean(body_mass_g),
    sigma = sd(body_mass_g),
    
    # 1시그마 경계
    low_1s = mu - (1 * sigma),
    upp_1s = mu + (1 * sigma),
    
    # 2시그마 경계
    low_2s = mu - (2 * sigma),
    upp_2s = mu + (2 * sigma),
    
    # 3시그마 경계
    low_3s = mu - (3 * sigma),
    upp_3s = mu + (3 * sigma)
  )

# 2. 히스토그램 및 다중 시그마 선 시각화
penguins_mass_multi_sigma %>% 
  ggplot(aes(x = body_mass_g)) +
  # 히스토그램 기본형
  geom_histogram(binwidth = 100, fill = "gainsboro", color = "darkgray") +
  
  # [중심] 평균선 (파란색 실선)
  geom_vline(aes(xintercept = mu, color = "Mean"), linetype = "solid", linewidth = 1.2) +
  
  # [1시그마] 녹색 점선 (약 68.26% 수용)
  geom_vline(aes(xintercept = low_1s, color = "1-Sigma (±1σ)"), linetype = "dotted", linewidth = 0.8) +
  geom_vline(aes(xintercept = upp_1s, color = "1-Sigma (±1σ)"), linetype = "dotted", linewidth = 0.8) +
  
  # [2시그마] 주황색 파선 (약 95.44% 수용)
  geom_vline(aes(xintercept = low_2s, color = "2-Sigma (±2σ)"), linetype = "longdash", linewidth = 0.8) +
  geom_vline(aes(xintercept = upp_2s, color = "2-Sigma (±2σ)"), linetype = "longdash", linewidth = 0.8) +
  
  # [3시그마] 빨간색 대시선 (약 99.73% 수용)
  geom_vline(aes(xintercept = low_3s, color = "3-Sigma (±3σ)"), linetype = "dashed", linewidth = 0.8) +
  geom_vline(aes(xintercept = upp_3s, color = "3-Sigma (±3σ)"), linetype = "dashed", linewidth = 0.8) +
  
  # 선 색상 맵핑 및 범례 이름 설정
  scale_color_manual(
    name = "정규분포 기준선",
    values = c(
      "Mean" = "royalblue",
      "1-Sigma (±1σ)" = "forestgreen",
      "2-Sigma (±2σ)" = "darkorange",
      "3-Sigma (±3σ)" = "firebrick"
    )
  ) +
  
  # 레이블 및 테마 설정
  labs(
    title = "아델리 펭귄 체중 분포 및 시그마(σ)별 경계선",
    subtitle = "시그마 구간이 넓어질수록 더 극단적인 데이터 영역을 의미합니다.",
    x = "체중 (body_mass_g)",
    y = "빈도수 (Count)"
  ) +
  theme_minimal() +
  theme(
    plot.title = element_text(face = "bold", size = 14),
    legend.position = "right" # 오른쪽에 범례 표시
  )
