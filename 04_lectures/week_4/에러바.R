#26-0611 thu 11:14

#install.packages("psych")
library(psych)
library(GGally)
library(showtext)
showtext_auto()
library(palmerpenguins)
library(tidyverse)

# 시각화 (stat_summary 활용)
penguins |> 
  ggplot(aes(x = species, y = bill_length_mm, fill = species)) +
  stat_summary(fun = mean, geom = "bar", alpha = 0.8, show.legend = FALSE) +
  stat_summary(fun.min = function(x) mean(x) - sd(x),
               fun.max = function(x) mean(x) + sd(x),
               geom = "errorbar", width = 0.2, linewidth = 0.8) +
  theme_minimal()

#
penguins |> 
  summary()

penguins |> 
  describe()

mpg |> 
  describe()

#
penguins |> 
  ggplot(aes(x = body_mass_g)) +
  geom_histogram()

penguins |> 
  ggplot(aes(x = bill_depth_mm)) +
  geom_histogram()

penguins |> 
  glimpse()

penguins |> 
  str()

# ggpair ----
library(GGally)
ggpairs(penguins, 
        aes(color = species),
        columns = c('bill_length_mm', 
                    'bill_depth_mm', 
                    'flipper_length_mm', 
                    'body_mass_g')
        ) +
  scale_color_brewer(palette = 'Set1') +
  scale_fill_brewer(palette = 'Set1') 

#
penguins |> 
  ggplot(aes(x = body_mass_g, y = bill_length_mm)) +
  geom_point(aes(color = species)) +
  facet_wrap(.~island) +
  theme_minimal() +
  theme(legend.position = 'top')


#
penguins |> 
  drop_na() |> 
  #filter(species == 'Adelie') |> 
  ggplot(aes(x = species, y = bill_length_mm, fill = species)) +
  stat_summary(fun = mean, geom = "bar")  +
  stat_summary(fun.min = function(x) mean(x) - sd(x),
               fun.max = function(x) mean(x) + sd(x), 
               geom = 'errorbar', width = .1, linewidth = .5) +
  facet_wrap(.~sex)


#
penguins |> 
  ggplot(aes(x = species, y = bill_length_mm, fill = species)) +
  stat_summary(fun = mean, geom = 'bar') +
  scale_fill_brewer(palette = 'Dark2') +
  stat_summary(fun.min = function(x) mean(x) - sd(x),
               fun.max = function(x) mean(x) + sd(x),
               geom = "errorbar", width = 0.2, linewidth = 0.8) +
  theme_minimal()

# 표준편차 직관 ----
ggplot(mpg, aes(x = class, y = hwy, fill = class)) +
  stat_summary(fun = mean, geom = "bar", alpha = 0.8, width = 0.6, show.legend = FALSE) +
  stat_summary(fun.min = function(x) mean(x) - sd(x),
               fun.max = function(x) mean(x) + sd(x),
               geom = "errorbar", width = 0.15, linewidth = 1, color = "black") +
  scale_fill_brewer(palette = "Set1") +
  theme_minimal()

#




#
mpg |> 
  filter(class %in% c('subcompact', 'pickup')) |> 
  count(class, model)

# 신뢰구간 ----
library(tidyverse)
library(palmerpenguins)

# 데이터 준비 및 결측치 제거
penguins_clean <- penguins %>% 
  filter(!is.na(body_mass_g)) %>% 
  mutate(year = as.factor(year)) # 연도를 범주형 변수로 변환

# ggplot 시각화
ggplot(data = penguins_clean, aes(x = year, y = body_mass_g)) +
  # 1. 95% 신뢰구간 에러바 그리기 (mean_cl_boot 또는 mean_se 사용)
  stat_summary(
    fun.data = mean_cl_normal, # 정규분포 가정을 기반으로 95% CI 계산
    geom = "errorbar", 
    width = 0.1,               # 에러바 위아래 수평 모자(cap) 너비
    color = "#1f77b4", 
    size = 0.8
  ) +
  # 2. 각 연도별 평균값 점 찍기
  stat_summary(
    fun = mean, 
    geom = "point", 
    size = 3, 
    color = "#1f77b4"
  ) +
  # 3. 그래프 스타일링 (현장 보고서용 깔끔한 테마)
  theme_minimal(base_family = "NanumGothic") + # 한글 폰트 설정 시
  labs(
    title = "연도별 펭귄 몸무게의 95% 신뢰구간 (95% CI)",
    subtitle = "단순 평균 비교의 함정을 피하기 위한 확실성 검증",
    x = "조사 연도 (Year)",
    y = "몸무게 (Body Mass, g)"
  ) +
  theme(
    plot.title = element_text(face = "bold", size = 14),
    axis.title = element_text(size = 11),
    panel.grid.minor = element_blank()
  )

# 더 나은 제안 ----
penguins_clean <- penguins %>% filter(!is.na(bill_length_mm))

ggplot(penguins_clean, aes(x = species, y = bill_length_mm, fill = species)) +
  stat_summary(fun = mean, geom = "bar", alpha = 0.7, show.legend = FALSE) +
  stat_summary(fun.data = mean_cl_normal,   # 95% CI (t-distribution)
               geom = "errorbar", width = 0.2, linewidth = 0.9) +
  labs(title = "Bill Length by Species (Mean ± 95% CI)",
       subtitle = "M&E 보고서에 적합한 버전",
       y = "Bill Length (mm)") +
  theme_minimal()

#
ggplot(penguins_clean, aes(x = species, y = bill_length_mm, fill = species)) +
  geom_boxplot(alpha = 0.7, outlier.shape = NA) +
  geom_jitter(width = 0.2, alpha = 0.4, size = 1.5) +
  stat_summary(fun = mean, geom = "point", shape = 23, 
               size = 3, fill = "white") +
  labs(title = "Boxplot + Individual Points + Mean") +
  theme_minimal()

## 제미나이 버전 ----
penguins |> 
  filter(!is.na(bill_length_mm)) |> 
  ggplot(aes(x = species, y = bill_length_mm, color = species)) +
  # 95% 신뢰구간 계산 및 에러바 시각화
  stat_summary(fun.data = mean_cl_normal, geom = "errorbar", width = 0.1, linewidth = 0.8) +
  # 평균값 점 찍기
  stat_summary(fun = mean, geom = "point", size = 3) +
  theme_minimal() +
  labs(title = "품종별 부리 길이의 95% 신뢰구간(CI) 비교") +
  theme(legend.position = "none")

penguins |> 
  filter(!is.na(bill_length_mm)) |> 
  ggplot(aes(x = species, y = bill_length_mm, fill = species)) +
  geom_boxplot(alpha = 0.5, outlier.shape = NA, width = 0.5) +
  geom_jitter(aes(color = species), width = 0.15, alpha = 0.4, size = 1.5) +
  theme_minimal() +
  theme(legend.position = "none")

#
penguins |> 
  filter(!is.na(bill_length_mm)) |> 
  ggplot(aes(x = species, y = bill_length_mm, fill = species)) +
  # 데이터의 밀도(분포) 모양을 보여주는 바이올린
  geom_violin(alpha = 0.6, color = "white", trim = FALSE) +
  # 내부에 평균과 95% 신뢰구간 에러바 얹기
  stat_summary(fun.data = mean_cl_normal, geom = "pointrange", color = "black", size = 0.6) +
  theme_minimal() +
  theme(legend.position = "none")

# gpt ----
ggplot(penguins,
       aes(species,
           bill_length_mm,
           fill = species)) +
  geom_boxplot() +
  geom_jitter(width=.1,
              alpha=.3) +
  scale_fill_brewer(palette = 'Set1')
