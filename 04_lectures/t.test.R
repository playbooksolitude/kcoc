#26-0617 wedn

#
library(tidyverse)
library(palmerpenguins)
library(treemapify)
#install.packages("effectsize")
library("effectsize")

#
penguins_clean <- penguins %>% na.omit()

# Gentoo vs Adelie의 body_mass_g 비교 예시
data_compare <- penguins_clean %>%
  filter(species %in% c("Gentoo", "Adelie"))

data_compare

ggplot(data_compare, aes(x = body_mass_g, fill = species)) +
  geom_histogram(bins = 20, alpha = 0.7) +
  facet_wrap(~ species, ncol = 1) +
  labs(title = "t-test 전에 분포 시각화") +
  theme_minimal()

# QQ-plot (더 명확함)
ggplot(data_compare, aes(sample = body_mass_g)) +
  stat_qq() + stat_qq_line() +
  facet_wrap(~ species) +
  labs(title = "Q-Q Plot으로 정규성 확인")

# 그룹별 정규성 검정
data_compare %>%
  group_by(species) %>%
  summarise(
    shapiro_p = shapiro.test(body_mass_g)$p.value,
    n = n()
  )

#
# 기본 t-test (등분산 가정)
t.test(body_mass_g ~ species, data = data_compare, var.equal = TRUE)

# Welch's t-test (등분산 가정 안 함 → 실무에서 더 자주 씀)
t.test(body_mass_g ~ species, data = data_compare, var.equal = FALSE)

penguins |> 
  drop_na(species, island) |> 
  group_by(species, island) |> 
  reframe(mean_bill_length = mean(bill_length_mm, na.rm = T),
          mean_bill_depth = mean(bill_depth_mm, na.rm = T), 
          mean_mass = mean(body_mass_g, na.rm = T), 
          .groups = 'drop',
          n = n())

#
penguins_summary <- penguins %>%
  filter(!is.na(body_mass_g)) %>% # 결측치 제거
  group_by(species, island) %>%
  summarise(
    n = n(),
    avg_mass = mean(body_mass_g),
    .groups = 'drop'
  )

ggplot(penguins_summary, aes(area = n, 
                             fill = avg_mass, 
                             label = paste(species, island, sep="\n"))) +
  geom_treemap() +
  # 타일 안에 레이블 추가 (자동 줄바꿈 reflow = TRUE)
  geom_treemap_text(colour = "white", place = "centre", reflow = TRUE) 
  scale_fill_viridis_c(name = "평균 몸무게 (g)")

# 효과크기 ----

# 턱끈(Chinstrap)과 젠투(Gentoo) 펭귄만 필터링하고 결측치를 제거합니다.
penguins_sub <- penguins %>% 
  filter(species %in% c("Chinstrap", "Gentoo")) %>% 
  filter(!is.na(body_mass_g))

# 3. 독립표본 t-검정 수행 (Welch's t-test)
# R은 기본적으로 등분산 가정이 필요 없는 안전한 '웰치 t-검정'을 수행합니다.
t_result <- t.test(body_mass_g ~ species, data = penguins_sub)

# t-검정 결과 전체 출력 (이 창에서 p-value를 확인할 수 있습니다)
print(t_result)

# 4. p-value만 깔끔하게 추출하기
p_value <- t_result$p.value
cat("\n[추출된 p-value]:", p_value, "\n")

# 5. 효과크기(Cohen's d) 구하기
# t-test와 한 쌍인 Cohen's d를 계산합니다.
cohens_d_result <- cohens_d(body_mass_g ~ species, data = penguins_sub)

# 효과크기 결과 출력
print(cohens_d_result)
  
# 
penguins |> str()
penguins |> count(island)


# sex 따른 체중 비교 ----
penguins |> 
  drop_na(sex, body_mass_g) -> penguins_sex
t.test(body_mass_g ~ sex, data = penguins_sex)

penguins |> 
  drop_na(island, body_mass_g) |> 
  filter(island %in% c('Biscoe', 'Dream')) -> penguins_island
t.test(body_mass_g ~ island, data = penguins_island)

penguins |> 
  drop_na(sex, body_mass_g) -> penguins_sex

# aov 
aov(body_mass_g ~ island + species, data = penguins)
lm(body_mass_g ~ island + species + sex, data = penguins)

## gpt ----
summary(
  aov(body_mass_g ~ island + species,
      data = penguins)
)

TukeyHSD(
  aov(body_mass_g ~ species,
      data = penguins)
)

summary(
  lm(body_mass_g ~ island + species + sex,
     data = penguins)
)



    