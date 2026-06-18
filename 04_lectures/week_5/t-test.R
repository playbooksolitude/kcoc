#26-0616 tues


library(writexl)
library(ggpubr)
library(tidyverse)
library(ggrepel)

# https://grok.com/chat/fae09df6-3c68-43d8-8b7b-fd96192a41b6
# https://grok.com/c/6eac1ec0-bf08-4beb-9522-e990ca7ba7d4
# https://grok.com/chat/a11b32a9-30b0-4ce2-94c8-d5bbc79e59ef
# 2 종류의 수면제(진정제) 효과를 비교한 실험 데이터
# 10명의 환자를 대상으로 수면 시간 증가 효과를 측정
# 10명에게 각각 다른 두 종류의 약을 투여
# paird data

#sleep 데이터는 동일한 10명의 환자가 두 약물을 모두 받은 쌍체 설계 데이터이므로, Paired t-test가 적합

?sleep
sleep |> names()

#data 변형
sleep

pivot_longer(data = sleep, cols = group,
             names_to = 'name', values_to = 'value')

# ID 기준 평균 
sleep |> 
  group_by(ID) |> 
  reframe(avg = mean(extra), 
          n = n())

## 3-1 평균 
sleep |> 
  group_by(group) |> 
  reframe(avg = mean(extra), 
          n = n())


#2-1 실험 기준 pivot_wider
sleep |> 
  pivot_wider(names_from = group, 
              values_from = extra, 
              names_prefix = '수면약_')

#2-2 ID 기준 pivot_wider
sleep |> 
  pivot_wider(names_from = ID, 
              values_from = extra, 
              names_prefix = '참가자_')


#2-3 ID 기준 pivot_longer
sleep |> 
  pivot_wider(names_from = ID, 
              values_from = extra, 
              names_prefix = '참가자_') |> 
  pivot_longer(cols = !group, 
               names_to = 'ID', 
               values_to = 'extra')




#3 편차 구하기
sleep |> 
  group_by(group) |> 
  mutate(extra_deviation = extra - mean(extra)) |> 
  ungroup() 



# 4 시각화 ----
#4-1 geom_point
sleep |> 
  ggplot(aes(x = group, y = extra, color = group)) +
  geom_point(size = 5) 


#4-2 오버플롯팅
sleep |> 
  count(extra, sort = T)


sleep |> 
  ggplot(aes(x = group, y = extra, color = group)) +
  geom_point(position = position_jitter(.1), size = 5)


#4-3 비교
library(patchwork)

sleep |> 
  ggplot(aes(x = group, y = extra, color = group)) +
  geom_point(size = 5, show.legend = F) +
  theme_minimal() +
  theme(axis.title = element_text(size = 18), 
        axis.text = element_text(size = 16)) +
  sleep |> 
  ggplot(aes(x = group, y = extra, color = group)) +
  geom_point(position = position_jitter(.1), size = 5, 
             show.legend = F) +
  theme_minimal() +
  theme(axis.title.y.left = element_blank(), 
        axis.title.x = element_text(size = 18), 
        axis.text = element_text(size = 16)) +
  plot_layout(guides = 'collect')


# ggpaired ----
ggpaired(sleep, 
         x = "group", 
         y = "extra", 
         point.size = 4,
         color = "group", 
         line.color = "gray50", 
         line.size = 0.4) +
  ggtitle(label = 'sleep visualization') +
  theme(legend.position = 'none') +
  labs(x = 'group', y = 'extra')

#
(ggpaired(sleep, x = "group", y = "extra",
          color = "ID", line.color = "gray", line.size = 0.4,
          palette = "npg") +
    ggtitle(label = 'sleep visualization') -> b)

ggarrange(a, b)


# geom_path ----
sleep |> 
  ggplot(aes(x = group, y = extra, color = ID)) +
  geom_point() +
  geom_path(aes(group = ID))  


# geom_line() ----
ggplot(sleep, aes(x = group, y = extra, group = ID)) +
  # 1. 배경에 박스플롯 그리기 (color에 group을 주어 색상 분리)
  # geom_boxplot(aes(fill = group), alpha = 0.3, outlier.shape = NA) +
  # # 2. 동일 인물의 전/후 데이터를 잇는 선 그리기
  geom_line(color = "gray50", size = 0.4) +
  # 3. 개별 데이터 점 찍기
  geom_point(aes(color = group), size = 4) +
  # 깔끔한 테마 적용
  theme_minimal()


#
# geom_label() ----
ggplot(sleep, aes(x = group, y = extra, group = ID)) +
  # 1. 배경에 박스플롯 그리기 (color에 group을 주어 색상 분리)
  # 2. 동일 인물의 전/후 데이터를 잇는 선 그리기
  geom_line(color = "gray50", size = 0.4) +
  geom_point(color = "gray50", size = 0.4) +
  # 3. 개별 데이터 점 찍기
  geom_label(aes(label = ID, 
                 color = group), size = 6) +
  # 깔끔한 테마 적용
  theme_minimal()


## nudge_x ----
ggplot(sleep, aes(x = group, y = extra, group = ID)) +
  geom_line(color = "gray50", size = 0.4) +
  geom_label_repel(
    aes(label = ID, 
        color = group), 
    size = 5,
    nudge_x = ifelse(sleep$group == 1, -0.15, 0.15)) +
  theme_minimal()


# test ----
# geom_point()# test ----
?t.test()
t.test(extra ~ 1, data = sleep)
t.test(sleep, paired = T)

# One sample t-test ----
## 이렇게 할 경우 그룹1, 그룹2 가리지 않고 0과 비교함
t.test(sleep$extra)


# Two sample t-test ----
## 대응표본(쌍체표본) 
# pivot_wider ----
(sleep |> 
   pivot_wider(names_from = group, 
               values_from = extra, names_prefix = 'group_') -> sleep_3)

t.test(sleep_3$group_1, sleep_3$group_2, paired = TRUE)


# t.test 종류 3가지 ----
## one smaple 단일표본 
## two sample 독립표본 
## two sample 대응표본 3가지만 존재 


# 올바른 방식 ----
t.test(sleep_3$group_1, sleep_3$group_2, paired = T)
#자유도가 9

##잘못된 방식 ----
t.test(extra ~ group, data = sleep)
#자유도가 17


#Welch Two Sample t-test:
# 가정: 두 그룹이 독립적이며, 두 그룹의 분산이 같지 않을 수 있음(Welch의 수정 적용). sleep 데이터에서는 동일 환자가 두 약물을 모두 받았으므로 이 가정이 맞지 않음.


#25-0807 thu 
shapiro.test(resid(sleep))

# 정규분포 여부 ----
?sleep
lm(extra ~ group, data = sleep) -> output
shapiro.test(resid(output))

#
qqnorm(resid(output))
qqline(resid(output), col = 'tomato')
?t.test()
sleep |> 
  group_by(group) |> 
  reframe(mean = mean(extra))


plot(extra ~ group, data = sleep)

sleep
?t.test()
# 등분산 여부 ----
var.test(extra ~ group, data = sleep)

# t.test ----
## 잘못된 경우
t.test(extra ~ group, data = sleep, paird = TRUE)

sleep |> 
  ggplot(aes(x = ID, y = extra, color = group)) +
  geom_point() +
  geom_path(aes(group = group))

sleep |> 
  pivot_wider(names_from = ID, values_from = extra)

t.test(extra ~ group, data = sleep, paired = T)

# 도움말 - 정석 ----
t.test(sleep$extra)


# One-sample t-test ----
## 한 집단의 평균을 특정 값과 비교
#특정 약을 복용한 환자 10명의 수면 시간 증가량(extra) 평균이 0(효과 없음)인지 확인
#group_1의 평균이 0과 다른지 알려줍니다.
t.test(extra ~ 1, data = sleep)


#Two-sample t-test ----
## 두 집단의 평균 차이
sleep2 <- reshape(sleep, direction = "wide",
                  idvar = "ID", timevar = "group")

## Traditional interface
### t.test()에 데이터를 직접 벡터 형태로 입력하는 방식
t.test(sleep2$extra.1, sleep2$extra.2, paired = TRUE)

## Formula interface
### t.test()에 공식형태로 입력하는 방식
t.test(Pair(extra.1, extra.2) ~ 1, data = sleep2)
?t.test()

# cor ----
sleep
cor.test(sleep$extra, sleep$group)
sleep |> 
  mutate(group = as.integer(group)) -> sleep2
cor.test(sleep2$extra, sleep2$group)

# 피어슨 상관계수(Pearson correlation)는 두 연속형 변수 간의 선형 관계를 측정하는 데 적합합니다. 하지만 group은 본질적으로 범주형 변수(약물 1 vs 약물 2)이며, 이를 정수형(1, 2)으로 변환했다고 해서 연속형 변수가 되지 않습니다.

#25-0910 ----
# https://grok.com/c/6eac1ec0-bf08-4beb-9522-e990ca7ba7d4
# https://grok.com/chat/a11b32a9-30b0-4ce2-94c8-d5bbc79e59ef

tibble(
  num = c(1:10),
  A = c(90,40,90,40,90,40,90,40,90,40),
  B = c(10,60,10,60,10,60,10,60,10,60)) -> exp1_ttest

exp1_ttest |> 
  mutate()
?t.test()
sleep
t.test(exp1_ttest$A, exp1_ttest$B, paired = T)

# Two-sample 
t.test(1:10, y = c(7:20))      # P = .00001855
t.test(1:10, y = c(7:20, 200)) # P = .1245    -- NOT significant anymore


(tibble(
  num = c(1:10),
  A = c(90,10,90,10,90,10,90,10,90,10),
  B = c(40,60,40,60,40,60,40,60,40,60)) -> exp2_ttest)

t.test(exp2_ttest$A, exp2_ttest$B)

sleep
t.test(sleep$extra)
(sleep2 <- reshape(sleep, direction = "wide",
                   idvar = "ID", timevar = "group"))
t.test(sleep2$extra.1, sleep2$extra.2, paired = TRUE)


#
sleep |> 
  pivot_wider(names_from = group, 
              values_from = extra, 
              names_prefix = 'group_') |> 
  group_by(ID) |> 
  mutate_(mean_extra = mean())





penguins_1_table |> 
  pivot_longer(cols = !species,
               names_to = "type", 
               values_to = "value") |> 
  ggplot(aes(x = species, y = value)) +
  geom_bar(stat = 'identity') +
  facet_wrap(.~type, scales = 'free_y') +
  geom_label(aes(label = value))

penguins |> 
  filter(species %in% c('Adelie', 'Chinstrap')) -> penguins_Adelie_Chinstrap

penguins |> 
  filter(species %in% c('Gentoo', 'Chinstrap')) -> penguins_Gentoo_Chinstrap


Chinstrap vs Gentoo



















