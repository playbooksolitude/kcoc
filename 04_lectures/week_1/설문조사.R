#26-0504 wedn 12:46

#
source("./p_load.R", echo = T)
library(wordcloud2)
library(tidytext)
library("stopwords")
library(RColorBrewer)

#
# read_sheet("https://docs.google.com/spreadsheets/d/1SIBt-lRtQJbnAKtLHy8g4WS4S5sVKy5OdjhVKIwF2ro/edit?gid=1283738616#gid=1283738616", sheet = 'tidyset') -> kcoc_1sheet

# kcoc_1sheet |> 
#   write_csv("./04_lectures/week_1/99.참여자설문조사/survey.csv")

read_csv("./04_lectures/week_1/99.참여자설문조사/survey.csv") -> kcoc_1sheet


#
kcoc_1sheet |> 
  mutate(
    across(
      where(is.character), as.factor)
    ) -> kcoc_2_factor
  
#
# mpg |> 
#   mutate(
#     across(
#       where(is.character), as.factor)
#   )

#kcoc_2_factor |>   view()

# 1. 익명처리 (성명 제거, NO.도 필요시 제거)
(df_anon <- kcoc_2_factor %>%
  select(-성명, -NO.) %>% 
  mutate(ID = row_number()) %>% 
  select(ID, everything()))


(df_anon %>%
  mutate(
    직위 = case_when(
      str_detect(직위, "성과관리 AM|임팩트부문 대표") ~ "대표",
      str_detect(직위, "책임매니저") ~ "책임매니저",
      str_detect(직위, "실장") ~ "실장",
      TRUE ~ 직위
    )
) -> df_anon)



# 2-1. 현재 직무 경력 분포
ggplot(df_anon, aes(x = fct_reorder(`현재 직무 경력`, `현재 직무 경력`, .fun = length))) +
  geom_bar(fill = "#2E86C1") +
  coord_flip() +
  labs(title = "참여자 현재 직무 경력 분포",
       x = "경력 구간", y = "인원 수") +
  theme_minimal(base_size = 14)


# 2-2. 직위 분포
ggplot(df_anon, aes(x = fct_infreq(직위))) +
  geom_bar(fill = "#E67E22") +
#  coord_flip() +
  labs(title = "참여자 직위 분포", x = "직위", y = "인원") +
  theme_minimal()


# 2-3. 국제개발협력 경력 vs 현재 직무 경력 (교차표)
table(df_anon$`국제개발협력 경력`, df_anon$`현재 직무 경력`) |> 
  as.data.frame() |> 
  tibble() |> 
  ggplot(aes(x = Var1, y = Var2, fill = Freq)) +
  geom_tile(color = 'snow') +
  geom_text(aes(label = Freq), size = 7, color = 'snow') +
  theme_minimal() +
  labs(x = '국제개발협력 경력', 
       y = '현재 직무 경력') +
  theme(
    axis.text = element_text(size = 12)
  )
  

#3. 데이터 분석 수준
ggplot(df_anon, aes(x = fct_infreq(`데이터 분석 수준`))) +
  geom_bar(fill = "#27AE60") +
  coord_flip() +
  labs(title = "참여자 데이터 분석 수준",
       subtitle = "대부분 초급 수준",
       x = "", y = "인원 수") +
  theme_minimal(base_size = 13)


#4 도구 분리 (여러 개 쓰는 사람 많음)
tools_long <- df_anon %>%
  select(`활용 경험이 있는 분석 도구`) %>%
  mutate(tool = str_split(`활용 경험이 있는 분석 도구`, ",|/", simplify = FALSE)) %>%
  unnest(tool) %>%
  mutate(tool = str_trim(tool)) %>%
  filter(tool != "") %>%
  mutate(tool = str_to_lower(tool))

tool_count <- tools_long %>%
  count(tool, sort = TRUE)

ggplot(tool_count, aes(x = reorder(tool, n), y = n)) +
  geom_col(fill = "#8E44AD") +
  coord_flip() +
  labs(title = "참여자들이 익숙한 분석 도구",
       x = "도구", y = "언급 횟수") +
  theme_minimal()

#5 sdgs ----
sdg_count <- df_anon %>%
  count(`관심있는 SDGs 주제`, sort = TRUE)

ggplot(sdg_count, aes(x = reorder(`관심있는 SDGs 주제`, n), y = n)) +
  geom_col(fill = "#F39C12") +
  coord_flip() +
  labs(title = "관심 있는 SDGs 주제",
       x = "SDGs 주제", y = "인원") +
  theme_minimal(base_size = 12)


#6 참여 동기 텍스트 전처리
#불용어 
# 한국어 불용어 리스트 직접 정의 (실무에서 자주 사용하는 버전)
korean_stopwords <- tibble(word = c(
  "것", "수", "등", "및", "위해", "통해", "이번", "때", "후", "전", 
  "더", "많이", "하고", "합니다", "했습니다", "한다", "하는", "하여", 
  "될", "있습니다", "있어", "있고", "있음", "있게", "통한", "대한", 
  "으로", "에서", "에게", "와", "과", "를", "을", "의", "가", "이", 
  "은", "는", "들", "에", "도", "만", "까지", "부터", "해서", "하게",
  "해서", "되고", "되는", "되며", "되었습니다", "합니다", "하였습니다",
  "보고", "통해", "기반", "중", "현재", "모든", "각", "여러", "함",
  "있을", "있도록", "위한", "있는", "통한", "대한", "관련", "주요",
  '싶습니다'
))

# 다시 워드 크라우드 
motivation_text <- df_anon %>%
  select(text = `참여 동기`) %>%
  mutate(text = str_replace_all(text, "[[:punct:]]", " ")) %>% 
  mutate(text = str_squish(text)) %>%                    # 공백 정리
  unnest_tokens(word, text) %>%
  anti_join(korean_stopwords, by = "word") %>%          # 불용어 제거
  filter(nchar(word) >= 3 & nchar(word) <= 3) %>%     # ← 핵심: 2~5글자만
  filter(!str_detect(word, "^[0-9]+$"))                  # 숫자만 있는 단어 제거


# re 
motivation_text <- df_anon %>%
  select(text = `참여 동기`) %>%
  mutate(text = str_replace_all(text, "[[:punct:]]", " ")) %>%
  mutate(text = str_squish(text)) %>%
  unnest_tokens(word, text) %>%
  anti_join(korean_stopwords, by = "word") %>%
  filter(nchar(word) >= 2 & nchar(word) <= 4) %>%
  filter(!str_detect(word, "^[0-9]+$")) %>%
  # ===== 핵심: 끝자리 조사 제거 =====
filter(!str_detect(word, "[에의한를을가이은는도만부터까지하고해서]$")) %>%
  filter(!str_detect(word, "(.+)(한|함|함|적|적|성|화|화)$"))  # ~한, ~함, ~적, ~화 등


# RColorBrewer로 색상 팔레트 생성
colors <- brewer.pal(8, "Set1")

word_freq <- motivation_text %>%
  count(word, sort = TRUE) |> 
#  filter(n >= 2) %>%                    # 최소 3번 이상 등장
  slice_max(n, n = 25)                  # ← 상위 40개만 (필요시 30이나 25로 줄이기)

wordcloud2(word_freq, 
           size = 1.5, 
           color = "random-light", 
           #color = colors, 
           backgroundColor = "white",
           minSize = 5,
           rotateRatio = 0.3)


# 기대하는 점 워드 크라우드 ----
expect_text <- df_anon %>%
  select(text = `기대하는 점`) %>%
  mutate(text = str_replace_all(text, "[[:punct:]]", " ")) %>%
  mutate(text = str_squish(text)) %>%
  unnest_tokens(word, text) %>%
  anti_join(korean_stopwords, by = "word") %>%
  filter(nchar(word) >= 2 & nchar(word) <= 5) %>%
  filter(!str_detect(word, "^[0-9]+$")) %>%
  filter(!str_detect(word, "[에의한를을가이은는도만부터까지하고해서적성화함]$"))

word_freq_expect <- expect_text %>%
  count(word, sort = TRUE) %>%
  #filter(n >= 2) %>%
  slice_max(n, n = 25)

# 워드클라우드
colors <- brewer.pal(8, "Set1")

wordcloud2(word_freq_expect,
           size = 1.7,
           color = colors,
           backgroundColor = "white",
           minSize = 8,
           rotateRatio = 0.25,
           fontFamily = "NanumGothic")


# 직위 정리 (이전 코드 활용) ----
df_clean <- df_anon %>%
  mutate(
    직위_clean = case_when(
      str_detect(직위, "성과관리 AM|임팩트부문 대표") ~ "대표",
      TRUE ~ 직위
    )
  )

# 2-1. 직위별 관심 SDGs
df_clean %>%
  count(직위_clean, `관심있는 SDGs 주제`, sort = TRUE) %>%
  ggplot(aes(x = 직위_clean, y = n, fill = `관심있는 SDGs 주제`)) +
  geom_col(position = "dodge") +
  labs(title = "직위별 관심 SDGs 주제",
       x = "직위", y = "인원") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
  scale_fill_brewer(palette = 'Set1')




# 2-2. 직위별 데이터 분석 수준
df_clean %>%
  count(직위_clean, `데이터 분석 수준`) %>%
  ggplot(aes(x = `데이터 분석 수준`, y = 직위_clean, fill = n)) +
  geom_tile(color = 'snow') +
  labs(title = "직위별 데이터 분석 수준") +
  theme_minimal() +
  geom_text(aes(label = n), color ='snow', size = 7) +
  theme(
    legend.position = 'none'
  )

## 2-2.1 대안1 -----
sdg_by_position <- df_clean %>%
  count(직위_clean, `관심있는 SDGs 주제`) %>%
  arrange(직위_clean)

ggplot(sdg_by_position, aes(x = 직위_clean, 
                            y = `관심있는 SDGs 주제`, 
                            fill = n)) +
  geom_tile(color = "white", size = 0.8) +
  geom_text(aes(label = n), color = "white", fontface = "bold") +
  #scale_fill_gradient(low = "#E3F2FD", high = "#0D47A1") +
  labs(title = "직위별 관심 SDGs 주제",
       subtitle = "Heatmap 시각화 (색이 진할수록 관심 인원 많음)",
       x = "직위", y = "SDGs 주제") +
  theme_minimal(base_size = 13) +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))


ggplot(sdg_by_position, aes(x = `관심있는 SDGs 주제`, y = n)) +
  geom_segment(aes(xend = `관심있는 SDGs 주제`, yend = 0), color = "#2E86C1") +
  geom_point(size = 5, color = "#2E86C1") +
  facet_wrap(~ 직위_clean, scales = "free_y") +
  coord_flip() +
  labs(title = "직위별 관심 SDGs 주제 (Lollipop Chart)",
       y = "인원 수") +
  theme_minimal()


# 2-3 tidy 전처리
# df_clean %>%
#   count(직위_clean, `데이터 분석 수준`) |> 
#   view()

df_clean %>%
  count(직위_clean, `데이터 분석 수준`) |> 
  mutate(
  `데이터 분석 수준` = case_when(
    str_detect(`데이터 분석 수준`, '초급') ~ '초급',
    str_detect(`데이터 분석 수준`, '입문') ~ '입문',
    TRUE ~ '기타'
  )
  )-> df_clean_casewhen


df_clean_casewhen %>%
    #count(직위_clean, `데이터 분석 수준`) %>%
    ggplot(aes(x = `데이터 분석 수준`, y = 직위_clean, fill = n)) +
    geom_tile(color = 'snow') +
    labs(title = "직위별 데이터 분석 수준") +
    theme_minimal() +
    geom_text(aes(label = n), color ='snow', size = 7) +
    theme(
      legend.position = 'none'
    )

## 대안 ----
ggplot(df_clean, aes(x = 직위_clean
                     #fill = `데이터 분석 수준`
                     )
       ) +
  geom_bar(position = "dodge", color = "white") +
  geom_label(aes(label = after_stat(count)), 
            stat = "count", position = position_dodge(0.9), 
            #vjust = -0.5, 
            size = 7
            ) +
  labs(title = "직위별 데이터 분석 수준",
       x = "직위", y = "인원 수") +
  theme_minimal(base_size = 14) +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

level_by_pos <- df_clean %>%
  count(직위_clean, `데이터 분석 수준`)

ggplot(level_by_pos, aes(x = 직위_clean, 
                         y = n
                         #color = `데이터 분석 수준`
                         )
       ) +
  geom_segment(aes(xend = 직위_clean, yend = 0), size = 1.2) +
  geom_point(size = 6) +
  coord_flip() +
  labs(title = "직위별 데이터 분석 수준",
       subtitle = "대부분 초급 수준") +
  theme_minimal() +
  theme(
    axis.text = element_text(size = 14),
    axis.title = element_blank()
  )



# 2-3. 직위별 사용 도구 (복수 응답)
tools_by_position <- df_clean %>%
  select(직위_clean, tool = `활용 경험이 있는 분석 도구`) %>%
  mutate(tool = str_split(tool, ",|/", simplify = FALSE)) %>%
  unnest(tool) %>%
  mutate(tool = str_trim(str_to_lower(tool))) %>%
  filter(tool != "") %>%
  count(직위_clean, tool, sort = TRUE)


tools_by_position |> 
  ggplot(aes(
    x = 직위_clean, y = n, fill = tool
  )) +
  geom_bar(stat = 'identity') +
  theme(
    legend.position = 'top'
  ) +
  scale_fill_brewer(palette = 'Dark2')

#MetBrewer::display_all()
MetBrewer::display_all()

tools_by_position |> 
  ggplot(aes(
    x = 직위_clean, y = n, fill = tool
  )) +
  geom_bar(stat = 'identity') +
  theme(
    legend.position = 'top'
  ) +
  MetBrewer::scale_fill_met_d(name = 'VanGogh2')


ggplot(tools_by_position, aes(x = 직위_clean, y = n, fill = 직위_clean)) +
  geom_col() +
  facet_wrap(~ tool) +
#  coord_flip() +
  labs(title = "도구별 직위 분포") +
  theme_minimal() +
  scale_fill_brewer(palette = 'Set1') 



