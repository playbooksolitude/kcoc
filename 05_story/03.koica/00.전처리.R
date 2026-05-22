#26-0304 wedn 

#https://www.oda.go.kr/opo/nnstat/opoNstatInfoOthBcOecdList.do
#https://gemini.google.com/app/8b2f1da146166e84
#https://gemini.google.com/app/e4f1a4df03d98820

source("./p_load.R",echo = T)
#install.packages("highcharter")
library('highcharter')
library(ggfx)

# quiz ----
# https://gemini.google.com/app/c32631815c184e03
#oecd2023_1sheet |> write_csv("./01.kcoc/05.files/oecd2023.csv")
#
# read_sheet("https://docs.google.com/spreadsheets/d/1L9w8gEojZSl5pHVc8-Pl--kvIKqM1OD4IDwsSnDrZME/edit") -> oecd2023_1sheet

#1 csv ----
read_csv("./01.kcoc/05.files/oecd2023.csv") -> oecd2023_1sheet
read_csv("./01.kcoc/05.files/oecd2023_5select.csv") -> oecd2023_5select

#oecd2023_1sheet |> view()

# 2 NA 컬럼 제외 select ----
### NA 컬럼만 조회 ----
oecd2023_1sheet |>
  select(
    where(~all(is.na(.)))
  )
# 
# oecd2023_1sheet |> 
#   select(
#     where(~all(is.na(.)))
#   )
# oecd2023_1sheet |> #colnames()
#   select(
#     where(~ !all(is.na(.)))
#   ) -> oecd2023_2notNA

oecd2023_1sheet -> oecd2023_2notNA
oecd2023_2notNA |> colnames()


#3 특수문자 치환 ----
## 3-1 특수문자 ----
(oecd2023_2notNA |>
   # 열 이름에서 \n을 공백으로 변경
   rename_with(~ str_replace_all(., "\n", "")) |> 
   rename_with(~ str_replace_all(., "\\(", "_")) |>
   rename_with(~ str_replace_all(., "/", "_")) |> 
   rename_with(~ str_replace_all(., "\\)", "")) |>
   rename_with(~ str_replace_all(., "\\[", "_")) |> 
   rename_with(~ str_replace_all(., "\\]", "")) |> #colnames()
   rename_with(~ str_replace_all(., " ", "_")) |> #colnames()
   mutate(
     `사업개시_예정일` = ymd(`사업개시_예정일`),
     `사업완공_예정일` = ymd(`사업완공_예정일`)
   ) -> oecd2023_3date)



#4 수여국 대륙 분리 ----
## separate_wider_delim ----
oecd2023_3date |>
  separate_wider_delim(cols = 대륙명, delim = " > ",
                       names_sep = '_',
                       too_few = "align_start") ->  #|> view()
  oecd2023_4sep

# #5 상세분석 ----
# oecd2023_4sep |> colnames()
# oecd2023_5select |> view()
# oecd2023_5select |> sapply(n_distinct)
# 
# 
# #
# oecd2023_4sep |> 
#   sapply(n_distinct) |> 
#   data.frame()


# ## 5-1 사업분야, 지출액----
# oecd2023_4sep |> 
#   select(사업번호, contains('대륙명'), 수원국, 사업실시기관명, 
#          사업구분, `양자/다자간 구분`, 원조유형, `사업명(한글)` ,
#          사업분야, 사업설명, 
#          SDGs, 사업설명, 성평등, `환경 지원`, 재난위험경감, 
#          `사업개시(예정)일`, `사업완공(예정)일`, 
#          `지출액[백만달러]`, 
#          `순지출액[백만달러]`, 약정일) -> oecd2023_5select #|> view()

(oecd2023_4sep -> oecd2023_5select)
#oecd2023_5select |> write_csv("./01.kcoc/05.files/oecd2023_5select.csv")
