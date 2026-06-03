#26-0523 sat 09:47

#
library(tidyverse)
library(googlesheets4)

# 1 불러오기 ----
## 1.1 sheet ----
#gs4_auth(cache = ".secrets", email = TRUE)

koica_evalrep_1_sheet <- read_sheet("https://docs.google.com/spreadsheets/d/1FdBaFNHuJs80EdOVZcEJe5sK15vZqxnQWwVc0fazENs/edit?gid=8927854#gid=8927854")

#
# koica_clean <- koica_evalrep_1_sheet |> 
#   mutate(
#     # 기초선 열 변환
#     기초선 = map_chr(기초선, \(x) if (is.null(x)) NA_character_ else as.character(x)),
#     # 목표치 열 변환
#     목표치 = map_chr(목표치, \(x) if (is.null(x)) NA_character_ else as.character(x))
#   )