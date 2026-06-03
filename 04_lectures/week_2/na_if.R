
#26-0523 sat 10:18


library(dplyr)

# 엑셀에서 "NULL"이라는 문자열로 들어온 가상의 데이터 프레임
(df <- tibble(
  x = c(1, 2, 3),
  y = c("a", "NULL", "b"),  # 여기에 "NULL" 글자가 있습니다.
  z = c("NULL", "M", "K")
))

# y와 z 컬럼의 "NULL"을 진짜 NA로 바꾸기
df_clean <- df %>% 
  mutate(
    y = na_if(y, "NULL"),
    z = na_if(z, "NULL")
  )

print(df_clean)
