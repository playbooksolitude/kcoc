library(leaflet)
library(tidygeocoder)


##
author: "최요한"
date: "`r Sys.Date()`"
format: 
  html:
  toc: true
toc-depth: 2           # 번호 깊이
toc-float: true        # 스크롤을 내려도 목차가 따라옵니다.
number-depth: 3
number-sections: true   # 자동 번호
code-fold: false         # 
code-summary: "소스 코드 확인하기"
smooth-scroll: true  # 클릭 시 부드럽게 이동
code-copy: true        # 코드 복사 버튼 활성화
# theme: Zephyr
# theme: Flatly
#theme:
#  light: [flatly, cosmo]  #본문 flatly, #내비게이션 등 cosmo 적용
#  dark: darkly
# css: styles.css
embed-resources: true
editor: visual


##

leaflet() |> 
  setView(174.764, -36.877, zoom = 16) |> 
  addTiles() |> 
  addMarkers(174.764, -36.877, "Maungawhau")

library(leaflet)

# 서울시청 좌표: 경도(126.9784), 위도(37.5665)
leaflet() |> 
  setView(lng = 126.9784, lat = 37.5665, zoom = 20) |> 
  addTiles() |> 
  addMarkers(lng = 126.9784, lat = 37.5665, popup = "서울시청")


leaflet() |> 
  setView(126.9784, 37.5665, zoom = 14) |> 
  # 국토교통부 브이월드 기본 지도 타일 쓰기
  addTiles("http://xdworld.vworld.kr:8080/2d/Base/service/{z}/{x}/{y}.png", 
           attribution = "Vworld") |> 
  addMarkers(126.9784, 37.5665, popup = "서울시청")

# 주소로 찾ㅣ
df_addresses <- data.frame(
  name = c("서울시청", "부산역", "제주 성산일출봉"),
  address = c("서울특별시 중구 세종대로 110", 
              "부산광역시 동구 중앙대로 206", 
              "제주특별자치도 서귀포시 성산읍 성산리 1")
)

#
# 2. 지오코딩 수행 (오픈스트리트맵 기반 'osm' 서비스 이용)
df_coords <- df_addresses |>
  geocode(address = address, 
          method = 'osm', 
          lat = latitude, 
          long = longitude)

# 3. 결과 확인 (경도, 위도 컬럼이 자동으로 추가됩니다)
print(df_coords)

# 4. 변환된 좌표로 바로 leaflet 지도 그리기
leaflet(df_coords) |>
  setView(lng = 127.5, lat = 36.5, zoom = 7) |> # 대한민국 중심부
  addTiles() |>
  addMarkers(lng = ~longitude, lat = ~latitude, popup = ~name)
