#26-0301 sun

# 설치 (최초 1회)
# install.packages("RKaggle")

# 로드
library(RKaggle)

# 예시 1 - 데이터셋 전체를 다운로드하고 자동으로 읽기
gapminder_data <- get_dataset("domenicoddmasini/gapminder-dataset-for-data-viz")

#
gapminder_data
