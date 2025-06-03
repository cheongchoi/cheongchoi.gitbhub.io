## 필요한 패키지 로드
library("sf")
library("tidyverse")

## 지도만 현출하고자 할때,
# 1. 전국 시군구 
## SHP 파일 읽기
sido = read_sf('sig.shp')
print(sido)
sido <- sido %>%
  st_set_crs(5179)


## 한글인코딩 문제 해결
sido$SIG_KOR_NM <- iconv(sido$SIG_KOR_NM, from='CP949', to="UTF-8",
                         sub=NA, mark=TRUE, toRaw=FALSE)

## 서울만 
seoul <- sido %>%
  filter(str_detect(SIG_CD, "^11"))
head(seoul)


## 전국 시군구 지도 그리기: 
sido %>%
  ggplot() + geom_sf()+
  theme_void()


sido %>%
  ggplot() + 
  geom_sf(aes(fill = SIG_CD), show.legend = FALSE) +
  ggtitle("전국 시군구") +
  theme_void()


### 서울지도
seoul <- sido %>%
  filter(str_detect(SIG_CD, "^11"))
head(seoul)

seoul$SIG_KOR_NM <- iconv(seoul$SIG_KOR_NM, from='CP949', to="UTF-8",
                              sub=NA, mark=TRUE, toRaw=FALSE)

seoul %>%
  ggplot() + 
  geom_sf(aes(fill = SIG_CD), show.legend = FALSE) +
  ggtitle("Seoul Map") +
  theme_void()

### 부산, 울산, 경남 지도
puk <- sido %>%
  filter(str_detect(SIG_CD, "^26") |
           str_detect(SIG_CD, "^31") |
           str_detect(SIG_CD, "^48"))


puk %>%
  ggplot() + 
  geom_sf(aes(fill = SIG_CD), show.legend = FALSE) +
  ggtitle("부산/울산/경남 지도") +
  theme_void()


# stat_seoultxt 파일
## https://data.seoul.go.kr/dataList/1391/S/2/datasetView.do 다운로드

library("readxl")
df_stat = read_xlsx("stat_seoul.xlsx", range="B7:D31", col_names = FALSE)
names <- c("SIG_CD", "count", "size")
colnames(df_stat) <- names

## 시군구 대표 경도위도
df_lonlat = read_xlsx("long_lat.xlsx", range="B1:E296")
df_lonlat_seoul = df_lonlat %>%
                  filter(do == "서울")

str(df_lonlat_seoul)

seoul.sf <- 
  st_as_sf(df_lonlat_seoul, coords = c("longitude", "latitude"), remove = FALSE, crs = 4326)
st_crs(seoul.sf)$epsg

seoul_ll <- df_lonlat_seoul %>%
  st_as_sf(coords = c("longitude", "latitude"), crs = 4326)

## 서울 지역의 행정구역 데이터와 서울 통계 병합
seoul_master = left_join(x = seoul, y=df_stat,
                         by = c("SIG_KOR_NM" = "SIG_CD"))
head(seoul_master)



seoul_master %>%
  ggplot() + 
  geom_sf(aes(fill = count), show.legend = TRUE) +
  scale_fill_gradient(low = "#FFFFFF", high = "#0000FF")  +
  ggtitle("서울시군구별 문화시설 개소") +
  theme_void()

## 최종 지도

seoul_master %>%
  ggplot() + 
  geom_sf(aes(fill = count), show.legend = TRUE) +
  scale_fill_gradient(low = "#FFFFFF", high = "#0000FF")  + 
  geom_sf_text(data=seoul_ll, aes(label=city), size= 2.5)  +
  ggtitle("서울시군구별 문화시설 개소") +
  theme_void()


