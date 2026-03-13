# ==============================================================================
# 고정항목을 활용한 국가 간 경찰신뢰 측정 연구 (DIF 통제 및 비모수적 보정)
# ==============================================================================
# [분석 개요]
# 세계가치조사(WVS) 데이터를 활용하여 국가 간 설문 응답 성향 차이(차별문항작용, DIF)를
# 통제하고, 가족/이웃/타인 신뢰 항목을 고정항목(Anchoring Vignettes)으로 사용하여
# 경찰 신뢰도를 비모수적으로 보정 및 비교하는 연구 코드입니다.

# 참고: anchors 패키지는 CRAN에서 내려갔으므로 아래 코드로 수동 설치해야 합니다.
# install.packages("anchors_3.0-8.tar.gz", repos = NULL, type = "source")
# 또는 remotes::install_github("IQSS/anchors")

# 1. 패키지 로드 및 환경 설정 --------------------------------------------------
library(anchors)     # 고정항목(Anchoring Vignettes) 분석을 위한 핵심 패키지
library(tidyverse)   # 데이터 전처리 및 시각화를 위한 패키지 모음 (dplyr, ggplot2 등)
library(labelled)    # WVS 데이터의 변수 라벨(Label) 및 메타데이터를 다루기 위한 패키지
library(countrycode) # 국가 코드(숫자)를 표준화된 국가명(문자열)으로 변환
library(writexl)     # 분석 결과를 엑셀 파일(.xlsx)로 내보내기 위한 패키지
library(readxl)      # 외부 엑셀 파일(CPI 지수 등)을 불러오기 위한 패키지
library(boot)        # 신뢰구간 추정을 위한 비모수적 부트스트래핑 패키지

# 작업 디렉토리 설정 (공유 시 연구자들이 본인의 폴더 구조에 맞게 수정하도록 안내)
# setwd("본인의/작업/폴더/경로")

# 분석 과정에서 생성될 파일들을 저장할 하위 폴더들을 자동으로 생성합니다.
# showWarnings = FALSE 옵션은 폴더가 이미 존재해도 에러 메시지를 띄우지 않게 합니다.
dir.create("data", showWarnings = FALSE)
dir.create("figures", showWarnings = FALSE)
dir.create("result", showWarnings = FALSE)


# 2. 데이터 불러오기 및 전처리 -------------------------------------------------
# 2-1. 세계가치조사(WVS) 시계열 데이터(1981-2022) 로드
# 세계가치조사 데이터는 WVS에서 다운받으시기 바람

# df <- readRDS("E:/Study/WVS/WVS/WVS_Time_Series_1981-2022_rds_v5_0.rds")
# df <- readRDS("E:/Study/WVS/WVS/WVS_Time_Series_1981-2022_rds_v5_0.rds")
# names(df) <- tolower(names(df)) # 변수명을 소문자로 통일
# 
# # 메모리 최적화: 이후 분석에 필요한 핵심 변수 16개만 즉시 추출하여 덮어씌움
# df <- df %>%
#   dplyr::select(
#     s020,        # 조사 연도 (year)
#     s003,        # 국가 코드 (country)
#     
#     # 주요 정부기관 신뢰도
#     e069_06,     # 경찰 (Police)
#     e069_02,     # 군대 (Army)
#     e069_04,     # 언론 (Press)
#     e069_07,     # 국회 (Parliament)
#     
#     # 고정항목 (Anchoring Vignettes)
#     d001_b,      # 가족 (Family)
#     g007_18_b,   # 이웃 (Neighborhood)
#     g007_33_b,   # 아는 사람 (Acquaintance)
#     g007_34_b,   # 처음 만난 사람 (First time meet)
#     
#     # 통제 및 인구통계학적 변수
#     h001,        # 개인의 안정감 (Secure)
#     x001,        # 성별 (Sex)
#     x003,        # 연령 (Age)
#     x007,        # 결혼 상태 (Marital status)
#     x025a_01,    # 교육 수준 1 (ISCED)
#     x025         # 교육 수준 2
#   )
# save(df, file = "df2.Rdata" )

load(file = "df2.Rdata")
# 데이터 처리를 편리하게 하기 위해 모든 변수명을 소문자로 통일합니다.
names(df) <- tolower(names(df))

# 연도(s020) 변수를 숫자형으로 변환하고, 국가 코드(s003)를 영문 국가명으로 변환합니다.
df$year <- as.numeric(df$s020)
df$country <- countrycode(df$s003, origin = 'iso3n', destination ='country.name')

# countrycode 패키지가 인식하지 못하는 북아일랜드(코드 909)를 수동으로 입력합니다.
df$country[df$s003 == 909] <- "Northern Ireland"

# ------------------------------------------------------------------------------
# 2-2. 측정 항목 리코딩 (1-4 척도 역순산: 숫자가 클수록 신뢰도가 높도록 변경)
# ------------------------------------------------------------------------------
# 원본 설문은 1(매우 신뢰) ~ 4(전혀 신뢰 안 함)로 되어 있으므로, 
# 분석의 직관성을 높이기 위해 4(매우 신뢰) ~ 1(전혀 신뢰 안 함)로 점수를 뒤집어주는 함수입니다.
recode_trust <- function(x) {
  case_when(x == 1 ~ 4, x == 2 ~ 3, x == 3 ~ 2, x == 4 ~ 1, TRUE ~ NA_real_)
}

df <- df %>%
  mutate(
    # [주요 정부기관 신뢰도 (Target Variables)]
    self  = recode_trust(e069_06), # 경찰 (Police) - 본 연구의 핵심 타겟 변수
    self2 = recode_trust(e069_02), # 군대 (Army)
    self3 = recode_trust(e069_04), # 언론 (Press)
    self4 = recode_trust(e069_07), # 국회 (Parliament)
    
    # [고정항목 (Anchoring Vignettes) - 심리적 거리순으로 배열]
    # 이 항목들은 응답자 개인의 주관적 응답 기준(DIF)을 파악하는 데 사용됩니다.
    vign4 = recode_trust(d001_b),    # 가족 (가장 높은 신뢰/가까운 거리)
    vign3 = recode_trust(g007_18_b), # 이웃 
    vign2 = recode_trust(g007_33_b), # 아는 사람 
    vign1 = recode_trust(g007_34_b), # 처음 만난 사람 (가장 낮은 신뢰/먼 거리)
    
    # [통제변수]
    secure = recode_trust(h001)      # 개인의 안정감 (Security)
  )

# ------------------------------------------------------------------------------
# 2-3. 인구통계학적 변수 리코딩
# ------------------------------------------------------------------------------
# 성별, 결혼 상태, 교육 수준, 연령 등의 통제 변수들을 분석하기 편한 텍스트나 숫자 그룹으로 묶어줍니다.
df <- df %>%
  mutate(
    sex = case_when(x001 == 1 ~ "male", x001 == 2 ~ "female", TRUE ~ NA_character_),
    marital = case_when(
      x007 == 1 ~ "Married", x007 == 2 ~ "AsMarried", x007 == 3 ~ "Divorced",
      x007 == 4 ~ "Separated", x007 == 5 ~ "Widowed", x007 == 6 ~ "Single", TRUE ~ NA_character_
    ),
    # 교육 수준은 두 가지 다른 문항(x025a_01, x025)을 결합하여 Low/High로 이분화합니다.
    edu1 = case_when(x025a_01 %in% 1:5 ~ "Low", x025a_01 %in% 6:8 ~ "High", TRUE ~ NA_character_),
    edu2 = case_when(x025 %in% 1:6 ~ "Low", x025 %in% 7:8 ~ "High", TRUE ~ NA_character_),
    edu = ifelse(!is.na(edu1), edu1, edu2), # edu1이 있으면 쓰고 없으면 edu2 사용
    age = ifelse(x003 < 13, NA, x003) # 13세 미만 비정상값 결측 처리
  )

# 분석에 필요한 변수만 골라내어 'master' 데이터셋으로 저장합니다. (용량 최적화)
master <- df %>%
  select(country, year, self, self2, self3, self4, vign1, vign2, vign3, vign4, sex, age, edu, marital, secure)
saveRDS(master, file = "data/master.rds")

# 각 국가별로 가장 최근에 조사된 연도의 데이터만 필터링하여 횡단면 비교(Cross-sectional) 데이터셋을 구축합니다.
df_recent <- master %>%
  na.omit() %>% # 분석에 사용할 모든 변수의 결측치 제거
  group_by(country) %>%
  filter(year == max(year, na.rm = TRUE)) %>% # 국가별 최대(최신) 연도만 남김
  ungroup()


# 3. 기술통계 및 밀도 분포 시각화 ----------------------------------------------
# 국가별로 고정항목 및 주요 기관 신뢰도의 평균(m)과 표준편차(s)를 계산합니다.
dfmg <- df_recent %>%
  group_by(country, year) %>%
  summarise(across(c(self, self2, self3, self4, vign1, vign2, vign3, vign4), 
                   list(m = ~mean(., na.rm = TRUE), s = ~sd(., na.rm = TRUE))),
            count = n(), .groups = "drop")

# Figure: 밀도 곡선 시각화 (국가 간 신뢰도 분포가 어떻게 다른지 확인)
# 고화질 TIFF 포맷으로 저장 (논문 투고용)
tiff("figures/density_plot.tiff", width = 10, height = 5, units = 'in', res = 300, compression = "lzw")
par(mfrow = c(1, 2)) # 1행 2열로 그래프 2개 배치

# a. 고정항목 밀도곡선 (가족, 이웃, 타인 등 척도 기준점들의 국가 간 분포)
plot(density(dfmg$vign1_m, na.rm = TRUE), main = "a. 고정항목과 경찰신뢰 항목 평균의 밀도곡선", 
     xlab = "평균", xlim = c(1, 4), ylim = c(0, 4), col = "red", lty = 2, lwd = 2)
lines(density(dfmg$vign3_m, na.rm = TRUE), col = "grey", lty = 3, lwd = 2) # 이웃
lines(density(dfmg$vign4_m, na.rm = TRUE), col = "blue", lty = 4, lwd = 2) # 가족
lines(density(dfmg$self_m, na.rm = TRUE), col = "black", lty = 1, lwd = 2) # 경찰
legend("topleft", legend = c("처음 만난 사람", "이웃", "가족", "경찰"), 
       col = c("red", "grey", "blue", "black"), lty = c(2, 3, 4, 1), lwd = 2, inset = 0.05)

# b. 정부기관별 신뢰 밀도곡선 (군대, 언론, 국회 등 기관 간 분포 비교)
plot(density(dfmg$self2_m, na.rm = TRUE), main = "b. 정부기관별 신뢰 평균의 밀도곡선", 
     xlab = "평균", xlim = c(1, 4), ylim = c(0, 4), col = "red", lty = 2, lwd = 2)
lines(density(dfmg$self3_m, na.rm = TRUE), col = "grey", lty = 3, lwd = 2) # 언론
lines(density(dfmg$self4_m, na.rm = TRUE), col = "blue", lty = 4, lwd = 2) # 국회
lines(density(dfmg$self_m, na.rm = TRUE), col = "black", lty = 1, lwd = 2) # 경찰
legend("topleft", legend = c("군대", "언론", "국회", "경찰"), 
       col = c("red", "grey", "blue", "black"), lty = c(2, 3, 4, 1), lwd = 2, inset = 0.05)
dev.off()


# 4. 차별문항작용(DIF) 통제: 고정항목 활용 비모수 척도 보정 (Anchors) -----------
# anchors 패키지를 사용하여 개인별 응답을 고정항목(vign1,3,4) 기준으로 재조정합니다.
# - Method "C": 순위 기반의 가장 기본적인 척도
# - Method "B": 구간 범위가 겹칠 때 동점 처리를 개선한 척도
a1c <- anchors(self ~ vign1 + vign3 + vign4, df_recent, method="C")
a1b <- anchors(self ~ vign1 + vign3 + vign4, df_recent, method="B")

# 계산된 척도 값을 원본 데이터에 새로운 변수(Ce, Cs, Be, Bs)로 삽입합니다.
df_anchors <- insert(df_recent, a1c)
df_anchors <- insert(df_anchors, a1b)

# 구간의 상한선(e)과 하한선(s)의 중간값을 취하여 단일 최종 척도로 변환합니다.
df_anchors$C_minent <- (df_anchors$Ce + df_anchors$Cs) / 2
df_anchors$B_minent <- (df_anchors$Be + df_anchors$Bs) / 2

# 국가별로 보정 전(selfmean)과 보정 후(Cmean, Bmean)의 신뢰도 평균 및 순위를 도출합니다.
df_rank <- df_anchors %>%
  group_by(country, year) %>%
  summarise(
    selfmean = mean(self, na.rm = TRUE),
    Bmean = mean(B_minent, na.rm = TRUE),
    Cmean = mean(C_minent, na.rm = TRUE),
    count = n(), .groups = "drop"
  ) %>%
  mutate(
    srank = rank(-selfmean), # 원점수 기준 순위
    brank = rank(-Bmean),    # B-scale 기준 순위
    crank = rank(-Cmean),    # C-scale 기준 순위
    dif = abs(srank - crank),# 보정 전후 순위 차이 절대값
    difrank = rank(-dif)     # 순위 변동폭의 순위 (변동이 클수록 상위)
  )

# 순위표를 엑셀로 저장합니다.
write_xlsx(df_rank, path = "result/calibrated_trust_ranks.xlsx")


# [외부 타당도 검증] 
# 보정된 경찰 신뢰도 지표가 국가 수준의 국제 지표(부패인식지수, CPI)와 
# 얼마나 상관성을 갖는지 검증하여 보정 방법론의 타당성을 입증합니다.
cpi <- read_xlsx("E:/Study/WVS/CorruptionIndex/dfCPI.xlsx") %>% rename_all(tolower)
df_rank <- left_join(df_rank, cpi, by = c("country", "year"))

# 스피어만 순위 상관분석 수행
cor.test(df_rank$selfmean, df_rank$cpi, method = "spearman", use = "complete.obs") # 보정 전
cor.test(df_rank$Cmean, df_rank$cpi, method = "spearman", use = "complete.obs")    # 보정 후


# 5. 케이스 스터디 (보정 전후 순위 역전 및 국가 간 응답 성향 비교) ------------
# 두 국가의 원점수 비율과 Anchoring 스케일 비율을 시각적으로 비교하는 커스텀 함수입니다.
plot_country_comparison <- function(data, countries, file_name) {
  
  # 1. 시각화에 필요한 핵심 변수(경찰신뢰 및 고정항목 3개)만 추출한 후 결측치 제거
  df_sub <- data %>%
    filter(country %in% countries) %>%
    group_by(country) %>%
    filter(year == max(year, na.rm = TRUE)) %>% 
    ungroup() %>%
    select(country, self, vign1, vign3, vign4) %>%
    na.omit()
  
  # 2. 데이터 유효성 검사 (비교할 국가의 데이터가 온전한지 확인)
  available_countries <- unique(df_sub$country)
  if(length(available_countries) < 2) {
    stop(paste("에러: 다음 국가의 유효한 데이터가 부족합니다 ->", 
               paste(setdiff(countries, available_countries), collapse=", ")))
  }
  
  # 3. 국가명은 factor로 처리하여 데이터가 한쪽 국가에 몰리거나 없어도 
  # 테이블 행(Row) 개수가 무조건 2개로 고정되도록 안전장치를 겁니다.
  df_sub$country <- factor(df_sub$country, levels = countries)
  # 참고: self, vign1,3,4 변수들은 뒷단의 anchors() 함수 작동을 위해 반드시 숫자형(numeric)이어야 합니다.
  
  new_names <- c("Not at all", "Not very much", "Somewhat", "Completely")
  
  # TIFF 파일 설정 및 레이아웃 분할 (2행 3열 그래프 그리드)
  tiff(file_name, width = 10, height = 6, units = "in", res = 300, compression = "lzw")
  par(mfrow = c(2, 3), mar = c(5, 4, 1, 2))
  
  vars <- c("self", "vign1", "vign3", "vign4")
  titles <- c("a. Trust in the Police", "b. Trust in strangers", "c. Trust in neighborhood", "d. Trust in Family")
  
  # 4. 원점수 빈도 그래프 생성 (반복문을 활용해 코드 축약)
  for(i in 1:4) {
    # ★ 핵심 로직: 빈 응답 범주(0명) 때문에 에러가 발생하지 않도록,
    # table() 안에서만 임시로 factor(..., levels=1:4)를 적용하여 1~4번 응답열이 무조건 유지되게 합니다.
    tab <- prop.table(table(df_sub$country, factor(df_sub[[vars[i]]], levels = 1:4)), margin = 1)
    
    # 막대그래프 출력
    bp <- barplot(tab, beside = TRUE, space = c(0.3, 4), col = c("black", "grey"),
                  names.arg = new_names, cex.names = 0.8, xlab = titles[i], ylab = "Proportion", ylim = c(0, 0.9))
    
    # 각 막대 위에 세로로 국가명(레이블) 추가
    text(x = bp[1, 1] + 0.2, y = tab[1, 1] + 0.1, labels = rownames(tab)[1], cex = 0.9, srt = 90, adj = 0, pos = 3, xpd = TRUE)
    text(x = bp[2, 1] + 0.2, y = tab[2, 1] + 0.1, labels = rownames(tab)[2], cex = 0.9, srt = 90, adj = 0, pos = 3, xpd = TRUE)
  }
  
  # 5. Anchors Plot (C scale, B scale) 추가
  # df_sub의 값들이 숫자형을 유지하고 있으므로 anchors()가 정상적으로 척도 구간을 계산합니다.
  sub_c <- anchors(self ~ vign1 + vign3 + vign4, df_sub, method = "C")
  sub_b <- anchors(self ~ vign1 + vign3 + vign4, df_sub, method = "B")
  
  barplot(sub_c, ties = "uniform", ylim = c(0, 0.5), main = "C scale: Trust in Police", xlab = "e. C scale")
  barplot(sub_b, ties = "uniform", ylim = c(0, 0.5), main = "B scale: Trust in Police", xlab = "f. B scale")
  
  dev.off() # 저장 종료
}

# 사례 1: 에콰도르 vs 불가리아 (DIF 통제 후 경찰 신뢰도 순위가 역전되는 현상 시각화)
plot_country_comparison(master, c("Ecuador", "Bulgaria"), "figures/Analy_Ecuador_Bulgaria2.tiff")

# 사례 2: 네덜란드 vs 그리스 (응답 스케일의 극단값/중간값 쏠림 차이를 보정하는 양상)
plot_country_comparison(df_recent, c("Netherlands", "Greece"), "figures/Analy_Netherlands_Greece.tiff")


# 6. 비모수적 부트스트랩을 통한 신뢰구간 추정 (Confidence Intervals) -------------
# 일반적인 모수적 통계(예: 정규분포 가정) 대신, 표본을 무작위로 복원 추출(Bootstrap)하여
# 신뢰도 평균의 상하한 신뢰구간(95%)을 직접 구하는 과정입니다.

# 부트스트랩 내부에서 반복 수행될 통계량 추출 함수를 정의합니다.
anchors_boot_func <- function(d, i) {
  # i는 부트스트랩이 생성한 랜덤 인덱스입니다. 이 인덱스대로 표본을 재구성합니다.
  d_sub <- d[i, ] %>% select(self, vign1, vign3, vign4) %>% filter(complete.cases(.))
  
  result <- tryCatch({
    # 1. 원점수(Self) 평균 추출
    s_val <- mean(d_sub$self, na.rm = TRUE)
    
    # 2. C-scale 보정 후 평균 추출
    z_c <- anchors(self ~ vign1 + vign3 + vign4, d_sub, method = "C")
    res_c <- insert(d_sub, z_c)
    c_val <- mean((res_c$Ce + res_c$Cs) / 2, na.rm = TRUE)
    
    # 3. B-scale 보정 후 평균 추출
    z_b <- anchors(self ~ vign1 + vign3 + vign4, d_sub, method = "B")
    res_b <- insert(d_sub, z_b)
    b_val <- mean((res_b$Be + res_b$Bs) / 2, na.rm = TRUE)
    
    # 3개의 계산된 평균값을 벡터로 반환합니다.
    return(c(c_val, b_val, s_val))
    
  }, error = function(e) { 
    # 특정 부트스트랩 샘플에서 수렴하지 않거나 에러가 날 경우 NA 반환
    return(c(NA, NA, NA)) 
  })
  
  return(result)
}

# 재현성을 위한 시드(Seed) 고정
set.seed(123)
countries <- unique(df_recent$country)
final_ci_results <- data.frame()





# 각 국가별로 루프를 돌며 부트스트래핑 수행
# (주의: 전체 국가 대상 실행 시 시간이 꽤 오래 걸릴 수 있습니다.)
for (cntry in countries) {
  target_data <- df_recent %>% filter(country == cntry)
  
  # 표본 수가 30개 미만인 국가는 통계적 의미가 약하므로 건너뜁니다.
  if(nrow(target_data) < 30) next 
  
  # boot 패키지를 사용하여 표본 재추출을 R번 반복합니다.
  # 현재 코드 테스트를 위해 R=200으로 설정되어 있으나, 
  # 실제 논문 출판 수준의 안정성을 위해서는 R=1000 이상을 권장합니다.
  b_out <- boot::boot(data = target_data, statistic = anchors_boot_func, R = 200) 
  
  # 백분위수(percentile) 기반 95% 신뢰구간 추출 (type="perc")
  ci_c <- boot::boot.ci(b_out, type = "perc", index = 1)
  ci_b <- boot::boot.ci(b_out, type = "perc", index = 2)
  ci_s <- boot::boot.ci(b_out, type = "perc", index = 3)
  
  # 결과 데이터프레임 조립 (boot 평균, 하한선(Low), 상한선(High))
  row_res <- data.frame(
    country = cntry,
    Smean_boot = b_out$t0[3], Smean_low = ci_s$percent[4], Smean_high = ci_s$percent[5],
    Cmean_boot = b_out$t0[1], Cmean_low = ci_c$percent[4], Cmean_high = ci_c$percent[5],
    Bmean_boot = b_out$t0[2], Bmean_low = ci_b$percent[4], Bmean_high = ci_b$percent[5]
  )
  final_ci_results <- rbind(final_ci_results, row_res)
  print(paste(cntry, "분석 완료"))
}

# 위에서 산출한 부트스트랩 신뢰구간 결과와 앞서 구한 순위 데이터(df_rank)를 국가명 기준으로 병합합니다.
df_final_with_ci <- left_join(df_rank, final_ci_results, by = "country")

# 최종 완성된 분석 결과표를 저장합니다.
saveRDS(df_final_with_ci, file = "result/final_ci_results.RDS")
write_xlsx(df_final_with_ci, path = "result/final_ci_results.xlsx")