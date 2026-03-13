# ==============================================================================
# Conjoint Analysis Script
# ==============================================================================

# 1. 환경 설정 및 패키지 로드 --------------------------------------------------
# 분석에 필요한 다양한 R 패키지들을 불러옵니다.
library(conjoint)      # 전통적인 컨조인트 분석 및 직교설계(Orthogonal Design) 지원
library(skpr)          # 실험 설계 평가 및 생성
library(AlgDesign)     # 알고리즘 기반 실험 설계 (D-optimal 등)
library(mlogit)        # 다항 로짓 모형(Multinomial Logit Model) 추정을 위한 핵심 패키지
library(texreg)        # 회귀분석 결과를 논문용 표 형식(HTML, LaTeX, Word 등)으로 변환
library(dotwhisker)    # 회귀계수(부분가치 효용)와 신뢰구간을 Forest Plot(점과 수염) 형태로 시각화
library(ggplot2)       # 데이터 시각화
library(jtools)        # 회귀분석 결과 시각화 및 요약 보조 패키지
library(grid)          # 격자(Grid) 기반 그래픽 시스템 (표 그리기 용도)
library(gridExtra)     # 여러 개의 그리드 그래픽을 배열하기 위한 패키지
library(dplyr)         # 데이터 전처리 및 조작 (파이프 연산자 %>% 포함)
library(ExpertChoice)  # 이산 선택 실험(DCE) 설계를 돕는 패키지
library(DoE.base)      # 실험계획법(Design of Experiments) 기본 패키지
library(readxl)        # 엑셀 파일(.xlsx)을 불러오기 위한 패키지
library(MASS)          # 다변량 정규분포 난수 생성 등 다양한 통계 함수 지원


# 2. 실험 설계 (Expert Choice & DoE) -------------------------------------------
# 연구에서 측정하고자 하는 3가지 속성(연장기간, 급여수준, 근로방식)과 각각의 3개 수준(Levels)을 정의합니다.
kim <- list(
  length = c("1", "2", "3"),
  wage   = c("1", "2", "3"),
  work   = c("1", "2", "3")
)

# 가능한 모든 조합(3x3x3 = 27개)을 생성하는 Full factorial 설계를 만듭니다.
kim_examp <- full_factorial(kim)
akim_examp <- augment_levels(kim_examp)
# 각 속성별 수준의 개수를 계산합니다. (여기서는 모두 3개)
nlevels <- unlist(purrr::map(kim_examp, function(x) length(levels(x))))

# DoE 패키지를 활용하여 주효과를 추정할 수 있는 최소 규모의 직교배열(Fractional Factorial Design)을 추출합니다.
fractional_factorial_kim <- oa.design(nlevels = nlevels, columns = "min34")
colnames(fractional_factorial_kim) <- colnames(kim_examp)
fractional_factorial_kim <- search_design(kim_examp, fractional_factorial_kim)

# 추출된 일부실시법(Fractional Factorial) 설계가 주효과를 추정하기에 통계적으로 효율적인지(A-efficiency, D-efficiency) 확인합니다.
row1_main_effects <- fractional_factorial_efficiency(~ length + wage + work, fractional_factorial_kim)

# Modulo method를 이용하여 응답자에게 제시할 대안들의 묶음(Choice set)을 생성합니다.
# 베이스라인 대안을 기준으로 다른 대안들을 체계적으로 이동(shifting)시켜 선택압을 높입니다.
dce_modulo_examp1 <- modulo_method(fractional_factorial_kim, list(c(1, 0, 1), c(0, 1, 0)))
dce_modulo_examp2 <- modulo_method(fractional_factorial_kim, list(c(1, 0, 1), c(1, 2, 1), c(0, 2, 0)))

# 두 가지 Modulo 옵션의 효율성을 비교하여 더 나은 설계를 선택하기 위한 과정입니다.
dce_efficency_menu_example1 <- dce_efficiency(akim_examp, dce_modulo_examp1)
dce_efficency_menu_example2 <- dce_efficiency(akim_examp, dce_modulo_examp2)

# 최종적으로 선택된 설계(examp1)를 바탕으로 실제 설문지용 질문 프레임을 생성합니다.
kim_question_table <- construct_question_frame(akim_examp, dce_modulo_examp1)

# 숫자(1, 2, 3)로 표기된 수준들을 응답자가 이해할 수 있는 실제 텍스트 라벨로 변환합니다.
levels(kim_question_table$length) <- c("1년 연장", "2년 연장", "3년 연장")
levels(kim_question_table$wage)   <- c("60%급여", "60%급여", "50%급여") # (주의: 수준 텍스트가 중복인지 확인 필요)
levels(kim_question_table$work)   <- c("근로시간단축", "편한 직무로 보직전환", "계약제로 재고용")

# 완성된 질문지 프레임을 엑셀에서 열어볼 수 있도록 CSV(cp949 인코딩)로 저장합니다.
write.csv(kim_question_table, "kim_question_table.csv", row.names = TRUE, fileEncoding = 'cp949')


# 3. 대안 생성 및 카드 시각화 (Conjoint) ---------------------------------------
# (2장에서 만든 설계와 별개로, conjoint 패키지를 활용하여 시각화용 카드를 생성하는 파트입니다.)
experiment <- expand.grid(
  length = c("A", "B", "C"),
  wage   = c("A", "B", "C"),
  work   = c("A", "B", "C")
)

# conjoint 패키지의 caFactorialDesign을 이용해 속성 간 상관관계가 0이 되는 직교설계를 생성합니다.
design1 <- caFactorialDesign(data = experiment, type = "orthogonal")
design1 <- data.frame(lapply(design1, as.numeric)) # 숫자형 데이터로 변환

# 대안 shifting 기법: 첫 번째 대안(design1)을 바탕으로 두 번째(design2), 세 번째(design3) 대안을 생성합니다.
# 1->2, 2->3, 3->1 로 수준을 순환시켜 한 선택지 내에서 속성이 중복되지 않도록 만듭니다.
design2 <- design1
design2[design1 == 1] <- 2
design2[design1 == 2] <- 3
design2[design1 == 3] <- 1

design3 <- design2
design3[design2 == 1] <- 2
design3[design2 == 2] <- 3
design3[design2 == 3] <- 1

fulldesign <- cbind(design1, design2, design3)

# mlogit 분석의 구조에 맞추기 위해 넓은 형태(Wide)의 데이터를 긴 형태(Long format)로 변환합니다.
# No-choice(아무것도 선택하지 않음) 옵션을 위해 'stimulus = 4'인 행(0,0,0)을 추가합니다.
fulldesign_long <- data.frame()
for (i in 1:nrow(design1)) {
  tmp <- data.frame(
    choice = i, 
    stimulus = 1:4, 
    rbind(design1[i, ], design2[i, ], design3[i, ], rep(0, 3)) # 변수 3개(length, wage, work)
  )
  fulldesign_long <- rbind(fulldesign_long, tmp)
}

# 응답자에게 제시할 Choice Set(선택 카드) 이미지를 반복문을 통해 PNG 파일로 저장합니다.
for (i in 1:nrow(design1)) {
  attr <- c("연장기간", "급여수준", "근로방식")
  choice_tab <- data.frame(cbind(
    attr,
    t(design1[i, ]),
    t(design2[i, ]),
    t(design3[i, ])
  )) 
  
  colnames(choice_tab) <- c(" ", "A 유형", "B 유형", "C 유형")
  
  # 요인 레벨(숫자 1,2,3)을 실제 설문용 텍스트로 치환합니다.
  choice_tab[1, 2:4] <- c("1년", "2년", "3년")[as.numeric(choice_tab[1, 2:4])]
  choice_tab[2, 2:4] <- c("70% 급여", "60% 급여", "60% 급여")[as.numeric(choice_tab[2, 2:4])] # (수준 확인 필요)
  choice_tab[3, 2:4] <- c("근로시간 단축", "편한 보직으로 전환", "계약직으로 재고용")[as.numeric(choice_tab[3, 2:4])]
  
  png(paste0("Kims_Conjoint_", i, ".png"), width = 10, height = 3, units = "cm", res = 400)
  
  # 표(Table) 형태의 그래픽 객체(grob)를 생성하고 스타일을 지정합니다.
  g <- tableGrob(
    choice_tab,
    cols = colnames(choice_tab), 
    rows = NULL,
    theme = ttheme_default(
      base_size = 12, 
      core = list(
        fg_params = list(hjust = rep(0.5, 4), x = rep(0.5, 4), fontsize = 9),
        bg_params = list(fill = c(rep("grey80", 3), rep(c("grey95", "grey90", "grey95"), 3)))
      ),
      colhead = list(bg_params = list(fill = "lightskyblue1"), fg_params = list(fontsize = 10))
    )
  )
  
  g$widths <- unit(c(0.25, 0.25, 0.25, 0.25), "npc")
  grid.draw(g)
  dev.off() # 저장 완료
}


# 4. 데이터 전처리 (mlogit 분석용) ---------------------------------------------
# 실제 설문 응답 결과가 담긴 엑셀 파일을 불러옵니다.
# 4. 데이터 전처리 (mlogit 분석용) ---------------------------------------------
# 실제 설문 응답 결과가 담긴 엑셀 파일을 불러오면서, 후속 분석에 필요한 변수만 추출합니다.
# df <- read_excel("df.xlsx") %>%
#   dplyr::select(
#     v21, v22, v23, v24, v25, v26, v27, v28,                      # 8개의 Choice Set 응답 결과
#     gender  # 개인 특성 및 서브그룹 분류 변수
#   )
# save(df, file = "df.RData")

load("df.RData")
# 응답자가 선택한 A유형, B유형, C유형(텍스트)을 분석에 사용할 인덱스(1, 2, 3)로 변환합니다.
# 미응답이나 아무것도 선택하지 않은 경우(No-choice)는 기본값인 4로 처리됩니다.
for(i in 1:8) {
  col_name <- paste0("v2", i)
  set_name <- paste0("set", i)
  df[[set_name]] <- 4
  df[[set_name]][df[[col_name]] == "A유형"] <- 1
  df[[set_name]][df[[col_name]] == "B유형"] <- 2
  df[[set_name]][df[[col_name]] == "C유형"] <- 3
}

df$respondentID <- 1:nrow(df) # 각 응답자에게 고유 ID 부여

# 다항 로짓 모형(mlogit) 구동을 위해서는 데이터가 '응답자 단위'가 아니라 '선택 대안 단위(Long format)'여야 합니다.
r <- 8 # 응답자가 응답한 선택 카드의 수(Choice sets)
k <- 4 # 하나의 카드 안에 있는 대안의 수 (A, B, C, 아무것도 선택 안함 = 총 4개)
minl_frame <- data.frame()

for (i in 1:nrow(df)){
  # 빈 데이터 프레임 뼈대 생성 (1명당 8개 세트 * 4개 대안 = 32행)
  tmp <- data.frame(
    respondentID = df$respondentID[i],
    choice = rep(1:r, each = k), 
    stimulus = rep(1:k, r)
  )
  
  tmp_choice <- rep(0, r * k) # 일단 모든 선택 여부를 0(선택 안함)으로 초기화
  
  # 각 카드에서 응답자가 실제로 '선택(1)'한 위치(Index)를 계산합니다.
  get_indices <- c(df$set1[i], df$set2[i], df$set3[i], df$set4[i], 
                   df$set5[i], df$set6[i], df$set7[i], df$set8[i]) + 
    rep(k, r) * (0:(r-1))
  
  tmp_choice[get_indices] <- 1 # 계산된 위치에만 1(선택함) 부여
  tmp <- cbind(tmp, tmp_choice)
  minl_frame <- rbind(minl_frame, tmp) # 완성된 개인 데이터를 전체 셋에 누적 병합
}

# 회귀분석을 위해 독립변수(속성 수준)를 더미 변수와 이펙트(효과) 변수로 변환합니다.
fulldesign_long <- fulldesign_long %>%
  mutate(
    # Dummy coding: 1번 수준을 기준(0)으로 두고 나머지 변수의 유무를 0과 1로 코딩합니다.
    # 해석 시: 기준이 되는 1번 수준과의 효용 차이(절대값)를 보여줍니다.
    length1 = ifelse(length == 1, 1, 0),
    length2 = ifelse(length == 2, 1, 0),
    length3 = ifelse(length == 3, 1, 0),
    wage1   = ifelse(wage == 1, 1, 0),
    wage2   = ifelse(wage == 2, 1, 0),
    wage3   = ifelse(wage == 3, 1, 0),
    work1   = ifelse(work == 1, 1, 0),
    work2   = ifelse(work == 2, 1, 0),
    work3   = ifelse(work == 3, 1, 0),
    none    = ifelse(stimulus == 4, 1, 0),
    
    # Effect coding: 수준들의 전체 평균을 0으로 두고 코딩합니다. (기준 수준은 -1로 코딩됨)
    # 해석 시: 속성의 평균 효용을 0으로 뒀을 때 특정 수준이 평균보다 얼마나 높고 낮은지를 보여줍니다.
    lengthE2 = case_when(length == 1 ~ -1, length == 2 ~ 1, TRUE ~ 0),
    lengthE3 = case_when(length == 1 ~ -1, length == 3 ~ 1, TRUE ~ 0),
    wageE2   = case_when(wage == 1 ~ -1, wage == 2 ~ 1, TRUE ~ 0),
    wageE3   = case_when(wage == 1 ~ -1, wage == 3 ~ 1, TRUE ~ 0),
    workE2   = case_when(work == 1 ~ -1, work == 2 ~ 1, TRUE ~ 0),
    workE3   = case_when(work == 1 ~ -1, work == 3 ~ 1, TRUE ~ 0),
    noneE    = ifelse(stimulus == 4, 1, -1)
  )

# Long format으로 쪼갠 응답 데이터와 실험 속성 데이터를 병합합니다.
merged_data <- merge(minl_frame, fulldesign_long, by = c("choice", "stimulus"))
merged_data <- merged_data[order(merged_data$respondentID), ]

# 개인 특성 변수(성별, 연령, 소속 등)를 컨조인트 데이터 프레임에 함께 병합합니다.
indiv_vars <- c("respondentID", "gender")
merged_data <- merge(merged_data, df[indiv_vars], by = "respondentID")

# mlogit 패키지가 분석할 수 있는 특수한 데이터 객체 형식(mlogit.data)으로 최종 변환합니다.
cbc <- mlogit.data(merged_data, choice = "tmp_choice", shape = "long", alt.var = "stimulus", id.var = "respondentID")


# 5. 파트워스(Partworth) 모형 분석 및 시각화 -----------------------------------

# 5.1 Dummy Coding Model (더미 코딩 기반 다항 로짓 모형)
# 1수준(length1, wage1, work1)을 베이스라인(0)으로 두고 나머지 수준들의 부분가치(Partworth) 효용을 추정합니다.
ml1 <- mlogit(tmp_choice ~ length2 + length3 + wage2 + wage3 + work2 + work3 + none | 0, cbc)

# 추정된 계수(estimate)와 표준오차(std.error)를 dotwhisker 시각화를 위한 데이터 프레임으로 정리합니다.
modelframe <- data.frame(
  term = c('2yr', '3yr', '70%', '60%', 'transfer', 'part_time', 'No-choice option'),
  estimate = coef(ml1),
  std.error = summary(ml1)$CoefTable[, "Std. Error"]
)

# 시각화할 때 Y축의 변수들을 속성(length, wage, work)별로 묶어줄 라벨 브라켓을 정의합니다.
three_brackets <- list(
  c("length\nRef: 1yr", "2yr", "3yr"), 
  c("wage\nRef: 80%", '70%', '60%'),
  c("work\nRef: reduction", 'transfer', 'part-time')
) 

# 안전한 방식: { }로 ggplot 객체를 완전히 묶은 후 add_brackets(그루핑 라벨)을 적용합니다. (파이프 충돌 방지)
p1 <- { dwplot(modelframe, vline = geom_vline(xintercept = 0, colour = "grey60", linetype = 1)) + 
    theme_bw() + scale_colour_grey() + theme(legend.position = "none") + 
    xlab("Estimated partworth utilities") + ylab("") } %>% 
  add_brackets(three_brackets) 
p1

# add_brackets의 결과물은 grob 객체이므로 ggsave가 아닌 grid.draw() 함수를 통해 이미지로 저장합니다.
png("Fig_1.png", width = 18, height = 18, units = "cm", res = 400)
grid.draw(p1)
dev.off()

pdf("Fig_1.pdf", width = 7, height = 7)
grid.draw(p1)
dev.off()


# 5.2 Effect Coding Model (이펙트 코딩 기반 다항 로짓 모형)
# 각 속성의 전체 평균을 0으로 기준을 잡기 때문에, 각 계수는 '전체 평균 대비 선호/비선호 정도'를 의미합니다.
ml1e <- mlogit(tmp_choice ~ lengthE2 + lengthE3 + wageE2 + wageE3 + workE2 + workE3 + none | 0, cbc)

covMatrix <- vcov(ml1e) # 공분산 행렬 추출 (기준 수준의 표준 오차를 계산하기 위함)

# 이펙트 코딩에서는 생략되었던 1수준(1yr, 80%, reduction)의 계수 값을 나머지 계수들의 합에 마이너스(-)를 붙여 계산 복원합니다.
modelframe_e <- data.frame(
  term = c('1yr', '2yr', '3yr', '80%', '70%', '60%', 'reduction', 'transfer', 'part-time', 'No-choice option'),
  estimate = c(
    -(coef(ml1e)[1] + coef(ml1e)[2]), coef(ml1e)[1], coef(ml1e)[2],
    -(coef(ml1e)[3] + coef(ml1e)[4]), coef(ml1e)[3], coef(ml1e)[4],
    -(coef(ml1e)[5] + coef(ml1e)[6]), coef(ml1e)[5], coef(ml1e)[6],
    coef(ml1e)[7]
  ),
  std.error = c(
    sqrt(sum(covMatrix[1:2, 1:2])), summary(ml1e)$CoefTable[1, "Std. Error"], summary(ml1e)$CoefTable[2, "Std. Error"], 
    sqrt(sum(covMatrix[3:4, 3:4])), summary(ml1e)$CoefTable[3, "Std. Error"], summary(ml1e)$CoefTable[4, "Std. Error"], 
    sqrt(sum(covMatrix[5:6, 5:6])), summary(ml1e)$CoefTable[5, "Std. Error"], summary(ml1e)$CoefTable[6, "Std. Error"], 
    summary(ml1e)$CoefTable[7, "Std. Error"]
  )
)

three_brackets_e <- list(
  c("length", "1yr", "3yr"), c("wage", '80%', '60%'), c("work", 'reduction', 'part-time')
) 

p1e <- { dwplot(modelframe_e, vline = geom_vline(xintercept = 0, colour = "grey60", linetype = 1)) + 
    theme_bw() + scale_colour_grey() + theme(legend.position = "none") + 
    xlab("Estimated partworth utilities") + ylab("") } %>% 
  add_brackets(three_brackets_e)
p1e

png("Fig_1e.png", width = 18, height = 18, units = "cm", res = 400)
grid.draw(p1e)
dev.off()

pdf("Fig_1e.pdf", width = 7, height = 7)
grid.draw(p1e)
dev.off()


# 6. 서브그룹(Subgroup) 비교 분석 ----------------------------------------------
# 그룹별 추정 모형 결과를 시각화용 데이터 프레임으로 일정하게 추출해주는 커스텀 함수를 생성합니다.
extract_modelframe <- function(model_obj, group_name) {
  data.frame(
    term = c('2yr', '3yr', '70%', '60%', 'transfer', 'part_time', 'No-choice option'),
    estimate = coef(model_obj),
    std.error = summary(model_obj)$CoefTable[, "Std. Error"],
    model = group_name
  )
}

# 6.1 성별(Gender) 분석
# 전체 데이터(cbc)를 남성과 여성으로 부분 추출(subset)하여 각각 별개의 로짓 모형을 추정합니다.
ml2g1 <- mlogit(tmp_choice ~ length2 + length3 + wage2 + wage3 + work2 + work3 + none | 0, cbc[cbc$gender == "남성", ])
ml2g2 <- mlogit(tmp_choice ~ length2 + length3 + wage2 + wage3 + work2 + work3 + none | 0, cbc[cbc$gender == "여성", ])
summary(ml2g1)

# 추출 함수를 이용해 남성/여성 모형의 결과를 하나의 표로 아래위로 합칩니다(rbind).
modelframe_allg2 <- rbind(
  extract_modelframe(ml2g1, "male"),
  extract_modelframe(ml2g2, "female")
)
modelframe_allg2$model <- factor(modelframe_allg2$model, levels = c("male", "female"))

# 성별 간의 부분가치(Partworth) 차이를 하나의 그래프 위에 점과 선으로 비교 시각화합니다.
p3 <- { dwplot(modelframe_allg2, vline = geom_vline(xintercept = 0, colour = "grey60", linetype = 1), 
               dot_args = list(aes(shape = model), size = 2.5)) + 
    theme_bw() + xlab("Estimated partworth utilities") + ylab("") +  
    theme(plot.title = element_text(face = "bold"), legend.position = "bottom", legend.title = element_blank()) + 
    scale_color_grey() } %>% 
  add_brackets(three_brackets)
p3

png("Fig_3.png", width = 18, height = 18, units = "cm", res = 400)
grid.draw(p3)
dev.off()

pdf("Fig_3.pdf", width = 7, height = 7)
grid.draw(p3)
dev.off()

# 분석이 끝난 후 사용했던 메모리를 반환하기 위한 정리 코드입니다.
# rm(list=ls())
# gc()
