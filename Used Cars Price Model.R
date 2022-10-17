
# ----------------------------------------------------------------------------
# 실습 데이터셋 준비
# ----------------------------------------------------------------------------

# 관련 패키지를 호출합니다.
library(tidyverse)

# 중고차 가격 데이터를 포함하는 링크를 설정합니다.
url <- 'https://bit.ly/Used_Cars_Midsize'

# 링크에 포함된 텍스트 데이터를 읽고 데이터프레임을 생성합니다.
# [주의] 한글 인코딩 방식이 'UTF-8'이 아니면 에러를 반환합니다.
df <- read.csv(file = url)

# 텍스트 파일의 인코딩 방식을 확인합니다.
guess_encoding(file = url)

# 한글 인코딩 방식을 지정하여 텍스트 데이터를 다시 읽습니다.
df <- read.csv(file = url, fileEncoding = 'EUC-KR')

# df의 구조를 확인합니다.
str(object = df)


# ----------------------------------------------------------------------------
# 데이터 전처리
# ----------------------------------------------------------------------------

# df의 열별 자료형이 문자형인지 여부를 TRUE와 FALSE로 반환합니다.
map_chr(.x = df, .f = class) == 'character' -> locs
print(x = locs)

# df에 있는 문자형 변수를 범주형으로 일괄 변환합니다.
df[locs] <- map_df(.x = df[locs], .f = as.factor)

# df의 요약 데이터를 확인합니다.
# 수치형 벡터는 기술 통계량, 범주형 벡터는 범주별 빈도수를 반환합니다.
summary(object = df)


# ----------------------------------------------------------------------------
# 데이터 시각화
# ----------------------------------------------------------------------------

# 관련 패키지를 호출합니다.
library(showtext)

# 구글폰트에서 한글 폰트를 설치합니다.
# 링크: https://fonts.google.com/?subset=korean
font_add_google(name = 'Gamja Flower')

# 방금 설치한 한글 폰트를 모든 그래픽 장치에서 사용하도록 설정합니다.
showtext_auto()

# 사용자 테마를 설정합니다.
mytheme <- theme_bw(base_family = 'Gamja Flower')

# 히스토그램을 그려서 중고차 가격의 분포를 확인합니다.
ggplot(data = df, 
       map = aes(x = 가격)) + 
  geom_histogram(breaks = seq(from = 0, to = 2500, by = 100), 
                 fill = 'white', color = 'black') + 
  mytheme

# 상자 수염 그림을 그립니다.
ggplot(data = df, 
       map = aes(y = 가격)) + 
  geom_boxplot(outlier.color = 'red') + 
  mytheme

# 산점도를 그려서 연속형 입력변수와의 관계를 확인합니다.
# 주행거리와 가격의 산점도를 그립니다.
ggplot(data = df, 
       map = aes(x = 주행거리, y = 가격)) + 
  geom_point(color = 'gray') + 
  geom_smooth(method = 'lm', se = FALSE, color = 'red') + 
  mytheme

# 연식개월과 가격의 산점도를 그립니다.
ggplot(data = df, 
       map = aes(x = 연식개월, y = 가격)) + 
  geom_point(color = 'gray') + 
  geom_smooth(method = 'lm', se = FALSE, color = 'red') + 
  mytheme


# 상자 수염 그림을 그려서 범주형 입력변수와의 관계를 확인합니다.
# 제조사 범주별 가격의 분포에 평균을 추가한 그래프를 그립니다.
df %>% group_by(제조사) %>% summarise(m = mean(x = 가격)) -> avgs
ggplot(data = df, 
       map = aes(x = 제조사, y = 가격)) + 
  geom_boxplot(map = aes(fill = 제조사)) + 
  geom_point(data = avgs, map = aes(y = m), size = 5) + 
  geom_hline(yintercept = mean(x = df$가격), color = 'red') + 
  scale_fill_brewer(palette = 'YlOrRd') + 
  mytheme + 
  theme(legend.position = 'none')

# 사고여부 범주별 가격의 분포에 평균을 추가한 그래프를 그립니다.
df %>% group_by(사고여부) %>% summarise(m = mean(x = 가격)) -> avgs
ggplot(data = df, 
       map = aes(x = 사고여부, y = 가격)) + 
  geom_boxplot(map = aes(fill = 사고여부)) + 
  geom_point(data = avgs, map = aes(y = m), size = 5) + 
  geom_hline(yintercept = mean(x = df$가격), color = 'red') + 
  scale_fill_brewer(palette = 'YlOrRd') + 
  mytheme + 
  theme(legend.position = 'none')

# 연료형태 범주별 가격의 분포에 평균을 추가한 그래프를 그립니다.
df %>% group_by(연료형태) %>% summarise(m = mean(x = 가격)) -> avgs
ggplot(data = df, 
       map = aes(x = 연료형태, y = 가격)) + 
  geom_boxplot(map = aes(fill = 연료형태)) + 
  geom_point(data = avgs, map = aes(y = m), size = 5) + 
  geom_hline(yintercept = mean(x = df$가격), color = 'red') + 
  scale_fill_brewer(palette = 'YlOrRd') + 
  mytheme + 
  theme(legend.position = 'none')


# ----------------------------------------------------------------------------
# 연속형 입력변수의 상관관계 확인
# ----------------------------------------------------------------------------

# df의 열별 자료형이 수치형(정수, 실수)인지 여부를 TRUE와 FALSE로 반환합니다.
map_chr(.x = df, .f = class) %in% c('integer', 'numeric') -> locs

# df에서 수치형 변수를 선택하고 상관계수 행렬을 출력합니다.
# [참고] 목표변수와 입력변수의 상관계수는 절대값이 1에 가까울수록 좋습니다.
# 하지만 입력변수끼리의 상관계수는 절대값이 0에 가까울수록 좋습니다.
cor(x = df[locs]) %>% round(digits = 4)

# 피어슨 상관분석을 실행하고 유의확률을 확인합니다.
# [참고] 유의확률(p-value)이 유의수준(0.05)보다 작으면 귀무가설을 기각합니다.
# 귀무가설: 모상관계수가 0이다.
cor.test(x = df$주행거리, y = df$가격)
cor.test(x = df$연식개월, y = df$가격)

# df에서 수치형 변수를 선택하고 가격과의 피어슨 상관분석 유의확률을 반환합니다.
# [참고] 아래 코드는 수치형 변수의 개수가 많을 때 유용합니다.
map_dbl(.x = df[locs], .f = function(x) {
  test <- cor.test(x = x, y = df$가격)
  pval <- round(x = test$p.value, digits = 4)
  return(pval)
})


# ----------------------------------------------------------------------------
# 두 집단의 평균 비교(독립표본 t-검정)
# ----------------------------------------------------------------------------

# 제조사 범주별 가격의 잔차(실제값 - 평균)를 얻기 위해 회귀모형을 적합합니다.
fit <- lm(formula = 가격 ~ 제조사, data = df)

# 잔차에 대한 정규성 검정을 실행합니다.
# 귀무가설: 데이터가 정규분포한다.
shapiro.test(x = fit$residuals)

# 제조사 범주별 등분산성 검정을 실행합니다.
# 귀무가설: 두 집단의 모분산을 나눈 비(ratio)가 1이다.
var.test(formula = 가격 ~ 제조사, data = df)

# 등분산 가정을 만족하는 독립표본 t-검정을 실행합니다.
# 귀무가설: 두 집단의 모평균이 같다.
t.test(formula = 가격 ~ 제조사, data = df, var.equal = TRUE)

# 윌콕슨 순위합 검정을 실행합니다.
# [참고] 두 집단 분포 위치가 같은지 여부를 판단하는 비모수적인 검정 방법입니다.
# 귀무가설: 두 집단 간 분포의 위치가 같다.
wilcox.test(formula = 가격 ~ 제조사, data = df)


# 사고여부 범주별 가격의 잔차(실제값 - 평균)를 얻기 위해 회귀모형을 적합합니다.
fit <- lm(formula = 가격 ~ 사고여부, data = df)

# 잔차에 대한 정규성 검정을 실행합니다.
shapiro.test(x = fit$residuals)

# 사고여부 범주별 등분산성 검정을 실행합니다.
var.test(formula = 가격 ~ 사고여부, data = df)

# 등분산 가정을 만족하는 독립표본 t-검정을 실행합니다.
t.test(formula = 가격 ~ 사고여부, data = df, var.equal = TRUE)

# 윌콕슨 순위합 검정을 실행합니다.
wilcox.test(formula = 가격 ~ 사고여부, data = df)


# ----------------------------------------------------------------------------
# 세 집단의 평균 비교(일원배치 분산분석)
# ----------------------------------------------------------------------------

# 연료형태 범주별 가격의 잔차(실제값 - 평균)를 얻기 위해 회귀모형을 적합합니다.
fit <- lm(formula = 가격 ~ 연료형태, data = df)

# 잔차에 대한 정규성 검정을 실행합니다.
shapiro.test(x = fit$residuals)

# 연료형태 범주별 등분산성 검정을 실행합니다.
# [참고] 레벤 테스트는 셋 이상 집단의 등분산성을 검정할 때 사용합니다.
# 레벤 테스트는 데이터의 정규성 만족 여부와 상관 없이 실행할 수 있습니다.
# 귀무가설: 모든 집단의 모분산이 같다.
library(car)
leveneTest(y = 가격 ~ 연료형태, data = df)

# 등분산 가정을 만족하지 않는 일원배치 분산분석을 실행합니다.
# 귀무가설: 모든 집단의 모평균이 같다.
oneway.test(formula = 가격 ~ 연료형태, data = df, var.equal = FALSE)

# 윌콕슨 순위합 검정을 실행합니다.
# [참고] 셋 이상 집단 분포 위치가 같은지 여부를 판단하는 비모수적인 검정 방법입니다.
# 귀무가설: 모든 집단 간 분포의 위치가 같다.
kruskal.test(formula = 가격 ~ 연료형태, data = df)

# 분산분석 결과 귀무가설을 기각했다면 사후분석을 통해 어떤 집단 간 평균이 다른지 
# 확인합니다. 검정을 반복하므로 다중비교라고도 합니다.
pairwise.t.test(x = df$가격, g = df$연료형태, p.adj = 'bonferroni')
DescTools::ScheffeTest(x = df$가격, g = df$연료형태)
DescTools::NemenyiTest(x = df$가격, g = df$연료형태)


# ----------------------------------------------------------------------------
# 실습 데이터셋 정리
# ----------------------------------------------------------------------------

# 집단별 가격 평균이 같은 열과 행을 삭제합니다.
df %>% 
  select(-제조사) %>% 
  filter(연료형태 != '디젤') %>% 
  mutate(연료형태 = factor(x = 연료형태)) -> df

# 실습 데이터셋의 70%를 무작위로 선택하고 index를 생성합니다.
set.seed(seed = 1)
n <- nrow(x = df)
index <- sample(x = n, size = n * 0.7, replace = FALSE)

# index를 포함하는 행을 훈련셋(trSet), 포함하지 않는 행을 시험셋(teSet)에 
# 할당합니다.
df %>% slice(index) -> trSet
df %>% slice(-index) -> teSet

# 훈련셋과 시험셋의 목표변수 평균을 확인합니다.
mean(x = trSet$가격)
mean(x = teSet$가격)


# ----------------------------------------------------------------------------
# 선형 회귀모형 적합
# ----------------------------------------------------------------------------

# 훈련셋에서 가격을 제외한 모든 입력변수를 사용한 선형 회귀모형을 적합합니다.
fit1 <- lm(formula = 가격 ~ ., data = trSet)

# 선형 회귀모형 적합 결과를 확인합니다.
# F-통계량의 유의확률, 회귀계수별 t-통계량의 유의확률을 차례대로 확인합니다.
summary(object = fit1)

# 그래프를 그려서 잔차가정 만족 여부를 확인합니다.
par(mfrow = c(2, 2))
plot(x = fit1)
par(mfrow = c(1, 1))

# 잔차의 정규성 검정을 실행합니다.
shapiro.test(x = fit1$residuals)

# 잔차의 등분산성 검정을 실행합니다.
ncvTest(model = fit1)

# 잔차의 독립성 검정을 실행합니다.
durbinWatsonTest(model = fit1, simulate = FALSE)

# 입력변수별 잔차의 선형성 여부를 그래프로 확인합니다.
crPlots(model = fit1)


# ----------------------------------------------------------------------------
# 영향점 확인
# ----------------------------------------------------------------------------

# 선형 회귀모형의 영향점을 확인합니다.
# 관측값별 표준화 잔차와 레버리지 및 쿡의 거리를 계산합니다.
# 쿡의 거리는 해당 관측값을 제외했을 때 다른 관측값들의 추정값이 변동하는 크기를
# 계산한 것이며, 쿡의 거리가 큰 값일 때 해당 관측값을 영향점이라 판단합니다.
aug <- broom::augment(x = fit1)

# 훈련셋에 쿡의 거리를 기준으로 영향점 여부를 '1'과 '0'으로 추가합니다.
# [참고] 쿡의 거리가 4/n(행 길이)보다 크면 영향점으로 판단합니다.
trSet %>% 
  mutate(영향점 = ifelse(test = aug$.cooksd > 4 / nrow(x = aug), 
                         yes = '1', 
                         no = '0')) -> trSet

# 영향점 범주별 빈도수를 확인합니다.
table(trSet$영향점)

# 연식개월과 가격의 산점도를 그리고 영향점을 위치(빨간점)를 확인합니다.
ggplot(data = trSet, 
       map = aes(x = 연식개월, y = 가격)) + 
  geom_point(map = aes(color = 영향점)) + 
  geom_smooth(method = 'lm', color = 'black', se = FALSE) + 
  scale_color_discrete(type = c('gray', 'red')) + 
  mytheme

# 주행거리와 가격의 산점도를 그리고 영향점을 위치(빨간점)를 확인합니다.
ggplot(data = trSet, 
       map = aes(x = 주행거리, y = 가격)) + 
  geom_point(map = aes(color = 영향점)) + 
  geom_smooth(method = 'lm', color = 'black', se = FALSE) + 
  scale_color_discrete(type = c('gray', 'red')) + 
  mytheme

# 훈련셋에서 영향점을 제거합니다.
trSet %>% 
  filter(영향점 != '1') %>% 
  select(-영향점) -> trSet


# ----------------------------------------------------------------------------
# 선형 회귀모형 재적합
# ----------------------------------------------------------------------------

# 영향점을 제거한 훈련셋으로 선형 회귀모형을 다시 적합합니다.
fit2 <- lm(formula = 가격 ~ ., data = trSet)

# 선형 회귀모형 적합 결과를 확인합니다.
summary(object = fit2)

# 그래프를 그려서 잔차가정 만족 여부를 확인합니다.
par(mfrow = c(2, 2))
plot(x = fit2)
par(mfrow = c(1, 1))

# 잔차의 정규성 검정을 실행합니다.
shapiro.test(x = fit2$residuals)

# 잔차의 등분산성 검정을 실행합니다.
ncvTest(model = fit2)

# 잔차의 독립성 검정을 실행합니다.
durbinWatsonTest(model = fit2, simulate = FALSE)

# 입력변수별 잔차의 선형성 여부를 그래프로 확인합니다.
crPlots(model = fit2)


# ----------------------------------------------------------------------------
# 선형 회귀모형의 해석
# ----------------------------------------------------------------------------

# 입력변수별 분산팽창지수를 통해 다중공선성 입력변수를 확인합니다.
# [참고] 분산팽창지수가 10 이상이면 다른 입력변수에 종속되었다고 판단합니다.
vif(mod = fit2)


# 주행거리와 가격의 산점도에 사고여부 범주별 회귀직선을 추가합니다.
ggplot(data = trSet, 
       map = aes(x = 주행거리, y = 가격, color = 사고여부)) + 
  geom_point(alpha = 0.2) + 
  geom_smooth(method = 'lm', se = FALSE) + 
  theme_bw(base_family = 'Gamja Flower')

# 연식개월과 가격의 산점도에 사고여부 범주별 회귀직선을 추가합니다.
ggplot(data = trSet, 
       map = aes(x = 연식개월, y = 가격, color = 사고여부)) + 
  geom_point(alpha = 0.2) + 
  geom_smooth(method = 'lm', se = FALSE) + 
  theme_bw(base_family = 'Gamja Flower')


# 입력변수별 회귀계수를 확인합니다.
fit2$coefficients %>% round(digits = 4)

# 입력변수별 표준화 회귀계수를 확인합니다.
# [참고] 표준화 회귀계수에서 부호는 방향을 의미할 뿐이며 절대값이 중요합니다.
beta.z <- reghelper::beta(model = fit2)
beta.z$coefficients[, 1] %>% abs() %>% sort() %>% round(digits = 4)


# ----------------------------------------------------------------------------
# 선형 회귀모형의 성능 평가
# ----------------------------------------------------------------------------

# 관련 패키지를 호출합니다.
library(MLmetrics)

# 선형 회귀모형에 시험셋을 적용하여 목표변수의 추정값을 생성합니다.
teReal <- teSet$가격
tePred <- predict(object = fit2, newdata = teSet)

# 시험셋의 다양한 회귀모형 성능 지표를 확인합니다.
MSE(y_pred = tePred, y_true = teReal)
RMSE(y_pred = tePred, y_true = teReal)
MAE(y_pred = tePred, y_true = teReal)
MAPE(y_pred = tePred, y_true = teReal)


## End of Document
