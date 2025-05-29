# Naka45
1+1

`1+2`もいいね！
#1
rm(list=ls())
#2
print("Hello,R World")
#3
5 + 3 * 2 - 10 / 2
#4
(5+3)*(2-10)/2
#5

sqrt(841)
#6

?sqrt
#7

2^3
#8

98^7
#9

sqrt(-4)
#10
#11
x<-42
x
##12
hoge<-1:10
hoge
hoge2<-hoge*2
hoge2
str(hoge2)
hoge2[3]
hoge2[2:5]
hoge2[c(2,4,6,8,10)]
matrix(hoge2,ncol=2)
hoge3<-matrix(hoge2,ncol=2,byrow=TRUE)
hoge3
dim(hoge3)
hoge3[1,]
hoge3[,2]
hoge3[2,2]
hoge3 <- as.data.frame(hoge3)
str(hoge3)
colnames(hoge3)<-c("A,","B")
hoge3
library(tidyverse)
hoge3<-as_tibble(hoge3)


library(tidyverse)

### 6.2.5 データ読み込みと基本統計量 ###
# 41. データ読み込み
dat <- read_csv("BaseballDecade.csv")

# 42. 先頭部分確認
head(dat)

# 43. 末尾部分確認
tail(dat)

# 44. 行数・列数確認
dim(dat)

# 45. 変数名一覧
names(dat)

# 46. 要約統計量
summary(dat)

# 47. データ構造確認
str(dat)

# 48. クラス確認
class(dat)

# 49. tibble型として保持
dat.tb <- dat

# 50. tibbleデータ確認
dat.tb

### 6.2.6 データフレームの列・変数の操作 ###
# 51. Name変数表示
dat.tb$Name

# 52. 選手名出現回数
table(dat.tb$Name)

# 53. パイプ演算子入力（RStudioショートカット）
# 実際のコードには反映されません

# 54. 選手名出現回数（降順）
dat.tb$Name %>% table() %>% sort(decreasing = TRUE)

# 55. チーム名種類
dat.tb$team %>% unique()

# 56. チーム名数
dat.tb$team %>% unique() %>% length()

# 57-59. Factor型への変換
dat.tb$team <- dat.tb$team %>% as.factor()
dat.tb$bloodType <- dat.tb$bloodType %>% as.factor()
dat.tb$position <- dat.tb$position %>% as.factor()

# 60. Factor変数の度数
dat.tb %>% select(team, bloodType, position) %>% summary()

### 6.2.7 数値データの集計と新しい変数の作成 ###
# 61. 身長の平均
dat.tb$height %>% mean(na.rm = TRUE)

# 62. 身長の分散
dat.tb$height %>% var(na.rm = TRUE)

# 63. 身長の標準偏差
dat.tb$height %>% sd(na.rm = TRUE)

# 64. 身長の範囲
dat.tb$height %>% range(na.rm = TRUE)

# 65. 給与の四分位数
dat.tb$salary %>% quantile(na.rm = TRUE)

# 66. カスタムパーセンタイル
dat.tb$salary %>% quantile(
  probs = c(0, 0.25, 0.33, 0.95, 1),
  na.rm = TRUE
)

# 67. BMI計算
dat.tb <- dat.tb %>% mutate(bmi = weight / ((height/100)^2))

# 68. BMI要約統計量
dat.tb$bmi %>% summary()

# 69. BMIカテゴリ作成
dat.tb <- dat.tb %>% mutate(
  bmi_category = ifelse(bmi >= 25, "HighBMI", "Standard")
)

# 70. BMIカテゴリ頻度
dat.tb$bmi_category %>% table()

### 6.2.8 データの分類と条件付き処理 ###

# 71. 投手と野手を区別する変数position2を作成
dat.tb <- dat.tb %>%
  mutate(position2 = case_when(
    position == "投手" ~ "投手",
    TRUE ~ "野手"
  ))

# 72. position2をFactor型に変換
dat.tb$position2 <- dat.tb$position2 %>% as.factor()

# 73. position2の頻度確認
dat.tb$position2 %>% table()

# 74. positionとposition2のクロス集計表
table(dat.tb$position, dat.tb$position2)

# 75. セ・リーグとパ・リーグを区別するLeague変数作成
dat.tb <- dat.tb %>%
  mutate(League = case_when(
    team %in% c("Giants", "Carp", "Tigers", "Swallows", "Dragons", "DeNA") ~ "Central",
    TRUE ~ "Pacific"
  ))

# 76. LeagueをFactor型に変換
dat.tb$League <- dat.tb$League %>% as.factor()

# 77. リーグごとのチーム数確認
table(dat.tb$team, dat.tb$League)






















  
  
  
  
  
  