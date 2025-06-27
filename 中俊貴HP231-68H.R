-# Naka45
1+1

#`1+2`もいいね！
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

### 6.2.5 データ読み込みと基本統計量
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
0
# 77. リーグごとのチーム数確認
table(dat.tb$team, dat.tb$League)

#78
dat.tb<-dat.tb %>% 
  mutate(year_num=Year %>% str_remove("年度") %>% as.numeric())

#79
dat.tb %>% select(Year,year_num) %>% head() %>% head()
#80

dat.tb %>% filter(position2 == "野手") %>% head()
 


dat.tb %>% filter(year_num == 2020 & League == "Central") %>% head()

# 85. 2020年セ・リーグの選手数
dat.tb %>% filter(year_num == 2020 & League == "Central") %>% nrow()

# 86. 選手名、チーム、身長、体重を表示
dat.tb %>% select(Name, team, height, weight) %>% head()

# 87. 2020年の選手名、チーム、年俸を表示
dat.tb %>% select(Name, team, salary, year_num) %>% filter(year_num == 2020) %>% head()
#88
dat.tb %>% arrange(desc(salary)) %>% head(1)

dat.tb %>% filter(year_num == 2020 & League == "Central") %>%
  arrange(desc(salary)) %>% head(1)
#90

dat.tb %>% filter(team=="Giants") %>% 
  summarise(avg_height=mean(height),avg_weight=mean(weight))


dat.tb %>% group_by(team) %>%
  summarise(mean_salary = mean(salary)) %>% arrange(desc(mean_salary))


##91 グループ集計と要約統計量
dat.tb %>% group_by(team) %>% 
  summarise(mean_salary=mean(salary)) %>% arrange(desc(mean_salary))

##92
dat.tb %>% group_by(year_num , team) %>%
 summarise(mean_salary=mean(salary)) %>% head(10)



##93
dat.tb %>% group_by(year_num,team) %>% 
  summarize(
    mean_salary=mean(salary),
    max_salary=max(salary),
    min_salary=min(salary)
  )%>% head(10)


  dat.tb %>% group_by(year_num, team) %>%
    summarise(mean_salary = mean(salary),
              max_salary = max(salary),
              min_salary = min(salary)) %>% head(10)

##94 
  dat.tb %>% group_by(bloodType) %>% 
    summarize(mean_bml=mean(bmi)) %>% 
    arrange(desc(mean_bml))
  dat.tb %>% group_by(bloodType) %>%
    summarise(mean_bmi = mean(bmi)) %>% arrange(desc(mean_bmi))                            
                                                
                                                
                                                
##96  ポジションの平均身長と体重を確認する                                            
  dat.tb %>% group_by(position) %>%
    summarise(avg_height = mean(height),
              avg_weight = mean(weight)) %>%
    arrange(desc(avg_height))                                             
                                                
                                                
              
  dat.tb %>% 
    group_by(position) %>%
    summarise(avg_height = mean(height, na.rm = TRUE),
              avg_weight = mean(weight, na.rm = TRUE)) %>%
    arrange(desc(avg_height))                                              
dat.tb %>% 
    group_by(position) %>%
    summarise(avg_height = mean(height, na.rm = TRUE),
              avg_weight = mean(weight, na.rm = TRUE)) %>%
    arrange(desc(avg_height))                                               

##97年代ごとのHR総数を計算する

dat.tb %>% group_by(year_num) %>% summarise(total_HR = sum(HR, na.rm = TRUE))



#98　データフレーム作成する

dat.tb %>% select(year_num,Name,height,weight) %>% head()

##99 2020年度に限定する

dat.tb %>% select(year_num,Name,height,weight) %>% 
  filter(year_num==2020) %>% head()

#100 データを修正する

dat.tb2 <- dat.tb %>% select(year_num, Name, height, weight) %>%
  filter(year_num == 2020) %>% select(-year_num)

head(dat.tb2)

#101

model<-lm(height~weight,data = dat.tb2)

summary(model)

#102

dat.tb2 %>% pivot_longer(-Name,names_to = "variable",values_to = "value") %>% head()

##106

dat.tb2_long<-dat.tb2 %>% 
  pivot_longer(-Name,names_to = "variable",values_to = "value")
#107
str(dat.tb2_long)

#108
dat.tb2_long %>% group_by(variable) %>% summarise(mean_value=mean(value))


#109
dat.tb2_long %>% pivot_wider(names_from = variable, values_from = value) %>% head()





#110. 野手のみのデータで、打撃成績に関する変数を抽出する：
bat_stats <- dat.tb %>% filter(position2 == "野手") %>%
  select(year_num, Name, AtBats, Hit, HR)

bat_stats


# 111. 打率を計算する（安打数 ÷ 打席数）
bat_stats <- bat_stats %>% mutate(avg = Hit / AtBats)
# 解説: bat_statsデータフレームに新しい列avgを追加し、Hit(安打数)をAtBats(打席数)で割った値を格納

# 112. 新しく計算した打率を確認する
head(bat_stats)
# 解説: bat_statsの先頭6行を表示して、avg列が正しく計算されているか確認

# 113. 打率でソートして上位10人を表示する
bat_stats %>% arrange(desc(avg)) %>% head(10)
# 解説: avg列を降順に並べ替え、上位10選手を表示

# 114. 年度別に打率の平均を計算する
bat_stats %>% 
  group_by(year_num) %>% 
  summarise(avg_batting = mean(avg, na.rm = TRUE))
# 解説: Year_num(年度)ごとにグループ化し、各年度の平均打率を計算

# 115. データを縦長形式に変換する
bat_stats_long <- bat_stats %>%
  pivot_longer(c(AtBats, Hit, HR, avg), names_to = "stat", values_to = "value")
# 解説: AtBats, Hit, HR, avgの列を縦長形式に変換。統計指標名はstat列、値はvalue列に格納

# 116. 縦長データを確認する
head(bat_stats_long)
# 解説: 変換後の縦長データの先頭6行を確認

# 117. 選手と統計指標でグループ化して年間の平均値を計算
bat_stats_long %>% 
  group_by(Name, stat) %>%
  summarise(mean_value = mean(value, na.rm = TRUE)) %>% 
  head()
# 解説: 選手名(Name)と統計指標(stat)ごとに平均値を計算し、先頭6行を表示

# 118. 平均値を横長形式に戻す
player_avgs <- bat_stats_long %>% 
  group_by(Name, stat) %>%
  summarise(mean_value = mean(value, na.rm = TRUE)) %>%
  pivot_wider(names_from = stat, values_from = mean_value)
# 解説: 縦長データから、選手ごとに統計指標を列にした横長形式に変換

# 119. 打率が最も高かった選手を確認
player_avgs %>% arrange(desc(avg)) %>% head(1)


#dat.tb の身長についてヒストグラムを描く：


ggplot(dat.tb, aes(x = height)) + geom_histogram()


#ヒストグラムのビンの幅を調整する

ggplot(dat.tb, aes(x = height)) + geom_histogram(binwidth = 2)

#ヒストグラムに色を追加する：
ggplot(dat.tb, aes(x = height)) + geom_histogram(fill = "blue", color = "black")


#125dat.tb の身長と体重について散布図を描く：

ggplot(dat.tb, aes(x = height, y = weight)) + geom_point()

#126散布図の各点を血液型で色分けする
ggplot(dat.tb, aes(x = height, y = weight, color = bloodType)) + geom_point()



#128. 散布図の点のサイズを大きくする：


ggplot(dat.tb, aes(x = height, y = weight, shape = bloodType)) + geom_point()


#129. 散布図にタイトルを追加する：
ggplot(dat.tb, aes(x = height, y = weight, color = bloodType)) +
  geom_point(size = 3) + labs(title = "身長と体重の関係") 

#130. 散布図の軸ラベルを日本語に変更する：
ggplot(dat.tb, aes(x = height, y = weight, color = bloodType)) +
  geom_point(size = 3) + labs(title = "身長と体重の関係",
                              x = "身長(cm)", y = "体重(kg)")


#131. dat.tb の身長と体重についての散布図を、チームごとに分割する：


ggplot(dat.tb, aes(x = height, y = weight)) +
  geom_point() + facet_wrap(~ team)

##132. 散布図に回帰直線を追加する：
ggplot(dat.tb, aes(x = height, y = weight)) +
  geom_point() + geom_smooth(method = "lm")

##133. 散布図に平滑化曲線を追加する：
ggplot(dat.tb, aes(x = height, y = weight)) +
  geom_point() + geom_smooth()

##134. リーグ別に色分けした散布図と回帰線を描く：
ggplot(dat.tb, aes(x = height, y = weight, color = League)) +
  geom_point() + geom_smooth(method = "lm")


#135. ポジション別の身長のボックスプロットを描く：
ggplot(dat.tb, aes(x = position, y = height)) + geom_boxplot()



##136. ボックスプロットを投手と野手の2 グループに簡略化する：
ggplot(dat.tb, aes(x = position2, y = height)) + geom_boxplot()


##137. ボックスプロットに個々のデータ点を追加する：

ggplot(dat.tb, aes(x = position2, y = height)) +
  geom_boxplot() + geom_jitter(width = 0.2, alpha = 0.5)



#138. x 軸に血液型、y 軸に体重としたバイオリンプロットを描画する：

ggplot(dat.tb, aes(x = bloodType, y = weight)) + geom_violin()


#140. x 軸を血液型、y 軸を体重とし、リーグで色分けしたバイオリンプロットを描く：

ggplot(dat.tb, aes(x = bloodType, y = weight, fill = League)) +
  geom_violin()


#141. x 軸を血液型、y 軸を体重とし、リーグでグラフを分割する：

ggplot(dat.tb, aes(x = bloodType, y = weight)) +
  geom_violin() + facet_wrap(~ League)

#142. リーグでの分割を上下に配置する：
ggplot(dat.tb, aes(x = bloodType, y = weight)) +
  geom_violin() + facet_grid(League ~ .)


##143. リーグと年代で分割した配置をする：
ggplot(dat.tb, aes(x = bloodType, y = weight)) +
  geom_violin() + facet_grid(League ~ year_num)


##144. 年度別の平均体重を折れ線グラフで表示する：

dat.tb %>% group_by(year_num) %>%
  summarise(avg_weight = mean(weight)) %>%
  ggplot(aes(x = year_num, y = avg_weight)) + geom_line()


#145. 折れ線グラフにポイントを追加する：

dat.tb %>% group_by(year_num) %>%
  summarise(avg_weight = mean(weight)) %>%
  ggplot(aes(x = year_num, y = avg_weight)) + geom_line() + geom_point()

#146. グラフのテーマを変更する：


dat.tb %>% group_by(year_num) %>%
summarise(avg_weight = mean(weight)) %>%
  ggplot(aes(x = year_num, y = avg_weight)) +
  geom_line() + geom_point() + theme_minimal()

#148. 横軸に年度、縦軸に総HR 数をとった棒グラフを作る：

dat.tb %>% group_by(year_num) %>%
  summarise(avg_weight = mean(weight)) %>%
  ggplot(aes(x = year_num, y = avg_weight)) +
  geom_line() + geom_point() + theme_light()


#149. 棒グラフを年度ごとに塗り分ける：
dat.tb %>% group_by(year_num) %>%
  summarise(total_HR = sum(HR, na.rm = TRUE)) %>%
  ggplot(aes(x = factor(year_num), y = total_HR, fill = factor(year_num))) +
  geom_col()


#151. 棒グラフの凡例を非表示にする：
dat.tb %>% group_by(year_num) %>%
  summarise(total_HR = sum(HR, na.rm = TRUE)) %>%
  ggplot(aes(x = factor(year_num), y = total_HR, fill = factor(year_num))) +
  geom_col() + theme(legend.position = "none")

#152. 棒グラフの色の塗り分けパターンを変更する：
dat.tb %>% group_by(year_num) %>%
  summarise(total_HR = sum(HR, na.rm = TRUE)) %>%
  ggplot(aes(x = factor(year_num), y = total_HR, fill = factor(year_num))) +
  geom_col() + scale_fill_brewer(palette = "Set1") +
  theme(legend.position = "none")



RColorBrewer::display.brewer.all()

#154. グレースケールで塗り分ける：
dat.tb %>% group_by(year_num) %>%
  summarise(total_HR = sum(HR, na.rm = TRUE)) %>%
  ggplot(aes(x = factor(year_num), y = total_HR, fill = factor(year_num))) +
  geom_col() + scale_fill_grey() +
  theme(legend.position = "none")


#155. テーマを白黒基調に変更する：
dat.tb %>% group_by(year_num) %>%
  summarise(total_HR = sum(HR, na.rm = TRUE)) %>%
  ggplot(aes(x = factor(year_num), y = total_HR, fill = factor(year_num))) +
  geom_col() + scale_fill_brewer(palette = "Blues") +
  theme_bw() + theme(legend.position = "none")


ggsave("yearly_hr.png", plot = g, width = 8, height = 6, dpi = 300)


getwd()









#156. 作成したグラフをオブジェクトに代入する：


g <- dat.tb %>% group_by(year_num) %>%
  summarise(total_HR = sum(HR, na.rm = TRUE)) %>%
  ggplot(aes(x = factor(year_num), y = total_HR, fill = factor(year_num))) +
  geom_col() + scale_fill_brewer(palette = "Blues") +
  theme_bw() + theme(legend.position = "none") +
  labs(title = "年度別HR 総数", x = "年度", y = "総HR 数")

#157. 作成したグラフオブジェクトを表示する：  を実行する
print(g)

#158. グラフをファイルに保存する：
ggsave("yearly_hr.png", plot = g, width = 8, height = 6, dpi = 300)


dat.tb %>% group_by(year_num) %>%
  summarise(total_HR = sum(HR, na.rm = TRUE)) %>%
  ggplot(aes(x = year_num, y = total_HR)) +
  geom_line() + geom_point() +
  labs(title = "年間HR 総数の推移", x = "年度", y = "総HR 数")

#162. チームごとの年間HR 数の推移を折れ線グラフで表示する：
dat.tb %>% group_by(year_num, team) %>%
  summarise(total_HR = sum(HR, na.rm = TRUE), .groups = "drop") %>%
  ggplot(aes(x = year_num, y = total_HR, color = team)) +
  geom_line() + geom_point()


#163. 折れ線グラフの線の太さとポイントのサイズを調整する：
dat.tb %>% group_by(year_num, team) %>%
  summarise(total_HR = sum(HR, na.rm = TRUE), .groups = "drop") %>%
  ggplot(aes(x = year_num, y = total_HR, color = team)) +
  geom_line(linewidth = 1) + geom_point(size = 3)





#164. チームが多くて見づらいので、特定のチームだけ抽出する：



dat.tb %>% filter(team %in% c("Giants", "Tigers", "Carp")) %>%
  group_by(year_num, team) %>%
  summarise(total_HR = sum(HR, na.rm = TRUE), .groups = "drop") %>%
ggplot(aes(x = year_num, y = total_HR, color = team)) +
  geom_line(linewidth = 1) + geom_point(size = 3)

#165. 特定の選手のHR 数の推移を表示する：
dat.tb %>% filter(Name == "鈴木誠也") %>%
  ggplot(aes(x = year_num, y = HR)) +
  geom_line() + geom_point() +
  labs(title = "鈴木誠也選手のHR 数推移")


#166. データから複数の変数を選んで、組み合わせの相関を可視化する：
install.packages("GGally")
library(GGally)
GGally::ggpairs(dat.tb %>% select(height, weight, HR, salary))


dat.tb %>% select(League, height, weight, HR, salary) %>%
  GGally::ggpairs(mapping = aes(color = League))
install.packages("summarytools")
library(summarytools)
#168. summarytools パッケージで記述統計の詳細レポートを作成する：
summarytools::dfSummary(dat.tb) %>% summarytools::view()

#169. ggplot2 の拡張パッケージを使って3D 散布図を作成する：

install.packages("plotly")
library(plotly)
plotly::plot_ly(dat.tb, x = ~height, y = ~weight, z = ~salary,
                color = ~League, type = "scatter3d", mode = "markers")

#171. 変数Year_num でグループ化し、nest 関数でデータをまとめる：
nested_data <- dat.tb %>% group_by(year_num) %>% nest()
nested_data

#173. ネストされたデータの最初の要素を確認する：
nested_data$data[[1]]
library(purrr)
#各年度のデータサイズを確認する：174

nested_data %>% mutate(data_size = map_int(data, nrow))


#175. 各年度の平均給与を計算する：
nested_data %>% mutate(avg_salary = map_dbl(data, ~ mean(.$salary)))

#177. 年度ごとに身長と体重の相関係数を算出する：

nested_data %>%
  mutate(correlation = map_dbl(data, ~ cor(.$height, .$weight)))


#178. 年度ごとに身長を体重で回帰した分析を変数として追加する：

nested_data <- nested_data %>%
  mutate(model = map(data, ~ lm(height ~ weight, data = .)))
nested_data


#179. 回帰モデルから切片と係数を抽出する：
nested_data %>%
  mutate(coef = map(model, ~ coef(.))) %>%
  mutate(intercept = map_dbl(coef, ~ .[1]),
         slope = map_dbl(coef, ~ .[2]))


#180. 年度ごとの回帰モデルの要約統計量を取得する：

nested_data %>%
  mutate(model_summary = map(model, ~ summary(.))) %>%
  mutate(r_squared = map_dbl(model_summary, ~ .$r.squared))

#181. チームごとに最も年俸の高い選手を特定する：

top_players <- dat.tb %>%
  group_by(team) %>%
  filter(salary == max(salary)) %>%
  select(year_num, team, Name, salary)

#182. 2 つのデータセットを作成し、結合する準備をする：

team_data <- dat.tb %>%
  group_by(team) %>%
  summarise(avg_salary = mean(salary),
            avg_height = mean(height),
            avg_weight = mean(weight))

#183. inner_join を使ってデータを結合する：

team_data %>% inner_join(top_players, by = "team")



#184. left_join を使ってデータを結合する：
team_data %>% left_join(top_players, by = "team")

#185. full_join を使ってデータを結合する：

team_data %>% full_join(top_players, by = "team")

#186. anti_join を使用して、マッチしないレコードを見つける：
team_data %>% anti_join(top_players, by = "team")

#187. 複数のキーを使ってデータを結合する：

yearly_team_avg <- dat.tb %>%
  group_by(year_num, team) %>%
  summarise(avg_salary = mean(salary), .groups = "drop")


yearly_team_avg %>% inner_join(top_players, by = c("year_num", "team"))

#188. bind_rows を使って2 つのデータフレームを縦に結合する：

df1 <- dat.tb %>% filter(year_num == 2015) %>% select(Name, team, salary)
df2 <- dat.tb %>% filter(year_num == 2020) %>% select(Name, team, salary)
bind_rows(df1, df2)

#189. bind_rows で結合する際に識別子を追加する：

bind_rows("2015" = df1, "2020" = df2, .id = "year")

#191. 詳細な記述統計レポートを生成する：

if(!require(psych)) install.packages("psych")
psych::describe(dat.tb %>% select(height, weight, salary, HR))

#192. 変数間の相関を計算して視覚化する：
install.packages("corrplot")
library(corrplot)
cor_matrix <- cor(dat.tb %>% select(height, weight, salary, HR), use = "complete.obs")
corrplot::corrplot(cor_matrix, method = "circle")

#193. 主成分分析（PCA）を実行する：

pca_result <- prcomp(dat.tb %>% select(height, weight, salary, HR) %>% na.omit,
                     scale. = TRUE, center = TRUE)
summary(pca_result)

install.packages("factoextra")
library(factoextra)
factoextra::fviz_pca_biplot(pca_result)

#195. クラスタリング分析を実行する：

scaled_data <- scale(dat.tb %>% select(height, weight))
kmeans_result <- kmeans(scaled_data, centers = 3)
dat.tb$cluster <- as.factor(kmeans_result$cluster)

#196. クラスタリング結果を散布図で可視化する：

ggplot(dat.tb, aes(x = height, y = weight, color = cluster)) +
  geom_point() +
  labs(title = "KMeans クラスタリング結果") 

#197. インタラクティブなデータテーブルを作成する：
if(!require(DT)) install.packages("DT")
DT::datatable(dat.tb %>% select(Name, team, position, height, weight, salary))

#198. 年度別の総HR 数の推移をアニメーションで表示する：


if(!require(gganimate)) install.packages("gganimate")
library(gganimate)
hr_by_year <- dat.tb %>%
  group_by(year_num, team) %>%
  summarise(total_HR = sum(HR, na.rm = TRUE), .groups = "drop")
anim <- ggplot(hr_by_year, aes(x = team, y = total_HR, fill = team)) +
  geom_col() +
  labs(title = "Year: {frame_time}") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
  transition_time(year_num)
animate(anim, nframes = 100)



install.packages("ggplot2")
library(ggplot2)
