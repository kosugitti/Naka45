#消費価値観（できるだけ長く使えるものを買う）


#SEN_01_MA
#消費価値観（とにかく安くて経済的なものを買う）


syohi<-main$SEN_01_MA
head(syohi)

syohi2<-main$SEN_16_MA 
head(syohi2)
syohi[syohi == ""] <- NA
syohi2[syohi2 == ""] <- NA
cross_table <- table(syohi, syohi2, useNA = "no")
chisq.test(cross_table)
library(dplyr)
cross_table <- table(syohi, syohi2)
cross_table
#φ係数（Phi coefficient）
phi <- sqrt(20.25 / sum(cross_table))
print(phi)
#オッズ比
or <- (407 * 928) / (311 * 810)  # (a*d)/(b*c)
print(or)
# 調整済み標準化残差の計算
residuals <- chisq.test(cross_table)$stdres
print(residuals)


mosaicplot(cross_table, 
           main = "継続性と経済性の関係",
           color = c("#FF9999", "#99CCFF"),
           shade = TRUE)
library(ggplot2)
prop_table <- prop.table(cross_table, margin = 1)  # 行方向の割合

ggplot(as.data.frame(prop_table), 
       aes(x = syohi, y = Freq, fill = syohi2)) +
  geom_bar(stat = "identity", position = "dodge") +
  labs(title = "syohi別のsyohi2割合", y = "割合") +
  scale_fill_brewer(palette = "Set1")


##クラスター分析

convenience<-main$CHANNEL_01_MX

drug<-main$CHANNEL_04_MX

convenience<-na.omit(convenience)


drug<-na.omit(drug)


df <- data.frame(
  convenience = convenience,
  drug = drug
)

raw_data <- data.frame(
  convenience = c("ほとんど毎日", "週に１回程度", "月に１～２回程度", "ほとんど利用していない"),
  drug = c("週に２～３回程度", "半年に１～２回程度", "年に１回程度", "ほとんど毎日")
)

# 頻度を数値に変換する関数
convert_frequency <- function(x) {
  case_when(
    x == "ほとんど毎日"       ~ 1,
    x == "週に２～３回程度"   ~ 2,
    x == "週に１回程度"       ~ 3,
    x == "月に１～２回程度"   ~ 4,
    x == "半年に１～２回程度" ~ 5,
    x == "年に１回程度"       ~ 6,
    x == "ほとんど利用していない" ~ 7,
    TRUE ~ NA_real_
  )
}
# 数値化適用
df <- raw_data %>%
  mutate(across(everything(), convert_frequency))
# 数値化適用
 df <- raw_data %>%
  +  mutate(across(everything(), convert_frequency))
 head(df)
str(df_clean)




# 欠損値処理
df_clean <- na.omit(df)

# 標準化（数値列のみ明示的に選択）
scaled_df <- scale(df_clean[, c("convenience", "drug")])

library(factoextra)

# エルボー法
fviz_nbclust(scaled_df, kmeans, method = "wss") +
  geom_vline(xintercept = 3, linetype = 2)

# シルエット法
fviz_nbclust(scaled_df, kmeans, method = "silhouette")

set.seed(123)
final_k <- 3  # 前ステップで決定
final_res <- kmeans(scaled_df, centers = final_k, nstart = 25)

# クラスター別平均値
cluster_means <- aggregate(df_clean, 
                           by = list(cluster = final_res$cluster), 
                           FUN = mean)

# ビジネスフレンドリーなラベル作成
cluster_labels <- c(
  "1" = "高コンビニ利用/低ドラッグ利用層",
  "2" = "バランス利用層",
  "3" = "低コンビニ利用/高ドラッグ利用層"
)

# デバイスリセット
if(!is.null(dev.list())) dev.off()

# 散布図
library(ggplot2)
ggplot(data.frame(scaled_df, 
                  Cluster = factor(final_res$cluster)),
       aes(x = convenience, y = drug, color = Cluster)) +
  geom_point(size = 3) +
  stat_ellipse() +
  scale_color_brewer(palette = "Set1", labels = cluster_labels) +
  labs(title = "顧客セグメンテーション",
       x = "コンビニ利用頻度（標準化）",
       y = "ドラッグストア利用頻度（標準化）")






action_plan <- data.frame(
  Cluster = 1:3,
  Segment = cluster_labels,
  Action = c(
    "コンビニポイント還元",
    "クロスチャネルプロモーション",
    "ドラッグストア特典"
  ),
  KPI = c(
    "コンビニリピート率+15%",
    "両チャネル利用維持",
    "ドラッグ客単価+10%"
  )
)

# 必ず実行
str(df_clean)
stopifnot(all(sapply(df_clean, is.numeric)))

# 基本プロットでテスト
plot(1:10) 

# クラスターの安定性チェック
library(cluster)
silhouette_score <- mean(silhouette(final_res$cluster, dist(scaled_df))[, 3])







# クラスター数の決定（エルボー法とシルエット法）
fviz_nbclust(scaled_df, kmeans, method = "wss")  # エルボー法
fviz_nbclust(scaled_df, kmeans, method = "silhouette")  # シルエット法


# k-meansクラスタリングの実行
final_res <- kmeans(scaled_df, centers = 3, nstart = 25)












SEN_01_MA_subset <- main$SEN_01_MA[1:33]
head(SEN_01_MA_subset )

library(psych)
install.packages("polycor")
library(polycor)
polychor(cheap,economy)
# データの切り出し（先頭33行）
SEN_01_MA_subset <- main$SEN_01_MA[1:33, ]
SEN_01_MA_subset<-na.omit(SEN_01_MA_subset)
# テトラポリック相関の計算
tetra_result <- tetrachoric(SEN_01_MA_subset)

# 相関行列を表示
print(tetra_result$rho)












# データフレームの作成（順序変数としてfactorに変換）
SEN_data <- data.frame(
  cheap = ordered(main$SEN_01_MA),
  economy = ordered(main$SEN_02_MA),
  quality = ordered(main$SEN_03_MA),
  bland = ordered(main$SEN_04_MA),
  design = ordered(main$SEN_05_MA),
  design2 = ordered(main$SEN_06_MA),
  famous = ordered(main$SEN_07_MA),
  conven = ordered(main$SEN_08_MA)
)

# ポリクリック相関行列の計算
polychoric_matrix <- hetcor(SEN_data, type = "polychoric")$correlations

# 結果の表示（小数点以下3桁に丸める）
print(round(polychoric_matrix, 3))




# シンプルなヒートマップ
image(1:ncol(polychoric_matrix), 1:nrow(polychoric_matrix),
      t(polychoric_matrix[nrow(polychoric_matrix):1,]), 
      col = colorRampPalette(c("blue", "white", "red"))(100),
      axes = FALSE, xlab = "", ylab = "", main = "Polychoric Correlation Matrix")
axis(1, at = 1:ncol(polychoric_matrix), labels = colnames(polychoric_matrix), las = 2)
axis(2, at = 1:nrow(polychoric_matrix), labels = rev(rownames(polychoric_matrix)), las = 1)

install.packages("reshape2")
library(reshape2)




melted_cormat <- melt(polychoric_matrix)

# ggplot2でヒートマップを作成
ggplot(data = melted_cormat, aes(x = Var1, y = Var2, fill = value)) +
  geom_tile(color = "white") +
  scale_fill_gradient2(low = "blue", high = "red", mid = "white", 
                       midpoint = 0, limit = c(-1,1), space = "Lab",
                       name="Polychoric\nCorrelation") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, vjust = 1, hjust = 1)) +
  coord_fixed() +
  ggtitle("Polychoric Correlation Matrix of SEN Data") +
  geom_text(aes(label = round(value, 2)), color = "black", size = 3)


