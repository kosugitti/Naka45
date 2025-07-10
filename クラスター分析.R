
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
df <- raw_data %>%
  mutate(across(everything(), convert_frequency))
# 数値化適用

df <- raw_data %>%
    mutate(across(everything(), convert_frequency))
 head(df)
str(df_clean)

# 数値列の有無を確認
sapply(df_clean, is.numeric)

numeric_cols <- c("convenience", "drug")  # 実際の列名に変更
scaled_df <- scale(df_clean[, numeric_cols])
 numeric_cols

 scaled_df
 
 if (nrow(scaled_df) < 1) stop("有効なデータがありません")
  # エルボー法実行
   wss <- sapply(1:min(10, nrow(scaled_df)-1), function(k) {  # 行数-1までを上限
       kmeans(scaled_df, k, nstart = 25)$tot.withinss
      })
   plot(seq_along(wss), wss, type = "b", xlab = "クラスター数k")
   df_clean <- data.frame(
     convenience = c(1, 3, 5, 2, 4),  # 数値列
     drug = c(2, 4, 1, 5, 3)          # 数値列
   )
   scaled_df <- scale(df_clean)  #
   df_clean 
   
str(scaled_df)
 
print(paste("数値列数:", sum(sapply(scaled_df, is.numeric))))

 head(scaled_df, 5)
 
 if (nrow(scaled_df) <= 1) {
    stop("分析には2行以上のデータが必要です")
    }
 max_k <- min(10, nrow(scaled_df) - 1)
 
 if (max_k >= 2) {
     avg_sil <- sapply(2:max_k, function(k) {
        km.res <- kmeans(scaled_df, centers = k, nstart = 25)
         ss <- silhouette(km.res$cluster, dist(scaled_df))
         mean(ss[, 3])
       })
     df <- data.frame(
         convenience = sample(1:7, 100, replace = TRUE),  # 1-7の利用頻度
         drug = sample(1:7, 100, replace = TRUE)
        )
 
      df_clean <- na.omit(df)
     
        # 3. 標準化（数値列のみ）
        scaled_df <- scale(df_clean[, sapply(df_clean, is.numeric)])
     
      # 4. シルエット分析（安全版）
        max_k <- min(10, nrow(scaled_df) - 1)
      if (max_k >= 2) {
         avg_sil <- sapply(2:max_k, function(k) {
             set.seed(123)
         +   km.res <- kmeans(scaled_df, k, nstart = 25)
             mean(silhouette(km.res$cluster, dist(scaled_df))[, 3])
           })
        plot(2:max_k, avg_sil, type = "b")
        } else {
           print(paste("有効データ数不足:", nrow(scaled_df)))
    
          
          
          
          
          
          set.seed(123)
          df <- data.frame(
            convenience = sample(1:7, 300, replace = TRUE),  # 1-7の利用頻度尺度
            drug = sample(1:7, 300, replace = TRUE)
          )
          
          # 欠損値処理
          df_clean <- na.omit(df)
          
          # 標準化（数値列のみ明示的に選択）
          scaled_df <- scale(df_clean[, c("convenience", "drug")])          
          
          fviz_nbclust(scaled_df, kmeans, method = "wss") +
            geom_vline(xintercept = 3, linetype = 2)
          
          # シルエット法
          fviz_nbclust(scaled_df, kmeans, method = "silhouette")
          
          
          

          
          # エルボー法
          fviz_nbclust(scaled_df, kmeans, method = "wss") +
            geom_vline(xintercept = 3, linetype = 2)
          
          # シルエット法
          fviz_nbclust(scaled_df, kmeans, method = "silhouette")
          
          while (!is.null(dev.list())) dev.off()
          
          # 新しいデバイスを開く
          graphics.off()
          grDevices::windows()  # Windowsの場合
          # Macの場合は grDevices::quartz()      
          
          wss <- sapply(1:10, function(k){kmeans(scaled_df, k, nstart=25)$tot.withinss})
          plot(1:10, wss, type="b", xlab="クラスター数k", ylab="WSS")
          abline(v=3, lty=2)          
          
          if(!is.null(dev.list())) dev.off()         
          
          set.seed(123)
          final_k <- 3  # 前ステップで決定
          final_res <- kmeans(scaled_df, centers = final_k, nstart = 25)
          
          cluster_means <- aggregate(df_clean, 
                                     by = list(cluster = final_res$cluster), 
                                     FUN = mean)
          
          # ビジネスフレンドリーなラベル作成
          cluster_labels <- c(
            "1" = "高コンビニ利用/低ドラッグ利用層",
            "2" = "バランス利用層",
            "3" = "低コンビニ利用/高ドラッグ利用層"
          )
          
          
          
          ggplot(data.frame(scaled_df, 
                            Cluster = factor(final_res$cluster)),
                 aes(x = convenience, y = drug, color = Cluster)) +
            geom_point(size = 3) +
            stat_ellipse() +
            scale_color_brewer(palette = "Set1", labels = cluster_labels) +
            labs(title = "顧客セグメンテーション",
                 x = "コンビニ利用頻度（標準化）",
                 y = "ドラッグストア利用頻度（標準化）")  
          
          
          # クラスター別統計量
          library(dplyr)
          df_clean %>%
            mutate(Cluster = final_res$cluster) %>%
            group_by(Cluster) %>%
            summarise(
              n = n(),
              across(everything(), list(mean = mean, sd = sd))
              
          
          
              ction_plan <- data.frame(
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
              
              
              
              
              
              
              
              
              
              
              
              
              
              
              
              
              
              
              
              
              
              
              
                         
              
              
              
              
              
              
              
              
     
     
     
     
     
     
     
     
     
     
     
     
     
     
     
     
     
     
         
   
 
 
 
 
 
 
 
 
 
 
 
 
 
 
 
 
 
 
 
 
 
 
 













