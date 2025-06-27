install.packages("pacman")
library(tidyverse)
pacman::p_load(summarytools,tidverse)
source("")
dfSummary(main) %>% stview()
vimain %>% names()
LABEL<-vimain$ラベル
dfm<-dfSummary(main)
dfm$Variable<-vimain$ラベル
dfm %>% stview

main %>% 
  as_tibble %>% 
  select(4,12,13) %>% 
  group_by(MARRIAGE,FAMILY_CD) %>% 
  summarise(n=n()) %>% 
  arrange(-n)
main %>% 
  select(438,493,588) %>% 
  mutate(across(2:17,~as.numeric((.)-1) %>% 
                  mutate(radio=rowSums{select(.,3:17),na.rm=TURE) %>% 
                  select(1,radio) %>% 
                  tible()
 main %>%
                  select(430, 492, 493:508) %>%
                  mutate(across(3:17, ~as.numeric(.) - 1)) %>%
                  mutate(radio = rowSums(select(., 3:17), na.rm = TRUE)) %>%
                           select(1, RADIO_TIME) %>%
  group_by(HOB_05_MA,RADIO_TIME) %>% 
   summarise(n=n(),.groups = "drop") %>% 
   ggplot(aes(x=RADIO_TIME,y=n,fill=HOB_05_MA))
 geom_col(position = "dodge")+
   facet_wrap(~HOB_05_MA,scales="Free_y")
                   
 
 
 
 
 
 main %>%
   select(430, 492, 493:508) %>%  # 必要な列を選択
   mutate(across(3:18, ~as.numeric(.) - 1)) %>%  # 3-18列目を数値化し1を引く
   mutate(RADIO_TIME = rowSums(select(., 3:18), na.rm = TRUE)) %>%  # 3-18列目の合計を計算
   select(1, RADIO_TIME, HOB_05_MA) %>%  # 必要な列のみ選択（HOB_05_MAも含める）
   group_by(HOB_05_MA, RADIO_TIME) %>%  # HOB_05_MAとRADIO_TIMEでグループ化
   summarise(n = n(), .groups = "drop") %>%  # カウントを計算
   ggplot(aes(x = RADIO_TIME, y = n, fill = HOB_05_MA)) +  # ggplotで描画
   geom_col(position = "dodge") +  # ドッジ型の棒グラフ
   facet_wrap(~HOB_05_MA, scales = "free_y") +  # HOB_05_MAごとにファセット、y軸スケール自由
   labs(x = "Radio Time", y = "Count", fill = "HOB_05_MA") +  # ラベル設定
   theme_minimal()  # テーマをミニマルに

              
                main
              
 
##購買意欲の変化
vimain %>% 
  filter(str_detect(ラベル,"\\d{2}/\\d{2}")) %>% 
select(ラベル,変数名) %>% 
  mutate(ItemID=str_sub(変数名,7,16),
date=str_extract(ラベル,"\\d{2}/\\d{2}"),
type=str_sub(変数名,1,2),
product=str_extract(ラベル,"^[^(]+")
) %>% 
  select(ItemID,date,type,product,変数名)

main %>% 
  select(SampleID,selected_vars$変数名) %>% 
  mutate(across(-SampleID,as.numeric)) %>% 
  pivot_longer(-SampleID) %>% 
  right_join(selected_vars,by=c("name"="変数名")) %>% 
  select(-name) %>% pull(data) %>% 
  group_by(type) %>% 
  pivot_wider()





vimain %>%
  filter(str_detect(ラベル, "\d{2}/\d{2}")) %>%
  select(ラベル, 変数名) %>%
  mutate(
    ItemID = str_sub(変数名, 7, 16),
    date = str_extract(ラベル, "\d{2}/\d{2}"),
    type = str_sub(変数名, 1, 2),
    product = str_extract(ラベル, "^[^（]+")
  ) %>%
  select(ItemID, date, type, product, 変数名)


selected_vars <- vimain %>% 
  filter(str_detect(ラベル, "\\d{2}/\\d{2}")) %>% 
  select(ラベル, 変数名) %>% 
  mutate(
    ItemID = str_sub(変数名, 7, 16),
    date = str_extract(ラベル, "\\d{2}/\\d{2}"),
    type = str_sub(変数名, 1, 2),
    product = str_extract(ラベル, "^[^（]+")
  ) %>% 
  select(ItemID, date, type, product, 変数名)

result <- main %>%
  select(SampleID, selected_vars$変数名) %>%
  mutate(across(-SampleID, as.numeric)) %>%
  pivot_longer(-SampleID) %>%
  right_join(selected_vars, by = c("name" = "変数名")) %>%
  select(-name) %>%
  mutate(date = lubridate::ymd(paste0("2024/", date))) %>%
  filter(type == "PS") %>% 
  group_by(ItemID) %>%
  mutate(
    period = case_when(
      date == min(date) ~ "pre",
      date == max(date) ~ "post",
      TRUE ~ NA_character_
    )
  ) %>% 
  ungroup() %>% 
  unite("type_period", type, period, sep = "") %>%
  pivot_wider(
    id_cols = c(SampleID, ItemID, product),
    names_from = type_period,
    values_from = value
  ) %>% 
  mutate(diff = PSpre - PSpost) %>% 
  group_by(product) %>% 
  summarise(
    diff_mean = mean(diff, na.rm = TRUE),
    diff_sd = sd(diff, na.rm = TRUE)
  ) %>% 
  arrange(-diff_mean, diff_sd)

# 結果の表示
print(result)



result <- main %>%
  select(SampleID, selected_vars$変数名) %>%
  mutate(across(-SampleID, as.numeric)) %>%
  pivot_longer(-SampleID) %>%
  right_join(selected_vars, by = c("name" = "変数名")) %>%
  select(-name) %>%
  mutate(date = lubridate::ymd(paste0("2024/", date))) %>%
  filter(type == "PS") %>% 
  group_by(ItemID) %>%
  mutate(
    period = case_when(
      date == min(date) ~ "pre",
      date == max(date) ~ "post",
      TRUE ~ NA_character_
    )
  ) %>% 
  ungroup() %>%
  
  # ここを修正: unite() の結果を確認
  unite("type_period", type, period, sep = "_", remove = FALSE) %>%  # sep="_"で明確に区切り、remove=FALSEで元の列を保持
  # type_period列が作成されたか確認
  # select(type, period, type_period) %>% head()  # 確認用
  
  pivot_wider(
    id_cols = c(SampleID, ItemID, product),
    names_from = type_period,  # ここで使用
    values_from = value
  ) %>% 
  mutate(diff = PS_pre - PS_post) %>%  # 列名がPS_pre, PS_postになるように注意
  group_by(product) %>% 
  summarise(
    diff_mean = mean(diff, na.rm = TRUE),
    diff_sd = sd(diff, na.rm = TRUE)
  ) %>% 
  arrange(-diff_mean, diff_sd)


















  