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
hoge3
##ベースボール
library("tidyverse")
dat<-read.csv("BaseballDecade.csv")
head(dat)
tail(dat)
dim(dat)
names(dat)
colnames(dat)
summary(dat)
str(dat)
class(dat)
dat.tb<-dat
dat.tb

dat.tb$Name
table(dat.tb$Name)
dat.tb$Name %>% table() %>% sort(decreasing = TRUE)
dat.tb$team %>% unique()
dat.tb$team %>% unique() %>% length()
dat.tb$team <-dat.tb$team %>% as.factor()
dat.tb$bloodType <-dat.tb$bloodType<-dat.tb$bloodType %>% as.factor()
dat.tb$bloodType
dat.tb$position<-dat.tb$position %>% as.factor()
dat.tb %>% select(team,bloodType,position) %>% summary()
dat.tb$height %>% mean()
dat.tb$height %>% var()
dat.tb$salary %>% quantile()
dat.tb$salary %>% quantile(probs=0,0.25,0.33,0.95,1)
dat.tb<-dat.tb %>% mutate(bmi_category=ifelse(bim>=25,"HighBMI","Standard"))
install.packages("dplyr")
library("dplyr")
dat.tb <- dat.tb %>% mutate(bmi = weight / ((height/100)^2)) 
dat.tb$bmi_category %>% table()

dat.tb <- dat.tb %>%
  mutate(position2 = case_when(position == "投手" ~ "投手", TRUE ~ "野手"))
  
  

# dat.tb を data.frame に変換
dat.tb <- as.data.frame(dat.tb)

# その後、mutate 関数を適用
dat.tb <- dat.tb %>% mutate(bmi = weight / ((height/100)^2))


dat.tb <- as_tibble(dat.tb)

# その後、mutate 関数を適用
dat.tb <- dat.tb %>% mutate(bmi = weight / ((height/100)^2))

  
  
  
  
  
  
  
  
  
  
  