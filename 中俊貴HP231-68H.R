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













