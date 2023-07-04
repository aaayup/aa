df <- read.csv("data_0625.csv", fileEncoding="euc-kr")
str(df)
summary(df)
library(dplyr)

#윤달 제거
df_NA <- df %>% filter(year== 'D' & mmddhh>=22900 & mmddhh <30100)
df_copy <- df %>% filter(!(year== 'D' & mmddhh>=22900 & mmddhh <30100))
str(df_copy) 
nrow(df)-nrow(df_copy)
str(df_copy)
summary(df_copy)

#일평균, 최소최대 변수 생성
daily_stats <- df_copy %>% group_by(date = as.Date(ymdh)) %>% 
                                summarize(mean_ta = mean(ta, na.rm = TRUE),
                                                min_ta = min(ta, na.rm = TRUE),
                                                        max_ta = max(ta, na.rm = TRUE))


str(df_copy$ymdh)
head(daily_stats)


#월별 최저최고평균 그래프
##################

# Set the plot size


# Create the plot with multiple lines
plot(daily_stats$date, daily_stats$max_ta, type = "l", col = "blue",
     ylim=c(-25, 40),
     xlab = "Duration", ylab = "Temperature",
     main = "기간에 따른 평균기온(Max, Mean, Min)",
     xaxt = "n")

lines(daily_stats$date, daily_stats$mean_ta, type = "l", col = "green")

lines(daily_stats$date, daily_stats$min_ta, type = "l", col = "red")


# Customize x-axis labels
axis(1, at = seq(min(daily_stats$date), max(daily_stats$date), by = "5 months"),
     labels = format(seq(min(daily_stats$date), max(daily_stats$date), by = "5 months"), "%Y-%m"))


legend("bottomleft", legend = c("Max", "Mean", "Min"),
       col = c("blue", "green", "red"), lty =1, cex = 0.4, lwd=2,)


#Q1, Q3
quantile(daily_stats$mean_ta)[c(2,4)]

#3등분 점선
abline(h=c(5.91, 22.24), lty=2, lwd=2.5)



#3등분으로 데이터나누기
str(daily_stats)
daily_stats_c <- daily_stats
a <- daily_stats_c %>% filter(mean_ta < 5.910833) 
str(a)
b <- filter(daily_stats_c, mean_ta >= 5.910833 & mean_ta <= 22.239167)
str(b)
c <- daily_stats_c %>% filter(mean_ta>22.239167) 
str(c)

str(df_copy)
 
 
 # mean_ta < Q1, df

cat(dim(df_copy[df$ymd %in% a$date, ]), ' rows\n')
head(df_copy[df_copy$ymd %in% a$date, ]$ymd)
df_a <- df_copy[df_copy$ymd %in% a$date, ]
head(df_a)

# Q1 <= mean_ta <= Q3, df
cat(dim(df_copy[df_copy$ymd %in% b$date, ]), ' rows\n')
head(df_copy[df_copy$ymd %in% b$date, ]$ymd)
df_b <- df_copy[df_copy$ymd %in% b$date, ]

  # mean_ta > Q3, df
cat(dim(df_copy[df_copy$ymd %in% c$date, ]), ' rows\n')
head(df_copy[df_copy$ymd %in% c$date, ]$ymd)
df_c <- df_copy[df_copy$ymd %in% c$date, ]
head(df_c)


#결측치 대체
#day_split 변수 하루 나누기(새벽~밤)
str(df_copy)
df_copy$day_split <- ifelse( df_copy$hour >=0 & df_copy$hour<=5, '새벽',
                           ifelse(df_copy$hour >=6 & df_copy$hour<=11, '아침',
                                  ifelse(df_copy$hour >=12 & df_copy$hour <=16, '낮',
                                         ifelse( df_copy$hour >=17 & df_copy$hour <=20, '저녁','밤' ))))
table(df_copy$day_split)
head(df_copy)


#hm 결측치
df %>% filter(is.na(hm))

#ta, hm 결측치 채우기 (td 채우기 전)
#평균기온 채우는 방법 : month, day, day_split 변수를 이용하여 imputation
#상대습도(hm) : 위와 동일
#이슬점온도(td) : imputation 후, 평균기온과 상대습도를 이용하여 계산
df_copy$ta <- na.locf(df_copy$ta, fromLast=F)
summary(df_copy)
df_copy$hm <- na.locf(df_copy$hm, fromLast=F)

#td 함수로 채우기
dew_point_temperature <- function(ta, hm) {
  
  b <- 17.62
  c <- 243.12
  
  alpha <- c*(log(hm/100)+b*ta/(c+ta))
  beta <- (b-log(abs(ta)/100)+b*ta/(c+ta))
  dew_point <- alpha/beta
  
  return(dew_point)
}

df_copy$td <- ifelse(is.na(df_copy$td), dew_point_temperature(df_copy$ta, df_copy$hm), df_copy$td)
summary(df_copy)


#1시간 누적 강수량(rn), 1시간 누적 강수 유무(re)
      #re 구간별 변수 만들기 : ex) 5-10, 10-15, 15-20, re interval
      #조건(rn=0 | NA)인 경우 re 결측치 0으로 채우기
#나머지 rn 결측치는  re interval 평균값 보고 결정
#re 결측치는 rn이 0인 경우 0으로 채우기
#나머지 결측치는 re interval group by 평균으로 채우거나 그대로 두기
df <- read.csv("data_0701.csv")
summary(df)


#re -> re_interval (0~4=1, 5~9=2, 10~14=3,  ..., 54~60=12)
for (i in seq(1, 59, by = 5)) { 
    df$re_interval[df$re %in% (i:(i+4))] <- i %/% 5
}

seq(1, 59, by = 5)
df$re_interval[df$re %in% 60] <-  12
df$re_interval[df$re %in% 0] <-  0

table(df$re_interval)
summary(df)
library(dplyr)
df %>% group_by(re_interval) %>% summarise(min=min(re, na.rm=T), max=max(re, na.rm=T))
df %>% filter(rn==0 & !re==0)%>% select('rn', 're')  #멘토링 문의
summary(df)


#조건(rn=0 | NA)인 경우 re 결측치 0으로 채우기
nrow(df %>% filter(is.na(rn) | rn == 0))
df_copy <- df %>% mutate(re = ifelse(rn == 0 | is.na(rn), 0, re))

summary(df_copy$re)
summary(df$re)
1573-82 #만큼 0으로 대체

df_rena <- df %>% filter(is.na(rn) | rn == 0) %>% filter(is.na(re))
dim(df_rena)

summary(df_copy)


#나머지 rn 결측치는 re interval 평균값 보고 결정
df <- read.csv("data_0702.csv")
df <- df[,-c(1,2)]
summary(df)
summary(df$rn)  #7719개 
summary(df$re)  #82개 

for (i in seq(1, 59, by = 5)) { 
  df$re_interval[df$re %in% (i:(i+4))] <- i %/% 5+1
}
df$re_interval[df$re %in% 60] <-  12
df$re_interval[df$re %in% 0] <-  0

str(df)
library(dplyr)
summary(df$re_interval)
summary(df$re_interval)

df_mean_rn <- df %>% filter(!is.na(rn) & !rn==0) %>% group_by(re_interval) %>% summarise(n=n(), mean_rn = mean(rn, na.rm=T)) #채택
str(df_mean_rn)
df_copy <- df

df <- read.csv('re_interval별 rn평균.csv')
str(df)
library(dplyr)

#mean_rn이용해서 re_interval 채우기
df_copy <- df %>% mutate(re_interval = case_when(
    is.na(re_interval) & rn > 0.156 & rn < 0.223 ~ 1,
    is.na(re_interval) & rn >= 0.223 & rn < 0.329 ~ 2,
    is.na(re_interval) & rn >= 0.329 & rn < 0.447 ~ 3,
    is.na(re_interval) & rn >= 0.447 & rn < 0.554 ~ 4,
    is.na(re_interval) & rn >= 0.554 & rn < 0.725 ~ 5,
    is.na(re_interval) & rn >= 0.725 & rn < 0.933 ~ 6,
    is.na(re_interval) & rn >= 0.933 & rn < 1.12 ~ 7,
    is.na(re_interval) & rn >= 1.12 & rn < 1.44 ~ 8,
    is.na(re_interval) & rn >= 1.44 & rn < 1.68 ~ 9,
    is.na(re_interval) & rn >= 1.68 & rn <2.01 ~ 10,
    is.na(re_interval) & rn >= 2.01 & rn <2.30 ~ 11,
    is.na(re_interval) ~ 12,
    TRUE ~ re_interval
  ))

  #성공
df[4850:4900, c('re','re_interval', 'rn')]
summary(df)
summary(df_copy)
df_copy %>% filter(is.na(re)) %>% select('re', 'rn', 're_interval')

#re==0 -> na.rn==0 
df <- read.csv('data_0703.csv')
str(df)

summary(df$rn)
df[4850:4900, c('re','re_interval', 'rn')]
library(dplyr)
df <- df %>% mutate(rn = ifelse(re == 0, 0, rn))

df[4850:4900, c('re','re_interval', 'rn')]
summary(df)


#ws결측치 -> 그룹 바이 (month, stn, day)의 ws 평균으로 채우기
df %>% groupy_by(month, stn, day) %>% summarise(mean_ws=mean)



#test 데이터셋 결측치(-99.9, -99) train 데이터셋으로 채우기
df <- read.csv("test_0629_ymd.csv")
str(df)
summary(df)
df_copy <- df
  #day_split 변수 생성
df_copy$day_split <- ifelse( df_copy$hour >=0 & df_copy$hour<=5, '새벽',
                             ifelse(df_copy$hour >=6 & df_copy$hour<=11, '아침',
                                    ifelse(df_copy$hour >=12 & df_copy$hour <=16, '낮',
                                           ifelse( df_copy$hour >=17 & df_copy$hour <=20, '저녁','밤' ))))

#re -> re_interval (0~4=1, 5~9=2, 10~14=3,  ..., 54~60=12)
for (i in seq(1, 59, by = 5)) { 
  df_copy$re_interval[df_copy$re %in% (i:(i+4))] <- i %/% 5
}

seq(1, 59, by = 5)
df_copy$re_interval[df$re %in% 60] <-  12
df_copy$re_interval[df$re %in% 0] <-  0

table(df_copy$re_interval)
str(df_copy)

#조건(rn=0 | NA)인 경우 re 결측치 0으로 채우기
nrow(df_copy %>% filter(is.na(rn) | rn == 0))
df_copy <- df_copy %>% mutate(re = ifelse(rn == 0 | is.na(rn), 0, re))

summary(df_copy)
#mean_rn이용해서 re_interval 채우기
df_copy <- df_copy %>% mutate(re_interval = case_when(
  is.na(re_interval) & rn > 0.156 & rn < 0.223 ~ 1,
  is.na(re_interval) & rn >= 0.223 & rn < 0.329 ~ 2,
  is.na(re_interval) & rn >= 0.329 & rn < 0.447 ~ 3,
  is.na(re_interval) & rn >= 0.447 & rn < 0.554 ~ 4,
  is.na(re_interval) & rn >= 0.554 & rn < 0.725 ~ 5,
  is.na(re_interval) & rn >= 0.725 & rn < 0.933 ~ 6,
  is.na(re_interval) & rn >= 0.933 & rn < 1.12 ~ 7,
  is.na(re_interval) & rn >= 1.12 & rn < 1.44 ~ 8,
  is.na(re_interval) & rn >= 1.44 & rn < 1.68 ~ 9,
  is.na(re_interval) & rn >= 1.68 & rn <2.01 ~ 10,
  is.na(re_interval) & rn >= 2.01 & rn <2.30 ~ 11,
  is.na(re_interval) ~ 12,
  TRUE ~ re_interval
))

summary(df_copy)

#re==0 -> na.rn==0 
df_copy <- df_copy %>% mutate(rn = ifelse(re == 0, 0, rn))

#ws결측치 -> 그룹 바이 (month, stn, day)의 ws 평균으로 채우기 
df <- read.csv('train_0704.csv')
summary(df)
library(dplyr)
df_mean_ws <- df %>% group_by(stn, month, day) %>% summarise(mean_ws=mean(ws, na.rm=T), .groups = 'drop')
str(df_mean_ws)
df_join <- left_join(df, df_mean_ws, by = c("stn", "month", "day"))
summary(df_join)
df <- df_join

  #확인
summary(df$ws)
summary(df_join$ws)
df[is.na(df$ws),]


df$ws <- ifelse(is.na(df$ws), df$mean_ws, df$ws)
df_join[22821:22836,]

df <- df_join
summary(df)


#inflection 추가
df$inflection <- ifelse(df$mmddhh>=20100 & df$mmddhh < 80200, 'up', 'down')

summary(df)

write.csv(df, file='train_07042.csv')

#write.csv(df_train, file='train_0704f.csv')
#write.csv(df_join, file='test_0704f.csv')
#validation: 0.3, train: 0.7
#ww 변수 중요도에서 낮으면 지우기



df_train <- read.csv("train_0704f.csv")
df_test <- read.csv("test_0704f.csv")

#idx <- sample(1:nrow(df_train), size = nrow(df_train)*0.7, replace=FALSE)
#train <- df_train[idx,]
#test <- data[-idx,]

#ww 라벨인코딩, factor() 둘 다 해서 보기
str(df_test)
val <- df_train %>% filter(year=='F' | year=='E')
train <- df_train %>% filter(year!='F' & year!='E')

str(train)

model1 <- lm(ts ~ factor(stn) + year + ta + td + hm + ws + rn + re  + si + ss + sn + factor(day_split) + factor(inflection), data=train) 
model2 <- lm(ts ~ factor(stn) + year + ta + td + hm + ws + rn + re_interval  + si + ss + sn + factor(day_split) + factor(inflection), data=train) 

summary(model1)
summary(model2)

library(car)

vif(model1)
vif(model2)

predict(model1, val)
factor(df_test$year)
factor(df_train$year)
