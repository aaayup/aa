#test 데이터셋 결측치(-99.9, -99) train 데이터셋으로 채우기
df <- read.csv("test_0629_ymd.csv")
df <- read.csv("test_0704.csv")
summary(df)

str(df)

df_copy <- df
summary(df_copy)


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
library(dplyr)
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
summary(df_copy)

df_copy %>% filter(is.na(re) & is.na(rn))
write.csv(df_copy, 'test_final.csv')
summary(df_copy)


#hm 결측치
df_copy %>% filter(is.na(hm))

#ta, hm 결측치 채우기 (td 채우기 전)
#train에 stn, month, day, day_split의 평균으로 결측 채우기 (ta, hm)

#평균기온 채우는 방법 : month, day, day_split 변수를 이용하여 imputation
#상대습도(hm) : 위와 동일
#이슬점온도(td) : imputation 후, 평균기온과 상대습도를 이용하여 계산

#train에 stn, month, day, day_split의 평균으로 결측 채우기 (ta, hm)
  
df_train <- read.csv("train_07042.csv")
summary(df_train)
df_test <- read.csv("test_final.csv")
summary(df_test)
library(dplyr)
  #ta
df_train_mean_ta <- df_train %>% group_by(month, day, day_split) %>% summarise(mean_ta = mean(ta, na.rm=T), .groups = 'drop')

df_test_join <- left_join(df_test, df_train_mean_ta, by = c("month", "day", 'day_split'))
df_test_join %>% select("month", "day", 'day_split', 'mean_ta')

  #hm
df_train_mean_hm <- df_train %>% group_by(month, day, day_split) %>% summarise(mean_hm = mean(hm, na.rm=T), .groups = 'drop')

df_test_join <- left_join(df_test_join, df_train_mean_hm, by = c("month", "day", 'day_split'))
df_test_join %>% select("month", "day", 'day_split', 'mean_hm')

    #확인
summary(df_test_join)

  #결측치 채우기
df_test_join$ta <- ifelse(is.na(df_test_join$ta), df_test_join$mean_ta, df_test_join$ta )
df_test_join$hm <- ifelse(is.na(df_test_join$hm), df_test_join$mean_hm, df_test_join$hm )

    #확인
df_test_join %>% select("month", "day", 'day_split', 'mean_hm', 'mean_ta')
summary(df_test_join)  #NA X

df_copy <- df_test_join

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


#ws결측치 -> 그룹 바이 (month, stn, day)의 ws 평균으로 채우기 
#test: df_copy, train: df_train
str(df_copy)
str(df_train)
library(dplyr)
df_mean_ws <- df_train %>% group_by(month, day) %>% summarise(mean_ws=mean(ws, na.rm=T), .groups = 'drop')
str(df_mean_ws)
str(df_copy)
df_join <- left_join(df_copy, df_mean_ws, by = c("month", "day"))#################
summary(df_join)

summary(df_join$ws)
summary(df_join$ws)
df[is.na(df$ws),]
summary(df_copy)

df_join$ws <- ifelse(is.na(df_join$ws), df_join$mean_ws, df_join$ws)
df_join[22821:22836,]

str(df_train)
df_train <- df_train[,-c(1,2,3,31)]

#test_0704f
summary(df_train)
df <- df_join
df_summary(df)

str(df)
summary(df)


#(month>= 2 & day >=1) & (month <= 8 & day <2) : up | mmddhh >= 20100 & mmddhh<80200
#inflection 추가
df_test$inflection <- ifelse(df_test$mmddhh>=20100 & df_test$mmddhh < 80200, 'up', 'down')

summary(df_test)
df_test <- df_test[,-c(1,2,24,25,26)]
summary(df_copy)

#mean_ta, mean_hm 제거
df_copy <- df_copy[,-c(1,23,24)]
summary(df_copy)

summary(df_train)

df_train <- df_train[,-c(1,2,3,31)]

#write.csv(df_train, file='train_0704f.csv')
write.csv(df_test, file='test_0704f.csv')

