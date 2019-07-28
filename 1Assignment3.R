library(forecast)
library(tseries)
library(zoo)
library(psych)
library(caret)
library(Hmisc)
library(ggplot2)
library(vars)

temperature <- read.csv("F:/Big Data Analytics YorkU/Course 5/Project 3/weatherstats_toronto_hourly_edited.csv")
str(temperature)
class(temperature)
head(temperature)
tail(temperature)
any(is.na(temperature))
colSums(is.na(temperature))
temp1 <- temperature [- c(8, 12, 13, 15, 16, 17 : 19)]
head(temp1)
View(temp1)
colSums(is.na(temp1))
dim(temp1)
names(temp1)
mean_pre_st <- mean(temp1$pressure_station, na.rm = T)
mean_pre_st
mean_pre_se <- mean(temp1$pressure_sea, na.rm = T)
mean_pre_se
mean_win_dir_10 <- mean(temp1$wind_dir_10s, na.rm = T)
mean_win_dir_10
mean_win_sp <- mean(temp1$wind_speed, na.rm = T)
mean_win_sp
mean_rel_hum <- mean(temp1$relative_humidity, na.rm = T)
mean_rel_hum
mean_dew_poi <- mean(temp1$dew_point, na.rm = T)
mean_dew_poi
mean_temp <- mean(temp1$temperature, na.rm = T)
mean_temp
mean_visi <- mean(temp1$visibility, na.rm = T)
mean_visi
for (i in 1:11) {
  temp1[is.na(temp1[,i]), i] <- mean(temp1[,i], na.rm = T)
}
temp1
# round(temp1$dew_point, digits = 2)
View(temp1)
colSums(is.na(temp1))

str(temp1)

summary (temp1)

#changing the Date Time attribute to date type

df_weather<-temp1
df_weather$date_time_local<-as.Date(df_weather$date_time_local)
View(df_weather)

ggplot(df_weather, aes(date_time_local, temperature)) + geom_line() + scale_x_date('month')  + ylab("temperature") + xlab("")
 
#ggplot(temp1, aes(date_time_local, temperature)) + geom_line() + scale_x_date('month')  + ylab("temperature") + xlab("") 

#Checking max and min values for some of the attributes 

max(temperature)
class(df_weather$temperature)
max(df_weather$temperature)
min(df_weather$temperature)
max(df_weather$date_time_local)
min(df_weather$date_time_local)
View(df_weather)

#removing columns with too many missing values

df_weather <- df_weather [- c(2, 5)]
View(df_weather)

df_weatherFin<-data.frame("Date"=c(df_weather$date_time_local[1]),"Temp"=c(1),"Humidity"=c(1),"Dew"=c(1),"Pressure_st"=c(1),"Pressure_se"=c(1),"Wind_dir"=c(1),"Wind_speed"=c(1))
View(df_weatherFin)
#length(df_weather$date_time_local)
#index<-1

# for (i in 2:(length(df_weather$date_time_local))-1)
# {
#  
#   #index<index+1
#   if (df_weather$date_time_local[i]==df_weather$date_time_local[i+1])
#   {
#     
#     temp<-temp+df_weather$temperature[i+1]
#     humidity<-humidity+df_weather$humidex[i+1]
#     dew<-dew+df_weather$dew_point[i+1]
#     pressure_st<-pressure_st+df_weather$pressure_station[i+1]
#     pressure_se<-pressure_se+df_weather$pressure_sea[i+1]
#     wind_dir<-wind_dir+df_weather$wind_dir_10s[i+1]
#     wind_speed<-wind_speed+df_weather$wind_speed[i+1]
#     count<-count+1
#     a<-a+1
#   }
#   else{
#     
#     df_weatherFin$Date[index]<-df_weather$date_time_local[i]
#     df_weatherFin$Temp[index]<-temp/count
#     df_weatherFin$Humidity[index]<-humidity/count
#     df_weatherFin$Dew[index]<-dew/count
#     df_weatherFin$Pressure_st[index]<-pressure_st/count
#     df_weatherFin$Pressure_se[index]<-pressure_se/count
#     df_weatherFin$Wind_dir[index]<-wind_dir/count
#     df_weatherFin$Wind_speed[index]<-wind_speed/count
#     index<-index+1
#     
#     temp<-df_weather$temperature[i+1]
#     humidity<-df_weather$humidex[i+1]
#     dew<-df_weather$dew_point[i+1]
#     pressure_st<-df_weather$pressure_station[i+1]
#     pressure_se<-df_weather$pressure_sea[i+1]
#     wind_dir<-df_weather$wind_dir_10s[i+1]
#     wind_speed<-df_weather$wind_speed[i+1]
#     count<-1
#     b<-b+1
#    
#   }
# }
# count
# View(df_weatherFin)
# i

#reducing the data frame to a day frequency

df_final<-aggregate(cbind(temperature,humidex,pressure_station,pressure_sea,wind_dir_10s,wind_speed,dew_point)~date_time_local,df_weather,mean)
View(df_final)

ggplot(df_final, aes(date_time_local,temperature)) + geom_line() + scale_x_date('month')  + ylab("Temperature") +
  xlab("")

temp_ts = ts(df_final[, c('temperature')])

df_final$clean_temp = tsclean(temp_ts)

ggplot() +
  geom_line(data = df_final, aes(date_time_local,temp_ts)) + ylab('Cleaned Temperature')

ggplot(df_final, aes(date_time_local,humidex)) + geom_line() + scale_x_date('month')  + ylab("Humidity") +
  xlab("")




ggplot(df_final, aes(date_time_local,pressure_station)) + geom_line() + scale_x_date('month')  + ylab("Pressure") +
  xlab("")


ggplot(df_final, aes(date_time_local,pressure_sea)) + geom_line() + scale_x_date('month')  + ylab("Pressure Sea") +
  xlab("")



ggplot(df_final, aes(date_time_local,dew_point)) + geom_line() + scale_x_date('month')  + ylab("Dew") +
  xlab("")

ggplot(df_final, aes(date_time_local,wind_dir_10s)) + geom_line() + scale_x_date('month')  + ylab("Wind Direction") +
  xlab("")

ggplot(df_final, aes(date_time_local,wind_speed)) + geom_line() + scale_x_date('month')  + ylab("Wind Speed") +
  xlab("")

#creating the TimeSeries object

ts_final<-as.ts(df_final[2:8],frequency=365, start=c(1953, 1))

View(ts_final)

df_final$temp_ma = ma(df_final$temperature, order=7) 
df_final$temp_ma30 = ma(df_final$temperature, order=30)

ggplot() +
  geom_line(data = df_final, aes(x = date_time_local, y = temperature, colour = " Daily Temperature")) +
  geom_line(data = df_final, aes(x = date_time_local, y = temp_ma,   colour = "Weekly Moving Average Temperature"))  +
  geom_line(data = df_final, aes(x = date_time_local, y = temp_ma30, colour = "Monthly Moving Average Temperature"))  +
  ylab('Temperature')

#creating the moving averages for the decomposition

temperature_ma = ts(na.omit(df_final$temp_ma), frequency=30)
decomp = stl(temperature_ma, s.window="periodic")
deseasonal_cnt <- seasadj(decomp)
plot(decomp)

View(ts_final)


par(mar=c(1,1,1,1))
plot.ts(ts_final[,1], main = "temperature", ylab = "", xlab = "")
plot.ts(ts_final[,2], main = "Humidity", ylab = "", xlab = "")
plot.ts(ts_final[,3], main = "Pressure", ylab = "", xlab = "")
plot.ts(ts_final[,5], main = "Wind Direction", ylab = "", xlab = "")
plot.ts(ts_final[,6], main = "Wind Speed", ylab = "", xlab = "")
plot.ts(ts_final[,7], main = "Dew Point", ylab = "", xlab = "")
plot.ts(ts_final)

# ts_backup<-ts_final
# ts_final<-ts_final[,which(colnames(ts_final)!="date_time_local")]

plot(ts_final)

numDiff<-ndiffs(ts_final)
numDiff

par(mar=c(1, 1, 1, 1))


weather_diff<-diff(ts_final,differences = numDiff)
plot(weather_diff,plot.type="single", col=1:7)
legend("bottomleft",legend = colnames(ts_final),ncol_2,lty = 1,col = 1:7,cex = .9)

require(vars)

VARselect(ts_final, lag.max = 5, type = "const")

#P value is 5 for every criterion
#Creating the model
model_weather <- VAR(ts_final, p = 2, type = "const")
names(model_weather)
summary(model_weather)

par(mar=c(5.1, 4.1, 4.1, 2.1))
dev.off()
#par(mar=c(1, 1, 1, 1))
plot(model_weather)

#forecast for 10 days

forecast_weather <- predict(model_weather, n.ahead = 10, ci = 0.95)
names(forecast_weather)
par(mar=c(1, 1, 1, 1))
plot(forecast_weather)
fanchart(forecast_weather)

View(forecast_weather)
forecast_weather


