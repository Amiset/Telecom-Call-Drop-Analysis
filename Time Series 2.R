library(readr)
library(ggpubr)
library(dplyr)
library(tidyr)
library(lubridate)
library(ggplot2)
library (forecast)


#read data file
telco2 <- read_csv("Telco2.csv")
#retain data only for tower id 1717
telco_1717<-telco2[(telco2$outgoing_site_id==1717),]#only site ID 1717

bp <- ggbarplot(telco_1717, x = "Weather", y = "Call_Dropped",
                color = "grey",fill = 'Start_Time_HH_MM_SS_s',            # Set bar border colors to white
                x.text.angle = 45           # Rotate vertically x axis texts
)
bp

scatter_plotA <- ggscatter(telco_1717, x = "Weather", y = "Call_Dropped",fig.height=20,
                           ylim = c(10,65),yticks.by =5,font.tickslab=c(8,"plain","red"),
                           add = "reg.line",           # add regression line
                           shape = 'Start_Time_HH_MM_SS_s',
                           #sort.by.groups = TRUE,      # Sort inside each group
                           x.text.angle = 45  )         # Rotate vertically x axis texts

scatter_plotA

scatter_plot1 <- ggscatter(telco_1717, x = "Weather", y = "Call_Dropped",
                ylim = c(10,65),yticks.by =5,font.tickslab=c(8,"plain","red"),
                add = "reg.line",           # add regression line
                shape = 'Traffic',palette = 'jco',
                sort.by.groups = TRUE,      # Sort inside each group
                x.text.angle = 45  )         # Rotate vertically x axis texts

scatter_plot1

scatter_plot2 <- ggscatter(telco_1717, x = "Total Calls", y = "Call_Dropped",
                           add = "reg.line",           # add regression line
                           shape = 'Weather', palette = 'jco',
                           sort.by.groups = TRUE,      # Sort inside each group
                           x.text.angle = 45  )         # Rotate vertically x axis texts
scatter_plot2


scatter_plot3 <- ggscatter(telco_1717, x = "Total Calls", y = "Call_Dropped",
                           add = "reg.line",           # add regression line
                           #shape = 'Weather',palette = 'jco',
                           sort.by.groups = TRUE,      # Sort inside each group
                           x.text.angle = 45  )         # Rotate vertically x axis texts
scatter_plot3

scatter_plot4 <- ggscatter(telco_1717, x = "Call_Dropped", y = "Traffic",
                           add = "reg.line",           # add regression line
                           #shape = 'Weather',palette = 'jco',
                           sort.by.groups = TRUE,      # Sort inside each group
                           x.text.angle = 45  )         # Rotate vertically x axis texts
scatter_plot4

scatter_plot5 <- ggscatter(telco_1717, x = "Start_Time_MM_DD_YYYY", y = "Call_Dropped",
                           add = "reg.line",           # add regression line
                           #shape = 'Weather',palette = 'jco',
                           sort.by.groups = TRUE,      # Sort inside each group
                           x.text.angle = 45  )         # Rotate vertically x axis texts
scatter_plot5

#remove duplicate rows for site ID 1717
telco_1717 <- telco_1717 %>% distinct()
#telco_1717<- telco_1717 %>%group_by(telco_1717$)


#drop cols 4 to 8 since - univariate analysis for call dropped 
telco_1717 <- select(telco_1717,-4:-8) #drop col 4 to 8
#check data
summary(telco_1717)
#convert Start_Time_MM_DD_YYYY col to datatime format
telco_1717 <- telco_1717 %>% mutate(Start_Time_MM_DD_YYYY = ymd(Start_Time_MM_DD_YYYY))
#split Start_Time_HH_MM_SS_s col into three cols A,B,C
telco_1717 <- telco_1717%>% separate (Start_Time_HH_MM_SS_s, c("A", "B", "C"), " ")
#merge Start_Time_MM_DD_YYYY col with A and rename it as time_pd
telco_1717 <- telco_1717 %>% unite(time_pd, c(Start_Time_MM_DD_YYYY, A),sep =" ")
#drop the cols B & C since not required
telco_1717 <- select(telco_1717,c(-1,-3:-4)) #drop col 1, 3 and 4
#View(telco_1717) - check

#convert the col time_pd into POSIXct class and create separate col
telco_1717$time_pd_posix <- as.POSIXct(telco_1717$time_pd, format = "%Y-%m-%d %H:%M")
class(telco_1717$time_pd_posix) #check class
#View(telco_1717) - check

# Using scale_x_datetime
decompo_1 <- ggplot(telco_1717, aes(x = time_pd_posix, y = Call_Dropped)) + 
  geom_line() + 
  scale_x_datetime(date_labels = "%p-%d", date_breaks = "48 hour") + 
  theme_classic()
decompo_1

decompo_2 <- ggplot(telco_1717, aes(x = time_pd_posix, y = Call_Dropped)) +
  geom_line() +
  geom_smooth(method = "loess", se = FALSE, span = 0.3) +
  theme_classic()

decompo_2

scatter_plot6 <- ggscatter(telco_1717, x = "time_pd_posix", y = "Call_Dropped",
                           add = "reg.line",           # add regression line
                           #shape = 'Weather',palette = 'jco',
                           sort.by.groups = TRUE,      # Sort inside each group
                           x.text.angle = 45  )         # Rotate vertically x axis texts
scatter_plot6

#subsetting into train and test data
telco_1717_train <- telco_1717[1:174,]
telco_1717_test <- telco_1717[175:186,]

# Transforming data into 'time series' object
telco_1717_train_ts <- ts(telco_1717_train$Call_Dropped, start = 01, end = 29, freq = 4)  # Specify start and end year, measurement frequency (monthly = 12)
telco_1717_train_ts

ts.plot(telco_1717_train_ts)

telco_1717_train_stl <- stl(telco_1717_train_ts, s.window = "period")

# Generate plots
plot(telco_1717_train_stl)

#Root mean Square error function(rmse)
rmse <- function(actual,pred){
  #mape <- sqrt(mean(abs((actual - pred)/actual)))*100
  rmse <- (sqrt(mean(abs(pred - actual))^2))
  return (rmse)
}

#Naive forecasting model - used as benchmark for forecast models
naive_mod <- naive(telco_1717_train_ts, h = 12)
summary(naive_mod)

#to find MAPE error on test data
telco_1717_test$naive = 31
rmse(telco_1717_test$Call_Dropped, telco_1717_test$naive)  ## 14.33 %

#Simple exponential smoothing

se_model <- ses(telco_1717_train_ts, h = 12)
summary(se_model)

#finding MAPE error 
telco_1717_se_fc <- as.data.frame(se_model)
telco_1717_test$Sim_Exp <-  telco_1717_se_fc$`Point Forecast`
rmse(telco_1717_test$Call_Dropped, telco_1717_test$Sim_Exp) #12.337 %

#Holt's Trend Method
holt_model <- holt(telco_1717_train_ts, h = 12)
summary(holt_model)

df_holt = as.data.frame(holt_model)
telco_1717_test$holt = df_holt$`Point Forecast`#34.67
rmse(telco_1717_test$Call_Dropped, telco_1717_test$holt) #11.575 %

#ARIMA
arima_model <- auto.arima(telco_1717_train_ts)
summary(arima_model)#37.55

forecast_arima <-  forecast::forecast(arima_model, h=12)
df_arima <-  as.data.frame(forecast_arima)
telco_1717_test$arima <- df_arima$`Point Forecast`
rmse(telco_1717_test$Call_Dropped, telco_1717_test$arima)  ## 12.33

model_tbats <- tbats(telco_1717_train_ts)
summary(model_tbats)


for_tbats <- forecast::forecast(model_tbats, h = 12)
df_tbats = as.data.frame(for_tbats)
telco_1717_test$tbats = df_tbats$`Point Forecast`
rmse(telco_1717_test$Call_Dropped, telco_1717_test$tbats) 
model_tbats
