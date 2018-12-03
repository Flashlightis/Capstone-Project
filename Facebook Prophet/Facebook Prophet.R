## Remove all 2018 data befor running Facebook Prophet ---

df3_clean$Date = as.Date(df3_clean$Date) ## Need to make convert "Date" into a date from a factor
clean_df <- subset(df3_clean, Date < "2018-01-01") ## removed 2018 for FB Prophet, so can predict 2018 traffic

## Remove all other variables in Web_Metric except Sessions for Predictive Modelling

fbp_sessions <- clean_df[ which(clean_df$Web_Metric == 'Sessions'), ]

qplot(x = Date, y = Number, data = fbp_sessions) ## Quick Plot of the data to look for visible trends
summary(fbp_sessions) ## Get Statistical Summary of fpb_sessions, looking for missing data & we see a 0 need to remove

fbp_sessions$Number[fbp_sessions$Number==0] <- NA ## Removed the 0 observations for Facebook Profit

## Log transformation of data to check for seasonality
ds <- fbp_sessions$Date
y <- log(fbp_sessions$Number)
fbp <- data.frame(ds, y)
View(fbp)

qplot(ds, y, data = fbp) ## Still no seasonality

fbp_1 <- prophet(fbp)

fbp_1

## Prediction Using FB Prophet

future_sessions <- make_future_dataframe(fbp_1, periods = 365)

tail(future_sessions)
forecast_sessions <- predict(fbp_1, future_sessions)
tail(forecast_sessions)
tail(forecast_sessions[c('ds', 'yhat', 'yhat_lower', 'yhat_upper')])
plot(fbp_1, forecast_sessions)
prophet_plot_components(fbp_1, forecast_sessions)

## using 2012-2016 to test Prophet against actual 2017 data

fbp_sessions_2016 <- subset(fbp_sessions, Date < "2017-01-01") ## Removing 2017 data

ds <- fbp_sessions_2016$Date
y <- log(fbp_sessions_2016$Number)
fbp_2 <- data.frame(ds, y)
View(fbp_2)

fbp_3 <- prophet(fbp_2)

fbp_3

future_sessions_2017 <- make_future_dataframe(fbp_3, periods = 365)
tail(future_sessions_2017)

forecast_sessions_2017 <- predict(fbp_3, future_sessions_2017)
tail(forecast_sessions_2017)

tail(forecast_sessions_2017[c('ds', 'yhat', 'yhat_lower', 'yhat_upper')])
exp(2.741682) # is yhat & is equal to: 15.51306 / actual is: 11
exp(2.336989) # is yhat lower & is equal to: 10.35003 / actua; is 11

prophet_plot_components(fbp_3, forecast_sessions_2017)

## Creating new Dataset to capture MAPE for 2018 only
## Need to take out the 2018 data (Jan-March 12th) in all the Facebook Prophet prediction

fc_2018 <- subset(forecast_sessions, ds > "2017-12-31" & ds < "2018-11-2")
mape_2018 <- fc_2018 [c(1, 15:16, 19)] ## just including the columns that I want (yhat & date)
clean_2018 <- gather(df3_2018, Web_Metric, Number, c("Sessions")) ## preparing to join mape_2018
clean_2018_1 <- clean_2018 [10:11] ## created df with just Sessions & Number
mape_2018 <- cbind(mape_2018, clean_2018_1) ## combined datasets
mape_2018$Predicted <- exp(mape_2018[,4]) ## created new variable of predicted values based of of yhat
mape_2018$Predicted_Lower <- exp(mape_2018[,2])
mape_2018$Predicted_Upper <- exp(mape_2018[,3])
colnames(mape_2018)[1] <- "Date" ## Rename columns
colnames(mape_2018)[6] <- "Actual"
mape_2018$MAPE <- abs((mape_2018[,6] - mape_2018[,7]) / mape_2018[,6]) * 100 ## calculated MAPE for each variable
mape_2018$MAPE_Upper <- abs((mape_2018[,6] - mape_2018[,9]) / mape_2018[,6]) * 100
mape_2018$MAPE_Lower <- abs((mape_2018[,6] - mape_2018[,8]) / mape_2018[,6]) * 100
summary(mape_2018)

## Make Tidy in order to plot

tidy_mape2018_1 <- gather(mape_2018, Results, Number, c("Actual", "Predicted", "Predicted_Lower", "Predicted_Upper")) 
tidy_mape2018_2 <- gather(mape_2018, All_Mape, Number, c("MAPE", "MAPE_Lower", "MAPE_Upper", "Actual"))

## 2018 Plots

tidy_mape2018_1_line <- ggplot(tidy_mape2018_1, aes(x = Date, y = Number, col = Results)) +
  geom_line() +
  ggtitle("2018 Actual vs Predicted, Predicted Lower, and Predicted Upper") +
  theme(plot.title = element_text(hjust = 0.5)) +
  labs(x = "Results", y = "Count")

tidy_mape2018_1_line

tidy_mape2018_2_line <- ggplot(tidy_mape2018_2, aes(x = Date, y = Number, col = All_Mape)) +
  geom_line() +
  ggtitle("2018 Actual vs APE, APE Lower & APE Upper") +
  theme(plot.title = element_text(hjust = 0.5)) +
  labs(x = "ALL_APE", y = "Count")

tidy_mape2018_2_line

## Creating new Dataset to capture MAPE for 2017 - using only predicted 2017 data vs actual

fc_2017 <- subset(forecast_sessions_2017, ds > "2016-12-31" & ds < "2017-12-31")
mape_2017_2 <- fc_2017 [c(1, 15:16, 19)] ## just including the columns that I want (yhat & date)

clean_2017_1 <- df3_2017 [3] ## created df with just Sessions
mape_2017_2 <- cbind(mape_2017_2, clean_2017_1) ## combined datasets
mape_2017_2$Predicted <- exp(mape_2017_2[,4]) ## created new variable of predicted values based of of yhat
mape_2017_2$Predicted_Lower <- exp(mape_2017_2[,2])
mape_2017_2$Predicted_Upper <- exp(mape_2017_2[,3])
colnames(mape_2017_2)[1] <- "Date" ## Rename columns

mape_2017_2$MAPE <- abs((mape_2017_2[,6] - mape_2017_2[,7]) / mape_2017_2[,6]) * 100 ## calculated MAPE for each variable
mape_2017_2$MAPE_Upper <- abs((mape_2017_2[,6] - mape_2017_2[,9]) / mape_2017_2[,6]) * 100
mape_2017_2$MAPE_Lower <- abs((mape_2017_2[,6] - mape_2017_2[,8]) / mape_2017_2[,6]) * 100
summary(mape_2017_2)

## Summarize data into months so it's easy to view

tidy_2017_pred <- mutate(mape_2017_2, 
                         month = as.POSIXlt(Date)$mon + 1)
tidy_2017_pred <- group_by(tidy_2017_pred, month)

tidy_mape2017_predicted_month <- summarize(tidy_2017_pred, yhat_lower = sum(yhat_lower, na.rm = TRUE), 
                                           yhat_upper = sum(yhat_upper, na.rm = TRUE),
                                           yhat = sum(yhat, na.rm = TRUE), 
                                           MAPE = sum(MAPE, na.rm = TRUE),
                                           MAPE_Upper = sum(MAPE_Upper, na.rm = TRUE),
                                           MAPE_Lower = sum(MAPE_Lower, na.rm = TRUE),
                                           Sessions = sum(Sessions, na.rm = TRUE),
                                           Predicted = sum(Predicted, na.rm = TRUE),
                                           Predicted_Lower = sum(Predicted_Lower, na.rm = TRUE),
                                           Predicted_Upper = sum(Predicted_Upper, na.rm = TRUE))


## Make tidy so can plot

tidy_mape2017_predicted_month <- gather(tidy_mape2017_predicted_month, Web_Metric, Actual, c("Sessions"))
tidy_mape2017_predicted_month <- gather(tidy_mape2017_predicted_month, Results, Number, c("Actual", "Predicted", "Predicted_Lower", "Predicted_Upper")) 

tidy_mape2017_predicted_month <- gather(tidy_mape2017_predicted_month, All_Mape, MAPE_Number, c("MAPE", "MAPE_Lower", "MAPE_Upper"))

## 2017 Plots

predicted_2017_line <- ggplot(tidy_mape2017_predicted_month, aes(x = month, y = Number, col = Results)) +
  geom_line() +
  ggtitle("2017 Actual vs Predicted, Predicted Lower, and Predicted Upper") +
  theme(plot.title = element_text(hjust = 0.5)) +
  labs(x = "Results", y = "Count") +
  scale_x_discrete(limits = c("Jan","Feb","March","April","May","June","July","Aug","Sep","Oct","Nov","Dec"))
predicted_2017_line

predicted_mape2017_line <- ggplot(tidy_mape2017_predicted_month, aes(x = month, y = MAPE_Number, col = All_Mape)) +
  geom_line() +
  ggtitle("2017 APE vs APE Lower, and APE Upper") +
  theme(plot.title = element_text(hjust = 0.5)) +
  labs(x = "All_MAPE", y = "Count") +
  scale_x_discrete(limits = c("Jan","Feb","March","April","May","June","July","Aug","Sep","Oct","Nov","Dec"))

predicted_mape2017_line

str(tidy_mape2017_predicted)

## Creating new DF with all actual 2018 data - until Nov. 2nd 2018

df3_2018_sessions <- df3_2018 [c(2:3)] ## removed all variables except Date & Sessions
actual_2018_sessions <- rbind(df3_2018_sessions, actual_2018) ## combined sessions to include all sessions to date

fc_2018 <- subset(forecast_sessions, ds > "2017-12-31" & ds < "2018-11-2")
mape_2018 <- fc_2018 [c(1, 15:16, 19)] ## just including the columns that I want (yhat & date)
mape_2018 <- mape_2018 [2:4] ## removed ds from df - getting ready to join with actual_2018_Sessions
traffic_2018 <- cbind(actual_2018_sessions, mape_2018) ## combined datasets to get ready for anaylsis
traffic_2018$Predicted <- exp(traffic_2018[,5]) ## created new variable of predicted values based of of yhat
traffic_2018$Predicted_Lower <- exp(traffic_2018[,3])
traffic_2018$Predicted_Upper <- exp(traffic_2018[,4])
traffic_2018$APE <- abs((traffic_2018[,2] - traffic_2018[,6]) / traffic_2018[,2]) * 100 ## calculated APE for each variable
traffic_2018$APE_Upper <- abs((traffic_2018[,2] - traffic_2018[,8]) / traffic_2018[,2]) * 100
traffic_2018$APE_Lower <- abs((traffic_2018[,2] - traffic_2018[,7]) / traffic_2018[,2]) * 100
summary(traffic_2018)

## Summarize data into months so it's easy to view

traffic_2018_pred <- mutate(traffic_2018, 
                         month = as.POSIXlt(Date)$mon + 1)
traffic_2018_pred <- group_by(traffic_2018_pred, month)

traffic_2018_month <- summarize(traffic_2018_pred, yhat_lower = sum(yhat_lower, na.rm = TRUE), 
                                           yhat_upper = sum(yhat_upper, na.rm = TRUE),
                                           yhat = sum(yhat, na.rm = TRUE), 
                                           APE = sum(APE, na.rm = TRUE),
                                           APE_Upper = sum(APE_Upper, na.rm = TRUE),
                                           APE_Lower = sum(APE_Lower, na.rm = TRUE),
                                           Sessions = sum(Sessions, na.rm = TRUE),
                                           Predicted = sum(Predicted, na.rm = TRUE),
                                           Predicted_Lower = sum(Predicted_Lower, na.rm = TRUE),
                                           Predicted_Upper = sum(Predicted_Upper, na.rm = TRUE))


## Make tidy so can plot

traffic_2018_month <- gather(traffic_2018_month, Web_Metric, Actual, c("Sessions"))
traffic_2018_month <- gather(traffic_2018_month, Results, Number, c("Actual", "Predicted", "Predicted_Lower", "Predicted_Upper")) 

## Plotting 2018 to check actual performance of AdWord campaign 

traffic_2018_month_line <- ggplot(traffic_2018_month, aes(x = month, y = Number, col = Results)) +
  geom_line() +
  ggtitle("2018 Actual vs Predicted, Predicted Lower, and Predicted Upper") +
  theme(plot.title = element_text(hjust = 0.5)) +
  labs(x = "Results", y = "Count") +
  scale_x_discrete(limits = c("Jan","Feb","March","April","May","June","July","Aug","Sep","Oct","Nov","Dec"))

traffic_2018_month_line
