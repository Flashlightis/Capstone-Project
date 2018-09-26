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
