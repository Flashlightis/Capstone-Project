# Make Tidy - All Data
df3_clean <- gather(df3, Web_Metric, Number, c("Sessions", "Users", "New_Users", "Pageviews", 
                                               "Number_Session", "Pages_Session", "Avg_S_Duration",
                                               "Bounce")) 

df4_m2 <- mutate(df3_2014, 
                 month = as.POSIXlt(Date)$mon + 1)
df4_m2 <- group_by(df4_m2, month)

df4_2014_month <- summarize(df4_m2, Sessions = sum(Sessions, na.rm = TRUE), 
                            Pageviews = sum(Pageviews, na.rm = TRUE),
                            Users = sum(Users, na.rm = TRUE), 
                            New_Users = sum(New_Users, na.rm = TRUE),
                            Number_Session = sum(Number_Session, na.rm = TRUE),
                            Pages_Session = sum(Pages_Session, na.rm = TRUE),
                            Avg_S_Duration = sum(Avg_S_Duration, na.rm = TRUE),
                            Bounce = mean(Bounce, na.rm = TRUE))

df4_2014_month <- gather(df4_2014_month, Web_Metric, Number, c("Sessions", "Users", "New_Users")) # Make Tidy

# 2015

df5_m2 <- mutate(df3_2015, 
                 month = as.POSIXlt(Date)$mon + 1)
df5_m2 <- group_by(df5_m2, month)

df5_2015_month <- summarize(df5_m2, Sessions = sum(Sessions, na.rm = TRUE), 
                            Pageviews = sum(Pageviews, na.rm = TRUE),
                            Users = sum(Users, na.rm = TRUE), 
                            New_Users = sum(New_Users, na.rm = TRUE),
                            Number_Session = sum(Number_Session, na.rm = TRUE),
                            Pages_Session = sum(Pages_Session, na.rm = TRUE),
                            Avg_S_Duration = sum(Avg_S_Duration, na.rm = TRUE),
                            Bounce = mean(Bounce, na.rm = TRUE))

df5_2015_month <- gather(df5_2015_month, Web_Metric, Number, c("Sessions", "Users", "New_Users")) # Make Tidy

# 2016

df6_m2 <- mutate(df3_2016, 
                 month = as.POSIXlt(Date)$mon + 1)
df6_m2 <- group_by(df6_m2, month)

df6_2016_month <- summarize(df6_m2, Sessions = sum(Sessions, na.rm = TRUE), 
                            Pageviews = sum(Pageviews, na.rm = TRUE),
                            Users = sum(Users, na.rm = TRUE), 
                            New_Users = sum(New_Users, na.rm = TRUE),
                            Number_Session = sum(Number_Session, na.rm = TRUE),
                            Pages_Session = sum(Pages_Session, na.rm = TRUE),
                            Avg_S_Duration = sum(Avg_S_Duration, na.rm = TRUE),
                            Bounce = mean(Bounce, na.rm = TRUE))

df6_2016_month <- gather(df6_2016_month, Web_Metric, Number, c("Sessions", "Users", "New_Users")) # Make Tidy

# 2017
df7_m2 <- mutate(df3_2017, 
                 month = as.POSIXlt(Date)$mon + 1)
df7_m2 <- group_by(df7_m2, month)

df7_2017_month <- summarize(df7_m2, Sessions = sum(Sessions, na.rm = TRUE), 
                            Pageviews = sum(Pageviews, na.rm = TRUE),
                            Users = sum(Users, na.rm = TRUE), 
                            New_Users = sum(New_Users, na.rm = TRUE),
                            Number_Session = sum(Number_Session, na.rm = TRUE),
                            Pages_Session = sum(Pages_Session, na.rm = TRUE),
                            Avg_S_Duration = sum(Avg_S_Duration, na.rm = TRUE),
                            Bounce = mean(Bounce, na.rm = TRUE))

df7_2017_month <- gather(df7_2017_month, Web_Metric, Number, c("Sessions", "Users", "New_Users")) # Make Tidy


# Convert to Tidy Dataset ---

df3_ty2 <- gather(df3_Total_year, Web_Metric, Number, Sessions:Users)

df3_ty3 <- gather(df3_Total_year, Web_Metric, Number, c("Sessions", "Users", "New_Users"))

View(df3_ty)
View(df3_ty2)
View(df3_ty3)
