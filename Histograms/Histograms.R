# Hist Total Dataset - Sessions
hist_total <- ggplot(df3, aes(x = Sessions)) +
  geom_histogram() +
  ggtitle("Histogram Total Sessions 2012-2018")
hist_total

hist_total_1 <- ggplot(df3, aes(x = Sessions)) +
  geom_histogram(binwidth = 0.2)

hist_total_1

#Sessions Histograms by Year

hist_2012_year <- ggplot(df3_2012, aes(x = Sessions)) +
  geom_histogram() +
  ggtitle("Histogram 2012 Sessions")
hist_2012_year

hist_2013_year <- ggplot(df3_2013, aes(x = Sessions)) +
  geom_histogram() +
  ggtitle("Histogram 2013 Sessions")
hist_2013_year

hist_2014_year <- ggplot(df3_2014, aes(x = Sessions)) +
  geom_histogram() +
  ggtitle("Histogram 2014 Sessions")
hist_2014_year

hist_2015_year <- ggplot(df3_2015, aes(x = Sessions)) +
  geom_histogram() +
  ggtitle("Histogram 2015 Sessions Total")
hist_2015_year

hist_2016_year <- ggplot(df3_2016, aes(x = Sessions)) +
  geom_histogram() +
  ggtitle("Histogram 2016 Sessions Total")
hist_2016_year

hist_2017_year <- ggplot(df3_2017, aes(x = Sessions)) +
  geom_histogram() +
  ggtitle("Histogram 2017 Sessions Total")
hist_2017_year

hist_2018_year <- ggplot(df3_2018, aes(x = Sessions)) +
  geom_histogram() +
  ggtitle("Histogram 2018 Sessions - Data only until March")
hist_2018_year

#New Users Histograms Total
hist_nw_total <- ggplot(df3, aes(x = New_Users)) +
  geom_histogram() +
  ggtitle("Histogram Total New Users 2012-2018")
hist_nw_total

#Users Histograms Total
hist_users_total <- ggplot(df3, aes(x = Users)) +
  geom_histogram() +
  ggtitle("Histogram Total Users 2012-2018")
hist_users_total

#Pageviews Histograms Total
hist_pv_total <- ggplot(df3, aes(x = Pageviews)) +
  geom_histogram() +
  ggtitle("Histogram Total Pageviews 2012-2018")
hist_pv_total

#Find Pageviews Outliners
df3_pv <- df3[ which(df3$Pageviews > 300), ] 
View(df3_pv)
#Remove pageview outliners since only occured twice
df3_pv_removed = df3[-197,]
df3_pv_removed = df3_pv_removed[-1563,]

df3_pv_removed$Pageviews > 300 <- NULL 

df3_pv <- df3[ which(df3$Pageviews > 300), ] 

pv_removed <- df3$Pageviews[ df3$Pageviews > 300 ]
View(pv_removed)

hist_pv_removed <- ggplot(df3_pv_removed, aes(x = Pageviews)) +
  geom_histogram() +
  ggtitle("Histogram Pageviews 2012-2018 (No Outliners)")
hist_pv_removed

#Number per Session Histograms Total
hist_ns_total <- ggplot(df3, aes(x = Number_Session)) +
  geom_histogram() +
  ggtitle("Histogram Total Number per Session 2012-2018")
hist_ns_total

#Pages per Session Histograms Total
hist_ps_total <- ggplot(df3, aes(x = Pages_Session)) +
  geom_histogram() +
  ggtitle("Histogram Total Pages per Session 2012-2018")
hist_ps_total

#Average Session Duration Histograms Total
hist_asd_total <- ggplot(df3, aes(x = Avg_S_Duration)) +
  geom_histogram() +
  ggtitle("Histogram Total Average Session Duration 2012-2018")
hist_asd_total

#Bounce Rate Histograms Total
hist_br_total <- ggplot(df3, aes(x = Bounce)) +
  geom_histogram() +
  ggtitle("Histogram Total Bounce Rate 2012-2018")
hist_br_total

