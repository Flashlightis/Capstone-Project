# Stats Summaries ---

summary(df3$Sessions)
sd(df3$Sessions)
summary(df3$Pageviews)
sd(df3$Pageviews)

summary(df3_2012$Sessions)

sapply(df3$Sessions, var)

# Boxplots by Year ---

ggplot(df3_2013_month, aes(x = Web_Metric, y = Number, color = Web_Metric)) +
  geom_boxplot() +
  ggtitle("Box Plot for 2013 Web Traffic") +
  theme(plot.title = element_text(hjust = 0.5))

ggplot(df4_2014_month, aes(x = Web_Metric, y = Number, color = Web_Metric)) +
  geom_boxplot() +
  ggtitle("Box Plot for 2014 Web Traffic") +
  theme(plot.title = element_text(hjust = 0.5))

ggplot(df5_2015_month, aes(x = Web_Metric, y = Number, color = Web_Metric)) +
  geom_boxplot() +
  ggtitle("Box Plot for 2015 Web Traffic") +
  theme(plot.title = element_text(hjust = 0.5))

ggplot(df6_2016_month, aes(x = Web_Metric, y = Number, color = Web_Metric)) +
  geom_boxplot() +
  ggtitle("Box Plot for 2016 Web Traffic") +
  theme(plot.title = element_text(hjust = 0.5))

ggplot(df7_2017_month, aes(x = Web_Metric, y = Number, color = Web_Metric)) +
  geom_boxplot() +
  ggtitle("Box Plot for 2017 Web Traffic") +
  theme(plot.title = element_text(hjust = 0.5))

# Box Plot for All Metrics for All Time ---

ggplot(df3_clean, aes(x = Web_Metric, y = Number, color = Web_Metric)) +
  geom_boxplot() +
  ggtitle("Box Plot for 2012-2018 Web Traffic") +
  theme(plot.title = element_text(hjust = 0.5))

# Box Plot Per Metric for All Time ---

ggplot(df3_clean, aes(x = Web_Metric, y = Number, color = Web_Metric)) +
  geom_boxplot() +
  ggtitle("Box Plot for All Pageviews") +
  theme(plot.title = element_text(hjust = 0.5)) +
  scale_x_discrete(limits = c("Pageviews"))

ggplot(df3_clean, aes(x = Web_Metric, y = Number, color = Web_Metric)) +
  geom_boxplot() +
  ggtitle("Box Plot for All Pages per Session") +
  theme(plot.title = element_text(hjust = 0.5)) +
  scale_x_discrete(limits = c("Pages_Session"))

ggplot(df3_clean, aes(x = Web_Metric, y = Number, color = Web_Metric)) +
  geom_boxplot() +
  ggtitle("Box Plot for All Average Session Duration") +
  theme(plot.title = element_text(hjust = 0.5)) +
  scale_x_discrete(limits = c("Avg_S_Duration"))

ggplot(df3_clean, aes(x = Web_Metric, y = Number, color = Web_Metric)) +
  geom_boxplot() +
  ggtitle("Box Plot for All Bounce Rate") +
  theme(plot.title = element_text(hjust = 0.5)) +
  scale_x_discrete(limits = c("Bounce"))

cor.test(df3$Sessions, df3$Users)


