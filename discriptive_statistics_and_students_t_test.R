random_num = c(7.2,6.7,3.7,1.8,10.8, 5.0, 4.4, 4.4,7.7, 7.2)
random_num

#descriptibe statistics 記述統計量
summary(random_num)#minimum value, lower quartile, middle quartile(median), mean, upper quartile, maximum value 最小値、第一四分位数、中央値、平均値、第三四分位数、最大値
var(random_num)#variance 分散
sd(random_num)#standard deviation 標準偏差

mean(random_num)#mean 平均

#student's t-test t検定
qt(0.05,9)
(5.89-6)/2.54
qt(0.05,9)/sqrt(10)
(5.89-6)/2.54*sqrt(10)
