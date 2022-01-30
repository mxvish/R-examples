x=c(61,50,49,55,51,48,46,55,65,70)
y=c(59,48,48,54,49,50,48,50,63,67)

#statistical significance=5%, two-tailed test, paired samples t-test 有意水準５％, 両側検定 対応のあるt検定
t.test(x,y,paired=T)

#scatter plot 散布図
plot(x,y)
#correlation coefficient 相関係数
cor.test(x,y)
