#Wilcoxon signed-rank test ウィルコクソンの符号順位検定
x=c(137, 152,165,142,130,152,142,148)
y=c(135,146,158, 135, 139,147,145,135)
wilcox.test(x, y, paired=T)
