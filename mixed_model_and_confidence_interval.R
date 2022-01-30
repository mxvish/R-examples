#mixed model 混合効果モデルのあてはめlme()を使うためのパッケージ "nlme"
install.packages("nlme")
library(nlme)

data("Orthodont")
plot(Orthodont)
aa=Orthodont
names(aa)
dim(aa)                                                                                                                                                                                                     

library("lme4")
y=lmer(distance~age*Sex+(1|Subject), data=aa)
summary(y)#estimateが変数の係数
confint(y)#confidence interval 信頼区間　1をまたがないと優位に影響がある
