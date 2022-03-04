#install.packages("modeest")
install.packages("conjoint")
#library(modeest)
library(conjoint)

allData <- expand.grid(
  aa = c("あり","なし"),
  bb = c("あり","なし"),
  cc = c("あり","なし"),
  dd = c("あり","なし"),
  ee = c("あり","なし"),
  price = c("100","500","1000","2000")
)
pf <- caFactorialDesign(data=allData,type="orthogonal")
pf

vod <- expand.grid(
  aa = c("国内作品のみ","国内＋海外"),
  bb = c("映画のみ","映画＋バラエティ"),
  cc = c("１台","２台"),
  price = c("500","1000","1500","2000")
)
pf2 <- caFactorialDesign(data=vod,type="orthogonal")
print(pf2)
