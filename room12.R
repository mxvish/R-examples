rm(list=ls())
data<-read.csv("room12.csv")
View(data)
summary(data)

#利用者のデータのみに絞る
user<-data[data$生協食堂の利用頻度を教えてください!="全く利用しない", ]
View(user)
summary(user)

barplot(table(user$"メニュー数に対してどう思いますか."),col="lightblue",main="メニュー数に対してどう思いますか.")
barplot(table(user$"では.メニュー数に対して満足していますか."),col="lightblue",main="では.メニュー数に対して満足していますか.") 
barplot(table(user$"次に.値段に対してどう思いますか."),col="lightblue",main="次に.値段に対してどう思いますか.")
barplot(table(user$"では.値段に対して満足していますか."),col="lightblue",main="では.値段に対して満足していますか.")
barplot(table(user$"生協食堂の営業時間についてどう思いますか."),col="lightblue",main="生協食堂の営業時間についてどう思いますか.")
barplot(table(user$"では.営業時間に対して満足していますか."),col="lightblue",main="では.営業時間に対して満足していますか.")

#利用頻度ごとにグループ分け
onceonamonth<-user[user$生協食堂の利用頻度を教えてください=="月1",]
onceperweek<-user[user$生協食堂の利用頻度を教えてください=="週1~2",]
threetimesperweek<-user[user$生協食堂の利用頻度を教えてください=="週3~5",]
everyday<-user[user$生協食堂の利用頻度を教えてください=="毎日",]

#データ(user$生協食堂の利用頻度を教えてください)を数値(一ヶ月あたりの利用回数)に置き換える
i=1
while (i<= nrow(user)){
  if(user[i, 5]=="月1") {
    user[i, 5]=1
  } else if(user[i, 5]=="週1~2"){
    user[i, 5]=6
  } else if(user[i, 5]=="週3~5"){
    user[i, 5]=16
  } else {
    user[i, 5]=28
  }
  i=i+1
}
View(user)

lm1=lm(生協食堂の利用頻度を教えてください~(メニュー数に対してどう思いますか.
                          +次に.値段に対してどう思いますか.)^2,data=user)
summary(lm1)
step(lm1)

lmfin=lm(formula = 生協食堂の利用頻度を教えてください ~ 
           メニュー数に対してどう思いますか., data = user)
summary(lmfin)
#--------------上が有意でないから目的変数変更-------
lm1=lm(生協食堂の利用頻度を教えてください~(メニュー数に対してどう思いますか.
                          +次に.値段に対してどう思いますか.
                          +生協食堂の営業時間についてどう思いますか.)^2,data=user)
step(lm1)

#step()で選択されたモデルで分析
lmfin=lm(formula = 生協食堂の利用頻度を教えてください ~ 
           メニュー数に対してどう思いますか. + 生協食堂の営業時間についてどう思いますか., 
         data = user)
summary(lmfin)

lm2=lm(生協食堂の利用頻度を教えてください~(では.メニュー数に対して満足していますか.
                          +では.値段に対して満足していますか.
                          +では.営業時間に対して満足していますか.)^2,data=user)
step(lm2)

#step()で選択されたモデルで分析
lmfin2=lm(formula = 生協食堂の利用頻度を教えてください ~ 
            では.メニュー数に対して満足していますか., 
          data = user)
summary(lmfin2)

#全く利用しない人のデータ
notuser<-data[data$生協食堂の利用頻度を教えてください=="全く利用しない", ]
View(notuser)
barplot(table(notuser$利用しない理由は何ですか.),col="lightblue",main="利用しない理由")
