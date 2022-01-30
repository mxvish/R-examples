# Poisson  distribution ポアソン分布
# source  https://sendai-u.repo.nii.ac.jp/?action=pages_view_main&active_action=repository_view_main_item_detail&item_id=135&item_no=1&page_id=28&block_id=36
plot(0:70, dpois(0:70, 10.55), type="h", lwd=3, main="来所回数別割合比較", xlab="回数", ylab="割合",　ylim=c(0,0.15))

# Log-normal distribution 対数正規分布
# source  https://www.nta.go.jp/publication/statistics/kokuzeicho/minkan2018/pdf/000.pdf
data=c(8.1, 13.7, 15.2, 17.2, 14.9, 10.2, 6.5, 4.4, 2.9, 1.9, 3.6, 0.8, 0.3, 0.3)
barplot(data, names.arg=c("~100","100~","200~","300~","400~","500~","600~"
                          ,"700~","800~","900~","1000~","1500~","2000~","2500~"), main="給与階級別給与所得者構成比", xlab="income(万円)", ylab="percentage", ylim=c(0,20))     
