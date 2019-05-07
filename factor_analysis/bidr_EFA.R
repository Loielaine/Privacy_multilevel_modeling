library(readr)
library(psych)
library(dplyr)
dir<-'/Users/Loielaine/Desktop/Good_Good_Study/2018Survey project/data/'
data<-read.csv2(paste0(dir,'interviewee_complete.csv'),sep =',')
opt <- options(fit.indices = c("GFI","CFI" , "RMSEA","AIC","BIC"))
#EFA
behavior_v <- c(paste0("bidr", 21:33,"_pmm"),paste0("bidr", 35:40,"_pmm")) #bidr21~40
behavior<-data[,behavior_v]
behavior<-apply(behavior,2,as.numeric)
#behavior[is.null(behavior)]<-NA

bidr_cor<-cor(behavior)

#1 factor EFA
bidr_factors1<-fa(r=bidr_cor,nfactors = 1,rotate="varimax",n.obs=1502)
bidr_factors1
score_factors1<-factor.scores(behavior,bidr_factors1)
bidr_scores1<-score_factors1$scores
fa.stats(r=behavior,f=bidr_factors1,n.obs=1502)
write.csv(bidr_scores1, file = paste0(dir,"bidr_scores_1.csv"),row.names =FALSE)

#2 factor EFA
bidr_factors2<-fa(r=bidr_cor,nfactors = 2,rotate="varimax",n.obs=1502)
bidr_factors2
fa.stats(r=behavior,f=bidr_factors2,n.obs=1502)
score_factors2<-factor.scores(behavior,bidr_factors2)
bidr_scores<-score_factors2$scores
colnames(bidr_scores)<-c("bidr_F1","bidr_F2")
write.csv(bidr_scores, file = paste0(dir,"bidr_scores.csv"),row.names =FALSE)


##Z-score
bidr_scores<-read.csv2(paste0(dir,"bidr_scores.csv"),sep =',')
bidr_scores<-data.frame(apply(bidr_scores,2,as.numeric))
data_std<-scale(bidr_scores,center = T,scale = T)
#1 std
data_std[bidr_scores<=1]<-0
data_std[bidr_scores>1]<-1
apply(data_std,2,sum)
colnames(data_std)<-c("bidr_F1_c","bidr_F2_c")
write.csv(data_std, file = paste0(dir,"bidr_scores_cate.csv"),row.names =FALSE)


fa.parallel(bidr_cor, n.obs=1502,fa="fa")


#sum scores directly
behavior<-data.frame(behavior)
behavior$sum<-apply(behavior,1,sum)
behavior$sum_scale<-scale(behavior$sum,center = T,scale = T)
behavior$sum_cate<-behavior$sum_scale
behavior[behavior$sum_scale<=1,"sum_cate"]<-0
behavior[behavior$sum_scale>1,"sum_cate"]<-1
sum(behavior$sum_cate)

bidr_sum<-behavior[,21:22]
write.csv(bidr_sum, file = paste0(dir,"bidr_sum_scores.csv"),row.names =FALSE)
