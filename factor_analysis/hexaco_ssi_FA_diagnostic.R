# Interview Privacy Factor Analysis  #
# liyixi #
# 2018.12.14 #

library(readr)
library(psych)
library(sem)
library(dplyr)
dir<-'/Users/Loielaine/Desktop/Good_Good_Study/2018Survey project/data/'
#opt <- options(fit.indices = c("GFI", "AGFI", "RMSEA", "NFI", "NNFI", "CFI", "RNI", "IFI", "SRMR", "AIC", "AICc", "BIC", "CAIC"))
opt <- options(fit.indices = c("GFI","CFI" , "RMSEA","AIC","BIC"))
##hexaco
#sum items
hexaco<-read.csv2(paste0(dir,'hexaco_pmm.csv'),sep =',' )
##further comparision
freq<-data.frame()
for(i in 1:60){
  freq[i,1]<-sum(hexaco[,i]==1)
  freq[i,2]<-sum(hexaco[,i]==2)
  freq[i,3]<-sum(hexaco[,i]==3)
  freq[i,4]<-sum(hexaco[,i]==4)
  freq[i,5]<-sum(hexaco[,i]==5)
}

#1.all
#humility
hexaco$sincerity<-hexaco[,6]+ hexaco[,30]+hexaco[,54]
hexaco$fairness<-hexaco[,12]+hexaco[,36]+hexaco[,60]
hexaco$greedavoidance<-hexaco[,18]+ hexaco[,42]
hexaco$modesty<-hexaco[,24]+ hexaco[,48]

#emotionality
hexaco$fearfulness<-hexaco[,5]+hexaco[,29]+ hexaco[,53]
hexaco$anxiety<-hexaco[,11]+ hexaco[,35]
hexaco$dependence<-hexaco[,17]+ hexaco[,41]
hexaco$sentimentality<-hexaco[,23]+hexaco[,47]+ hexaco[,59]

#extraversion
hexaco$selfesteem<-hexaco[,4]+ hexaco[,28]+ hexaco[,52]
hexaco$boldness<- hexaco[,10]+hexaco[,34]+hexaco[,58]
hexaco$socialbility<-hexaco[,16]+hexaco[,40]
hexaco$liveliness<-hexaco[,22]+ hexaco[,46]

#agreeableness
hexaco$forgiveness<-hexaco[,3]+hexaco[,27]
hexaco$gentleness<- hexaco[,9]+hexaco[,33]+hexaco[,51]
hexaco$flexibility<- hexaco[,15]+hexaco[,39]+ hexaco[,57]
hexaco$patience<- hexaco[,21]+hexaco[,45]

#conscientiousness
hexaco$organization<-hexaco[,2]+ hexaco[,26]
hexaco$diligence<-hexaco[,8]+ hexaco[,32]
hexaco$perfectionism<- hexaco[,14]+hexaco[,38]+hexaco[,50]
hexaco$prudence<- hexaco[,20]+ hexaco[,44]+ hexaco[,56]

#openess
hexaco$aesthetic<- hexaco[,1]+hexaco[,25]
hexaco$inquisitiveness<-hexaco[,7]+ hexaco[,31]
hexaco$creativity<-hexaco[,13]+hexaco[,37]+ hexaco[,49]
hexaco$unconventionality<- hexaco[,19]+hexaco[,43]+ hexaco[,55]

#EFA
data<-hexaco[,61:84]
hex_cor<-cor(data)
hexaco_all<-fa(r=hex_cor,n.obs = 64,nfactors = 6,rotate = "varimax")
hexaco_all
summary(hexaco_all)

doubt<-c(1,3,9,19,24,27,29,31,32,46,48,55,56)
#2.remove all doubts
#humility
hexaco$sincerity<-hexaco[,6]+ hexaco[,30]+hexaco[,54]
hexaco$fairness<-hexaco[,12]+hexaco[,36]+hexaco[,60]
hexaco$greedavoidance<-hexaco[,18]+ hexaco[,42]
hexaco$modesty<-NULL

#emotionality
hexaco$fearfulness<-hexaco[,5]+ hexaco[,53]
hexaco$anxiety<-hexaco[,11]+ hexaco[,35]
hexaco$dependence<-hexaco[,17]+ hexaco[,41]
hexaco$sentimentality<-hexaco[,23]+hexaco[,47]+ hexaco[,59]

#extraversion
hexaco$selfesteem<-hexaco[,4]+ hexaco[,28]+ hexaco[,52]
hexaco$boldness<- hexaco[,10]+hexaco[,34]+hexaco[,58]
hexaco$socialbility<-hexaco[,16]+hexaco[,40]
hexaco$liveliness<-hexaco[,22]

#agreeableness
hexaco$forgiveness<-NULL
hexaco$gentleness<- hexaco[,33]+hexaco[,51]
hexaco$flexibility<- hexaco[,15]+hexaco[,39]+ hexaco[,57]
hexaco$patience<- hexaco[,21]+hexaco[,45]

#conscientiousness
hexaco$organization<-hexaco[,2]+ hexaco[,26]
hexaco$diligence<-hexaco[,8]
hexaco$perfectionism<- hexaco[,14]+hexaco[,38]+hexaco[,50]
hexaco$prudence<- hexaco[,20]+ hexaco[,44]

#openess
hexaco$aesthetic<-hexaco[,25]
hexaco$inquisitiveness<-hexaco[,7]
hexaco$creativity<-hexaco[,13]+hexaco[,37]+ hexaco[,49]
hexaco$unconventionality<- +hexaco[,43]

#EFA
data<-hexaco[,61:82]
hex_cor<-cor(data)
hexaco_rm_all<-fa(r=hex_cor,n.obs = 64,nfactors = 6,rotate = "varimax")
hexaco_rm_all
summary(hexaco_rm_all)

#3.remove doubt sum
#humility
hexaco$sincerity<-hexaco[,6]+ hexaco[,30]+hexaco[,54]
hexaco$fairness<-hexaco[,12]+hexaco[,36]+hexaco[,60]
hexaco$greedavoidance<-hexaco[,18]+ hexaco[,42]
hexaco$modesty<-hexaco[,24]+ hexaco[,48]

#emotionality
hexaco$fearfulness<-hexaco[,5]+hexaco[,29]+ hexaco[,53]
hexaco$anxiety<-hexaco[,11]+ hexaco[,35]
hexaco$dependence<-hexaco[,17]+ hexaco[,41]
hexaco$sentimentality<-NULL

#extraversion
hexaco$selfesteem<-hexaco[,4]+ hexaco[,28]+ hexaco[,52]
hexaco$boldness<- hexaco[,10]+hexaco[,34]+hexaco[,58]
hexaco$socialbility<-NULL
hexaco$liveliness<-hexaco[,22]+ hexaco[,46]

#agreeableness
hexaco$forgiveness<-hexaco[,3]+hexaco[,27]
hexaco$gentleness<- hexaco[,9]+hexaco[,33]+hexaco[,51]
hexaco$flexibility<- hexaco[,15]+hexaco[,39]+ hexaco[,57]
hexaco$patience<- hexaco[,21]+hexaco[,45]

#conscientiousness
hexaco$organization<-hexaco[,2]+ hexaco[,26]
hexaco$diligence<-hexaco[,8]+ hexaco[,32]
hexaco$perfectionism<-NULL
hexaco$prudence<- NULL
#openess
hexaco$aesthetic<- hexaco[,1]+hexaco[,25]
hexaco$inquisitiveness<-hexaco[,7]+ hexaco[,31]
hexaco$creativity<-hexaco[,13]+hexaco[,37]+ hexaco[,49]
hexaco$unconventionality<- hexaco[,19]+hexaco[,43]+ hexaco[,55]

#EFA
data<-hexaco[,61:80]
hex_cor<-cor(data)
hexaco_rm_sum<-fa(r=hex_cor,n.obs = 64,nfactors = 6,rotate = "varimax")
hexaco_rm_sum

#4-8 rm one each time
#sum items
hexaco<-read.csv2(paste0(dir,'hexaco_pmm.csv'),sep =',' )
#humility
hexaco$sincerity<-hexaco[,6]+ hexaco[,30]+hexaco[,54]
hexaco$fairness<-hexaco[,12]+hexaco[,36]+hexaco[,60]
hexaco$greedavoidance<-hexaco[,18]+ hexaco[,42]
hexaco$modesty<-hexaco[,24]+ hexaco[,48]

#emotionality
hexaco$fearfulness<-hexaco[,5]+hexaco[,29]+ hexaco[,53]
hexaco$anxiety<-hexaco[,11]+ hexaco[,35]
hexaco$dependence<-hexaco[,17]+ hexaco[,41]
hexaco$sentimentality<-hexaco[,23]+hexaco[,47]+ hexaco[,59]

#extraversion
hexaco$selfesteem<-hexaco[,4]+ hexaco[,28]+ hexaco[,52]
hexaco$boldness<- hexaco[,10]+hexaco[,34]+hexaco[,58]
hexaco$socialbility<-hexaco[,16]+hexaco[,40]
hexaco$liveliness<-hexaco[,22]+ hexaco[,46]

#agreeableness
hexaco$forgiveness<-hexaco[,3]+hexaco[,27]
hexaco$gentleness<- hexaco[,9]+hexaco[,33]+hexaco[,51]
hexaco$flexibility<- hexaco[,15]+hexaco[,39]+ hexaco[,57]
hexaco$patience<- hexaco[,21]+hexaco[,45]

#conscientiousness
hexaco$organization<-hexaco[,2]+ hexaco[,26]
hexaco$diligence<-hexaco[,8]+ hexaco[,32]
hexaco$perfectionism<- hexaco[,14]+hexaco[,38]+hexaco[,50]
hexaco$prudence<- hexaco[,20]+ hexaco[,44]+ hexaco[,56]

#openess
hexaco$aesthetic<- hexaco[,1]+hexaco[,25]
hexaco$inquisitiveness<-hexaco[,7]+ hexaco[,31]
hexaco$creativity<-hexaco[,13]+hexaco[,37]+ hexaco[,49]
hexaco$unconventionality<- hexaco[,19]+hexaco[,43]+ hexaco[,55]

data<-hexaco[,61:84]
#rm sentimentality
data$sentimentality<-NULL
#EFA
hex_cor<-cor(data)
hexaco_rm_sentimentality<-fa(r=hex_cor,n.obs = 64,nfactors = 6,rotate = "varimax")
hexaco_rm_sentimentality
summary(hexaco_rm_sentimentality)

#rm socialbility
data$socialbility<-NULL
#EFA
hex_cor<-cor(data)
hexaco_rm_social<-fa(r=hex_cor,n.obs = 64,nfactors = 6,rotate = "varimax")
hexaco_rm_social

#rm socialbility
data$perfectionism<-NULL
#EFA
hex_cor<-cor(data)
hexaco_rm_perfect<-fa(r=hex_cor,n.obs = 64,nfactors = 6,rotate = "varimax")
hexaco_rm_perfect

#rm socialbility
data$prudence<-NULL
#EFA
hex_cor<-cor(data)
hexaco_rm_prudence<-fa(r=hex_cor,n.obs = 64,nfactors = 6,rotate = "varimax")
hexaco_rm_prudence

#rm 3
data$socialbility<-NULL
data$perfectionism<-NULL
data$prudence<-NULL
hex_cor<-cor(data)
hexaco_rm_soc_per_pru<-fa(r=hex_cor,n.obs = 64,nfactors = 6,rotate = "varimax")
hexaco_rm_soc_per_pru

#rm 2
data$socialbility<-NULL
data$prudence<-NULL
hex_cor<-cor(data)
hexaco_rm_soc_pru<-fa(r=hex_cor,n.obs = 64,nfactors = 6,rotate = "varimax")
hexaco_rm_soc_pru

#rm 2
data$socialbility<-NULL
data$perfectionism<-NULL
hex_cor<-cor(data)
hexaco_rm_soc_per<-fa(r=hex_cor,n.obs = 64,nfactors = 6,rotate = "varimax")
hexaco_rm_soc_per

#rm 2
data$prudence<-NULL
data$perfectionism<-NULL
hex_cor<-cor(data)
hexaco_rm_pru_per<-fa(r=hex_cor,n.obs = 64,nfactors = 6,rotate = "varimax")
hexaco_rm_pru_per

##rm 56
#humility
hexaco$sincerity<-hexaco[,6]+ hexaco[,30]+hexaco[,54]
hexaco$fairness<-hexaco[,12]+hexaco[,36]+hexaco[,60]
hexaco$greedavoidance<-hexaco[,18]+ hexaco[,42]
hexaco$modesty<-hexaco[,24]+ hexaco[,48]

#emotionality
hexaco$fearfulness<-hexaco[,5]+hexaco[,29]+ hexaco[,53]
hexaco$anxiety<-hexaco[,11]+ hexaco[,35]
hexaco$dependence<-hexaco[,17]+ hexaco[,41]
hexaco$sentimentality<-hexaco[,23]+hexaco[,47]+ hexaco[,59]

#extraversion
hexaco$selfesteem<-hexaco[,4]+ hexaco[,28]+ hexaco[,52]
hexaco$boldness<- hexaco[,10]+hexaco[,34]+hexaco[,58]
hexaco$socialbility<-hexaco[,16]+hexaco[,40]
hexaco$liveliness<-hexaco[,22]+ hexaco[,46]

#agreeableness
hexaco$forgiveness<-hexaco[,3]+hexaco[,27]
hexaco$gentleness<- hexaco[,9]+hexaco[,33]+hexaco[,51]
hexaco$flexibility<- hexaco[,15]+hexaco[,39]+ hexaco[,57]
hexaco$patience<- hexaco[,21]+hexaco[,45]

#conscientiousness
hexaco$organization<-hexaco[,2]+ hexaco[,26]
hexaco$diligence<-hexaco[,8]+ hexaco[,32]
hexaco$perfectionism<- hexaco[,14]+hexaco[,38]+hexaco[,50]
hexaco$prudence<- hexaco[,20]+ hexaco[,44]

#openess
hexaco$aesthetic<- hexaco[,1]+hexaco[,25]
hexaco$inquisitiveness<-hexaco[,7]+ hexaco[,31]
hexaco$creativity<-hexaco[,13]+hexaco[,37]+ hexaco[,49]
hexaco$unconventionality<- hexaco[,19]+hexaco[,43]+ hexaco[,55]

#EFA
data<-hexaco[,61:84]
hex_cor<-cor(data)
hexaco_rm_56<-fa(r=hex_cor,n.obs = 64,nfactors = 6,rotate = "varimax")
hexaco_rm_56
#summary(hexaco_all)


#rm 50
#humility
hexaco$sincerity<-hexaco[,6]+ hexaco[,30]+hexaco[,54]
hexaco$fairness<-hexaco[,12]+hexaco[,36]+hexaco[,60]
hexaco$greedavoidance<-hexaco[,18]+ hexaco[,42]
hexaco$modesty<-hexaco[,24]+ hexaco[,48]

#emotionality
hexaco$fearfulness<-hexaco[,5]+hexaco[,29]+ hexaco[,53]
hexaco$anxiety<-hexaco[,11]+ hexaco[,35]
hexaco$dependence<-hexaco[,17]+ hexaco[,41]
hexaco$sentimentality<-hexaco[,47]+ hexaco[,59]

#extraversion
hexaco$selfesteem<-hexaco[,4]+ hexaco[,28]+ hexaco[,52]
hexaco$boldness<- hexaco[,10]+hexaco[,34]+hexaco[,58]
hexaco$socialbility<-hexaco[,16]+hexaco[,40]
hexaco$liveliness<-hexaco[,22]+ hexaco[,46]

#agreeableness
hexaco$forgiveness<-hexaco[,3]+hexaco[,27]
hexaco$gentleness<- hexaco[,9]+hexaco[,33]+hexaco[,51]
hexaco$flexibility<- hexaco[,15]+hexaco[,39]+ hexaco[,57]
hexaco$patience<- hexaco[,21]+hexaco[,45]

#conscientiousness
hexaco$organization<-hexaco[,2]+ hexaco[,26]
hexaco$diligence<-hexaco[,8]+ hexaco[,32]
hexaco$perfectionism<- hexaco[,14]+hexaco[,38]
hexaco$prudence<- hexaco[,20]+ hexaco[,44]+ hexaco[,56]

#openess
hexaco$aesthetic<- hexaco[,1]+hexaco[,25]
hexaco$inquisitiveness<-hexaco[,7]+ hexaco[,31]
hexaco$creativity<-hexaco[,13]+hexaco[,37]+ hexaco[,49]
hexaco$unconventionality<- hexaco[,19]+hexaco[,43]+ hexaco[,55]

#EFA
data<-hexaco[,61:84]
hex_cor<-cor(data)
hexaco_rm_23<-fa(r=hex_cor,n.obs = 64,nfactors = 6,rotate = "varimax")
hexaco_rm_23
#summary(hexaco_all)

#rm 56, 50
hexaco<-read.csv2(paste0(dir,'hexaco_pmm.csv'),sep =',' )
hexaco$sincerity<-hexaco[,6]+ hexaco[,30]+hexaco[,54]
hexaco$fairness<-hexaco[,12]+hexaco[,36]+hexaco[,60]
hexaco$greedavoidance<-hexaco[,18]+ hexaco[,42]
hexaco$modesty<-hexaco[,24]+ hexaco[,48]

#emotionality
hexaco$fearfulness<-hexaco[,5]+hexaco[,29]+ hexaco[,53]
hexaco$anxiety<-hexaco[,11]+ hexaco[,35]
hexaco$dependence<-hexaco[,17]+ hexaco[,41]
hexaco$sentimentality<-hexaco[,23]+hexaco[,47]+ hexaco[,59]

#extraversion
hexaco$selfesteem<-hexaco[,4]+ hexaco[,28]+ hexaco[,52]
hexaco$boldness<- hexaco[,10]+hexaco[,34]+hexaco[,58]
hexaco$socialbility<-hexaco[,16]+hexaco[,40]
hexaco$liveliness<-hexaco[,22]+ hexaco[,46]

#agreeableness
hexaco$forgiveness<-hexaco[,3]+hexaco[,27]
hexaco$gentleness<- hexaco[,9]+hexaco[,33]+hexaco[,51]
hexaco$flexibility<- hexaco[,15]+hexaco[,39]+ hexaco[,57]
hexaco$patience<- hexaco[,21]+hexaco[,45]

#conscientiousness
hexaco$organization<-hexaco[,2]+ hexaco[,26]
hexaco$diligence<-hexaco[,8]+ hexaco[,32]
hexaco$perfectionism<- hexaco[,14]+hexaco[,38]
hexaco$prudence<- hexaco[,20]+ hexaco[,44]

#openess
hexaco$aesthetic<- hexaco[,1]+hexaco[,25]
hexaco$inquisitiveness<-hexaco[,7]+ hexaco[,31]
hexaco$creativity<-hexaco[,13]+hexaco[,37]+ hexaco[,49]
hexaco$unconventionality<- hexaco[,19]+hexaco[,43]+ hexaco[,55]

#EFA
data<-hexaco[,61:84]
hex_cor<-cor(data)
hexaco_rm_50_56<-fa(r=hex_cor,n.obs = 64,nfactors = 6,rotate = "varimax")
hexaco_rm_50_56

#rm 56 16
#humility
hexaco<-read.csv2(paste0(dir,'hexaco_pmm.csv'),sep =',' )
hexaco$sincerity<-hexaco[,6]+ hexaco[,30]+hexaco[,54]
hexaco$fairness<-hexaco[,12]+hexaco[,36]+hexaco[,60]
hexaco$greedavoidance<-hexaco[,18]+ hexaco[,42]
hexaco$modesty<-hexaco[,24]+ hexaco[,48]

#emotionality
hexaco$fearfulness<-hexaco[,5]+hexaco[,29]+ hexaco[,53]
hexaco$anxiety<-hexaco[,11]+ hexaco[,35]
hexaco$dependence<-hexaco[,17]+ hexaco[,41]
hexaco$sentimentality<-hexaco[,23]+hexaco[,47]+ hexaco[,59]

#extraversion
hexaco$selfesteem<-hexaco[,4]+ hexaco[,28]+ hexaco[,52]
hexaco$boldness<- hexaco[,10]+hexaco[,34]+hexaco[,58]
hexaco$socialbility<-hexaco[,40]
hexaco$liveliness<-hexaco[,22]+ hexaco[,46]

#agreeableness
hexaco$forgiveness<-hexaco[,3]+hexaco[,27]
hexaco$gentleness<- hexaco[,9]+hexaco[,33]+hexaco[,51]
hexaco$flexibility<- hexaco[,15]+hexaco[,39]+ hexaco[,57]
hexaco$patience<- hexaco[,21]+hexaco[,45]

#conscientiousness
hexaco$organization<-hexaco[,2]+ hexaco[,26]
hexaco$diligence<-hexaco[,8]+ hexaco[,32]
hexaco$perfectionism<- hexaco[,14]+hexaco[,38]+hexaco[,50]
hexaco$prudence<- hexaco[,20]+ hexaco[,44]

#openess
hexaco$aesthetic<- hexaco[,1]+hexaco[,25]
hexaco$inquisitiveness<-hexaco[,7]+ hexaco[,31]
hexaco$creativity<-hexaco[,13]+hexaco[,37]+ hexaco[,49]
hexaco$unconventionality<- hexaco[,19]+hexaco[,43]+ hexaco[,55]

#EFA
data<-hexaco[,61:84]
hex_cor<-cor(data)
hexaco_rm_56_16<-fa(r=hex_cor,n.obs = 64,nfactors = 6,rotate = "varimax")
hexaco_rm_56_16
summary(hexaco_all)

#rm 56 40
#humility
hexaco<-read.csv2(paste0(dir,'hexaco_pmm.csv'),sep =',' )
hexaco$sincerity<-hexaco[,6]+ hexaco[,30]+hexaco[,54]
hexaco$fairness<-hexaco[,12]+hexaco[,36]+hexaco[,60]
hexaco$greedavoidance<-hexaco[,18]+ hexaco[,42]
hexaco$modesty<-hexaco[,24]+ hexaco[,48]

#emotionality
hexaco$fearfulness<-hexaco[,5]+hexaco[,29]+ hexaco[,53]
hexaco$anxiety<-hexaco[,11]+ hexaco[,35]
hexaco$dependence<-hexaco[,17]+ hexaco[,41]
hexaco$sentimentality<-hexaco[,23]+hexaco[,47]+ hexaco[,59]

#extraversion
hexaco$selfesteem<-hexaco[,4]+ hexaco[,28]+ hexaco[,52]
hexaco$boldness<- hexaco[,10]+hexaco[,34]+hexaco[,58]
hexaco$socialbility<-hexaco[,16]
hexaco$liveliness<-hexaco[,22]+ hexaco[,46]

#agreeableness
hexaco$forgiveness<-hexaco[,3]+hexaco[,27]
hexaco$gentleness<- hexaco[,9]+hexaco[,33]+hexaco[,51]
hexaco$flexibility<- hexaco[,15]+hexaco[,39]+ hexaco[,57]
hexaco$patience<- hexaco[,21]+hexaco[,45]

#conscientiousness
hexaco$organization<-hexaco[,2]+ hexaco[,26]
hexaco$diligence<-hexaco[,8]+ hexaco[,32]
hexaco$perfectionism<- hexaco[,14]+hexaco[,38]+hexaco[,50]
hexaco$prudence<- hexaco[,20]+ hexaco[,44]

#openess
hexaco$aesthetic<- hexaco[,1]+hexaco[,25]
hexaco$inquisitiveness<-hexaco[,7]+ hexaco[,31]
hexaco$creativity<-hexaco[,13]+hexaco[,37]+ hexaco[,49]
hexaco$unconventionality<- hexaco[,19]+hexaco[,43]+ hexaco[,55]

#EFA
data<-hexaco[,61:84]
hex_cor<-cor(data)
hexaco_rm_56_40<-fa(r=hex_cor,n.obs = 64,nfactors = 6,rotate = "varimax")
hexaco_rm_56_40

#rm 56 40 50
#humility
hexaco<-read.csv2(paste0(dir,'hexaco_pmm.csv'),sep =',' )
hexaco$sincerity<-hexaco[,6]+ hexaco[,30]+hexaco[,54]
hexaco$fairness<-hexaco[,12]+hexaco[,36]+hexaco[,60]
hexaco$greedavoidance<-hexaco[,18]+ hexaco[,42]
hexaco$modesty<-hexaco[,24]+ hexaco[,48]

#emotionality
hexaco$fearfulness<-hexaco[,5]+hexaco[,29]+ hexaco[,53]
hexaco$anxiety<-hexaco[,11]+ hexaco[,35]
hexaco$dependence<-hexaco[,17]+ hexaco[,41]
hexaco$sentimentality<-hexaco[,23]+hexaco[,47]+ hexaco[,59]

#extraversion
hexaco$selfesteem<-hexaco[,4]+ hexaco[,28]+ hexaco[,52]
hexaco$boldness<- hexaco[,10]+hexaco[,34]+hexaco[,58]
hexaco$socialbility<-hexaco[,16]
hexaco$liveliness<-hexaco[,22]+ hexaco[,46]

#agreeableness
hexaco$forgiveness<-hexaco[,3]+hexaco[,27]
hexaco$gentleness<- hexaco[,9]+hexaco[,33]+hexaco[,51]
hexaco$flexibility<- hexaco[,15]+hexaco[,39]+ hexaco[,57]
hexaco$patience<- hexaco[,21]+hexaco[,45]

#conscientiousness
hexaco$organization<-hexaco[,2]+ hexaco[,26]
hexaco$diligence<-hexaco[,8]+ hexaco[,32]
hexaco$perfectionism<- hexaco[,14]+hexaco[,38]
hexaco$prudence<- hexaco[,20]+ hexaco[,44]

#openess
hexaco$aesthetic<- hexaco[,1]+hexaco[,25]
hexaco$inquisitiveness<-hexaco[,7]+ hexaco[,31]
hexaco$creativity<-hexaco[,13]+hexaco[,37]+ hexaco[,49]
hexaco$unconventionality<- hexaco[,19]+hexaco[,43]+ hexaco[,55]

#EFA
data<-hexaco[,61:84]
hex_cor<-cor(data)
hexaco_rm_56_40_50<-fa(r=hex_cor,n.obs = 64,nfactors = 6,rotate = "varimax")
hexaco_rm_56_40_50

#rm 56 40 16
#humility
hexaco<-read.csv2(paste0(dir,'hexaco_pmm.csv'),sep =',' )
hexaco$sincerity<-hexaco[,6]+ hexaco[,30]+hexaco[,54]
hexaco$fairness<-hexaco[,12]+hexaco[,36]+hexaco[,60]
hexaco$greedavoidance<-hexaco[,18]+ hexaco[,42]
hexaco$modesty<-hexaco[,24]+ hexaco[,48]

#emotionality
hexaco$fearfulness<-hexaco[,5]+hexaco[,29]+ hexaco[,53]
hexaco$anxiety<-hexaco[,11]+ hexaco[,35]
hexaco$dependence<-hexaco[,17]+ hexaco[,41]
hexaco$sentimentality<-hexaco[,23]+hexaco[,47]+ hexaco[,59]

#extraversion
hexaco$selfesteem<-hexaco[,4]+ hexaco[,28]+ hexaco[,52]
hexaco$boldness<- hexaco[,10]+hexaco[,34]+hexaco[,58]
hexaco$socialbility<-NULL
hexaco$liveliness<-hexaco[,22]+ hexaco[,46]

#agreeableness
hexaco$forgiveness<-hexaco[,3]+hexaco[,27]
hexaco$gentleness<- hexaco[,9]+hexaco[,33]+hexaco[,51]
hexaco$flexibility<- hexaco[,15]+hexaco[,39]+ hexaco[,57]
hexaco$patience<- hexaco[,21]+hexaco[,45]

#conscientiousness
hexaco$organization<-hexaco[,2]+ hexaco[,26]
hexaco$diligence<-hexaco[,8]+ hexaco[,32]
hexaco$perfectionism<- hexaco[,14]+hexaco[,38]+hexaco[,50]
hexaco$prudence<- hexaco[,20]+ hexaco[,44]

#openess
hexaco$aesthetic<- hexaco[,1]+hexaco[,25]
hexaco$inquisitiveness<-hexaco[,7]+ hexaco[,31]
hexaco$creativity<-hexaco[,13]+hexaco[,37]+ hexaco[,49]
hexaco$unconventionality<- hexaco[,19]+hexaco[,43]+ hexaco[,55]

#EFA
data<-hexaco[,61:83]
hex_cor<-cor(data)
hexaco_rm_56_40_16<-fa(r=hex_cor,n.obs = 64,nfactors = 6,rotate = "varimax")
hexaco_rm_56_40_16



##CFA rm 56 40
hex_cor_56_40<-cor(data)

hex_loadings<-data.frame(abs(unclass(hexaco_rm_56_40$loadings)))
hex_loadings<-data.frame(t(apply(hex_loadings,1,function(x) replace(x,x==max(x),100))))
formula<-c()
factors<-colnames(hex_loadings)
variables<-colnames(hex_cor_56_40)
for(i in 1:24){
  for(j in 1:6){
    if(hex_loadings[i,j]==100){
      formula<-c(formula,paste0(factors[j]," -> ",variables[i],",","theta",as.character(i),",NA"),"\n")    
    }
  }
}
for(j in 1:6){
  formula<-c(formula,paste0(factors[j]," <-> ",factors[j],",NA,1"),"\n")
}

cfa_model1<-specifyModel(text="MR4 -> sincerity,theta1,NA 
 MR3 -> fairness,theta2,NA 
                         MR6 -> greedavoidance,theta3,NA 
                         MR5 -> modesty,theta4,NA 
                         MR2 -> fearfulness,theta5,NA 
                         MR6 -> anxiety,theta6,NA 
                         MR2 -> dependence,theta7,NA 
                         MR3 -> sentimentality,theta8,NA 
                         MR1 -> selfesteem,theta9,NA 
                         MR5 -> boldness,theta10,NA 
                         MR4 -> socialbility,theta11,NA 
                         MR1 -> liveliness,theta12,NA 
                         MR1 -> forgiveness,theta13,NA 
                         MR1 -> gentleness,theta14,NA 
                         MR2 -> flexibility,theta15,NA 
                         MR3 -> patience,theta16,NA 
                         MR3 -> organization,theta17,NA 
                         MR3 -> diligence,theta18,NA 
                         MR4 -> perfectionism,theta19,NA 
                         MR4 -> prudence,theta20,NA 
                         MR6 -> aesthetic,theta21,NA 
                         MR1 -> inquisitiveness,theta22,NA 
                         MR6 -> creativity,theta23,NA 
                         MR1 -> unconventionality,theta24,NA 
                         MR3 <-> MR3,NA,1 
                         MR1 <-> MR1,NA,1 
                         MR4 <-> MR4,NA,1 
                         MR5 <-> MR5,NA,1 
                         MR2 <-> MR2,NA,1 
                         MR6 <-> MR6,NA,1 ")
N<-64
cfa1.fit <- sem(cfa_model1, S=hex_cor_56_40,N=N)
summary(cfa1.fit)
AIC(cfa1.fit)
BIC(cfa1.fit)
mean(normalizedResiduals(cfa1.fit))
