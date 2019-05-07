# Interview Privacy Factor Analysis  #
# liyixi #
# 2018.10.9 #

library(readr)
library(psych)
library(sem)
library(dplyr)
library(mice)
dir<-'/Users/Loielaine/Desktop/Good_Good_Study/2018Survey project/data/'

#all items
#hexaco
data<-read_csv(paste0(dir,"hexaco_rv.csv"))
ini <- mice(data,maxit = 0)
#pred_bidr<-ini$predictorMatrix
#method_bidr<-c(rep("",5),rep("pmm",19))
imp1<-mice(data,seed = 12345,m=1)
imp1$method
data_complete1<-complete(imp1)
summary(data_complete1)
write_csv(data_complete1,paste0(dir,"hexaco_pmm.csv"))

#ssi
data<-read_csv(paste0(dir,"ssi_rv.csv"))
ini <- mice(data,maxit = 0)
#pred_bidr<-ini$predictorMatrix
#method_bidr<-c(rep("",5),rep("pmm",19))
imp1<-mice(data,seed = 12345,m=1)
imp1$method
data_complete1<-complete(imp1)
summary(data_complete1)
write_csv(data_complete1,paste0(dir,"ssi_pmm.csv"))

#sum
#ssi
data<-read_csv(paste0(dir,"ssi.csv"))
ini <- mice(data,maxit = 0)
#pred_bidr<-ini$predictorMatrix
#method_bidr<-c(rep("",5),rep("pmm",19))
imp1<-mice(data,seed = 12345,m=1)
imp1$method
data_complete1<-complete(imp1)
summary(data_complete1)
write_csv(data_complete1,paste0(dir,"ssi_sum_pmm.csv"))

#hexaco
data<-read_csv(paste0(dir,"hexaco.csv"))
ini <- mice(data,maxit = 0)
#pred_bidr<-ini$predictorMatrix
#method_bidr<-c(rep("",5),rep("pmm",19))
imp1<-mice(data,seed = 12345,m=1)
imp1$method
data_complete1<-complete(imp1)
summary(data_complete1)
write_csv(data_complete1,paste0(dir,"hexaco_sum_pmm.csv"))


