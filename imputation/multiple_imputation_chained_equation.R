# Interview Privacy Factor Analysis  #
# liyixi #
# 2018.11.6 #

library(readr)
library(psych)
library(mice)
dir<-'/Users/Loielaine/Desktop/Good_Good_Study/2018Survey project/data/'
data<-read.csv2(paste0(dir,'behavior_inverse.csv'),sep =',',na.strings=c("","NA"))[,1:19]
demo<-read.csv2(paste0(dir,'demo.csv'),sep =',',na.strings=c("","NA"))
demo[demo$gender==3,"gender"]<-0 #male
demo[demo$gender==5,"gender"]<-1 #female
demo[demo$jobnow==1,"jobnow"]<-0 #employed
demo[demo$jobnow==2,"jobnow"]<-1 #unemployed
data$gender<-demo$gender 
data$educ<-demo$educ
data$marital<-demo$marital
data$employment<-demo$jobnow
data$age<-2018-demo$birth
data[(data$age)>100,"age"]<-NA
data[data<0]<-NA
#data<-apply(data,2,as.numeric)

#initialize
data_raw<-data.frame(data)
#data_raw[,1:23]<-apply(data_raw[,1:23],2,factor)
demo_raw<-data.frame(data_raw[,20:24])
#data_srmi<-data_raw


##demographic imputation
ini <- mice(demo_raw,maxit = 0)

pred1 <- ini$predictorMatrix
pred1[,"employment"]<-c(1,0,1,0,0)
pred1[,"educ"]<-c(1,0,1,1,0)
pred1[,"age"]<-c(1,1,1,1,0)
#method<-c("","polyreg","","polyreg","norm")
method1<-c("","pmm","","pmm","pmm")
imp1<-mice(demo_raw,method = method1, predictorMatrix= pred1,seed = 12345,m=25)
demo_complete1<-complete(imp1)

pred2 <- ini$predictorMatrix
method2<-c("","pmm","","pmm","pmm")
imp2<-mice(demo_raw,method = method2, predictorMatrix= pred2,seed = 12345,m=25)
demo_complete2<-complete(imp2)

write_csv(demo_complete1,paste0(dir,"demo_complete1.csv"))
write_csv(demo_complete2,paste0(dir,"demo_complete2.csv"))

##bidr imputation
#use demo_complete2
data_raw$gender<-demo_complete2$gender 
data_raw$educ<-demo_complete2$educ
data_raw$marital<-demo_complete2$marital
data_raw$employment<-demo_complete2$employment
data_raw$age<-demo_complete2$age

write_csv(data_raw,paste0(dir,"demo_bidr_raw.csv"))

#order1
ini <- mice(data_raw,maxit = 0)
pred_bidr<-ini$predictorMatrix
method_bidr<-ini$method
imp_bidr<-mice(data_raw,method = method_bidr, predictorMatrix= pred_bidr,seed = 12345,m=5)
imp_bidr$imp$bidr22
data_complete<-complete(imp_bidr)
write_csv(data_complete,paste0(dir,"demo_bidr_complete.csv"))
summary(data_complete)

#order2
data_reorder_raw<-read.csv2(paste0(dir,'demo_bidr_raw_reorder.csv'),sep =',')
col <-c(names(data_reorder_raw))
data_reorder_raw<-demo_complete2
for( i in 1:19){
  data_reorder_raw[,col[20-i]] <- data_raw[,col[20-i]]
}

##comparision of the results
missing_index<-rownames(data_raw[rowSums(is.na(data_raw))>0,])
imputation<-data_complete2[missing_index,]
imputation_female<-imputation[imputation$gender==1,]
imputation_male<-imputation[imputation$gender==0,]
imputation<-apply(imputation, 2,as.numeric)
imputation_female<-apply(imputation_female, 2,as.numeric)
imputation_male<-apply(imputation_male, 2,as.numeric)
write_csv(imputation,paste0(dir,"demo_bidr_imputation.csv"))
data.frame(mean=apply(imputation,2,mean),std=apply(imputation,2,sd))
data.frame(mean=apply(imputation_female,2,mean),std=apply(imputation_female,2,sd))
data.frame(mean=apply(imputation_male,2,mean),std=apply(imputation_male,2,sd))

#imputation<-data_complete2[missing_index,]
data_complete2_female<-data_complete2[data_complete2$gender==1,]
data_complete2_male<-data_complete2[data_complete2$gender==0,]
data_complete2<-apply(data_complete2, 2,as.numeric)
data_complete2_female<-apply(data_complete2_female, 2,as.numeric)
data_complete2_male<-apply(data_complete2_male, 2,as.numeric)
#write_csv(imputation,paste0(dir,"demo_bidr_imputation.csv"))
mean<-data.frame(apply(data_complete2,2,mean),apply(data_complete2,2,sd),apply(data_complete2_male,2,mean),apply(data_complete2_male,2,sd),apply(data_complete2_female,2,mean),apply(data_complete2_female,2,sd))
write_csv(mean,paste0(dir,"mean.csv"))


##11/12/2018 update:
##bidr imputation by two methods:pmm & polr (m=1)
#pmm
ini <- mice(data_reorder_raw,maxit = 0)
pred_bidr<-ini$predictorMatrix
method_bidr<-c(rep("",5),rep("pmm",19))
imp_bidr1<-mice(data_reorder_raw,method = method_bidr, predictorMatrix= pred_bidr,seed = 12345,m=1)
data_complete1<-complete(imp_bidr1)
summary(data_complete1)
imputation1<-data_complete1[missing_index,]
summary(imputation1)
write_csv(data_complete1,paste0(dir,"demo_bidr_complete_pmm.csv"))
write_csv(imputation,paste0(dir,"demo_bidr_imputation_pmm.csv"))

#polr
ini <- mice(data_reorder_raw,maxit = 0)
pred_bidr<-ini$predictorMatrix
method_bidr<-c(rep("",5),rep("polr",19))
imp_bidr2<-mice(data_reorder_raw,method = method_bidr, predictorMatrix= pred_bidr,seed = 12345,m=1)
data_complete2<-complete(imp_bidr2)
summary(data_complete2)
imputation2<-data_complete2[missing_index,]
summary(imputation2)
write_csv(data_complete2,paste0(dir,"demo_bidr_complete_polr.csv"))
write_csv(imputation2,paste0(dir,"demo_bidr_imputation_polr.csv"))

##comparision of two imputation
#wilcox.test
p<-c()
d<-c()
imputation1_n <-apply(imputation1[,6:24],2,as.numeric)
imputation2_n <-apply(imputation2[,6:24],2,as.numeric)
for(i in 1:19){
 #d[i]<-ks.test(imputation1_n[,i],imputation2_n[,i],simulate.p.value = T)$statistics
 #format.pval(p[i]<-ks.test(imputation1_n[,i],imputation2_n[,i],simulate.p.value = T)$p.value)
  p[i]<-wilcox.test(imputation1_n[,i],imputation2_n[,i],simulate.p.value = T,conf.int = T)$p.value
  d[i]<-wilcox.test(imputation1_n[,i],imputation2_n[,i],simulate.p.value = T,conf.int = T)$estimate
}
print(p)

#anova test for group difference 
for(i in 1:19){
compare1<-data.frame(as.numeric(imputation1[,5+i]))
for(l in 1:7){
 compare1[paste("l",l,sep="")] <- ifelse(compare1[,1]==l,1,0)
}
compare1$group<-1
colnames(compare1) <-c('Y','l1','l2','l3','l4','l5','l6','l7','group')
compare2<-data.frame(as.numeric(imputation2[,5+i]))
for(l in 1:7){
  compare2[paste("l",l,sep="")] <- ifelse(compare2[,1]==l,1,0)
}
compare2$group<-2
colnames(compare2) <-c('Y','l1','l2','l3','l4','l5','l6','l7','group')
anova_data<-rbind(compare1,compare2)
anova_data<-data.frame(apply(anova_data,2,as.numeric))
fit <- aov(Y ~ l2+l3+l4+l5+l6+l7+group+l2*group+l3*group+l4*group+l5*group+l6*group+l7*group,data=anova_data)
print(colnames(imputation1)[i+5])
print(summary(fit))
print("-----------------------------------------------------------")
}

for(i in 1:19){
  compare1<-data.frame(as.numeric(imputation1[,5+i]))
  compare1$group<-1
  colnames(compare1) <-c('Y','group')
  compare2<-data.frame(as.numeric(imputation2[,5+i]))
  compare2$group<-2
  colnames(compare2) <-c('Y','group')
  anova_data<-rbind(compare1,compare2)
  anova_data<-data.frame(apply(anova_data,2,as.numeric))
  fit <- aov(Y ~ group,data=anova_data)
  print(colnames(imputation1)[i+5])
  print(summary(fit))
  print("-----------------------------------------------------------")
}



#anova test for gender difference  
for(i in 1:19){
  compare1<-imputation1[,1:5]
  compare1$Y <-imputation1[,5+i]
  compare1$group<-1
  colnames(compare1) <-c('gender','educ','marital','employment','age', 'Y','group')
  compare2<-imputation2[,1:5]
  compare2$Y <-imputation2[,5+i]
  compare2$group<-2
  colnames(compare2) <-c('gender','educ','marital','employment','age', 'Y','group')
  anova_data<-rbind(compare1,compare2)
  anova_data<-data.frame(apply(anova_data,2,as.numeric))
  fit <- aov(Y ~ gender+educ+marital+employment+age+group+gender*group+educ*group+marital*group+employment*group+age*group,data=anova_data)
  print(colnames(imputation1)[i+5])
  print(summary(fit))
  print("-----------------------------------------------------------")
}

for(i in 1:19){
  compare1<-imputation1[,1:5]
  compare1$Y <-imputation1[,5+i]
  compare1$group<-1
  colnames(compare1) <-c('gender','educ','marital','employment','age', 'Y','group')
  compare2<-imputation2[,1:5]
  compare2$Y <-imputation2[,5+i]
  compare2$group<-2
  colnames(compare2) <-c('gender','educ','marital','employment','age', 'Y','group')
  anova_data<-rbind(compare1,compare2)
  anova_data<-data.frame(apply(anova_data,2,as.numeric))
  fit <- aov(Y ~ gender+educ+marital+employment+age+group,data=anova_data)
  print(colnames(imputation1)[i+5])
  print(summary(fit))
  print(fit$coefficients)
  print("-----------------------------------------------------------")
}

#complete data--polr
for(i in 1:19){
  compare2<-data_complete2[,1:5]
  compare2$Y <-data_complete2[,5+i]
  colnames(compare2) <-c('gender','educ','marital','employment','age', 'Y')
  anova_data<-data.frame(apply(compare2,2,as.numeric))
  fit <- aov(Y ~ gender+educ+marital+employment+age+age*marital+age*educ+age*employment+employment*educ,data=anova_data)
  print(colnames(imputation1)[i+5])
  print(summary(fit))
  print(fit$coefficients)
  print("-----------------------------------------------------------")
}
