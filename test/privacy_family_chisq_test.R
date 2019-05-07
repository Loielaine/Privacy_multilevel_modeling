library(readr)
library(psych)
dir<-'/Users/Loielaine/Desktop/Good_Good_Study/2018Survey project/data/'
data<-read.csv2(paste0(dir,'interviewee_complete.csv'),sep =',')
data<-read.csv2(paste0(dir,'interviewee_complete.csv'),sep =',')
family_var<-c("friend_diff_categorical","no_companion_categorical","traditions_categorical","rltp_partents_categorical",
              "rltp_spouse_categorical","rltp_siblings_categorical", "secret_spouse_categorical","secret_parent_categorical")

family<-data[,family_var]

#family continuous
family_continuous_var<-c("friend_diff","no_companion","traditions","rltp_partents",
                         "rltp_spouse","rltp_siblings", "secret_spouse","secret_parent")
family_c<-data[,family_continuous_var]
family_c$friend_diff[(family_c$friend_diff<0)]<-NA
family_c$no_companion[(family_c$no_companion<0)]<-NA
family_c$traditions[(family_c$traditions<0)]<-NA

for(c in c(4,5,6)){
  family_c[,c][family_c[,c]==97]<-NA
  family_c[,c][family_c[,c]<0]<-NA
}

#secret
for(c in c(7,8)){
  family_c[,c][family_c[,c]<0]<-NA
  family_c[,c][family_c[,c]==7]<-NA
}

privacy<-read.csv2(paste0(dir,"privacy.csv"),sep=",")
io<-colnames(privacy)

summary<-data.frame(matrix(nrow=10,ncol=32))
rownames(summary)<-c("io_parent=1","io_parent=0","io_spouse=1","io_spouse=0","io_siblings=1","io_siblings=0","io_son_daughter=1","io_son_daughter=0","io_parentinlaw=1","io_parentinlaw=0")
colnames(summary)<-rep(c("count",'mean','se','p-value'),8)
options(digits=4)
for(i in 1:5){
  print(paste0("------------------------",io[i],"---------------------"))
  for(j in 1:8){
    print(family_continuous_var[j])
    cat("\n")
    subdata<-cbind(privacy[,i],family_c[,j])
    subdata<-data.frame(subdata[complete.cases(subdata),])
    subdata[,1]<-as.factor(subdata[,1])
    mean<-matrix(tapply(subdata$X2,subdata$X1,mean))
    se<-matrix(tapply(subdata$X2,subdata$X1,sd))
    summary[2*i-1,4*j-2]<-mean[1]
    summary[2*i,4*j-2]<-mean[2]
    summary[2*i-1,4*j-1]<-se[1]
    summary[2*i,4*j-1]<-se[2]
  
    aov<-aov(X2 ~ X1, data = subdata)
    s<-data.frame(t(unlist(summary(aov))))
    summary[2*i-1,4*j]<-round(s$Pr..F.1,3)
    if(s$Pr..F.1<=0.05){
      summary[2*i,4*j]<-'sig'
    }else{
      summary[2*i,4*j]<-'not sig'
    }
  }
}
write.csv(summary,file=paste0(dir,'privacy_family_test.csv'))

#update
privacy[,'present']<-apply(privacy[,1:5],1,sum)
privacy[,'present_c']<-0
for(i in 1:1502){
  if(privacy[i,'present']>0){
    privacy[i,'present_c']<-1
  }
}
summary<-data.frame(matrix(nrow=2,ncol=44))
colnames(summary)<-rep(c("count",'mean','se',"test"),11)
rownames(summary)<-c('not present','present')
for(j in 1:11){
  subdata<-cbind(privacy[,'present_c'],family_c[,j])
  subdata<-data.frame(subdata[complete.cases(subdata),])
  summary[2*i-1,4*j-3]<-nrow(subdata[subdata$X1==0,])
  summary[2*i,4*j-3]<-nrow(subdata[subdata$X1==1,])
  subdata[,1]<-as.factor(subdata[,1])
  mean<-matrix(tapply(subdata$X2,subdata$X1,mean))
  se<-matrix(tapply(subdata$X2,subdata$X1,sd))
  summary[1,4*j-2]<-mean[1]
  summary[2,4*j-2]<-mean[2]
  summary[1,4*j-1]<-se[1]
  summary[2,4*j-1]<-se[2]
  aov<-aov(X2 ~ X1, data = subdata)
  s<-data.frame(t(unlist(summary(aov))))
  summary[1,4*j]<-round(s$Pr..F.1,3)
  if(s$Pr..F.1<=0.05){
    summary[2,4*j]<-'sig'
  }else{
    summary[2,4*j]<-'not sig'
  }
}
write.csv(summary,file=paste0(dir,'privacy_family_test0326.csv'))

