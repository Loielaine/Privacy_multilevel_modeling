# Interview Privacy Factor Analysis  #
# liyixi #
# 2018.10.9 #

library(readr)
library(psych)
library(sem)
dir<-'/Users/Loielaine/Desktop/Good_Good_Study/2018Survey project/data/'
data<-read.csv2(paste0(dir,'behavior_inverse.csv'),sep =',')[,1:19]
data<-apply(data,2,as.numeric)
data[is.null(data)]<-NA

data_complete<-data[complete.cases(data),]
summary(data_complete)

#correlation
bidr_cor<-cor(data_complete)
write.csv(bidr_cor,file=paste0(dir,'bidr_cor.csv'))

#Exploratory Factor Analysis(EFA)
bidr_factors<-fa(r=bidr_cor,nfactors = 2)
#bidr_factors$values
#bidr_factors$e.values
bidr_factors


#Confirmatory Factor Analysis(CFA)
#2 factors CFA
cfa_model1<- specifyModel(text = "
##Factor1##
F1 -> bidr21 ,theta01,NA
F1 -> bidr23 ,theta02,NA
F1 -> bidr25 ,theta03,NA
F1 -> bidr27 ,theta04,NA
F1 -> bidr29 ,theta05,NA
F1 -> bidr31 ,theta06,NA
F1 -> bidr33 ,theta07,NA
F1 -> bidr35 ,theta08,NA
F1 -> bidr36 ,theta09,NA
F1 -> bidr37 ,theta10,NA
F1 -> bidr38 ,theta11,NA
F1 -> bidr39 ,theta12,NA
F1 -> bidr40 ,theta13,NA
##Factor 2##
F2 -> bidr22 ,theta14,NA
F2 -> bidr24 ,theta15,NA
F2 -> bidr26 ,theta16,NA
F2 -> bidr28 ,theta17,NA
F2 -> bidr30 ,theta18,NA
F2 -> bidr32 ,theta19,NA
##Unique variance##
bidr21 <-> bidr21 ,uvar01,NA
bidr23 <-> bidr23 ,uvar02,NA
bidr25 <-> bidr25 ,uvar03,NA
bidr27 <-> bidr27 ,uvar04,NA
bidr29 <-> bidr29 ,uvar05,NA
bidr31 <-> bidr31 ,uvar06,NA
bidr33 <-> bidr33 ,uvar07,NA
bidr35 <-> bidr35 ,uvar08,NA
bidr36 <-> bidr36 ,uvar09,NA
bidr37 <-> bidr37 ,uvar10,NA
bidr38 <-> bidr38 ,uvar11,NA
bidr39 <-> bidr39 ,uvar12,NA
bidr40 <-> bidr40 ,uvar13,NA
bidr22 <-> bidr22 ,uvar14,NA
bidr24 <-> bidr24 ,uvar15,NA
bidr26 <-> bidr26 ,uvar16,NA
bidr28 <-> bidr28 ,uvar17,NA
bidr30 <-> bidr30 ,uvar18,NA
bidr32 <-> bidr32 ,uvar19,NA
##Factor varaince fixed at 1##
F1 <-> F1, NA, 1
F2 <-> F2, NA, 1
")
N<-1171
cfa1.fit <- sem(cfa_model1,bidr_cor,N)
summary(cfa1.fit)
cfa1.fit$A
modIndices(cfa1.fit)

#3 factors CFA
cfa_model2<- specifyModel(text = "
                          ##Factor1##
                          F1 -> bidr21 ,theta01,NA
                          F1 -> bidr23 ,theta02,NA
                          F1 -> bidr27 ,theta03,NA
                          F1 -> bidr31 ,theta04,NA
                          F1 -> bidr33 ,theta05,NA
                          F1 -> bidr35 ,theta06,NA
                          F1 -> bidr36 ,theta07,NA
                          F1 -> bidr37 ,theta08,NA
                          F1 -> bidr39 ,theta09,NA
                          F1 -> bidr40 ,theta10,NA
                          ##Factor 2##
                          F2 -> bidr22 ,theta11,NA
                          F2 -> bidr24 ,theta12,NA
                          F2 -> bidr26 ,theta13,NA
                          F2 -> bidr28 ,theta14,NA
                          F2 -> bidr30 ,theta15,NA
                          F2 -> bidr32 ,theta16,NA
                          ##Factor 3##
                          F3 -> bidr25 ,theta17,NA
                          F3 -> bidr29 ,theta18,NA
                          F3 -> bidr38 ,theta19,NA
                          ##Unique variance##
                          bidr21 <-> bidr21 ,uvar01,NA
                          bidr23 <-> bidr23 ,uvar02,NA
                          bidr25 <-> bidr25 ,uvar03,NA
                          bidr27 <-> bidr27 ,uvar04,NA
                          bidr29 <-> bidr29 ,uvar05,NA
                          bidr31 <-> bidr31 ,uvar06,NA
                          bidr33 <-> bidr33 ,uvar07,NA
                          bidr35 <-> bidr35 ,uvar08,NA
                          bidr36 <-> bidr36 ,uvar09,NA
                          bidr37 <-> bidr37 ,uvar10,NA
                          bidr38 <-> bidr38 ,uvar11,NA
                          bidr39 <-> bidr39 ,uvar12,NA
                          bidr40 <-> bidr40 ,uvar13,NA
                          bidr22 <-> bidr22 ,uvar14,NA
                          bidr24 <-> bidr24 ,uvar15,NA
                          bidr26 <-> bidr26 ,uvar16,NA
                          bidr28 <-> bidr28 ,uvar17,NA
                          bidr30 <-> bidr30 ,uvar18,NA
                          bidr32 <-> bidr32 ,uvar19,NA
                          ##Factor varaince fixed at 1##
                          F1 <-> F1, NA, 1
                          F2 <-> F2, NA, 1
                          F3 <-> F3, NA, 1
                          ")
N<-1171
cfa2.fit <- sem(cfa_model2,bidr_cor,N)
summary(cfa2.fit)
cfa2.fit$A[,20:22]
modIndices(cfa2.fit)

#update cfa2
cfa_model4 <- update(cfa_model2,text = 'add, F1 -> bidr38, theta20, NA')
cfa3.fit <- sem(cfa_model4,bidr_cor,N)
summary(cfa3.fit)
