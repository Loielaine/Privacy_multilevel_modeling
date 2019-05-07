# Interview Privacy Factor Analysis  #
# liyixi #
# 2018.10.9 #

library(readr)
library(psych)
library(sem)
library(dplyr)
dir<-'/Users/Loielaine/Desktop/Good_Good_Study/2018Survey project/data/'
#opt <- options(fit.indices = c("GFI", "AGFI", "RMSEA", "NFI", "NNFI", "CFI", "RNI", "IFI", "SRMR", "AIC", "AICc", "BIC", "CAIC"))
opt <- options(fit.indices = c("GFI","CFI" , "RMSEA","AIC","BIC"))
#hexaco
data<-read.csv2(paste0(dir,'hexaco_pmm.csv'),sep =',')
data_complete<-data[complete.cases(data),]
data_cen<-scale(data_complete)
summary(data_cen)

hexaco_cor<-cor(data_cen)
write.csv(hexaco_cor,file=paste0(dir,'hexaco_cor.csv'))

#Exploratory Factor Analysis(EFA)
hexaco_factors<-fa(r=hexaco_cor,nfactors = 6,rotate = "varimax")
hexaco_factors

#ssi
data<-read.csv2(paste0(dir,'ssi.csv'),sep =',')
data_complete<-data[complete.cases(data),]
data_cen<-scale(data_complete)
summary(data_cen)

ssi_cor<-cor(data_cen)
write.csv(ssi_cor,file=paste0(dir,'ssi_cor.csv'))

#Exploratory Factor Analysis(EFA)
ssi_factors<-fa(r=ssi_cor,nfactors = 6,rotate = "varimax")
ssi_factors

#12.1
##all itmes
#hexaco
data<-read.csv2(paste0(dir,'hexaco_pmm.csv'),sep =',')
data_complete<-data[complete.cases(data),]
data_cen<-scale(data_complete)
summary(data_cen)

hexaco_cor<-cor(data_cen)
write.csv(hexaco_cor,file=paste0(dir,'hexaco_cor.csv'))

#Exploratory Factor Analysis(EFA)
hexaco_factors<-fa(r=hexaco_cor,nfactors = 6,rotate = "varimax")
hexaco_factors

#ssi
data<-read.csv2(paste0(dir,'ssi_pmm.csv'),sep =',')
data_complete<-data[complete.cases(data),]
data_cen<-scale(data_complete)
summary(data_cen)

ssi_cor<-cor(data_cen)
write.csv(ssi_cor,file=paste0(dir,'ssi_cor.csv'))


#Exploratory Factor Analysis(EFA)
ssi_factors<-fa(r=ssi_cor,nfactors = 12,rotate = "varimax")
ssi_factors$Structure
write.csv(ssi_factors$loadings,file=paste0(dir,'ssi_fa.csv'))

##
#hexaco
data<-read.csv2(paste0(dir,'hexaco_sum_pmm.csv'),sep =',')
data_complete<-data[complete.cases(data),]
data_cen<-scale(data_complete)
summary(data_cen)

hexaco_cor<-cor(data_cen)
write.csv(hexaco_cor,file=paste0(dir,'hexaco_cor.csv'))

#Exploratory Factor Analysis(EFA)
hexaco_factors<-fa(r=hexaco_cor,nfactors = 6,rotate = "varimax")
hexaco_factors

#ssi
data<-read.csv2(paste0(dir,'ssi_sum_pmm.csv'),sep =',')
data_complete<-data[complete.cases(data),]
data_cen<-scale(data_complete)
summary(data_cen)

ssi_cor<-cor(data_cen)
write.csv(ssi_cor,file=paste0(dir,'ssi_cor.csv'))

#Exploratory Factor Analysis(EFA)
ssi_factors<-fa(r=ssi_cor,nfactors = 6,rotate = "varimax")
ssi_factors

##12.3 update
##compare the fit of different models

##1.SSI
data<-read.csv2(paste0(dir,'ssi_pmm.csv'),sep =',')
data_complete<-data[complete.cases(data),]
data_cen<-scale(data_complete)
summary(data_cen)
ssi_cor<-cor(data_cen)
write.csv(ssi_cor,file=paste0(dir,'ssi_cor.csv'))
##(1)4 factors
##4 factors EFA all items
ssi_factors<-fa(r=ssi_cor,nfactors = 4,rotate = "varimax")

ssi_loadings<-data.frame(abs(unclass(ssi_factors$loadings)))
ssi_loadings<-data.frame(t(apply(ssi_loadings,1,function(x) replace(x,x==max(x),100))))
formula<-c()
factors<-colnames(ssi_loadings)
for(i in 1:90){
  for(j in 1:4){
    if(ssi_loadings[i,j]==100){
      formula<-c(formula,paste0(factors[j]," -> ","ssi",as.character(i),",","theta",as.character(i),",NA"),"\n")    
    }
  }
}
for(j in 1:4){
  formula<-c(formula,paste0(factors[j]," <-> ",factors[j],",NA,1"),"\n")
}
##5 factors CFA 
cfa_model0<- specifyModel(text = 
                            "MR1 -> ssi1,theta1,NA 
 MR2 -> ssi2,theta2,NA 
                          MR2 -> ssi3,theta3,NA 
                          MR2 -> ssi4,theta4,NA 
                          MR4 -> ssi5,theta5,NA 
                          MR1 -> ssi6,theta6,NA 
                          MR4 -> ssi7,theta7,NA 
                          MR4 -> ssi8,theta8,NA 
                          MR4 -> ssi9,theta9,NA 
                          MR3 -> ssi10,theta10,NA 
                          MR2 -> ssi11,theta11,NA 
                          MR4 -> ssi12,theta12,NA 
                          MR4 -> ssi13,theta13,NA 
                          MR4 -> ssi14,theta14,NA 
                          MR2 -> ssi15,theta15,NA 
                          MR3 -> ssi16,theta16,NA 
                          MR4 -> ssi17,theta17,NA 
                          MR2 -> ssi18,theta18,NA 
                          MR3 -> ssi19,theta19,NA 
                          MR4 -> ssi20,theta20,NA 
                          MR2 -> ssi21,theta21,NA 
                          MR3 -> ssi22,theta22,NA 
                          MR2 -> ssi23,theta23,NA 
                          MR1 -> ssi24,theta24,NA 
                          MR1 -> ssi25,theta25,NA 
                          MR4 -> ssi26,theta26,NA 
                          MR1 -> ssi27,theta27,NA 
                          MR3 -> ssi28,theta28,NA 
                          MR2 -> ssi29,theta29,NA 
                          MR2 -> ssi30,theta30,NA 
                          MR4 -> ssi31,theta31,NA 
                          MR1 -> ssi32,theta32,NA 
                          MR1 -> ssi33,theta33,NA 
                          MR1 -> ssi34,theta34,NA 
                          MR2 -> ssi35,theta35,NA 
                          MR2 -> ssi36,theta36,NA 
                          MR4 -> ssi37,theta37,NA 
                          MR1 -> ssi38,theta38,NA 
                          MR2 -> ssi39,theta39,NA 
                          MR1 -> ssi40,theta40,NA 
                          MR3 -> ssi41,theta41,NA 
                          MR1 -> ssi42,theta42,NA 
                          MR3 -> ssi43,theta43,NA 
                          MR3 -> ssi44,theta44,NA 
                          MR1 -> ssi45,theta45,NA 
                          MR1 -> ssi46,theta46,NA 
                          MR4 -> ssi47,theta47,NA 
                          MR2 -> ssi48,theta48,NA 
                          MR1 -> ssi49,theta49,NA 
                          MR1 -> ssi50,theta50,NA 
                          MR1 -> ssi51,theta51,NA 
                          MR1 -> ssi52,theta52,NA 
                          MR3 -> ssi53,theta53,NA 
                          MR3 -> ssi54,theta54,NA 
                          MR2 -> ssi55,theta55,NA 
                          MR3 -> ssi56,theta56,NA 
                          MR1 -> ssi57,theta57,NA 
                          MR3 -> ssi58,theta58,NA 
                          MR2 -> ssi59,theta59,NA 
                          MR2 -> ssi60,theta60,NA 
                          MR1 -> ssi61,theta61,NA 
                          MR2 -> ssi62,theta62,NA 
                          MR1 -> ssi63,theta63,NA 
                          MR3 -> ssi64,theta64,NA 
                          MR2 -> ssi65,theta65,NA 
                          MR2 -> ssi66,theta66,NA 
                          MR4 -> ssi67,theta67,NA 
                          MR4 -> ssi68,theta68,NA 
                          MR2 -> ssi69,theta69,NA 
                          MR1 -> ssi70,theta70,NA 
                          MR1 -> ssi71,theta71,NA 
                          MR4 -> ssi72,theta72,NA 
                          MR3 -> ssi73,theta73,NA 
                          MR1 -> ssi74,theta74,NA 
                          MR2 -> ssi75,theta75,NA 
                          MR2 -> ssi76,theta76,NA 
                          MR2 -> ssi77,theta77,NA 
                          MR1 -> ssi78,theta78,NA 
                          MR1 -> ssi79,theta79,NA 
                          MR4 -> ssi80,theta80,NA 
                          MR2 -> ssi81,theta81,NA 
                          MR3 -> ssi82,theta82,NA 
                          MR2 -> ssi83,theta83,NA 
                          MR2 -> ssi84,theta84,NA 
                          MR4 -> ssi85,theta85,NA 
                          MR1 -> ssi86,theta86,NA 
                          MR1 -> ssi87,theta87,NA 
                          MR1 -> ssi88,theta88,NA 
                          MR2 -> ssi89,theta89,NA 
                          MR3 -> ssi90,theta90,NA 
                          MR1 <-> MR1,NA,1 
                          MR2 <-> MR2,NA,1 
                          MR3 <-> MR3,NA,1 
                          MR4 <-> MR4,NA,1"
)

N<-64
cfa0.fit <- sem(cfa_model0, S=cor.smooth(ssi_cor),N=N)
summary(cfa0.fit)
##(1)5 factors
##5 factors EFA all items
ssi_factors<-fa(r=ssi_cor,nfactors = 5,rotate = "varimax")

ssi_loadings<-data.frame(abs(unclass(ssi_factors$loadings)))
ssi_loadings<-data.frame(t(apply(ssi_loadings,1,function(x) replace(x,x==max(x),100))))
formula<-c()
factors<-colnames(ssi_loadings)
for(i in 1:90){
  for(j in 1:5){
    if(ssi_loadings[i,j]==100){
      formula<-c(formula,paste0(factors[j]," -> ","ssi",as.character(i),",","theta",as.character(i),",NA"),"\n")    
    }
  }
}
for(j in 1:5){
  formula<-c(formula,paste0(factors[j]," <-> ",factors[j],",NA,1"),"\n")
  }
##5 factors CFA 
cfa_model1<- specifyModel(text = 
                        "MR1 -> ssi1,theta1,NA 
                         MR4 -> ssi2,theta2,NA 
                         MR4 -> ssi3,theta3,NA 
                         MR2 -> ssi4,theta4,NA 
                         MR4 -> ssi5,theta5,NA 
                         MR5 -> ssi6,theta6,NA 
                         MR4 -> ssi7,theta7,NA 
                         MR4 -> ssi8,theta8,NA 
                         MR4 -> ssi9,theta9,NA 
                         MR3 -> ssi10,theta10,NA 
                         MR4 -> ssi11,theta11,NA 
                         MR4 -> ssi12,theta12,NA 
                         MR4 -> ssi13,theta13,NA 
                         MR1 -> ssi14,theta14,NA 
                         MR4 -> ssi15,theta15,NA 
                         MR3 -> ssi16,theta16,NA 
                         MR1 -> ssi17,theta17,NA 
                         MR2 -> ssi18,theta18,NA 
                         MR4 -> ssi19,theta19,NA 
                         MR1 -> ssi20,theta20,NA 
                         MR4 -> ssi21,theta21,NA 
                         MR3 -> ssi22,theta22,NA 
                         MR4 -> ssi23,theta23,NA 
                         MR2 -> ssi24,theta24,NA 
                         MR5 -> ssi25,theta25,NA 
                         MR1 -> ssi26,theta26,NA 
                         MR1 -> ssi27,theta27,NA 
                         MR1 -> ssi28,theta28,NA 
                         MR4 -> ssi29,theta29,NA 
                         MR5 -> ssi30,theta30,NA 
                         MR4 -> ssi31,theta31,NA 
                         MR1 -> ssi32,theta32,NA 
                         MR1 -> ssi33,theta33,NA 
                         MR1 -> ssi34,theta34,NA 
                         MR2 -> ssi35,theta35,NA 
                         MR2 -> ssi36,theta36,NA 
                         MR4 -> ssi37,theta37,NA 
                         MR1 -> ssi38,theta38,NA 
                         MR5 -> ssi39,theta39,NA 
                         MR1 -> ssi40,theta40,NA 
                         MR3 -> ssi41,theta41,NA 
                         MR1 -> ssi42,theta42,NA 
                         MR3 -> ssi43,theta43,NA 
                         MR3 -> ssi44,theta44,NA 
                         MR1 -> ssi45,theta45,NA 
                         MR1 -> ssi46,theta46,NA 
                         MR4 -> ssi47,theta47,NA 
                         MR2 -> ssi48,theta48,NA 
                         MR1 -> ssi49,theta49,NA 
                         MR1 -> ssi50,theta50,NA 
                         MR1 -> ssi51,theta51,NA 
                         MR1 -> ssi52,theta52,NA 
                         MR1 -> ssi53,theta53,NA 
                         MR3 -> ssi54,theta54,NA 
                         MR2 -> ssi55,theta55,NA 
                         MR3 -> ssi56,theta56,NA 
                         MR1 -> ssi57,theta57,NA 
                         MR3 -> ssi58,theta58,NA 
                         MR4 -> ssi59,theta59,NA 
                         MR4 -> ssi60,theta60,NA 
                         MR1 -> ssi61,theta61,NA 
                         MR3 -> ssi62,theta62,NA 
                         MR1 -> ssi63,theta63,NA 
                         MR3 -> ssi64,theta64,NA 
                         MR4 -> ssi65,theta65,NA 
                         MR2 -> ssi66,theta66,NA 
                         MR5 -> ssi67,theta67,NA 
                         MR4 -> ssi68,theta68,NA 
                         MR4 -> ssi69,theta69,NA 
                         MR1 -> ssi70,theta70,NA 
                         MR1 -> ssi71,theta71,NA 
                         MR4 -> ssi72,theta72,NA 
                         MR3 -> ssi73,theta73,NA 
                         MR2 -> ssi74,theta74,NA 
                         MR2 -> ssi75,theta75,NA 
                         MR2 -> ssi76,theta76,NA 
                         MR5 -> ssi77,theta77,NA 
                         MR1 -> ssi78,theta78,NA 
                         MR1 -> ssi79,theta79,NA 
                         MR4 -> ssi80,theta80,NA 
                         MR4 -> ssi81,theta81,NA 
                         MR3 -> ssi82,theta82,NA 
                         MR4 -> ssi83,theta83,NA 
                         MR2 -> ssi84,theta84,NA 
                         MR2 -> ssi85,theta85,NA 
                         MR1 -> ssi86,theta86,NA 
                         MR1 -> ssi87,theta87,NA 
                         MR1 -> ssi88,theta88,NA 
                         MR4 -> ssi89,theta89,NA 
                         MR3 -> ssi90,theta90,NA 
                         MR1 <-> MR1,NA,1 
                         MR4 <-> MR4,NA,1 
                         MR3 <-> MR3,NA,1 
                         MR2 <-> MR2,NA,1 
                         MR5 <-> MR5,NA,1 "
                       )

N<-64
cfa1.fit <- sem(cfa_model1, S=cor.smooth(ssi_cor),N=N)
summary(cfa1.fit)

##(2)6 factors
##6 factors EFA all items
ssi_factors<-fa(r=ssi_cor,nfactors = 6,rotate = "varimax")
ssi_loadings<-data.frame(abs(unclass(ssi_factors$loadings)))
ssi_loadings<-data.frame(t(apply(ssi_loadings,1,function(x) replace(x,x==max(x),100))))
formula<-c()
factors<-colnames(ssi_loadings)
for(i in 1:90){
  for(j in 1:6){
    if(ssi_loadings[i,j]==100){
      formula<-c(formula,paste0(factors[j]," -> ","ssi",as.character(i),",","theta",as.character(i),",NA"),"\n")    
    }
  }
}
for(j in 1:6){
  formula<-c(formula,paste0(factors[j]," <-> ",factors[j],",NA,1"),"\n")
}
##6 factors CFA 
cfa_model2<- specifyModel(text = 
"MR3 -> ssi1,theta1,NA 
 MR6 -> ssi2,theta2,NA 
MR6 -> ssi3,theta3,NA 
MR1 -> ssi4,theta4,NA 
MR6 -> ssi5,theta5,NA 
MR1 -> ssi6,theta6,NA 
MR4 -> ssi7,theta7,NA 
MR4 -> ssi8,theta8,NA 
MR4 -> ssi9,theta9,NA 
MR1 -> ssi10,theta10,NA 
MR4 -> ssi11,theta11,NA 
MR5 -> ssi12,theta12,NA 
MR6 -> ssi13,theta13,NA 
MR3 -> ssi14,theta14,NA 
MR4 -> ssi15,theta15,NA 
MR1 -> ssi16,theta16,NA 
MR3 -> ssi17,theta17,NA 
MR2 -> ssi18,theta18,NA 
MR4 -> ssi19,theta19,NA 
MR3 -> ssi20,theta20,NA 
MR4 -> ssi21,theta21,NA 
MR1 -> ssi22,theta22,NA 
MR4 -> ssi23,theta23,NA 
MR2 -> ssi24,theta24,NA 
MR5 -> ssi25,theta25,NA 
MR3 -> ssi26,theta26,NA 
MR3 -> ssi27,theta27,NA 
MR1 -> ssi28,theta28,NA 
MR4 -> ssi29,theta29,NA 
MR5 -> ssi30,theta30,NA 
MR4 -> ssi31,theta31,NA 
MR3 -> ssi32,theta32,NA 
MR3 -> ssi33,theta33,NA 
MR3 -> ssi34,theta34,NA 
MR6 -> ssi35,theta35,NA 
MR2 -> ssi36,theta36,NA 
MR3 -> ssi37,theta37,NA 
MR3 -> ssi38,theta38,NA 
MR4 -> ssi39,theta39,NA 
MR3 -> ssi40,theta40,NA 
MR6 -> ssi41,theta41,NA 
MR5 -> ssi42,theta42,NA 
MR1 -> ssi43,theta43,NA 
MR1 -> ssi44,theta44,NA 
MR1 -> ssi45,theta45,NA 
MR6 -> ssi46,theta46,NA 
MR6 -> ssi47,theta47,NA 
MR6 -> ssi48,theta48,NA 
MR3 -> ssi49,theta49,NA 
MR3 -> ssi50,theta50,NA 
MR3 -> ssi51,theta51,NA 
MR3 -> ssi52,theta52,NA 
MR1 -> ssi53,theta53,NA 
MR1 -> ssi54,theta54,NA 
MR2 -> ssi55,theta55,NA 
MR6 -> ssi56,theta56,NA 
MR3 -> ssi57,theta57,NA 
MR1 -> ssi58,theta58,NA 
MR4 -> ssi59,theta59,NA 
MR2 -> ssi60,theta60,NA 
MR3 -> ssi61,theta61,NA 
MR4 -> ssi62,theta62,NA 
MR3 -> ssi63,theta63,NA 
MR1 -> ssi64,theta64,NA 
MR6 -> ssi65,theta65,NA 
MR2 -> ssi66,theta66,NA 
MR5 -> ssi67,theta67,NA 
MR4 -> ssi68,theta68,NA 
MR5 -> ssi69,theta69,NA 
MR3 -> ssi70,theta70,NA 
MR3 -> ssi71,theta71,NA 
MR6 -> ssi72,theta72,NA 
MR3 -> ssi73,theta73,NA 
MR2 -> ssi74,theta74,NA 
MR2 -> ssi75,theta75,NA 
MR2 -> ssi76,theta76,NA 
MR5 -> ssi77,theta77,NA 
MR3 -> ssi78,theta78,NA 
MR5 -> ssi79,theta79,NA 
MR4 -> ssi80,theta80,NA 
MR4 -> ssi81,theta81,NA 
MR1 -> ssi82,theta82,NA 
MR6 -> ssi83,theta83,NA 
MR2 -> ssi84,theta84,NA 
MR2 -> ssi85,theta85,NA 
MR3 -> ssi86,theta86,NA 
MR3 -> ssi87,theta87,NA 
MR3 -> ssi88,theta88,NA 
MR6 -> ssi89,theta89,NA 
MR1 -> ssi90,theta90,NA 
MR3 <-> MR3,NA,1 
MR1 <-> MR1,NA,1 
MR4 <-> MR4,NA,1 
MR6 <-> MR6,NA,1 
MR2 <-> MR2,NA,1 
MR5 <-> MR5,NA,1 ")
N<-64
cfa2.fit <- sem(cfa_model2, S=cor.smooth(ssi_cor),N=N)
summary(cfa2.fit)

##(3)6 factors
##6 factors CFA all itmes, according to the categories
cfa_model3<- specifyModel(text = 
                            "MR3 -> ssi1,theta1,NA 
                          MR6 -> ssi2,theta2,NA 
                          MR4 -> ssi3,theta3,NA 
                          MR1 -> ssi4,theta4,NA 
                          MR2 -> ssi5,theta5,NA 
                          MR5 -> ssi6,theta6,NA 
                          MR3 -> ssi7,theta7,NA 
                          MR6 -> ssi8,theta8,NA 
                          MR4 -> ssi9,theta9,NA 
                          MR1 -> ssi10,theta10,NA 
                          MR2 -> ssi11,theta11,NA 
                          MR5 -> ssi12,theta12,NA 
                          MR3 -> ssi13,theta13,NA 
                          MR6 -> ssi14,theta14,NA 
                          MR4 -> ssi15,theta15,NA 
                          MR1 -> ssi16,theta16,NA 
                          MR2 -> ssi17,theta17,NA 
                          MR5 -> ssi18,theta18,NA 
                          MR3 -> ssi19,theta19,NA 
                          MR6 -> ssi20,theta20,NA 
                          MR4 -> ssi21,theta21,NA 
                          MR1 -> ssi22,theta22,NA 
                          MR2 -> ssi23,theta23,NA 
                          MR5 -> ssi24,theta24,NA 
                          MR3 -> ssi25,theta25,NA 
                          MR6 -> ssi26,theta26,NA 
                          MR4 -> ssi27,theta27,NA 
                          MR1 -> ssi28,theta28,NA 
                          MR2 -> ssi29,theta29,NA 
                          MR5 -> ssi30,theta30,NA 
                          MR3 -> ssi31,theta31,NA 
                          MR6 -> ssi32,theta32,NA 
                          MR4 -> ssi33,theta33,NA 
                          MR1 -> ssi34,theta34,NA 
                          MR2 -> ssi35,theta35,NA 
                          MR5 -> ssi36,theta36,NA 
                          MR3 -> ssi37,theta37,NA 
                          MR6 -> ssi38,theta38,NA 
                          MR4 -> ssi39,theta39,NA 
                          MR1 -> ssi40,theta40,NA 
                          MR2 -> ssi41,theta41,NA 
                          MR5 -> ssi42,theta42,NA 
                          MR3 -> ssi43,theta43,NA 
                          MR6 -> ssi44,theta44,NA 
                          MR4 -> ssi45,theta45,NA 
                          MR1 -> ssi46,theta46,NA 
                          MR2 -> ssi47,theta47,NA 
                          MR5 -> ssi48,theta48,NA 
                          MR3 -> ssi49,theta49,NA 
                          MR6 -> ssi50,theta50,NA 
                          MR4 -> ssi51,theta51,NA 
                          MR1 -> ssi52,theta52,NA 
                          MR2 -> ssi53,theta53,NA 
                          MR5 -> ssi54,theta54,NA 
                          MR3 -> ssi55,theta55,NA 
                          MR6 -> ssi56,theta56,NA 
                          MR4 -> ssi57,theta57,NA 
                          MR1 -> ssi58,theta58,NA 
                          MR2 -> ssi59,theta59,NA 
                          MR5 -> ssi60,theta60,NA 
                          MR3 -> ssi61,theta61,NA 
                          MR6 -> ssi62,theta62,NA 
                          MR4 -> ssi63,theta63,NA 
                          MR1 -> ssi64,theta64,NA 
                          MR2 -> ssi65,theta65,NA 
                          MR5 -> ssi66,theta66,NA 
                          MR3 -> ssi67,theta67,NA 
                          MR6 -> ssi68,theta68,NA 
                          MR4 -> ssi69,theta69,NA 
                          MR1 -> ssi70,theta70,NA 
                          MR2 -> ssi71,theta71,NA 
                          MR5 -> ssi72,theta72,NA 
                          MR3 -> ssi73,theta73,NA 
                          MR6 -> ssi74,theta74,NA 
                          MR4 -> ssi75,theta75,NA 
                          MR1 -> ssi76,theta76,NA 
                          MR2 -> ssi77,theta77,NA 
                          MR5 -> ssi78,theta78,NA 
                          MR3 -> ssi79,theta79,NA 
                          MR6 -> ssi80,theta80,NA 
                          MR4 -> ssi81,theta81,NA 
                          MR1 -> ssi82,theta82,NA 
                          MR2 -> ssi83,theta83,NA 
                          MR5 -> ssi84,theta84,NA 
                          MR3 -> ssi85,theta85,NA 
                          MR6 -> ssi86,theta86,NA 
                          MR4 -> ssi87,theta87,NA 
                          MR1 -> ssi88,theta88,NA 
                          MR2 -> ssi89,theta89,NA 
                          MR5 -> ssi90,theta90,NA 
                          MR3 <-> MR3,NA,1 
                          MR1 <-> MR1,NA,1 
                          MR4 <-> MR4,NA,1 
                          MR6 <-> MR6,NA,1 
                          MR2 <-> MR2,NA,1 
                          MR5 <-> MR5,NA,1")
N<-64
cfa3.fit <- sem(cfa_model3, S=cor.smooth(ssi_cor),N=N)
summary(cfa3.fit)

##6 factors CFA summed itmes, according to the categories
data<-read.csv2(paste0(dir,'ssi_sum_pmm.csv'),sep =',')
data_complete<-data[complete.cases(data),]
data_cen<-scale(data_complete)
summary(data_cen)

ssi_cor<-cor(data)
rownames(ssi_cor)<-c(paste0("ssi",1:18))
colnames(ssi_cor)<-c(paste0("ssi",1:18))
cfa_model4<- specifyModel(text = 
                            "MR1 -> EE1,theta1,NA 
                          MR1 -> EE2,theta2,NA 
                          MR1 -> EE3,theta3,NA 
                          MR2 -> ES1,theta4,NA 
                          MR2 -> ES2,theta5,NA 
                          MR2 -> ES3,theta6,NA 
                          MR3 -> EC1,theta7,NA 
                          MR3 -> EC2,theta8,NA 
                          MR3 -> EC3,theta9,NA 
                          MR4 -> SE1,theta10,NA 
                          MR4 -> SE2,theta11,NA 
                          MR4 -> SE3,theta12,NA 
                          MR5 -> SS1,theta13,NA 
                          MR5 -> SS2,theta14,NA 
                          MR5 -> SS3,theta15,NA 
                          MR6 -> SC1,theta16,NA 
                          MR6 -> SC2,theta17,NA 
                          MR6 -> SC3,theta18,NA 
                          MR3 <-> MR3,NA,1 
                          MR1 <-> MR1,NA,1 
                          MR4 <-> MR4,NA,1 
                          MR6 <-> MR6,NA,1 
                          MR2 <-> MR2,NA,1 
                          MR5 <-> MR5,NA,1")
N<-64
cfa4.fit <- sem(cfa_model4, S=cor.smooth(ssi_cor),N=N)
summary(cfa4.fit)
AIC(cfa4.fit)
BIC(cfa4.fit)
mean(normalizedResiduals(cfa4.fit))

##EFA summed items, 6 factors
ssi_factors<-fa(r=ssi_cor,nfactors = 6,rotate = "varimax")

ssi_loadings<-data.frame(abs(unclass(ssi_factors$loadings)))
ssi_loadings<-data.frame(t(apply(ssi_loadings,1,function(x) replace(x,x==max(x),100))))
formula<-c()
factors<-colnames(ssi_loadings)
for(i in 1:18){
  for(j in 1:6){
    if(ssi_loadings[i,j]==100){
      formula<-c(formula,paste0(factors[j]," -> ","ssi",as.character(i),",","theta",as.character(i),",NA"),"\n")    
    }
  }
}
for(j in 1:6){
  formula<-c(formula,paste0(factors[j]," <-> ",factors[j],",NA,1"),"\n")
}
cfa_model4.1<- specifyModel(text = 
                            "MR6 -> ssi1,theta1,NA 
 MR4 -> ssi2,theta2,NA 
                            MR6 -> ssi3,theta3,NA 
                            MR1 -> ssi4,theta4,NA 
                            MR1 -> ssi5,theta5,NA 
                            MR1 -> ssi6,theta6,NA 
                            MR2 -> ssi7,theta7,NA 
                            MR2 -> ssi8,theta8,NA 
                            MR2 -> ssi9,theta9,NA 
                            MR1 -> ssi10,theta10,NA 
                            MR1 -> ssi11,theta11,NA 
                            MR1 -> ssi12,theta12,NA 
                            MR3 -> ssi13,theta13,NA 
                            MR3 -> ssi14,theta14,NA 
                            MR1 -> ssi15,theta15,NA 
                            MR1 -> ssi16,theta16,NA 
                            MR5 -> ssi17,theta17,NA 
                            MR3 -> ssi18,theta18,NA 
                            MR1 <-> MR1,NA,1 
                            MR2 <-> MR2,NA,1 
                            MR3 <-> MR3,NA,1 
                            MR6 <-> MR6,NA,1 
                            MR5 <-> MR5,NA,1 
                            MR4 <-> MR4,NA,1")
N<-64
cfa4.1.fit <- sem(cfa_model4.1, S=cor.smooth(ssi_cor),N=N)
summary(cfa4.fit)
AIC(cfa4.1.fit)
BIC(cfa4.1.fit)
mean(normalizedResiduals(cfa4.1.fit))


##EFA summed items, 5 factors
ssi_factors<-fa(r=ssi_cor,nfactors = 5,rotate = "varimax")

ssi_loadings<-data.frame(abs(unclass(ssi_factors$loadings)))
ssi_loadings<-data.frame(t(apply(ssi_loadings,1,function(x) replace(x,x==max(x),100))))
formula<-c()
factors<-colnames(ssi_loadings)
for(i in 1:18){
  for(j in 1:5){
    if(ssi_loadings[i,j]==100){
      formula<-c(formula,paste0(factors[j]," -> ","ssi",as.character(i),",","theta",as.character(i),",NA"),"\n")    
    }
  }
}
for(j in 1:5){
  formula<-c(formula,paste0(factors[j]," <-> ",factors[j],",NA,1"),"\n")
}
cfa_model4.2<- specifyModel(text = 
                              "MR2 -> ssi1,theta1,NA 
 MR4 -> ssi2,theta2,NA 
                            MR1 -> ssi3,theta3,NA 
                            MR1 -> ssi4,theta4,NA 
                            MR1 -> ssi5,theta5,NA 
                            MR1 -> ssi6,theta6,NA 
                            MR2 -> ssi7,theta7,NA 
                            MR2 -> ssi8,theta8,NA 
                            MR2 -> ssi9,theta9,NA 
                            MR1 -> ssi10,theta10,NA 
                            MR1 -> ssi11,theta11,NA 
                            MR1 -> ssi12,theta12,NA 
                            MR3 -> ssi13,theta13,NA 
                            MR3 -> ssi14,theta14,NA 
                            MR1 -> ssi15,theta15,NA 
                            MR1 -> ssi16,theta16,NA 
                            MR5 -> ssi17,theta17,NA 
                            MR3 -> ssi18,theta18,NA 
                            MR1 <-> MR1,NA,1 
                            MR2 <-> MR2,NA,1 
                            MR3 <-> MR3,NA,1 
                            MR5 <-> MR5,NA,1 
                            MR4 <-> MR4,NA,1")
N<-64
cfa4.2.fit <- sem(cfa_model4.2, S=cor.smooth(ssi_cor),N=N)
AIC(cfa4.2.fit)
BIC(cfa4.2.fit)
mean(normalizedResiduals(cfa4.2.fit))


##2.Hexaco
data<-read.csv2(paste0(dir,'hexaco_pmm.csv'),sep =',')
data_complete<-data[complete.cases(data),]
data_cen<-scale(data_complete)
summary(data_cen)
hexaco_cor<-cor(data_cen)
write.csv(hexaco_cor,file=paste0(dir,'hexaco_cor.csv'))

##(1)5 factors
##5 factors EFA all items
hex_factors<-fa(r=hexaco_cor,nfactors = 5,rotate = "varimax")

hex_loadings<-data.frame(abs(unclass(hex_factors$loadings)))
hex_loadings<-data.frame(t(apply(hex_loadings,1,function(x) replace(x,x==max(x),100))))
formula<-c()
factors<-colnames(hex_loadings)
for(i in 1:60){
  for(j in 1:5){
    if(hex_loadings[i,j]==100){
      formula<-c(formula,paste0(factors[j]," -> ","hexaco",as.character(i),",","theta",as.character(i),",NA"),"\n")    
    }
  }
}
for(j in 1:5){
  formula<-c(formula,paste0(factors[j]," <-> ",factors[j],",NA,1"),"\n")
}
##5 factors CFA 
cfa_model5<-specifyModel(text="MR1 -> hexaco1,theta1,NA 
 MR4 -> hexaco2,theta2,NA 
                         MR3 -> hexaco3,theta3,NA 
                         MR4 -> hexaco4,theta4,NA 
                         MR2 -> hexaco5,theta5,NA 
                         MR5 -> hexaco6,theta6,NA 
                         MR4 -> hexaco7,theta7,NA 
                         MR5 -> hexaco8,theta8,NA 
                         MR4 -> hexaco9,theta9,NA 
                         MR1 -> hexaco10,theta10,NA 
                         MR2 -> hexaco11,theta11,NA 
                         MR4 -> hexaco12,theta12,NA 
                         MR3 -> hexaco13,theta13,NA 
                         MR5 -> hexaco14,theta14,NA 
                         MR2 -> hexaco15,theta15,NA 
                         MR1 -> hexaco16,theta16,NA 
                         MR2 -> hexaco17,theta17,NA 
                         MR3 -> hexaco18,theta18,NA 
                         MR1 -> hexaco19,theta19,NA 
                         MR5 -> hexaco20,theta20,NA 
                         MR1 -> hexaco21,theta21,NA 
                         MR4 -> hexaco22,theta22,NA 
                         MR2 -> hexaco23,theta23,NA 
                         MR2 -> hexaco24,theta24,NA 
                         MR5 -> hexaco25,theta25,NA 
                         MR3 -> hexaco26,theta26,NA 
                         MR1 -> hexaco27,theta27,NA 
                         MR1 -> hexaco28,theta28,NA 
                         MR5 -> hexaco29,theta29,NA 
                         MR4 -> hexaco30,theta30,NA 
                         MR1 -> hexaco31,theta31,NA 
                         MR1 -> hexaco32,theta32,NA 
                         MR1 -> hexaco33,theta33,NA 
                         MR3 -> hexaco34,theta34,NA 
                         MR2 -> hexaco35,theta35,NA 
                         MR3 -> hexaco36,theta36,NA 
                         MR5 -> hexaco37,theta37,NA 
                         MR1 -> hexaco38,theta38,NA 
                         MR1 -> hexaco39,theta39,NA 
                         MR1 -> hexaco40,theta40,NA 
                         MR1 -> hexaco41,theta41,NA 
                         MR2 -> hexaco42,theta42,NA 
                         MR1 -> hexaco43,theta43,NA 
                         MR1 -> hexaco44,theta44,NA 
                         MR2 -> hexaco45,theta45,NA 
                         MR1 -> hexaco46,theta46,NA 
                         MR3 -> hexaco47,theta47,NA 
                         MR5 -> hexaco48,theta48,NA 
                         MR1 -> hexaco49,theta49,NA 
                         MR1 -> hexaco50,theta50,NA 
                         MR4 -> hexaco51,theta51,NA 
                         MR2 -> hexaco52,theta52,NA 
                         MR5 -> hexaco53,theta53,NA 
                         MR4 -> hexaco54,theta54,NA 
                         MR2 -> hexaco55,theta55,NA 
                         MR2 -> hexaco56,theta56,NA 
                         MR2 -> hexaco57,theta57,NA 
                         MR5 -> hexaco58,theta58,NA 
                         MR3 -> hexaco59,theta59,NA 
                         MR3 -> hexaco60,theta60,NA 
                         MR1 <-> MR1,NA,1 
                         MR2 <-> MR2,NA,1 
                         MR3 <-> MR3,NA,1 
                         MR5 <-> MR5,NA,1 
                         MR4 <-> MR4,NA,1")
N<-64
cfa5.fit <- sem(cfa_model5, S=cor.smooth(hexaco_cor),N=N)
summary(cfa5.fit)
cfa5.fit$robust.vcov

##(2)6 factors
##6 factors EFA all items
hex_factors<-fa(r=hexaco_cor,nfactors = 6,rotate = "varimax")

hex_loadings<-data.frame(abs(unclass(hex_factors$loadings)))
hex_loadings<-data.frame(t(apply(hex_loadings,1,function(x) replace(x,x==max(x),100))))
formula<-c()
factors<-colnames(hex_loadings)
for(i in 1:60){
  for(j in 1:6){
    if(hex_loadings[i,j]==100){
      formula<-c(formula,paste0(factors[j]," -> ","hexaco",as.character(i),",","theta",as.character(i),",NA"),"\n")    
    }
  }
}
for(j in 1:6){
  formula<-c(formula,paste0(factors[j]," <-> ",factors[j],",NA,1"),"\n")
}
##6 factors CFA 
cfa_model6<-specifyModel(text="MR1 -> hexaco1,theta1,NA 
 MR4 -> hexaco2,theta2,NA 
                         MR6 -> hexaco3,theta3,NA 
                         MR4 -> hexaco4,theta4,NA 
                         MR2 -> hexaco5,theta5,NA 
                         MR5 -> hexaco6,theta6,NA 
                         MR4 -> hexaco7,theta7,NA 
                         MR6 -> hexaco8,theta8,NA 
                         MR4 -> hexaco9,theta9,NA 
                         MR4 -> hexaco10,theta10,NA 
                         MR2 -> hexaco11,theta11,NA 
                         MR4 -> hexaco12,theta12,NA 
                         MR3 -> hexaco13,theta13,NA 
                         MR5 -> hexaco14,theta14,NA 
                         MR1 -> hexaco15,theta15,NA 
                         MR1 -> hexaco16,theta16,NA 
                         MR2 -> hexaco17,theta17,NA 
                         MR5 -> hexaco18,theta18,NA 
                         MR5 -> hexaco19,theta19,NA 
                         MR5 -> hexaco20,theta20,NA 
                         MR1 -> hexaco21,theta21,NA 
                         MR6 -> hexaco22,theta22,NA 
                         MR2 -> hexaco23,theta23,NA 
                         MR2 -> hexaco24,theta24,NA 
                         MR6 -> hexaco25,theta25,NA 
                         MR3 -> hexaco26,theta26,NA 
                         MR1 -> hexaco27,theta27,NA 
                         MR1 -> hexaco28,theta28,NA 
                         MR2 -> hexaco29,theta29,NA 
                         MR4 -> hexaco30,theta30,NA 
                         MR1 -> hexaco31,theta31,NA 
                         MR5 -> hexaco32,theta32,NA 
                         MR1 -> hexaco33,theta33,NA 
                         MR3 -> hexaco34,theta34,NA 
                         MR6 -> hexaco35,theta35,NA 
                         MR3 -> hexaco36,theta36,NA 
                         MR6 -> hexaco37,theta37,NA 
                         MR1 -> hexaco38,theta38,NA 
                         MR1 -> hexaco39,theta39,NA 
                         MR1 -> hexaco40,theta40,NA 
                         MR1 -> hexaco41,theta41,NA 
                         MR6 -> hexaco42,theta42,NA 
                         MR6 -> hexaco43,theta43,NA 
                         MR1 -> hexaco44,theta44,NA 
                         MR2 -> hexaco45,theta45,NA 
                         MR1 -> hexaco46,theta46,NA 
                         MR3 -> hexaco47,theta47,NA 
                         MR3 -> hexaco48,theta48,NA 
                         MR6 -> hexaco49,theta49,NA 
                         MR1 -> hexaco50,theta50,NA 
                         MR4 -> hexaco51,theta51,NA 
                         MR2 -> hexaco52,theta52,NA 
                         MR5 -> hexaco53,theta53,NA 
                         MR6 -> hexaco54,theta54,NA 
                         MR1 -> hexaco55,theta55,NA 
                         MR3 -> hexaco56,theta56,NA 
                         MR2 -> hexaco57,theta57,NA 
                         MR5 -> hexaco58,theta58,NA 
                         MR3 -> hexaco59,theta59,NA 
                         MR3 -> hexaco60,theta60,NA 
                         MR1 <-> MR1,NA,1 
                         MR3 <-> MR3,NA,1 
                         MR2 <-> MR2,NA,1 
                         MR5 <-> MR5,NA,1 
                         MR6 <-> MR6,NA,1 
                         MR4 <-> MR4,NA,1")
N<-64
cfa6.fit <- sem(cfa_model6, S=hexaco_cor,N=N)
summary(cfa6.fit)
##(3)6 factors
##6 factors CFA all itmes, according to the categories
cfa_model7<-specifyModel(text="MR1 -> hexaco1,theta1,NA 
 MR4 -> hexaco2,theta2,NA 
                         MR6 -> hexaco3,theta3,NA 
                         MR3 -> hexaco4,theta4,NA 
                         MR2 -> hexaco5,theta5,NA 
                         MR5 -> hexaco6,theta6,NA 
                         MR1 -> hexaco7,theta7,NA 
                         MR4 -> hexaco8,theta8,NA 
                         MR6 -> hexaco9,theta9,NA 
                         MR3 -> hexaco10,theta10,NA 
                         MR2 -> hexaco11,theta11,NA 
                         MR5 -> hexaco12,theta12,NA 
                         MR1 -> hexaco13,theta13,NA 
                         MR4 -> hexaco14,theta14,NA 
                         MR6 -> hexaco15,theta15,NA 
                         MR3 -> hexaco16,theta16,NA 
                         MR2 -> hexaco17,theta17,NA 
                         MR5 -> hexaco18,theta18,NA 
                         MR1 -> hexaco19,theta19,NA 
                         MR4 -> hexaco20,theta20,NA 
                         MR6 -> hexaco21,theta21,NA 
                         MR3 -> hexaco22,theta22,NA 
                         MR2 -> hexaco23,theta23,NA 
                         MR5 -> hexaco24,theta24,NA 
                         MR1 -> hexaco25,theta25,NA 
                         MR4 -> hexaco26,theta26,NA 
                         MR6 -> hexaco27,theta27,NA 
                         MR3 -> hexaco28,theta28,NA 
                         MR2 -> hexaco29,theta29,NA 
                         MR5 -> hexaco30,theta30,NA 
                         MR1 -> hexaco31,theta31,NA 
                         MR4 -> hexaco32,theta32,NA 
                         MR6 -> hexaco33,theta33,NA 
                         MR3 -> hexaco34,theta34,NA 
                         MR2 -> hexaco35,theta35,NA 
                         MR5 -> hexaco36,theta36,NA 
                         MR1 -> hexaco37,theta37,NA 
                         MR4 -> hexaco38,theta38,NA 
                         MR6 -> hexaco39,theta39,NA 
                         MR3 -> hexaco40,theta40,NA 
                         MR2 -> hexaco41,theta41,NA 
                         MR5 -> hexaco42,theta42,NA 
                         MR1 -> hexaco43,theta43,NA 
                         MR4 -> hexaco44,theta44,NA 
                         MR6 -> hexaco45,theta45,NA 
                         MR3 -> hexaco46,theta46,NA 
                         MR2 -> hexaco47,theta47,NA 
                         MR5 -> hexaco48,theta48,NA 
                         MR1 -> hexaco49,theta49,NA 
                         MR4 -> hexaco50,theta50,NA 
                         MR6 -> hexaco51,theta51,NA 
                         MR3 -> hexaco52,theta52,NA 
                         MR2 -> hexaco53,theta53,NA 
                         MR5 -> hexaco54,theta54,NA 
                         MR1 -> hexaco55,theta55,NA 
                         MR4 -> hexaco56,theta56,NA 
                         MR6 -> hexaco57,theta57,NA 
                         MR3 -> hexaco58,theta58,NA 
                         MR2 -> hexaco59,theta59,NA 
                         MR5 -> hexaco60,theta60,NA 
                         MR1 <-> MR1,NA,1 
                         MR3 <-> MR3,NA,1 
                         MR2 <-> MR2,NA,1 
                         MR5 <-> MR5,NA,1 
                         MR6 <-> MR6,NA,1 
                         MR4 <-> MR4,NA,1")
N<-64
cfa7.fit <- sem(cfa_model7, S=hexaco_cor,N=N)
summary(cfa7.fit)
##6 factors CFA summed itmes, according to the categories
data<-read.csv2(paste0(dir,'hexaco_sum_pmm.csv'),sep =',')
data_complete<-data[complete.cases(data),]
data_cen<-scale(data_complete)
summary(data_cen)
hex_cor<-cor(data_cen)
colnames(hex_cor)<-c(paste0('hexaco',1:24))
rownames(hex_cor)<-c(paste0('hexaco',1:24))
cfa_model8<-specifyModel(text="MR1 -> hexaco1,theta1,NA 
                            MR1 -> hexaco2,theta2,NA 
                         MR1 -> hexaco3,theta3,NA 
                         MR1 -> hexaco4,theta4,NA 
                         MR2 -> hexaco5,theta5,NA 
                         MR2 -> hexaco6,theta6,NA 
                         MR2 -> hexaco7,theta7,NA 
                         MR2 -> hexaco8,theta8,NA 
                         MR3 -> hexaco9,theta9,NA 
                         MR3 -> hexaco10,theta10,NA 
                         MR3 -> hexaco11,theta11,NA 
                         MR3 -> hexaco12,theta12,NA 
                         MR4 -> hexaco13,theta13,NA 
                         MR4 -> hexaco14,theta14,NA 
                         MR4 -> hexaco15,theta15,NA 
                         MR4 -> hexaco16,theta16,NA 
                         MR5 -> hexaco17,theta17,NA 
                         MR5 -> hexaco18,theta18,NA 
                         MR5 -> hexaco19,theta19,NA 
                         MR5 -> hexaco20,theta20,NA 
                         MR6 -> hexaco21,theta21,NA 
                         MR6 -> hexaco22,theta22,NA 
                         MR6 -> hexaco23,theta23,NA 
                         MR6 -> hexaco24,theta24,NA 
                         MR1 <-> MR1,NA,1 
                         MR3 <-> MR3,NA,1 
                         MR2 <-> MR2,NA,1 
                         MR5 <-> MR5,NA,1 
                         MR6 <-> MR6,NA,1 
                         MR4 <-> MR4,NA,1")
N<-64
cfa8.fit <- sem(cfa_model8, S=hex_cor,N=N)
summary(cfa8.fit)
##7 factors CFA summed itmes, according to the EFA
data<-read.csv2(paste0(dir,'hexaco_sum_pmm.csv'),sep =',')
data_complete<-data[complete.cases(data),]
data_cen<-scale(data_complete)
summary(data_cen)
hex_cor<-cor(data_cen)
colnames(hex_cor)<-c(paste0('hexaco',1:24))
rownames(hex_cor)<-c(paste0('hexaco',1:24))

hex_factors<-fa(r=hex_cor,nfactors = 6,rotate = "varimax")

hex_loadings<-data.frame(abs(unclass(hex_factors$loadings)))
hex_loadings<-data.frame(t(apply(hex_loadings,1,function(x) replace(x,x==max(x),100))))
formula<-c()
factors<-colnames(hex_loadings)
for(i in 1:24){
  for(j in 1:6){
    if(hex_loadings[i,j]==100){
      formula<-c(formula,paste0(factors[j]," -> ","hexaco",as.character(i),",","theta",as.character(i),",NA"),"\n")    
    }
  }
}
for(j in 1:6){
  formula<-c(formula,paste0(factors[j]," <-> ",factors[j],",NA,1"),"\n")
}

cfa_model9<-specifyModel(text="MR3 -> hexaco1,theta1,NA 
 MR3 -> hexaco2,theta2,NA 
                         MR6 -> hexaco3,theta3,NA 
                         MR2 -> hexaco4,theta4,NA 
                         MR1 -> hexaco5,theta5,NA 
                         MR4 -> hexaco6,theta6,NA 
                         MR1 -> hexaco7,theta7,NA 
                         MR1 -> hexaco8,theta8,NA 
                         MR6 -> hexaco9,theta9,NA 
                         MR2 -> hexaco10,theta10,NA 
                         MR2 -> hexaco11,theta11,NA 
                         MR5 -> hexaco12,theta12,NA 
                         MR5 -> hexaco13,theta13,NA 
                         MR5 -> hexaco14,theta14,NA 
                         MR1 -> hexaco15,theta15,NA 
                         MR3 -> hexaco16,theta16,NA 
                         MR3 -> hexaco17,theta17,NA 
                         MR3 -> hexaco18,theta18,NA 
                         MR5 -> hexaco19,theta19,NA 
                         MR3 -> hexaco20,theta20,NA 
                         MR4 -> hexaco21,theta21,NA 
                         MR5 -> hexaco22,theta22,NA 
                         MR4 -> hexaco23,theta23,NA 
                         MR5 -> hexaco24,theta24,NA 
                         MR3 <-> MR3,NA,1 
                         MR5 <-> MR5,NA,1 
                         MR2 <-> MR2,NA,1 
                         MR1 <-> MR1,NA,1 
                         MR4 <-> MR4,NA,1 
                         MR6 <-> MR6,NA,1")
N<-64
cfa9.fit <- sem(cfa_model9, S=cor.smooth(hex_cor),N=N)
#cfa9.fit <- sem(cfa_model9,data=data_complete)
cfa9.fit
AIC(cfa9.fit)
BIC(cfa9.fit)
mean(normalizedResiduals(cfa9.fit))

##12.14 uodate:remove variables and likehood ration test
#Hexaco
#sum
data<-read.csv2(paste0(dir,'hexaco_sum_pmm.csv'),sep =',')
hex_cor<-cor(data)
hex_sum<-fa(r=hex_cor,n.obs=64,nfactors = 6,rotate = "varimax")
hex_sum
#-socialbility
data<-read.csv2(paste0(dir,'hexaco_sum_pmm.csv'),sep =',')
data$socialbility<-NULL
hex_cor1<-cor(data)
hex1<-fa(r=hex_cor1, n.obs=64,nfactors =6,rotate = "varimax")
hex1
anova(hex_factors,hex1)

#-prudence
data<-read.csv2(paste0(dir,'hexaco_sum_pmm.csv'),sep =',')
data$prudence<-NULL
hex_cor1<-cor(data)
hex2<-fa(r=hex_cor1, n.obs=64,nfactors =6,rotate = "varimax")
hex2
anova(hex_factors,hex2)

#-socialbility,prudence
data<-read.csv2(paste0(dir,'hexaco_sum_pmm.csv'),sep =',')
data$prudence<-NULL
data$socialbility<-NULL
hex_cor1<-cor(data)
hex3<-fa(r=hex_cor1, n.obs=64,nfactors =6,rotate = "varimax")
hex3
anova(hex_factors,hex3)

#-sentimental
data<-read.csv2(paste0(dir,'hexaco_sum_pmm.csv'),sep =',')
data$sentimentality<-NULL
hex_cor1<-cor(data)
hex4<-fa(r=hex_cor1, n.obs=64,nfactors =6,rotate = "varimax")
hex4
anova(hex_factors,hex4)

##all itmes
data<-read.csv2(paste0(dir,'hexaco_pmm.csv'),sep =',')
hexaco_cor<-cor(data)
hex_all<-fa(r=hexaco_cor, n.obs=64,nfactors =6,rotate = "varimax")
hex_all

#-socialbility
data<-read.csv2(paste0(dir,'hexaco_pmm.csv'),sep =',')
data$hexaco16<-NULL
data$hexaco40<-NULL
hex_cor<-cor(data)
hex_1<-fa(r=hex_cor, n.obs=64,nfactors =6,rotate = "varimax")
hex_1
anova(hex_1,hex_all)

#-prudence
data<-read.csv2(paste0(dir,'hexaco_pmm.csv'),sep =',')
data$hexaco20<-NULL
data$hexaco44<-NULL
data$hexaco56<-NULL
hex_cor<-cor(data)
hex_2<-fa(r=hex_cor, n.obs=64,nfactors =6,rotate = "varimax")
hex_2
anova(hex_2,hex_all)

#-prudence
data<-read.csv2(paste0(dir,'hexaco_pmm.csv'),sep =',')
data$hexaco23<-NULL
data$hexaco47<-NULL
data$hexaco59<-NULL
hex_cor<-cor(data)
hex_3<-fa(r=hex_cor, n.obs=64,nfactors =6,rotate = "varimax")
hex_3
anova(hex_3,hex_all)

#-flexbility
data<-read.csv2(paste0(dir,'hexaco_pmm.csv'),sep =',')
data$hexaco15<-NULL
data$hexaco39<-NULL
data$hexaco57<-NULL
hex_cor<-cor(data)
hex_4<-fa(r=hex_cor, n.obs=64,nfactors =6,rotate = "varimax")
hex_4
anova(hex_4,hex_all)

#-
data<-read.csv2(paste0(dir,'hexaco_pmm.csv'),sep =',')
spec<-c(3,10,19,21,27,31,32,46,47,48,58)
data<-subset(data, select = -spec)
hex_cor<-cor(data)
hex_5<-fa(r=hex_cor, n.obs=64,nfactors =6,rotate = "varimax")
hex_5
anova(hex_5,hex_all)

data<-read.csv2(paste0(dir,'hexaco_pmm.csv'),sep =',')
spec<-c(47,48,58)
data<-subset(data, select = -spec)
hex_cor<-cor(data)
hex_6<-fa(r=hex_cor, n.obs=64,nfactors =6,rotate = "varimax")
hex_6
anova(hex_5,hex_6)
colnames(hex_cor)<-c(paste0('hexaco',1:57))
rownames(hex_cor)<-c(paste0('hexaco',1:57))
hex_loadings<-data.frame(abs(unclass(hex_6$loadings)))
hex_loadings<-data.frame(t(apply(hex_loadings,1,function(x) replace(x,x==max(x),100))))
formula<-c()
factors<-colnames(hex_loadings)
for(i in 1:57){
  for(j in 1:6){
    if(hex_loadings[i,j]==100){
      formula<-c(formula,paste0(factors[j]," -> ","hexaco",as.character(i),",","theta",as.character(i),",NA"),"\n")    
    }
  }
}
for(j in 1:6){
  formula<-c(formula,paste0(factors[j]," <-> ",factors[j],",NA,1"),"\n")
}
##6 factors CFA 
cfa_model2<-specifyModel(text="MR1 -> hexaco1,theta1,NA 
 MR3 -> hexaco2,theta2,NA 
                        MR6 -> hexaco3,theta3,NA 
                        MR3 -> hexaco4,theta4,NA 
                        MR2 -> hexaco5,theta5,NA 
                        MR5 -> hexaco6,theta6,NA 
                        MR3 -> hexaco7,theta7,NA 
                        MR6 -> hexaco8,theta8,NA 
                        MR3 -> hexaco9,theta9,NA 
                        MR6 -> hexaco10,theta10,NA 
                        MR2 -> hexaco11,theta11,NA 
                        MR3 -> hexaco12,theta12,NA 
                        MR4 -> hexaco13,theta13,NA 
                        MR5 -> hexaco14,theta14,NA 
                        MR1 -> hexaco15,theta15,NA 
                        MR1 -> hexaco16,theta16,NA 
                        MR2 -> hexaco17,theta17,NA 
                        MR5 -> hexaco18,theta18,NA 
                        MR5 -> hexaco19,theta19,NA 
                        MR5 -> hexaco20,theta20,NA 
                        MR1 -> hexaco21,theta21,NA 
                        MR1 -> hexaco22,theta22,NA 
                        MR2 -> hexaco23,theta23,NA 
                        MR2 -> hexaco24,theta24,NA 
                        MR6 -> hexaco25,theta25,NA 
                        MR4 -> hexaco26,theta26,NA 
                        MR1 -> hexaco27,theta27,NA 
                        MR1 -> hexaco28,theta28,NA 
                        MR2 -> hexaco29,theta29,NA 
                        MR3 -> hexaco30,theta30,NA 
                        MR1 -> hexaco31,theta31,NA 
                        MR5 -> hexaco32,theta32,NA 
                        MR1 -> hexaco33,theta33,NA 
                        MR4 -> hexaco34,theta34,NA 
                        MR6 -> hexaco35,theta35,NA 
                        MR4 -> hexaco36,theta36,NA 
                        MR6 -> hexaco37,theta37,NA 
                        MR1 -> hexaco38,theta38,NA 
                        MR1 -> hexaco39,theta39,NA 
                        MR1 -> hexaco40,theta40,NA 
                        MR1 -> hexaco41,theta41,NA 
                        MR6 -> hexaco42,theta42,NA 
                        MR6 -> hexaco43,theta43,NA 
                        MR1 -> hexaco44,theta44,NA 
                        MR2 -> hexaco45,theta45,NA 
                        MR1 -> hexaco46,theta46,NA 
                        MR6 -> hexaco47,theta47,NA 
                        MR1 -> hexaco48,theta48,NA 
                        MR3 -> hexaco49,theta49,NA 
                        MR2 -> hexaco50,theta50,NA 
                        MR5 -> hexaco51,theta51,NA 
                        MR6 -> hexaco52,theta52,NA 
                        MR6 -> hexaco53,theta53,NA 
                        MR4 -> hexaco54,theta54,NA 
                        MR2 -> hexaco55,theta55,NA 
                        MR4 -> hexaco56,theta56,NA 
                        MR4 -> hexaco57,theta57,NA 
                        MR1 <-> MR1,NA,1 
                        MR4 <-> MR4,NA,1 
                        MR2 <-> MR2,NA,1 
                        MR3 <-> MR3,NA,1 
                        MR6 <-> MR6,NA,1 
                        MR5 <-> MR5,NA,1 ")
N<-64
cfa.fit2 <- sem(cfa_model2, S=hex_cor,N=N)
summary(cfa.fit2)
