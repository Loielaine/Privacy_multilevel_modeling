# Interview Privacy Factor Analysis  #
# liyixi #
# 2018.11.4 #

library(readr)
library(psych)
library(nnet)
dir<-'/Users/Loielaine/Desktop/Good_Good_Study/2018Survey project/data/'
data<-read.csv2(paste0(dir,'behavior_inverse.csv'),sep =',')[,1:19]
demo<-read.csv2(paste0(dir,'demo.csv'),sep =',')
data$gender<-demo$gender 
data$educ<-demo$educ
data$marital<-demo$marital
data$employment<-demo$jobnow
data$age<-2018-demo$birth
data[(data$age)>100,"age"]<-NA
data[data<0]<-NA
data<-apply(data,2,as.numeric)
data[,1:23]<-as.factor(data[,1:23])

#initialize
data_raw<-data.frame(data)
data_srmi<-data_raw

##demographic imputation
#impute employment~gender,marital
employ_impute<-multinom(employment~gender+marital, data=data_srmi)
summary(employ_impute)
sum(is.na(data_srmi$employment))
data_srmi[is.na(data_srmi$employment),"employment"] <- predict(employ_impute,newdata = data_srmi[is.na(data_srmi$employment),],type='class')

#impute educ~gender,marital,employment
educ_impute<-multinom(educ~gender+marital+employment, data=data_srmi)
summary(educ_impute)
sum(is.na(data_srmi$educ))
data_srmi[is.na(data_srmi$educ),"educ"] <- predict(educ_impute, newdata = data_srmi[is.na(data_srmi$educ),], type='class')

#impute age~gender,marital,employment,educ
age_impute<-glm(age~gender+marital+employment+educ, data=data_srmi)
summary(age_impute)
sum(is.na(data_srmi$age))
data_srmi[is.na(data_srmi$age),"age"] <- predict(age_impute,newdata=data_srmi[is.na(data_srmi$age),], type='link')

write.table(data_srmi,paste0(dir,"data_demo_imputed.csv"),row.names=FALSE)

#sequential imputation of bidr
#srclib <<-"/Users/Loielaine/Desktop/Good_Good_Study/2018Survey project/code/srclib/R"
#source(file.path(srclib, "init.R", fsep=.Platform$file.sep))

variables<-c("age","gender","educ","employment","marital")
missing<-c("bidr22",
           "bidr32",
           "bidr28",
           "bidr40",
          "bidr24",
           "bidr27",
          "bidr38",
          "bidr21",
          "bidr26",
           "bidr29",
           "bidr36",
           "bidr25",
           "bidr23",
           "bidr39",
           "bidr35",
           "bidr37",
           "bidr31",
           "bidr33",
           "bidr30")

#sequential_imputing
model<- multinom(bidr22~age+gender+marital+employment+educ,data=data_srmi)
data_srmi[is.na(data_srmi$bidr22),"bidr22"] <- predict(model,newdata=data_srmi[is.na(data_srmi$bidr22),], type='class')
sum(is.na(data_srmi$bidr22))
summary(data_srmi$bidr22)

model<- multinom(bidr32~bidr22+age+gender+marital+employment+educ,data=data_srmi)
data_srmi[is.na(data_srmi$bidr32),"bidr32"] <- predict(model,newdata=data_srmi[is.na(data_srmi$bidr32),], type='class')

model<- multinom(bidr28~bidr32+bidr22+age+gender+marital+employment+educ,data=data_srmi)
data_srmi[is.na(data_srmi$bidr28),"bidr28"] <- predict(model,newdata=data_srmi[is.na(data_srmi$bidr28),], type='class')

model<- multinom(bidr40~bidr28+bidr32+bidr22+age+gender+marital+employment+educ,data=data_srmi)
data_srmi[is.na(data_srmi$bidr40),"bidr40"] <- predict(model,newdata=data_srmi[is.na(data_srmi$bidr40),], type='class')

model<- multinom(bidr24~bidr40+bidr28+bidr32+bidr22+age+gender+marital+employment+educ,data=data_srmi)
data_srmi[is.na(data_srmi$bidr24),"bidr24"] <- predict(model,newdata=data_srmi[is.na(data_srmi$bidr24),], type='class')

model<- multinom(bidr27~bidr24+bidr40+bidr28+bidr32+bidr22+age+gender+marital+employment+educ,data=data_srmi)
data_srmi[is.na(data_srmi$bidr27),"bidr27"] <- predict(model,newdata=data_srmi[is.na(data_srmi$bidr27),], type='class')

model<- multinom(bidr38~bidr27+bidr24+bidr40+bidr28+bidr32+bidr22+age+gender+marital+employment+educ,data=data_srmi)
data_srmi[is.na(data_srmi$bidr38),"bidr38"] <- predict(model,newdata=data_srmi[is.na(data_srmi$bidr38),], type='class')

model<- multinom(bidr21~bidr38+bidr27+bidr24+bidr40+bidr28+bidr32+bidr22+age+gender+marital+employment+educ,data=data_srmi)
data_srmi[is.na(data_srmi$bidr21),"bidr21"] <- predict(model,newdata=data_srmi[is.na(data_srmi$bidr21),], type='class')

model<- multinom(bidr26~bidr21+bidr38+bidr27+bidr24+bidr40+bidr28+bidr32+bidr22+age+gender+marital+employment+educ,data=data_srmi)
data_srmi[is.na(data_srmi$bidr26),"bidr26"] <- predict(model,newdata=data_srmi[is.na(data_srmi$bidr26),], type='class')

model<- multinom(bidr29~bidr26+bidr21+bidr38+bidr27+bidr24+bidr40+bidr28+bidr32+bidr22+age+gender+marital+employment+educ,data=data_srmi)
data_srmi[is.na(data_srmi$bidr29),"bidr29"] <- predict(model,newdata=data_srmi[is.na(data_srmi$bidr29),], type='class')

model<- multinom(bidr36~bidr29+bidr26+bidr21+bidr38+bidr27+bidr24+bidr40+bidr28+bidr32+bidr22+age+gender+marital+employment+educ,data=data_srmi)
data_srmi[is.na(data_srmi$bidr36),"bidr36"] <- predict(model,newdata=data_srmi[is.na(data_srmi$bidr36),], type='class')

model<- multinom(bidr25~bidr36+bidr29+bidr26+bidr21+bidr38+bidr27+bidr24+bidr40+bidr28+bidr32+bidr22+age+gender+marital+employment+educ,data=data_srmi)
data_srmi[is.na(data_srmi$bidr25),"bidr25"] <- predict(model,newdata=data_srmi[is.na(data_srmi$bidr25),], type='class')

model<- multinom(bidr23~bidr25+bidr36+bidr29+bidr26+bidr21+bidr38+bidr27+bidr24+bidr40+bidr28+bidr32+bidr22+age+gender+marital+employment+educ,data=data_srmi)
data_srmi[is.na(data_srmi$bidr23),"bidr23"] <- predict(model,newdata=data_srmi[is.na(data_srmi$bidr23),], type='class')

model<- multinom(bidr39~bidr23+bidr25+bidr36+bidr29+bidr26+bidr21+bidr38+bidr27+bidr24+bidr40+bidr28+bidr32+bidr22+age+gender+marital+employment+educ,data=data_srmi)
data_srmi[is.na(data_srmi$bidr39),"bidr39"] <- predict(model,newdata=data_srmi[is.na(data_srmi$bidr39),], type='class')

model<- multinom(bidr35~bidr39+bidr23+bidr25+bidr36+bidr29+bidr26+bidr21+bidr38+bidr27+bidr24+bidr40+bidr28+bidr32+bidr22+age+gender+marital+employment+educ,data=data_srmi)
data_srmi[is.na(data_srmi$bidr35),"bidr35"] <- predict(model,newdata=data_srmi[is.na(data_srmi$bidr35),], type='class')

model<- multinom(bidr37~bidr35+bidr39+bidr23+bidr25+bidr36+bidr29+bidr26+bidr21+bidr38+bidr27+bidr24+bidr40+bidr28+bidr32+bidr22+age+gender+marital+employment+educ,data=data_srmi)
data_srmi[is.na(data_srmi$bidr37),"bidr37"] <- predict(model,newdata=data_srmi[is.na(data_srmi$bidr37),], type='class')

model<- multinom(bidr31~bidr37+bidr35+bidr39+bidr23+bidr25+bidr36+bidr29+bidr26+bidr21+bidr38+bidr27+bidr24+bidr40+bidr28+bidr32+bidr22+age+gender+marital+employment+educ,data=data_srmi)
data_srmi[is.na(data_srmi$bidr31),"bidr31"] <- predict(model,newdata=data_srmi[is.na(data_srmi$bidr31),], type='class')

model<- multinom(bidr33~bidr31+bidr37+bidr35+bidr39+bidr23+bidr25+bidr36+bidr29+bidr26+bidr21+bidr38+bidr27+bidr24+bidr40+bidr28+bidr32+bidr22+age+gender+marital+employment+educ,data=data_srmi)
data_srmi[is.na(data_srmi$bidr33),"bidr33"] <- predict(model,newdata=data_srmi[is.na(data_srmi$bidr33),], type='class')

model<- multinom(bidr30~bidr33+bidr31+bidr37+bidr35+bidr39+bidr23+bidr25+bidr36+bidr29+bidr26+bidr21+bidr38+bidr27+bidr24+bidr40+bidr28+bidr32+bidr22+age+gender+marital+employment+educ,data=data_srmi)
data_srmi[is.na(data_srmi$bidr30),"bidr30"] <- predict(model,newdata=data_srmi[is.na(data_srmi$bidr30),], type='class')

#partial imputing
model<- multinom(bidr22~bidr30+bidr33+bidr31+bidr37+bidr35+bidr39+bidr23+bidr25+bidr36+bidr29+bidr26+bidr21+bidr38+bidr27+bidr24+bidr40+bidr28+bidr32+age+gender+marital+employment+educ,data=data_srmi)
data_srmi[is.na(data_srmi$bidr30),"bidr30"] <- predict(model,newdata=data_srmi[is.na(data_srmi$bidr30),], type='class')




data_bidr_demo_imputed<-data_srmi
data_bidr_demo_imputed[,1:23]<-as.factor(data_bidr_demo_imputed[,1:23])

write.table(data_bidr_demo_imputed,paste0(dir,"data_bidr_demo_imputed.csv"),row.names=FALSE)



