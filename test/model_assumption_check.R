#library(gvlma)
library(car)
library(showtext)
library(showtextdb)
# model assumption check

# friend_diff
privacy[,6]<-privacy[,1]*sum_score
privacy[,7]<-privacy[,2]*sum_score
m10<-glm(family_c[,1]~ privacy[,1]+privacy[,2]+privacy[,3]+privacy[,4]+privacy[,5]+sum_score+privacy[,6]+age+gender+edu+nfamily+employ+muni+marital,weights = weight)
m11<-glm(family_c[,1]~ privacy[,1]+privacy[,2]+privacy[,3]+privacy[,4]+privacy[,5]+sum_score+privacy[,7]+age+gender+edu+nfamily+employ+muni+marital,weights = weight)
m15<-glm(family_c[,1]~ privacy[,1]+privacy[,2]+privacy[,3]+privacy[,4]+privacy[,5]+sum_score+privacy[,6]+privacy[,7]+age+gender+edu+nfamily+employ+muni+marital,weights = weight)
summary(m15)
coef(summary(m15))[,c(1,4)]
pdf(paste0(dir,"friend_diff","_m10",'.pdf'),width = 9,height = 10)
showtext.begin()
crPlots(m10)
par(mfrow = c(2, 2))
plot(m10)
showtext.end()
dev.off()

pdf(paste0(dir,"friend_diff","_m11",'.pdf'),width = 9,height = 10)
showtext.begin()

crPlots(m11)
par(mfrow = c(2, 2))
plot(m11)
showtext.end()
dev.off()

pdf(paste0(dir,"friend_diff","_m15",'.pdf'),width = 9,height = 10)
showtext.begin()
crPlots(m15)
par(mfrow = c(2, 2))
plot(m15)
showtext.end()
dev.off()

#No_companion
m9<-glm(family_c[,2]~ privacy[,1]+privacy[,2]+privacy[,3]+privacy[,4]+privacy[,5]+sum_score+age+gender+edu+nfamily+employ+muni+marital,weights = weight)
pdf(paste0(dir,"No_companion","_m9",'.pdf'),width = 9,height = 10)
showtext.begin()
crPlots(m9)
par(mfrow = c(2, 2))
plot(m9)
showtext.end()
dev.off()

#Traditions
m9<-glm(family_c[,3]~ privacy[,1]+privacy[,2]+privacy[,3]+privacy[,4]+privacy[,5]+sum_score+age+gender+edu+nfamily+employ+muni+marital,weights = weight)
pdf(paste0(dir,"Traditions","_m9",'.pdf'),width = 9,height = 10)
showtext.begin()
crPlots(m9)
par(mfrow = c(2, 2))
plot(m9)
showtext.end()
dev.off()

m15<-glm(family_c[,3]~ privacy[,1]+privacy[,2]+privacy[,3]+privacy[,4]+privacy[,5]+sum_score+I(bidr_sum[,1]^2)+age+gender+edu+nfamily+employ+muni+marital,weights = weight)
pdf(paste0(dir,"Traditions","_m15",'.pdf'),width = 9,height = 10)
showtext.begin()
crPlots(m15)
par(mfrow = c(2, 2))
plot(m15)
showtext.end()
dev.off()

#rlp_parent
m9<-glm(family_c[,4]~ privacy[,1]+privacy[,2]+privacy[,3]+privacy[,4]+privacy[,5]+sum_score+age+gender+edu+nfamily+employ+muni+marital,weights = weight)
pdf(paste0(dir,"rlp_parent","_m9",'.pdf'),width = 9,height = 10)
showtext.begin()
crPlots(m9)
par(mfrow = c(2, 2))
plot(m9)
showtext.end()
dev.off()

m9<-glm(family_c[,4]~ privacy[,1]+privacy[,2]+privacy[,3]+privacy[,4]+privacy[,5]+sum_score+age+gender+edu+nfamily+employ+muni+marital,weights = weight,family=gaussian(link="log"))
pdf(paste0(dir,"rlp_parent","_m9_c",'.pdf'),width = 9,height = 10)
showtext.begin()
crPlots(m9)
par(mfrow = c(2, 2))
plot(m9)
showtext.end()
dev.off()


#m2
m2<-glm(family_cm[,4]~ io1_parent+io1_spouse+io1_siblings+io1_children+io1_parentinlaw+sum_score+age+gender+edu+nfamily+employ+muni+marital,weights = weight,data=family_cm)
pdf(paste0(dir,"rlp_parent","_m2",'.pdf'),width = 9,height = 10)
showtext.begin()
crPlots(m2)
par(mfrow = c(2, 2))
plot(m2)
showtext.end()
dev.off()

#rlp_spouse
privacy[,6]<-privacy[,4]*sum_score
privacy[,7]<-privacy[,5]*sum_score
m13<-glm(family_c[,5]~ privacy[,1]+privacy[,2]+privacy[,3]+privacy[,4]+privacy[,5]+sum_score+privacy[,6]+age+gender+edu+nfamily+employ+muni,weights = weight)
m14<-glm(family_c[,5]~ privacy[,1]+privacy[,2]+privacy[,3]+privacy[,4]+privacy[,5]+sum_score+privacy[,7]+age+gender+edu+nfamily+employ+muni,weights = weight)
m15<-glm(family_c[,5]~ privacy[,1]+privacy[,2]+privacy[,3]+privacy[,4]+privacy[,5]+sum_score+privacy[,6]+privacy[,7]+age+gender+edu+nfamily+employ+muni,weights = weight)

#m9<-glm(family_c[,4]~ privacy[,1]+privacy[,2]+privacy[,3]+privacy[,4]+privacy[,5]+sum_score+age+gender+edu+nfamily+employ+muni+marital,weights = weight)
pdf(paste0(dir,"rlp_spouse","_m13",'.pdf'),width = 9,height = 10)
showtext.begin()
crPlots(m13)
par(mfrow = c(2, 2))
plot(m13)
showtext.end()
dev.off()

pdf(paste0(dir,"rlp_spouse","_m14",'.pdf'),width = 9,height = 10)
showtext.begin()
crPlots(m14)
par(mfrow = c(2, 2))
plot(m14)
showtext.end()
dev.off()

pdf(paste0(dir,"rlp_spouse","_m15",'.pdf'),width = 9,height = 10)
showtext.begin()
crPlots(m15)
par(mfrow = c(2, 2))
plot(m15)
showtext.end()
dev.off()
round(coef(summary(m15))[,c(1,4)],3)

#m6
m6<-glm(family_cm[,5]~ io1_parent+io1_spouse+io1_siblings+io1_children+io1_parentinlaw+sum_score+V9+age+gender+edu+nfamily+employ+muni,weights = weight,data=family_cm)
pdf(paste0(dir,"rlp_spouse","_m6",'.pdf'),width = 9,height = 10)
showtext.begin()
crPlots(m6)
par(mfrow = c(2, 2))
plot(m6)
showtext.end()
dev.off()
round(coef(summary(m15))[,c(1,4)],3)


#rlp_siblings
m14<-glm(family_c[,6]~ privacy[,1]+privacy[,2]+privacy[,3]+privacy[,4]+privacy[,5]+sum_score+privacy[,7]+age+gender+edu+nfamily+employ+muni+marital,weights = weight)
pdf(paste0(dir,"rlp_siblings","_m14",'.pdf'),width = 9,height = 10)
showtext.begin()
crPlots(m14)
par(mfrow = c(2, 2))
plot(m14)
showtext.end()
dev.off()

m7<-glm(family_cm[,6]~ io1_parent+io1_spouse+io1_siblings+io1_children+io1_parentinlaw+sum_score+V10+age+gender+edu+nfamily+employ+muni+marital,weights = weight,data=family_cm)
pdf(paste0(dir,"rlp_siblings","_m7",'.pdf'),width = 9,height = 10)
showtext.begin()
crPlots(m7)
par(mfrow = c(2, 2))
plot(m7)
showtext.end()
dev.off()



#secret_spouse
m14<-glm(family_c[,7]~ privacy[,1]+privacy[,2]+privacy[,3]+privacy[,4]+privacy[,5]+sum_score+privacy[,7]+age+gender+edu+nfamily+employ+muni,weights = weight)
pdf(paste0(dir,"secret_spouse","_m14",'.pdf'),width = 9,height = 10)
showtext.begin()
crPlots(m14)
par(mfrow = c(2, 2))
plot(m14)
showtext.end()
dev.off()

#secret_parent
m9<-glm(family_c[,8]~ privacy[,1]+privacy[,2]+privacy[,3]+privacy[,4]+privacy[,5]+sum_score+age+gender+edu+nfamily+employ+muni+marital,weights = weight)
pdf(paste0(dir,"secret_parent","_m9",'.pdf'),width = 9,height = 10)
showtext.begin()
crPlots(m9)
par(mfrow = c(2, 2))
plot(m9)
showtext.end()
dev.off()

#event mixed
m2<-glm(family_cm[,11]~ io1_parent+io1_spouse+io1_siblings+io1_children+io1_parentinlaw+sum_score+age+gender+edu+nfamily+employ+muni+marital,weights = weight,data=family_cm)
pdf(paste0(dir,"event_mixed","_m2",'.pdf'),width = 9,height = 10)
showtext.begin()
crPlots(m2)
par(mfrow = c(2, 2))
plot(m2)
showtext.end()
dev.off()



