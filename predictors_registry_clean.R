#################################
# helix and edw
#################################
library(lubridate)
# predictors
# age
helix.predictor=data.frame(Age=as.numeric(helix.data$Age))
edw.predictor=data.frame(Age=as.numeric(edw.data$Age))
# prior heart failure
helix.predictor$PriorHF=helix.data$PriorHF=="Yes"
tmp=edw.data$PriorHF
tmp[tmp=="NULL"]=NA
edw.predictor$PriorHF=tmp=="1"
# prior cardio shock
helix.predictor$PriorCardioShock=helix.data$PriorCardioShock=="Yes"
tmp=edw.data$PriorCardioShock
tmp[tmp=="NULL"]=NA
edw.predictor$PriorCardioShock=tmp=="1"
# prior cardiac arrest
helix.predictor$PriorCardiacArrest=helix.data$PriorCardiacArrest=="Yes"
tmp=edw.data$PriorCardiacArrest
tmp[tmp=="NULL"]=NA
edw.predictor$PriorCardiacArrest=tmp=="1"
# diabetes composite
data=helix.data
Diabetes=rep(NA,length(data$Diabetes))
Diabetes[data$Diabetes=="No"]=1
Diabetes[data$Diabetes=="Yes" & data$DiabetesControl=="Oral"]=2
Diabetes[data$Diabetes=="Yes" & data$DiabetesControl=="Insulin"]=3
Diabetes[data$Diabetes=="Yes" & (data$DiabetesControl %in% c("None","Diet","Other"))]=4
helix.predictor$Diabetescat=factor(Diabetes)
data=edw.data
Diabetes=rep(NA,length(data$Diabetes))
Diabetes[data$Diabetes=="0"]=1
Diabetes[data$Diabetes=="1" &data$DiabetesControl=="3"]=2
Diabetes[data$Diabetes=="1" & data$DiabetesControl=="4"]=3
Diabetes[data$Diabetes=="1" & (data$DiabetesControl %in% c("1","2","5"))]=4
edw.predictor$Diabetescat=factor(Diabetes)
# CAD presentation
data=helix.data
CADPresentation=rep(NA,length(data$CADPresentation))
CADPresentation[which(data$CADPresentation%in%c("No symptom, no angina","Symptom unlikely to be ischemic"))]=1
CADPresentation[which(data$CADPresentation=="Stable angina")]=2
CADPresentation[which(data$CADPresentation=="Unstable angina")]=3
CADPresentation[which(data$CADPresentation=="Non-STEMI")]=4
CADPresentation[data$CADPresentation=="ST-Elevation MI (STEMI) or equivalent" & data$ThromTherapy=="No"]=5
CADPresentation[data$CADPresentation=="ST-Elevation MI (STEMI) or equivalent" & data$ThromTherapy=="Yes"]=6
helix.predictor$CADPresentationcat=factor(CADPresentation)
data=edw.data
CADPresentation=rep(NA,length(data$CADPresentation))
CADPresentation[which(data$CADPresentation%in%c("1","2"))]=1 
CADPresentation[which(data$CADPresentation=="3")]=2
CADPresentation[which(data$CADPresentation=="4")]=3
CADPresentation[which(data$CADPresentation=="5")]=4
CADPresentation[data$CADPresentation=="6" & data$ThromTherapy=="0"]=5
CADPresentation[data$CADPresentation=="6" & data$ThromTherapy=="1"]=6
edw.predictor$CADPresentationcat=factor(CADPresentation)
# HF within 2 weeks
data=helix.data
Prior2weeksHF=rep(NA,length(data$Prior2weeksHF))
Prior2weeksHF[data$Prior2weeksHF=="No"]=1
Prior2weeksHF[data$Prior2weeksHF=="Yes" & data$Prior2weekNYHA=="Class I"]=2
Prior2weeksHF[data$Prior2weeksHF=="Yes" & data$Prior2weekNYHA=="Class II"]=3
Prior2weeksHF[data$Prior2weeksHF=="Yes" & data$Prior2weekNYHA=="Class III"]=4
Prior2weeksHF[data$Prior2weeksHF=="Yes" & data$Prior2weekNYHA=="Class IV"]=5
helix.predictor$Prior2weeksHFcat=factor(Prior2weeksHF)
data=edw.data
Prior2weeksHF=rep(NA,length(data$Prior2weeksHF))
Prior2weeksHF[data$Prior2weeksHF=="0"]=1
Prior2weeksHF[data$Prior2weeksHF=="1" & data$Prior2weekNYHA=="1"]=2
Prior2weeksHF[data$Prior2weeksHF=="1" & data$Prior2weekNYHA=="2"]=3
Prior2weeksHF[data$Prior2weeksHF=="1" & data$Prior2weekNYHA=="3"]=4
Prior2weeksHF[data$Prior2weeksHF=="1" & data$Prior2weekNYHA=="4"]=5
edw.predictor$Prior2weeksHFcat=factor(Prior2weeksHF)
# GFR
data=helix.data
age=as.numeric(data$Age)
creatinine=as.numeric(data$PreProcCreat)
female=data$Sex=="Female"
black=data$RaceBlack=="Yes"
fmfactor= ifelse(female, 0.742,1)
blkfactor= ifelse(black, 1.210,1) 
egfr=186.3*((creatinine)^(-1.154))*(age^(-0.203))*fmfactor*blkfactor
helix.predictor$egfr=egfr
data=edw.data
age=as.numeric(data$Age)
creatinine=as.numeric(data$PreProcCreat)
data$Sex[data$Sex=="NULL"]=NA
female=data$Sex=="2"
data$RaceBlack[data$RaceBlack=="NULL"]=NA
black=data$RaceBlack=="1"
fmfactor= ifelse(female, 0.742,1)
blkfactor= ifelse(black, 1.210,1) 
egfr=186.3*((creatinine)^(-1.154))*(age^(-0.203))*fmfactor*blkfactor
edw.predictor$egfr=egfr
# hemoglobin
helix.predictor$PreProcHgb=as.numeric(helix.data$PreProcHgb)
edw.predictor$PreProcHgb=as.numeric(edw.data$PreProcHgb)
# admission source
AdmtSource=rep(NA,length(helix.data$AdmtSource))
AdmtSource[helix.data$AdmtSource=="Emergency department"]=1
AdmtSource[helix.data$AdmtSource=="Transfer in from another acute care facility"]=2
AdmtSource[helix.data$AdmtSource=="Other"]=3
helix.predictor$AdmtSource=factor(AdmtSource)
edw.predictor$AdmtSource=factor(as.numeric(edw.data$AdmtSource))
# BMI
helix.predictor$BMI=10000*as.numeric(helix.data$Weight)/(as.numeric(helix.data$Height)^2)
edw.predictor$BMI=10000*as.numeric(edw.data$Weight)/(as.numeric(edw.data$Height)^2)
# PCI status
PCIStatus=rep(NA,length(helix.data$PCIStatus))
PCIStatus[helix.data$PCIStatus=="Elective"]=1
PCIStatus[helix.data$PCIStatus=="Urgent"]=2
PCIStatus[helix.data$PCIStatus=="Emergency"]=3
PCIStatus[helix.data$PCIStatus=="Salvage"]=4
helix.predictor$PCIStatus=factor(PCIStatus)
edw.predictor$PCIStatus=factor(as.numeric(edw.data$PCIStatus))
# LVEF
helix.predictor$PrePCILVEF=as.numeric(helix.data$PrePCILVEF)
edw.predictor$PrePCILVEF=as.numeric(edw.data$PrePCILVEF)

#################################
# lumedx
#################################
# age
lumedx.data$Age=interval(lumedx.data$DOB,lumedx.data$ProcedureDate)/years(1)
lumedx.predictor=data.frame(Age=as.numeric(lumedx.data$Age))
# prior heart failure
tmp=lumedx.data$ACCCHF
tmp[tmp=="NULL"]=NA
lumedx.predictor$PriorHF=tmp=="Yes"
# prior cardio shock
tmp=lumedx.data$CardiogenicShock24Hours
tmp[tmp=="NULL"]=NA
lumedx.predictor$PriorCardioShock=tmp=="1"
# prior cardiac arrest
data=lumedx.data
tmp=rep(0,length(data$CardiacArrest24Hours))
# tmp[data$CardiacArrest24Hours=="0"|(data$CardiacArrestInHospital=="0"&
#       data$CardiacArrestOutHospital=="0"&data$CardiacArrestPostEMS=="0"&
#       data$CardiacArrestTransferFacility=="0"&data$CardiacArrestWitness=="0")]=0
tmp[data$CardiacArrest24Hours=="1"|data$CardiacArrestInHospital=="1"|
      data$CardiacArrestOutHospital=="1"|data$CardiacArrestPostEMS=="1"|
      data$CardiacArrestTransferFacility=="1"|data$CardiacArrestWitness=="1"]=1
lumedx.predictor$PriorCardiacArrest=tmp
# diabetes composite
data=lumedx.data
Diabetes=rep(NA,length(data$DiabetesTherapy))
Diabetes[data$DiabetesHistory=="No"]=1
Diabetes[data$DiabetesHistory=="Yes" & data$DiabetesTherapy=="Oral"]=2
Diabetes[data$DiabetesHistory=="Yes" & data$DiabetesTherapy=="Insulin"]=3
Diabetes[data$DiabetesHistory=="Yes" & (data$DiabetesTherapy %in% c("None","Diet","Other"))]=4
lumedx.predictor$Diabetescat=factor(Diabetes)
# CAD presentation
data=lumedx.data
CADPresentation=rep(NA,length(data$AnginaType))
CADPresentation[data$AnginaType%in%c("No symptoms, no angina","Symptom unlikely to be ischemic")|
                  data$PCIIndication%in%c("CAD (without ischemic Sx)","CAD (without Ischemic Sx)")]=1
CADPresentation[data$AnginaType=="Stable angina"|data$PCIIndication%in%c("Stable angina","Stable Angina")]=2
CADPresentation[(data$AnginaType=="Unstable angina")|data$PCIIndication%in%c("New Onset Angina <= 2 months","PCI for high risk Non-STEMI or unstable angina")]=3
CADPresentation[(data$AnginaType=="Non-STEMI")|data$PCIIndication=="NSTE - ACS"]=4
CADPresentation[(data$AnginaType=="STEMI or equivalent"|data$PCIIndication%in%c("Immediate PCI for STEMI",
                                                                                "PCI for STEMI (Stable, >12 hrs from Sx onset)",
                                                                                "PCI for STEMI (Stable, >12 hrs from Sx onset)",
                                                                                "PCI for STEMI (Unstable, >12 hrs from Sx onset)",
                                                                                "STEMI - Immediate PCI for Acute STEMI",
                                                                                "STEMI - Stable (<= 12 hrs from Sx)",
                                                                                "STEMI - Stable (> 12 hrs from Sx)",
                                                                                "STEMI - Unstable (> 12 hrs from Sx)")) 
                & data$STEMIThrombolytics%in%c("0","NULL")]=5
CADPresentation[(data$AnginaType=="STEMI or equivalent"|data$PCIIndication%in%c("Immediate PCI for STEMI",
                                                                                "PCI for STEMI (Stable, >12 hrs from Sx onset)",
                                                                                "PCI for STEMI (Stable, >12 hrs from Sx onset)",
                                                                                "PCI for STEMI (Unstable, >12 hrs from Sx onset)",
                                                                                "STEMI - Immediate PCI for Acute STEMI",
                                                                                "STEMI - Stable (<= 12 hrs from Sx)",
                                                                                "STEMI - Stable (> 12 hrs from Sx)",
                                                                                "STEMI - Unstable (> 12 hrs from Sx)")) 
                & data$STEMIThrombolytics=="1"]=6
lumedx.predictor$CADPresentationcat=factor(CADPresentation)
# HF within 2 weeks
data=lumedx.data
Prior2weeksHF=rep(NA,length(data$ACCCathIndCHF))
Prior2weeksHF[data$ACCCathIndCHF=="0"|data$HFNewDiagnosis=="0"]=1
Prior2weeksHF[(data$ACCCathIndCHF=="1" & data$CHFClass=="Class 1")|
                (data$HFNewDiagnosis=="1"&data$PriorNYHA=="Class I")]=2
Prior2weeksHF[data$ACCCathIndCHF=="1" & data$CHFClass=="Class 2"|
                (data$HFNewDiagnosis=="1"&data$PriorNYHA=="Class II")]=3
Prior2weeksHF[data$ACCCathIndCHF=="1" & data$CHFClass=="Class 3"|
                (data$HFNewDiagnosis=="1"&data$PriorNYHA=="Class III")]=4
Prior2weeksHF[data$ACCCathIndCHF=="1" & data$CHFClass=="Class 4"|
                (data$HFNewDiagnosis=="1"&data$PriorNYHA=="Class IV")]=5
lumedx.predictor$Prior2weeksHFcat=factor(Prior2weeksHF)
# GFR
data=lumedx.data
age=as.numeric(data$Age)
creatinine=as.numeric(data$LabCreatininePre)
female=data$Gender=="Female"
black=data$RaceAfrican_American=="Yes"
fmfactor= ifelse(female, 0.742,1)
blkfactor= ifelse(black, 1.210,1) 
egfr=186.3*((creatinine)^(-1.154))*(age^(-0.203))*fmfactor*blkfactor
lumedx.predictor$egfr=egfr
# hemoglobin
lumedx.predictor$PreProcHgb=as.numeric(lumedx.data$LabHemoglobinPre)
# admission source
AdmtSource=rep(NA,length(lumedx.data$Admit_Status))
AdmtSource[(lumedx.data$Admit_Status=="Emergency Department")|(lumedx.data$CardiacArrestPostEMS=="1")]=1
AdmtSource[(lumedx.data$Admit_Status=="Transfer Acute Care")|(lumedx.data$CardiacArrestTransferFacility=="1")]=2
AdmtSource[lumedx.data$Admit_Status=="Other"]=3
lumedx.predictor$AdmtSource=factor(AdmtSource)
# BMI
lumedx.predictor$BMI=10000*as.numeric(lumedx.data$`Weight kg`)/(as.numeric(lumedx.data$`Height cm`)^2)
# PCI status
PCIStatus=lumedx.data$InterventionScheduling
PCIStatus[PCIStatus=="Elective"]=1
PCIStatus[PCIStatus=="Urgent"]=2
PCIStatus[PCIStatus=="Emergency"]=3
PCIStatus[PCIStatus=="Salvage"]=4
lumedx.predictor$PCIStatus=factor(PCIStatus)
# LVEF
lumedx.predictor$PrePCILVEF=as.numeric(lumedx.data$PreProcPhysioLVEFPercent)

# > sapply(helix.predictor, function(x) 100*length(which(is.na(x)))/length(x))
# Age            PriorHF   PriorCardioShock PriorCardiacArrest 
# 0.00000000         0.07117438         0.00000000         0.00000000 
# Diabetescat CADPresentationcat   Prior2weeksHFcat               egfr 
# 0.14234875         0.10676157         0.81850534         0.07117438 
# PreProcHgb         AdmtSource                BMI          PCIStatus 
# 5.16014235         0.28469751         0.14234875         0.07117438 
# PrePCILVEF 
# 33.84341637 
# > sapply(edw.predictor, function(x) 100*length(which(is.na(x)))/length(x))
# Age            PriorHF   PriorCardioShock PriorCardiacArrest 
# 0.00000000         0.06331117         0.00000000         0.00000000 
# Diabetescat CADPresentationcat   Prior2weeksHFcat               egfr 
# 0.00000000         0.00000000         0.03165559         0.06331117 
# PreProcHgb         AdmtSource                BMI          PCIStatus 
# 3.26052548         0.03165559         0.06331117         0.00000000 
# PrePCILVEF 
# 40.86736309 
sapply(lumedx.predictor, function(x) 100*length(which(is.na(x)))/length(x))
# Age            PriorHF   PriorCardioShock PriorCardiacArrest 
# 0.0000000          0.0000000          0.0000000          0.0000000 
# Diabetescat CADPresentationcat   Prior2weeksHFcat               egfr 
# 39.2437697         27.4133486         72.6725867          0.0000000 
# PreProcHgb         AdmtSource                BMI          PCIStatus 
# 5.5571469         96.4766543          0.4010312          0.0000000 
# PrePCILVEF 
# 37.2959038 




predictor=rbind(helix.predictor,edw.predictor,lumedx.predictor)
outcome=c(helix.data$AKI,edw.data$AKI,lumedx.data$AKI)
pid=c(helix.data$NCDRPatientID,edw.data$NCDRPatientID,lumedx.data$MRN)



ids.registry=data.frame(
  fid=c(helix.data$fid,edw.data$fid,lumedx.data$fid),
  did=c(rep(1,dim(helix.data)[1]),rep(2,dim(edw.data)[1]),rep(3,dim(lumedx.data)[1])),
  AKI=outcome,
  pid=pid
)
predictor.registry=predictor
registry=cbind(ids.registry,predictor.registry)

registry.xgboost=registry
registry.logreg=cbind(ids.registry,predictor.predreg)

################################################################################################################
AKI=c(helix.data.v2$AKI,edw.data.v2$AKI,lumedx.data.v2$AKI)
helix.data.v2$BMI=10000*as.numeric(helix.data.v2$Weight)/(as.numeric(helix.data.v2$Height)^2)
edw.data.v2$BMI=10000*as.numeric(edw.data.v2$Weight)/(as.numeric(edw.data.v2$Height)^2)
lumedx.data.v2$BMI=10000*as.numeric(lumedx.data.v2$`Weight kg`)/(as.numeric(lumedx.data.v2$`Height cm`)^2)

tmp=as.numeric(c(helix.data.v2$Age,edw.data.v2$Age,lumedx.data.v2$Age))
tmp=c(helix.data.v2$BMI,edw.data.v2$BMI,lumedx.data.v2$BMI)
tmp=c(helix.predictor.v2$egfr,edw.predictor.v2$egfr,lumedx.predictor.v2$egfr)
mean(tmp,na.rm=T)
sd(tmp,na.rm=T)
mean(tmp[AKI==T],na.rm=T)
sd(tmp[AKI==T],na.rm=T)
mean(tmp[AKI==F],na.rm=T)
sd(tmp[AKI==F],na.rm=T)

tmp=helix.data.v2$PriorCardioShock
tmp=tmp=="Yes"
tmp2=edw.data.v2$PriorCardioShock
tmp2=tmp2=="1"
tmp3=lumedx.data.v2$DiabetesHistory
tmp3=tmp3=="Yes"
tmp=c(tmp,tmp2,tmp3)
table(tmp)
mean(tmp,na.rm=T)
table(tmp[AKI==T])
mean(tmp[AKI==T],na.rm=T)
table(tmp[AKI==F])
mean(tmp[AKI==F],na.rm=T)

tmp=c(helix.predictor.v2$PriorCardiacArrest,edw.predictor.v2$PriorCardiacArrest,
      lumedx.predictor.v2$PriorCardiacArrest)
table(tmp)
table(tmp)/length(tmp)
table(tmp[AKI==T])
table(tmp[AKI==T])/length(tmp[AKI==T])
table(tmp[AKI==F])
table(tmp[AKI==F])/length(tmp[AKI==F])


t.test2 <- function(m1,m2,s1,s2,n1,n2,m0=0,equal.variance=FALSE)
{
  if( equal.variance==FALSE ) 
  {
    se <- sqrt( (s1^2/n1) + (s2^2/n2) )
    # welch-satterthwaite df
    df <- ( (s1^2/n1 + s2^2/n2)^2 )/( (s1^2/n1)^2/(n1-1) + (s2^2/n2)^2/(n2-1) )
  } else
  {
    # pooled standard deviation, scaled by the sample sizes
    se <- sqrt( (1/n1 + 1/n2) * ((n1-1)*s1^2 + (n2-1)*s2^2)/(n1+n2-2) ) 
    df <- n1+n2-2
  }      
  t <- (m1-m2-m0)/se 
  dat <- c(m1-m2, se, t, 2*pt(-abs(t),df))    
  names(dat) <- c("Difference of means", "Std Error", "t", "p-value")
  return(dat) 
}

#######################################################
# needs to be done after data splitting
library(VIM)

# imputation
# missing for lumedx data due to version change:
# Diabetescat, CADPresentationcat, Prior2weeksHFcat, AdmtSource
predictor.missing=sapply(predictor, function(x) 100*length(which(is.na(x)))/length(x))
names(sort(predictor.missing))
vars=c("Age","PriorCardioShock","PriorCardiacArrest","PCIStatus","PriorHF","egfr",
           "BMI","PreProcHgb","CADPresentationcat")
distvars=c("Age","PriorCardioShock","PriorCardiacArrest","PCIStatus","PriorHF","egfr",
       "BMI","PreProcHgb","CADPresentationcat","Diabetescat","Prior2weeksHFcat","AdmtSource","PrePCILVEF")
curdata=predictor
predictor.imp=kNN(curdata,variable = vars,dist_var = distvars,trace=T,k=5,imp_var = F,weightDist = F)

vars=c("Diabetescat")
c=which(predictor.imp$Diabetescat!=1|is.na(predictor.imp$Diabetescat))
curdata=predictor.imp[c,]
predictor.imp.diab=kNN(curdata,variable = vars,dist_var = distvars,trace=T,k=5,imp_var = F,weightDist = F)
predictor.imp$Diabetescat[c]=predictor.imp.diab$Diabetescat

vars=c("Prior2weeksHFcat","AdmtSource","PrePCILVEF")
curdata=predictor.imp
predictor.imp=kNN(curdata,variable = vars,dist_var = distvars,trace=T,k=5,imp_var = F,weightDist = F)

sapply(predictor.imp, function(x) 100*length(which(is.na(x)))/length(x))


# arrange predictor order to apply the registry model
library(onehot)
predictor.imp.onehot=predict(onehot(predictor.imp),predictor.imp)
load("datapoints.RData")
index=rep(NA,(length(colnames(datapts))-2))
for (i in 1:(length(colnames(datapts))-2)){
  index[i]=which(colnames(predictor.imp.onehot)==colnames(datapts)[i])
}
predictor.final=predictor.imp.onehot[,index]




#
c=which(helix.data$CurrentDialysis=="Yes")
c.fid.helix=helix.data$fid[c]
c=which(edw.data$CurrentDialysis=="1")
c.fid.edw=edw.data$fid[c]
c=which(lumedx.data$ACCHxKidneyFailure=="Yes")
c.fid.lumedx=lumedx.data$fid[c]

c=which(helix.data$CurrentDialysis=="Yes")
helix.data.v2=helix.data[-c,]
c=which(edw.data$CurrentDialysis=="1")
edw.data.v2=edw.data[-c,]
c=which(lumedx.data$ACCHxKidneyFailure=="Yes")
lumedx.data.v2=lumedx.data[-c,]

c=which(helix.data$CurrentDialysis=="Yes")
helix.predictor.v2=helix.predictor[-c,]
c=which(edw.data$CurrentDialysis=="1")
edw.predictor.v2=edw.predictor[-c,]
c=which(lumedx.data$ACCHxKidneyFailure=="Yes")
lumedx.predictor.v2=lumedx.predictor[-c,]



ehr.test.v2=ehr.test
prediction.bp.ts.xgboost.full.v2=prediction.bp.ts.xgboost.full
for(i in 1:5){
  tmp=ehr.test[[i]]
  tmpred=prediction.bp.ts.xgboost.full[[i]]
  tmp.helix=tmp[tmp$did==1,]
  pred.helix=tmpred[tmp$did==1]
  pred.helix=pred.helix[-which(tmp.helix$fid%in%c.fid.helix)]
  tmp.helix=tmp.helix[-which(tmp.helix$fid%in%c.fid.helix),]
  tmp.edw=tmp[tmp$did==2,]
  pred.edw=tmpred[tmp$did==2]
  pred.edw=pred.edw[-which(tmp.edw$fid%in%c.fid.edw)]
  tmp.edw=tmp.edw[-which(tmp.edw$fid%in%c.fid.edw),]
  tmp.lumedx=tmp[tmp$did==3,]
  pred.lumedx=tmpred[tmp$did==3]
  pred.lumedx=pred.lumedx[-which(tmp.lumedx$fid%in%c.fid.lumedx)]
  tmp.lumedx=tmp.lumedx[-which(tmp.lumedx$fid%in%c.fid.lumedx),]
  ehr.test.v2[[i]]=rbind(tmp.helix,tmp.edw,tmp.lumedx)
  prediction.bp.ts.xgboost.full.v2[[i]]=c(pred.helix,pred.edw,pred.lumedx)
}

ehr.train.v2=ehr.train
for(i in 1:5){
  tmp=ehr.train[[i]]
  tmp.helix=tmp[tmp$did==1,]
  tmp.helix=tmp.helix[-which(tmp.helix$fid%in%c.fid.helix),]
  tmp.edw=tmp[tmp$did==2,]
  tmp.edw=tmp.edw[-which(tmp.edw$fid%in%c.fid.edw),]
  tmp.lumedx=tmp[tmp$did==3,]
  tmp.lumedx=tmp.lumedx[-which(tmp.lumedx$fid%in%c.fid.lumedx),]
  ehr.train.v2[[i]]=rbind(tmp.helix,tmp.edw,tmp.lumedx)
}

ehr.train.ts.v2=ehr.train.ts
for(i in 1:5){
  tmp=ehr.train.ts[[i]]
  tmp.helix=tmp[tmp$did==1,]
  tmp.helix=tmp.helix[-which(tmp.helix$fid%in%c.fid.helix),]
  tmp.edw=tmp[tmp$did==2,]
  tmp.edw=tmp.edw[-which(tmp.edw$fid%in%c.fid.edw),]
  tmp.lumedx=tmp[tmp$did==3,]
  tmp.lumedx=tmp.lumedx[-which(tmp.lumedx$fid%in%c.fid.lumedx),]
  ehr.train.ts.v2[[i]]=rbind(tmp.helix,tmp.edw,tmp.lumedx)
}
ehr.test.ts.v2=ehr.test.ts
for(i in 1:5){
  tmp=ehr.test.ts[[i]]
  tmp.helix=tmp[tmp$did==1,]
  tmp.helix=tmp.helix[-which(tmp.helix$fid%in%c.fid.helix),]
  tmp.edw=tmp[tmp$did==2,]
  tmp.edw=tmp.edw[-which(tmp.edw$fid%in%c.fid.edw),]
  tmp.lumedx=tmp[tmp$did==3,]
  tmp.lumedx=tmp.lumedx[-which(tmp.lumedx$fid%in%c.fid.lumedx),]
  ehr.test.ts.v2[[i]]=rbind(tmp.helix,tmp.edw,tmp.lumedx)
}


# ehr.test.v2=ehr.test
pred.bp.xgboost.full.v2=vector("list",5)
for(i in 1:5){
  tmp=ehr.test.ts[[i]]
  tmpred=pred.bp.xgboost.full[[i]][[1]]
  tmp.helix=tmp[tmp$did==1,]
  pred.helix=tmpred[tmp$did==1]
  pred.helix=pred.helix[-which(tmp.helix$fid%in%c.fid.helix)]
  tmp.helix=tmp.helix[-which(tmp.helix$fid%in%c.fid.helix),]
  tmp.edw=tmp[tmp$did==2,]
  pred.edw=tmpred[tmp$did==2]
  pred.edw=pred.edw[-which(tmp.edw$fid%in%c.fid.edw)]
  tmp.edw=tmp.edw[-which(tmp.edw$fid%in%c.fid.edw),]
  tmp.lumedx=tmp[tmp$did==3,]
  pred.lumedx=tmpred[tmp$did==3]
  pred.lumedx=pred.lumedx[-which(tmp.lumedx$fid%in%c.fid.lumedx)]
  tmp.lumedx=tmp.lumedx[-which(tmp.lumedx$fid%in%c.fid.lumedx),]
  pred.bp.xgboost.full.v2[[i]]=c(pred.helix,pred.edw,pred.lumedx)
}


prediction.bp.ts.xgboost.full.v2=prediction.bp.ts.xgboost.full
for(i in 1:5){
  tmp=ehr.test[[i]]
  tmpred=prediction.bp.ts.xgboost.full[[i]]
  tmp.helix=tmp[tmp$did==1,]
  pred.helix=tmpred[tmp$did==1]
  pred.helix=pred.helix[-which(tmp.helix$fid%in%c.fid.helix)]
  tmp.helix=tmp.helix[-which(tmp.helix$fid%in%c.fid.helix),]
  tmp.edw=tmp[tmp$did==2,]
  pred.edw=tmpred[tmp$did==2]
  pred.edw=pred.edw[-which(tmp.edw$fid%in%c.fid.edw)]
  tmp.edw=tmp.edw[-which(tmp.edw$fid%in%c.fid.edw),]
  tmp.lumedx=tmp[tmp$did==3,]
  pred.lumedx=tmpred[tmp$did==3]
  pred.lumedx=pred.lumedx[-which(tmp.lumedx$fid%in%c.fid.lumedx)]
  tmp.lumedx=tmp.lumedx[-which(tmp.lumedx$fid%in%c.fid.lumedx),]
  ehr.test.v2[[i]]=rbind(tmp.helix,tmp.edw,tmp.lumedx)
  prediction.bp.ts.xgboost.full.v2[[i]]=c(pred.helix,pred.edw,pred.lumedx)
}

prediction.lp.xgboost.full.v2=prediction.lp.xgboost.full
for(i in 1:5){
  tmp=ehr.test[[i]]
  tmpred=prediction.lp.xgboost.full[[i]]
  tmp.helix=tmp[tmp$did==1,]
  pred.helix=tmpred[tmp$did==1]
  pred.helix=pred.helix[-which(tmp.helix$fid%in%c.fid.helix)]
  tmp.helix=tmp.helix[-which(tmp.helix$fid%in%c.fid.helix),]
  tmp.edw=tmp[tmp$did==2,]
  pred.edw=tmpred[tmp$did==2]
  pred.edw=pred.edw[-which(tmp.edw$fid%in%c.fid.edw)]
  tmp.edw=tmp.edw[-which(tmp.edw$fid%in%c.fid.edw),]
  tmp.lumedx=tmp[tmp$did==3,]
  pred.lumedx=tmpred[tmp$did==3]
  pred.lumedx=pred.lumedx[-which(tmp.lumedx$fid%in%c.fid.lumedx)]
  tmp.lumedx=tmp.lumedx[-which(tmp.lumedx$fid%in%c.fid.lumedx),]
  ehr.test.v2[[i]]=rbind(tmp.helix,tmp.edw,tmp.lumedx)
  prediction.lp.xgboost.full.v2[[i]]=c(pred.helix,pred.edw,pred.lumedx)
}

prediction.lp.ts.xgboost.full.v2=prediction.lp.ts.xgboost.full
for(i in 1:5){
  tmp=ehr.test.ts[[i]]
  tmpred=prediction.lp.ts.xgboost.full[[i]]
  tmp.helix=tmp[tmp$did==1,]
  pred.helix=tmpred[tmp$did==1]
  pred.helix=pred.helix[-which(tmp.helix$fid%in%c.fid.helix)]
  tmp.helix=tmp.helix[-which(tmp.helix$fid%in%c.fid.helix),]
  tmp.edw=tmp[tmp$did==2,]
  pred.edw=tmpred[tmp$did==2]
  pred.edw=pred.edw[-which(tmp.edw$fid%in%c.fid.edw)]
  tmp.edw=tmp.edw[-which(tmp.edw$fid%in%c.fid.edw),]
  tmp.lumedx=tmp[tmp$did==3,]
  pred.lumedx=tmpred[tmp$did==3]
  pred.lumedx=pred.lumedx[-which(tmp.lumedx$fid%in%c.fid.lumedx)]
  tmp.lumedx=tmp.lumedx[-which(tmp.lumedx$fid%in%c.fid.lumedx),]
  # ehr.test.v2[[i]]=rbind(tmp.helix,tmp.edw,tmp.lumedx)
  prediction.lp.ts.xgboost.full.v2[[i]]=c(pred.helix,pred.edw,pred.lumedx)
}


auc=rep(NA,5)
brier=auc
slope=auc
intert=auc
rel=auc
predrng=auc
for(i in 1:5){
  pred.old=ehr.test.v2[[i]]$pred.calib
  tmp=prediction.performance(pred.old,ehr.test.v2[[i]]$AKI,10)
  auc[i]=tmp[[1]]
  brier[i]=tmp[[4]]
  slope[i]=tmp[[3]]
  intert[i]=tmp[[2]]
  rel[i]=tmp[[5]]
  predrng[i]=tmp[[8]]
}

pred=prediction.bp.xgboost
auc=rep(NA,5)
rmse=auc
cors=auc
brier=auc
slope=auc
intert=auc
rel=auc
predrng=auc
for(i in 1:5){
  pred.old=ehr.test.v2[[i]]$pred.calib
  pred.new=pred[[i]]
  rmse[i]=sqrt(mean((pred.old-pred.new)^2))
  cors[i]=cor(pred.old,pred.new)
  tmp=prediction.performance(pred.new,ehr.test.v2[[i]]$AKI,10)
  auc[i]=tmp[[1]]
  brier[i]=tmp[[4]]
  slope[i]=tmp[[3]]
  intert[i]=tmp[[2]]
  rel[i]=tmp[[5]]
  predrng[i]=tmp[[8]]
}


