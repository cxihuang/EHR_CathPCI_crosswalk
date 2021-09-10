library(readxl)
library(onehot)
library(lubridate)
library(readr)
folder='/home/ch53/EHR_PCI_AKI/Data_raw/'
#################################
# registry data: Heli, EDW and Lumedx
#################################
helix.data=as.data.frame(read_excel(paste(folder,'registry/Task128502_CathPCI_Helix_12_02_19.xlsx',sep=''),sheet=1))
edw.data=as.data.frame(read_excel(paste(folder,'registry/Task128502_CathPCI_EDW_12_02_19.xlsx',sep=''),sheet=1)) 
lumedx.data.1=as.data.frame(read_excel(paste(folder,'registry/Task128502_CathPCI_LumeDx_12_12_19v3.xlsx',sep=""),sheet=1))
lumedx.data.2=as.data.frame(read_excel(paste(folder,'registry/Task128502_CathPCI_LumeDx_11_03_20v4.xlsx',sep=""),sheet=1))
lumedx.data.3=as.data.frame(read_excel(paste(folder,'registry/Task128502_CathPCI_LumeDxData_Ritu_11_05_20v5.xlsx',sep=""),sheet=1))
lumedx.data=merge(lumedx.data.1,lumedx.data.2[,c("SS_Event_Cath_ID","Case_Start")],by=c("SS_Event_Cath_ID"))
lumedx.data=merge(lumedx.data,lumedx.data.3[,c("SS_Event_Cath_ID","LastName","FirstName")],by=c("SS_Event_Cath_ID"))
# MRN
helix.data$MRNnum=as.numeric(sapply(helix.data$MRN,function(x){gsub("[^0-9]","",x)}))
edw.data$MRNnum=as.numeric(sapply(edw.data$MRN,function(x){gsub("[^0-9]","",x)}))
lumedx.data$MRNnum=as.numeric(sapply(lumedx.data$MRN,function(x){gsub("[^0-9]","",x)}))
# DOB
helix.data$DOB=as.Date(helix.data$DOB,format = "%m/%d/%Y")
edw.data$DOB=as.Date(edw.data$DOB)
lumedx.data$DOB=as.Date(lumedx.data$Date_of_Birth)
# procedure date and time
helix.data$ProcedureDate=date(helix.data$ProcedureDate)
helix.data$ProcedureInst=strptime(paste(helix.data$ProcedureDate,helix.data$ProcedureTime),format="%Y-%m-%d %H:%M:%S",tz="UTC")
helix.data$ArrivalDate=as.Date(helix.data$ArrivalDate)
helix.data$DischargeDate=as.Date(helix.data$DischargeDate)
edw.data$ProcedureDate=date(edw.data$ProcedureDate)
edw.data$ProcedureInst=strptime(paste(edw.data$ProcedureDate,edw.data$ProcedureTime),format="%Y-%m-%d %H:%M:%S",tz="UTC")
edw.data$ArrivalDate=as.Date(edw.data$ArrivalDate)
edw.data$DischargeDate=as.Date(edw.data$DischargeDate)
lumedx.data$ProcedureDate=date(lumedx.data$Date_of_Cath)
lumedx.data$ProcedureTime=format(as.POSIXct(lumedx.data$Case_Start,tz="UTC"),"%H:%M:%S")
lumedx.data$ProcedureInst=strptime(paste(lumedx.data$ProcedureDate,lumedx.data$ProcedureTime),format="%Y-%m-%d %H:%M:%S",tz="UTC")
lumedx.data$ArrivalDate=as.Date(lumedx.data$EpisodeArrivalDate)
lumedx.data$DischargeDate=as.Date(lumedx.data$Discharge_Date)
lumedx.data$ArrivalTime=format(as.POSIXct(lumedx.data$EpisodeArrivalTime,tz="UTC",format="%Y-%m-%d %H:%M:%S"),"%H:%M:%S")

# remove same-day procedures
c=which(helix.data$ArrivalDate==helix.data$DischargeDate)
helix.data=helix.data[-c,]
c=which(edw.data$ArrivalDate==edw.data$DischargeDate)
edw.data=edw.data[-c,]
c=which(lumedx.data$ArrivalDate==lumedx.data$DischargeDate)
lumedx.data=lumedx.data[-c,]

# remove subsequent procedures of single hospitalization
helix.data=helix.data[with(helix.data,order(MRNnum,ArrivalDate,ArrivalTime,ProcedureDate,ProcedureTime)),]
dup=duplicated(helix.data[,c("MRNnum","ArrivalDate","ArrivalTime")])
helix.data=helix.data[!dup,]
edw.data=edw.data[with(edw.data,order(DOB,ArrivalDate,ArrivalTime,ProcedureDate,ProcedureTime)),]
dup=duplicated(edw.data[,c("DOB","ArrivalDate","ArrivalTime")])
edw.data=edw.data[!dup,]
lumedx.data=lumedx.data[with(lumedx.data,order(MRNnum,ArrivalDate,ArrivalTime,ProcedureDate,ProcedureTime)),]
dup=duplicated(lumedx.data[,c("MRNnum","ArrivalDate","ArrivalTime")])
lumedx.data=lumedx.data[!dup,]

# remove missing outcome
data=helix.data # missing coded as NA
change.creat=round(as.numeric(data$PostProcCreat)-as.numeric(data$PreProcCreat),digits=1)
AKI=(change.creat>=0.3)|(as.numeric(data$PostProcCreat)>=1.5*as.numeric(data$PreProcCreat))|(data$PostDialysis=="Yes")
helix.data$AKI=AKI
data=edw.data # missing coded as NULL
change.creat=round(as.numeric(data$PostProcCreat)-as.numeric(data$PreProcCreat),digits=1)
AKI=(change.creat>=0.3)|(as.numeric(data$PostProcCreat)>=1.5*as.numeric(data$PreProcCreat))|(data$PostDialysis=="1")
edw.data$AKI=AKI
data=lumedx.data # missing coded as NULL
change.creat=round(as.numeric(data$LabCreatininePost)-as.numeric(data$LabCreatininePre),digits=1)
AKI=(change.creat>=0.3)|(as.numeric(data$LabCreatininePost)>=1.5*as.numeric(data$LabCreatininePre))|(data$CompNewDialysis=="Yes")
lumedx.data$AKI=AKI
helix.data=helix.data[!is.na(helix.data$AKI),]
edw.data=edw.data[!is.na(edw.data$AKI),]
lumedx.data=lumedx.data[!is.na(lumedx.data$AKI),]

# remove current dialysis
c=which(helix.data$CurrentDialysis=="Yes")
helix.data=helix.data[-c,]
c=which(edw.data$CurrentDialysis=="1")
edw.data=edw.data[-c,]
c=which(lumedx.data$ACCHxKidneyFailure=="Yes")
lumedx.data=lumedx.data[-c,]
#################################
# EHR data
#################################
helix.demo=as.data.frame(read_excel(paste(folder,'epic_helix/128502-CathPCIlist_Epicdata.xlsx',sep=''),sheet=1))
helix.PCIenct=as.data.frame(read_excel(paste(folder,'epic_helix/128502-CathPCIlist_Epicdata.xlsx',sep=''),sheet=2))
helix.diag=as.data.frame(read_delim(paste(folder,'epic_helix/128502_Helix_pat_all_diagnosis_list.txt',sep=''),delim = "|"))
helix.diag=helix.diag[-c(1093669),]
helix.proc=as.data.frame(read_delim(paste(folder,'epic_helix/128502_Helix_pat_all_proc_list.txt',sep=''),delim = "|"))
helix.proc=helix.proc[-c(70148),]
helix.meds=as.data.frame(read_delim(paste(folder,'epic_helix/128502_Helix_pat_all_medication_list.txt',sep=''),delim = "|"))
helix.meds=helix.meds[-c(879928),]
helix.lab=as.data.frame(read_delim(paste(folder,'epic_helix/128502_Helix_pat_labs.txt',sep=''),delim = "|"))
helix.lab=helix.lab[-c(422404),]

edw.demo=as.data.frame(read_excel(paste(folder,'epic_EDW/128502-CathPCIlist_EDW_Epicdata.xlsx',sep=''),sheet=1))
edw.PCIenct=as.data.frame(read_excel(paste(folder,'epic_EDW/128502-CathPCIlist_EDW_Epicdata.xlsx',sep=''),sheet=2))
edw.diag=as.data.frame(read_delim(paste(folder,'epic_EDW/128502_EDW_pat_all_diagnosis_list.txt',sep=''),delim = "|"))
edw.diag=edw.diag[-c(1169035),]
edw.proc=as.data.frame(read_delim(paste(folder,'epic_EDW/128502_EDW_pat_all_proc_list.txt',sep=''),delim = "|"))
edw.proc=edw.proc[-c(79008),]
edw.meds=as.data.frame(read_delim(paste(folder,'epic_EDW/128502_EDW_pat_all_medication_list.txt',sep=''),delim = "|"))
edw.meds=edw.meds[-c(971502),]
edw.lab=as.data.frame(read_delim(paste(folder,'epic_EDW/128502_EDW_pat_labs.txt',sep=''),delim = "|"))
edw.lab=edw.lab[-c(389683),]

lumedx.demo=as.data.frame(read_excel(paste(folder,'epic_LumeDx/128502-CathPCIlist_Lumedx_Epicdata.xlsx',sep=''),sheet=1))
lumedx.PCIenct=as.data.frame(read_excel(paste(folder,'epic_LumeDx/128502-CathPCIlist_Lumedx_Epicdata.xlsx',sep=''),sheet=2))
lumedx.diag=as.data.frame(read_delim(paste(folder,'epic_LumeDx/128502_Lumedx_pat_all_diagnosis_list.txt',sep=''),delim = "|"))
lumedx.diag=lumedx.diag[-c(862793),]
lumedx.proc=as.data.frame(read_delim(paste(folder,'epic_LumeDx/128502_Lumedx_pat_all_proc_list.txt',sep=''),delim = "|"))
lumedx.proc=lumedx.proc[-c(72124),]
lumedx.meds=as.data.frame(read_delim(paste(folder,'epic_LumeDx/128502_Lumedx_pat_all_medication_list.txt',sep=''),delim = "|"))
lumedx.meds=lumedx.meds[-c(916404),]
lumedx.lab=as.data.frame(read_delim(paste(folder,'epic_LumeDx/128502_Lumedx_pat_labs.txt',sep=''),delim = "|"))
lumedx.lab=lumedx.lab[-c(512526),]

#################################
# Mapping: registry vs EHR
#################################
# map Helix registry and EHR procedures: by MRN and procedure date
# files: helix.PCIenct (EHR) and helix.data (registry)
# merge demographic and pci encounter data
helix.PCIdemo=merge(helix.PCIenct,helix.demo,by=c("PAT_ID","PAT_MRN_ID"))
dup=duplicated(helix.PCIdemo)
table(dup)
helix.PCIdemo$MRNnum=as.numeric(sapply(helix.PCIdemo$PAT_MRN_ID,function(x){gsub("[^0-9]","",x)}))
helix.PCIdemo$ARRIVALDATE=as.Date(helix.PCIdemo$ARRIVALDATE)
helix.PCIdemo$PROCEDUREDATE=as.Date(helix.PCIdemo$PROCEDUREDATE)
helix.PCIdemo=helix.PCIdemo[with(helix.PCIdemo,order(MRNnum, ARRIVALDATE,PROCEDUREDATE)),]
helix.data$fid=1:dim(helix.data)[1]
helix.PCIdemo$fid=rep(NA,dim(helix.PCIdemo)[1])
helix.PCIdemo$PROCEDUREINST=helix.PCIdemo$HOSP_ADMSN_TIME
for (i in 1:length(helix.PCIdemo$fid)){
  c=which((helix.data$MRNnum==helix.PCIdemo$MRNnum[i])&
            (helix.data$ArrivalDate==helix.PCIdemo$ARRIVALDATE[i])&
            (helix.data$ProcedureDate==helix.PCIdemo$PROCEDUREDATE[i])&
            (helix.data$DischargeDate==date(helix.PCIdemo$HOSP_DISCHRG_TIME[i]))
  )
  if (length(c)>0){
    if(length(c)>1){break}
    helix.PCIdemo$fid[i]=helix.data$fid[c]
    helix.PCIdemo$PROCEDUREINST[i]=helix.data$ProcedureInst[c]
  }
}
# find duplicated fids
helix.PCIdemo=helix.PCIdemo[with(helix.PCIdemo,order(MRNnum,ARRIVALDATE,HOSP_ADMSN_TIME,PROCEDUREDATE,HOSP_DISCHRG_TIME)),]
dup=duplicated(helix.PCIdemo$fid,incomparables = c(NA))
c=which(dup==T)
# helix.PCIdemo$fid[c-1]=NA
helix.PCIdemo=helix.PCIdemo[!is.na(helix.PCIdemo$fid),]

# map edw registry and EHR data: by DOB and procedure date
# files: edw.data (registry) and edw.PCIenct
edw.PCIdemo=merge(edw.PCIenct,edw.demo,by=c("PAT_ID","PAT_MRN_ID"))
dup=duplicated(edw.PCIdemo)
table(dup)
edw.PCIdemo$MRNnum=as.numeric(sapply(edw.PCIdemo$PAT_MRN_ID,function(x){gsub("[^0-9]","",x)}))
edw.PCIdemo$ARRIVALDATE=as.Date(edw.PCIdemo$ARRIVALDATE)
edw.PCIdemo$PROCEDUREDATE=as.Date(edw.PCIdemo$PROCEDUREDATE)
edw.PCIdemo$BIRTH_DATE=as.Date(edw.PCIdemo$BIRTH_DATE)
edw.PCIdemo=edw.PCIdemo[with(edw.PCIdemo,order(BIRTH_DATE, ARRIVALDATE,PROCEDUREDATE)),]
edw.data$fid=1:dim(edw.data)[1]
edw.PCIdemo$fid=rep(NA,dim(edw.PCIdemo)[1])
edw.PCIdemo$PROCEDUREINST=edw.PCIdemo$HOSP_ADMSN_TIME
for (i in 1:length(edw.PCIdemo$fid)){
  c=which((edw.data$DOB==edw.PCIdemo$BIRTH_DATE[i])&
            (edw.data$ArrivalDate==edw.PCIdemo$ARRIVALDATE[i])&
            (edw.data$ProcedureDate==edw.PCIdemo$PROCEDUREDATE[i])&
            (edw.data$DischargeDate==date(edw.PCIdemo$HOSP_DISCHRG_TIME[i]))
  )
  if (length(c)>0){
    if(length(c)>1){break}
    edw.PCIdemo$fid[i]=edw.data$fid[c]
    edw.PCIdemo$PROCEDUREINST[i]=edw.data$ProcedureInst[c]
  }
}
# one patient with DOB error: use MRN for matching
# c=(which(is.na(edw.PCIdemo$fid)))
# cc=which(edw.data$MRNnum==edw.PCIdemo$MRNnum[c])
# edw.PCIdemo$fid[c]=edw.data$fid[cc]
# edw.PCIdemo$PROCEDUREINST[c]=edw.data$ProcedureInst[cc]
# find duplicated fids
edw.PCIdemo=edw.PCIdemo[with(edw.PCIdemo,order(MRNnum,ARRIVALDATE,HOSP_ADMSN_TIME,PROCEDUREDATE,HOSP_DISCHRG_TIME)),]
dup=duplicated(edw.PCIdemo$fid,incomparables = c(NA))
c=which(dup==T)
# edw.PCIdemo$fid[c-1]=NA
edw.PCIdemo=edw.PCIdemo[!is.na(edw.PCIdemo$fid),]

# map lumedx registry and EHR data: by MRN
lumedx.PCIdemo=merge(lumedx.PCIenct,lumedx.demo,by=c("PAT_ID","PAT_MRN_ID"))
dup=duplicated(lumedx.PCIdemo)
table(dup)
lumedx.PCIdemo$MRNnum=as.numeric(sapply(lumedx.PCIdemo$PAT_MRN_ID,function(x){gsub("[^0-9]","",x)}))
lumedx.PCIdemo$ARRIVALDATE=as.Date(lumedx.PCIdemo$ARRIVALDATE)
lumedx.PCIdemo$PROCEDUREDATE=as.Date(lumedx.PCIdemo$PROCEDUREDATE)
lumedx.PCIdemo=lumedx.PCIdemo[with(lumedx.PCIdemo,order(MRNnum, ARRIVALDATE,PROCEDUREDATE)),]
lumedx.data$fid=1:dim(lumedx.data)[1]
lumedx.PCIdemo$fid=rep(NA,dim(lumedx.PCIdemo)[1])
lumedx.PCIdemo$PROCEDUREINST=lumedx.PCIdemo$HOSP_ADMSN_TIME
dupcs=vector()
for (i in 1:length(lumedx.PCIdemo$fid)){
  c=which((lumedx.data$MRNnum==lumedx.PCIdemo$MRNnum[i])&
            (lumedx.data$ArrivalDate==lumedx.PCIdemo$ARRIVALDATE[i])&
            (lumedx.data$ProcedureDate==lumedx.PCIdemo$PROCEDUREDATE[i])&
            (lumedx.data$DischargeDate==date(lumedx.PCIdemo$HOSP_DISCHRG_TIME[i]))
  )
  if (length(c)>0){
    if(length(c)>1){dupcs=c(dupcs,i);break}
    lumedx.PCIdemo$fid[i]=lumedx.data$fid[c]
    lumedx.PCIdemo$PROCEDUREINST[i]=lumedx.data$ProcedureInst[c]
  }
}
lumedx.PCIdemo=lumedx.PCIdemo[with(lumedx.PCIdemo,order(MRNnum,ARRIVALDATE,HOSP_ADMSN_TIME,PROCEDUREDATE,HOSP_DISCHRG_TIME)),]

dup=duplicated(lumedx.PCIdemo$fid,incomparables = c(NA))
c=which(dup==T)
# lumedx.PCIdemo$fid[c-1]=NA
lumedx.PCIdemo=lumedx.PCIdemo[!is.na(lumedx.PCIdemo$fid),]

# trimming rgistry data
fid=intersect(helix.data$fid,helix.PCIdemo$fid)
helix.data=helix.data[helix.data$fid%in%fid,]
helix.PCIdemo=helix.PCIdemo[helix.PCIdemo$fid%in%fid,]

fid=intersect(edw.data$fid,edw.PCIdemo$fid)
edw.data=edw.data[edw.data$fid%in%fid,]
edw.PCIdemo=edw.PCIdemo[edw.PCIdemo$fid%in%fid,]

fid=intersect(lumedx.data$fid,lumedx.PCIdemo$fid)
lumedx.data=lumedx.data[lumedx.data$fid%in%fid,]
lumedx.PCIdemo=lumedx.PCIdemo[lumedx.PCIdemo$fid%in%fid,]

save(helix.data,edw.data,lumedx.data,helix.PCIdemo,edw.PCIdemo,lumedx.PCIdemo,
     file = "mapped_registry_EHR_04202021.RData")




