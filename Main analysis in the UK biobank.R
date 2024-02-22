rm(list=ls())
library(tidyverse)
library(Rmisc)
library(sjmisc)
library(lubridate)
library(survival)
library(survminer)
library(haven)
library(rms)
library(ggplot2)
library(openxlsx)
library(survMisc)
library(CoxR2)
#cancers
#----------------------------------------------------------------------------------------------------------------------
folder_path <- "D:/R/UKB_outcome/mutiple outcome/cancer/result/"
file_names<-list.files(path = folder_path,pattern = "\\.csv$",full.names = TRUE)
file_names
data_list <- lapply(file_names, read.csv)
data_list
combined_cancers <- data_list %>%
  reduce(full_join, by = "eid")
combined_cancers
colnames(combined_cancers)

folder_path <- "D:/R/UKB_outcome/mutiple outcome/Circulation system disease/result/"
file_names<-list.files(path = folder_path,pattern = "\\.csv$",full.names = TRUE)
file_names
data_list <- lapply(file_names, read.csv)
data_list
combined_circulation <- data_list %>%
  reduce(full_join, by = "eid")
combined_circulation
colnames(combined_circulation)

folder_path <- "D:/R/UKB_outcome/mutiple outcome/Digestive system disease/result/"
file_names<-list.files(path = folder_path,pattern = "\\.csv$",full.names = TRUE)
file_names
data_list <- lapply(file_names, read.csv)
data_list
combined_digestive <- data_list %>%
  reduce(full_join, by = "eid")
combined_digestive
colnames(combined_digestive)

folder_path <- "D:/R/UKB_outcome/mutiple outcome/Endocrine, metabolic disease/result/"
file_names<-list.files(path = folder_path,pattern = "\\.csv$",full.names = TRUE)
file_names
data_list <- lapply(file_names, read.csv)
data_list
combined_metabolic <- data_list %>%
  reduce(full_join, by = "eid")
combined_metabolic
colnames(combined_metabolic)

folder_path <- "D:/R/UKB_outcome/mutiple outcome/Genitourinary system/result/"
file_names<-list.files(path = folder_path,pattern = "\\.csv$",full.names = TRUE)
file_names
data_list <- lapply(file_names, read.csv)
data_list
combined_genitourinary <- data_list %>%
  reduce(full_join, by = "eid")
combined_genitourinary
colnames(combined_genitourinary)


folder_path <- "D:/R/UKB_outcome/mutiple outcome/Hematopoietic system/result/"
file_names<-list.files(path = folder_path,pattern = "\\.csv$",full.names = TRUE)
file_names
data_list <- lapply(file_names, read.csv)
data_list
combined_Hematopoietic <- data_list %>%
  reduce(full_join, by = "eid")
combined_Hematopoietic
colnames(combined_Hematopoietic)

folder_path <- "D:/R/UKB_outcome/mutiple outcome/Musculoskeletal system and connective tissue/result/"
file_names<-list.files(path = folder_path,pattern = "\\.csv$",full.names = TRUE)
file_names
data_list <- lapply(file_names, read.csv)
data_list
combined_Musculoskeletal<- data_list %>%
  reduce(full_join, by = "eid")
combined_Musculoskeletal
colnames(combined_Musculoskeletal)


folder_path <- "D:/R/UKB_outcome/mutiple outcome/Nervous system disease/result/"
file_names<-list.files(path = folder_path,pattern = "\\.csv$",full.names = TRUE)
file_names
data_list <- lapply(file_names, read.csv)
data_list
combined_Nervous<- data_list %>%
  reduce(full_join, by = "eid")
combined_Nervous
colnames(combined_Nervous)

folder_path <- "D:/R/UKB_outcome/mutiple outcome/Respiratory system disease/result/"
file_names<-list.files(path = folder_path,pattern = "\\.csv$",full.names = TRUE)
file_names
data_list <- lapply(file_names, read.csv)
data_list
combined_Respiratory<- data_list %>%
  reduce(full_join, by = "eid")
combined_Respiratory
colnames(combined_Respiratory)

folder_path <- "D:/R/UKB_outcome/mutiple outcome/Mental and behavioural disorder/result/"
file_names<-list.files(path = folder_path,pattern = "\\.csv$",full.names = TRUE)
file_names
data_list <- lapply(file_names, read.csv)
data_list
combined_mental<- data_list %>%
  reduce(full_join, by = "eid")
combined_mental
colnames(combined_mental)


folder_path <- "D:/R/UKB_outcome/mutiple outcome/Eye/result/"
file_names<-list.files(path = folder_path,pattern = "\\.csv$",full.names = TRUE)
file_names
data_list <- lapply(file_names, read.csv)
data_list
combined_Eye<- data_list %>%
  reduce(full_join, by = "eid")
combined_Eye
colnames(combined_Eye)


folder_path <- "D:/R/UKB_outcome/mutiple outcome/Ear/result/"
file_names<-list.files(path = folder_path,pattern = "\\.csv$",full.names = TRUE)
file_names
data_list <- lapply(file_names, read.csv)
data_list
combined_Ear<- data_list %>%
  reduce(full_join, by = "eid")
combined_Ear
colnames(combined_Ear)

folder_path <- "D:/R/UKB_outcome/mutiple outcome/Skin/result/"
file_names<-list.files(path = folder_path,pattern = "\\.csv$",full.names = TRUE)
file_names
data_list <- lapply(file_names, read.csv)
data_list
combined_Skin<- data_list %>%
  reduce(full_join, by = "eid")
combined_Skin
colnames(combined_Skin)

folder_path <- "D:/R/UKB_outcome/mutiple outcome/Infectious/result/"
file_names<-list.files(path = folder_path,pattern = "\\.csv$",full.names = TRUE)
file_names
data_list <- lapply(file_names, read.csv)
data_list
combined_Infectious<- data_list %>%
  reduce(full_join, by = "eid")
combined_Infectious
colnames(combined_Infectious)

data1<-read.csv("D:/R/UKB_data/loneliness_muti/data.impu.done.csv")
summary(data1$age)
data1$lonely<-factor(data1$lonely)
data1$able_to_confide<-factor(data1$able_to_confide)
data1$loneliness_group<-factor(data1$loneliness_group)
data1$sex<-factor(data1$sex)
data1$Ethnicity<-factor(data1$Ethnicity)
data1$region<-factor(data1$region)
data1$Employment<-factor(data1$Employment)
data1$education_level<-factor(data1$education_level)
data1$Smoking<-factor(data1$Smoking)
data1$alcohol_intake<-factor(data1$alcohol_intake)
data1$age<-as.numeric(data1$age)
data1$MVPA_self<-as.numeric(data1$MVPA_self)
data1<-left_join(data1,combined_cancers,by="eid")
data1<-left_join(data1,combined_circulation,by="eid")
data1<-left_join(data1,combined_digestive,by="eid")
data1<-left_join(data1,combined_metabolic,by="eid")
data1<-left_join(data1,combined_genitourinary,by="eid")
data1<-left_join(data1,combined_Hematopoietic,by="eid")
data1<-left_join(data1,combined_Musculoskeletal,by="eid")
data1<-left_join(data1,combined_Nervous,by="eid")
data1<-left_join(data1,combined_Respiratory,by="eid")
data1<-left_join(data1,combined_mental,by="eid")
data1<-left_join(data1,combined_Eye,by="eid")
data1<-left_join(data1,combined_Ear,by="eid")
data1<-left_join(data1,combined_Skin,by="eid")
data1<-left_join(data1,combined_Infectious,by="eid")

disease_types<-c("cancers.HDC.ICD10","bladdercancer.HDC.ICD10","braincancer.HDC.ICD10","breastcancer.HDC.ICD10",
                 "colorectalcancer.HDC.ICD10","kidneycancer.HDC.ICD10","leukemia.HDC.ICD10",
                 "livercancer.HDC.ICD10","lungcancer.HDC.ICD10","malignantmelanoma.HDC.ICD10",
                 "multiplemyeloma.HDC.ICD10","non_Hodgkin_lymphoma.HDC.ICD10","oesophaguscancer.HDC.ICD10",
                 "oralcancer.HDC.ICD10","pancreascancer.HDC.ICD10","prostatecancer.HDC.ICD10",
                 "stomachcancer.HDC.ICD10","thyroidcancer.HDC.ICD10","uterus_accessories_cancer.HDC.ICD10",
                 "CVD.HDC.ICD10","HF.HDC.ICD10","AF.HDC.ICD10","hypertension.HDC.ICD10","IHD.HDC.ICD10","PAD.HDC.ICD10",
                 "stroke.HDC.ICD10","Digestive.HDC.ICD10","gallbladder_biliary_tract_pancreas.HDC.ICD10","Chronicliverdisease.HDC.ICD10","IBD.HDC.ICD10",
                 "metabolic_disease.HDC.ICD10","T2DM.HDC.ICD10","thyroiddisorders.HDC.ICD10","hyperthyroidism.HDC.ICD10",
                 "hypothyroidism.HDC.ICD10","Genitourinary.HDC.ICD10",
                 "CKD.HDC.ICD10","Urolithiasis.HDC.ICD10","Hematopoietic.HDC.ICD10","anaemias.HDC.ICD10","haemorrhagic.HDC.ICD10",
                 "Musculoskeletal_connectivetissue.HDC.ICD10","gout.HDC.ICD10","Rheumatoidarthritis.HDC.ICD10",
                 "SLE.HDC.ICD10","Nervous.HDC.ICD10","AD.HDC.ICD10","epilepsy.HDC.ICD10","migraine.HDC.ICD10",
                 "multiplesclerosis.HDC.ICD10","PD.HDC.ICD10","Respiratory.HDC.ICD10","Asthma.HDC.ICD10",
                 "COPD.HDC.ICD10","Mental.HDC.ICD10","anxiety.HDC.ICD10","insomnia.HDC.ICD10","OSA.HDC.ICD10",
                 "depression.HDC.ICD10","schizophrenia.HDC.ICD10",
                 "bipolar.HDC.ICD10","PTSD.HDC.ICD10","sleep_disorder.HDC.ICD10","substance_abuse.HDC.ICD10",
                 "eyes.HDC.ICD10","glaucoma.HDC.ICD10","cataract.HDC.ICD10","ears.HDC.ICD10","hear_loss.HDC.ICD10","disorders_of_vestibular_function.HDC.ICD10",
                 "atopic_dermatitis.HDC.ICD10","skins.HDC.ICD10","atopic_dermatitis.HDC.ICD10","psoriasis.HDC.ICD10",
                 "infectious.HDC.ICD10","Gastrointestinal_infections.HDC.ICD10","Influenza.HDC.ICD10","Pneumonia.HDC.ICD10",
                 "Skin_and_subcutaneous_tissue_infections.HDC.ICD10","Urinary_tract_infection.HDC.ICD10")
length(disease_types)  #80


colnames(data1)
frq(data1$loneliness_group)
model<-function(x,exposure,ti,out,dataname){
  z <- length(table(x))-1
  cox1 <- coxph(Surv(ti,out) ~x+  age + sex , data=dataname)
  cox2 <- coxph(Surv(ti,out) ~x+ age + sex+Ethnicity+region+education_level+Employment+
                  Smoking+alcohol_intake+BMI+MVPA_self, data=dataname)
  level <- c("No loneliness ","Loneliness ")
  a<-cbind("HR" =  summary(cox1)$conf.int[1:z,c(1)],
           "low" = summary(cox1)$conf.int[1:z,c(3)],
           "up" = summary(cox1)$conf.int[1:z,c(4)],
           "pvalue" =summary(cox1)$coefficients[1:z,5])
  b<-cbind("HR" =  summary(cox2)$conf.int[1:z,c(1)],
           "low" = summary(cox2)$conf.int[1:z,c(3)],
           "up" = summary(cox2)$conf.int[1:z,c(4)],
           "pvalue" =summary(cox2)$coefficients[1:z,5])
  reference <- c(1,1,1,0)
  b<-rbind(reference,b)
  a <- rbind(reference,a)
  a <- cbind(a,level, "exposure" = rep(exposure), "model" = rep("model1"))
  b <- cbind(b,level, "exposure" = rep(exposure), "model" = rep("model2"))
  d <- rbind(a,b)
  result<-as.data.frame(d)
  result[,1]<-sprintf("%0.2f", as.numeric(result[,1]))
  result[,2]<-sprintf("%0.2f", as.numeric(result[,2]))
  result[,3]<-sprintf("%0.2f", as.numeric(result[,3]))
  result[,4]<-sprintf("%0.9f", as.numeric(result[,4]))
  result$blank<-rep("")
  result <- result %>% unite(CI,`HR`,`low`, sep = "(",remove = F)  
  result <- result %>% unite(CI,`CI`,`up`, sep = "-",remove = F)  
  result <- result %>% unite(CI,`CI`,`blank`, sep = ")")
  result <- rownames_to_column(result, var = "rowname")
  return(result)}

case<-function(x,x1,model,time,outcome,data){
  group_vars1 <- syms(as.character(x))
  group_vars2 <- syms(as.character(outcome))
  group_vars3 <- syms(as.character(time)) 
  
  a<-data%>%group_by(!!!group_vars1)%>%dplyr::summarise(sum(!!!group_vars2))
  b<-data%>%group_by(!!!group_vars1)%>%dplyr::summarise(sum(!!!group_vars3)/365)
  d<-merge(a,b,by=x,all.x = T)
  d
  d$personyear<-d[,2]/d[,3]*1000
  d
  c<-table(x1)
  c
  c<-data.frame(c) 
  names(c)
  c
  
  z<-cbind( "N" =c[,c(2)],
            "case"= d[,2],
            "follow up"= d[,3],
            "person years"=d[,4],
            "model" =rep(model)
  )
  z<-data.frame(z)
  names(z)
  z[,3]<-sprintf("%0.0f", as.numeric(z[,3]))
  z[,4]<-sprintf("%0.2f", as.numeric(z[,4]))
  z1<-z%>% unite(case_p,'case',"follow.up",sep="/")
  result<-z1
  y<- length(table(x1))-1
  level <- c("No loneliness","Loneliness")
  result<-cbind(level,result)
  return(result)
}


result_list_cox <- list()
result_list_case<-list()
colnames(data1)
for (disease_type in disease_types) {

  incidental_var <- paste("Incidental", disease_type, sep = ".")
  fuduration_var <- paste("FUduration", disease_type, sep = ".")
  
  df <- filter(data1, !is.na(get(incidental_var)))

  result_cox <- model(df$loneliness_group, paste("Loneliness~", incidental_var),
                      df[[fuduration_var]], df[[incidental_var]], df)
  result_case <- case(c("loneliness_group"), df$loneliness_group,
                      paste("Loneliness~", disease_type),
                      c(fuduration_var),
                      c(incidental_var),
                      df)

  result_list_cox[[disease_type]] <- result_cox
  result_list_case[[disease_type]] <- result_case
}

Disease_cox<- do.call(rbind, result_list_cox)
Disease_cox
Disease_case<- do.call(rbind, result_list_case)
Disease_case

write.csv(Disease_cox,"D:/R/UKB_data/loneliness_muti/Cox_loneliness.csv")
write.csv(Disease_case,"D:/R/UKB_data/loneliness_muti/Case_loneliness.csv")









#LONELY
#--------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
model<-function(x,exposure,ti,out,dataname){
  z <- length(table(x))-1
  cox1 <- coxph(Surv(ti,out) ~x+  age + sex , data=dataname)
  cox2 <- coxph(Surv(ti,out) ~x+ age + sex+Ethnicity+region+education_level+Employment+
                  Smoking+alcohol_intake+BMI+MVPA_self, data=dataname)
  level <- c("No lonely ","Lonely ")
  a<-cbind("HR" =  summary(cox1)$conf.int[1:z,c(1)],
           "low" = summary(cox1)$conf.int[1:z,c(3)],
           "up" = summary(cox1)$conf.int[1:z,c(4)],
           "pvalue" =summary(cox1)$coefficients[1:z,5])
  b<-cbind("HR" =  summary(cox2)$conf.int[1:z,c(1)],
           "low" = summary(cox2)$conf.int[1:z,c(3)],
           "up" = summary(cox2)$conf.int[1:z,c(4)],
           "pvalue" =summary(cox2)$coefficients[1:z,5])
  reference <- c(1,1,1,0)
  b<-rbind(reference,b)
  a <- rbind(reference,a)
  a <- cbind(a,level, "exposure" = rep(exposure), "model" = rep("model1"))
  b <- cbind(b,level, "exposure" = rep(exposure), "model" = rep("model2"))
  d <- rbind(a,b)
  result<-as.data.frame(d)
  result[,1]<-sprintf("%0.2f", as.numeric(result[,1]))
  result[,2]<-sprintf("%0.2f", as.numeric(result[,2]))
  result[,3]<-sprintf("%0.2f", as.numeric(result[,3]))
  result[,4]<-sprintf("%0.3f", as.numeric(result[,4]))
  result$blank<-rep("")
  result <- result %>% unite(CI,`HR`,`low`, sep = "(",remove = F)  
  result <- result %>% unite(CI,`CI`,`up`, sep = "-",remove = F)  
  result <- result %>% unite(CI,`CI`,`blank`, sep = ")")
  result <- rownames_to_column(result, var = "rowname")
  return(result)}

case<-function(x,x1,model,time,outcome,data){
  group_vars1 <- syms(as.character(x))
  group_vars2 <- syms(as.character(outcome))
  group_vars3 <- syms(as.character(time)) 
  
  a<-data%>%group_by(!!!group_vars1)%>%dplyr::summarise(sum(!!!group_vars2))
  b<-data%>%group_by(!!!group_vars1)%>%dplyr::summarise(sum(!!!group_vars3)/365)
  d<-merge(a,b,by=x,all.x = T)
  d
  d$personyear<-d[,2]/d[,3]*1000
  d
  c<-table(x1)
  c
  c<-data.frame(c) 
  names(c)
  c
  
  z<-cbind( "N" =c[,c(2)],
            "case"= d[,2],
            "follow up"= d[,3],
            "person years"=d[,4],
            "model" =rep(model)
  )
  z<-data.frame(z)
  names(z)
  z[,3]<-sprintf("%0.0f", as.numeric(z[,3]))
  z[,4]<-sprintf("%0.2f", as.numeric(z[,4]))
  z1<-z%>% unite(case_p,'case',"follow.up",sep="/")
  result<-z1
  y<- length(table(x1))-1
  level <- c("No lonely ","Lonely ")
  result<-cbind(level,result)
  return(result)
}
colnames(data1)
frq(data1$lonely)
result_list_cox <- list()
result_list_case<-list()
for (disease_type in disease_types) {
  incidental_var <- paste("Incidental", disease_type, sep = ".")
  fuduration_var <- paste("FUduration", disease_type, sep = ".")

  df <- filter(data1, !is.na(get(incidental_var)))
  result_cox <- model(df$lonely, paste("lonely~", incidental_var),
                      df[[fuduration_var]], df[[incidental_var]], df)
  result_case <- case(c("lonely"), df$lonely,
                      paste("lonely~", disease_type),
                      c(fuduration_var),
                      c(incidental_var),
                      df)

  result_list_cox[[disease_type]] <- result_cox
  result_list_case[[disease_type]] <- result_case
}

Disease_cox_lonely<- do.call(rbind, result_list_cox)
Disease_cox_lonely
Disease_case_lonely<- do.call(rbind, result_list_case)
Disease_case_lonely

write.csv(Disease_cox_lonely,"D:/R/UKB_data/loneliness_muti/Cox_lonely.csv")
write.csv(Disease_case_lonely,"D:/R/UKB_data/loneliness_muti/Case_lonely.csv")







#Able to confide
#---------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
model<-function(x,exposure,ti,out,dataname){
  z <- length(table(x))-1
  cox1 <- coxph(Surv(ti,out) ~x+  age + sex , data=dataname)
  cox2 <- coxph(Surv(ti,out) ~x+ age + sex+Ethnicity+region+education_level+Employment+
                  Smoking+alcohol_intake+BMI+MVPA_self, data=dataname)
  level <- c("Confide ","Never confide ")
  a<-cbind("HR" =  summary(cox1)$conf.int[1:z,c(1)],
           "low" = summary(cox1)$conf.int[1:z,c(3)],
           "up" = summary(cox1)$conf.int[1:z,c(4)],
           "pvalue" =summary(cox1)$coefficients[1:z,5])
  b<-cbind("HR" =  summary(cox2)$conf.int[1:z,c(1)],
           "low" = summary(cox2)$conf.int[1:z,c(3)],
           "up" = summary(cox2)$conf.int[1:z,c(4)],
           "pvalue" =summary(cox2)$coefficients[1:z,5])
  reference <- c(1,1,1,0)
  b<-rbind(reference,b)
  a <- rbind(reference,a)
  a <- cbind(a,level, "exposure" = rep(exposure), "model" = rep("model1"))
  b <- cbind(b,level, "exposure" = rep(exposure), "model" = rep("model2"))
  d <- rbind(a,b)
  result<-as.data.frame(d)
  result[,1]<-sprintf("%0.2f", as.numeric(result[,1]))
  result[,2]<-sprintf("%0.2f", as.numeric(result[,2]))
  result[,3]<-sprintf("%0.2f", as.numeric(result[,3]))
  result[,4]<-sprintf("%0.3f", as.numeric(result[,4]))
  result$blank<-rep("")
  result <- result %>% unite(CI,`HR`,`low`, sep = "(",remove = F)  
  result <- result %>% unite(CI,`CI`,`up`, sep = "-",remove = F)  
  result <- result %>% unite(CI,`CI`,`blank`, sep = ")")
  result <- rownames_to_column(result, var = "rowname")
  return(result)}

case<-function(x,x1,model,time,outcome,data){
  group_vars1 <- syms(as.character(x))
  group_vars2 <- syms(as.character(outcome))
  group_vars3 <- syms(as.character(time)) 
  
  a<-data%>%group_by(!!!group_vars1)%>%dplyr::summarise(sum(!!!group_vars2))
  b<-data%>%group_by(!!!group_vars1)%>%dplyr::summarise(sum(!!!group_vars3)/365)
  d<-merge(a,b,by=x,all.x = T)
  d
  d$personyear<-d[,2]/d[,3]*1000
  d
  c<-table(x1)
  c
  c<-data.frame(c) 
  names(c)
  c
  
  z<-cbind( "N" =c[,c(2)],
            "case"= d[,2],
            "follow up"= d[,3],
            "person years"=d[,4],
            "model" =rep(model)
  )
  z<-data.frame(z)
  names(z)
  z[,3]<-sprintf("%0.0f", as.numeric(z[,3]))
  z[,4]<-sprintf("%0.2f", as.numeric(z[,4]))
  z1<-z%>% unite(case_p,'case',"follow.up",sep="/")
  result<-z1
  y<- length(table(x1))-1
  level <- c("Confide ","Never confide ")
  result<-cbind(level,result)
  return(result)
}
colnames(data1)
frq(data1$able_to_confide)
result_list_cox <- list()
result_list_case<-list()
for (disease_type in disease_types) {
  incidental_var <- paste("Incidental", disease_type, sep = ".")
  fuduration_var <- paste("FUduration", disease_type, sep = ".")
 
  df <- filter(data1, !is.na(get(incidental_var)))

  result_cox <- model(df$able_to_confide, paste("able_to_confide~", incidental_var),
                      df[[fuduration_var]], df[[incidental_var]], df)
  result_case <- case(c("able_to_confide"), df$able_to_confide,
                      paste("able_to_confide~", disease_type),
                      c(fuduration_var),
                      c(incidental_var),
                      df)

  result_list_cox[[disease_type]] <- result_cox
  result_list_case[[disease_type]] <- result_case
}

Disease_cox_able_to_confide<- do.call(rbind, result_list_cox)
Disease_cox_able_to_confide
Disease_case_able_to_confide<- do.call(rbind, result_list_case)
Disease_case_able_to_confide

write.csv(Disease_cox_able_to_confide,"D:/R/UKB_data/loneliness_muti/Cox_able_to_confide.csv")
write.csv(Disease_case_able_to_confide,"D:/R/UKB_data/loneliness_muti/Case_able_to_confide.csv")


#PAF
#loneliness
data1<-read.csv("D:/R/UKB_data/loneliness_muti/PAF_loneliness_calcu.csv")

data1$PAF<-NA
data1$PAF_low<-NA
data1$PAF_up<-NA
for (i in seq(1, 157, by = 2)) {
  A1<-data1$N[i]/(data1$N[i]+data1$N[i+1])
  A2<-data1$N[i+1]/(data1$N[i]+data1$N[i+1])
  data1$PAF[i+1]<-(1-1/(data1$HR[i]*A1+data1$HR[i+1]*A2))*100
  data1$PAF_low[i+1]<-(1-1/(data1$low[i]*A1+data1$low[i+1]*A2))*100
  data1$PAF_up[i+1]<-(1-1/(data1$up[i]*A1+data1$up[i+1]*A2))*100
}
colnames(data1)
data1[,8]<-sprintf("%0.2f", as.numeric(data1[,8]))
data1[,9]<-sprintf("%0.2f", as.numeric(data1[,9]))
data1[,10]<-sprintf("%0.2f", as.numeric(data1[,10]))
data1$blank<-rep("")
data1<- data1 %>% unite(PAF_CI,`PAF`,`PAF_low`, sep = "(",remove = F)  
data1 <- data1 %>% unite(PAF_CI,`PAF_CI`,`PAF_up`, sep = "-",remove = F)  
data1 <- data1 %>% unite(PAF_CI,`PAF_CI`,`blank`, sep = ")")
write.csv(data1,"D:/R/UKB_data/loneliness_muti/PAF_loneliness(UKB).csv")

#lonely
data1<-read.csv("D:/R/UKB_data/loneliness_muti/PAF_lonely_calcu.csv")
data1$PAF<-NA
data1$PAF_low<-NA
data1$PAF_up<-NA
for (i in seq(1, 157, by = 2)) {
  A1<-data1$N[i]/(data1$N[i]+data1$N[i+1])
  A2<-data1$N[i+1]/(data1$N[i]+data1$N[i+1])
  data1$PAF[i+1]<-(1-1/(data1$HR[i]*A1+data1$HR[i+1]*A2))*100
  data1$PAF_low[i+1]<-(1-1/(data1$low[i]*A1+data1$low[i+1]*A2))*100
  data1$PAF_up[i+1]<-(1-1/(data1$up[i]*A1+data1$up[i+1]*A2))*100
}
colnames(data1)
data1[,8]<-sprintf("%0.2f", as.numeric(data1[,8]))
data1[,9]<-sprintf("%0.2f", as.numeric(data1[,9]))
data1[,10]<-sprintf("%0.2f", as.numeric(data1[,10]))
data1$blank<-rep("")
data1<- data1 %>% unite(PAF_CI,`PAF`,`PAF_low`, sep = "(",remove = F)  
data1 <- data1 %>% unite(PAF_CI,`PAF_CI`,`PAF_up`, sep = "-",remove = F)  
data1 <- data1 %>% unite(PAF_CI,`PAF_CI`,`blank`, sep = ")")
write.csv(data1,"D:/R/UKB_data/loneliness_muti/PAF_lonely(UKB).csv")


#confide
data1<-read.csv("D:/R/UKB_data/loneliness_muti/PAF_confide_calcu.csv")
data1$PAF<-NA
data1$PAF_low<-NA
data1$PAF_up<-NA
for (i in seq(1, 157, by = 2)) {
  A1<-data1$N[i]/(data1$N[i]+data1$N[i+1])
  A2<-data1$N[i+1]/(data1$N[i]+data1$N[i+1])
  data1$PAF[i+1]<-(1-1/(data1$HR[i]*A1+data1$HR[i+1]*A2))*100
  data1$PAF_low[i+1]<-(1-1/(data1$low[i]*A1+data1$low[i+1]*A2))*100
  data1$PAF_up[i+1]<-(1-1/(data1$up[i]*A1+data1$up[i+1]*A2))*100
}
colnames(data1)
data1[,8]<-sprintf("%0.2f", as.numeric(data1[,8]))
data1[,9]<-sprintf("%0.2f", as.numeric(data1[,9]))
data1[,10]<-sprintf("%0.2f", as.numeric(data1[,10]))
data1$blank<-rep("")
data1<- data1 %>% unite(PAF_CI,`PAF`,`PAF_low`, sep = "(",remove = F)  
data1 <- data1 %>% unite(PAF_CI,`PAF_CI`,`PAF_up`, sep = "-",remove = F)  
data1 <- data1 %>% unite(PAF_CI,`PAF_CI`,`blank`, sep = ")")
write.csv(data1,"D:/R/UKB_data/loneliness_muti/PAF_confide(UKB).csv")


#HRS PAF 
data1<-read.csv("D:/R/UKB_data/loneliness_muti/HRS/PAF_calcu(HRS).csv")
data1$PAF<-NA
data1$PAF_low<-NA
data1$PAF_up<-NA
for (i in seq(1, 25, by = 2)) {
  A1<-data1$N[i]/(data1$N[i]+data1$N[i+1])
  A2<-data1$N[i+1]/(data1$N[i]+data1$N[i+1])
  data1$PAF[i+1]<-(1-1/(data1$HR[i]*A1+data1$HR[i+1]*A2))*100
  data1$PAF_low[i+1]<-(1-1/(data1$low[i]*A1+data1$low[i+1]*A2))*100
  data1$PAF_up[i+1]<-(1-1/(data1$up[i]*A1+data1$up[i+1]*A2))*100
}
colnames(data1)
data1[,8]<-sprintf("%0.2f", as.numeric(data1[,8]))
data1[,9]<-sprintf("%0.2f", as.numeric(data1[,9]))
data1[,10]<-sprintf("%0.2f", as.numeric(data1[,10]))
data1$blank<-rep("")
data1<- data1 %>% unite(PAF_CI,`PAF`,`PAF_low`, sep = "(",remove = F)  
data1 <- data1 %>% unite(PAF_CI,`PAF_CI`,`PAF_up`, sep = "-",remove = F)  
data1 <- data1 %>% unite(PAF_CI,`PAF_CI`,`blank`, sep = ")")
write.csv(data1,"D:/R/UKB_data/loneliness_muti/HRS/PAF(HRS).csv")


#CHARLS PAF


data1<-read.csv("D:/R/UKB_data/loneliness_muti/CHARLS/PAF_calcu(CHARLS).csv")
data1$PAF<-NA
data1$PAF_low<-NA
data1$PAF_up<-NA
for (i in seq(1, 25, by = 2)) {
  A1<-data1$N[i]/(data1$N[i]+data1$N[i+1])
  A2<-data1$N[i+1]/(data1$N[i]+data1$N[i+1])
  data1$PAF[i+1]<-(1-1/(data1$HR[i]*A1+data1$HR[i+1]*A2))*100
  data1$PAF_low[i+1]<-(1-1/(data1$low[i]*A1+data1$low[i+1]*A2))*100
  data1$PAF_up[i+1]<-(1-1/(data1$up[i]*A1+data1$up[i+1]*A2))*100
}
colnames(data1)
data1[,8]<-sprintf("%0.2f", as.numeric(data1[,8]))
data1[,9]<-sprintf("%0.2f", as.numeric(data1[,9]))
data1[,10]<-sprintf("%0.2f", as.numeric(data1[,10]))
data1$blank<-rep("")
data1<- data1 %>% unite(PAF_CI,`PAF`,`PAF_low`, sep = "(",remove = F)  
data1 <- data1 %>% unite(PAF_CI,`PAF_CI`,`PAF_up`, sep = "-",remove = F)  
data1 <- data1 %>% unite(PAF_CI,`PAF_CI`,`blank`, sep = ")")
write.csv(data1,"D:/R/UKB_data/loneliness_muti/CHARLS/PAF(CHARLS).csv")



loneliness_cox<-read.csv("D:/R/UKB_data/loneliness_muti/Cox_loneliness_bonferroni.csv")
loneliness_p<-loneliness_cox$pvalue
loneliness_p_bomferroni<-p.adjust(loneliness_cox$pvalue, 
                                  method ="bonferroni")  
loneliness_cox$pvalue
loneliness_p_bomferroni
loneliness_cox<-cbind(loneliness_cox,loneliness_p_bomferroni)
write.csv(loneliness_cox,"D:/R/UKB_data/loneliness_muti/Cox_loneliness_bonferroni.csv")

loneliness_cox<-read.csv("D:/R/UKB_data/loneliness_muti/CHARLS/CHARLS_FDR.csv")
loneliness_p<-loneliness_cox$pvalue
?p.adjust
loneliness_p_FDR<-p.adjust(loneliness_cox$pvalue,  # P
                           method ="bonferroni")                 
loneliness_cox<-cbind(loneliness_cox,loneliness_p_FDR)
write.csv(loneliness_cox,"D:/R/UKB_data/loneliness_muti/CHARLS/CHARLS_bonferroni.csv")

loneliness_cox<-read.csv("D:/R/UKB_data/loneliness_muti/HRS/HRS_FDR.csv")
loneliness_p<-loneliness_cox$pvalue
loneliness_p_FDR<-p.adjust(loneliness_cox$pvalue,  # P
                           method = "bonferroni")                  
loneliness_cox<-cbind(loneliness_cox,loneliness_p_FDR)
write.csv(loneliness_cox,"D:/R/UKB_data/loneliness_muti/HRS/HRS_bonferroni.csv")