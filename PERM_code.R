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
data <- read.csv("F:/desktop/R/UKB data/inflammation/data.impute.done.perm1.csv")
names(data)

infla <- read.csv("F:/desktop/R/UKB data/inflammation/ukb668959.csv")
names(infla)

infla1 <- infla %>% mutate(
    WBC_count=X30000.0.0,
    Neutrophill_count=X30140.0.0,
    Neutrophill_per=X30200.0.0,
    Monocyte_count=X30130.0.0,
    Monocyte_per=X30190.0.0,
    Lymphocyte_count=X30120.0.0,
    Lymphocyte_per=X30180.0.0,
    Basophil_count=X30160.0.0,
    Basophil_per=X30220.0.0,
    Eosinophil_count=X30150.0.0,
    Eosinophil_per=X30210.0.0,
    CRP=X30710.0.0,
    Platelet_count=X30080.0.0) %>% dplyr::select(
        eid,Neutrophill_per,Lymphocyte_per,
        Basophil_per,Eosinophil_per,Monocyte_per)

names(infla1)

data1 <-data %>% left_join(infla1,by="eid")

names(data1)



library(lattice) 
library(MASS)
library(nnet)
library(mice) 


init = mice(data1, maxit=0)
meth = init$method
predM = init$predictorMatrix
meth
predM
predM[, c("eid")]=0
predM
imputated = mice(data1, method=meth, predictorMatrix=predM, m = 5, maxit=5, seed = 10000)

saveRDS(imputated, "imputated.rds")


data.impu.done <- complete(imputated, 1)

summary(data.impu.done)

write.csv(data.impu.done,"F:/desktop/R/UKB data/inflammation/data.impute.done.perm2.csv")

data1<- read.csv("F:/desktop/R/UKB data/inflammation/data.impute.done.perm2.csv")

withdraw<-read.table("F:/desktop/R/UKB data/inflammation/UKB withdrawn.csv", quote="\"", comment.char="")
data1<-filter(data1,!(data1$eid %in% withdraw$V1)) 


#----------------------------------------------------------------------------------------------------------------------
folder_path <- "F:/desktop/R/UKB data/inflammation/mutiple outcome/cancer/result/"
file_names<-list.files(path = folder_path,pattern = "\\.csv$",full.names = TRUE)
file_names
data_list <- lapply(file_names, read.csv)
data_list

combined_cancers <- data_list %>%
  reduce(full_join, by = "eid")
colnames(combined_cancers)


folder_path <- "F:/desktop/R/UKB data/inflammation/mutiple outcome/Circulation system disease/result/"
file_names<-list.files(path = folder_path,pattern = "\\.csv$",full.names = TRUE)
file_names
data_list <- lapply(file_names, read.csv)
data_list
combined_circulation <- data_list %>%
  reduce(full_join, by = "eid")
combined_circulation
colnames(combined_circulation)

folder_path <- "F:/desktop/R/UKB data/inflammation/mutiple outcome/Digestive system disease/result/"
file_names<-list.files(path = folder_path,pattern = "\\.csv$",full.names = TRUE)
file_names
data_list <- lapply(file_names, read.csv)
data_list
combined_digestive <- data_list %>%
  reduce(full_join, by = "eid")
combined_digestive
colnames(combined_digestive)

folder_path <- "F:/desktop/R/UKB data/inflammation/mutiple outcome/Endocrine, metabolic disease/result/"
file_names<-list.files(path = folder_path,pattern = "\\.csv$",full.names = TRUE)
file_names
data_list <- lapply(file_names, read.csv)
data_list
combined_metabolic <- data_list %>%
  reduce(full_join, by = "eid")
combined_metabolic
colnames(combined_metabolic)

folder_path <-  "F:/desktop/R/UKB data/inflammation/mutiple outcome/Genitourinary system/result/"
file_names<-list.files(path = folder_path,pattern = "\\.csv$",full.names = TRUE)
file_names
data_list <- lapply(file_names, read.csv)
data_list
combined_genitourinary <- data_list %>%
  reduce(full_join, by = "eid")
combined_genitourinary
colnames(combined_genitourinary)


folder_path <- "F:/desktop/R/UKB data/inflammation/mutiple outcome/Hematopoietic system/result/"
file_names<-list.files(path = folder_path,pattern = "\\.csv$",full.names = TRUE)
file_names
data_list <- lapply(file_names, read.csv)
data_list
combined_Hematopoietic <- data_list %>%
  reduce(full_join, by = "eid")
combined_Hematopoietic
colnames(combined_Hematopoietic)

folder_path <-"F:/desktop/R/UKB data/inflammation/mutiple outcome/Musculoskeletal system and connective tissue/result/"
file_names<-list.files(path = folder_path,pattern = "\\.csv$",full.names = TRUE)
file_names
data_list <- lapply(file_names, read.csv)
data_list
combined_Musculoskeletal<- data_list %>%
  reduce(full_join, by = "eid")
combined_Musculoskeletal
colnames(combined_Musculoskeletal)


folder_path <- "F:/desktop/R/UKB data/inflammation/mutiple outcome/Nervous system disease/result/"
file_names<-list.files(path = folder_path,pattern = "\\.csv$",full.names = TRUE)
file_names
data_list <- lapply(file_names, read.csv)
data_list
combined_Nervous<- data_list %>%
  reduce(full_join, by = "eid")
combined_Nervous
colnames(combined_Nervous)

folder_path <- "F:/desktop/R/UKB data/inflammation/mutiple outcome/Respiratory system disease/result/"
file_names<-list.files(path = folder_path,pattern = "\\.csv$",full.names = TRUE)
file_names
data_list <- lapply(file_names, read.csv)
data_list
combined_Respiratory<- data_list %>%
  reduce(full_join, by = "eid")
combined_Respiratory
colnames(combined_Respiratory)

folder_path <-  "F:/desktop/R/UKB data/inflammation/mutiple outcome/Mental and behavioural disorder/result/"
file_names<-list.files(path = folder_path,pattern = "\\.csv$",full.names = TRUE)
file_names
data_list <- lapply(file_names, read.csv)
data_list
combined_mental<- data_list %>%
  reduce(full_join, by = "eid")
combined_mental
colnames(combined_mental)


folder_path <- "F:/desktop/R/UKB data/inflammation/mutiple outcome/Infectious/result/"
file_names<-list.files(path = folder_path,pattern = "\\.csv$",full.names = TRUE)
file_names
data_list <- lapply(file_names, read.csv)
data_list
combined_Infectious<- data_list %>%
  reduce(full_join, by = "eid")
combined_Infectious
colnames(combined_Infectious)

folder_path <- "F:/desktop/R/UKB data/inflammation/mutiple outcome/Eye/result/"
file_names<-list.files(path = folder_path,pattern = "\\.csv$",full.names = TRUE)
file_names
data_list <- lapply(file_names, read.csv)
data_list
combined_Eye<- data_list %>%
  reduce(full_join, by = "eid")
combined_Eye
colnames(combined_Eye)


folder_path <- "F:/desktop/R/UKB data/inflammation/mutiple outcome/Ear/result/"
file_names<-list.files(path = folder_path,pattern = "\\.csv$",full.names = TRUE)
file_names
data_list <- lapply(file_names, read.csv)
data_list
combined_Ear<- data_list %>%
  reduce(full_join, by = "eid")
combined_Ear
colnames(combined_Ear)

folder_path <-"F:/desktop/R/UKB data/inflammation/mutiple outcome/Skin/result/"
file_names<-list.files(path = folder_path,pattern = "\\.csv$",full.names = TRUE)
file_names
data_list <- lapply(file_names, read.csv)
data_list
combined_Skin<- data_list %>%
  reduce(full_join, by = "eid")
combined_Skin
colnames(combined_Skin)




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

#function 
colnames(data1)
PERM<-function(x,exposure,ti,out,dataname){
  z<-length(table(x))-1
  x<-as.factor(x)
  #minimally 
  cox0 <- coxph(Surv(ti,out) ~ x + age + sex + Ethnicity + region, data = dataname)
  cox1<- coxph(Surv(ti,out) ~ x + age + sex + Ethnicity + region+TDI+education_level+
                 Employment, data = dataname)
  
  
  #behaviours
  cox2 <- coxph(Surv(ti,out) ~ x +age + sex + Ethnicity + region+
                  Final_Healthy_diet_score+Smoking+alcohol_intake+sleep_duration+
                  MVPA_self, data = dataname)
  
  #physiological factors
  cox3 <- coxph(Surv(ti,out) ~ x + age + sex + Ethnicity + region
                +BMI+SBP+DBP+glucose+LDL_C, data = dataname)
  
  #psychological
  cox4 <- coxph(Surv(ti,out) ~ x + age + sex + Ethnicity + region
                +help+depression+anxiety, data = dataname)
  #comorbidities
  cox5 <- coxph(Surv(ti,out) ~ x + age + sex + Ethnicity + region+
                longstanding+cancer+anti_hypertension_medicine.0+
                  cholestetol_lowering_mediaction.0+hypoglycemic0, data = dataname)
  #inflammation
  cox6 <- coxph(Surv(ti,out) ~ x + age + sex + Ethnicity + region+WBC_count+Neutrophill_count+Neutrophill_per+
                Monocyte_count+Monocyte_per+Lymphocyte_count+Lymphocyte_per+Basophil_count+Basophil_per+
                    Eosinophil_count+Eosinophil_per+CRP+Platelet_count, data = dataname)

  #all
  cox7 <- coxph(Surv(ti,out) ~x +age + sex + Ethnicity + region+TDI+education_level+
                  Employment+Final_Healthy_diet_score+Smoking+alcohol_intake+sleep_duration+
                  MVPA_self+BMI+SBP+DBP+glucose+LDL_C+help+depression+anxiety+
                  longstanding+cancer+anti_hypertension_medicine.0+
                  cholestetol_lowering_mediaction.0+hypoglycemic0+WBC_count+Neutrophill_count+Neutrophill_per+
                Monocyte_count+Monocyte_per+Lymphocyte_count+Lymphocyte_per+Basophil_count+Basophil_per+
                    Eosinophil_count+Eosinophil_per+CRP+Platelet_count,data=dataname)
  a<-cbind("HR" =  summary(cox0)$conf.int[1:z,c(1)],
           "low" = summary(cox0)$conf.int[1:z,c(3)],
           "up" = summary(cox0)$conf.int[1:z,c(4)],
           "pvalue" =summary(cox0)$coefficients[1:z,5])
  b<-cbind("HR" =  summary(cox1)$conf.int[1:z,c(1)],
           "low" = summary(cox1)$conf.int[1:z,c(3)],
           "up" = summary(cox1)$conf.int[1:z,c(4)],
           "pvalue" =summary(cox1)$coefficients[1:z,5])
  c<-cbind("HR" =  summary(cox2)$conf.int[1:z,c(1)],
           "low" = summary(cox2)$conf.int[1:z,c(3)],
           "up" = summary(cox2)$conf.int[1:z,c(4)],
           "pvalue" =summary(cox2)$coefficients[1:z,5])
  d<-cbind("HR" =  summary(cox3)$conf.int[1:z,c(1)],
           "low" = summary(cox3)$conf.int[1:z,c(3)],
           "up" = summary(cox3)$conf.int[1:z,c(4)],
           "pvalue" =summary(cox3)$coefficients[1:z,5])
  e<-cbind("HR" =  summary(cox4)$conf.int[1:z,c(1)],
           "low" = summary(cox4)$conf.int[1:z,c(3)],
           "up" = summary(cox4)$conf.int[1:z,c(4)],
           "pvalue" =summary(cox4)$coefficients[1:z,5])
  f<-cbind("HR" =  summary(cox5)$conf.int[1:z,c(1)],
           "low" = summary(cox5)$conf.int[1:z,c(3)],
           "up" = summary(cox5)$conf.int[1:z,c(4)],
           "pvalue" =summary(cox5)$coefficients[1:z,5])
  g<-cbind("HR" =  summary(cox6)$conf.int[1:z,c(1)],
           "low" = summary(cox6)$conf.int[1:z,c(3)],
           "up" = summary(cox6)$conf.int[1:z,c(4)],
           "pvalue" =summary(cox6)$coefficients[1:z,5])
  h<-cbind("HR" =  summary(cox7)$conf.int[1:z,c(1)],
           "low" = summary(cox7)$conf.int[1:z,c(3)],
           "up" = summary(cox7)$conf.int[1:z,c(4)],
           "pvalue" =summary(cox7)$coefficients[1:z,5])
  
  level<-c("No loneliness", "Loneliness")
  reference <- c(1,1,1,0)
  b <- rbind(reference,b)
  a <- rbind(reference,a)
  c <- rbind(reference,c)
  d <- rbind(reference,d)
  e <- rbind(reference,e)
  f <- rbind(reference,f)
  g <- rbind(reference,g)
  h <- rbind(reference,h)
  
  a <- cbind(a,level, "risk factor" = rep("minimal"),"exposure" = rep(exposure))
  b <- cbind(b,level, "risk factor" = rep("Socioeconomic factors"),"exposure" = rep(exposure))
  c <- cbind(c,level, "risk factor" = rep("Health behaviours"),"exposure" = rep(exposure))
  d <- cbind(d,level, "risk factor" = rep("Metabolic factors"),"exposure" = rep(exposure))
  e <- cbind(e,level, "risk factor" = rep("Psychological factors"),"exposure" = rep(exposure))
  f <- cbind(f,level, "risk factor" = rep("Cormobidities"),"exposure" = rep(exposure))
  g <- cbind(g,level, "risk factor" = rep("inflammation"),"exposure" = rep(exposure))
  h <- cbind(h,level, "risk factor" = rep("all"),"exposure" = rep(exposure))
  
  
  j <- rbind(a,b,c,d,e,f,g,h)
  result<-as.data.frame(j)
  result[,1]<-sprintf("%0.2f", as.numeric(result[,1]))
  result[,2]<-sprintf("%0.2f", as.numeric(result[,2]))
  result[,3]<-sprintf("%0.2f", as.numeric(result[,3]))
  result[,4]<-sprintf("%0.3f", as.numeric(result[,4]))
  result$blank<-rep("")
  result <- result %>% unite(CI,`HR`,`low`, sep = "(",remove = F)  
  result <- result %>% unite(CI,`CI`,`up`, sep = "-",remove = F)  
  result <- result %>% unite(CI,`CI`,`blank`, sep = ")")
  
  result[,2]<-as.character(result[,2])
  result[,2]<-as.numeric(result[,2])
  result$PERM <- rep(0)
  
    result$PERM[4] <- (result[2,2] - result[4,2]) / (result[2,2] - 1) *100
    result$PERM[6] <- (result[2,2] - result[6,2]) / (result[2,2] - 1) *100
    result$PERM[8] <- (result[2,2] - result[8,2]) / (result[2,2] - 1) *100
    result$PERM[10] <- (result[2,2] - result[10,2]) / (result[2,2] - 1) *100
    result$PERM[12] <- (result[2,2] - result[12,2]) / (result[2,2] - 1) *100
    result$PERM[14] <- (result[2,2] - result[14,2]) / (result[2,2] - 1) *100
    result$PERM[16] <- (result[2,2] - result[16,2]) / (result[2,2] - 1) *100
    #result$PERM[18] <- (result[2,2] - result[18,2]) / (result[2,2] - 1) *100
  
  result[,9]<-sprintf("%0.0f", as.numeric(result[,9]))
  result <- result[,-3:-4]
  result <- rownames_to_column(result, var = "rowname")
  return(result)
}

#run functon----------
data_IHD<-filter(data1,is.na(data1$Incidental.IHD.HDC.ICD10)==F)
x1<-PERM(data_IHD$loneliness_group,"loneliness~IHD",data_IHD$FUduration.IHD.HDC.ICD10,data_IHD$Incidental.IHD.HDC.ICD10,data_IHD)
x1
data_stroke<-filter(data1,is.na(data1$Incidental.stroke.HDC.ICD10)==F)
x2<-PERM(data_stroke$loneliness_group,"loneliness~stroke",data_stroke$FUduration.stroke.HDC.ICD10,data_stroke$Incidental.stroke.HDC.ICD10,data_stroke)
x2

data_hypothyroidism<-filter(data1,is.na(data1$Incidental.hypothyroidism.HDC.ICD10)==F)
x3<-PERM(data_hypothyroidism$loneliness_group,"loneliness~hypothyroidism",data_hypothyroidism$FUduration.hypothyroidism.HDC.ICD10,data_hypothyroidism$Incidental.hypothyroidism.HDC.ICD10,data_hypothyroidism)
x3

data_Asthma<-filter(data1,is.na(data1$Incidental.Asthma.HDC.ICD10)==F)
x4<-PERM(data_Asthma$loneliness_group,"loneliness~Asthma",data_Asthma$FUduration.Asthma.HDC.ICD10,data_Asthma$Incidental.Asthma.HDC.ICD10,data_Asthma)
x4


data_OSA<-filter(data1,is.na(data1$Incidental.OSA.HDC.ICD10)==F)
x5<-PERM(data_OSA$loneliness_group,"loneliness~OSA",data_OSA$FUduration.OSA.HDC.ICD10,data_OSA$Incidental.OSA.HDC.ICD10,data_OSA)

data_hear<-filter(data1,is.na(data1$Incidental.hear_loss.HDC.ICD10)==F)
x6<-PERM(data_hear$loneliness_group,"loneliness~hear_loss",data_hear$FUduration.hear_loss.HDC.ICD10,data_hear$Incidental.hear_loss.HDC.ICD10,data_hear)


x7<-PERM(data_anxiety$loneliness_group,"loneliness~anxiety",data_anxiety$FUduration.anxiety.HDC.ICD10,data_anxiety$Incidental.anxiety.HDC.ICD10,data_anxiety)
x7

#function 
colnames(data1)
PERM<-function(x,exposure,ti,out,dataname){
    z<-length(table(x))-1
    x<-as.factor(x)
    #minimally 
    cox0 <- coxph(Surv(ti,out) ~ x + age + sex + Ethnicity + region, data = dataname)
    
    cox1<- coxph(Surv(ti,out) ~ x + age + sex + Ethnicity + region+TDI+education_level+
                     Employment, data = dataname)
    
    
    #behaviours
    cox2 <- coxph(Surv(ti,out) ~ x +age + sex + Ethnicity + region+
                      Final_Healthy_diet_score+Smoking+alcohol_intake+sleep_duration+
                      MVPA_self, data = dataname)
    
    #physiological factors
    cox3 <- coxph(Surv(ti,out) ~ x + age + sex + Ethnicity + region
                  +BMI+SBP+DBP+glucose+LDL_C, data = dataname)
    

    #comorbidities
    cox4 <- coxph(Surv(ti,out) ~ x + age + sex + Ethnicity + region+
                      longstanding+cancer+anti_hypertension_medicine.0+
                      cholestetol_lowering_mediaction.0+hypoglycemic0, data = dataname)
    #inflammation
    cox5 <- coxph(Surv(ti,out) ~ x + age + sex + Ethnicity + region+WBC_count+Neutrophill_count+Neutrophill_per+
                      Monocyte_count+Monocyte_per+Lymphocyte_count+Lymphocyte_per+Basophil_count+Basophil_per+
                      Eosinophil_count+Eosinophil_per+CRP+Platelet_count, data = dataname)
    
    #all
    cox6 <- coxph(Surv(ti,out) ~x +age + sex + Ethnicity + region+TDI+education_level+
                      Employment+Final_Healthy_diet_score+Smoking+alcohol_intake+sleep_duration+
                      MVPA_self+BMI+SBP+DBP+glucose+LDL_C+
                      longstanding+cancer+anti_hypertension_medicine.0+
                      cholestetol_lowering_mediaction.0+hypoglycemic0+WBC_count+Neutrophill_count+Neutrophill_per+
                      Monocyte_count+Monocyte_per+Lymphocyte_count+Lymphocyte_per+Basophil_count+Basophil_per+
                      Eosinophil_count+Eosinophil_per+CRP+Platelet_count+psycho.0,data=dataname)
    a<-cbind("HR" =  summary(cox0)$conf.int[1:z,c(1)],
             "low" = summary(cox0)$conf.int[1:z,c(3)],
             "up" = summary(cox0)$conf.int[1:z,c(4)],
             "pvalue" =summary(cox0)$coefficients[1:z,5])
    b<-cbind("HR" =  summary(cox1)$conf.int[1:z,c(1)],
             "low" = summary(cox1)$conf.int[1:z,c(3)],
             "up" = summary(cox1)$conf.int[1:z,c(4)],
             "pvalue" =summary(cox1)$coefficients[1:z,5])
    c<-cbind("HR" =  summary(cox2)$conf.int[1:z,c(1)],
             "low" = summary(cox2)$conf.int[1:z,c(3)],
             "up" = summary(cox2)$conf.int[1:z,c(4)],
             "pvalue" =summary(cox2)$coefficients[1:z,5])
    d<-cbind("HR" =  summary(cox3)$conf.int[1:z,c(1)],
             "low" = summary(cox3)$conf.int[1:z,c(3)],
             "up" = summary(cox3)$conf.int[1:z,c(4)],
             "pvalue" =summary(cox3)$coefficients[1:z,5])
    e<-cbind("HR" =  summary(cox4)$conf.int[1:z,c(1)],
             "low" = summary(cox4)$conf.int[1:z,c(3)],
             "up" = summary(cox4)$conf.int[1:z,c(4)],
             "pvalue" =summary(cox4)$coefficients[1:z,5])
    f<-cbind("HR" =  summary(cox5)$conf.int[1:z,c(1)],
             "low" = summary(cox5)$conf.int[1:z,c(3)],
             "up" = summary(cox5)$conf.int[1:z,c(4)],
             "pvalue" =summary(cox5)$coefficients[1:z,5])
    g<-cbind("HR" =  summary(cox6)$conf.int[1:z,c(1)],
             "low" = summary(cox6)$conf.int[1:z,c(3)],
             "up" = summary(cox6)$conf.int[1:z,c(4)],
             "pvalue" =summary(cox6)$coefficients[1:z,5])

    
    level<-c("No loneliness", "Loneliness")
    reference <- c(1,1,1,0)
    b <- rbind(reference,b)
    a <- rbind(reference,a)
    c <- rbind(reference,c)
    d <- rbind(reference,d)
    e <- rbind(reference,e)
    f <- rbind(reference,f)
    g <- rbind(reference,g)
    
    a <- cbind(a,level, "risk factor" = rep("minimal"),"exposure" = rep(exposure))
    b <- cbind(b,level, "risk factor" = rep("Socioeconomic factors"),"exposure" = rep(exposure))
    c <- cbind(c,level, "risk factor" = rep("Health behaviours"),"exposure" = rep(exposure))
    d <- cbind(d,level, "risk factor" = rep("Metabolic factors"),"exposure" = rep(exposure))
    e <- cbind(e,level, "risk factor" = rep("Cormobidities"),"exposure" = rep(exposure))
    f <- cbind(f,level, "risk factor" = rep("inflammation"),"exposure" = rep(exposure))
    g <- cbind(g,level, "risk factor" = rep("all"),"exposure" = rep(exposure))
    
    j <- rbind(a,b,c,d,e,f,g)
    result<-as.data.frame(j)
    result[,1]<-sprintf("%0.2f", as.numeric(result[,1]))
    result[,2]<-sprintf("%0.2f", as.numeric(result[,2]))
    result[,3]<-sprintf("%0.2f", as.numeric(result[,3]))
    result[,4]<-sprintf("%0.3f", as.numeric(result[,4]))
    result$blank<-rep("")
    result <- result %>% unite(CI,`HR`,`low`, sep = "(",remove = F)  
    result <- result %>% unite(CI,`CI`,`up`, sep = "-",remove = F)  
    result <- result %>% unite(CI,`CI`,`blank`, sep = ")")
    
    result[,2]<-as.character(result[,2])
    result[,2]<-as.numeric(result[,2])
    result$PERM <- rep(0)
    
    result$PERM[4] <- (result[2,2] - result[4,2]) / (result[2,2] - 1) *100
    result$PERM[6] <- (result[2,2] - result[6,2]) / (result[2,2] - 1) *100
    result$PERM[8] <- (result[2,2] - result[8,2]) / (result[2,2] - 1) *100
    result$PERM[10] <- (result[2,2] - result[10,2]) / (result[2,2] - 1) *100
    result$PERM[12] <- (result[2,2] - result[12,2]) / (result[2,2] - 1) *100
    result$PERM[14] <- (result[2,2] - result[14,2]) / (result[2,2] - 1) *100
    #result$PERM[18] <- (result[2,2] - result[18,2]) / (result[2,2] - 1) *100
    
    result[,9]<-sprintf("%0.0f", as.numeric(result[,9]))
    result <- result[,-3:-4]
    result <- rownames_to_column(result, var = "rowname")
    return(result)
}

#run functon----------

data_anxiety<-filter(data1,is.na(data1$Incidental.anxiety.HDC.ICD10)==F)
x7<-PERM(data_anxiety$loneliness_group,"loneliness~anxiety",data_anxiety$FUduration.anxiety.HDC.ICD10,data_anxiety$Incidental.anxiety.HDC.ICD10,data_anxiety)
x7

data_depression<-filter(data1,is.na(data1$Incidental.depression.HDC.ICD10)==F)
x8<-PERM(data_depression$loneliness_group,"loneliness~depression",data_depression$FUduration.depression.HDC.ICD10,data_depression$Incidental.depression.HDC.ICD10,data_depression)
x8


data_abuse<-filter(data1,is.na(data1$Incidental.substance_abuse.HDC.ICD10)==F)
x9<-PERM(data_abuse$loneliness_group,"loneliness~abuse",data_abuse$FUduration.substance_abuse.HDC.ICD10,data_abuse$Incidental.substance_abuse.HDC.ICD10,data_abuse)
x9

perm_result<-rbind(x1,x2,x3,x4,x5,x6,x7,x8,x9)
perm_result
write.csv(perm_result,"F:/desktop/R/UKB data/inflammation/perm_result_new.csv")



library(forestplot)
library(base)
library(eoffice)
library(readxl)
library(forestploter)
library(ggplot2)
library(dplyr)
setwd("F:/desktop/R/UKB data/inflammation/")
result_IHD<-read.csv("perm_IHD.CSV")
result_IHD$mean<-as.numeric(result_IHD$mean)
result_IHD$low<-as.numeric(result_IHD$low)
result_IHD$up<-as.numeric(result_IHD$up)
result_IHD
a1<-result_IHD[,c(1:4)]
a2<-result_IHD[,5]
a3<-result_IHD[,6]
a1$` ` <- paste(rep(" ", 8), collapse = " ")
a1$`HR (95%CI)`<-a2
a1$`PERM (%)`<-a3
a1
fig_IHD<- forest(a1[,c(1,5:7)],
              est=result_IHD$mean,
              lower=result_IHD$low,
              upper=result_IHD$up,
              ci_column = 2,
              new_page =TRUE,
              graphwidth = unit(0.5,"npc"),
              sizes = 0.5,
              # arrow_lab = c("HR (95% CI)"),
              xlim = c(1,1.55),
              ticks_at = c(1,1.25,1.5),
              ref_line = 1,
              xlab = "Hazard Ratio (95% CI)")

fig_IHD <- add_border(fig_IHD,part = "body", row = 0,gp = gpar(lwd = 2))
fig_IHD <- add_border(fig_IHD,part = "body",where = "top", row = 0,gp = gpar(lwd = 2))
fig_IHD <- add_border(fig_IHD,part = "body", row = 9,gp = gpar(lwd = 2))
fig_IHD<-edit_plot(fig_IHD,row = 1,col = 4, 
                gp=gpar(col="#eff3f2",fontface="italic"))
fig_IHD<-edit_plot(fig_IHD,row = c(2:9),which = "background",
                gp=gpar(fill="white"))

fig_IHD<-edit_plot(fig_IHD,row = 1,which = "background",
                gp=gpar(fill="#eff3f2"))
fig_IHD<-edit_plot(fig_IHD,row=2,col=2,
                   which = "ci",
                   gp = gpar(col = "#E21A1C",fill = "#E21A1C"))
fig_IHD<-edit_plot(fig_IHD,row=3,col=2,
                   which = "ci",
                   gp = gpar(col = "#009F72",fill = "#009F72"))
fig_IHD<-edit_plot(fig_IHD,row=4,col=2,
                   which = "ci",
                   gp = gpar(col = "#d25f27",fill = "#d25f27"))
fig_IHD<-edit_plot(fig_IHD,row=5,col=2,
                   which = "ci",
                   gp = gpar(col = "#E49E21",fill = "#E49E21"))
fig_IHD<-edit_plot(fig_IHD,row=6,col=2,
                   which = "ci",
                   gp = gpar(col = "#FB9A99",fill = "#FB9A99"))
fig_IHD<-edit_plot(fig_IHD,row=7,col=2,
                   which = "ci",
                   gp = gpar(col = "#1F78B4",fill = "#1F78B4"))

fig_IHD<-edit_plot(fig_IHD, row=8, col=2,
                   which = "ci",
                   gp = gpar(col = "#9966CC", fill = "#9966CC"))

fig_IHD<-edit_plot(fig_IHD,row=9,col=2,
                   which = "ci",
                   gp = gpar(col = "black",fill = "black"))



fig_IHD
fig_IHD<-insert_text(fig_IHD,text = "Ischemic Heart Disease",col = 1,part = "header",just = "left",
                  gp=gpar(fontface="bold"))
ggsave(fig_IHD,filename="fig_IHD.tiff",width = 10,height =6,dpi = 300)



result_stroke<-read.csv("perm_stroke.CSV")
result_stroke$mean<-as.numeric(result_stroke$mean)
result_stroke$low<-as.numeric(result_stroke$low)
result_stroke$up<-as.numeric(result_stroke$up)
result_stroke
a1<-result_stroke[,c(1:4)]
a2<-result_stroke[,5]
a3<-result_stroke[,6]
a1$` ` <- paste(rep(" ", 8), collapse = " ")
a1$`HR (95%CI)`<-a2
a1$`PERM (%)`<-a3
a1
fig_stroke<- forest(a1[,c(1,5:7)],
                    est=result_stroke$mean,
                    lower=result_stroke$low,
                    upper=result_stroke$up,
                    ci_column = 2,
                    new_page =TRUE,
                    graphwidth = unit(0.5,"npc"),
                    sizes = 0.5,
                    # arrow_lab = c("HR (95% CI)"),
                    xlim = c(1,1.54),
                    ticks_at = c(1,1.25,1.5),
                    ref_line = 1,
                    xlab = "Hazard Ratio (95% CI)")
fig_stroke <- add_border(fig_stroke,part = "body", row = 0,gp = gpar(lwd = 2))
fig_stroke <- add_border(fig_stroke,part = "body",where = "top", row = 0,gp = gpar(lwd = 2))
fig_stroke <- add_border(fig_stroke,part = "body", row = 9,gp = gpar(lwd = 2))
fig_stroke<-edit_plot(fig_stroke,row = 1,col = 4, 
                      gp=gpar(col="#eff3f2",fontface="italic"))
fig_stroke<-edit_plot(fig_stroke,row = c(2:9),which = "background",
                      gp=gpar(fill="white"))

fig_stroke<-edit_plot(fig_stroke,row = 1,which = "background",
                      gp=gpar(fill="#eff3f2"))
fig_stroke<-edit_plot(fig_stroke,row=2,col=2,
                      which = "ci",
                      gp = gpar(col = "#E21A1C",fill = "#E21A1C"))
fig_stroke<-edit_plot(fig_stroke,row=3,col=2,
                      which = "ci",
                      gp = gpar(col = "#009F72",fill = "#009F72"))
fig_stroke<-edit_plot(fig_stroke,row=4,col=2,
                      which = "ci",
                      gp = gpar(col = "#d25f27",fill = "#d25f27"))
fig_stroke<-edit_plot(fig_stroke,row=5,col=2,
                      which = "ci",
                      gp = gpar(col = "#E49E21",fill = "#E49E21"))
fig_stroke<-edit_plot(fig_stroke,row=6,col=2,
                      which = "ci",
                      gp = gpar(col = "#FB9A99",fill = "#FB9A99"))
fig_stroke<-edit_plot(fig_stroke,row=7,col=2,
                      which = "ci",
                      gp = gpar(col = "#1F78B4",fill = "#1F78B4"))

fig_stroke<-edit_plot(fig_stroke, row=8, col=2,
                   which = "ci",
                   gp = gpar(col = "#9966CC", fill = "#9966CC"))

fig_stroke<-edit_plot(fig_stroke,row=9,col=2,
                   which = "ci",
                   gp = gpar(col = "black",fill = "black"))

fig_stroke
fig_stroke<-insert_text(fig_stroke,text = "Stroke",col = 1,part = "header",just = "left",
                        gp=gpar(fontface="bold"))
ggsave(fig_stroke,filename="fig_stroke.tiff",width = 10,height =6,dpi = 300)


result_hypothyroidism<-read.csv("perm_hypothyroidism.CSV")
result_hypothyroidism$mean<-as.numeric(result_hypothyroidism$mean)
result_hypothyroidism$low<-as.numeric(result_hypothyroidism$low)
result_hypothyroidism$up<-as.numeric(result_hypothyroidism$up)
result_hypothyroidism
a1<-result_hypothyroidism[,c(1:4)]
a2<-result_hypothyroidism[,5]
a3<-result_hypothyroidism[,6]
a1$` ` <- paste(rep(" ", 8), collapse = " ")
a1$`HR (95%CI)`<-a2
a1$`PERM (%)`<-a3
a1
fig_hypothyroidism<- forest(a1[,c(1,5:7)],
                            est=result_hypothyroidism$mean,
                            lower=result_hypothyroidism$low,
                            upper=result_hypothyroidism$up,
                            ci_column = 2,
                            new_page =TRUE,
                            graphwidth = unit(0.5,"npc"),
                            sizes = 0.5,
                            # arrow_lab = c("HR (95% CI)"),
                            xlim = c(1,1.5),
                            ticks_at = c(1,1.25,1.5),
                            ref_line = 1,
                            xlab = "Hazard Ratio (95% CI)")
fig_hypothyroidism <- add_border(fig_hypothyroidism,part = "body", row = 0,gp = gpar(lwd = 2))
fig_hypothyroidism <- add_border(fig_hypothyroidism,part = "body",where = "top", row = 0,gp = gpar(lwd = 2))
fig_hypothyroidism <- add_border(fig_hypothyroidism,part = "body", row = 9,gp = gpar(lwd = 2))
fig_hypothyroidism<-edit_plot(fig_hypothyroidism,row = 1,col = 4, 
                              gp=gpar(col="#eff3f2",fontface="italic"))
fig_hypothyroidism<-edit_plot(fig_hypothyroidism,row = c(2:9),which = "background",
                              gp=gpar(fill="white"))

fig_hypothyroidism<-edit_plot(fig_hypothyroidism,row = 1,which = "background",
                              gp=gpar(fill="#eff3f2"))
fig_hypothyroidism<-edit_plot(fig_hypothyroidism,row=2,col=2,
                              which = "ci",
                              gp = gpar(col = "#E21A1C",fill = "#E21A1C"))
fig_hypothyroidism<-edit_plot(fig_hypothyroidism,row=3,col=2,
                              which = "ci",
                              gp = gpar(col = "#009F72",fill = "#009F72"))
fig_hypothyroidism<-edit_plot(fig_hypothyroidism,row=4,col=2,
                              which = "ci",
                              gp = gpar(col = "#d25f27",fill = "#d25f27"))
fig_hypothyroidism<-edit_plot(fig_hypothyroidism,row=5,col=2,
                              which = "ci",
                              gp = gpar(col = "#E49E21",fill = "#E49E21"))
fig_hypothyroidism<-edit_plot(fig_hypothyroidism,row=6,col=2,
                              which = "ci",
                              gp = gpar(col = "#FB9A99",fill = "#FB9A99"))
fig_hypothyroidism<-edit_plot(fig_hypothyroidism,row=7,col=2,
                              which = "ci",
                              gp = gpar(col = "#1F78B4",fill = "#1F78B4"))

fig_hypothyroidism<-edit_plot(fig_hypothyroidism, row=8, col=2,
                      which = "ci",
                      gp = gpar(col = "#9966CC", fill = "#9966CC"))

fig_hypothyroidism<-edit_plot(fig_hypothyroidism,row=9,col=2,
                      which = "ci",
                      gp = gpar(col = "black",fill = "black"))


fig_hypothyroidism
fig_hypothyroidism<-insert_text(fig_hypothyroidism,text = "Hypothyroidism",col = 1,part = "header",just = "left",
                                gp=gpar(fontface="bold"))
ggsave(fig_hypothyroidism,filename="fig_hypothyroidism.tiff",width = 10,height =6,dpi = 300)


result_asthma<-read.csv("perm_asthma.CSV")
result_asthma$mean<-as.numeric(result_asthma$mean)
result_asthma$low<-as.numeric(result_asthma$low)
result_asthma$up<-as.numeric(result_asthma$up)
result_asthma
a1<-result_asthma[,c(1:4)]
a2<-result_asthma[,5]
a3<-result_asthma[,6]
a1$` ` <- paste(rep(" ", 8), collapse = " ")
a1$`HR (95%CI)`<-a2
a1$`PERM (%)`<-a3
a1
fig_asthma<- forest(a1[,c(1,5:7)],
                    est=result_asthma$mean,
                    lower=result_asthma$low,
                    upper=result_asthma$up,
                    ci_column = 2,
                    new_page =TRUE,
                    graphwidth = unit(0.5,"npc"),
                    sizes = 0.5,
                    # arrow_lab = c("HR (95% CI)"),
                    xlim = c(1,1.5),
                    ticks_at = c(1,1.25,1.5),
                    ref_line = 1,
                    xlab = "Hazard Ratio (95% CI)")
fig_asthma <- add_border(fig_asthma,part = "body", row = 0,gp = gpar(lwd = 2))
fig_asthma <- add_border(fig_asthma,part = "body",where = "top", row = 0,gp = gpar(lwd = 2))
fig_asthma <- add_border(fig_asthma,part = "body", row = 9,gp = gpar(lwd = 2))
fig_asthma<-edit_plot(fig_asthma,row = 1,col = 4, 
                      gp=gpar(col="#eff3f2",fontface="italic"))
fig_asthma<-edit_plot(fig_asthma,row = c(2:9),which = "background",
                      gp=gpar(fill="white"))

fig_asthma<-edit_plot(fig_asthma,row = 1,which = "background",
                      gp=gpar(fill="#eff3f2"))
fig_asthma<-edit_plot(fig_asthma,row=2,col=2,
                      which = "ci",
                      gp = gpar(col = "#E21A1C",fill = "#E21A1C"))
fig_asthma<-edit_plot(fig_asthma,row=3,col=2,
                      which = "ci",
                      gp = gpar(col = "#009F72",fill = "#009F72"))
fig_asthma<-edit_plot(fig_asthma,row=4,col=2,
                      which = "ci",
                      gp = gpar(col = "#d25f27",fill = "#d25f27"))
fig_asthma<-edit_plot(fig_asthma,row=5,col=2,
                      which = "ci",
                      gp = gpar(col = "#E49E21",fill = "#E49E21"))
fig_asthma<-edit_plot(fig_asthma,row=6,col=2,
                      which = "ci",
                      gp = gpar(col = "#FB9A99",fill = "#FB9A99"))
fig_asthma<-edit_plot(fig_asthma,row=7,col=2,
                      which = "ci",
                      gp = gpar(col = "#1F78B4",fill = "#1F78B4"))

fig_asthma<-edit_plot(fig_asthma, row=8, col=2,
                              which = "ci",
                              gp = gpar(col = "#9966CC", fill = "#9966CC"))

fig_asthma<-edit_plot(fig_asthma,row=9,col=2,
                              which = "ci",
                              gp = gpar(col = "black",fill = "black"))


fig_asthma
fig_asthma<-insert_text(fig_asthma,text = "Asthma",col = 1,part = "header",just = "left",
                        gp=gpar(fontface="bold"))
ggsave(fig_asthma,filename="fig_asthma.tiff",width = 10,height =6,dpi = 300)





result_depression<-read.csv("perm_depression.CSV")
result_depression$mean<-as.numeric(result_depression$mean)
result_depression$low<-as.numeric(result_depression$low)
result_depression$up<-as.numeric(result_depression$up)
result_depression
a1<-result_depression[,c(1:4)]
a2<-result_depression[,5]
a3<-result_depression[,6]
a1$` ` <- paste(rep(" ", 7), collapse = " ")
a1$`HR (95%CI)`<-a2
a1$`PERM (%)`<-a3
a1
fig_depression<- forest(a1[,c(1,5:7)],
                        est=result_depression$mean,
                        lower=result_depression$low,
                        upper=result_depression$up,
                        ci_column = 2,
                        new_page =TRUE,
                        graphwidth = unit(0.5,"npc"),
                        sizes = 0.5,
                        # arrow_lab = c("HR (95% CI)"),
                        xlim = c(1,2.76),
                        ticks_at = c(1,1.5,2,2.5),
                        ref_line = 1,
                        xlab = "Hazard Ratio (95% CI)")
fig_depression <- add_border(fig_depression,part = "body", row = 0,gp = gpar(lwd = 2))
fig_depression <- add_border(fig_depression,part = "body",where = "top", row = 0,gp = gpar(lwd = 2))
fig_depression <- add_border(fig_depression,part = "body", row = 8,gp = gpar(lwd = 2))
fig_depression<-edit_plot(fig_depression,row = 1,col = 4, 
                          gp=gpar(col="#eff3f2",fontface="italic"))
fig_depression<-edit_plot(fig_depression,row = c(2:8),which = "background",
                          gp=gpar(fill="white"))

fig_depression<-edit_plot(fig_depression,row = 1,which = "background",
                          gp=gpar(fill="#eff3f2"))
fig_depression<-edit_plot(fig_depression,row=2,col=2,
                          which = "ci",
                          gp = gpar(col = "#E21A1C",fill = "#E21A1C"))
fig_depression<-edit_plot(fig_depression,row=3,col=2,
                          which = "ci",
                          gp = gpar(col = "#009F72",fill = "#009F72"))
fig_depression<-edit_plot(fig_depression,row=4,col=2,
                          which = "ci",
                          gp = gpar(col = "#d25f27",fill = "#d25f27"))
fig_depression<-edit_plot(fig_depression,row=5,col=2,
                          which = "ci",
                          gp = gpar(col = "#E49E21",fill = "#E49E21"))
fig_depression<-edit_plot(fig_depression,row=6,col=2,
                          which = "ci",
                          gp = gpar(col = "#FB9A99",fill = "#FB9A99"))

fig_depression<-edit_plot(fig_depression, row=7, col=2,
                      which = "ci",
                      gp = gpar(col = "#9966CC", fill = "#9966CC"))

fig_depression<-edit_plot(fig_depression,row=8,col=2,
                      which = "ci",
                      gp = gpar(col = "black",fill = "black"))


fig_depression<-insert_text(fig_depression,text = "Depression",col = 1,part = "header",just = "left",
                            gp=gpar(fontface="bold"))
ggsave(fig_depression,filename="fig_depression.tiff",width = 10,height =6,dpi = 300)


result_anxiety<-read.csv("perm_anxiety.CSV")
result_anxiety$mean<-as.numeric(result_anxiety$mean)
result_anxiety$low<-as.numeric(result_anxiety$low)
result_anxiety$up<-as.numeric(result_anxiety$up)
result_anxiety
a1<-result_anxiety[,c(1:4)]
a2<-result_anxiety[,5]
a3<-result_anxiety[,6]
a1$` ` <- paste(rep(" ", 7), collapse = " ")
a1$`HR (95%CI)`<-a2
a1$`PERM (%)`<-a3
a1
fig_anxiety<- forest(a1[,c(1,5:7)],
                     est=result_anxiety$mean,
                     lower=result_anxiety$low,
                     upper=result_anxiety$up,
                     ci_column = 2,
                     new_page =TRUE,
                     graphwidth = unit(0.5,"npc"),
                     sizes = 0.5,
                     # arrow_lab = c("HR (95% CI)"),
                     xlim = c(1,2.5),
                     ticks_at = c(1,1.5,2,2.5),
                     ref_line = 1,
                     xlab = "Hazard Ratio (95% CI)")
fig_anxiety <- add_border(fig_anxiety,part = "body", row = 0,gp = gpar(lwd = 2))
fig_anxiety <- add_border(fig_anxiety,part = "body",where = "top", row = 0,gp = gpar(lwd = 2))
fig_anxiety <- add_border(fig_anxiety,part = "body", row = 8,gp = gpar(lwd = 2))
fig_anxiety<-edit_plot(fig_anxiety,row = 1,col = 4, 
                       gp=gpar(col="#eff3f2",fontface="italic"))
fig_anxiety<-edit_plot(fig_anxiety,row = c(2:8),which = "background",
                       gp=gpar(fill="white"))

fig_anxiety<-edit_plot(fig_anxiety,row = 1,which = "background",
                       gp=gpar(fill="#eff3f2"))
fig_anxiety<-edit_plot(fig_anxiety,row=2,col=2,
                       which = "ci",
                       gp = gpar(col = "#E21A1C",fill = "#E21A1C"))
fig_anxiety<-edit_plot(fig_anxiety,row=3,col=2,
                       which = "ci",
                       gp = gpar(col = "#009F72",fill = "#009F72"))
fig_anxiety<-edit_plot(fig_anxiety,row=4,col=2,
                       which = "ci",
                       gp = gpar(col = "#d25f27",fill = "#d25f27"))
fig_anxiety<-edit_plot(fig_anxiety,row=5,col=2,
                       which = "ci",
                       gp = gpar(col = "#E49E21",fill = "#E49E21"))
fig_anxiety<-edit_plot(fig_anxiety,row=6,col=2,
                       which = "ci",
                       gp = gpar(col = "#FB9A99",fill = "#FB9A99"))

fig_anxiety<-edit_plot(fig_anxiety, row=7, col=2,
                          which = "ci",
                          gp = gpar(col = "#9966CC", fill = "#9966CC"))

fig_anxiety<-edit_plot(fig_anxiety,row=8,col=2,
                          which = "ci",
                          gp = gpar(col = "black",fill = "black"))


fig_anxiety
fig_anxiety<-insert_text(fig_anxiety,text = "Anxiety",col = 1,part = "header",just = "left",
                         gp=gpar(fontface="bold"))
ggsave(fig_anxiety,filename="fig_anxiety.tiff",width = 10,height =6,dpi = 300)


result_abuse<-read.csv("perm_abuse.CSV")
result_abuse$mean<-as.numeric(result_abuse$mean)
result_abuse$low<-as.numeric(result_abuse$low)
result_abuse$up<-as.numeric(result_abuse$up)
result_abuse
a1<-result_abuse[,c(1:4)]
a2<-result_abuse[,5]
a3<-result_abuse[,6]
a1$` ` <- paste(rep(" ", 7), collapse = " ")
a1$`HR (95%CI)`<-a2
a1$`PERM (%)`<-a3
a1
fig_abuse<- forest(a1[,c(1,5:7)],
                   est=result_abuse$mean,
                   lower=result_abuse$low,
                   upper=result_abuse$up,
                   ci_column = 2,
                   new_page =TRUE,
                   graphwidth = unit(0.5,"npc"),
                   sizes = 0.5,
                   # arrow_lab = c("HR (95% CI)"),
                   xlim = c(1,2.5),
                   ticks_at = c(1,1.5,2,2.5),
                   ref_line = 1,
                   xlab = "Hazard Ratio (95% CI)")
fig_abuse <- add_border(fig_abuse,part = "body", row = 0,gp = gpar(lwd = 2))
fig_abuse <- add_border(fig_abuse,part = "body",where = "top", row = 0,gp = gpar(lwd = 2))
fig_abuse <- add_border(fig_abuse,part = "body", row = 8,gp = gpar(lwd = 2))
fig_abuse<-edit_plot(fig_abuse,row = 1,col = 4, 
                     gp=gpar(col="#eff3f2",fontface="italic"))
fig_abuse<-edit_plot(fig_abuse,row = c(2:8),which = "background",
                     gp=gpar(fill="white"))

fig_abuse<-edit_plot(fig_abuse,row = 1,which = "background",
                     gp=gpar(fill="#eff3f2"))
fig_abuse<-edit_plot(fig_abuse,row=2,col=2,
                     which = "ci",
                     gp = gpar(col = "#E21A1C",fill = "#E21A1C"))
fig_abuse<-edit_plot(fig_abuse,row=3,col=2,
                     which = "ci",
                     gp = gpar(col = "#009F72",fill = "#009F72"))
fig_abuse<-edit_plot(fig_abuse,row=4,col=2,
                     which = "ci",
                     gp = gpar(col = "#d25f27",fill = "#d25f27"))
fig_abuse<-edit_plot(fig_abuse,row=5,col=2,
                     which = "ci",
                     gp = gpar(col = "#E49E21",fill = "#E49E21"))
fig_abuse<-edit_plot(fig_abuse,row=6,col=2,
                     which = "ci",
                     gp = gpar(col = "#FB9A99",fill = "#FB9A99"))

fig_abuse<-edit_plot(fig_abuse, row=7, col=2,
                       which = "ci",
                       gp = gpar(col = "#9966CC", fill = "#9966CC"))

fig_abuse<-edit_plot(fig_abuse,row=8,col=2,
                       which = "ci",
                       gp = gpar(col = "black",fill = "black"))


fig_abuse
fig_abuse<-insert_text(fig_abuse,text = "Psychoactive Substance Abuse",col = 1,part = "header",just = "left",
                       gp=gpar(fontface="bold"))
ggsave(fig_abuse,filename="fig_abuse.tiff",width = 10,height =6,dpi = 300)



result_OSA<-read.csv("perm_OSA.CSV")
result_OSA$mean<-as.numeric(result_OSA$mean)
result_OSA$low<-as.numeric(result_OSA$low)
result_OSA$up<-as.numeric(result_OSA$up)
result_OSA
a1<-result_OSA[,c(1:4)]
a2<-result_OSA[,5]
a3<-result_OSA[,6]
a1$` ` <- paste(rep(" ", 8), collapse = " ")
a1$`HR (95%CI)`<-a2
a1$`PERM (%)`<-a3
a1
fig_OSA<- forest(a1[,c(1,5:7)],
                 est=result_OSA$mean,
                 lower=result_OSA$low,
                 upper=result_OSA$up,
                 ci_column = 2,
                 new_page =TRUE,
                 graphwidth = unit(0.5,"npc"),
                 sizes = 0.5,
                 # arrow_lab = c("HR (95% CI)"),
                 xlim = c(1,2.09),
                 ticks_at = c(1,1.5,2),
                 ref_line = 1,
                 xlab = "Hazard Ratio (95% CI)")

fig_OSA <- add_border(fig_OSA,part = "body", row = 0,gp = gpar(lwd = 2))
fig_OSA <- add_border(fig_OSA,part = "body",where = "top", row = 0,gp = gpar(lwd = 2))
fig_OSA <- add_border(fig_OSA,part = "body", row = 9,gp = gpar(lwd = 2))
fig_OSA<-edit_plot(fig_OSA,row = 1,col = 4, 
                   gp=gpar(col="#eff3f2",fontface="italic"))
fig_OSA<-edit_plot(fig_OSA,row = c(2:9),which = "background",
                   gp=gpar(fill="white"))

fig_OSA<-edit_plot(fig_OSA,row = 1,which = "background",
                   gp=gpar(fill="#eff3f2"))
fig_OSA<-edit_plot(fig_OSA,row=2,col=2,
                   which = "ci",
                   gp = gpar(col = "#E21A1C",fill = "#E21A1C"))
fig_OSA<-edit_plot(fig_OSA,row=3,col=2,
                   which = "ci",
                   gp = gpar(col = "#009F72",fill = "#009F72"))
fig_OSA<-edit_plot(fig_OSA,row=4,col=2,
                   which = "ci",
                   gp = gpar(col = "#d25f27",fill = "#d25f27"))
fig_OSA<-edit_plot(fig_OSA,row=5,col=2,
                   which = "ci",
                   gp = gpar(col = "#E49E21",fill = "#E49E21"))
fig_OSA<-edit_plot(fig_OSA,row=6,col=2,
                   which = "ci",
                   gp = gpar(col = "#FB9A99",fill = "#FB9A99"))
fig_OSA<-edit_plot(fig_OSA,row=7,col=2,
                   which = "ci",
                   gp = gpar(col = "#1F78B4",fill = "#1F78B4"))

fig_OSA<-edit_plot(fig_OSA, row=8, col=2,
                      which = "ci",
                      gp = gpar(col = "#9966CC", fill = "#9966CC"))

fig_OSA<-edit_plot(fig_OSA,row=9,col=2,
                      which = "ci",
                      gp = gpar(col = "black",fill = "black"))


fig_OSA
fig_OSA<-insert_text(fig_OSA,text = "Sleep Apnoea",col = 1,part = "header",just = "left",
                     gp=gpar(fontface="bold"))
ggsave(fig_OSA,filename="fig_OSA.tiff",width = 10,height =6,dpi = 300)



result_hear<-read.csv("perm_hearing_lose.CSV")
result_hear$mean<-as.numeric(result_hear$mean)
result_hear$low<-as.numeric(result_hear$low)
result_hear$up<-as.numeric(result_hear$up)
result_hear
a1<-result_hear[,c(1:4)]
a2<-result_hear[,5]
a3<-result_hear[,6]
a1$` ` <- paste(rep(" ", 8), collapse = " ")
a1$`HR (95%CI)`<-a2
a1$`PERM (%)`<-a3
a1
fig_hear<- forest(a1[,c(1,5:7)],
                  est=result_hear$mean,
                  lower=result_hear$low,
                  upper=result_hear$up,
                  ci_column = 2,
                  new_page =TRUE,
                  graphwidth = unit(0.5,"npc"),
                  sizes = 0.5,
                  # arrow_lab = c("HR (95% CI)"),
                  xlim = c(1,1.55),
                  ticks_at = c(1,1.25,1.5),
                  ref_line = 1,
                  xlab = "Hazard Ratio (95% CI)")

fig_hear <- add_border(fig_hear,part = "body", row = 0,gp = gpar(lwd = 2))
fig_hear <- add_border(fig_hear,part = "body",where = "top", row = 0,gp = gpar(lwd = 2))
fig_hear <- add_border(fig_hear,part = "body", row = 9,gp = gpar(lwd = 2))
fig_hear<-edit_plot(fig_hear,row = 1,col = 4, 
                    gp=gpar(col="#eff3f2",fontface="italic"))
fig_hear<-edit_plot(fig_hear,row = c(2:9),which = "background",
                    gp=gpar(fill="white"))

fig_hear<-edit_plot(fig_hear,row = 1,which = "background",
                    gp=gpar(fill="#eff3f2"))
fig_hear<-edit_plot(fig_hear,row=2,col=2,
                    which = "ci",
                    gp = gpar(col = "#E21A1C",fill = "#E21A1C"))
fig_hear<-edit_plot(fig_hear,row=3,col=2,
                    which = "ci",
                    gp = gpar(col = "#009F72",fill = "#009F72"))
fig_hear<-edit_plot(fig_hear,row=4,col=2,
                    which = "ci",
                    gp = gpar(col = "#d25f27",fill = "#d25f27"))
fig_hear<-edit_plot(fig_hear,row=5,col=2,
                    which = "ci",
                    gp = gpar(col = "#E49E21",fill = "#E49E21"))
fig_hear<-edit_plot(fig_hear,row=6,col=2,
                    which = "ci",
                    gp = gpar(col = "#FB9A99",fill = "#FB9A99"))
fig_hear<-edit_plot(fig_hear,row=7,col=2,
                    which = "ci",
                    gp = gpar(col = "#1F78B4",fill = "#1F78B4"))

fig_hear<-edit_plot(fig_hear, row=8, col=2,
                   which = "ci",
                   gp = gpar(col = "#9966CC", fill = "#9966CC"))

fig_hear<-edit_plot(fig_hear,row=9,col=2,
                   which = "ci",
                   gp = gpar(col = "black",fill = "black"))


fig_hear
fig_hear<-insert_text(fig_hear,text = "Hearing Loss",col = 1,part = "header",just = "left",
                      gp=gpar(fontface="bold"))
ggsave(fig_hear,filename="fig_hear.tiff",width = 10,height =6,dpi = 300)





?ggarrange
fig_permall<-ggarrange(fig_IHD,fig_stroke,fig_hypothyroidism,fig_asthma,
                       fig_depression,fig_anxiety,fig_abuse,
                       fig_OSA,fig_hear,ncol=3,nrow = 3)

fig_permall
ggsave(fig_permall,filename="fig_permall.tiff",width = 17,height =9,dpi = 300)




maxt<-read.csv("matrix1.csv")
colnames(maxt)
maxt<-maxt%>%dplyr::rename(`PERM(%)`=`PERM....`)
maxt$Mediators<-factor(maxt$Mediators,levels = c("Socioeconomic factors","Health behaviors","Metabolic factors",
                       "Psychological factors","Comorbidities","Inflammatory factors ","All"))
maxt$Diseases<-factor(maxt$Diseases,levels = c("Hearing loss", "Sleep Apnoea",
                                               "Psychoactive Substance Abuse",
                                               "Depression","Anxiety","Asthma",
                                               "Hypothroidism",
                                               "Stroke","Ischemic Heart Disease"))


p2<-ggplot(data=maxt,aes(x=Mediators,y=Diseases,fill=`PERM(%)`))+
  geom_tile()+
  scale_fill_gradient(low = "white", high = "#256cae")+
  annotate("text",x=4,y=4,
           label = expression(italic("NA")), 
           size = 6)+
  annotate("text",x=4,y=3,
           label = expression(italic("NA")), 
           size = 6)+
  annotate("text",x=4,y=5,
           label = expression(italic("NA")), 
           size = 6)+

  geom_text(aes(label = sprintf("%.0f", `PERM(%)`)),  size = 6)+
  theme_classic()+
  theme (axis.text.x = element_text (size = 13,face = "bold"),
         axis.text.y = element_text (size = 13,face = "bold"))+
  xlab("Mediators") + theme(axis.title.x = element_text(size = 18, face = "bold"))+
  ylab("")+ theme(axis.title.y = element_text(size = 18, face = "bold"))+
  scale_x_discrete(labels=function(x) str_wrap(x, width=15))+
  scale_y_discrete(labels=function(x) str_wrap(x, width=25))
p2
ggsave(p2,filename="matrix.tiff",width = 14,height =10,dpi = 300)
