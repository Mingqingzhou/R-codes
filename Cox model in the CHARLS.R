library(tidyverse)
library(survival)
library(survminer)
library(haven)
library(dplyr)
rm(list=ls())
setwd("F:/desktop/Yannis/CHARLS")

data<-read.csv("loneliness_outcome_im.csv")
glimpse(data)
view(data$ID)


###result function
Cox<-function(outcome,time,exposure,data){

    
    cox1<- coxph(Surv(time,outcome) ~lonely+ age.im + sex.im,data=data)  

    
    cox2 <-coxph(Surv(time,outcome)~lonely+ age.im + sex.im + ethnic.im+emplyment.im
                  +education.im+smoking.im+drinking.im+MBI_group
                  +activity_group.im,data=data)
    
    a<-cbind("HR" =  summary(cox1)$conf.int[1,1],
             "low" = summary(cox1)$conf.int[1,3],
             "up" = summary(cox1)$conf.int[1,4],
             "pvalue" =summary(cox1)$coefficients[1,5])
    b<-cbind("HR" =  summary(cox2)$conf.int[1,1],
             "low" = summary(cox2)$conf.int[1,3],
             "up" = summary(cox2)$conf.int[1,4],
             "pvalue" =summary(cox2)$coefficients[1,5])
    reference<-c(1,1,1,0)
    model1<-rbind(reference,a[1,])
    model2<-rbind(reference,b[1,])
    model1<- cbind("level"=c("reference","loneliness"),model1,"model"= "model1","exposure" = rep(exposure))
    model2<- cbind("level"=c("reference","loneliness"),model2,"model"= "model2","exposure" = rep(exposure))
    g <- rbind(model1,model2)
    class(g)
    result<-as.data.frame(g)
    result
    result[,2]<-sprintf("%0.2f", as.numeric(result[,2]))
    result[,3]<-sprintf("%0.2f", as.numeric(result[,3]))
    result[,4]<-sprintf("%0.2f", as.numeric(result[,4]))
    result[,5]<-sprintf("%0.3f", as.numeric(result[,5]))
    result
    result$blank<-rep("")
    result <- result %>% unite(CI,`HR`,`low`, sep = "(",remove = F)  
    result <- result %>% unite(CI,`CI`,`up`, sep = "-",remove = F)  
    result <- result %>% unite(CI,`CI`,`blank`, sep = ")")
    result
    result <- result[,-3:-5]
    result
    return(result)
}   

###----------------------------------------------------------------------------------------------------
data1 <- filter(data,diabetes.im==0&!is.na(incident_diabetes))
frq(data1$incident_diabetes)
a<-Cox(data1$incident_diabetes,data1$diabetes_followup,"diabetes",data1)
a

data2 <- filter(data,hypertension==0&!is.na(incident_hypertenison))
frq(data2$incident_hypertenison)
b<-Cox(data2$incident_hypertenison,data2$hypertension_followup,"hypertenison",data2)
b

data3 <- filter(data,Lung_Diseases==0&!is.na(incident_lung_disease))
c<-Cox(data3$incident_lung_disease,data3$lung_disease_followup,"incident_lung_disease",data3)
c

data4 <- filter(data,Liver_Disease==0&!is.na(incident_liver_disease))
d<-Cox(data4$incident_liver_disease,data4$liver_disease_followup,"liver_disease",data4)
d

data5 <- filter(data,Cancer==0&!is.na(incident_cancer))
frq(data5$incident_cancer)
e<-Cox(data5$incident_cancer,data5$cancer_followup,"cancer",data5)
e

data6 <- filter(data,Kidney_Diease==0&!is.na(incident_kidney_disease))
frq(data6$incident_kidney_disease)
f<-Cox(data6$incident_kidney_disease,data6$kidney_disease_followup,"kidney_disease",data6)
f

data7 <- filter(data,Stroke==0&!is.na(incident_stroke))
frq(data7$incident_kidney_disease)
g<-Cox(data7$incident_stroke,data7$stroke_followup,"stroke",data7)
g

data8 <- filter(data,Digestive_Disease==0&!is.na(incident_digestive_disease))
frq(data8$incident_digestive_disease)
h<-Cox(data8$incident_digestive_disease,data8$digestive_disease_followup,"digestive_disease",data8)
h

data9 <- filter(data,Heart_Problems==0&!is.na(incident_heart_disease))
frq(data9$incident_heart_disease)
i<-Cox(data9$incident_heart_disease,data9$heart_disease_followup,"heart_disease",data9)
i

data10 <- filter(data,Arthritis_or_Rheumatism==0&!is.na(incident_rheumatism))
frq(data10$incident_rheumatism)
j<-Cox(data10$incident_rheumatism,data10$rheumatism_followup,"Rheumatism",data10)
j

data11 <- filter(data,Memory_Disease==0&!is.na(incident_memory_disease))
frq(data11$incident_memory_disease)
k<-Cox(data11$incident_memory_disease,data11$memory_disease_followup,"memory_disease",data11)
k

data12 <- filter(data,Psychiatric_Problems==0&!is.na(incident_psychiatric_problems))
frq(data12$incident_psychiatric_problems)
l<-Cox(data12$incident_psychiatric_problems,data12$psychiatric_problems_followup,"psychiatric_problems",data12)
l

data13 <- filter(data,Asthma==0&!is.na(incident_asthma))
frq(data13$incident_asthma)
m<-Cox(data13$incident_asthma,data13$asthma_followup,"asthma",data13)
m

result<-rbind(a,b,c,d,e,f,g,h,i,j,k,l,m)
write.csv(result,file = "The effect of loneliness on 13 diseases.csv",row.names = F)
##########case-------------------------------------------------------------------------------------------
case<-function(x,x1,model,time,outcome,data){
    group_vars1 <- syms(x)
    group_vars2 <- syms(outcome)
    group_vars3 <- syms(time)  
    a<-data%>%group_by(!!!group_vars1)%>%dplyr::summarise(sum(!!!group_vars2))
    b<-data%>%group_by(!!!group_vars1)%>%dplyr::summarise(sum(!!!group_vars3))
    d <- cbind(a[,2],b[,2])
    d$personyear<-d[,1]/d[,2]*1000
    c<-table(x1)
    c<-data.frame(c,level=c("No_loneliness","loneliness"))
    z<-cbind( "level"=c[,3],
              "N" =c[,2],
              "case"= d[,1],
              "follow up"= d[,2],
              "person years"=d[,3],
              "model" =rep(model))
    z[,4]<-sprintf("%0.0f", as.numeric(z[,4]))
    z[,5]<-sprintf("%0.1f", as.numeric(z[,5]))
    z
    return(z)
}
a<- case(c("lonely"),data1$lonely,"diabetes",c("diabetes_followup"),c("incident_diabetes"),data1) 
a
b<- case(c("lonely"),data2$lonely,"hypertension",c("hypertension_followup"),c("incident_hypertenison"),data2) 
b
c<- case(c("lonely"),data3$lonely,"lung_disease",c("lung_disease_followup"),c("incident_lung_disease"),data3) 
c
d<- case(c("lonely"),data4$lonely,"liver_disease",c("liver_disease_followup"),c("incident_liver_disease"),data4) 
d
e<- case(c("lonely"),data5$lonely,"cancer",c("cancer_followup"),c("incident_cancer"),data5) 
e
f<- case(c("lonely"),data6$lonely,"kidney_disease",c("kidney_disease_followup"),c("incident_kidney_disease"),data6) 
f
g<- case(c("lonely"),data7$lonely,"stroke",c("stroke_followup"),c("incident_stroke"),data7)
g
h<- case(c("lonely"),data8$lonely,"digestive_disease",c("digestive_disease_followup"),c("incident_digestive_disease"),data8) 
h
i<- case(c("lonely"),data9$lonely,"heart_disease",c("heart_disease_followup"),c("incident_heart_disease"),data9) 
i
j<- case(c("lonely"),data10$lonely,"rheumatism",c("rheumatism_followup"),c("incident_rheumatism"),data10) 
j
k<- case(c("lonely"),data11$lonely,"memory_disease",c("memory_disease_followup"),c("incident_memory_disease"),data11) 
k
l<- case(c("lonely"),data12$lonely,"psychiatric_problems",c("psychiatric_problems_followup"),c("incident_psychiatric_problems"),data12) 
l
m<- case(c("lonely"),data13$lonely,"asthma",c("asthma_followup"),c("incident_asthma"),data13) 
m

case<-rbind(a,b,c,d,e,f,g,h,i,j,k,l,m)

write.csv(case,file = "13_diseases_case.csv",row.names = F)








