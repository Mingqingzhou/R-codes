library(survival)
library(survminer)
library(haven)
library(dplyr)
rm(list=ls())

setwd("F:/desktop/Yannis/HRS/Sample_1_06_10_14_18")
data<-read.csv("final_data_imput.csv")
glimpse(data)


###result function
Cox<-function(outcome,time,exposure,data){
    
    cox1<- coxph(Surv(time,outcome) ~lonely_group+ age.im + sex.im, data=data)  
    
    
    cox2 <-coxph(Surv(time,outcome)~lonely_group+ age.im + sex.im + ethnic.im+employment.im
                  +education.im+smoking.im+drinking.im+BMI.im+activity.im,data=data)
    
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
data1 <- filter(data,Heart_Problems==0&!is.na(incident_heart_disease))
frq(data1$incident_heart_disease)
frq(data1$Heart_Problems)
a<-Cox(data1$incident_heart_disease,data1$followup_heart_disease,"heart_disease",data1)
a

names(data)

data2 <- filter(data,hypertension==0&!is.na(incident_high_pressure))
b<-Cox(data2$incident_high_pressure,data2$followup_high_pressure,"hypertenison",data2)
b

data3 <- filter(data,heart_attack==0&!is.na(incident_heart_attack))
frq(data3$incident_heart_attack)
c<-Cox(data3$incident_heart_attack,data3$followup_heart_attack,"heart_attack",data3)
c

data4 <- filter(data,angina==0&!is.na(incident_angina))
d<-Cox(data4$incident_angina,data4$followup_angina,"angina",data4)
d

data5 <- filter(data,congestive_HF==0&!is.na(incident_congestive_HF))
e<-Cox(data5$incident_congestive_HF,data5$followup_congestive_HF,"congestive_HF",data5)
e

data6 <- filter(data,abnormal_rhythm==0&!is.na(incident_abnormal_rhythm))
f<-Cox(data6$incident_abnormal_rhythm,data6$followup_abnormal_rhythm,"abnormal_rhythm",data6)
f

data7 <- filter(data,Arthritis_or_Rheumatism==0&!is.na(incident_arthritis))
g<-Cox(data7$incident_arthritis,data7$followup_arthritis,"arthritis",data7)
g


data8 <- filter(data,Stroke==0&!is.na(incident_stroke))
h<-Cox(data8$incident_stroke,data8$followup_stroke,"stroke",data8)
h

data9 <- filter(data,Cancer==0&!is.na(incident_cancer))
frq(data9$incident_cancer)
i<-Cox(data9$incident_cancer,data9$followup_cancer,"cancer",data9)
i

data10 <- filter(data,Lung_Diseases==0&!is.na(incident_lung_disease))
frq(data10$incident_lung_disease)

data10 <- data10[!is.infinite(data10$followup_lung_disease), ]
j<-Cox(data10$incident_lung_disease,data10$followup_lung_disease,"lung_disease",data10)
j


data11 <- filter(data,Memory_Disease==0&!is.na(incident_memory_disease))

frq(data11$incident_memory_disease)
k<-Cox(data11$incident_memory_disease,data11$followup_memory_disease,"memory_disease",data11)
k

data12 <- filter(data,Psychiatric_Problems==0 & !is.na(incident_psychiatric_problems))
frq(data12$incident_psychiatric_problems)
l<-Cox(data12$incident_psychiatric_problems,data12$followup_psychiatric_problems,"psychiatric_problems",data12)
l

data13 <- filter(data,diabetes==0 & !is.na(incident_diabetes))
frq(data13$incident_diabetes)
m<-Cox(data13$incident_diabetes,data13$followup_diabetes,"diabets",data13)
m



names(data)


result<-rbind(a,b,c,d,e,f,g,h,i,j,k,l,m)
result
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
a<- case(c("lonely_group"),data1$lonely_group,"heart_disease",c("followup_heart_disease"),c("incident_heart_disease"),data1) 
a
b<- case(c("lonely_group"),data2$lonely_group,"hypertension",c("followup_high_pressure"),c("incident_high_pressure"),data2) 
b
c<- case(c("lonely_group"),data3$lonely_group,"heart_attack",c("followup_heart_attack"),c("incident_heart_attack"),data3) 
c
d<- case(c("lonely_group"),data4$lonely_group,"angina",c("followup_angina"),c("incident_angina"),data4) 
d
e<- case(c("lonely_group"),data5$lonely_group,"congestive_HF",c("followup_congestive_HF"),c("incident_congestive_HF"),data5) 
e
f<- case(c("lonely_group"),data6$lonely_group,"abnormal_rhythm",c("followup_abnormal_rhythm"),c("incident_abnormal_rhythm"),data6) 
f
g<- case(c("lonely_group"),data7$lonely_group,"Arthritis_or_Rheumatism",c("followup_arthritis"),c("incident_arthritis"),data7)
g
h<- case(c("lonely_group"),data8$lonely_group,"stroke",c("followup_stroke"),c("incident_stroke"),data8) 
h
i<- case(c("lonely_group"),data9$lonely_group,"cancer",c("followup_cancer"),c("incident_cancer"),data9) 
i
j<- case(c("lonely_group"),data10$lonely_group,"lung_disease",c("followup_lung_disease"),c("incident_lung_disease"),data10) 
j
k<- case(c("lonely_group"),data11$lonely_group,"memory_disease",c("followup_memory_disease"),c("incident_memory_disease"),data11) 
k
l<- case(c("lonely_group"),data12$lonely_group,"psychiatric_problems",c("followup_psychiatric_problems"),c("incident_psychiatric_problems"),data12) 
l
m<- case(c("lonely_group"),data13$lonely_group,"diabetes",c("followup_diabetes"),c("incident_diabetes"),data13) 
m

case<-rbind(a,b,c,d,e,f,g,h,i,j,k,l,m)

write.csv(case,file = "13_diseases_case.csv",row.names = F)








