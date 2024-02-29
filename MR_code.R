rm(list=ls())
library(data.table)
library(TwoSampleMR)
library(MRPRESSO)


input_path <- "F:/desktop/R/MR code/all/all_snp"
folders <- list.files(input_path, full.names = T, recursive = FALSE)
name1 <- c("ref","alt","rsids","beta","sebeta","pval")
name2 <- c("SNP","effect_allele","other_allele","beta","standard_error","p_value")
name3 <- c("SNP","beta.outcome","se.outcome","effect_allele.outcome","other_allele.outcome","pval.outcome")
name4 <- c("SNP","BETA","standard_error","A1","A2","P")
name5 <- c("variant_id","effect_allele","other_allele","beta","standard_error","p_value")
name6 <- c("SNP","BETA","SE","A1","A2","P")

result <- data.frame()
Q_result <- data.frame()
ple_result <- data.frame()
presso_result <- list()

error_folders <- vector()
noname <- vector()



loneliness_exp_dat <- read.csv("F:/desktop/R/MR code/all/loneliness_need_ukb_8476_e06_0.01.csv")
mr_method_list()


for (i in folders ) {
      data <- fread(i)
      if (all(name1 %in% names(data))){
          tryCatch({
          ex_out_mer<- merge(loneliness_exp_dat,data,by.x="SNP",by.y="rsids")
          ex_out_mer$pval <- round(ex_out_mer$pval,3) 
          ex_out_mer_re <- subset(ex_out_mer,pval>=5e-6)#
          name<- paste0("loneliness-e6-",basename (i),".csv")
          out <- file.path("C:/Users/zmq/AppData/Local/R/win-library/4.3/TwoSampleMR/MRnew", name )
          write.csv(ex_out_mer_re,out,row.names = FALSE)
          name <- paste0("MRnew/",name)
          outcome_file1<-system.file(name, package="TwoSampleMR")
          outcome_exp_dat1 <- read_outcome_data(
              snps=loneliness_exp_dat$SNP,
              filename = outcome_file1,
              sep = ",",
              snp_col = "SNP",
              beta_col = "beta",
              se_col = "sebeta",
              effect_allele_col = "alt",
              other_allele_col = "ref",
              pval_col = "pval")
          
          dat1 <- harmonise_data(exposure_dat=loneliness_exp_dat, outcome_dat=outcome_exp_dat1)#
          
          res0<- generate_odds_ratios(mr_res = mr(dat1,method_list = c("mr_ivw_mre","mr_ivw","mr_weighted_median","mr_egger_regression ")))
          
          MRoutcome0 <- data.frame("method"=res0$method,"nsnp"=res0$nsnp,"OR"=res0$or,"CI_low"=res0$or_lci95,
                                   "CI_up"=res0$or_uci95,"P"=res0$pval,"outcome"=basename (i),"exposure"="loneliness")
          
          MRoutcome_modified0 <- MRoutcome0%>%
              dplyr::mutate(OR= round(OR, 2),
                            CI_low= round(CI_low, 2),
                            CI_up=round(CI_up, 2),
                            P=round(P, 3))
          MRoutcome_modified0$CI <- paste0(MRoutcome_modified0$CI_low,"—",MRoutcome_modified0$CI_up )
          
          result <- rbind(result,MRoutcome_modified0) 
         
          Q_test <- mr_heterogeneity(dat1)
          Q<- data.frame("outcome"=basename(i),"Q"=Q_test$Q[2],"Q_pval"=Q_test$Q_pval[2],"Q_test"="Q_test")
          Q_result <- rbind(Q_result,Q)
          
          ple_test <- mr_pleiotropy_test(dat1)
          ple <- data.frame("outcome"=basename(i),"intercept"=ple_test$egger_intercept,"se"=ple_test$se,"pval"=ple_test$pval,"pleiotropy"="pleiotropy")
          ple_result <- rbind(ple_result,ple)
          presso<- mr_presso(BetaOutcome ="beta.outcome", BetaExposure = "beta.exposure", SdOutcome ="se.outcome", SdExposure = "se.exposure", 
                           OUTLIERtest = TRUE,DISTORTIONtest = TRUE, data = dat1, NbDistribution = 3000, SignifThreshold = 5e-6)  
          names(presso)[1] <- basename (i)
          presso_result <- c(presso_result, presso)
          
          }, error = function(e) {
              cat("error:", conditionMessage(e), "\n")
              error_folders <<- c(error_folders, basename(i))
              
          })
      } else if (all(name2 %in% names(data))){
          tryCatch({
          ex_out_mer<- merge(loneliness_exp_dat,data,by="SNP")
          ex_out_mer$p_value <- round(ex_out_mer$p_value,3) 
          ex_out_mer_re <- subset(ex_out_mer,p_value>=5e-6)#
          name<- paste0("loneliness-e6-",basename (i),".csv")
          out <- file.path("C:/Users/zmq/AppData/Local/R/win-library/4.3/TwoSampleMR/MRnew", name )
          write.csv(ex_out_mer_re,out,row.names = FALSE)
          name <- paste0("MRnew/",name)
          outcome_file1<-system.file(name, package="TwoSampleMR")
          outcome_exp_dat1 <- read_outcome_data(
              snps=loneliness_exp_dat$SNP,
              filename = outcome_file1,
              sep = ",",
              snp_col = "SNP",
              beta_col = "beta",
              se_col = "standard_error",
              effect_allele_col = "effect_allele",
              other_allele_col = "other_allele",
              pval_col = "p_value")
          
          dat1 <- harmonise_data(exposure_dat=loneliness_exp_dat, outcome_dat=outcome_exp_dat1)#
          
          res0<- generate_odds_ratios(mr_res = mr(dat1,method_list = c("mr_ivw_mre","mr_ivw","mr_weighted_median","mr_egger_regression ")))
          
          MRoutcome0 <- data.frame("method"=res0$method,"nsnp"=res0$nsnp,"OR"=res0$or,"CI_low"=res0$or_lci95,
                                   "CI_up"=res0$or_uci95,"P"=res0$pval,"outcome"=basename (i),"exposure"="loneliness")
          
          MRoutcome_modified0 <- MRoutcome0%>%
              dplyr::mutate(OR= round(OR, 2),
                            CI_low= round(CI_low, 2),
                            CI_up=round(CI_up, 2),
                            P=round(P, 3))
          MRoutcome_modified0$CI <- paste0(MRoutcome_modified0$CI_low,"—",MRoutcome_modified0$CI_up )
          
          result <- rbind(result,MRoutcome_modified0) 
          
          
          Q_test <- mr_heterogeneity(dat1)
          Q<- data.frame("outcome"=basename(i),"Q"=Q_test$Q[2],"Q_pval"=Q_test$Q_pval[2],"Q_test"="Q_test")
          Q_result <- rbind(Q_result,Q)
          
          ple_test <- mr_pleiotropy_test(dat1)
          ple <- data.frame("outcome"=basename(i),"intercept"=ple_test$egger_intercept,"se"=ple_test$se,"pval"=ple_test$pval,"pleiotropy"="pleiotropy")
          ple_result <- rbind(ple_result,ple)
          presso<- mr_presso(BetaOutcome ="beta.outcome", BetaExposure = "beta.exposure", SdOutcome ="se.outcome", SdExposure = "se.exposure", 
                          OUTLIERtest = TRUE,DISTORTIONtest = TRUE, data = dat1, NbDistribution = 3000, SignifThreshold = 5e-6)  
          names(presso)[1] <- basename (i)
          presso_result <- c(presso_result, presso)
          }, error = function(e) {
              cat("error:", conditionMessage(e), "\n")
              error_folders <<- c(error_folders, basename(i))
              
          })
          
      }else if(all(name3 %in% names(data))){
          tryCatch({
          ex_out_mer<- merge(loneliness_exp_dat,data,by="SNP")
          ex_out_mer$pval.outcome <- round(ex_out_mer$pval.outcome,3) 
          ex_out_mer_re <- subset(ex_out_mer,pval.outcome>=5e-6)#
          name<- paste0("loneliness-e6-",basename (i),".csv")
          out <- file.path("C:/Users/zmq/AppData/Local/R/win-library/4.3/TwoSampleMR/MRnew", name )
          write.csv(ex_out_mer_re,out,row.names = FALSE)
          name <- paste0("MRnew/",name)
          outcome_file1<-system.file(name, package="TwoSampleMR")
          outcome_exp_dat1 <- read_outcome_data(
              snps=loneliness_exp_dat$SNP,
              filename = outcome_file1,
              sep = ",",
              snp_col = "SNP",
              beta_col = "beta.outcome",
              se_col = "se.outcome",
              effect_allele_col = "effect_allele.outcome",
              other_allele_col = "other_allele.outcome",
              pval_col = "pval.outcome")
          
          dat1 <- harmonise_data(exposure_dat=loneliness_exp_dat, outcome_dat=outcome_exp_dat1)#
          
          res0<- generate_odds_ratios(mr_res = mr(dat1,method_list = c("mr_ivw_mre","mr_ivw","mr_weighted_median","mr_egger_regression ")))
          
          MRoutcome0 <- data.frame("method"=res0$method,"nsnp"=res0$nsnp,"OR"=res0$or,"CI_low"=res0$or_lci95,
                                   "CI_up"=res0$or_uci95,"P"=res0$pval,"outcome"=basename (i),"exposure"="loneliness")
          
          MRoutcome_modified0 <- MRoutcome0%>%
              dplyr::mutate(OR= round(OR, 2),
                            CI_low= round(CI_low, 2),
                            CI_up=round(CI_up, 2),
                            P=round(P, 3))
          MRoutcome_modified0$CI <- paste0(MRoutcome_modified0$CI_low,"—",MRoutcome_modified0$CI_up )
          
          result <- rbind(result,MRoutcome_modified0) 
          
          Q_test <- mr_heterogeneity(dat1)
          Q<- data.frame("outcome"=basename(i),"Q"=Q_test$Q[2],"Q_pval"=Q_test$Q_pval[2],"Q_test"="Q_test")
          Q_result <- rbind(Q_result,Q)
          
          ple_test <- mr_pleiotropy_test(dat1)
          ple <- data.frame("outcome"=basename(i),"intercept"=ple_test$egger_intercept,"se"=ple_test$se,"pval"=ple_test$pval,"pleiotropy"="pleiotropy")
          ple_result <- rbind(ple_result,ple)
          presso<- mr_presso(BetaOutcome ="beta.outcome", BetaExposure = "beta.exposure", SdOutcome ="se.outcome", SdExposure = "se.exposure", 
                             OUTLIERtest = TRUE,DISTORTIONtest = TRUE, data = dat1, NbDistribution = 3000, SignifThreshold = 5e-6)  
          names(presso)[1] <- basename (i)
          presso_result <- c(presso_result, presso)
          }, error = function(e) {
              cat("error:", conditionMessage(e), "\n")
              error_folders <<- c(error_folders, basename(i))
              
          })
      }else if(all(name4 %in% names(data))){
          tryCatch({
          ex_out_mer<- merge(loneliness_exp_dat,data,by="SNP")
          ex_out_mer$P <- round(ex_out_mer$P,3) 
          ex_out_mer_re <- subset(ex_out_mer,P>=5e-6)#
          name<- paste0("loneliness-e6-",basename (i),".csv")
          out <- file.path("C:/Users/zmq/AppData/Local/R/win-library/4.3/TwoSampleMR/MRnew", name )
          write.csv(ex_out_mer_re,out,row.names = FALSE)
          name <- paste0("MRnew/",name)
          outcome_file1<-system.file(name, package="TwoSampleMR")
          outcome_exp_dat1 <- read_outcome_data(
              snps=loneliness_exp_dat$SNP,
              filename = outcome_file1,
              sep = ",",
              snp_col = "SNP",
              beta_col = "BETA",
              se_col = "standard_error",
              effect_allele_col = "A1",
              other_allele_col = "A2",
              pval_col = "P")
          
          dat1 <- harmonise_data(exposure_dat=loneliness_exp_dat, outcome_dat=outcome_exp_dat1)#
          
          res0<- generate_odds_ratios(mr_res = mr(dat1,method_list = c("mr_ivw_mre","mr_ivw","mr_weighted_median","mr_egger_regression ")))
          
          MRoutcome0 <- data.frame("method"=res0$method,"nsnp"=res0$nsnp,"OR"=res0$or,"CI_low"=res0$or_lci95,
                                   "CI_up"=res0$or_uci95,"P"=res0$pval,"outcome"=basename (i),"exposure"="loneliness")
          
          MRoutcome_modified0 <- MRoutcome0%>%
              dplyr::mutate(OR= round(OR, 2),
                            CI_low= round(CI_low, 2),
                            CI_up=round(CI_up, 2),
                            P=round(P, 3))
          MRoutcome_modified0$CI <- paste0(MRoutcome_modified0$CI_low,"—",MRoutcome_modified0$CI_up )
          
          result <- rbind(result,MRoutcome_modified0) 
          
          Q_test <- mr_heterogeneity(dat1)
          Q<- data.frame("outcome"=basename(i),"Q"=Q_test$Q[2],"Q_pval"=Q_test$Q_pval[2],"Q_test"="Q_test")
          Q_result <- rbind(Q_result,Q)
          
          ple_test <- mr_pleiotropy_test(dat1)
          ple <- data.frame("outcome"=basename(i),"intercept"=ple_test$egger_intercept,"se"=ple_test$se,"pval"=ple_test$pval,"pleiotropy"="pleiotropy")
          ple_result<- rbind(ple_result,ple)
          presso<- mr_presso(BetaOutcome ="beta.outcome", BetaExposure = "beta.exposure", SdOutcome ="se.outcome", SdExposure = "se.exposure", 
                            OUTLIERtest = TRUE,DISTORTIONtest = TRUE, data = dat1, NbDistribution = 3000, SignifThreshold = 5e-6)  
           names(presso)[1] <- basename (i)
          presso_result <- c(presso_result, presso)
          }, error = function(e) {
              cat("error:", conditionMessage(e), "\n")
              error_folders <<- c(error_folders, basename(i))
              
          })
          
      } else if(all(name5 %in% names(data))){
          tryCatch({
              ex_out_mer<- merge(loneliness_exp_dat,data,by.x="SNP",by.y="variant_id")
              ex_out_mer$p_value <- round(ex_out_mer$p_value,3) 
              ex_out_mer_re <- subset(ex_out_mer,p_value>=5e-6)#
              name<- paste0("loneliness-e6-",basename (i),".csv")
              out <- file.path("C:/Users/zmq/AppData/Local/R/win-library/4.3/TwoSampleMR/MRnew", name )
              write.csv(ex_out_mer_re,out,row.names = FALSE)
              name <- paste0("MRnew/",name)
              outcome_file1<-system.file(name, package="TwoSampleMR")
              outcome_exp_dat1 <- read_outcome_data(
                  snps=loneliness_exp_dat$SNP,
                  filename = outcome_file1,
                  sep = ",",
                  snp_col = "SNP",
                  beta_col = "beta",
                  se_col = "standard_error",
                  effect_allele_col = "effect_allele",
                  other_allele_col = "other_allele",
                  pval_col = "p_value")
              
              dat1 <- harmonise_data(exposure_dat=loneliness_exp_dat, outcome_dat=outcome_exp_dat1)#
              
              res0<- generate_odds_ratios(mr_res = mr(dat1,method_list = c("mr_ivw_mre","mr_ivw","mr_weighted_median","mr_egger_regression ")))
              
              MRoutcome0 <- data.frame("method"=res0$method,"nsnp"=res0$nsnp,"OR"=res0$or,"CI_low"=res0$or_lci95,
                                       "CI_up"=res0$or_uci95,"P"=res0$pval,"outcome"=basename (i),"exposure"="loneliness")
              
              MRoutcome_modified0 <- MRoutcome0%>%
                  dplyr::mutate(OR= round(OR, 2),
                                CI_low= round(CI_low, 2),
                                CI_up=round(CI_up, 2),
                                P=round(P, 3))
              MRoutcome_modified0$CI <- paste0(MRoutcome_modified0$CI_low,"—",MRoutcome_modified0$CI_up)
              
              result <- rbind(result,MRoutcome_modified0) 
              
              Q_test <- mr_heterogeneity(dat1)
              Q<- data.frame("outcome"=basename(i),"Q"=Q_test$Q[2],"Q_pval"=Q_test$Q_pval[2],"Q_test"="Q_test")
              Q_result <- rbind(Q_result,Q)
              
              ple_test <- mr_pleiotropy_test(dat1)
              ple <- data.frame("outcome"=basename(i),"intercept"=ple_test$egger_intercept,"se"=ple_test$se,"pval"=ple_test$pval,"pleiotropy"="pleiotropy")
              ple_result<- rbind(ple_result,ple)
              presso<- mr_presso(BetaOutcome ="beta.outcome", BetaExposure = "beta.exposure", SdOutcome ="se.outcome", SdExposure = "se.exposure", 
                              OUTLIERtest = TRUE,DISTORTIONtest = TRUE, data = dat1, NbDistribution = 3000, SignifThreshold = 5e-6)  
              names(presso)[1] <- basename (i)
              presso_result <- c(presso_result, presso)
          }, error = function(e) {
              cat("error:", conditionMessage(e), "\n")
              error_folders <<- c(error_folders, basename(i))
              
          })
          
          }else if(all(name6 %in% names(data))){
          tryCatch({
              ex_out_mer<- merge(loneliness_exp_dat,data,by="SNP")
              ex_out_mer$P<- round(ex_out_mer$P,3) 
              ex_out_mer_re <- subset(ex_out_mer,P>=5e-6)#
              name<- paste0("loneliness-e6-",basename (i),".csv")
              out <- file.path("C:/Users/zmq/AppData/Local/R/win-library/4.3/TwoSampleMR/MRnew", name )
              write.csv(ex_out_mer_re,out,row.names = FALSE)
              name <- paste0("MRnew/",name)
              outcome_file1<-system.file(name, package="TwoSampleMR")
              outcome_exp_dat1 <- read_outcome_data(
                  snps=loneliness_exp_dat$SNP,
                  filename = outcome_file1,
                  sep = ",",
                  snp_col = "SNP",
                  beta_col = "BETA",
                  se_col = "SE",
                  effect_allele_col = "A1",
                  other_allele_col = "A2",
                  pval_col = "P")
              
              dat1 <- harmonise_data(exposure_dat=loneliness_exp_dat, outcome_dat=outcome_exp_dat1)#
              
              res0<- generate_odds_ratios(mr_res = mr(dat1,method_list = c("mr_ivw_mre","mr_ivw","mr_weighted_median","mr_egger_regression ")))
              
              MRoutcome0 <- data.frame("method"=res0$method,"nsnp"=res0$nsnp,"OR"=res0$or,"CI_low"=res0$or_lci95,
                                       "CI_up"=res0$or_uci95,"P"=res0$pval,"outcome"=basename (i),"exposure"="loneliness")
              
              MRoutcome_modified0 <- MRoutcome0%>%
                  dplyr::mutate(OR= round(OR, 2),
                                CI_low= round(CI_low, 2),
                                CI_up=round(CI_up, 2),
                                P=round(P, 3))
              MRoutcome_modified0$CI <- paste0(MRoutcome_modified0$CI_low,"—",MRoutcome_modified0$CI_up )
              
              result <- rbind(result,MRoutcome_modified0) 
              
              Q_test <- mr_heterogeneity(dat1)
              Q<- data.frame("outcome"=basename(i),"Q"=Q_test$Q[2],"Q_pval"=Q_test$Q_pval[2],"Q_test"="Q_test")
              Q_result <- rbind(Q_result,Q)
              
              ple_test <- mr_pleiotropy_test(dat1)
              ple <- data.frame("outcome"=basename(i),"intercept"=ple_test$egger_intercept,"se"=ple_test$se,"pval"=ple_test$pval,"pleiotropy"="pleiotropy")
              ple_result<- rbind(ple_result,ple)
              presso<- mr_presso(BetaOutcome ="beta.outcome", BetaExposure = "beta.exposure", SdOutcome ="se.outcome", SdExposure = "se.exposure", 
                                OUTLIERtest = TRUE,DISTORTIONtest = TRUE, data = dat1, NbDistribution = 3000, SignifThreshold = 5e-6)  
              names(presso)[1] <- basename (i)
              presso_result <- c(presso_result, presso)
          }, error = function(e) {
              cat("error:", conditionMessage(e), "\n")
              error_folders <<- c(error_folders, basename(i))
              
          })
              }else {
          cat("None of the names match")
           noname <<- c(noname, basename(i))
          
      }
}
              

result 

Q_result 

ple_result 


presso_result

error_folders 

noname



####
combined_data <- data.frame()

# 
n <- "Distortion Test"
for (i in seq(1, length(presso_result), by = 2)){
    study_name <- names(presso_result)[i]
    study_data <- presso_result[[i]]
    n_rows <- nrow(study_data)
    presso_Global <- presso_result[[i+1]]$`Global Test`
    
    if(all(n %in% names(presso_result[[i+1]]))){
        
      presso_Distortion <- presso_result[[i+1]]$`Distortion Test`
      if(is.numeric(presso_Distortion$`Outliers Indices`)){
          temp_data <- cbind(
              study_data[, c("MR Analysis", "Causal Estimate", "Sd", "T-stat", "P-value")],
              MR_PRESSO_outlier=rep(length(presso_Distortion$`Outliers Indices`), n_rows),
              MR_PRESSO_Disto_P=rep(presso_Distortion$Pvalue, n_rows),
              MR_PRESSO_global_Pvalue=rep(presso_Global$Pvalue, n_rows))
          }else if (presso_Distortion$`Outliers Indices`=="No significant outliers"){
              temp_data <- cbind(
                  study_data[, c("MR Analysis", "Causal Estimate", "Sd", "T-stat", "P-value")],
                  MR_PRESSO_outlier=rep(0, n_rows),
                  MR_PRESSO_Disto_P=rep(NA, n_rows),
                  MR_PRESSO_global_Pvalue=rep(presso_Global$Pvalue, n_rows))
          }
    }else{
      presso_Distortion <- list(
            `Outliers Indices` = NA,
            `Distortion Coefficient` = NA,
            Pvalue = NA
        )
    temp_data <- cbind(
        study_data[, c("MR Analysis", "Causal Estimate", "Sd", "T-stat", "P-value")],
        MR_PRESSO_outlier=rep(presso_Distortion$`Outliers Indices`, n_rows),
        MR_PRESSO_Disto_P=rep(presso_Distortion$Pvalue, n_rows),
        MR_PRESSO_global_Pvalue=rep(presso_Global$Pvalue, n_rows)
    )
    }
    temp_data$Study <- gsub(".gz", "", study_name)
    
    combined_data <- rbind(combined_data,temp_data)
    
    rm(presso_Distortion,presso_Global,temp_data)
    
}

write.csv(result,file = "F:/desktop/R/MR code/all/final_result/total_results-e6_0.01.csv",row.names = F)

write.csv(Q_result,file = "F:/desktop/R/MR code/all/final_result/total_Q-results-e6_0.01.csv",row.names = F)

write.csv(ple_result,file = "F:/desktop/R/MR code/all/final_result/total_ple-results-e6_0.01.csv",row.names = F)

write.csv(combined_data,file = "F:/desktop/R/MR code/all/final_result/total_presso-results-e6_0.01.csv",row.names = F)
