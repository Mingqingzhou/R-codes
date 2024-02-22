rm(list=ls())
library(tidyverse)
library(Rmisc)
library(sjmisc)
library(tidyverse)
library(tidyverse)
library(lubridate)
library(survival)
library(survminer)
library(haven)
library(rms)
library(ggplot2)
library(openxlsx)
library(survMisc)
library(forestplot)
library(forestploter)
library(readxl)
result_OSA<-read_excel("F:/desktop/R/yannis国自然/CMR.xlsx")
result_OSA$beta<-as.numeric(result_OSA$beta)
result_OSA$low<-as.numeric(result_OSA$low)
result_OSA$up<-as.numeric(result_OSA$up)
result_OSA
result_OSA$NO.of.SNPs <- as.character(result_OSA$NO.of.SNPs)
result_OSA[1,2] <- " "
result_OSA[1,3] <- " "
result_OSA[1,7] <- " "
result_OSA[1,8] <- " "
result_OSA[2:6,1] <- " "
a1<-result_OSA[,c(1:6)]

a2<-result_OSA[,7]
a3<-result_OSA[,8]
names(a1)
a1$` ` <- paste(rep(" ", 6), collapse = " ")
a1$`Beta (95%CI)`<-a2
a1$`P value`<-a3
a1
fig_OSA<- forest(a1[,c(1:3,7:9)],
                 est=result_OSA$beta,
                 lower=result_OSA$low,
                 upper=result_OSA$up,
                 ci_column = 4,
                 new_page =TRUE,
                 sizes = 0.5,
                 # arrow_lab = c("HR (95% CI)"),
                 xlim = c(-1,1),
                 ticks_at = c(-1,-0.5,0,0.5,1),
                 ref_line = 0,
                 xlab = "Beta (95% CI)",
                 theme = forest_theme(
                 base_size = 8,
                 xlab_cex = 1,
                 xaxis_cex = 0.75))

fig_OSA <- add_border(fig_OSA,part = "body", row = 0,gp = gpar(lwd = 2))
fig_OSA <- add_border(fig_OSA,part = "body",where = "top", row = 0,gp = gpar(lwd = 2))
fig_OSA <- add_border(fig_OSA,part = "body", row = 6,gp = gpar(lwd = 2))
fig_OSA<-edit_plot(fig_OSA,row = 1,which = "background",
                   gp=gpar(fill="gray", fontface = "bold"))

fig_OSA<-edit_plot(fig_OSA,row = 1,
                   gp=gpar(fill="gray", fontface = "bold"))

fig_OSA<-edit_plot(fig_OSA,row = c(2:6),which = "background",
                   gp=gpar(fill="white"))


p<-edit_plot(fig_OSA,row=2:6,col=4,
                   which = "ci",
                   gp = gpar(col = "#0053a0",fill = "#0053a0"))


ggsave(p,filename="F:/desktop/R/yannis国自然/fig_OSA.tiff",width = 6,height =3,dpi = 300,scale = 1.2)

#ggsave(fig_OSA,filename="F:/desktop/R/yannis国自然/fig_OSA.tiff",width = 5.64,height =2.47,dpi = 300)


result_OSA<-read_excel("F:/desktop/R/yannis国自然/CVD.xlsx")
result_OSA$beta<-as.numeric(result_OSA$beta)
result_OSA$low<-as.numeric(result_OSA$low)
result_OSA$up<-as.numeric(result_OSA$up)
result_OSA
result_OSA$NO.of.SNPs <- as.character(result_OSA$NO.of.SNPs)
result_OSA[1,2] <- " "
result_OSA[1,3] <- " "
result_OSA[1,7] <- " "
result_OSA[1,8] <- " "
result_OSA[2:3,1] <- " "
a1<-result_OSA[,c(1:6)]

a2<-result_OSA[,7]
a3<-result_OSA[,8]
names(a1)
a1$` ` <- paste(rep(" ", 3), collapse = " ")
a1$`OR (95%CI)`<-a2
a1$`P value`<-a3
a1
fig_OSA<- forest(a1[,c(1:3,7:9)],
                 est=result_OSA$beta,
                 lower=result_OSA$low,
                 upper=result_OSA$up,
                 ci_column = 4,
                 new_page =TRUE,
                 sizes = 0.5,
                 # arrow_lab = c("HR (95% CI)"),
                 xlim = c(0,3),
                 ticks_at = c(0,1,2,3),
                 ref_line = 1,
                 xlab = "OR (95% CI)",
                 theme = forest_theme(
                     base_size = 8,
                     xlab_cex = 1,
                     xaxis_cex = 0.75))

fig_OSA <- add_border(fig_OSA,part = "body", row = 0,gp = gpar(lwd = 2))
fig_OSA <- add_border(fig_OSA,part = "body",where = "top", row = 0,gp = gpar(lwd = 2))
fig_OSA <- add_border(fig_OSA,part = "body", row = 3,gp = gpar(lwd = 2))
fig_OSA<-edit_plot(fig_OSA,row = 1,which = "background",
                   gp=gpar(fill="gray", fontface = "bold"))

fig_OSA<-edit_plot(fig_OSA,row = 1,
                   gp=gpar(fill="gray", fontface = "bold"))

fig_OSA<-edit_plot(fig_OSA,row = c(2:3),which = "background",
                   gp=gpar(fill="white"))


p1<-edit_plot(fig_OSA,row=2:3,col=4,
                   which = "ci",
                   gp = gpar(col = "#0053a0",fill = "#0053a0"))



fig<-ggarrange(p,p1,ncol=1,nrow = 2)

ggsave(p1,filename="F:/desktop/R/yannis国自然/fig.tiff",width = 6,height =3,dpi = 300,scale = 1.2)








