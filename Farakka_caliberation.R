rm(list=ls())
library(dplyr)
library(tidyr)
library(ggplot2)
library(fitdistrplus)
library(stats)
library(xtable)
library(anytime)
library(devtools)
install_github("onofriandreapg/aomisc")
library(aomisc) ##nls package
options(scipen = 999) ##disabling exponential notation

jrc<-read.csv("jrcdata.csv")
colorado<-read.csv("Farakka_RW_FOBSdata_18.07.22.csv")
str(jrc)
str(colorado)

##Formatting date in jrc for merging
jrc<-jrc%>%mutate(day_measurement=case_when(month!="February"&tendaily==1~10,
                                            month!="February"&tendaily==2~20,
                                            month!="February"&tendaily==3~30,
                                            month=="February"&tendaily==1~10,
                                            month=="February"&tendaily==2~20,
                                            TRUE~28))
jrc$date_formatted<-paste(jrc$month,jrc$day_measurement,jrc$year,sep=" ")

##Formatting date in colorado for merging
colorado$month<-month.name[colorado$month]
colorado$date_formatted<-paste(colorado$month,colorado$day,colorado$year,sep=" ")
                               
##Creating new column called 'location' in jrc for matching
jrc<-jrc%>%mutate(location=case_when(variable=="flow_at_frk"~"flow_us_frk",
                                     variable=="release_BD"|variable=="flow_at_hrdg"~"flow_ds_frk",
                                            TRUE~"NA"))
colnames(jrc)[colnames(jrc)=="cumecs_flow"]<-"cumecs_jrc"
colnames(colorado)[colnames(colorado)=="disch_cumecs"]<-"cumecs_colorado"

##Merging the two datasets by 'location' and 'date_formatted'
merged_df<-merge(x=jrc,y=colorado,by=c("date_formatted","location"))
summary(merged_df)
merged_df$date_formatted<-anydate(merged_df$date_formatted)
merged_df$JRC_colorado_ratio<-merged_df$cumecs_jrc/merged_df$cumecs_colorado

##merged_df has 3 NA's for colorado data, remove
merged_df<-merged_df%>%drop_na(cumecs_colorado)

##Exporting the merged df
write.excel <- function(merged_df,row.names=FALSE,col.names=TRUE,...) {
  write.table(merged_df,file="clipboard-16384",sep="\t",row.names=row.names,col.names=col.names,...)
}

write.excel(merged_df)

##Checking distribution of both discharge variables
hist(merged_df$cumecs_jrc)
fit.norm.jrc<-fitdist(merged_df$cumecs_jrc,"norm")
fit.lognorm.jrc<-fitdist(merged_df$cumecs_jrc,"lnorm")
cdfcomp(list(fit.norm.jrc,fit.lognorm.jrc))
qqcomp(list(fit.norm.jrc,fit.lognorm.jrc)) ##log normal seems to fit the distribution a little more

hist(merged_df$cumecs_colorado)
fit.norm.colorado<-fitdist(merged_df$cumecs_colorado,"norm")
fit.lognorm.colorado<-fitdist(merged_df$cumecs_colorado,"lnorm")
cdfcomp(list(fit.norm.colorado,fit.lognorm.colorado))
qqcomp(list(fit.norm.colorado,fit.lognorm.colorado)) ##normal distribution

merged_df$month.x<-factor(merged_df$month.x,levels=c("January","February","March","April","May"))
prelim_plot<-ggplot(merged_df,aes(x=cumecs_colorado,y=cumecs_jrc))+
  geom_point(color="#4E84C4",size=0.9)

prelim_plot_monthwise<-ggplot(merged_df,aes(x=cumecs_colorado,y=cumecs_jrc))+
  geom_point(color="#4E84C4",size=0.9)+
  facet_wrap(~month.x)

ggsave("Monthwise_18.07.22.png", plot=prelim_plot_monthwise,width=35,height=25,unit="cm",dpi=600)    

############------------------------------------------
##Subsetting merged data, based on location (August 1, 2022)
us_frk<-merged_df%>%filter(location=="flow_us_frk" & variable=="flow_at_frk")
ds_frk_hrdg<-merged_df%>%filter(location=="flow_ds_frk" & variable=="flow_at_hrdg")
ds_frk_BD<-merged_df%>%filter(location=="flow_ds_frk" & variable=="release_BD")  

############------------------------------------------
##Caliberation regression for us_frk
plot(us_frk$cumecs_jrc~us_frk$cumecs_colorado)
monthwise_us_frk<-ggplot(data=us_frk,aes(x=cumecs_colorado,y=cumecs_jrc))+
  geom_point()+
  facet_wrap(~month.x)

##some visible outliers from scatterplot
us_frk%>%filter(month.x=="January")%>%filter(cumecs_jrc>4500)
us_frk%>%filter(month.x=="April")%>%filter(cumecs_colorado>30000)

##Models for lm_us_frk  
lm_us_frk<-lm(formula = cumecs_jrc~cumecs_colorado,data=us_frk)
summary(lm_us_frk)
plot(lm_us_frk)

lm_us_frk2<-lm(formula = cumecs_jrc~cumecs_colorado+I(cumecs_colorado^2),data=us_frk)
summary(lm_us_frk2)
plot(lm_us_frk2)

lm_us_frk_month<-lm(formula = cumecs_jrc~cumecs_colorado+month.x,data=us_frk)
summary(lm_us_frk_month)
plot(lm_us_frk_month)

lm_us_frk_month2<-lm(formula = cumecs_jrc~cumecs_colorado+I(cumecs_colorado^2)+month.x,data=us_frk)
summary(lm_us_frk_month2)
plot(lm_us_frk_month2)

lm_us_frk_month3<-lm(formula = cumecs_jrc~cumecs_colorado*month.x,data=us_frk)
summary(lm_us_frk_month3)
plot(lm_us_frk_month3) ##no significant interaction effect

##Modifying after removing outliers
us_frk_modified<-us_frk[-c(151,176,352,372),] ##based on lm_month results
lm_us_frk_modified_month<-lm(formula = cumecs_jrc~cumecs_colorado+month.x,data=us_frk_modified2)
summary(lm_us_frk_modified_month)
plot(lm_us_frk_modified_month)

us_frk_modified2<-us_frk%>%filter(sno!=61 & sno!=62 & sno!=310) ##based on lm_month results
lm_us_frk_modified2_month<-lm(formula = cumecs_jrc~cumecs_colorado+month.x,data=us_frk_modified2)
summary(lm_us_frk_modified2_month)
plot(lm_us_frk_modified2_month)

##unweighted subsampling and lm for us_frk
set.seed(5460)
us_frk_list_unweighted <- list()
for(i in 1:1000){
  us_frk_sample_unweighted<-us_frk[sample(nrow(us_frk), 100), c(1,4,7,2,6,9,13)]
  us_frk_list_unweighted[[i]]<-us_frk_sample_unweighted
}

us_frk_lm_list_unweighted <- list()
r_squared_us_frk_unweighted<-data.frame(r.squared=double(),
                                        adj.r.squared=double())
                                        
for(i in 1:length(us_frk_list_unweighted)){
  us_frk_lm_list_unweighted[[i]]<-lm(formula = cumecs_jrc~cumecs_colorado+month.x,data=us_frk_list_unweighted[[i]])
  r_squared_us_frk_unweighted[i,]<-data.frame(summary(us_frk_lm_list_unweighted[[i]])[c("r.squared","adj.r.squared")])
}

freq_rsquared_us_frk_unweighted<-data.frame(table(cut(r_squared_us_frk_unweighted$adj.r.squared,breaks=seq(0,1,by=0.1))))
print.xtable(xtable(freq_rsquared_us_frk_unweighted), type="html", file="freq_rsquared_us_frk_unweighted.html", include.rownames = FALSE)

##weighted subsampling and lm for us_frk
freq_us_frk_colorado<-data.frame(table(cut(us_frk$cumecs_colorado,breaks=10)))
freq_us_frk_colorado$relative_freq<-freq_us_frk_colorado$Freq/sum(freq_us_frk_colorado$Freq)
freq_us_frk_colorado$weight<-0.1/freq_us_frk_colorado$relative_freq

us_frk<-us_frk%>%mutate(weight= case_when(cumecs_colorado>518 & cumecs_colorado<3.76e+03~freq_us_frk_colorado$weight[1],
                                                cumecs_colorado>3.76e+03 & cumecs_colorado<6.98e+03~freq_us_frk_colorado$weight[2],
                                                cumecs_colorado>6.98e+03 & cumecs_colorado<1.02e+04~freq_us_frk_colorado$weight[3],
                                                cumecs_colorado>1.02e+04 & cumecs_colorado<1.34e+04~freq_us_frk_colorado$weight[4],
                                                cumecs_colorado>1.34e+04 & cumecs_colorado<1.66e+04~freq_us_frk_colorado$weight[5],
                                                cumecs_colorado>1.66e+04 & cumecs_colorado<1.98e+04~freq_us_frk_colorado$weight[6],
                                                cumecs_colorado>1.98e+04 & cumecs_colorado<2.3e+04~freq_us_frk_colorado$weight[7],
                                                cumecs_colorado>2.3e+04 & cumecs_colorado<2.63e+04~freq_us_frk_colorado$weight[8],
                                                cumecs_colorado>2.63e+04 & cumecs_colorado<2.95e+04~freq_us_frk_colorado$weight[9],
                                                TRUE~freq_us_frk_colorado$weight[10]))

set.seed(6460)
us_frk_list_weighted <- list()
for(i in 1:1000){
  us_frk_sample_weighted<-us_frk[sample(nrow(us_frk), 100, prob=us_frk$weight), c(1,4,7,2,6,9,13)]
  us_frk_list_weighted[[i]]<-us_frk_sample_weighted
}

us_frk_lm_list_weighted <- list()
r_squared_us_frk_weighted<-data.frame(r.squared=double(),adj.r.squared=double())
for(i in 1:length(us_frk_list_weighted)){
  us_frk_lm_list_weighted[[i]]<-lm(formula = cumecs_jrc~cumecs_colorado+month.x,data=us_frk_list_weighted[[i]])
  r_squared_us_frk_weighted[i,]<-data.frame(summary(us_frk_lm_list_weighted[[i]])[c("r.squared","adj.r.squared")])
}

freq_rsquared_us_frk_weighted<-data.frame(table(cut(r_squared_us_frk_weighted$adj.r.squared,breaks=seq(0,1,by=0.1))))
print.xtable(xtable(freq_rsquared_us_frk_weighted), type="html", file="freq_rsquared_us_frk_weighted.html", include.rownames = FALSE)

##min-max normalization to generate non-zero weights
us_frk$normalized_weight<-(us_frk$weight-min(us_frk$weight))/(max(us_frk$weight)-min(us_frk$weight))

##weighted subsampling using normalized_weight
set.seed(6459)
us_frk_list_norweighted <- list()
for(i in 1:1000){
  us_frk_sample_norweighted<-us_frk[sample(nrow(us_frk), 100, prob=us_frk$normalized_weight), c(1,4,7,2,6,9,13)]
  us_frk_list_norweighted[[i]]<-us_frk_sample_norweighted
}

us_frk_lm_list_norweighted <- list()
r_squared_us_frk_norweighted<-data.frame(r.squared=double(),adj.r.squared=double())
for(i in 1:length(us_frk_list_norweighted)){
  us_frk_lm_list_norweighted[[i]]<-lm(formula = cumecs_jrc~cumecs_colorado+month.x,data=us_frk_list_norweighted[[i]])
  r_squared_us_frk_norweighted[i,]<-data.frame(summary(us_frk_lm_list_norweighted[[i]])[c("r.squared","adj.r.squared")])
}

freq_rsquared_us_frk_norweighted<-data.frame(table(cut(r_squared_us_frk_norweighted$adj.r.squared,breaks=seq(0,1,by=0.1))))
print.xtable(xtable(freq_rsquared_us_frk_norweighted), type="html", file="freq_rsquared_us_frk_nor_weighted.html", include.rownames = FALSE)

############------------------------------------------
##Caliberation regression for ds_frk_hrdg
plot(ds_frk_hrdg$cumecs_jrc~ds_frk_hrdg$cumecs_colorado)
monthwise_dsfrk_hrdg<-ggplot(data=ds_frk_hrdg,aes(x=cumecs_colorado,y=cumecs_jrc))+
  geom_point()+
  facet_wrap(~month.x)

##Models for ds_frk_hrdg
lm_dsfrk_hrdg1<-lm(formula = cumecs_jrc~cumecs_colorado,data=ds_frk_hrdg)
summary(lm_dsfrk_hrdg1)

lm_dsfrk_hrdg_month<-lm(formula = cumecs_jrc~cumecs_colorado+month.x,data=ds_frk_hrdg)
summary(lm_dsfrk_hrdg_month)
plot(lm_dsfrk_hrdg_month)

lm_dsfrk_hrdg_month2<-lm(formula = cumecs_jrc~cumecs_colorado*month.x,data=ds_frk_hrdg)
summary(lm_dsfrk_hrdg_month2)
plot(lm_dsfrk_hrdg_month2) ##no significant interaction effect

##Modifying after removing outliers
dsfrk_hrdg_modified<-ds_frk_hrdg[-c(151,176,367),] ##based on lm_month results
lm_dsfrk_hrdg_modified_month<-lm(formula = cumecs_jrc~cumecs_colorado+month.x,data=dsfrk_hrdg_modified)
summary(lm_dsfrk_hrdg_modified_month)
plot(lm_dsfrk_hrdg_modified_month)

lm_dsfrk_hrdg_modified_month2<-lm(formula = cumecs_jrc~cumecs_colorado*month.x,data=dsfrk_hrdg_modified)
summary(lm_dsfrk_hrdg_modified_month2)
plot(lm_dsfrk_hrdg_modified_month2)

##unweighted subsampling and lm for ds_frk_hrdg
set.seed(5360)
dsfrk_hrdg_list_unweighted <- list()
for(i in 1:1000){
  dsfrk_hrdg_sample_unweighted<-ds_frk_hrdg[sample(nrow(ds_frk_hrdg), 100), c(1,4,7,2,6,9,13)]
  dsfrk_hrdg_list_unweighted[[i]]<-dsfrk_hrdg_sample_unweighted
}

dsfrk_hrdg_lm_list_unweighted <- list()
r_squared_dsfrk_hrdg_unweighted<-data.frame(r.squared=double(),
                                        adj.r.squared=double())

for(i in 1:length(dsfrk_hrdg_list_unweighted)){
  dsfrk_hrdg_lm_list_unweighted[[i]]<-lm(formula = cumecs_jrc~cumecs_colorado+month.x,data=dsfrk_hrdg_list_unweighted[[i]])
  r_squared_dsfrk_hrdg_unweighted[i,]<-data.frame(summary(dsfrk_hrdg_lm_list_unweighted[[i]])[c("r.squared","adj.r.squared")])
}

freq_rsquared_dsfrk_hrdg_unweighted<-data.frame(table(cut(r_squared_dsfrk_hrdg_unweighted$adj.r.squared,breaks=seq(0,1,by=0.1))))
print.xtable(xtable(freq_rsquared_dsfrk_hrdg_unweighted), type="html", file="freq_rsquared_dsfrk_hrdg_unweighted.html", include.rownames = FALSE)

##weighted subsampling and lm for ds_frk_hrdg
freq_dsfrk_hrdg_colorado<-data.frame(table(cut(ds_frk_hrdg$cumecs_colorado,breaks=10)))
freq_dsfrk_hrdg_colorado$relative_freq<-freq_dsfrk_hrdg_colorado$Freq/sum(freq_dsfrk_hrdg_colorado$Freq)
freq_dsfrk_hrdg_colorado$weight<-0.1/freq_dsfrk_hrdg_colorado$relative_freq

ds_frk_hrdg<-ds_frk_hrdg%>%mutate(weight= case_when(cumecs_colorado>-24.5 & cumecs_colorado<3.06e+03~freq_dsfrk_hrdg_colorado$weight[1],
                                          cumecs_colorado>3.06e+03 & cumecs_colorado<6.11e+03~freq_dsfrk_hrdg_colorado$weight[2],
                                          cumecs_colorado>6.11e+03 & cumecs_colorado<9.16e+03~freq_dsfrk_hrdg_colorado$weight[3],
                                          cumecs_colorado>9.16e+03 & cumecs_colorado<1.22e+04~freq_dsfrk_hrdg_colorado$weight[4],
                                          cumecs_colorado>1.22e+04 & cumecs_colorado<1.53e+04~freq_dsfrk_hrdg_colorado$weight[5],
                                          cumecs_colorado>1.53e+04 & cumecs_colorado<1.83e+04~freq_dsfrk_hrdg_colorado$weight[6],
                                          cumecs_colorado>1.83e+04 & cumecs_colorado<2.14e+04~freq_dsfrk_hrdg_colorado$weight[7],
                                          cumecs_colorado>2.14e+04 & cumecs_colorado<2.44e+04~freq_dsfrk_hrdg_colorado$weight[8],
                                          cumecs_colorado>2.44e+04 & cumecs_colorado<2.75e+04~freq_dsfrk_hrdg_colorado$weight[9],
                                          TRUE~freq_dsfrk_hrdg_colorado$weight[10]))

set.seed(6360)
dsfrk_hrdg_list_weighted <- list()
for(i in 1:1000){
  dsfrk_hrdg_sample_weighted<-ds_frk_hrdg[sample(nrow(ds_frk_hrdg), 100, prob=ds_frk_hrdg$weight), c(1,4,7,2,6,9,13)]
  dsfrk_hrdg_list_weighted[[i]]<-dsfrk_hrdg_sample_weighted
}

dsfrk_hrdg_lm_list_weighted <- list()
r_squared_dsfrk_hrdg_weighted<-data.frame(r.squared=double(),adj.r.squared=double())
for(i in 1:length(dsfrk_hrdg_list_weighted)){
  dsfrk_hrdg_lm_list_weighted[[i]]<-lm(formula = cumecs_jrc~cumecs_colorado+month.x,data=dsfrk_hrdg_list_weighted[[i]])
  r_squared_dsfrk_hrdg_weighted[i,]<-data.frame(summary(dsfrk_hrdg_lm_list_weighted[[i]])[c("r.squared","adj.r.squared")])
}

freq_rsquared_dsfrk_hrdg_weighted<-data.frame(table(cut(r_squared_dsfrk_hrdg_weighted$adj.r.squared,breaks=seq(0,1,by=0.1))))
print.xtable(xtable(freq_rsquared_dsfrk_hrdg_weighted), type="html", file="freq_rsquared_dsfrk_hrdg_weighted.html", include.rownames = FALSE)

##min-max normalization to generate non-zero weights
ds_frk_hrdg$normalized_weight<-(ds_frk_hrdg$weight-min(ds_frk_hrdg$weight))/(max(ds_frk_hrdg$weight)-min(ds_frk_hrdg$weight))

##weighted subsampling using normalized_weight
set.seed(6359)
dsfrk_hrdg_list_norweighted <- list()
for(i in 1:1000){
  dsfrk_hrdg_sample_norweighted<-ds_frk_hrdg[sample(nrow(ds_frk_hrdg), 100, prob=ds_frk_hrdg$normalized_weight), c(1,4,7,2,6,9,13)]
  dsfrk_hrdg_list_norweighted[[i]]<-dsfrk_hrdg_sample_norweighted
}

dsfrk_hrdg_lm_list_norweighted <- list()
r_squared_dsfrk_hrdg_norweighted<-data.frame(r.squared=double(),adj.r.squared=double())
for(i in 1:length(dsfrk_hrdg_list_norweighted)){
  dsfrk_hrdg_lm_list_norweighted[[i]]<-lm(formula = cumecs_jrc~cumecs_colorado+month.x,data=dsfrk_hrdg_list_norweighted[[i]])
  r_squared_dsfrk_hrdg_norweighted[i,]<-data.frame(summary(dsfrk_hrdg_lm_list_norweighted[[i]])[c("r.squared","adj.r.squared")])
}

freq_rsquared_dsfrk_hrdg_norweighted<-data.frame(table(cut(r_squared_dsfrk_hrdg_norweighted$adj.r.squared,breaks=seq(0,1,by=0.1))))
print.xtable(xtable(freq_rsquared_dsfrk_hrdg_norweighted), type="html", file="freq_rsquared_dsfrk_hrdg_nor_weighted.html", include.rownames = FALSE)

############------------------------------------------
##Caliberation regression for ds_frk_BD
plot(ds_frk_BD$cumecs_jrc~ds_frk_BD$cumecs_colorado)
monthwise_dsfrk_BD<-ggplot(data=ds_frk_BD,aes(x=cumecs_colorado,y=cumecs_jrc))+
  geom_point()+
  facet_wrap(~month.x)

##Models for ds_frk_hrdg
lm_dsfrk_BD1<-lm(formula = cumecs_jrc~cumecs_colorado,data=ds_frk_BD)
summary(lm_dsfrk_BD1)

lm_dsfrk_BD2<-lm(formula = cumecs_jrc~cumecs_colorado+I(cumecs_colorado^2),data=ds_frk_BD)
summary(lm_dsfrk_BD2)

lm_dsfrk_BD_month<-lm(formula = cumecs_jrc~cumecs_colorado+month.x,data=ds_frk_BD)
summary(lm_dsfrk_BD_month)
plot(lm_dsfrk_BD_month)

lm_dsfrk_BD_month2<-lm(formula = cumecs_jrc~cumecs_colorado*month.x,data=ds_frk_BD)
summary(lm_dsfrk_BD_month2)
plot(lm_dsfrk_hrdg_month2) ##no significant interaction effect

lm_dsfrk_BD_month3<-lm(formula = cumecs_jrc~cumecs_colorado+I(cumecs_colorado^2)+month.x,data=ds_frk_BD)
summary(lm_dsfrk_BD_month3)
plot(lm_dsfrk_BD_month3)

##Modifying after removing outliers
dsfrk_BD_modified<-ds_frk_BD[-c(151,176,352),] ##based on lm_month results
lm_dsfrk_BD_modified_month<-lm(formula = cumecs_jrc~cumecs_colorado+month.x,data=dsfrk_BD_modified)
summary(lm_dsfrk_BD_modified_month)
plot(lm_dsfrk_BD_modified_month)

lm_dsfrk_hrdg_modified_month2<-lm(formula = cumecs_jrc~cumecs_colorado*month.x,data=dsfrk_BD_modified)
summary(lm_dsfrk_hrdg_modified_month2)
plot(lm_dsfrk_hrdg_modified_month2)

##unweighted subsampling and lm for ds_frk_BD
set.seed(5260)
dsfrk_BD_list_unweighted <- list()
for(i in 1:1000){
  dsfrk_BD_sample_unweighted<-ds_frk_BD[sample(nrow(ds_frk_BD), 100), c(1,4,7,2,6,9,13)]
  dsfrk_BD_list_unweighted[[i]]<-dsfrk_BD_sample_unweighted
}

dsfrk_BD_lm_list_unweighted <- list()
r_squared_dsfrk_BD_unweighted<-data.frame(r.squared=double(),
                                            adj.r.squared=double())

for(i in 1:length(dsfrk_BD_list_unweighted)){
  dsfrk_BD_lm_list_unweighted[[i]]<-lm(formula = cumecs_jrc~cumecs_colorado+month.x,data=dsfrk_BD_list_unweighted[[i]])
  r_squared_dsfrk_BD_unweighted[i,]<-data.frame(summary(dsfrk_BD_lm_list_unweighted[[i]])[c("r.squared","adj.r.squared")])
}

freq_rsquared_dsfrk_BD_unweighted<-data.frame(table(cut(r_squared_dsfrk_BD_unweighted$adj.r.squared,breaks=seq(0,1,by=0.1))))
print.xtable(xtable(freq_rsquared_dsfrk_BD_unweighted), type="html", file="freq_rsquared_dsfrk_BD_unweighted.html", include.rownames = FALSE)

##weighted subsampling and lm for ds_frk_hrdg
freq_dsfrk_BD_colorado<-data.frame(table(cut(ds_frk_BD$cumecs_colorado,breaks=10)))
freq_dsfrk_BD_colorado$relative_freq<-freq_dsfrk_BD_colorado$Freq/sum(freq_dsfrk_BD_colorado$Freq)
freq_dsfrk_BD_colorado$weight<-0.1/freq_dsfrk_BD_colorado$relative_freq

ds_frk_BD<-ds_frk_BD%>%mutate(weight= case_when(cumecs_colorado>-24.5 & cumecs_colorado<3.06e+03~freq_dsfrk_BD_colorado$weight[1],
                                                    cumecs_colorado>3.06e+03 & cumecs_colorado<6.11e+03~freq_dsfrk_BD_colorado$weight[2],
                                                    cumecs_colorado>6.11e+03 & cumecs_colorado<9.16e+03~freq_dsfrk_BD_colorado$weight[3],
                                                    cumecs_colorado>9.16e+03 & cumecs_colorado<1.22e+04~freq_dsfrk_BD_colorado$weight[4],
                                                    cumecs_colorado>1.22e+04 & cumecs_colorado<1.53e+04~freq_dsfrk_BD_colorado$weight[5],
                                                    cumecs_colorado>1.53e+04 & cumecs_colorado<1.83e+04~freq_dsfrk_BD_colorado$weight[6],
                                                    cumecs_colorado>1.83e+04 & cumecs_colorado<2.14e+04~freq_dsfrk_BD_colorado$weight[7],
                                                    cumecs_colorado>2.14e+04 & cumecs_colorado<2.44e+04~freq_dsfrk_BD_colorado$weight[8],
                                                    cumecs_colorado>2.44e+04 & cumecs_colorado<2.75e+04~freq_dsfrk_BD_colorado$weight[9],
                                                    TRUE~freq_dsfrk_BD_colorado$weight[10]))

set.seed(6260)
dsfrk_BD_list_weighted <- list()
for(i in 1:1000){
  dsfrk_BD_sample_weighted<-ds_frk_BD[sample(nrow(ds_frk_BD), 100, prob=ds_frk_BD$weight), c(1,4,7,2,6,9,13)]
  dsfrk_BD_list_weighted[[i]]<-dsfrk_BD_sample_weighted
}

dsfrk_BD_lm_list_weighted <- list()
r_squared_dsfrk_BD_weighted<-data.frame(r.squared=double(),adj.r.squared=double())
for(i in 1:length(dsfrk_BD_list_weighted)){
  dsfrk_BD_lm_list_weighted[[i]]<-lm(formula = cumecs_jrc~cumecs_colorado+month.x,data=dsfrk_BD_list_weighted[[i]])
  r_squared_dsfrk_BD_weighted[i,]<-data.frame(summary(dsfrk_BD_lm_list_weighted[[i]])[c("r.squared","adj.r.squared")])
}

freq_rsquared_dsfrk_BD_weighted<-data.frame(table(cut(r_squared_dsfrk_BD_weighted$adj.r.squared,breaks=seq(0,1,by=0.1))))
print.xtable(xtable(freq_rsquared_dsfrk_BD_weighted), type="html", file="freq_rsquared_dsfrk_BD_weighted.html", include.rownames = FALSE)

##min-max normalization to generate non-zero weights
ds_frk_BD$normalized_weight<-(ds_frk_BD$weight-min(ds_frk_BD$weight))/(max(ds_frk_BD$weight)-min(ds_frk_BD$weight))

##weighted subsampling using normalized_weight
set.seed(6259)
dsfrk_BD_list_norweighted <- list()
for(i in 1:1000){
  dsfrk_BD_sample_norweighted<-ds_frk_BD[sample(nrow(ds_frk_BD), 100, prob=ds_frk_BD$normalized_weight), c(1,4,7,2,6,9,13)]
  dsfrk_BD_list_norweighted[[i]]<-dsfrk_BD_sample_norweighted
}

dsfrk_BD_lm_list_norweighted <- list()
r_squared_dsfrk_BD_norweighted<-data.frame(r.squared=double(),adj.r.squared=double())
for(i in 1:length(dsfrk_BD_list_norweighted)){
  dsfrk_BD_lm_list_norweighted[[i]]<-lm(formula = cumecs_jrc~cumecs_colorado+month.x,data=dsfrk_BD_list_norweighted[[i]])
  r_squared_dsfrk_BD_norweighted[i,]<-data.frame(summary(dsfrk_BD_lm_list_norweighted[[i]])[c("r.squared","adj.r.squared")])
}

freq_rsquared_dsfrk_BD_norweighted<-data.frame(table(cut(r_squared_dsfrk_BD_norweighted$adj.r.squared,breaks=seq(0,1,by=0.1))))
print.xtable(xtable(freq_rsquared_dsfrk_BD_norweighted), type="html", file="freq_rsquared_dsfrk_BD_nor_weighted.html", include.rownames = FALSE)


