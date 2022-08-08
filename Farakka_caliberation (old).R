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

plot(us_frk$cumecs_jrc~us_frk$cumecs_colorado)
monthwise_us_frk<-ggplot(data=us_frk,aes(x=cumecs_colorado,y=cumecs_jrc))+
  geom_point()+
  facet_wrap(~month.x)
  
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

##Modifying after removing outliers
which(us_frk$JRC_colorado_ratio>1) 
us_frk_modified<-us_frk[-c(289,338,365),] ##based on jrc colorado ratio

lm_us_frk_modified_month<-lm(formula = cumecs_jrc~cumecs_colorado+month.x,data=us_frk_modified)
summary(lm_us_frk_modified_month)
plot(lm_us_frk_modified_month)

us_frk_modified2<-us_frk[-c(151,176,352,372),] ##based on lm_month results
lm_us_frk_modified2_month<-lm(formula = cumecs_jrc~cumecs_colorado+month.x,data=us_frk_modified2)
summary(lm_us_frk_modified2_month)
plot(lm_us_frk_modified2_month)

############------------------------------------------
##Weighted subsampling based on cumecs_colorado 
freq_cumecs_colorado<-data.frame(table(cut(merged_df$cumecs_colorado,breaks=10)))
freq_cumecs_colorado$relative_freq<-freq_cumecs_colorado$Freq/sum(freq_cumecs_colorado$Freq)
freq_cumecs_colorado$weight<-0.1/freq_cumecs_colorado$relative_freq ##0.1 is inclusion prob of each of the 10 bins

merged_df<-merged_df%>%mutate(weight= case_when(cumecs_colorado>-26.7 & cumecs_colorado<3270~freq_cumecs_colorado$weight[1],
                              cumecs_colorado>3270 & cumecs_colorado<6540~freq_cumecs_colorado$weight[2],
                              cumecs_colorado>6540 & cumecs_colorado<9810~freq_cumecs_colorado$weight[3],
                              cumecs_colorado>9810 & cumecs_colorado<13100~freq_cumecs_colorado$weight[4],
                              cumecs_colorado>13100 & cumecs_colorado<16300~freq_cumecs_colorado$weight[5],
                              cumecs_colorado>16300 & cumecs_colorado<19600~freq_cumecs_colorado$weight[6],
                              cumecs_colorado>19600 & cumecs_colorado<22900~freq_cumecs_colorado$weight[7],
                              cumecs_colorado>22900 & cumecs_colorado<26200~freq_cumecs_colorado$weight[8],
                              cumecs_colorado>26200 & cumecs_colorado<29400~freq_cumecs_colorado$weight[9],
                              TRUE~freq_cumecs_colorado$weight[10]
                              ))

##min-max normalization to generate non-zero weights
merged_df$normalized_weight<-(merged_df$weight-min(merged_df$weight))/(max(merged_df$weight)-min(merged_df$weight))

##Drawing 1000 samples (sample size=100; weighted) from merged_df
##using normalized_weight
set.seed(8461)
df_list_nor_weighted <- list()##creating empty list
for(i in 1:1000){
  df_sample_nor_weighted<-merged_df[sample(nrow(merged_df), 100, prob=merged_df$normalized_weight), c(1,4,7,2,6,9,13)]
  df_list_nor_weighted[[i]]<-df_sample_nor_weighted
}

##lm models on the df_list_weighted
lm_month_list_nor_weighted <- list()
for(i in 1:length(df_list_nor_weighted)){
  lm_month_list_nor_weighted[[i]]<-lm(formula = cumecs_jrc~cumecs_colorado+month.x,data=df_list_nor_weighted[[i]])
  r_squared_df_nor_weighted[i,]<-data.frame(summary(lm_month_list_nor_weighted[[i]])[c("r.squared","adj.r.squared")])
}

freq_rsquared_nor_weighted<-data.frame(table(cut(r_squared_df_nor_weighted$adj.r.squared,breaks=seq(0,1,by=0.1))))

freq_rsquared_nor_weighted_xtable<-xtable(freq_rsquared_nor_weighted)
print.xtable(freq_rsquared_nor_weighted_xtable, type="html", file="freq_rsquared_normalized_weighted.html", include.rownames = FALSE)

##Drawing 1000 samples (sample size=100; weighted) from merged_df
set.seed(5461)
df_list_weighted <- list()##creating empty list
for(i in 1:1000){
  df_sample_weighted<-merged_df[sample(nrow(merged_df), 100, prob=merged_df$weight), c(1,4,7,2,6,9,13)]
  df_list_weighted[[i]]<-df_sample_weighted
}

##lm models on the df_list_weighted
lm_month_list_weighted <- list()
for(i in 1:length(df_list_weighted)){
  lm_month_list_weighted[[i]]<-lm(formula = cumecs_jrc~cumecs_colorado+month.x,data=df_list_weighted[[i]])
  r_squared_df_weighted[i,]<-data.frame(summary(lm_month_list_weighted[[i]])[c("r.squared","adj.r.squared")])
}

freq_rsquared_weighted<-data.frame(table(cut(r_squared_df_weighted$adj.r.squared,breaks=seq(0,1,by=0.1))))

freq_rsquared_weighted_xtable<-xtable(freq_rsquared_weighted)
print.xtable(freq_rsquared_weighted_xtable, type="html", file="freq_rsquared_weighted.html", include.rownames = FALSE)

##Subsampling method of linear regression (July 25, 2022)
##SUE not working out
##Using full merged dataset (without removing outliers)
sue.lm<-SUE.lm(cumecs_jrc~cumecs_colorado+month.x,data=merged_df,k=1000,ns=100,
                      consistency.check=TRUE,constant=0.25)

##Drawing 1000 random samples (sample size=100) from merged_df
set.seed(3461)
df_list <- list()##creating empty list
for(i in 1:1000){
  df_sample<-merged_df[sample(nrow(merged_df), 100), c(1,4,7,2,6,9,13)]
  df_list[[i]]<-df_sample
}

##lm models on the df_list
lm_month_list <- list()
for(i in 1:length(df_list)){
  lm_month_list[[i]]<-lm(formula = cumecs_jrc~cumecs_colorado+month.x,data=df_list[[i]])
  r_squared_df[i,]<-data.frame(summary(lm_month_list[[i]])[c("r.squared","adj.r.squared")])
}

length(which(r_squared_df$adj.r.squared>0.45))

freq_rsquare_unweighted_full<-data.frame(table(cut(r_squared_df$adj.r.squared,breaks=seq(0,1,by=0.1))))

freq_rsquare_unweighted_full_xtable<-xtable(freq_rsquare_unweighted_full)
print.xtable(freq_rsquare_unweighted_full_xtable, type="html", file="freq_rsquared_unweighted_full.html", include.rownames = FALSE)

##lm model:'cumecs_jrc~cumecs_colorado+month.x'to identify outliers
lm_month<-lm(formula = cumecs_jrc~cumecs_colorado+month.x,data=merged_df)
summary(lm_month)
plot(lm_month)
boxplot(lm_month$residuals)
boxplot.stats(lm_month$residuals)$out ##identifying outliers

lm_month_location<-lm(formula = cumecs_jrc~cumecs_colorado+month.x+location,data=merged_df)
summary(lm_month_location)

lm_month_us<-lm(formula = cumecs_jrc~cumecs_colorado+month.x,data=merged_df,subset=(location=="flow_us_frk"))
summary(lm_month_us)

##Modified dataset after removing 19 outliers from above step
merged_df_modified<-merged_df[-c(150,151,152,153,156,195,225,
                                 522,523,524,525,675,724,900,
                                 1050,1056,1062,1100,1116),]
lm_month_modified<-lm(formula = cumecs_jrc~cumecs_colorado+month.x,data=merged_df_modified)
summary(lm_month_modified)
plot(lm_month_modified)

merged_df_modified2<-merged_df_modified[-c(15,454,455),]
lm_month_modified2<-lm(formula = cumecs_jrc~cumecs_colorado+month.x,data=merged_df_modified2)
summary(lm_month_modified2)
plot(lm_month_modified2)

##Removing outliers based on JRC/colorado ratio 
merged_df_modified3<-merged_df[-c(205,206,811,812),]
hist(merged_df_modified3$JRC_colorado_prop)
boxplot.stats(merged_df_modified3$JRC_colorado_prop)$out

lm_month_modified3<-lm(formula = cumecs_jrc~cumecs_colorado+month.x,data=merged_df_modified3)
summary(lm_month_modified3)
plot(lm_month_modified3)

##and outliers from previous lm i.e 151, 152, 153
merged_df_modified4<-merged_df[-c(205,206,811,812,151,152,153),]
lm_month_modified4<-lm(formula = cumecs_jrc~cumecs_colorado+month.x,data=merged_df_modified4)
summary(lm_month_modified4)
plot(lm_month_modified4)

##Using merged_df_modified to generate 1000 new samples
set.seed(4461)
df_list_modified <- list()##creating empty list
for(i in 1:1000){
  df_sample_modified<-merged_df_modified[sample(nrow(merged_df_modified), 100), c(1,4,7,2,6,9,13)]
  df_list_modified[[i]]<-df_sample_modified
}

##lm models on the df_list
lm_month_list_modified <- list()
for(i in 1:length(df_list_modified)){
  lm_month_list_modified[[i]]<-lm(formula = cumecs_jrc~cumecs_colorado+month.x,data=df_list_modified[[i]])
  r_squared_df_modified[i,]<-data.frame(summary(lm_month_list_modified[[i]])[c("r.squared","adj.r.squared")])
}

freq_rsquare_unweighted_modified<-data.frame(table(cut(r_squared_df_modified$adj.r.squared,breaks=seq(0,1,by=0.1))))

freq_rsquare_unweighted_modified_xtable<-xtable(freq_rsquare_unweighted_modified)
print.xtable(freq_rsquare_unweighted_modified_xtable, type="html", file="freq_rsquared_unweighted_modified.html", include.rownames = FALSE)

############------------------------------------------
##Graphical summary (July 27, 2022)
summary_graph_jrc<-merged_df%>%dplyr::select(date_formatted,location,month.x,year.x,discharge=cumecs_jrc)
summary_graph_jrc$source<-rep("JRC",times=nrow(summary_graph_jrc))
summary_graph_colorado<-merged_df%>%dplyr::select(date_formatted,location,month.x,year.x,discharge=cumecs_colorado)
summary_grasummary_graph<-ph_colorado$source<-rep("Riverwatch-Colorado",times=nrow(summary_graph_colorado))
summary_graph<-rbind(summary_graph_jrc,summary_graph_colorado)

summary_graph<-summary_graph[order(summary_graph$date_formatted), ] 
summary_graph$month_day<-format(as.Date(summary_graph$date_formatted, format="%Y-%m-%d"),"%m-%d")

summary_plot<-ggplot(data = summary_graph,aes(x=month_day,y=discharge))+
  geom_point(aes(color=source,shape=location),size=2,alpha=0.8)+
  scale_color_manual(values=c("Riverwatch-Colorado"="#E69F00",
                                "JRC"="#56B4E9"))+
  facet_wrap(~year.x)+
  labs(x="Date (month-day)",y="Discharge (cubic metre per second)",color="Source",shape="Location")+
  theme_bw()+
  theme(legend.position="bottom",axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1))
  
ggsave("Summary_with_location_27.07.22.png", plot=summary_plot,width=40,height=40,unit="cm",dpi=600)    

############------------------------------------------
##Other lm models besides cumecs_jrc~cumecs_colorado+month.x
##Run on full dataset
lm1<-lm(formula = cumecs_jrc~cumecs_colorado,data=merged_df)
summary(lm)
plot(lm)

lm2<-lm(formula = cumecs_jrc~cumecs_colorado+I(cumecs_colorado^2),data=merged_df)
summary(lm2)

##Log-transformation on dependent/independent variables
lm_log<-lm(formula=log(cumecs_jrc)~cumecs_colorado,data=merged_df)
summary(lm_log)
plot(lm_log)
lm_log2<-lm(formula=cumecs_jrc~log(cumecs_colorado),data=merged_df)
summary(lm_log2)
plot(lm_log2)
lm_log3<-lm(formula=log(cumecs_jrc)~log(cumecs_colorado),data=merged_df)
summary(lm_log3)
plot(lm_log3)

lm_log4<-lm(formula=log(cumecs_jrc)~cumecs_colorado+month.x,data=merged_df)
summary(lm_log4)
plot(lm_log4)

lm_log5<-lm(formula=log(cumecs_jrc)~cumecs_colorado*month.x,data=merged_df)
summary(lm_log5)
plot(lm_log5)

##df_calib=Feb+April for caliberation---------------
df_calib<-merged_df%>%filter(month.x=="February"|month.x=="April")
plot<-ggplot(data = df_calib,aes(x=cumecs_colorado,y=log(cumecs_jrc)))+
      geom_point()+
      labs(x="Cumecs - colorado", y="log(Cumecs - jrc)")

ggsave("Feb_April_log_18.07.22.png", plot=plot,width=35,height=25,unit="cm",dpi=600)    

##lm model for df_calib
log_df_calib<-lm(formula=log(cumecs_jrc)~cumecs_colorado,data=df_calib)
summary(log_df_calib)

plot(lm_df_calib)

##nls model using power function for df_calib
nls_power<-nls(cumecs_jrc ~ NLS.powerCurve(cumecs_colorado, a, b),
                        data = df_calib)
summary(nls_power)
R2nls(nls_power)$PseudoR2
df_calib$nls_power_predicted<-predict(nls_power)

##plotting predicted values from nls_power
nls_power_fig<-ggplot(df_calib,aes(x=cumecs_colorado,y=cumecs_jrc))+
  geom_point(color="#4E84C4",size=0.9)+
  geom_line(aes(y=nls_power_predicted),color="red",size=0.9)

ggsave("nls_power_fig.png", plot=nls_power_fig,width=35,height=25,unit="cm",dpi=600)    

##nls model using power function for df_calib, with starting values
nls_power2<-nls(formula=cumecs_jrc ~ a + b * I(cumecs_colorado^z), start = list(a = 1, b = 1, z = 1), data = df_calib)
summary(nls_power2)
df_calib$nls_power2_predicted<-predict(nls_power2)

nls_power_fig_comparison1<-ggplot(df_calib,aes(x=cumecs_colorado,y=cumecs_jrc))+
  geom_point(color="#4E84C4",size=0.9)+
  geom_line(aes(y=nls_power_predicted),color="red",size=0.9)+ #red - selstart model
  geom_line(aes(y=nls_power2_predicted),color="green3",size=0.9) # green - initial values provided 

ggsave("nls_power_comparison1.png", plot=nls_power_fig_comparison1,width=35,height=25,unit="cm",dpi=600)    

##exponential growth function for df_calib
nls_expo1<-nls(formula=cumecs_jrc ~ I(a * exp(b * cumecs_colorado)), data = df_calib, start = list(a = 400, b = 0))
summary(nls_expo1)
df_calib$nls_expo_predicted<-predict(nls_expo1)

nls_power_expo_comparison<-ggplot(df_calib,aes(x=cumecs_colorado,y=cumecs_jrc))+
  geom_point(color="#4E84C4",size=0.9)+
  geom_line(aes(y=nls_power_predicted),color="red",size=0.9)+
  geom_line(aes(y=nls_power2_predicted),color="green3",size=0.9)+
  geom_line(aes(y=nls_expo_predicted),color="magenta",size=0.9)
  
ggsave("nls_power_expo_comparison.png", plot=nls_power_expo_comparison,width=35,height=25,unit="cm",dpi=600)    

##Using Feb and April separately for caliberation---------------
##Models for Feb
df_calib_Feb<-df_calib%>%filter(month.x=="February")

##lm models for Feb; all show poor fit
lm_log_Feb<-lm(formula=log(cumecs_jrc)~cumecs_colorado,data=df_calib_Feb)
summary(lm_log_Feb)
plot(lm_log_Feb)
lm_log2_Feb<-lm(formula=cumecs_jrc~log(cumecs_colorado),data=df_calib_Feb)
summary(lm_log2_Feb)
plot(lm_log2_Feb)
lm_log3_Feb<-lm(formula=log(cumecs_jrc)~log(cumecs_colorado),data=df_calib_Feb)
summary(lm_log3_Feb)
plot(lm_log3_Feb)

##exponential growth function for Feb
nls_expo_Feb<-nls(formula=cumecs_jrc ~ I(a * exp(b * cumecs_colorado)), data = df_calib_Feb, start = list(a = 800, b = 0))
summary(nls_expo_Feb)
df_calib_Feb$nls_expo_Feb_predicted<-predict(nls_expo_Feb)

nls_expo_Feb<-ggplot(df_calib_Feb,aes(x=cumecs_colorado,y=cumecs_jrc))+
  geom_point(color="#4E84C4",size=0.9)+
  geom_line(aes(y=nls_expo_Feb_predicted),color="red",size=0.9)

ggsave("nls_expo_Feb.png", plot=nls_expo_Feb,width=35,height=25,unit="cm",dpi=600)    

##Models for April
##lm model for April
df_calib_April<-df_calib%>%filter(month.x=="April")
lm_April<-lm(formula=cumecs_jrc~cumecs_colorado, data=df_calib_April)
summary(lm_April)
par(mfrow=c(2,2))
plot(lm_April, which=c(1,2,3,5), cex.lab=1.5, cex.axis=1.3)
dev.off()
df_calib_April$lm_April_predicted<-predict(lm_April)
                   
##quadratic model for April
quad_April<-lm(formula=cumecs_jrc~cumecs_colorado+I(cumecs_colorado^2), data=df_calib_April)
summary(quad_April)
par(mfrow=c(2,2))
plot(quad_April, which=c(1,2,3,5), cex.lab=1.5, cex.axis=1.3)
dev.off()

lm_log_April<-lm(formula=log(cumecs_jrc)~cumecs_colorado,data=df_calib_April)
summary(lm_log_April)
lm_log2_April<-lm(formula=cumecs_jrc~log(cumecs_colorado),data=df_calib_April)
summary(lm_log2_April)
lm_log3_April<-lm(formula=log(cumecs_jrc)~log(cumecs_colorado),data=df_calib_April)
summary(lm_log3_April)

lm_April<-ggplot(df_calib_April,aes(x=cumecs_colorado,y=cumecs_jrc))+
  geom_point(color="#4E84C4",size=0.9)+
  geom_line(aes(y=lm_April_predicted),color="red",size=0.9)

ggsave("lm_April.png", plot=lm_April,width=35,height=25,unit="cm",dpi=600)    

##exponential growth function for April
nls_expo_April<-nls(formula=cumecs_jrc ~ I(a * exp(b * cumecs_colorado)), data = df_calib_April, start = list(a = 400, b = 0))
summary(nls_expo_April)
df_calib_April$nls_expo_April_predicted<-predict(nls_expo_April)

nls_expo_April<-ggplot(df_calib_April,aes(x=cumecs_colorado,y=cumecs_jrc))+
  geom_point(color="#4E84C4",size=0.9)+
  geom_line(aes(y=lm_April_predicted),color="red",size=0.9)+
  geom_line(aes(y=nls_expo_April_predicted),color="green3",size=0.9)

ggsave("nls_expo_April.png", plot=nls_expo_April,width=35,height=25,unit="cm",dpi=600)    

##Michaelis-Menten Model for April
nls_mm_April<-nls(cumecs_jrc ~ SSmicmen(cumecs_colorado, Vm, K),
               data = df_calib_April)
summary(nls_mm_April)

