# Load Useful Packages, Functions and Themes
source("core.R")

########################
# Gather the main data #
########################
load("ECI_data.RData")

####################
# Compile the Data #
####################

# Define groups of variables
egy_vars=c("wells","oil","rigs","oilprice","exports_energy")
biz_vars=c("mfg","durable","nondurable","mfgchem","mfgmetal","mfgfood","mfgmachinery","mfgpetro","trade","cfib","exports_nonenergy","self_emp")
lab_vars=c("employment_rate","employment","unemployment","unemp_men_prime","unemp_women_prime","unemp_duration","EIclaims","SEPH","SEPHprivate","SEPHgoods","SEPHservices","SEPHconstruct","hours","full_part_emp","under_unemp","private_emp","partrate")
con_vars=c("housing","earnings","retail","restaurant_spend","vehicles","trucks","MLS")

# Form Matrix of Main Data
variables<-c(egy_vars,biz_vars,lab_vars,con_vars)
ABdata<-data.frame(When=mfg$When)
for (v in variables){
  ABdata<-ABdata %>%
    left_join(get(v) %>% mutate(When=as.yearmon(When)),by="When")
}
ABdata<-ABdata %>%
  filter(When>="Jan 2001") %>%
  ts(frequency=12,start=c(2001,01))

# Convert nominal variables to real
for (v in c("mfg","trade","earnings","retail","durables","nondurables","restaurant_spend",
            "mfgchem","mfgmetal","mfgfood","mfgmachinery","mfgpetro","mls","exports_energy","exports_nonenergy")){
  ABdata[,v]<-ABdata[,v]/deflate
}

# Define YoY % Changes for most variables, level differences for rates/indexes/hours
nrows<-dim(ABdata)[1]
ncols<-dim(ABdata)[2]
change=log(ABdata[13:nrows,2:ncols])-log(ABdata[1:(nrows-12),2:ncols]) # Year-over-year changes
nvars<-dim(change)[2]
change[,"cfib"]<-ABdata[13:nrows,"cfib"]-ABdata[1:(nrows-12),"cfib"]
change[,"unemployment"]<-ABdata[13:nrows,"unemployment"]-ABdata[1:(nrows-12),"unemployment"]
change[,"fulltime_share"]<-ABdata[13:nrows,"fulltime_share"]-ABdata[1:(nrows-12),"fulltime_share"]
change[,"partrate"]<-ABdata[13:nrows,"partrate"]-ABdata[1:(nrows-12),"partrate"]
change[,"under_unemp"]<-ABdata[13:nrows,"under_unemp"]-ABdata[1:(nrows-12),"under_unemp"]
change[,"employment_rate"]<-ABdata[13:nrows,"employment_rate"]-ABdata[1:(nrows-12),"employment_rate"]
change[,"unemp_men_prime"]<-ABdata[13:nrows,"unemp_men_prime"]-ABdata[1:(nrows-12),"unemp_men_prime"]
change[,"unemp_women_prime"]<-ABdata[13:nrows,"unemp_women_prime"]-ABdata[1:(nrows-12),"unemp_women_prime"]
change[,"hours"]<-ABdata[13:nrows,"hours"]-ABdata[1:(nrows-12),"hours"]

# Convert to time-series object
change<-ts(change,frequency=12,start=c(2002,1))

# Estimate the first principle component
scaled_data<-scale(change,center=T,scale=T)
PCAtest<-prcomp(change,scale=T,center=T)
print(PCAtest)
summary(PCAtest)
predicted<-predict(PCAtest,newdata=change) # extracts the first principle component
weights<-as.numeric(PCAtest$rotation[,1])/sum(as.numeric(PCAtest$rotation[,1]))
temp<-scale(change,center=T,scale=T) %*% weights # extracts the first principle component
ABindex<-temp/sd(temp) # Rescale to give unit variance

#####################################
# Plot the Index and Its Components #
#####################################
n1<-length(egy_vars)
n2<-n1+length(biz_vars)
n3<-n2+length(lab_vars)
n4<-n3+length(con_vars)
group_oil<-scaled_data[,1:n1] %*% weights[1:n1] # extracts the first principle component
group_biz<-scaled_data[,(1+n1):n2] %*% weights[(1+n1):n2] # extracts the first principle component
group_lab<-scaled_data[,(1+n2):n3] %*% weights[(1+n2):n3] # extracts the first principle component
group_HHs<-scaled_data[,(1+n3):n4] %*% weights[(1+n3):n4] # extracts the first principle component
plotdata<-data.frame(Ref_Date=seq(as.yearmon("2002-01"),length.out=length(ABindex),by=1/12)) %>%
  cbind(ABindex) %>%
  cbind(Energy=group_oil/sd(temp)) %>%
  cbind(Business=group_biz/sd(temp)) %>%
  cbind(Labour=group_lab/sd(temp)) %>%
  cbind(Households=group_HHs/sd(temp)) %>%
  gather(group,index,-Ref_Date,-ABindex) %>%
  mutate(group=ifelse(group=="Labour","Labour Markets",group),
         group=ifelse(group=="Energy","Energy Sector",group),
         group=ifelse(group=="Business","Business Activity",group),
         group=ifelse(group=="Households","Households Income/Spending",group)) %>% 
  filter(Ref_Date>="Jan 2011")
ggplot(plotdata,aes(Ref_Date,index,group=group,fill=group))+
  geom_col(position="stack",color="white",size=0.1)+
  geom_line(aes(y=ABindex),size=1.5)+
  geom_hline(yintercept=0,size=1,color="gray50")+
  scale_fill_brewer(name="",palette="Set1")+
  mytheme+
  scale_y_continuous(expand=c(0,0))+
  scale_x_continuous(expand=c(0,0),breaks=pretty_breaks(n=8),limit=c(NA,max(plotdata$Ref_Date)+1))+
  annotate('text',x=max(plotdata$Ref_Date)+0.35,hjust=0,y=0.35,label="Above\nTrend",size=3)+
  annotate('text',x=max(plotdata$Ref_Date)+0.35,hjust=0,y=-0.35,label="Below\nTrend",size=3)+
  geom_segment(x=max(plotdata$Ref_Date)+0.3,xend=max(plotdata$Ref_Date)+0.3,
               y=0.1,yend=0.5,arrow=arrow(length=unit(1,'mm')))+
  geom_segment(x=max(plotdata$Ref_Date)+0.3,xend=max(plotdata$Ref_Date)+0.3,
               y=-0.1,yend=-0.5,arrow=arrow(length=unit(1,'mm')))+
  geom_text(data=plotdata %>% filter(Ref_Date==max(Ref_Date),group=="Labour Markets"),
            aes(y=-1.25,label=paste("Latest:",round(ABindex,2))))+
  labs(y="Index of Economic Activity",
       x="",title=paste0("Economic Conditions Index for Alberta (",min(plotdata$Ref_Date)," to ",max(plotdata$Ref_Date),")"),
       subtitle="Monthly data of economic activity (GDP) is available only for Canada, not provinces. Instead, this index 'averages' 41 monthly data series.
The index is constructed to have mean zero and unit variance. A value of +1 means YoY growth is 1 standard deviation above trend.",
       caption="Sources: Own calculatons from various Statistics Canada data tables. Graph by @trevortombe.
It is the 1st principal component from 41 monthly data series. Based on the Chicago Fed National Activity Index.")
ggsave("plot.png",width=8,height=4.5,dpi=200)

ggplot(plotdata,aes(Ref_Date,index,group=group,fill=group))+
  geom_col(position="stack",color="white",size=0.1,width=1/12)+
  geom_line(aes(y=ABindex),size=1.5)+
  geom_hline(yintercept=0,size=1,color="gray50")+
  scale_fill_brewer(name="",palette="Set1")+
  mytheme+
  scale_y_continuous(expand=c(0,0),limit=c(-2.5,1.5),breaks = pretty_breaks(n=6))+
  scale_x_continuous(expand=c(0,0),breaks=pretty_breaks(n=8),limit=c(NA,max(plotdata$Ref_Date)+1))+
  annotate('text',x=max(plotdata$Ref_Date)+0.35,hjust=0,y=0.35,label="Above\nTrend",size=3)+
  annotate('text',x=max(plotdata$Ref_Date)+0.35,hjust=0,y=-0.35,label="Below\nTrend",size=3)+
  geom_segment(x=max(plotdata$Ref_Date)+0.3,xend=max(plotdata$Ref_Date)+0.3,
               y=0.1,yend=0.5,arrow=arrow(length=unit(1,'mm')))+
  geom_segment(x=max(plotdata$Ref_Date)+0.3,xend=max(plotdata$Ref_Date)+0.3,
               y=-0.1,yend=-0.5,arrow=arrow(length=unit(1,'mm')))+
  geom_text(data=plotdata %>% filter(Ref_Date==2019.4,group=="Labour Markets"),
            aes(y=-1.25,label="A Second\nRecession?"),fontface="bold",color="firebrick")+
  geom_text(data=plotdata %>% filter(Ref_Date==2014.5,group=="Labour Markets"),
            aes(y=-1.5,label="The 2015/16\nRecession"),fontface="bold",color="firebrick")+
  labs(y="Index of Economic Activity",
       x="",title="A Monthly Index of Economic Conditions in Alberta",
       subtitle="The index is constructed to have mean zero and unit variance. A value of +1 means YoY growth is 1 standard deviation above trend.",
       caption="Sources: Own calculatons from 41 monthly data series from Statistics Canada, CFIB, and the AER. Graph by @trevortombe.")
ggsave("plot.png",width=8,height=4.5,dpi=200)
