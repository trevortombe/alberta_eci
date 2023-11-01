rm(list=ls(all=TRUE)) # wipes previous workspace

###########################
# Setup the R Environment #
###########################
# Common Packages
packages<-c("curl","scales","zoo","tidyverse","tempdisagg",
            "ggseas","ggplot2","ggthemes","jsonlite","cansim",
            "data.table","rmarkdown","testit")
check.packages <- function(pkg){
  new.pkg <- pkg[!(pkg %in% installed.packages()[, "Package"])]
  if (length(new.pkg)) 
    install.packages(new.pkg,type="binary")
  sapply(pkg, require, character.only = TRUE)
}
check.packages(packages)

# Useful lists
provinces<-c("Canada","Newfoundland and Labrador","Prince Edward Island","Nova Scotia",
             "New Brunswick","Quebec","Ontario","Manitoba","Saskatchewan",
             "Alberta","British Columbia","Yukon","Northwest Territories","Nunavut")
tenprov<-c("Newfoundland and Labrador","Prince Edward Island","Nova Scotia",
           "New Brunswick","Quebec","Ontario","Manitoba","Saskatchewan",
           "Alberta","British Columbia")
provinces2<-c("CAN","NL","PE","NS",
              "NB","QC","ON","MB","SK",
              "AB","BC","YT","NT","NU")
provsort<-c("BC","AB","SK","MB","ON","QC","NB","NS","PE","NL")
provnames<-data.frame(GEO=provinces,short=provinces2)
provnames$short <- factor(provnames$short, levels = c("CAN","BC","AB","SK","MB","ON","QC","NB","NS","PE","NL","YT","NT","NU")) # Lock in factor level order
provorder<-tibble(GEO=c("BC","AB","SK","MB","ON","QC","NB","NS","PE","NL"),
                  order=as.numeric(seq(1,10)))

mytheme<-theme_minimal()+theme(
  axis.title.y = element_text(size=9),
  axis.title.x = element_text(size=9),
  panel.background = element_rect(fill='white',color=NA),
  plot.background = element_rect(fill='white',color=NA),
  legend.position = "top",
  legend.text=element_text(size=10),
  legend.margin=margin(c(0,0,-0.25,0),unit="cm"),
  plot.caption = element_text(size = 6, color = "gray40",hjust=1),
  plot.title = element_text(face = "bold"),
  plot.subtitle = element_text(size = 8, color = "gray40"),
  panel.grid.minor = element_blank(),
  panel.grid.major.x = element_blank()
)

###################
# Update the Data #
###################
source("AlbertaData.R")

####################
# Compile the Data #
####################

# Define groups of variables
egy_vars=c("oil","rigs","exports_energy")
biz_vars=c("mfg","trade","exports_nonenergy","self_emp")
lab_vars=c("employment","hours")
# con_vars=c("earnings","retail","vehicles")
con_vars=c("earnings","retail","housing","vehicles") # error in housing; removed Nov 1

# Form Matrix of Main Data
variables<-c(egy_vars,biz_vars,lab_vars,con_vars)
ABdata<-data.frame(When=mfg$When)
for (v in variables){
  ABdata<-ABdata %>%
    left_join(get(v),by="When")
}
ABdata<-ABdata %>%
  mutate(When=as.yearmon(When)) %>%
  filter(When>="Jan 2001") %>%
  drop_na() %>%
  ts(frequency=12,start=c(2001,01))

# Convert nominal variables to real
for (v in c("mfg","trade","earnings","retail","exports_energy","exports_nonenergy")){
  ABdata[,v]<-ABdata[,v]/deflate
}

# Define YoY % Changes for most variables, level differences for rates/indexes/hours
nrows<-dim(ABdata)[1]
ncols<-dim(ABdata)[2]
change=log(ABdata[13:nrows,2:ncols])-log(ABdata[1:(nrows-12),2:ncols]) # Year-over-year changes
nvars<-dim(change)[2]

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
         group=ifelse(group=="Households","Households Income/Spending",group))
ggplot(plotdata,aes(Ref_Date,index,group=group,fill=group))+
  geom_col(position="stack",color="white",size=0.1)+
  geom_line(aes(y=ABindex),size=1.25)+
  geom_hline(yintercept=0,size=0.75,color="gray50")+
  scale_fill_brewer(name="",palette="Set1")+
  mytheme+
  scale_x_continuous(expand=c(0,0),breaks=pretty_breaks(n=8),
                     limit=c(NA,max(plotdata$Ref_Date)+6))+
  annotate('text',x=max(plotdata$Ref_Date)+3.5,hjust=0,y=1,label="Above\nTrend",size=2.5)+
  annotate('text',x=max(plotdata$Ref_Date)+3.5,hjust=0,y=-1,label="Below\nTrend",size=2.5)+
  geom_segment(x=max(plotdata$Ref_Date)+3,xend=max(plotdata$Ref_Date)+3,
               y=0.1,yend=1.1,arrow=arrow(length=unit(1,'mm')),size=0.25)+
  geom_segment(x=max(plotdata$Ref_Date)+3,xend=max(plotdata$Ref_Date)+3,
               y=-0.1,yend=-1.1,arrow=arrow(length=unit(1,'mm')),size=0.25)+
  geom_label(data=plotdata %>% filter(Ref_Date==max(Ref_Date),group=="Labour Markets"),nudge_x = 0.5,
            aes(y=ABindex,label=paste("Latest:\n",round(ABindex,2))),hjust=0,size=3,
            fill='white',label.size=0,vjust=1)+
  geom_point(data=plotdata %>% filter(Ref_Date==max(Ref_Date),group=="Labour Markets"),
             aes(y=ABindex),size=2,shape=21,stroke=2,fill='white')+
  labs(y="Index of Economic Activity",
       x="",title=paste0("Economic Conditions Index for Alberta (",min(plotdata$Ref_Date)," to ",max(plotdata$Ref_Date),")"),
       subtitle="Monthly data of economic activity (GDP) is available only for Canada, not provinces. Instead, this index 'averages' several dozen monthly data series.
The index is constructed to have mean zero and unit variance. A value of +1 means YoY growth is 1 standard deviation above trend.",
       caption="Sources: Own calculatons from various Statistics Canada data tables. Graph by @trevortombe.
It is the 1st principal component from several monthly data series. Based on the Chicago Fed National Activity Index.")
ggsave("Figures/plot.png",width=8,height=4.5)

# Aggregate by Year
plotdata2<-data.frame(Ref_Date=seq(as.yearmon("2002-01"),
                                   length.out=length(ABindex),
                                   by=1/12)) %>%
  cbind(ABindex) %>%
  filter(Ref_Date>="Jan 2002") %>%
  mutate(year=year(Ref_Date)) %>%
  left_join(
    data.frame(GDP) %>% 
      mutate(GDP=as.numeric(GDP)) %>%
      cbind(data.frame(year=seq(1997,1997+length(GDP)-1,1))) %>% 
      mutate(GDPGrowth=(GDP/lag(GDP,1)-1)) %>%
      select(year,GDPGrowth,GDP),by="year"
  ) %>%
  mutate(GDPGrowth=ifelse(month(Ref_Date) %in% c(1,12),NA,GDPGrowth))

# Compare to Real GDP Growth Rates
regresults<-lm(GDPGrowth~ABindex,data=plotdata2)
coefficients(regresults)[1]
coefficients(regresults)[2]
plotdata3<-plotdata2 %>%
  mutate(index=coefficients(regresults)[2]*ABindex+coefficients(regresults)[1])
today<-max(plotdata2$Ref_Date)
# Your preferred color scheme (https://www.color-hex.com/color-palette/33490)
col<-c("#CC2529","#396ab1","#3E9651","#DA7C30","#535154","#6B4C9A","#922428","#948B3D")
colbar<-c("#D35E60","#7293CB","#84BA5B","#E1974C","#808585","#9067A7","#AB6857","#CCC210")
ggplot(plotdata3,aes(Ref_Date)) +
  geom_col(aes(y=GDPGrowth),fill=colbar[2],color=colbar[2]) +
  geom_hline(aes(yintercept=0), colour="black", size=1) +
  geom_line(aes(y=index),size=2,color=col[1])+
  annotate('text',x=2013.5,y=-.0375,label="Actual GDP\nGrowth",
           fontface="bold",color=colbar[2])+
  annotate('text',x=2018,y=.125,label="\"Alberta Economic\nConditions Index\"",
           fontface="bold",color=col[1],hjust=1)+
  mytheme+
  scale_y_continuous(breaks = pretty_breaks(8),label=percent)+
  scale_x_continuous(breaks=seq(2002.5,2021.75,2),
                     labels=seq(2002,2021,2))+
  labs(x="",y="Year-over-Year Change (%)",
       title=paste("Index of Alberta's Monthly Economic Conditions, Jan 2002 to",today),
       subtitle="The index is constructed to have mean zero and unit variance. It is then rescale to best fit actual GDP growth.",
       caption="Sources: Own calculatons from multiple monthly data series. Graph by @trevortombe.")
ggsave("Figures/GDPplot.png",width=8,height=4,dpi=200)

# Creates an update to the README page on GitHub
updated<-.POSIXct(Sys.time(), "America/Denver")
render("README.Rmd",output_format = "md_document")

# Save the Index as a CSV file
table<-plotdata %>%
  spread(group,index) %>%
  rename(`Alberta Economic Conditions Index`=ABindex,
         Date=Ref_Date)
write.csv(table,file="Data/ECI_Index_Data.csv",row.names = F)

# Level Index, Initial Values Correspond to 2002 monthly labour compensation levels relative to the 2001 average
labdata_ab<-get_cansim_vector('v1996500') %>%
  mutate(Ref_Date=as.yearmon(Date)) %>%
  filter(Ref_Date>="Jan 2001" & Ref_Date<="Dec 2001") %>%
  mutate(relative=VALUE/mean(VALUE)) %>%
  select(Ref_Date,index=relative)
labdata_ab<-labdata_ab %>%
  mutate(index=1) %>%
  rbind(plotdata3 %>% select(Ref_Date,index) %>% filter(year(Ref_Date)==2002)) %>%
  mutate(index=ifelse(year(Ref_Date)==2002,lag(index,12)*(1+index),index))
for (y in seq(2003,year(max(plotdata3$Ref_Date)))){
  labdata_ab<-labdata_ab %>%
    rbind(plotdata3 %>% select(Ref_Date,index) %>% filter(year(Ref_Date)==y)) %>%
    mutate(index=ifelse(year(Ref_Date)==y,lag(index,12)*(1+index),index))
}
p<-ggsdc(labdata_ab, aes(x = Ref_Date, y = index), method = "seas") + geom_line()
index_levels<-p$data %>%
  filter(component=="trend" | component=="irregular") %>%
  group_by(x) %>%
  summarise(index=sum(y)) %>%
  select(Ref_Date=x,index)

# Temporal Disaggregation of Annual GDP
GDP_series<-window(GDP,start=2001,frequency=1)
Index_series<-ts(index_levels$index,start=2001,frequency=12)
model<-td(GDP_series~Index_series,
   conversion = "sum",
   to = "monthly")
summary(model)
plot(model)
plot(predict(model))
testplot<-index_levels %>% 
  cbind(data.frame(monthly_gdp=predict(model))) %>%
  mutate(year=year(Ref_Date)) %>%
  group_by(year) %>%
  mutate(GDPtest=sum(monthly_gdp),
         annualized=monthly_gdp*12) %>% ungroup()
p<-ggplot(testplot %>% filter(Ref_Date>="Jan 2005"),aes(Ref_Date,annualized/1000))+
  annotate("rect",xmin=as.numeric(as.yearmon("Oct 2008")), 
           xmax=as.numeric(as.yearmon("May 2009")),
           ymin=-Inf, ymax=+Inf, alpha=0.2, fill="dodgerblue")+
  annotate("rect",xmin=as.numeric(as.yearmon("Oct 2014")), 
           xmax=as.numeric(as.yearmon("Jul 2016")),
           ymin=-Inf, ymax=+Inf, alpha=0.2, fill="dodgerblue")+
  annotate("rect",xmin=as.numeric(as.yearmon("Feb 2020")), 
           xmax=as.numeric(as.yearmon("May 2020")),
           ymin=-Inf, ymax=+Inf, alpha=0.2, fill="dodgerblue")+
  annotate("text",x=2014.6,hjust=1,y=Inf,vjust=1,alpha=0.5,
           label="Recessions",color="dodgerblue",size=2.5)+
  geom_line(size=1.5,color=col[1])+
  geom_point(data=filter(testplot,Ref_Date==max(Ref_Date)),
             stroke=2,size=2,shape=21,fill='white',color=col[1])+
  mytheme+
  scale_y_continuous(label=dollar)+
  scale_x_yearmon(format='%Y',breaks=pretty_breaks(7))
p+labs(x="",
       title="Experimental Estimates of Alberta's Monthly Real GDP",
       caption='Graph by @trevortombe',
       subtitle="Source: own calculations from a composite of several monthly indicators constructed to exactly match actual annual real GDP levels",
       y="Billions of (2012) Dollars, Annualized")
ggsave('Figures/MonthlyGDP_Experimental.png',width = 8,height=4)
p+labs(x="",
       y="Billions of (2012) Dollars, Annualized")
ggsave('Figures/MonthlyGDP_Experimental_notitle.png',width = 7,height=3.5)
