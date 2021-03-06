rm(list=ls(all=TRUE)) # wipes previous workspace

###########################
# Setup the R Environment #
###########################
# Common Packages
packages<-c("curl","scales","zoo","tidyverse",
            "ggseas","ggplot2","ggthemes","jsonlite",
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

# For the new StatCan Data Tables
getTABLE<-function(x) {
  url<-paste0("https://www150.statcan.gc.ca/n1/tbl/csv/",x,"-eng.zip")
  temp<-tempfile()
  download.file(url,temp)
  if (has_warning(unzip(temp,paste0(x,".csv")))) { # Avoids html landing page
    download.file(url,temp)
  }
  unzip(temp,paste0(x,".csv"))
  rawdata<-fread(paste0(x,".csv"),encoding="UTF-8",stringsAsFactors=FALSE)
  colnames(rawdata)[1]<-"Ref_Date"
  data<-rawdata %>%
    dplyr::rename(Value=VALUE) %>%
    select(-UOM_ID,-SCALAR_ID) %>%
    dplyr::rename_all(list(~make.names(.))) # this replaces the spaces with dots in the column names
  if (class(data$Ref_Date)=="character" & !grepl("/",data[1,"Ref_Date"])){
    data<-data %>%
      mutate(Ref_Date=as.yearmon(Ref_Date))
  }
  if ("GEO" %in% colnames(data)){
    data <- data %>%
      left_join(provnames,by="GEO")
  }
  if ("North.American.Industry.Classification.System..NAICS." %in% colnames(data)){
    data <- data %>%
      rename(NAICS=North.American.Industry.Classification.System..NAICS.) %>%
      mutate(NAICScode=str_match(NAICS,"\\[(.*?)\\]")[,2],
             NAICS=ifelse(regexpr(" \\[",NAICS)>1,
                          substr(NAICS,1,regexpr(" \\[",NAICS)-1),NAICS))
  }
  if (any(grepl("North.American.Product.Classification.System..NAPCS.",colnames(data)))){
    colnames(data)[grepl("North.American.Product.Classification.System..NAPCS.",colnames(data))]<-"NAPCS"
    data <- data %>%
      mutate(NAPCS=ifelse(regexpr(" \\[",NAPCS)>1,
                          substr(NAPCS,1,regexpr(" \\[",NAPCS)-1),NAPCS))
  }
  sourcetable<-gsub("(\\d{2})(\\d{2})(\\d{4})$","\\1-\\2-\\3",x)
  comment(data)<-paste("Statistics Canada data table",sourcetable)
  return(data)
}

mytheme<-theme_minimal()+theme(
  axis.title.y = element_text(size=9),
  axis.title.x = element_text(size=9),
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
source("ECI_data.R")

####################
# Compile the Data #
####################

# Define groups of variables (include wells or not, it's a long delayed variable sometimes)
egy_vars=c("oil","rigs","oilprice","exports_energy") # ,"wells"
biz_vars=c("mfg","durable","nondurable","mfgchem","mfgmetal","mfgfood","mfgmachinery","mfgpetro","trade","exports_nonenergy","self_emp")
lab_vars=c("employment_rate","employment","unemployment","unemp_men_prime","unemp_women_prime","unemp_duration","SEPH","SEPHprivate","SEPHgoods","SEPHservices","SEPHconstruct","hours","full_part_emp","under_unemp","private_emp","partrate")
con_vars=c("housing","earnings","retail","restaurant_spend","MLS","vehicles","trucks")

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
change=log(ABdata[2:nrows,2:ncols])-log(ABdata[1:(nrows-1),2:ncols]) # Month-over-Month changes
nvars<-dim(change)[2]
change[,"unemployment"]<-ABdata[2:nrows,"unemployment"]-ABdata[1:(nrows-1),"unemployment"]
change[,"fulltime_share"]<-ABdata[2:nrows,"fulltime_share"]-ABdata[1:(nrows-1),"fulltime_share"]
change[,"partrate"]<-ABdata[2:nrows,"partrate"]-ABdata[1:(nrows-1),"partrate"]
change[,"under_unemp"]<-ABdata[2:nrows,"under_unemp"]-ABdata[1:(nrows-1),"under_unemp"]
change[,"employment_rate"]<-ABdata[2:nrows,"employment_rate"]-ABdata[1:(nrows-1),"employment_rate"]
change[,"unemp_men_prime"]<-ABdata[2:nrows,"unemp_men_prime"]-ABdata[1:(nrows-1),"unemp_men_prime"]
change[,"unemp_women_prime"]<-ABdata[2:nrows,"unemp_women_prime"]-ABdata[1:(nrows-1),"unemp_women_prime"]
change[,"hours"]<-ABdata[2:nrows,"hours"]-ABdata[1:(nrows-1),"hours"]

# Convert to time-series object
change<-ts(change,frequency=12,start=c(2002,1))

# Estimate the first principle component
scaled_data<-scale(change,scale=T)
PCAtest<-prcomp(change,scale=T)
print(PCAtest)
summary(PCAtest)
predicted<-predict(PCAtest,newdata=change) # extracts the first principle component
weights<-as.numeric(PCAtest$rotation[,1])/sum(as.numeric(PCAtest$rotation[,1]))
temp<-scale(change,scale=T) %*% weights # extracts the first principle component
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
plotdata<-data.frame(Ref_Date=seq(as.yearmon("2001-02"),length.out=length(ABindex),by=1/12)) %>%
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
  geom_line(aes(y=ABindex),size=1.5)+
  geom_hline(yintercept=0,size=1,color="gray50")+
  scale_fill_brewer(name="",palette="Set1")+
  mytheme+
  scale_y_continuous(expand=c(0,0),limit=c(-6,2))+
  scale_x_continuous(expand=c(0,0),breaks=pretty_breaks(n=8),
                     limit=c(NA,max(plotdata$Ref_Date)+4))+
  annotate('text',x=max(plotdata$Ref_Date)+3,hjust=0,y=0.5,label="Above\nTrend",size=3)+
  annotate('text',x=max(plotdata$Ref_Date)+3,hjust=0,y=-0.5,label="Below\nTrend",size=3)+
  geom_segment(x=max(plotdata$Ref_Date)+2.5,xend=max(plotdata$Ref_Date)+2.5,
               y=0.1,yend=0.75,arrow=arrow(length=unit(1,'mm')))+
  geom_segment(x=max(plotdata$Ref_Date)+2.5,xend=max(plotdata$Ref_Date)+2.5,
               y=-0.1,yend=-0.75,arrow=arrow(length=unit(1,'mm')))+
  geom_text(data=plotdata %>% filter(Ref_Date==max(Ref_Date),group=="Labour Markets"),
            aes(y=ABindex,label=paste("Latest:\n",round(ABindex,2))),hjust=-0.25,fontface='bold')+
  geom_point(data=plotdata %>% filter(Ref_Date==max(Ref_Date),group=="Labour Markets"),
             aes(y=ABindex),size=2.5,shape=21,stroke=2.5,fill='white')+
  labs(y="Index of Economic Activity",
       x="",title=paste0("Economic Conditions Index for Alberta (",min(plotdata$Ref_Date)," to ",max(plotdata$Ref_Date),")"),
       subtitle="Monthly data of economic activity (GDP) is available only for Canada, not provinces. Instead, this index 'averages' several dozen monthly data series.
The index is constructed to have mean zero and unit variance. A value of +1 means YoY growth is 1 standard deviation above trend.",
       caption="Sources: Own calculatons from various Statistics Canada data tables. Graph by @trevortombe.
It is the 1st principal component from several dozen monthly data series. Based on the Chicago Fed National Activity Index.")
ggsave("plot_monthly.png",width=8,height=4.5,dpi=200)

temp<-plotdata %>%
  filter(group=="Energy Sector") %>%
  mutate(index=ifelse(Ref_Date==min(Ref_Date),1,1+ABindex/100),
         test=cumprod(index))
ggplot(temp,aes(Ref_Date,test))+geom_line()

GDP<-fromJSON(paste(url,"GrossDomesticProduct",sep="")) %>%
  filter(Industries=="All industries" & Type=="Gross domestic product at basic prices") %>%
  select(When,gdp=Alberta)
GDP<-ts(GDP$gdp,frequency=1,start=c(1997,1))
plotdata2<-data.frame(Ref_Date=seq(as.yearmon("2001-02"),
                                   length.out=length(ABindex),
                                   by=1/12)) %>%
  cbind(temp %>% select(index=test)) %>%
  filter(Ref_Date>="Feb 2001") %>%
  mutate(year=year(Ref_Date)) %>%
  left_join(
    data.frame(GDP) %>% 
      mutate(GDP=as.numeric(GDP)) %>%
      cbind(data.frame(year=seq(1997,2020,1))) %>% 
      mutate(GDPGrowth=(GDP/lag(GDP,1)-1)) %>%
      select(year,GDPGrowth,GDP),by="year"
  ) %>%
  mutate(GDPGrowth=ifelse(month(Ref_Date) %in% c(1,12),NA,GDPGrowth))

# Compare to Real GDP Growth Rates
regresults<-lm(GDP~index,data=plotdata2)
coefficients(regresults)[1]
coefficients(regresults)[2]
plotdata3<-plotdata2 %>%
  mutate(index2=coefficients(regresults)[2]*index+coefficients(regresults)[1])
ggplot(plotdata3,aes(Ref_Date,GDP))+geom_line()+geom_line(aes(y=index2,color='test'))