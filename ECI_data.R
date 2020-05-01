# Load Useful Packages, Functions and Themes
source("core.R")

########################
# Gather the core data #
########################
select<-dplyr::select # Conflicts with MASS package loaded later, this ensures dplyr dominates
url<-"http://economicdashboard.alberta.ca/Download/DownloadFile?extension=JSON&requestUrl=http%3A%2F%2Feconomicdashboard.alberta.ca%2Fapi%2F"

# Manufacturing
mfgdata<-getTABLE("16100048")
mfg<-mfgdata %>% 
  filter(NAICS=="Manufacturing",
         Seasonal.adjustment=="Seasonally adjusted",GEO=="Alberta") %>%
  mutate(Value=ifelse(is.na(Value),0,Value)) %>%
  select(When=Ref_Date,mfg=Value)
durable<-mfgdata %>% 
  filter(NAICS=="Durable goods industries",
         Seasonal.adjustment=="Seasonally adjusted",GEO=="Alberta") %>%
  mutate(Value=ifelse(is.na(Value),0,Value)) %>%
  select(When=Ref_Date,durables=Value)
nondurable<-mfgdata %>% 
  filter(NAICS=="Non-durable goods industries",
         Seasonal.adjustment=="Seasonally adjusted",GEO=="Alberta") %>%
  mutate(Value=ifelse(is.na(Value),0,Value)) %>%
  select(When=Ref_Date,nondurables=Value)
mfgpetro<-mfgdata %>% 
  filter(NAICS=="Petroleum and coal product manufacturing",
         Seasonal.adjustment=="Seasonally adjusted",GEO=="Alberta") %>%
  mutate(Value=ifelse(is.na(Value),0,Value)) %>%
  select(When=Ref_Date,mfgpetro=Value)
mfgchem<-mfgdata %>% 
  filter(NAICS=="Chemical manufacturing",
         Seasonal.adjustment=="Seasonally adjusted",GEO=="Alberta") %>%
  mutate(Value=ifelse(is.na(Value),0,Value)) %>%
  select(When=Ref_Date,mfgchem=Value)
mfgmetal<-mfgdata %>% 
  filter(NAICS=="Fabricated metal product manufacturing",
         Seasonal.adjustment=="Seasonally adjusted",GEO=="Alberta") %>%
  mutate(Value=ifelse(is.na(Value),0,Value)) %>%
  select(When=Ref_Date,mfgmetal=Value)
mfgfood<-mfgdata %>% 
  filter(NAICS=="Food manufacturing",
         Seasonal.adjustment=="Seasonally adjusted",GEO=="Alberta") %>%
  mutate(Value=ifelse(is.na(Value),0,Value)) %>%
  select(When=Ref_Date,mfgfood=Value)
mfgmachinery<-mfgdata %>% 
  filter(NAICS=="Machinery manufacturing",
         Seasonal.adjustment=="Seasonally adjusted",GEO=="Alberta") %>%
  mutate(Value=ifelse(is.na(Value),0,Value)) %>%
  select(When=Ref_Date,mfgmachinery=Value)

# WHolesale Trade
tradedata<-getTABLE("20100074")
trade<-tradedata %>%
  filter(NAICS=="Wholesale trade",
         Adjustments=="Seasonally adjusted",GEO=="Alberta") %>%
  select(When=Ref_Date,trade=Value)

# Housing Starts
housedata<-getTABLE("34100143")
housing<-housedata %>%
  filter(GEO=="Alberta",Housing.estimates=="Housing starts",
         Type.of.unit=="Total units") %>%
  select(When=Ref_Date,housing=Value)

# Average weekly earnings
earndata<-getTABLE("14100223")
earnings<-earndata %>%
  filter(GEO=="Alberta",Estimate=="Average weekly earnings including overtime for all employees",
         NAICS=="Industrial aggregate excluding unclassified businesses") %>%
  select(When=Ref_Date,earnings=Value)

# Retail Sales
retdata<-getTABLE("20100008")
retail<-retdata %>%
  filter(GEO=="Alberta",NAICS=="Retail trade",
         Adjustments=="Seasonally adjusted") %>%
  select(When=Ref_Date,retail=Value)

# Employment Rates
LFS<-getTABLE("14100287")
employment<-LFS %>%
  filter(Statistics=="Estimate" & Data.type=="Seasonally adjusted" &
           Labour.force.characteristics=="Employment" & Sex=="Both sexes" &
           Age.group=="15 years and over" & GEO=="Alberta") %>%
  select(When=Ref_Date,employment=Value)
employment_rate<-LFS %>%
  filter(Statistics=="Estimate" & Data.type=="Seasonally adjusted" &
           Labour.force.characteristics=="Employment rate" & Sex=="Both sexes" &
           Age.group=="15 years and over" & GEO=="Alberta") %>%
  select(When=Ref_Date,employment_rate=Value)
full_part_emp<-LFS %>%
  filter(Data.type=="Seasonally adjusted",Labour.force.characteristics %in% c("Full-time employment",
                                                                              "Employment"),
         Sex=="Both sexes",Statistics=="Estimate",Age.group=="15 years and over",GEO=="Alberta") %>%
  select(Ref_Date,Value,Labour.force.characteristics) %>%
  spread(Labour.force.characteristics,Value) %>%
  mutate(fulltime_share=`Full-time employment`/Employment) %>%
  select(When=Ref_Date,fulltime_share)

# Unemployment
unemployment<-LFS %>%
  filter(Statistics=="Estimate" & Data.type=="Seasonally adjusted" &
           Labour.force.characteristics=="Unemployment rate" & Sex=="Both sexes" &
           Age.group=="15 years and over" & GEO=="Alberta") %>%
  select(When=Ref_Date,unemployment=Value)
unemp_men_prime<-LFS %>%
  filter(Statistics=="Estimate" & Data.type=="Seasonally adjusted" &
           Labour.force.characteristics=="Unemployment rate" & Sex=="Males" &
           Age.group=="25 to 54 years" & GEO=="Alberta") %>%
  select(When=Ref_Date,unemp_men_prime=Value)
unemp_women_prime<-LFS %>%
  filter(Statistics=="Estimate" & Data.type=="Seasonally adjusted" &
           Labour.force.characteristics=="Unemployment rate" & Sex=="Females" &
           Age.group=="25 to 54 years" & GEO=="Alberta") %>%
  select(When=Ref_Date,unemp_women_prime=Value)
unemp<-getTABLE("14100342")
unemp_duration<-unemp %>%
  filter(Duration.of.unemployment=="Average weeks unemployed, no top-code",
         Ref_Date>="Jan 1997",
         Sex=="Both sexes",Age.group=="15 years and over",GEO=="Alberta",
         Statistics=="Estimate",Data.type=="Seasonally adjusted") %>%
  select(When=Ref_Date,unemp_duration=Value)

# Private and Self-Employment
pubdata<-getTABLE("14100288")
private_emp<-pubdata %>%
  filter(Statistics=="Estimate" & Data.type=="Seasonally adjusted" & 
           GEO=="Alberta" & Sex=="Both sexes" & 
           Class.of.worker=="Private sector employees") %>%
  filter(Ref_Date>="Jan 1997") %>%
  select(When=Ref_Date,private_emp=Value)
self_emp<-pubdata %>%
  filter(Statistics=="Estimate" & Data.type=="Seasonally adjusted" & 
           GEO=="Alberta" & Sex=="Both sexes" & 
           Class.of.worker=="Self-employed") %>%
  filter(Ref_Date>="Jan 1997") %>%
  select(When=Ref_Date,self_emp=Value)

# Underemployment rate, with seasonal adjustment
suppunemp<-getTABLE("14100077")
temp<-suppunemp %>%
  filter(Sex=="Both sexes",Age.group=="15 years and over",
         grepl("R8",Supplementary.unemployment.rates),GEO=="Alberta") %>%
  mutate(index=100*Value/Value[1]) %>%
  mutate(label="Underemployed") %>%
  filter(Ref_Date>="Jan 1997")
p<-ggsdc(temp, aes(x = Ref_Date, y = Value,group=label,color=label), method = "seas") + geom_line()
under_unemp<-p$data %>% # this is seasonally adjusted
  filter(component=="trend" | component=="irregular") %>%
  group_by(x) %>%
  summarise(rate=sum(y)) %>%
  select(When=x,under_unemp=rate)

# Prime-Age Participation Rate
partrate<-LFS %>%
  filter(Statistics=="Estimate" & Data.type=="Seasonally adjusted" &
           Labour.force.characteristics=="Participation rate" & Sex=="Both sexes" &
           Age.group=="25 to 54 years" & GEO=="Alberta") %>%
  select(When=Ref_Date,partrate=Value)

# Vehicle Sales, with Seasonal Adjustment
temp<-getTABLE("20100001")
vehicles<-temp %>%
  filter(GEO=="Alberta",Vehicle.type=="Total, new motor vehicles",
         Origin.of.manufacture=="Total, country of manufacture",
         Seasonal.adjustment=="Unadjusted",Sales=="Units",Ref_Date>="Jan 2000") %>%
  select(When=Ref_Date,vehicles=Value)
p<-ggsdc(vehicles, aes(x = When, y = vehicles),frequency=12,method = "seas") + geom_line()
vehicles<-p$data %>% 
  filter(component=="trend" | component=="irregular") %>% 
  group_by(x) %>%
  summarise(vehicles=sum(y)) %>%
  rename(When=x)
trucks<-temp %>%
  filter(GEO=="Alberta",Vehicle.type=="Trucks",Sales=="Units",Ref_Date>="Jan 2000",
         Origin.of.manufacture=="Total, country of manufacture") %>%
  select(When=Ref_Date,trucks=Value)
p<-ggsdc(trucks, aes(x = When, y = trucks),frequency=12,method = "seas") + geom_line()
trucks<-p$data %>% 
  filter(component=="trend" | component=="irregular") %>% 
  group_by(x) %>%
  summarise(trucks=sum(y)) %>%
  rename(When=x)

# Merchandise Exports
exports_data<-getTABLE("12100119")
exports_nonenergy<-exports_data %>%
  filter(Trade=="Domestic export" & GEO=="Alberta" & Principal.trading.partners=="All countries" &
           (NAPCS=="Total of all merchandise" | NAPCS=="Energy products")) %>%
  mutate(NAPCS=replace(NAPCS,NAPCS=="Total of all merchandise","Other Goods"),
         NAPCS=replace(NAPCS,NAPCS=="Energy products","Energy Products")) %>%
  group_by(Ref_Date,GEO) %>%
  mutate(energy=max(Value*(NAPCS=="Energy Products")),
         net=ifelse(NAPCS=="Energy Products",Value,Value-energy)) %>%
  ungroup() %>%
  filter(NAPCS=="Other Goods") %>%
  select(When=Ref_Date,exports_nonenergy=Value)
exports_energy<-exports_data %>%
  filter(Trade=="Domestic export" & GEO=="Alberta" & Principal.trading.partners=="All countries" &
           (NAPCS=="Total of all merchandise" | NAPCS=="Energy products")) %>%
  mutate(NAPCS=replace(NAPCS,NAPCS=="Total of all merchandise","Other Goods"),
         NAPCS=replace(NAPCS,NAPCS=="Energy products","Energy Products")) %>%
  group_by(Ref_Date,GEO) %>%
  mutate(energy=max(Value*(NAPCS=="Energy Products")),
         net=ifelse(NAPCS=="Energy Products",Value,Value-energy)) %>%
  ungroup() %>%
  filter(NAPCS=="Energy Products") %>%
  select(When=Ref_Date,exports_energy=Value)

# Wells Drilled, with Seasonal Adjustment
wells<-fromJSON(paste(url,"WellsDrilled",sep="")) %>%
  filter(Commodity=="All Commodities" & WellType=="All Well Types") %>%
  mutate(When=as.yearmon(When)) %>%
  arrange(When) %>%
  select(When,wells=Alberta)
# manual add wells for latest month (if unavailable) from https://www.aer.ca/providing-information/data-and-reports/statistical-reports/st59.html
wells<-wells %>%
  rbind(data.frame(When=as.yearmon("2020-02"),wells=467+42))
# Seasonally adjust
p<-ggsdc(wells, aes(x = When, y = wells),frequency=12,method = "seas") + geom_line()
wells<-p$data %>% 
  filter(component=="trend" | component=="irregular") %>% 
  group_by(x) %>%
  summarise(wells=sum(y)) %>%
  rename(When=x)

# Oil Production
oil<-fromJSON(paste(url,"OilProduction",sep="")) %>%
  filter(Type=="Total") %>%
  select(When,oil=Alberta)
#oil[(dim(oil)[1]+1),4]<-14275240.2 # add latest data point manually from http://www.aer.ca/documents/sts/st3/Oil_current.pdf
oldoil<-read.csv("old_oil.csv") %>% 
  mutate(When=as.yearmon(Date,"%b-%y")) %>% 
  select(When,oldoil=Adjusted.AB.Production) %>%
  filter(When<="Jan 2007") %>%
  mutate(oil=oldoil*(oil[1,2]/oldoil[n()])) %>%
  filter(When!="Jan 2007") %>%
  select(When,oil)
oil<-oil %>%
  mutate(When=as.yearmon(When)) %>%
  rbind(oldoil) %>%
  arrange(When)

# Oil Prices
oilprice<-fromJSON(paste(url,"OilPrice",sep="")) %>%
  filter(Type=="WTI") %>%
  select(When,oilprice=Alberta)

# Annual GDP Growth
GDP<-fromJSON(paste(url,"GrossDomesticProduct",sep="")) %>%
  filter(Industries=="All industries" & Type=="Gross domestic product at basic prices") %>%
  select(When,gdp=Alberta)
GDP<-ts(GDP$gdp,frequency=1,start=c(1997,1))

# CFIB Business Barometer -- update manually from their website
cfib<-read.csv("cfib.csv") %>%
  mutate(When=as.yearmon(Date,"%d/%m/%Y")) %>%
  select(When,cfib=Alberta)

# EI claims
EIdata<-getTABLE("14100005")
EIclaims<-EIdata %>%
  filter(GEO=="Alberta",Type.of.claim=="Initial and renewal claims, seasonally adjusted",
         Claim.detail=="Received") %>%
  select(When=Ref_Date,EIclaims=Value)

# Payroll jobs (SEPH employment)
SEPHdata<-getTABLE("14100223")
SEPH<-SEPHdata %>%
  filter(GEO=="Alberta",Estimate=="Employment for all employees",
         NAICS=="Industrial aggregate including unclassified businesses") %>%
  select(When=Ref_Date,seph=Value)
SEPHpublic<-SEPHdata %>%
  filter(GEO=="Alberta",Estimate=="Employment for all employees",
         NAICS %in% c("Public administration","Educational services","Health care and social assistance")) %>%
  group_by(Ref_Date) %>%
  summarise(seph_public=sum(Value)) %>%
  select(When=Ref_Date,seph_public)
SEPHprivate<-SEPH %>%
  left_join(SEPHpublic,by="When") %>%
  mutate(seph_private=seph-seph_public) %>%
  select(When,seph_private)
SEPHgoods<-SEPHdata %>%
  filter(GEO=="Alberta",Estimate=="Employment for all employees",
         NAICS=="Goods producing industries") %>%
  select(When=Ref_Date,seph_goods=Value)
SEPHservices<-SEPHdata %>%
  filter(GEO=="Alberta",Estimate=="Employment for all employees",
         NAICS=="Service producing industries") %>%
  select(When=Ref_Date,seph_services=Value)
SEPHconstruct<-SEPHdata %>%
  filter(GEO=="Alberta",Estimate=="Employment for all employees",
         NAICS=="Construction") %>%
  select(When=Ref_Date,seph_construct=Value)

# Average hours per week
hoursdata<-getTABLE("14100222")
hours<-hoursdata %>%
  filter(GEO=="Alberta" & Estimate=="Average weekly hours including overtime for hourly employees") %>%
  select(When=Ref_Date,hours=Value)

## Active drilling rigs
rigs<-fromJSON(paste(url,"RigActivity",sep="")) %>%
  filter(Type=="Active") %>%
  select(When,rigs=Alberta)

# MLS Sales, seasonally adjust
# October is mis-coded so have to fix manually
temp<-fromJSON(paste(url,"RealEstateSales",sep="")) %>%
  filter(Type=="Province") %>%
  select(mls=Alberta)
MLSdata<-temp %>%
  mutate(When=seq(as.yearmon("1980-01"),length.out=length(temp$mls),by=1/12))
p<-ggsdc(MLSdata, aes(x = When, y = mls),frequency=12,method = "seas") + geom_line()
MLS<-p$data %>% 
  filter(component=="trend" | component=="irregular") %>% 
  group_by(x) %>%
  summarise(mls=sum(y)) %>%
  rename(When=x)

## Food services and drinking places
food_services_data<-getTABLE("21100019")
restaurant_spend<-food_services_data %>%
  filter(GEO=="Alberta",
         NAICS=="Total, food services and drinking places",
         Seasonal.adjustment=="Seasonally adjusted",
         Service.detail=="Receipts") %>%
  select(When=Ref_Date,restaurant_spend=Value)

## Consumer Price Index Data -- to inflation adjust nominal variables
cpi<-getTABLE("18100004")
deflate<-cpi %>%
  filter(Products.and.product.groups=="All-items",
         GEO=="Alberta",Ref_Date>="Jan 2001") %>%
  select(Value) %>%
  ts(frequency = 12,start = c(2001,01))

# Save the data for use with AAI.R and AAI_monthly.R
save.image("ECI_data.RData")

