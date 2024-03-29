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

# Wholesale Trade: 20-10-0074-01
trade<-get_cansim_vector('v52367703') %>%
  select(When=Date,trade=VALUE)

# Housing Starts": 34-10-0143-01
housing<-get_cansim_vector('v729967') %>%
  select(When=Date,housing=VALUE)

# Average weekly earnings: 14-10-0223-01
earnings<-get_cansim_vector('v79311387') %>%
  select(When=Date,earnings=VALUE)

# Retail Sales: 20-01-0008-01
retail_old<-get_cansim_vector('52367215') %>%
  select(When=Date,old=VALUE)
retail_new<-get_cansim_vector('1446859973') %>%
  select(When=Date,new=VALUE)
retail<-data.frame(When=seq(min(retail_old$When),max(retail_new$When),by = "month")) %>%
  left_join(retail_old,by='When') %>%
  left_join(retail_new,by='When') %>%
  mutate(splice=old*weighted.mean(new,When==min(retail_new$When))/weighted.mean(old,When==min(retail_new$When))) %>%
  mutate(retail=ifelse(When<min(retail_new$When),splice,new)) %>%
  select(When,retail)

# Employment Rates: 14-10-0287-01
employment<-get_cansim_vector('v2064512') %>%
  select(When=Date,employment=VALUE)
employment_rate<-get_cansim_vector('v2064518') %>%
  select(When=Date,employment_rate=VALUE)

# Unemployment
unemployment<-get_cansim_vector('v2064516') %>%
  select(When=Date,unemployment=VALUE)
unemp_men_prime<-get_cansim_vector('v2064660') %>%
  select(When=Date,unemp_men_prime=VALUE)
unemp_women_prime<-get_cansim_vector('v2064669') %>%
  select(When=Date,unemp_women_prime=VALUE)
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
partrate<-get_cansim_vector('v2064652') %>%
  select(When=Date,partrate=VALUE)

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
exports_energy<-get_cansim_vector('v1567090501') %>%
  select(When=Date,exports_energy=VALUE)
exports<-get_cansim_vector('v1567090443') %>%
  select(When=Date,exports=VALUE)
exports_nonenergy<-exports %>%
  left_join(exports_energy,by="When") %>%
  mutate(exports_nonenergy=exports-exports_energy) %>%
  select(When,exports_nonenergy)

# Wells Drilled, with Seasonal Adjustment
well_url<-'https://api.economicdata.alberta.ca/api/data?code=f4a8bd80-ec75-4432-ab5e-a15add01e5cd'
wells<-fromJSON(well_url) %>%
  mutate(When=as.yearmon(Date)) %>%
  arrange(When) %>%
  select(When,wells=Value)
# manual add wells for latest month (if unavailable) from https://www.aer.ca/providing-information/data-and-reports/statistical-reports/st59.html
#wells<-wells %>%
#  rbind(data.frame(When=as.yearmon("2020-05"),wells=38))
# Seasonally adjust
p<-ggsdc(wells, aes(x = When, y = wells),frequency=12,method = "seas") + geom_line()
wells<-p$data %>% 
  filter(component=="trend" | component=="irregular") %>% 
  group_by(x) %>%
  summarise(wells=sum(y)) %>%
  rename(When=x)

# Oil Production
oil_url<-'https://api.economicdata.alberta.ca/api/data?code=b0881da3-704c-42b9-a429-c8eff0ec5c73'
oil<-fromJSON(oil_url) %>%
  mutate(When=as.yearmon(Date)) %>%
  select(When,oil=Value)
#oil[(dim(oil)[1]+1),4]<-14275240.2 # add latest data point manually from http://www.aer.ca/documents/sts/st3/Oil_current.pdf
oldoil<-read.csv("Data/old_oil.csv") %>% 
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

# WCS Oil Prices
price_url<-'https://api.economicdata.alberta.ca/api/data?code=667b15de-bd46-4be3-ad80-a94ba1e5ee30'
oilprice<-fromJSON(price_url) %>%
  mutate(When=as.yearmon(Date)) %>%
  select(When,oilprice=Value)

# Annual GDP Growth
GDP<-get_cansim_vector('62466959') %>%
  mutate(When=as.yearmon(Date)) %>%
  select(When,gdp=VALUE)
GDP<-ts(GDP$gdp,frequency=1,start=c(1997,1))
# 
# # CFIB Business Barometer -- update manually from their website
# cfib<-read.csv("cfib.csv") %>%
#   mutate(When=as.yearmon(Date,"%d/%m/%Y")) %>%
#   select(When,cfib=Alberta)

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
rig_url<-'https://api.economicdata.alberta.ca/api/data?code=9442d479-7ad6-47b6-aa12-ef03c750a50d'
rigs<-fromJSON(rig_url) %>%
  mutate(When=as.yearmon(Date)) %>%
  select(When,rigs=Value)

# MLS Sales, seasonally adjust
mls_url<-'https://api.economicdata.alberta.ca/api/data?code=2731cf85-3590-4a01-8a91-437ff439a7d2'
temp<-fromJSON(mls_url) %>%
  mutate(When=as.yearmon(Date)) %>%
  select(mls=Value)
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
# save.image("ECI_data.RData")

