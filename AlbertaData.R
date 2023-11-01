########################
# Gather the core data #
########################

# Manufacturing: 16-10-0048-01
mfg<-get_cansim_vector('v807466') %>%
  select(When=Date,mfg=VALUE)

# Wholesale Trade: 20-10-0074-01
trade<-get_cansim_vector('v52367703') %>%
  select(When=Date,trade=VALUE)

# Housing Starts": 34-10-0143-01 -- error in the cansim table on Nov 1, 2023
# housing<-get_cansim_vector('v729967') %>%
#   select(When=Date,housing=VALUE)
housing<-get_cansim_vector('v52299962') %>% # table 34-10-0156-01
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

# Employment: 14-10-0287-01
employment<-get_cansim_vector('v2064512') %>%
  select(When=Date,employment=VALUE)

# Self-Employment: 14-10-0288-01
self_emp<-get_cansim_vector('v2067016') %>%
  select(When=Date,self_emp=VALUE)

# Vehicle Sales: 20-10-0001-01, then seasonal adjust
vehicles<-get_cansim_vector('v42169942') %>%
  select(When=Date,vehicles=VALUE) %>%
  mutate(When=as.yearmon(When))
p<-ggsdc(vehicles, aes(x = When, y = vehicles),frequency=12,method = "seas") + geom_line()
vehicles<-p$data %>% 
  filter(component=="trend" | component=="irregular") %>% 
  group_by(x) %>%
  summarise(vehicles=sum(y)) %>%
  rename(When=x) %>%
  mutate(When=as.Date(When))

# Merchandise Exports: 12-10-0119-01
exports_energy<-get_cansim_vector('v1001819843') %>%
  select(When=Date,exports_energy=VALUE)
exports<-get_cansim_vector('v1001819785') %>%
  select(When=Date,exports=VALUE)
exports_nonenergy<-exports %>%
  left_join(exports_energy,by="When") %>%
  mutate(exports_nonenergy=exports-exports_energy) %>%
  select(When,exports_nonenergy)

# Oil Production
oil_url<-'https://api.economicdata.alberta.ca/api/data?code=b0881da3-704c-42b9-a429-c8eff0ec5c73'
oil<-fromJSON(oil_url) %>%
  mutate(When=as.yearmon(Date)) %>%
  select(When,oil=Value)
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
  arrange(When) %>%
  mutate(When=as.Date(When))

# Annual GDP Growth
GDP<-get_cansim_vector('62466959') %>%
  select(When=Date,gdp=VALUE)
GDP<-ts(GDP$gdp,frequency=1,start=c(1997,1))

# Average hours per week: 14-10-0222-01
hours<-get_cansim_vector('v54027409') %>%
  select(When=Date,hours=VALUE)

## Active drilling rigs
rig_url<-'https://api.economicdata.alberta.ca/api/data?code=9442d479-7ad6-47b6-aa12-ef03c750a50d'
rigs<-fromJSON(rig_url) %>%
  mutate(When=as.Date(Date)) %>%
  select(When,rigs=Value)

## Consumer Price Index Data: 18-10-0004-01
deflate<-get_cansim_vector('v41692327') %>%
  filter(Date>="2001-01-01") %>%
  select(VALUE) %>%
  ts(frequency = 12,start = c(2001,01))
