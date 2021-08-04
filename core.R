# Common Packages
packages<-c("curl","scales","zoo","tidyverse",
            "ggseas","ggplot2","ggthemes","jsonlite",
            "data.table","rmarkdown")
check.packages <- function(pkg){
  new.pkg <- pkg[!(pkg %in% installed.packages()[, "Package"])]
  if (length(new.pkg)) 
    install.packages(new.pkg)
  sapply(pkg, require, character.only = TRUE)
}
check.packages(packages)

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

# Set themes
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
mythemebar<-mytheme+theme(
  panel.grid.major.x = element_blank(),
  axis.text.x = element_text(size=12,hjust=0.5,face="bold",colour="black")
)

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
