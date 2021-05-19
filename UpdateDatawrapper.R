
packages<-c("curl","devtools")
check.packages <- function(pkg){
  new.pkg <- pkg[!(pkg %in% installed.packages()[, "Package"])]
  if (length(new.pkg)) 
    install.packages(new.pkg,type="binary")
  sapply(pkg, require, character.only = TRUE)
}
check.packages(packages)
devtools::install_github("munichrocker/DatawRappr")

# Authorize API Access to Datawrapper
require(DatawRappr)
datawrapper_auth(api_key = Sys.getenv("DATAWRAPPERAUTH"))

# The plot ID of the datawrapper
eci<-"fIIKi"

# Fetch the ECI Data
latestECI<-read.csv('https://github.com/trevortombe/alberta_eci/raw/master/ECI_Index_Data.csv')
end_x_year<-as.character(as.numeric(substr(as.character(latestECI[dim(latestECI)[1],1]),5,8))+2.5)

# Adjust the title and the position of the annotation
eci_data<-dw_retrieve_chart_metadata(eci)
eci_data[[1]]$metadata$visualize$`text-annotations`[[1]]$y
visual_meta<-eci_data[[1]]$metadata$visualize
visual_meta$`text-annotations`[[1]]$y<-as.character(round(latestECI[dim(latestECI)[1],2],2)) # update point to latest
visual_meta$`text-annotations`[[1]]$x<-as.character(2002+dim(latestECI)[1]/12-1/12) # update point to latest
visual_meta$`text-annotations`[[1]]$text<-paste("Latest:",round(latestECI[dim(latestECI)[1],2],2)) # update point to index value
visual_meta$`custom-range-x`[[2]]
dw_edit_chart(eci,visualize = visual_meta)
dw_edit_chart(eci,title=paste("Alberta Economic Conditions Index, Jan 2002 to",as.character(latestECI[dim(latestECI)[1],1])))
