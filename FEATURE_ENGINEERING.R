# Data preprocessing script for a dataset related to real estate or property listings. It's likely intended to clean and transform the data to make it more suitable for analysis.

# The code then proceeds with a series of data cleaning and transformation operations on the dataset "TEMP." The dataset "TEMP" is assumed to contain various columns, such as "price," "type," "location," "bedrooms," "bathrooms," "livingspace," and so on. The operations are as follows:
  
## Several gsub functions are used to remove unwanted characters and spaces from the "price" column.
## Similar gsub functions are used for the "type," "location," "bedrooms," "bathrooms," "livingspace," and "lotdim" columns.
## The str_match function is used to extract specific patterns from the "location," "livingspace," and "lotdim" columns.
## The "YEAR" column is extracted from the "solddate" column.
## A pattern is used to extract the "CONSTYEAR" (year of construction) from various columns like "f1," "f2," and so on.
## Similar patterns are used to extract various property features like "FACING," "FLOOR," "HEATING," "KITCHEN," "SHOWERANDBATH," "BASEMENT," "POOL," "GAR," and "LOCATION."
## Several binary variables (e.g., "FAC_BRICK," "FAC_STONE") are created based on the presence of specific features in the "FACING" column.
## Similar binary variables are created for "FLOOR," "HEATING," and other features.


setwd("/Users/simon-pierreboucher/Desktop/HEDORATE/webscrapping")
load("bd.RData")
library(sjmisc)
library(stringr)
TEMP=bd
TEMP$price=gsub("\n","",TEMP$price)
TEMP$price=gsub(",","",TEMP$price)
TEMP$price=gsub(" ","",TEMP$price)
TEMP$price=gsub("SalepriceprovidedbythesellertoDuProprio.","",TEMP$price)
TEMP$price=substr(TEMP$price,2,nchar(TEMP$price))
TEMP$type=gsub("\n        ","",TEMP$type)
TEMP$type=gsub("\n    ","",TEMP$type)
TEMP$location=gsub("\n                \n            ","%",TEMP$location)
TEMP$location=gsub("\n            ","%%",TEMP$location)
TEMP$location=gsub("\n        \n    ","%%%",TEMP$location)
TEMP$TEMP1 <- str_match(TEMP$location, "%\\s*(.*?)\\s*%%")
TEMP$CITY1<-TEMP$TEMP1[,2]
TEMP$TEMP2 <- str_match(TEMP$location, "%%\\s*(.*?)\\s*%%%")
TEMP$CITY2<-TEMP$TEMP2[,2]
TEMP$bedrooms=gsub("\n","",TEMP$bedrooms)
TEMP$bedrooms=gsub(" ","",TEMP$bedrooms)
TEMP$bedrooms=as.numeric(TEMP$bedrooms)

TEMP$bathrooms=gsub("\n","",TEMP$bathrooms)
TEMP$bathrooms=gsub(" ","",TEMP$bathrooms)
TEMP$bathrooms=as.numeric(TEMP$bathrooms)
TEMP$livingspace=gsub("\n                                    ","%",TEMP$livingspace)
TEMP$livingspace=gsub(" ft²","%%",TEMP$livingspace)
TEMP$livingspace=gsub(" ft","%%%",TEMP$livingspace)


TEMP$LA <- str_match(TEMP$livingspace, "%\\s*(.*?)\\s*%%")
TEMP$LA=gsub(" ","",TEMP$LA)
TEMP$LA=gsub(",","",TEMP$LA)
TEMP$LA=TEMP$LA[,2]

TEMP$LA_TEMP1 <- str_match(TEMP$livingspace, "%\\s*(.*?)\\s*x")
TEMP$LA_TEMP2 <- str_match(TEMP$livingspace, "x\\s*(.*?)\\s*%%%")
TEMP$LA_TEMP <- as.numeric(TEMP$LA_TEMP1[,2])*as.numeric(TEMP$LA_TEMP2[,2])
TEMP$LA<- ifelse(is.na(TEMP$LA_TEMP),TEMP$LA ,TEMP$LA_TEMP) 
TEMP$LA=as.numeric(TEMP$LA)

TEMP$lotdim=gsub("\n                                ","%",TEMP$lotdim)
TEMP$lotdim=gsub(" ft²","%%",TEMP$lotdim)
TEMP$lotdim=gsub(" ft","%%%",TEMP$lotdim)

TEMP$LD <- str_match(TEMP$lotdim, "%\\s*(.*?)\\s*%%")
TEMP$LD=gsub(" ","",TEMP$LD)
TEMP$LD=gsub(",","",TEMP$LD)
TEMP$LD=TEMP$LD[,2]
TEMP$LD_TEMP1 <- str_match(TEMP$lotdim, "%\\s*(.*?)\\s*x")
TEMP$LD_TEMP2 <- str_match(TEMP$lotdim, "x\\s*(.*?)\\s*%%%")
TEMP$LD_TEMP <- as.numeric(TEMP$LD_TEMP1[,2])*as.numeric(TEMP$LD_TEMP2[,2])
TEMP$LD<- ifelse(is.na(TEMP$LD_TEMP),TEMP$LD ,TEMP$LD_TEMP) 
TEMP$LD=as.numeric(TEMP$LD)
TEMP$YEAR=substr(TEMP$solddate,nchar(TEMP$solddate)-3,nchar(TEMP$solddate))
TEMP$YEAR_temp=as.numeric(TEMP$YEAR)
pattern<-"Year of construction\n"
TEMP$CONSTYEAR<- ifelse(grepl(pattern,TEMP$f1)==1, TEMP$f1, "") 
TEMP$CONSTYEAR<- ifelse(grepl(pattern,TEMP$f2)==1, TEMP$f2, TEMP$CONSTYEAR) 
TEMP$CONSTYEAR<- ifelse(grepl(pattern,TEMP$f3)==1, TEMP$f3, TEMP$CONSTYEAR) 
TEMP$CONSTYEAR<- ifelse(grepl(pattern,TEMP$f4)==1, TEMP$f4, TEMP$CONSTYEAR) 
TEMP$CONSTYEAR<- ifelse(grepl(pattern,TEMP$f5)==1, TEMP$f5, TEMP$CONSTYEAR) 
TEMP$CONSTYEAR<- ifelse(grepl(pattern,TEMP$f6)==1, TEMP$f6, TEMP$CONSTYEAR) 
TEMP$CONSTYEAR<- ifelse(grepl(pattern,TEMP$f7)==1, TEMP$f7, TEMP$CONSTYEAR) 
TEMP$CONSTYEAR<- ifelse(grepl(pattern,TEMP$f8)==1, TEMP$f8, TEMP$CONSTYEAR) 
TEMP$CONSTYEAR<- ifelse(grepl(pattern,TEMP$f9)==1, TEMP$f9, TEMP$CONSTYEAR) 
TEMP$CONSTYEAR<- ifelse(grepl(pattern,TEMP$f10)==1, TEMP$f10, TEMP$CONSTYEAR) 
TEMP$CONSTYEAR=gsub("\n                    Year of construction\n                    \n                    ","",TEMP$CONSTYEAR)
TEMP$CONSTYEAR=gsub("\n                ","",TEMP$CONSTYEAR)
TEMP$CONSTYEAR=as.numeric(TEMP$CONSTYEAR)
TEMP$AGE=ifelse(grepl("New",TEMP$type)==1,0 ,TEMP$YEAR_temp-TEMP$CONSTYEAR) 

pattern<-"External facing:\n"
TEMP$FACING<- ifelse(grepl(pattern,TEMP$Fd1)==1, TEMP$Fd1, "") 
TEMP$FACING<- ifelse(grepl(pattern,TEMP$Fd2)==1, TEMP$Fd2, TEMP$FACING) 
TEMP$FACING<- ifelse(grepl(pattern,TEMP$Fd3)==1, TEMP$Fd3, TEMP$FACING) 
TEMP$FACING<- ifelse(grepl(pattern,TEMP$Fd4)==1, TEMP$Fd4, TEMP$FACING) 
TEMP$FACING<- ifelse(grepl(pattern,TEMP$Fd5)==1, TEMP$Fd5, TEMP$FACING) 
TEMP$FACING<- ifelse(grepl(pattern,TEMP$Fd6)==1, TEMP$Fd6, TEMP$FACING) 
TEMP$FACING<- ifelse(grepl(pattern,TEMP$Fd7)==1, TEMP$Fd7, TEMP$FACING) 
TEMP$FACING<- ifelse(grepl(pattern,TEMP$Fd8)==1, TEMP$Fd8, TEMP$FACING) 
TEMP$FACING<- ifelse(grepl(pattern,TEMP$Fd9)==1, TEMP$Fd9, TEMP$FACING) 
TEMP$FACING<- ifelse(grepl(pattern,TEMP$Fd10)==1, TEMP$Fd10, TEMP$FACING) 
TEMP$FACING<- ifelse(grepl(pattern,TEMP$Fd11)==1, TEMP$Fd11, TEMP$FACING) 
TEMP$FACING<- ifelse(grepl(pattern,TEMP$Fd12)==1, TEMP$Fd12, TEMP$FACING) 
TEMP$FACING<- ifelse(grepl(pattern,TEMP$Fd13)==1, TEMP$Fd13, TEMP$FACING) 
TEMP$FACING<- ifelse(grepl(pattern,TEMP$Fd14)==1, TEMP$Fd14, TEMP$FACING) 
TEMP$FACING<- ifelse(grepl(pattern,TEMP$Fd15)==1, TEMP$Fd15, TEMP$FACING) 
TEMP$FACING<- ifelse(grepl(pattern,TEMP$Fd16)==1, TEMP$Fd16, TEMP$FACING) 
TEMP$FACING<- ifelse(grepl(pattern,TEMP$Fd17)==1, TEMP$Fd17, TEMP$FACING) 
TEMP$FACING<- ifelse(grepl(pattern,TEMP$Fd18)==1, TEMP$Fd18, TEMP$FACING) 
TEMP$FACING<- ifelse(grepl(pattern,TEMP$Fd19)==1, TEMP$Fd19, TEMP$FACING) 
TEMP$FACING<- ifelse(grepl(pattern,TEMP$Fd20)==1, TEMP$Fd20, TEMP$FACING) 

pattern<-"Floor coverings:\n"
TEMP$FLOOR<- ifelse(grepl(pattern,TEMP$Fd1)==1, TEMP$Fd1, "") 
TEMP$FLOOR<- ifelse(grepl(pattern,TEMP$Fd2)==1, TEMP$Fd2, TEMP$FLOOR) 
TEMP$FLOOR<- ifelse(grepl(pattern,TEMP$Fd3)==1, TEMP$Fd3, TEMP$FLOOR) 
TEMP$FLOOR<- ifelse(grepl(pattern,TEMP$Fd4)==1, TEMP$Fd4, TEMP$FLOOR) 
TEMP$FLOOR<- ifelse(grepl(pattern,TEMP$Fd5)==1, TEMP$Fd5, TEMP$FLOOR) 
TEMP$FLOOR<- ifelse(grepl(pattern,TEMP$Fd6)==1, TEMP$Fd6, TEMP$FLOOR) 
TEMP$FLOOR<- ifelse(grepl(pattern,TEMP$Fd7)==1, TEMP$Fd7, TEMP$FLOOR) 
TEMP$FLOOR<- ifelse(grepl(pattern,TEMP$Fd8)==1, TEMP$Fd8, TEMP$FLOOR) 
TEMP$FLOOR<- ifelse(grepl(pattern,TEMP$Fd9)==1, TEMP$Fd9, TEMP$FLOOR) 
TEMP$FLOOR<- ifelse(grepl(pattern,TEMP$Fd10)==1, TEMP$Fd10, TEMP$FLOOR) 
TEMP$FLOOR<- ifelse(grepl(pattern,TEMP$Fd11)==1, TEMP$Fd11, TEMP$FLOOR) 
TEMP$FLOOR<- ifelse(grepl(pattern,TEMP$Fd12)==1, TEMP$Fd12, TEMP$FLOOR) 
TEMP$FLOOR<- ifelse(grepl(pattern,TEMP$Fd13)==1, TEMP$Fd13, TEMP$FLOOR) 
TEMP$FLOOR<- ifelse(grepl(pattern,TEMP$Fd14)==1, TEMP$Fd14, TEMP$FLOOR) 
TEMP$FLOOR<- ifelse(grepl(pattern,TEMP$Fd15)==1, TEMP$Fd15, TEMP$FLOOR) 
TEMP$FLOOR<- ifelse(grepl(pattern,TEMP$Fd16)==1, TEMP$Fd16, TEMP$FLOOR) 
TEMP$FLOOR<- ifelse(grepl(pattern,TEMP$Fd17)==1, TEMP$Fd17, TEMP$FLOOR) 
TEMP$FLOOR<- ifelse(grepl(pattern,TEMP$Fd18)==1, TEMP$Fd18, TEMP$FLOOR) 
TEMP$FLOOR<- ifelse(grepl(pattern,TEMP$Fd19)==1, TEMP$Fd19, TEMP$FLOOR) 
TEMP$FLOOR<- ifelse(grepl(pattern,TEMP$Fd20)==1, TEMP$Fd20, TEMP$FLOOR) 

pattern<-"Heating source:\n"
TEMP$HEATING<- ifelse(grepl(pattern,TEMP$Fd1)==1, TEMP$Fd1, "") 
TEMP$HEATING<- ifelse(grepl(pattern,TEMP$Fd2)==1, TEMP$Fd2, TEMP$HEATING) 
TEMP$HEATING<- ifelse(grepl(pattern,TEMP$Fd3)==1, TEMP$Fd3, TEMP$HEATING) 
TEMP$HEATING<- ifelse(grepl(pattern,TEMP$Fd4)==1, TEMP$Fd4, TEMP$HEATING) 
TEMP$HEATING<- ifelse(grepl(pattern,TEMP$Fd5)==1, TEMP$Fd5, TEMP$HEATING) 
TEMP$HEATING<- ifelse(grepl(pattern,TEMP$Fd6)==1, TEMP$Fd6, TEMP$HEATING) 
TEMP$HEATING<- ifelse(grepl(pattern,TEMP$Fd7)==1, TEMP$Fd7, TEMP$HEATING) 
TEMP$HEATING<- ifelse(grepl(pattern,TEMP$Fd8)==1, TEMP$Fd8, TEMP$HEATING) 
TEMP$HEATING<- ifelse(grepl(pattern,TEMP$Fd9)==1, TEMP$Fd9, TEMP$HEATING) 
TEMP$HEATING<- ifelse(grepl(pattern,TEMP$Fd10)==1, TEMP$Fd10, TEMP$HEATING) 
TEMP$HEATING<- ifelse(grepl(pattern,TEMP$Fd11)==1, TEMP$Fd11, TEMP$HEATING) 
TEMP$HEATING<- ifelse(grepl(pattern,TEMP$Fd12)==1, TEMP$Fd12, TEMP$HEATING) 
TEMP$HEATING<- ifelse(grepl(pattern,TEMP$Fd13)==1, TEMP$Fd13, TEMP$HEATING) 
TEMP$HEATING<- ifelse(grepl(pattern,TEMP$Fd14)==1, TEMP$Fd14, TEMP$HEATING) 
TEMP$HEATING<- ifelse(grepl(pattern,TEMP$Fd15)==1, TEMP$Fd15, TEMP$HEATING) 
TEMP$HEATING<- ifelse(grepl(pattern,TEMP$Fd16)==1, TEMP$Fd16, TEMP$HEATING) 
TEMP$HEATING<- ifelse(grepl(pattern,TEMP$Fd17)==1, TEMP$Fd17, TEMP$HEATING) 
TEMP$HEATING<- ifelse(grepl(pattern,TEMP$Fd18)==1, TEMP$Fd18, TEMP$HEATING) 
TEMP$HEATING<- ifelse(grepl(pattern,TEMP$Fd19)==1, TEMP$Fd19, TEMP$HEATING) 
TEMP$HEATING<- ifelse(grepl(pattern,TEMP$Fd20)==1, TEMP$Fd20, TEMP$HEATING) 

pattern<-"Kitchen:\n"
TEMP$KITCHEN<- ifelse(grepl(pattern,TEMP$Fd1)==1, TEMP$Fd1, "") 
TEMP$KITCHEN<- ifelse(grepl(pattern,TEMP$Fd2)==1, TEMP$Fd2, TEMP$KITCHEN) 
TEMP$KITCHEN<- ifelse(grepl(pattern,TEMP$Fd3)==1, TEMP$Fd3, TEMP$KITCHEN) 
TEMP$KITCHEN<- ifelse(grepl(pattern,TEMP$Fd4)==1, TEMP$Fd4, TEMP$KITCHEN) 
TEMP$KITCHEN<- ifelse(grepl(pattern,TEMP$Fd5)==1, TEMP$Fd5, TEMP$KITCHEN) 
TEMP$KITCHEN<- ifelse(grepl(pattern,TEMP$Fd6)==1, TEMP$Fd6, TEMP$KITCHEN) 
TEMP$KITCHEN<- ifelse(grepl(pattern,TEMP$Fd7)==1, TEMP$Fd7, TEMP$KITCHEN) 
TEMP$KITCHEN<- ifelse(grepl(pattern,TEMP$Fd8)==1, TEMP$Fd8, TEMP$KITCHEN) 
TEMP$KITCHEN<- ifelse(grepl(pattern,TEMP$Fd9)==1, TEMP$Fd9, TEMP$KITCHEN) 
TEMP$KITCHEN<- ifelse(grepl(pattern,TEMP$Fd10)==1, TEMP$Fd10, TEMP$KITCHEN) 
TEMP$KITCHEN<- ifelse(grepl(pattern,TEMP$Fd11)==1, TEMP$Fd11, TEMP$KITCHEN) 
TEMP$KITCHEN<- ifelse(grepl(pattern,TEMP$Fd12)==1, TEMP$Fd12, TEMP$KITCHEN) 
TEMP$KITCHEN<- ifelse(grepl(pattern,TEMP$Fd13)==1, TEMP$Fd13, TEMP$KITCHEN) 
TEMP$KITCHEN<- ifelse(grepl(pattern,TEMP$Fd14)==1, TEMP$Fd14, TEMP$KITCHEN) 
TEMP$KITCHEN<- ifelse(grepl(pattern,TEMP$Fd15)==1, TEMP$Fd15, TEMP$KITCHEN) 
TEMP$KITCHEN<- ifelse(grepl(pattern,TEMP$Fd16)==1, TEMP$Fd16, TEMP$KITCHEN) 
TEMP$KITCHEN<- ifelse(grepl(pattern,TEMP$Fd17)==1, TEMP$Fd17, TEMP$KITCHEN) 
TEMP$KITCHEN<- ifelse(grepl(pattern,TEMP$Fd18)==1, TEMP$Fd18, TEMP$KITCHEN) 
TEMP$KITCHEN<- ifelse(grepl(pattern,TEMP$Fd19)==1, TEMP$Fd19, TEMP$KITCHEN) 
TEMP$KITCHEN<- ifelse(grepl(pattern,TEMP$Fd20)==1, TEMP$Fd20, TEMP$KITCHEN) 

pattern<-"Bathroom:\n"
TEMP$SHOWERANDBATH<- ifelse(grepl(pattern,TEMP$Fd1)==1, TEMP$Fd1, "") 
TEMP$SHOWERANDBATH<- ifelse(grepl(pattern,TEMP$Fd2)==1, TEMP$Fd2, TEMP$SHOWERANDBATH) 
TEMP$SHOWERANDBATH<- ifelse(grepl(pattern,TEMP$Fd3)==1, TEMP$Fd3, TEMP$SHOWERANDBATH) 
TEMP$SHOWERANDBATH<- ifelse(grepl(pattern,TEMP$Fd4)==1, TEMP$Fd4, TEMP$SHOWERANDBATH) 
TEMP$SHOWERANDBATH<- ifelse(grepl(pattern,TEMP$Fd5)==1, TEMP$Fd5, TEMP$SHOWERANDBATH) 
TEMP$SHOWERANDBATH<- ifelse(grepl(pattern,TEMP$Fd6)==1, TEMP$Fd6, TEMP$SHOWERANDBATH) 
TEMP$SHOWERANDBATH<- ifelse(grepl(pattern,TEMP$Fd7)==1, TEMP$Fd7, TEMP$SHOWERANDBATH) 
TEMP$SHOWERANDBATH<- ifelse(grepl(pattern,TEMP$Fd8)==1, TEMP$Fd8, TEMP$SHOWERANDBATH) 
TEMP$SHOWERANDBATH<- ifelse(grepl(pattern,TEMP$Fd9)==1, TEMP$Fd9, TEMP$SHOWERANDBATH) 
TEMP$SHOWERANDBATH<- ifelse(grepl(pattern,TEMP$Fd10)==1, TEMP$Fd10, TEMP$SHOWERANDBATH) 
TEMP$SHOWERANDBATH<- ifelse(grepl(pattern,TEMP$Fd11)==1, TEMP$Fd11, TEMP$SHOWERANDBATH) 
TEMP$SHOWERANDBATH<- ifelse(grepl(pattern,TEMP$Fd12)==1, TEMP$Fd12, TEMP$SHOWERANDBATH) 
TEMP$SHOWERANDBATH<- ifelse(grepl(pattern,TEMP$Fd13)==1, TEMP$Fd13, TEMP$SHOWERANDBATH) 
TEMP$SHOWERANDBATH<- ifelse(grepl(pattern,TEMP$Fd14)==1, TEMP$Fd14, TEMP$SHOWERANDBATH) 
TEMP$SHOWERANDBATH<- ifelse(grepl(pattern,TEMP$Fd15)==1, TEMP$Fd15, TEMP$SHOWERANDBATH) 
TEMP$SHOWERANDBATH<- ifelse(grepl(pattern,TEMP$Fd16)==1, TEMP$Fd16, TEMP$SHOWERANDBATH) 
TEMP$SHOWERANDBATH<- ifelse(grepl(pattern,TEMP$Fd17)==1, TEMP$Fd17, TEMP$SHOWERANDBATH) 
TEMP$SHOWERANDBATH<- ifelse(grepl(pattern,TEMP$Fd18)==1, TEMP$Fd18, TEMP$SHOWERANDBATH) 
TEMP$SHOWERANDBATH<- ifelse(grepl(pattern,TEMP$Fd19)==1, TEMP$Fd19, TEMP$SHOWERANDBATH) 
TEMP$SHOWERANDBATH<- ifelse(grepl(pattern,TEMP$Fd20)==1, TEMP$Fd20, TEMP$SHOWERANDBATH) 


pattern<-"Basement:\n"
TEMP$BASEMENT<- ifelse(grepl(pattern,TEMP$Fd1)==1, TEMP$Fd1, "") 
TEMP$BASEMENT<- ifelse(grepl(pattern,TEMP$Fd2)==1, TEMP$Fd2, TEMP$BASEMENT) 
TEMP$BASEMENT<- ifelse(grepl(pattern,TEMP$Fd3)==1, TEMP$Fd3, TEMP$BASEMENT) 
TEMP$BASEMENT<- ifelse(grepl(pattern,TEMP$Fd4)==1, TEMP$Fd4, TEMP$BASEMENT) 
TEMP$BASEMENT<- ifelse(grepl(pattern,TEMP$Fd5)==1, TEMP$Fd5, TEMP$BASEMENT) 
TEMP$BASEMENT<- ifelse(grepl(pattern,TEMP$Fd6)==1, TEMP$Fd6, TEMP$BASEMENT) 
TEMP$BASEMENT<- ifelse(grepl(pattern,TEMP$Fd7)==1, TEMP$Fd7, TEMP$BASEMENT) 
TEMP$BASEMENT<- ifelse(grepl(pattern,TEMP$Fd8)==1, TEMP$Fd8, TEMP$BASEMENT) 
TEMP$BASEMENT<- ifelse(grepl(pattern,TEMP$Fd9)==1, TEMP$Fd9, TEMP$BASEMENT) 
TEMP$BASEMENT<- ifelse(grepl(pattern,TEMP$Fd10)==1, TEMP$Fd10, TEMP$BASEMENT) 
TEMP$BASEMENT<- ifelse(grepl(pattern,TEMP$Fd11)==1, TEMP$Fd11, TEMP$BASEMENT) 
TEMP$BASEMENT<- ifelse(grepl(pattern,TEMP$Fd12)==1, TEMP$Fd12, TEMP$BASEMENT) 
TEMP$BASEMENT<- ifelse(grepl(pattern,TEMP$Fd13)==1, TEMP$Fd13, TEMP$BASEMENT) 
TEMP$BASEMENT<- ifelse(grepl(pattern,TEMP$Fd14)==1, TEMP$Fd14, TEMP$BASEMENT) 
TEMP$BASEMENT<- ifelse(grepl(pattern,TEMP$Fd15)==1, TEMP$Fd15, TEMP$BASEMENT) 
TEMP$BASEMENT<- ifelse(grepl(pattern,TEMP$Fd16)==1, TEMP$Fd16, TEMP$BASEMENT) 
TEMP$BASEMENT<- ifelse(grepl(pattern,TEMP$Fd17)==1, TEMP$Fd17, TEMP$BASEMENT) 
TEMP$BASEMENT<- ifelse(grepl(pattern,TEMP$Fd18)==1, TEMP$Fd18, TEMP$BASEMENT) 
TEMP$BASEMENT<- ifelse(grepl(pattern,TEMP$Fd19)==1, TEMP$Fd19, TEMP$BASEMENT) 
TEMP$BASEMENT<- ifelse(grepl(pattern,TEMP$Fd20)==1, TEMP$Fd20, TEMP$BASEMENT) 


pattern<-"Pool:\n"
TEMP$POOL<- ifelse(grepl(pattern,TEMP$Fd1)==1, TEMP$Fd1, "") 
TEMP$POOL<- ifelse(grepl(pattern,TEMP$Fd2)==1, TEMP$Fd2, TEMP$POOL) 
TEMP$POOL<- ifelse(grepl(pattern,TEMP$Fd3)==1, TEMP$Fd3, TEMP$POOL) 
TEMP$POOL<- ifelse(grepl(pattern,TEMP$Fd4)==1, TEMP$Fd4, TEMP$POOL) 
TEMP$POOL<- ifelse(grepl(pattern,TEMP$Fd5)==1, TEMP$Fd5, TEMP$POOL) 
TEMP$POOL<- ifelse(grepl(pattern,TEMP$Fd6)==1, TEMP$Fd6, TEMP$POOL) 
TEMP$POOL<- ifelse(grepl(pattern,TEMP$Fd7)==1, TEMP$Fd7, TEMP$POOL) 
TEMP$POOL<- ifelse(grepl(pattern,TEMP$Fd8)==1, TEMP$Fd8, TEMP$POOL) 
TEMP$POOL<- ifelse(grepl(pattern,TEMP$Fd9)==1, TEMP$Fd9, TEMP$POOL) 
TEMP$POOL<- ifelse(grepl(pattern,TEMP$Fd10)==1, TEMP$Fd10, TEMP$POOL) 
TEMP$POOL<- ifelse(grepl(pattern,TEMP$Fd11)==1, TEMP$Fd11, TEMP$POOL) 
TEMP$POOL<- ifelse(grepl(pattern,TEMP$Fd12)==1, TEMP$Fd12, TEMP$POOL) 
TEMP$POOL<- ifelse(grepl(pattern,TEMP$Fd13)==1, TEMP$Fd13, TEMP$POOL) 
TEMP$POOL<- ifelse(grepl(pattern,TEMP$Fd14)==1, TEMP$Fd14, TEMP$POOL) 
TEMP$POOL<- ifelse(grepl(pattern,TEMP$Fd15)==1, TEMP$Fd15, TEMP$POOL) 
TEMP$POOL<- ifelse(grepl(pattern,TEMP$Fd16)==1, TEMP$Fd16, TEMP$POOL) 
TEMP$POOL<- ifelse(grepl(pattern,TEMP$Fd17)==1, TEMP$Fd17, TEMP$POOL) 
TEMP$POOL<- ifelse(grepl(pattern,TEMP$Fd18)==1, TEMP$Fd18, TEMP$POOL) 
TEMP$POOL<- ifelse(grepl(pattern,TEMP$Fd19)==1, TEMP$Fd19, TEMP$POOL) 
TEMP$POOL<- ifelse(grepl(pattern,TEMP$Fd20)==1, TEMP$Fd20, TEMP$POOL) 


pattern<-"Garage:\n"
TEMP$GAR<- ifelse(grepl(pattern,TEMP$Fd1)==1, TEMP$Fd1, "") 
TEMP$GAR<- ifelse(grepl(pattern,TEMP$Fd2)==1, TEMP$Fd2, TEMP$GAR) 
TEMP$GAR<- ifelse(grepl(pattern,TEMP$Fd3)==1, TEMP$Fd3, TEMP$GAR) 
TEMP$GAR<- ifelse(grepl(pattern,TEMP$Fd4)==1, TEMP$Fd4, TEMP$GAR) 
TEMP$GAR<- ifelse(grepl(pattern,TEMP$Fd5)==1, TEMP$Fd5, TEMP$GAR) 
TEMP$GAR<- ifelse(grepl(pattern,TEMP$Fd6)==1, TEMP$Fd6, TEMP$GAR) 
TEMP$GAR<- ifelse(grepl(pattern,TEMP$Fd7)==1, TEMP$Fd7, TEMP$GAR) 
TEMP$GAR<- ifelse(grepl(pattern,TEMP$Fd8)==1, TEMP$Fd8, TEMP$GAR) 
TEMP$GAR<- ifelse(grepl(pattern,TEMP$Fd9)==1, TEMP$Fd9, TEMP$GAR) 
TEMP$GAR<- ifelse(grepl(pattern,TEMP$Fd10)==1, TEMP$Fd10, TEMP$GAR) 
TEMP$GAR<- ifelse(grepl(pattern,TEMP$Fd11)==1, TEMP$Fd11, TEMP$GAR) 
TEMP$GAR<- ifelse(grepl(pattern,TEMP$Fd12)==1, TEMP$Fd12, TEMP$GAR) 
TEMP$GAR<- ifelse(grepl(pattern,TEMP$Fd13)==1, TEMP$Fd13, TEMP$GAR) 
TEMP$GAR<- ifelse(grepl(pattern,TEMP$Fd14)==1, TEMP$Fd14, TEMP$GAR) 
TEMP$GAR<- ifelse(grepl(pattern,TEMP$Fd15)==1, TEMP$Fd15, TEMP$GAR) 
TEMP$GAR<- ifelse(grepl(pattern,TEMP$Fd16)==1, TEMP$Fd16, TEMP$GAR) 
TEMP$GAR<- ifelse(grepl(pattern,TEMP$Fd17)==1, TEMP$Fd17, TEMP$GAR) 
TEMP$GAR<- ifelse(grepl(pattern,TEMP$Fd18)==1, TEMP$Fd18, TEMP$GAR) 
TEMP$GAR<- ifelse(grepl(pattern,TEMP$Fd19)==1, TEMP$Fd19, TEMP$GAR) 
TEMP$GAR<- ifelse(grepl(pattern,TEMP$Fd20)==1, TEMP$Fd20, TEMP$GAR) 

pattern<-"Location:\n"
TEMP$LOCATION<- ifelse(grepl(pattern,TEMP$Fd1)==1, TEMP$Fd1, "") 
TEMP$LOCATION<- ifelse(grepl(pattern,TEMP$Fd2)==1, TEMP$Fd2, TEMP$LOCATION) 
TEMP$LOCATION<- ifelse(grepl(pattern,TEMP$Fd3)==1, TEMP$Fd3, TEMP$LOCATION) 
TEMP$LOCATION<- ifelse(grepl(pattern,TEMP$Fd4)==1, TEMP$Fd4, TEMP$LOCATION) 
TEMP$LOCATION<- ifelse(grepl(pattern,TEMP$Fd5)==1, TEMP$Fd5, TEMP$LOCATION) 
TEMP$LOCATION<- ifelse(grepl(pattern,TEMP$Fd6)==1, TEMP$Fd6, TEMP$LOCATION) 
TEMP$LOCATION<- ifelse(grepl(pattern,TEMP$Fd7)==1, TEMP$Fd7, TEMP$LOCATION) 
TEMP$LOCATION<- ifelse(grepl(pattern,TEMP$Fd8)==1, TEMP$Fd8, TEMP$LOCATION) 
TEMP$LOCATION<- ifelse(grepl(pattern,TEMP$Fd9)==1, TEMP$Fd9, TEMP$LOCATION) 
TEMP$LOCATION<- ifelse(grepl(pattern,TEMP$Fd10)==1, TEMP$Fd10, TEMP$LOCATION) 
TEMP$LOCATION<- ifelse(grepl(pattern,TEMP$Fd11)==1, TEMP$Fd11, TEMP$LOCATION) 
TEMP$LOCATION<- ifelse(grepl(pattern,TEMP$Fd12)==1, TEMP$Fd12, TEMP$LOCATION) 
TEMP$LOCATION<- ifelse(grepl(pattern,TEMP$Fd13)==1, TEMP$Fd13, TEMP$LOCATION) 
TEMP$LOCATION<- ifelse(grepl(pattern,TEMP$Fd14)==1, TEMP$Fd14, TEMP$LOCATION) 
TEMP$LOCATION<- ifelse(grepl(pattern,TEMP$Fd15)==1, TEMP$Fd15, TEMP$LOCATION) 
TEMP$LOCATION<- ifelse(grepl(pattern,TEMP$Fd16)==1, TEMP$Fd16, TEMP$LOCATION) 
TEMP$LOCATION<- ifelse(grepl(pattern,TEMP$Fd17)==1, TEMP$Fd17, TEMP$LOCATION) 
TEMP$LOCATION<- ifelse(grepl(pattern,TEMP$Fd18)==1, TEMP$Fd18, TEMP$LOCATION) 
TEMP$LOCATION<- ifelse(grepl(pattern,TEMP$Fd19)==1, TEMP$Fd19, TEMP$LOCATION) 
TEMP$LOCATION<- ifelse(grepl(pattern,TEMP$Fd20)==1, TEMP$Fd20, TEMP$LOCATION) 

TEMP$FAC_BRICK<- ifelse(grepl("Brick\n",TEMP$FACING)==1, 1, 0) 
TEMP$FAC_STONE<- ifelse(grepl("Stone\n",TEMP$FACING)==1, 1, 0) 
TEMP$FAC_VINYL<- ifelse(grepl("Vinyl Siding\n",TEMP$FACING)==1, 1, 0)
TEMP$FAC_WOOD<- ifelse(grepl("Wood\n",TEMP$FACING)==1, 1, 0)
TEMP$FAC_ALLUMINUIUM<- ifelse(grepl("Aluminium Siding\n",TEMP$FACING)==1, 1, 0)



TEMP$FLOOR_CERAMIC<- ifelse(grepl("Ceramic\n",TEMP$FLOOR)==1, 1, 0) 
TEMP$FLOOR_HARDWOOD<- ifelse(grepl("Hardwood\n",TEMP$FLOOR)==1, 1, 0) 
TEMP$FLOOR_LAMINATE<- ifelse(grepl("Laminate\n",TEMP$FLOOR)==1, 1, 0)

TEMP$HEATING_FORCEDAIR<- ifelse(grepl("Forced air\n",TEMP$HEATING)==1, 1, 0) 
TEMP$HEATING_NATURALGAS<- ifelse(grepl("Natural gas\n",TEMP$HEATING)==1, 1, 0) 
TEMP$HEATING_ELECTRICITY<- ifelse(grepl("Electric\n",TEMP$HEATING)==1, 1, 0)

TEMP$BASEMENT_FINISH<- ifelse(grepl("Totally finished\n",TEMP$BASEMENT)==1, 1, 0) 

TEMP$POOL_ABOVE<- ifelse(grepl("Above ground\n",TEMP$POOL)==1, 1, 0) 
TEMP$POOL_IN<- ifelse(grepl("Inground\n",TEMP$POOL)==1, 1, 0) 

TEMP$GAR_SINGLE<- ifelse(grepl("Single\n",TEMP$GAR)==1, 1, 0) 
TEMP$GAR_DOUBLE<- ifelse(grepl("Double\n",TEMP$GAR)==1, 1, 0) 


TEMP$LOCATION_NOBACK<- ifelse(grepl("No backyard neighbors\n",TEMP$LOCATION)==1, 1, 0) 
TEMP$LOCATION_RESIDENTIAL<- ifelse(grepl("Residential area\n",TEMP$LOCATION)==1, 1, 0) 
TEMP$LOCATION_PARK<- ifelse(grepl("Near park\n",TEMP$LOCATION)==1, 1, 0) 
TEMP$LOCATION_PUBLICT<- ifelse(grepl("Public transportation\n",TEMP$LOCATION)==1, 1, 0) 
TEMP$LOCATION_HIGHWAY<- ifelse(grepl("Highway access\n",TEMP$LOCATION)==1, 1, 0) 
TEMP$KITCHEN_ISLAND<- ifelse(grepl("Island\n",TEMP$KITCHEN)==1, 1, 0) 


TEMP$type=ifelse(TEMP$type=="New 2 Storey", "2 Storey",TEMP$type)
TEMP$type=ifelse(TEMP$type=="New Bungalow", "Bungalow",TEMP$type)
TEMP$type=ifelse(TEMP$type=="New Semi-detached", "Semi-detached",TEMP$type)
TEMP$type=ifelse(TEMP$type=="New Townhouse", "Townhouse",TEMP$type)
TEMP$type=ifelse(TEMP$type=="New 1  1/2  Storey", "1  1/2  Storey",TEMP$type)



DB_REGRESS=as.data.frame(cbind(TEMP$price,TEMP$type,TEMP$CITY1,TEMP$bedrooms,TEMP$bathrooms,TEMP$LA,TEMP$LD,TEMP$AGE,TEMP$YEAR,TEMP$POOL_ABOVE,TEMP$POOL_IN,TEMP$FAC_BRICK,TEMP$FAC_STONE,TEMP$FLOOR_CERAMIC,TEMP$FLOOR_HARDWOOD,TEMP$GAR_DOUBLE,TEMP$GAR_SINGLE,TEMP$HEATING_ELECTRICITY,TEMP$HEATING_NATURALGAS,TEMP$BASEMENT_FINISH,TEMP$LOCATION_HIGHWAY,TEMP$LOCATION_NOBACK,TEMP$LOCATION_RESIDENTIAL,TEMP$LOCATION_PUBLICT,TEMP$KITCHEN_ISLAND))
names(DB_REGRESS)[1]<-"PRICE"
names(DB_REGRESS)[2]<-"TYPE"
names(DB_REGRESS)[3]<-"CITY"
names(DB_REGRESS)[4]<-"BED"
names(DB_REGRESS)[5]<-"BATH"
names(DB_REGRESS)[6]<-"LA"
names(DB_REGRESS)[7]<-"LD"
names(DB_REGRESS)[8]<-"AGE"
names(DB_REGRESS)[9]<-"YOS"
names(DB_REGRESS)[10]<-"POOL_ABOVE"
names(DB_REGRESS)[11]<-"POOL_IN"
names(DB_REGRESS)[12]<-"BRICK"
names(DB_REGRESS)[13]<-"STONE"
names(DB_REGRESS)[14]<-"CERAMIC"
names(DB_REGRESS)[15]<-"HARDWOOD"
names(DB_REGRESS)[16]<-"GAR_DOUBLE"
names(DB_REGRESS)[17]<-"GAR_SINGLE"
names(DB_REGRESS)[18]<-"ELECTRICITY"
names(DB_REGRESS)[19]<-"NATURALGAS"
names(DB_REGRESS)[20]<-"BASEMENT_FINISH"
names(DB_REGRESS)[21]<-"HIGHWAY"
names(DB_REGRESS)[22]<-"NOBACK"
names(DB_REGRESS)[23]<-"RESIDENTIAL"
names(DB_REGRESS)[24]<-"PUBLICT"
names(DB_REGRESS)[25]<-"KITCHENISLAND"

DB_REGRESS$PRICE=as.numeric(DB_REGRESS$PRICE)
DB_REGRESS$BED=as.numeric(DB_REGRESS$BED)
DB_REGRESS$BATH=as.numeric(DB_REGRESS$BATH)
DB_REGRESS$LA=as.numeric(DB_REGRESS$LA)
DB_REGRESS$LD=as.numeric(DB_REGRESS$LD)
DB_REGRESS$AGE=as.numeric(DB_REGRESS$AGE) 
DB_REGRESS$TYPE=as.factor(DB_REGRESS$TYPE)
DB_REGRESS$YOS=as.factor(DB_REGRESS$YOS)
DB_REGRESS$CITY=as.factor(DB_REGRESS$CITY)
YY=na.omit(DB_REGRESS)
save(YY,file = "BD_HEDONIC.Rdata")
HOUSING_MODEL<-lm(PRICE ~ BED + BATH + LA + LD + AGE + POOL_ABOVE + POOL_IN + GAR_DOUBLE + GAR_SINGLE + BASEMENT_FINISH + BRICK + STONE + CERAMIC + HARDWOOD + HIGHWAY + NOBACK + RESIDENTIAL + PUBLICT+ KITCHENISLAND + TYPE + YOS + CITY ,data = YY)
summary(HOUSING_MODEL)
HEDONIC_COEF=as.data.frame(HOUSING_MODEL$coefficients)

HEDONIC_COEF$NAME=row.names(HEDONIC_COEF)
write.table(HEDONIC_COEF,file = "HEDONIC_COEF.txt")

# The code performed data cleaning and feature engineering on a real estate dataset. It creates new variables and cleans existing ones to prepare the data for analysis or modeling.

