## Project: Health Outcomes Analytics SPH ##
## Source: PMR Operational Main Dataset - PowerBi
## September 2024 ##

# Install packages & lib -------------------------------------------------------
install.packages(c("dplyr","wordcloud","RColorBrewer","tm","SnowballC","reshape2","psych","readr"))
install.packages(c("NLP", "magrittr","tidytuesdayR","tidyverse"))
install.packages("pastecs")
install.packages("plyr")
#devtools::install_github("lepennec/ggwordcloud")

install.packages("ggplot2")
install.packages("ggwordcloud")
install.packages("pastecs")
install.packages("vangogh")

library(dplyr)
library(wordcloud)
library(RColorBrewer)
library(tm)
library(SnowballC)
library(reshape2)
library(psych)
library(readr)
library(pastecs)

library(NLP)
library(magrittr)
library(tidytuesdayR)
library(tidyverse)
library(pastecs)
library(plyr)
library(ggplot2)
library(ggwordcloud)
library(vangogh)

### Path & load file ### -------------------------------------------------------
path <- setwd("C:/Users/ncber/OneDrive/Documentos/R-code-health-outcome-analytics")
file <- "/input/profiles.csv" #Tab: Operation Profile

profiles <-readr::read_csv(paste0(path,file,sep=""), show_col_types = FALSE) # From tableau
profiles <-profiles[with(profiles,order(`Project Number`,`Cycle ID`)),]

profilesx <-readr::read_csv(paste0(path,file,sep=""), show_col_types = FALSE) # From tableau
profilesx <-profilesx[with(profilesx,order(`Project Number`,`Cycle ID`)),]

# Leave the record of the first Convergence (oldest) cycle only
profiles$`Cycle ID`<-as.integer(profiles$`Cycle ID`)
profiles$min.cycle <-ave(profiles$`Cycle ID`,profiles$`Project Number`,FUN = min)
summary(profiles$`Cycle ID`)
profiles <- profiles[profiles$`Cycle ID`==profiles$min.cycle,]
profiles$min.cycle <-NULL

# Keep only health
profiles <- profiles[grepl("HEALTH-|e-HEALTH",profiles$Sector,ignore.case = T),]

# Drop grants and policy loans
profiles <- profiles[profiles$`Operation Type`=="Loan Operation",]
profiles <- profiles[profiles$`Lending Instrument`=="Investment Loan",]
#profiles$is.relevant<-profiles[profiles$`Lending Instrument`=="Investment Loan",]

table(profiles$`Operation Type`) # 58 projects of health & e-health check
table(profiles$`Lending Instrument`) # 58 projects of health & e-health check

# Filter columns
names(profiles)
#profiles <-profiles[-c(2,3,6,7,10,15,20:21,23)] # delete non-important columns
profiles<-profiles[c(1,4,5,8,9,14,17,22)]
print(profiles)
View(profiles)

profiles_c<-profiles[c(2,3,6:8)]

### General Development Objectives and Indicators ------------------------------

# Load Impacts (long term dev objectives)
file <- "/input/impact.csv" #Tab: General Development Objectives
impact <- readr::read_csv(paste0(path,file,sep=""), show_col_types = FALSE) #, encoding="UTF-8") # From tableau

# General Development Objective
g.ind <-unique(impact[c(2,4,8,9,11,14)])
#g.ind <-unique(impact[c(2,4,11)])
rm(impact)

# Bring name of operation from profile and reorder
#g.ind<-merge(profiles, g.ind[c(1:6)], by="Project Number", all.x = T)
g.ind<-merge(profiles, g.ind, by="Project Number", all.x = T)
g.ind$Long.op.name <-paste0(g.ind$`Project Number`," - ",g.ind$`Operation Name`)
g.ind$indicator_unit <-paste0(g.ind$Indicators," - ",g.ind$`Unit of Measure`)

#g.ind <-merge(x=g.ind, y=profiles, by.x="Project Number", by.y="Project Number", fill= -9999)
#exporttab <- merge(x=dwd_nogap, y=dwd_gap, by.x='x1', by.y='x2', fill=-9999)

# Add plus sign to differentiate different lines when text in merged
g.ind$`General Development Objective` <-paste0("+",g.ind$`General Development Objective`)
g.ind$indicator_unit <-paste("+",g.ind$indicator_unit)

#renaming variables
names(g.ind)
names(g.ind)[names(g.ind)=="General Development Objective"]<-"objectives"
names(g.ind)[names(g.ind)=="indicator_unit"]<-"objectives_indicators_units"
names(g.ind)[names(g.ind)=="Indicators"]<-"objectives_indicators"
names(g.ind)[names(g.ind)=="Unit of Measure"]<-"objectives_units"

# Create df to keep first (oldest) baseline year for each project
x <-g.ind[c(1,12)]                
x$`Baseline Year` <-as.integer(x$`Baseline Year`)
x$min.cycle <-ave(x$`Baseline Year`,x$`Project Number`,FUN = min)
x <-x[x$`Baseline Year`==x$min.cycle,]
x$min.cycle <-NULL
x <-x[!duplicated(x),]

summary(x$`Baseline Year`) #freq min baseline year

g.ind_u <-merge(x,g.ind,by =c("Project Number", "Baseline Year"),all.y = T) #Min baseline year
g.ind_u <-unique(g.ind_u)

# Drop everything and keep g.ind as main dataframe
g.ind <- g.ind_u
rm(g.ind_u)

## Outcomes --------------------------------------------------------------------

file <- "/input/Outcomes.csv" #Tab: Specific Development Objectives
#outcomes <- readxl::read_excel(paste0(path,file,sep=""))
outcomes <- readr::read_csv(paste0(path,file,sep=""), show_col_types = FALSE) # From tableau

install.packages("plyr")
install.packages("tidyr")
library(plyr)
library(tidyr)
library(dplyr)
library(dbplyr)
library(plyr)

# Specific Development Objectives and Indicators
#s.obj <-unique(outcomes[c(2,4)])
names(outcomes)
s.ind <-unique(outcomes[c(2,4,10,11,13,14)])
#rm(outcomes)

s.ind<-merge(profiles[c(2,3,6:8)], s.ind[c(1:6)], by="Project Number", all.x = T)
s.ind_u<-unique(s.ind)

# Oldest cycle of reporting
y <-s.ind[c(1,9)]                
y$`Baseline Year` <-as.integer(y$`Baseline Year`)
y$min.cycle <-ave(y$`Baseline Year`,y$`Project Number`,FUN = min)
y <-y[y$`Baseline Year`==y$min.cycle,]
y$min.cycle <-NULL
y <-y[!duplicated(y),]

summary(y$`Baseline Year`) #freq min baseline year

s.ind_u <- merge(y,s.ind_u,by =c("Project Number", "Baseline Year"),all.y = T) #Min baseline year

# Drop everything and keep s.ind as main dataframe
s.indx<-na.omit(s.ind_u)

s.ind<-unique(s.indx)
#rm(s.ind_a, s.ind_b, s.ind_c, s.ind_d, s.ind_e, s.ind_u, s.indx)

#s.ind  <-merge(s.ind,profiles[c(1,2)],by="Project Number",all.x = T)
s.ind$Long.op.name <-paste0(s.ind$`Project Number`," - ",s.ind$`Operation Name`)

names(s.ind)[names(s.ind)=="Specific Development objectives"]<-"outcomes"
names(s.ind)[names(s.ind)=="indicator_unit_means"]<-"outcomes_indicators_units"
names(s.ind)[names(s.ind)=="Indicator Definition"]<-"outcomes_indicators"
names(s.ind)[names(s.ind)=="Unit of Measure"]<-"outcomes_units"
names(s.ind)[names(s.ind)=="Means of Verification"]<-"outcomes_means"
names(s.ind)[names(s.ind)=="Project Number"]<-"project_number"

# Bring general objectives
#s.ind<-merge(profiles, s.ind[c(1:7)], by="Project Number", all.x = T)
g.ind<-unique(g.ind)

names(g.ind)[names(g.ind)=="Project Number"]<-"project_number"

#s.ind_x <-merge(s.ind,g.ind[c(1:8)],by = c("Project Number"), all.x = T)
s.ind_x <-merge(s.ind, g.ind[c(1,10)], by = c("project_number"), all.x = T)

s.uqx<-unique(s.ind_x)

s.uq<-s.uqx[!is.na(s.uqx$`outcomes`),] # eliminate operations with no information of outcomes

#s.ind_t<-unique(s.uq[-c(9,12,16)])

s.ind <- s.uqx #unique database
rm(s.uqx, s.uq)

data_r01<-s.ind[c(1,4,2,11,12,5,7:10,6)]

names(data_r01)[names(data_r01)=="Is Reformulated"]<-"reformulated"

openxlsx::write.xlsx(data_r01,file = paste0(path,"/data_r01.xlsx"))
write.csv(data_r01,file = paste0(path,"/data_r01.csv"), fileEncoding="UTF-8")
#write.csv(g.ind,file = paste0(path_ext,"/data_r01.csv"), fileEncoding="UTF-8") #Path in OneDrive

file <- "/data_r01.xlsx" #Tab: Operation Profile
data_r01 <- readxl::read_excel(paste0(path,file,sep=""))

## dummy for malaria and COVID
data_r01$health  <-grepl("salud|health|saúde|morbi|morta|death|disease",paste(data_r01$Long.op.name,data_r01$Objective,data_r01$objectives),ignore.case = T)
data_r01$malaria <-grepl("malaria",paste(data_r01$Long.op.name,data_r01$Objective,data_r01$objectives),ignore.case = T)
data_r01$covid   <-grepl("covid|coronavirus",paste(data_r01$Long.op.name,data_r01$Objective,data_r01$objectives),ignore.case = T)
#data_r01$is.relevant   <-grepl("covid|coronavirus",paste(data_r01$Long.op.name,data_r01$Objective,data_r01$objectives),ignore.case = T)

table(data_r01$project_number,data_r01$covid)
table(data_r01$project_number,data_r01$malaria)

table(data_r01$project_number) #45 projects

#AR-L1326 finances COVID
#AR-L1340 finances COVID
#BH-L1053 was reformulated to finance COVID 
#BL-L1036 finances COVID
#EC-L1270 finances COVID
#HO-L1199 was reformulated to finance COVID 
#NI-L1161 finances COVID
#NI-L1143 finances malaria

data_r01$covid[which(data_r01$project_number=="BL-L1036")] <- TRUE #prototipo COVID
data_r01$covid[which(data_r01$project_number=="EC-L1270")] <- TRUE #prototipo COVID
data_r01$covid[which(data_r01$project_number=="HO-L1199")] <- TRUE #prototipo COVID
data_r01$covid[which(data_r01$project_number=="NI-L1161")] <- TRUE #prototipo COVID
data_r01$reformulated[which(data_r01$project_number=="BH-L1053")] <- "YES" #reformulado
data_r01$reformulated[which(data_r01$project_number=="HO-L1199")] <- "YES" #reformulado

table(data_r01$project_number) #41 projects

no_covid <- data_r01[data_r01$covid=="FALSE",]
table(no_covid$project_number) #35 projects no covid

table(data_r01$reformulated, data_r01$project_number) #2 reformulated
table(data_r01$malaria, data_r01$project_number) #1 project

### Conciliación de Ramiro and Neili tables

#AR-L1358 is LBR (Loan Based on Results) - NCB , Quitar
#AR-L1196 is (Specific Investment Operation) - RCG, Incluir
#AR-L1340 is LBR (Loan Based on Results) - RCG
#BH-L1053 reformulada no COVID - RCG
#BR-L1606 is ESP (Specific Investment Operation) - NCB
#DR-L1069 is ESP (Specific Investment Operation) - NCB
#GU-L1183 is PBP (Programmatic Policy Base Loan) - RCG
#HO-L1105 is ESP (Specific Investment Operation) - RCG, Social Investment, no incluir
#JA-L1049 reformulada no COVID - RCG
#NI-L1143 is GOM (Global of Multiple Works Operation) - NCB, quitar
#NI-L1095 is ESP (Specific Investment Operation) - RCG, 
#PR-L1190 is ESP (Specific Investment Operation) - NCB

data_r02 <- no_covid[no_covid$reformulated=="NO",]
data_r02 <- data_r02[data_r02$project_number!="AR-L1358",]
data_r02 <- data_r02[data_r02$project_number!="NI-L1143",]

table(data_r02$covid, data_r02$project_number) #33 projects

openxlsx::write.xlsx(data_r02,file = paste0(path,"/data_r02.xlsx"))
write.csv(data_r02,file = paste0(path,"/data_r02.csv"), fileEncoding="UTF-8")

data_outcomes<-unique(data_r02[c(1,7)])

openxlsx::write.xlsx(data_outcomes,file = paste0(path,"/data_outcomes.xlsx"))
write.csv(data_outcomes,file = paste0(path,"/data_outcomes.csv"), fileEncoding="UTF-8")

### Merge with keywords ### ----------------------------------------------------
file <- "/input/outcomes_updated.xlsx" #Tab: Operation Profile
keywords <- readxl::read_excel(paste0(path,file,sep=""))

names(keywords)[names(keywords)=="outcomes_original"]<-"outcomes"
names(g.ind)[names(g.ind)=="outcomes_original"]<-"outcomes"

data_r03<-merge(data_r02, keywords, by=c("project_number", "outcomes"), all.x = T)

data_r03<-data_r03[-c(11)]

# Drop projects with outcomes compuestos
openxlsx::write.xlsx(data_r03,file = paste0(path,"/data_r03.xlsx"))
write.csv(data_r03,file = paste0(path,"/data_r03.csv"), fileEncoding="UTF-8")


### Source Data ### ----------------------------------------------------------------
file <- "/input/source_data.xlsx" #Tab: Operation Profile
source_data <- readxl::read_excel(paste0(path,file,sep=""))

names(source_data)[names(source_data)=="Project Number"]<-"project_number"

data01<-merge(source_data[-c(15:22)], keywords, by=c("project_number"), all.x = T)

data01<-data01[c(1,4,5:11,16:21,12:15)]

openxlsx::write.xlsx(data01,file = paste0(path,"/data_revised01.xlsx"))
write.csv(s.ind,file = paste0(path,"/data_revised01.csv"), fileEncoding="UTF-8")


### Check requirements ### ----------------------------------------------------------------
# Classify projects by outcomes/Specific development objectives
data_revised2$health     <-grepl("salud|health|saúde|diseas-|chronic|public|públic-|nutri-|illness",paste(db_f$Long.op.name,db_f$outcomes),ignore.case = T)
data_revised2$morbilityo  <-grepl("enferm-|mortalidad|mortality|mortalidade|morbilidad|morbidity|morbilidade|death|muerte",paste(db_f$Long.op.name,db_f$outcomes),ignore.case = T)
data_revised2$malaria    <-grepl("malaria",paste(db_f$Long.op.name,db_f$outcomes),ignore.case = T)
data_revised2$covid      <-grepl("covid|coronavirus",paste(db_f$Long.op.name,db_f$outcomes),ignore.case = T)
data_revised2$acces      <-grepl("acceso|access|acesso|-cobertura|cobertura|atenção",paste(db_f$Long.op.name,db_f$outcomes),ignore.case = T)
data_revised2$efficiency <-grepl("eficiencia|efficiency|efficient|efectividad",paste(db_f$Long.op.name,db_f$outcomes),ignore.case = T)
data_revised2$equity     <-grepl("equidad|equity|equidade",paste(db_f$Long.op.name,db_f$outcomes),ignore.case = T)
data_revised2$quality    <-grepl("calidad|quality|qualidade",paste(db_f$Long.op.name,db_f$outcomes),ignore.case = T)
data_revised2$e_health   <-grepl("digital|tele-|technology|tecnolog-",paste(db_f$Long.op.name,db_f$outcomes),ignore.case = T)
data_revised2$network    <-grepl("red|system|information|coordina-|integrate",paste(db_f$Long.op.name,db_f$outcomes),ignore.case = T)



### Colapse ### ----------------------------------------------------------------
path <- setwd("C:/Users/ncber/OneDrive/Documentos/R-code-health-outcome-analytics")
file <- "/input/data_revised2.xlsx" #Tab: Operation Profile
data_revised2 <- readxl::read_excel(paste0(path,file,sep=""))


xy <- data.frame(ddply(data_revised2, .(`project_number`), summarize, objectives_units = paste(`outcomes_indicators`, collapse = "\n")))



