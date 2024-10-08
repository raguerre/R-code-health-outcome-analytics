## Project: Health Outcomes Analytics SPH ##
## Source: PMR Operational Main Dataset - PowerBi
## August 2024 ##

# Install packages & lib -------------------------------------------------------
install.packages(c("dplyr","wordcloud","RColorBrewer","tm","SnowballC","reshape2","psych","readr"))
install.packages(c("NLP", "magrittr","tidytuesdayR","tidyverse"))
install.packages("pastecs")
install.packages("plyr")
#devtools::install_github("lepennec/ggwordcloud")

install.packages("ggplot2")
install.packages("ggwordcloud")
install.packages("pastecs")

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

#library(showtext)
#library(scales)
#library(MetBrewer)
#library(sp)
library(sysfonts)

## Check system & version
#Sys.which("R")
#version

### Profiles - Load data -------------------------------------------------------

# Path & load file
path <- setwd("C:/Users/ncber/OneDrive/Documentos/R-code-health-outcome-analytics")
#path_github <- setwd("https://github.com/raguerre/R-code-health-outcome-analytics") 
path_ext <-setwd("C:/Users/ncber/OneDrive - Inter-American Development Bank Group/output") #OneDrive folder

#file <- "/input_can.csv" # By VPC
file <- "/input/profiles.csv" #Tab: Operation Profile
profiles <-readr::read_csv(paste0(path,file,sep=""), show_col_types = FALSE) # From tableau
profiles <-profiles[with(profiles,order(`Project Number`,`Cycle ID`)),]

# Leave the record of the latest Convergence cycle only
#profiles$`Cycle ID`<-as.integer(profiles$`Cycle ID`)
#profiles$max.cycle <-ave(profiles$`Cycle ID`,profiles$`Project Number`,FUN = max)
#summary(profiles$`Cycle ID`)
#profiles <- profiles[profiles$`Cycle ID`==profiles$max.cycle,]
#profiles$max.cycle  <-NULL

# Leave the record of the first Convergence (oldest) cycle only
 profiles$`Cycle ID`<-as.integer(profiles$`Cycle ID`)
 profiles$min.cycle <-ave(profiles$`Cycle ID`,profiles$`Project Number`,FUN = min)
summary(profiles$`Cycle ID`)
           profiles <- profiles[profiles$`Cycle ID`==profiles$min.cycle,]
profiles$min.cycle  <-NULL

# Keep only health
profiles <- profiles[grepl("HEALTH | e-HEALTH",profiles$Sector,ignore.case = T),]

# Descriptive Stats
#stat.desc(profiles)
summary(profiles)
str(profiles) #show str variables
table(profiles$`Operation Type`)

# Start Bar plot ---------------------------------------------------------------
font_add_google(name = "Roboto Mono", family = "Roboto")
font_add_google(name = "Bebas Neue", family = "Bebas")
font <- "Roboto"
font_t <- "Bebas"

showtext_auto()
showtext_opts(dpi = 320)

plot <- ggplot(profiles) +
  aes(x = `Operation Type`) +
  geom_bar(stat = "count") +
  stat_count(geom = "text", colour="white", size=3.5,
  aes(label=..count..), position = position_stack(vjust = 0.5)) +
  theme_void() +
  theme(plot.title = element_text(family = font_t, size = 30, hjust = 0, face = "bold", color = "#000000", margin = margin(b = 5)),
        plot.title.position = "plot",
        plot.subtitle = element_text(family = font, size = 10, hjust = 0, color = "#000000", lineheight = 1.2, margin = margin(b = 20)),
        plot.caption.position = "plot",
        plot.caption = element_text(size = 9, family = font, color = "#000000", hjust = 0.5, margin = margin(t = 30)),
        legend.position = "top",
        legend.title = element_blank(),
        legend.text = element_text(size = 8, family = font, color = "#000000"),
        axis.text.x = element_text(family = font, size = 9, color = "#000000", margin = margin(t = 10)),
        plot.margin = unit(c(1, 1, 1, 1), "cm"),
        panel.background = element_rect(color = NA, fill = "#FFFFFF"),
        plot.background = element_rect(color = NA, fill = "#FFFFFF")) +
  labs(title = "Operations type",
       subtitle = "Operations managed by SPH at the IDB",
       caption = "Data: PMR Operation Main Dataset | Cohort: August 2024")

plot(plot)
ggsave(paste0("Operations Type_", format(Sys.time(), "%d%m%Y"), ".png"), dpi = 320, width = 10, height = 8)

# End Bar plot -----------------------------------------------------------------

# Drop grants and policy loans
profiles <- profiles[profiles$`Operation Type`=="Loan Operation",]
profiles <- profiles[profiles$`Lending Instrument`=="Investment Loan",]

table(profiles$`Operation Type`) # check

# Filter columns
names(profiles)
#profiles <-profiles[-c(2,3,6,7,10,15,20:21,23)] # delete non-important columns
profiles<-profiles[c(1,4,5,8,9,11,14,17)]
print(profiles)
View(profiles)

### General Development Objectives and Indicators ------------------------------

# Load Impacts (long term dev objectives)
file <- "/input/impact.csv" #Tab: General Development Objectives
impact <- readr::read_csv(paste0(path,file,sep=""), show_col_types = FALSE) #, encoding="UTF-8") # From tableau

# General Development Objective
g.ind <-unique(impact[c(2,4,8,9,11,14)])
#g.ind<-unique(profiles)
rm(impact)

# Bring name of operation from profile and reorder
g.ind<-merge(profiles, g.ind[c(1:6)], by="Project Number", all.x = T)

#g.ind <-merge(g.ind, profiles[c(1:6)], by="Project Number", all.x = T)
#g.ind <-merge(g.ind, profiles[c(1,2)],by.x="Project Number",by.y="Project.Number",all.y = T)
g.ind$Long.op.name <-paste0(g.ind$`Project Number`," - ",g.ind$`Operation Name`)
#g.ind <-g.ind[c(1,6,7,2,3,4,5)]
g.ind$indicator_unit <-paste0(g.ind$Indicators," - ",g.ind$`Unit of Measure`)

#g.ind <-merge(x=g.ind, y=profiles, by.x="Project Number", by.y="Project Number", fill= -9999)
#exporttab <- merge(x=dwd_nogap, y=dwd_gap, by.x='x1', by.y='x2', fill=-9999)

# Add plus sign to differentiate different lines when text in merged
g.ind$`General Development Objective` <-paste0("+",g.ind$`General Development Objective`)
g.ind$indicator_unit <-paste("+",g.ind$indicator_unit)

## collapse variables as text with newline within a cell

## alternative levels of aggregation
#g.ind_a <- data.frame(ddply(g.ind[c(1,4,8)], .(`Project Number`,`General Development Objective`), summarize, objectives_indicators_units = paste(indicator_unit, collapse = "\n")))
#g.ind_b <- data.frame(ddply(unique(g.ind[c(1,4)]), .(`Project Number`), summarize, objectives=paste(`General Development Objective`,collapse="\n")))
#g.ind_c <- data.frame(ddply(g.ind[c(1,4,8)], .(`Project Number`), summarize, objectives_indicators_units = paste(indicator_unit, collapse = "\n")))
#g.ind_d <- data.frame(ddply(g.ind[c(1,5)], .(`Project Number`), summarize, objectives_indicators = paste(Indicators, collapse = "\n")))
#g.ind_e <- data.frame(ddply(g.ind[c(1,6)], .(`Project Number`), summarize, objectives_units = paste(`Unit of Measure`, collapse = "\n")))

#renaming variables
names(g.ind)
names(g.ind)[names(g.ind)=="General Development Objective"]<-"objectives"
names(g.ind)[names(g.ind)=="indicator_unit"]<-"objectives_indicators_units"
names(g.ind)[names(g.ind)=="Indicators"]<-"objectives_indicators"
names(g.ind)[names(g.ind)=="Unit of Measure"]<-"objectives_units"

## keep one line per project
#g.ind_u <-merge(g.ind_b,g.ind_d,by="Project.Number")
#g.ind_u <-merge(g.ind_u,g.ind_e,by="Project.Number")

#rm(g.ind_a,g.ind_b,g.ind_c,g.ind_d,g.ind_e)

# Create df to keep most recent baseline year for each project
#ls(g.ind) list variables in df
#y <- g.ind[c(1,7)]
#y$`Baseline Year` <-as.integer(y$`Baseline Year`)
#      y$max.cycle <-ave(y$`Baseline Year`,y$`Project Number`,FUN = max)
#                y <-y[y$`Baseline Year`==y$max.cycle,]
#      y$max.cycle <-NULL
#                y <-y[!duplicated(y),]

#summary(y$`Baseline Year`) #freq min baseline year

# Create df to keep first (oldest) baseline year for each project
                x <-g.ind[c(1,12)]                
x$`Baseline Year` <-as.integer(x$`Baseline Year`)
      x$min.cycle <-ave(x$`Baseline Year`,x$`Project Number`,FUN = min)
                x <-x[x$`Baseline Year`==x$min.cycle,]
      x$min.cycle <-NULL
                x <-x[!duplicated(x),]

summary(x$`Baseline Year`) #freq min baseline year

# Add operation name and baseline year
#g.ind_u <- merge(unique(g.ind[c(1,2,3)]),g.ind_u,by.x ="Project Number",by.y = "Project.Number")
#g.ind_u <- merge(y,g.ind_u,by ="Project Number",all.y = T) #Max baseline year
g.ind_u <- merge(x,g.ind,by =c("Project Number", "Baseline Year"),all.y = T) #Min baseline year

#rm(y)

# Drop everything and keep g.ind as main dataframe
g.ind <- g.ind_u
rm(g.ind_u)

#g.ind<-na.omit(g.ind)

## Classify projects
g.ind$health_gind     <-grepl("salud|health|saúde|morbi|morta|death|disease",paste(g.ind$Long.op.name,g.ind$objectives),ignore.case = T)
g.ind$malaria_gind    <-grepl("malaria",paste(g.ind$Long.op.name,g.ind$objectives),ignore.case = T)
g.ind$covid_gind      <-grepl("covid|coronavirus|COVID-19",paste(g.ind$Long.op.name,g.ind$objectives),ignore.case = T)
g.ind$calidad_gind    <-grepl("calidad|quality|qualidade",paste(g.ind$Long.op.name,g.ind$objectives),ignore.case = T)
g.ind$acceso_gind     <-grepl("acceso|access|acesso",paste(g.ind$Long.op.name,g.ind$objectives),ignore.case = T)
g.ind$morbilidad_gind <-grepl("mortalidad|mortality|mortalidade|morbilidad|morbidity|morbilidade",paste(g.ind$Long.op.name,g.ind$objectives),ignore.case = T)
g.ind$eficiencia_gind <-grepl("eficiencia|efficiency|efficient",paste(g.ind$Long.op.name,g.ind$objectives),ignore.case = T)
g.ind$equidad_gind    <-grepl("equidad|equity|equidade",paste(g.ind$Long.op.name,g.ind$objectives),ignore.case = T)

g.ind<-g.ind[-c(14:21)]

openxlsx::write.xlsx(g.ind,file = paste0(path,"/unique.g.indicators.xlsx"))
write.csv(g.ind,file = paste0(path,"/unique.g.indicators.csv"), fileEncoding="UTF-8")
#write.csv(g.ind,"unique.g.indicators1.csv", fileEncoding="UTF-8")
write.csv(g.ind,file = paste0(path_ext,"/db_g_indicators.csv"), fileEncoding="UTF-8") #Path in OneDrive

### Worldcloud -----------------------------------------------------------------
## Run wordcloud.R

### Worldcloud -----------------------------------------------------------------

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
s.ind <-unique(outcomes[c(2,4,10:16)])
#rm(outcomes)

s.ind<-merge(profiles, s.ind[c(1:4)], by="Project Number", all.x = T)
s.ind_u<-unique(s.ind[c(1:4,6:11)])

# Oldest cycle of reporting
                y <-s.ind[c(1,11)]                
y$`Baseline Year` <-as.integer(y$`Baseline Year`)
      y$min.cycle <-ave(y$`Baseline Year`,y$`Project Number`,FUN = min)
                y <-y[y$`Baseline Year`==y$min.cycle,]
      y$min.cycle <-NULL
                y <-y[!duplicated(y),]

summary(y$`Baseline Year`) #freq min baseline year

s.ind_u <- merge(y,s.ind_u,by =c("Project Number", "Baseline Year"),all.y = T) #Min baseline year

# Drop everything and keep s.ind as main dataframe
s.indx<-na.omit(s.ind_u)

rm(s.ind_u)

s.ind<-unique(s.indx[c(1,4,7:10)])
rm(s.indx)

# old branch -------

#s.ind <-s.ind[c(1,6,2,3,4,5)]
#s.ind <-merge(s.ind, profiles[c(1,2)],by.y="Project.Number",all.y = T)


#df <- profiles[c(1,2)]
#Merge profile-outcome
#s.obj <- merge(s.obj,profiles,by = "Project Number",all.y = T)
#s.ind <- merge(s.ind,profiles,by = "Project Number",all.y = T)

# Merge general and specific objectives
#obj <- merge(g.obj[c(1,2)], s.obj,by = "Project Number")

#s.ind$covid        <-grepl("covid",s.ind$`Specific Development objectives`,ignore.case = T)
#s.ind$calidad      <-grepl("calidad|quality|qualidade",s.ind$`Specific Development objectives`,ignore.case = T)
#s.ind$acceso       <-grepl("acceso|access|acesso",s.ind$`Specific Development objectives`,ignore.case = T)
#s.ind$morbilidad   <-grepl("mortalidad|mortality|mortalidade|morbilidad|morbidity|morbilidade",s.ind$`Specific Development objectives`,ignore.case = T)
#s.ind$eficiencia   <-grepl("eficiencia|efficiency|efficient",s.ind$`Specific Development objectives`,ignore.case = T)
#s.ind$equidad      <-grepl("equidad|equity|equidade",s.ind$`Specific Development objectives`,ignore.case = T)

#s.ind_u <- merge(unique(g.ind[c(1,2,3)]),g.ind_u,by.x ="Project Number",by.y = "Project.Number")

# end old branch ---------

# Bring name of operation from profile and reorder

#s.ind  <-merge(s.ind,profiles[c(1,2)],by="Project Number",all.x = T)
s.ind$Long.op.name <-paste0(s.ind$`Project Number`," - ",s.ind$`Operation Name`)

#s.ind$indicator_unit_means <-paste0(s.ind$`Indicator Definition`," - ",s.ind$`Unit of Measure`)

## Add plus sign to differentiate when text is wrapped
#s.ind$`Specific Development objectives` <-paste0("+",s.ind$`Specific Development objectives`)
#s.ind$`Indicator Definition`            <-paste0("+",s.ind$`Indicator Definition`)
#s.ind$`Unit of Measure`                 <-paste0("+",s.ind$`Unit of Measure`)
#s.ind$`Means of Verification`           <-paste0("+",s.ind$`Means of Verification`)

## alternative levels of aggregation
#s.ind_a <- data.frame(ddply(s.ind[c(1,2,8)], .(`Project Number`,`Specific Development objectives`), summarize, outcomes_indicators_units = paste(indicator_unit_means, collapse = "\n")))
#s.ind_b <- data.frame(ddply(unique(s.ind[c(1,2)]), .(`Project Number`), summarize, outcomes=paste(`Specific Development objectives`,collapse="\n"))) #Specific Development Objectives
#s.ind_c <- data.frame(ddply(s.ind[c(1,3)], .(`Project Number`), summarize, outcomes_indicators = paste(`Indicator Definition`, collapse = "\n")))
#s.ind_d <- data.frame(ddply(s.ind[c(1,4)], .(`Project Number`), summarize, outcomes_units = paste(`Unit of Measure`, collapse = "\n")))
#s.ind_e <- data.frame(ddply(s.ind[c(1,5)], .(`Project Number`), summarize, outcomes_means = paste(`Means of Verification`, collapse = "\n")))

## keep one line per project
#s.ind_u <-merge(s.ind_b,s.ind_c,by="Project.Number")
#s.ind_u <-merge(s.ind_u,s.ind_d,by="Project.Number")
#s.ind_u <-merge(s.ind_u,s.ind_e,by="Project.Number")

#rm(s.ind_a,s.ind_b,s.ind_c,s.ind_d,s.ind_e)

#View(g.ind_a[duplicated(g.ind_a[,1]),])

##renaming variables
#names(s.ind)
names(s.ind)[names(s.ind)=="Specific Development objectives"]<-"outcomes"
#names(s.ind)[names(s.ind)=="indicator_unit_means"]<-"outcomes_indicators_units"
#names(s.ind)[names(s.ind)=="Indicator Definition"]<-"outcomes_indicators"
#names(s.ind)[names(s.ind)=="Unit of Measure"]<-"outcomes_units"
#names(s.ind)[names(s.ind)=="Means of Verification"]<-"outcomes_means"

# Bring general objectives
#s.ind<-merge(profiles, s.ind[c(1:7)], by="Project Number", all.x = T)
g.ind_x<-unique(g.ind[c(1,8,11)])

s.ind_x <-merge(s.ind,g.ind[c(1,8,11)],by = c("Project Number"), all.x = T)
#s.ind_x <-merge(g.ind_x,s.ind,by = c("Project Number"), all.x = T)

s.uqx<-unique(s.ind_x)
s.uq<-unique(s.ind_x[c(1,2,3,8,9)])

s.uq<-s.uq[!is.na(s.uq$`outcomes`),] # eliminate operations with no information of outcomes

rm(s.ind)

s.ind <- s.uq #unique database
rm(s.uq,s.ind_u,s.ind_x,s.ind_xx)

openxlsx::write.xlsx(s.ind,file = paste0(path,"/db_class_outcomes.csv"))
write.csv(s.ind,file = paste0(path,"/db_class_outcomes.csv"), fileEncoding="UTF-8")

write.csv(g.ind,file = paste0(path_ext,"/db_class_outcomes.csv"), fileEncoding="UTF-8") #Path in OneDrive

### Load outputs ---------------------------------------------------------------

file <- "/input/outputs.csv" #Tab: Specific Development Objectives
#outcomes <- readxl::read_excel(paste0(path,file,sep=""))
outputs <- readr::read_csv(paste0(path,file,sep=""), show_col_types = FALSE) # From tableau
outputs <-unique(df[c(2,7,8,9,11:13)])

# Attach component number to df
outputs$`Component Statement` <- paste0("C",outputs$`Component Order Number`,"-",outputs$`Component Statement`)
outputs$`Output Definition`   <- paste0("C",outputs$`Component Order Number`,"-",outputs$`Output Definition`)
outputs$`Output UOM`          <- paste0("C",outputs$`Component Order Number`,"-",outputs$`Output UOM`)
outputs$`Output MOV`          <- paste0("C",outputs$`Component Order Number`,"-",outputs$`Output MOV`)

# alternative levels of aggregation
components <-data.frame(ddply(unique(outputs[c(1,2,7)]),.(`Project Number`), summarize,components=paste(`Component Statement`,collapse = "\n")))
textputs   <-data.frame(ddply(unique(outputs[c(1,2,11)]),.(`Project Number`), summarize,outputs=paste(`Output Definition`,collapse = "\n")))
units      <-data.frame(ddply(unique(outputs[c(1,2,12)]),.(`Project Number`), summarize,output_units=paste(`Output UOM`,collapse = "\n")))
mov        <-data.frame(ddply(unique(outputs[c(2,13)]),.(`Project Number`), summarize,output_MoV=paste(`Output MOV`,collapse = "\n")))

# keep one line per project
oput    <-merge(components,textputs,by="Project.Number")
oput    <-merge(oput,units,by="Project.Number")
oput    <-merge(oput,mov,by="Project.Number")

rm(components,textputs,units,mov)

#outputs <- rbind(outputs,oput)
#rm(df,oput)

# Check column names of both data frames
#print(names(outputs))
#print(names(oput))

# Check the number of columns
#ncol(outputs)
#ncol(oput)

# Check the dimensions
#dim(outputs)
#dim(oput)

#output_prof         <- merge(outputs,profiles[c(1,2,4,5,8)],by="Project Number")
#outcomes_outpu      <- merge(output_prof,outcomes[c(1:4)],by="Project Number")

table(profiles$Sector) # Check sector
profiles <-profiles[!is.na(profiles$`Project Number`),]

### Classification & Subcategories para outcomes  ------------------------------

file <- "/db_class_outcomes.csv" #Database for analysis
#outcomes <- readxl::read_excel(paste0(path,file,sep=""))
db_f <- readr::read_csv(paste0(path,file,sep=""), show_col_types = FALSE) 

#db_f$health[which(data_f$`Project Number`=="EC-L1236")] <- FALSE # Este es discapacidad
#db_f$health[which(data_f$`Project Number`=="EC-L1258")] <- FALSE # Este es DiT
#db_f$health[which(data_f$`Project Number`=="PN-L1160")] <- FALSE # Este es Protección Social
#db_f$health[which(data_f$`Project Number`=="VE-L1017")] <- FALSE # Este es Orquesta
#db_f$health[which(data_f$`Project Number`=="JA-L1053")] <- FALSE # Este es Protección Social

# Classify projects by outcomes/Specific development objectives
db_f$health_sdo     <-grepl("salud|health|saúde|diseas-|chronic|public|públic-|nutri-|illness",paste(db_f$Long.op.name,db_f$outcomes),ignore.case = T)
db_f$morbility_sdo  <-grepl("enferm-|mortalidad|mortality|mortalidade|morbilidad|morbidity|morbilidade|death|muerte",paste(db_f$Long.op.name,db_f$outcomes),ignore.case = T)
db_f$malaria_sdo    <-grepl("malaria",paste(db_f$Long.op.name,db_f$outcomes),ignore.case = T)
db_f$covid_sdo      <-grepl("covid|coronavirus",paste(db_f$Long.op.name,db_f$outcomes),ignore.case = T)
db_f$acces_sdo      <-grepl("acceso|access|acesso|-cobertura|cobertura|atenção",paste(db_f$Long.op.name,db_f$outcomes),ignore.case = T)
db_f$efficiency_sdo <-grepl("eficiencia|efficiency|efficient|efectividad",paste(db_f$Long.op.name,db_f$outcomes),ignore.case = T)
db_f$equity_sdo     <-grepl("equidad|equity|equidade",paste(db_f$Long.op.name,db_f$outcomes),ignore.case = T)
db_f$quality_sdo    <-grepl("calidad|quality|qualidade",paste(db_f$Long.op.name,db_f$outcomes),ignore.case = T)
db_f$e_health_sdo   <-grepl("digital|tele-|technology|tecnolog-",paste(db_f$Long.op.name,db_f$outcomes),ignore.case = T)
db_f$network_sdo    <-grepl("red|system|information|coordina-|integrate",paste(db_f$Long.op.name,db_f$outcomes),ignore.case = T)

#Subcategorias de morbilidad
table(db_f$morbility_sdo) # check morbility

db_f$morbi_mat_inf_sdo<-with(db_f, morbility_sdo & grepl("infantil|mujeres|niños|niñez|infancia|neonatal-|embarazad-|matern-",paste(db_f$Long.op.name,db_f$objectives,db_f$outcomes),ignore.case = T))
db_f$morbi_ncd_sdo<-with(db_f, morbility_sdo & grepl("noncommunicable|ncd|cardiovasc-|cerebrovasc-|cancer|cáncer|niñez|infancia|hipertension|diabetes|HTA|DM|mental|respira-|musco-",paste(db_f$Long.op.name,db_f$objectives,db_f$outcomes),ignore.case = T))
db_f$morbi_cd_sdo<-with(db_f, covid_sdo|malaria_sdo|morbility_sdo & grepl("vih|hiv-|its|ets|malaria|covid|viruela|measles|leish-|-osis",paste(db_f$Long.op.name,db_f$objectives,db_f$outcomes),ignore.case = T))
db_f$morbi_ext_sdo<-with(db_f, morbility_sdo & grepl("transito|accident|accidente|its|ets|malaria|covid|viruela|measles|leish-|-osis",paste(db_f$Long.op.name,db_f$objectives,db_f$outcomes),ignore.case = T))

n_distinct(db_f$`Project Number`) #41 operations

#df %>%
#  group_by(db_f$`Project Number`) %>%
#  summarize(distinct_operations = n_distinct(outcomes))

#table(db_f$morbility,db_f$morbi_mat_inf) # Check morbi_mat_inf is T only if morbilidad is T

#Subcategorias calidad
table(db_f$quality_sdo) # check 

db_f$quality_resources_sdo<-with(db_f, quality_sdo & grepl("training|staff|equipo|medicament-|facili-|hospital-|capacity|capacidad|provisión",paste(db_f$Long.op.name,db_f$outcomes),ignore.case = T))
db_f$quality_improvement_sdo<-with(db_f, quality_sdo & grepl("estándar|standard|procesos|processes|fortalec-|strength-|aument-|mejor-|enhanc-|improv-",paste(db_f$Long.op.name,db_f$outcomes),ignore.case = T))
db_f$quality_opportunity_sdo<-with(db_f, quality_sdo & grepl("horas|time-|minimum",paste(db_f$Long.op.name,db_f$outcomes),ignore.case = T))
db_f$quality_responsivennes_sdo<-with(db_f, quality_sdo & grepl("satisfacción|satisfaction|choice|elección|communitary|comunidad",paste(db_f$Long.op.name,db_f$outcomes),ignore.case = T))

#Subcategirias efficiency
table(db_f$efficiency_sdo) # check 

db_f$efficiency_funding_sdo<-with(db_f, efficiency_sdo & grepl("beneficio|benefit|financing|brecha",paste(db_f$Long.op.name,db_f$outcomes),ignore.case = T))

#Subcategirias groups
db_f$group_women<-with(db_f, health_sdo & grepl("mujer-|embarazad-",paste(db_f$Long.op.name,db_f$outcomes),ignore.case = T))
db_f$group_child<-with(db_f, health_sdo & grepl("niñ-|infancia-|infatil",paste(db_f$Long.op.name,db_f$outcomes),ignore.case = T))
db_f$group_youth<-with(db_f, health_sdo & grepl("jov-|young-|youth",paste(db_f$Long.op.name,db_f$outcomes),ignore.case = T))
db_f$group_pwd<-with(db_f, health_sdo & grepl("discapacidad |young-|youth",paste(db_f$Long.op.name,db_f$outcomes),ignore.case = T))
db_f$group_aging<-with(db_f, health_sdo & grepl("adulto-|ltc|adultez|vejez|personas mayores",paste(db_f$Long.op.name,db_f$outcomes),ignore.case = T))

openxlsx::write.xlsx(db_f,file = paste0(path,"/db_classification.xlsx"))
write.csv(db_f,file = paste0(path,"/db_classification.csv"), fileEncoding="UTF-8")

openxlsx::write.xlsx(db_f,file = paste0(path_ext,"/db_classification.xlsx"))
write.csv(db_f,file = paste0(path_ext,"/db_classification.csv"), fileEncoding="UTF-8") #Path in OneDrive

### Wordclouds------------------------------------------------------------------

file <- "/db_classification.csv" #Tab: Specific Development Objectives
db <- readr::read_csv(paste0(path,file,sep=""), show_col_types = FALSE) # From tableau
db_f<-db[-c(1,2)]

# load libraries
install.packages("RColorBrewer")
install.packages("textTinyR")
install.packages("langdetect")
install.packages("text")
install.packages(c("dplyr","wordcloud","RColorBrewer","tm","SnowballC","reshape2","psych","readr"))
install.packages(c("NLP", "magrittr","tidytuesdayR","tidyverse"))
install.packages("ggplot2")
install.packages("ggwordcloud")
install.packages("tm")

library(text)
library(tidyverse)
library(showtext)
library(janitor)
library(sysfonts)
library(RColorBrewer)
library(textTinyR)
library(langdetect)

library(dplyr)
library(wordcloud)
library(RColorBrewer)
library(tm)
library(SnowballC)
library(reshape2)
library(psych)
library(readr)

library(NLP)
library(magrittr)
library(tidytuesdayR)
library(tidyverse)
library(ggplot2)
library(ggwordcloud)
library(tm)

# add font
font_add_google(name="Sigmar One", family="Sigmar One")
font_t<-"Sigmar One"

font_add_google(name="Hind", family="Hind")
font<-"Hind"

text_data <-unique(db_f[c(2,5)])
head(db_f$outcomes)
text_data <- as.character(text_data)

text_data <- na.omit(text_data)

### Create a text corpus, define transformation functions, and apply them to corpus
corpus <- Corpus(VectorSource(text_data))

to_lower <- content_transformer(tolower)
remove_punct <- content_transformer(function(x) removePunctuation(x))
remove_nums <- content_transformer(function(x) removeNumbers(x))

corpus <- tm_map(corpus, to_lower) 
corpus <- tm_map(corpus, remove_punct)
corpus <- tm_map(corpus, remove_nums)
corpus <- tm_map(corpus, removeWords, stopwords("english"))
corpus <- tm_map(corpus, removeWords, stopwords("spanish"))
corpus <- tm_map(corpus, removeWords, stopwords("portuguese"))

inspect(corpus) # Check the cleaned corpus
tdm <- TermDocumentMatrix(corpus) # Create a term-document matrix
matrix <- as.matrix(tdm) # Convert the term-document matrix to a matrix
word_freqs <- sort(rowSums(matrix), decreasing = TRUE) # Calculate word frequencies
word_freqs_df <- data.frame(word = names(word_freqs), # Convert to data frame for wordcloud
                            freq = word_freqs, stringsAsFactors = FALSE) 

set.seed(42)
cloud <- ggplot(word_freqs_df, aes(label = word, size = freq)) +
  geom_text_wordcloud_area(rm_outside = TRUE) +
  scale_size_area(max_size = 50) +
  theme_minimal(base_size = 30, base_family = "Montserrat") 

cloud + theme_light()+scale_color_gradient(low = "#85bb65", high = "#672fe6")
plot(cloud)

### Extracting verbs from outcomes ---------------------------------------------

install.packages("stringr")
library(stringr)

extract_keywords <- function(text) {
  words <- str_extract_all(text, "\\b\\w+(ing|ed|ado|ados|ido|ada|adas|ar|er|ir|ida|ção|idas)\\b")[[1]]
  return(words)
}

verbs<-db_f[c(1,4)]

## Use mapply to apply the extraction function to each cell of the data frame
result <- mapply(extract_keywords, verbs$`outcomes`, SIMPLIFY=FALSE)

## Convert the result back into a data frame with the same dimensions
verbs_df <- as.data.frame(matrix(result, nrow = nrow(verbs), byrow = TRUE), stringsAsFactors = FALSE)

verbs_df <- cbind(verbs, verbs_df)
verbs_t_df2 <- cbind(db_f, verbs_df, verbs)

xx <-unique(verbs_df)

openxlsx::write.xlsx(xx,file = paste0(path_ext,"/db_key_words2.xlsx"))
write.csv(xx,file = paste0(path_ext,"/db_key_words2.csv"), fileEncoding="UTF-8") #Path in OneDrive

## Wordcloud verbs ##

text_verb <- as.character(verbs_df$V1)
text_verb <- na.omit(text_verb)

### Create a text corpus, define transformation functions, and apply them to corpus
corpus <- Corpus(VectorSource(text_verb))

to_lower <- content_transformer(tolower)
remove_punct <- content_transformer(function(x) removePunctuation(x))
remove_nums <- content_transformer(function(x) removeNumbers(x))

corpus <- tm_map(corpus, to_lower) 
corpus <- tm_map(corpus, remove_punct)
corpus <- tm_map(corpus, remove_nums)
#corpus <- tm_map(corpus, removeWords, stopwords("english"))
#corpus <- tm_map(corpus, removeWords, stopwords("spanish"))
#corpus <- tm_map(corpus, removeWords, stopwords("portuguese"))

inspect(corpus) # Check the cleaned corpus
tdm <- TermDocumentMatrix(corpus) # Create a term-document matrix
matrix <- as.matrix(tdm) # Convert the term-document matrix to a matrix
word_freqs <- sort(rowSums(matrix), decreasing = TRUE) # Calculate word frequencies
word_freqs_df <- data.frame(word = names(word_freqs), # Convert to data frame for wordcloud
                            freq = word_freqs, stringsAsFactors = FALSE) 

word_freqs_df <- word_freqs_df %>%
  filter(word!="character")  # Exclude the word

set.seed(42)
cloud <- ggplot(word_freqs_df, aes(label = word, size = freq, color=freq)) +
  geom_text_wordcloud_area(rm_outside = TRUE) +
  scale_size_area(max_size = 60) +
  theme_minimal(base_size = 20, base_family = "Montserrat") +
  scale_color_gradient(low = "#0f3057", high = "#fda085")  # Light blue to navy
   
plot(cloud)

## Print the extracted keywords for each row and column
#print(result_df)

#matches <- mapply(db_f, function(text) str_extract_all(db_f$outcomes, "\\b\\w+(ing|ed|ado|ados|ido|ada|adas|ar|ida|er|ir|ção|)\\b")[[1]])
#matches2 <- str_extract_all(db_f$outcomes, "\\b\\w+ing\\b|\\b\\w+ed\\b|\\b\\w+ado\\b|\\b\\w+ados\\b|\\b\\w+ido\\b|\\b\\w+idos\\b|\\b\\w+ada\\b|\\b\\w+adas\\b|\\b\\w+ar\\b|\\b\\w+er\\b|\\b\\w+ir\\b")[[1]])

#xx<-db_f

#xx$key_word <- data.frame(matches[[1]])

#rm(matches)

### Extracting direct objects/what? from outcomes ---------------------------------------------

extract_objects <- function(text) {
  words <- str_extract_all(text, "\\b\\salud|health|saúde|diseases|disease|chronic|public|nutrición|illness|
                           enfermedad|mortalidad|mortality|mortalidade|morbilidad|morbidity|morbilidade|death|muerte|
                           acceso|access|acesso|cobertura|cobertura|atenção|
                           eficiencia|efficiency|efficient|efectividad|gasto|
                           equidad|equity|equidade|
                           calidad|quality|qualidade|
                           digital|telehealth|technology|tecnología|
                           red|system|information|coordina|integrate|systems\\b")[[1]]
  return(words)
}

objects<-db_f[c(1,4)]

## Use mapply to apply the extraction function to each cell of the data frame
result <- mapply(extract_objects, objects$`outcomes`, SIMPLIFY=FALSE)

## Convert the result back into a data frame with the same dimensions
objects_df <- as.data.frame(matrix(result, nrow = nrow(objects), byrow = TRUE), stringsAsFactors = FALSE)

objects_df <- cbind(verbs, objects_df)
objects_t_df2 <- cbind(db_f, objects_df, objects)

#openxlsx::write.xlsx(final_df,file = paste0(path_ext,"/db_key_words.xlsx"))
#write.csv(final_df,file = paste0(path_ext,"/db_key_words.csv"), fileEncoding="UTF-8") #Path in OneDrive

## Wordcloud verbs ##

text_verb <- as.character(verbs_df$V1)
text_verb <- na.omit(text_verb)

### Create a text corpus, define transformation functions, and apply them to corpus
corpus <- Corpus(VectorSource(text_verb))

to_lower <- content_transformer(tolower)
remove_punct <- content_transformer(function(x) removePunctuation(x))
remove_nums <- content_transformer(function(x) removeNumbers(x))

corpus <- tm_map(corpus, to_lower) 
corpus <- tm_map(corpus, remove_punct)
corpus <- tm_map(corpus, remove_nums)
#corpus <- tm_map(corpus, removeWords, stopwords("english"))
#corpus <- tm_map(corpus, removeWords, stopwords("spanish"))
#corpus <- tm_map(corpus, removeWords, stopwords("portuguese"))

inspect(corpus) # Check the cleaned corpus
tdm <- TermDocumentMatrix(corpus) # Create a term-document matrix
matrix <- as.matrix(tdm) # Convert the term-document matrix to a matrix
word_freqs <- sort(rowSums(matrix), decreasing = TRUE) # Calculate word frequencies
word_freqs_df <- data.frame(word = names(word_freqs), # Convert to data frame for wordcloud
                            freq = word_freqs, stringsAsFactors = FALSE) 

word_freqs_df <- word_freqs_df %>%
  filter(word!="character")  # Exclude the word

set.seed(42)
cloud <- ggplot(word_freqs_df, aes(label = word, size = freq, color=freq)) +
  geom_text_wordcloud_area(rm_outside = TRUE) +
  scale_size_area(max_size = 60) +
  theme_minimal(base_size = 20, base_family = "Montserrat") +
  scale_color_gradient(low = "#0f3057", high = "#fda085")  # Light blue to navy

plot(cloud)






# Classify projects by outcomes/Specific development objectives
db_f$health_sdo     <-grepl("salud|health|saúde|diseas-|chronic|public|públic-|nutri-|illness",paste(db_f$Long.op.name,db_f$outcomes),ignore.case = T)
db_f$morbility_sdo  <-grepl("enferm-|mortalidad|mortality|mortalidade|morbilidad|morbidity|morbilidade|death|muerte",paste(db_f$Long.op.name,db_f$outcomes),ignore.case = T)
db_f$malaria_sdo    <-grepl("malaria",paste(db_f$Long.op.name,db_f$outcomes),ignore.case = T)
db_f$covid_sdo      <-grepl("covid|coronavirus",paste(db_f$Long.op.name,db_f$outcomes),ignore.case = T)
db_f$acces_sdo      <-grepl("acceso|access|acesso|cobertura|cobertura|atenção",paste(db_f$Long.op.name,db_f$outcomes),ignore.case = T)
db_f$efficiency_sdo <-grepl("eficiencia|efficiency|efficient|efectividad|gasto",paste(db_f$Long.op.name,db_f$outcomes),ignore.case = T)
db_f$equity_sdo     <-grepl("equidad|equity|equidade",paste(db_f$Long.op.name,db_f$outcomes),ignore.case = T)
db_f$quality_sdo    <-grepl("calidad|quality|qualidade",paste(db_f$Long.op.name,db_f$outcomes),ignore.case = T)
db_f$e_health_sdo   <-grepl("digital|tele-|technology|tecnolog-",paste(db_f$Long.op.name,db_f$outcomes),ignore.case = T)
db_f$network_sdo    <-grepl("red|system|information|coordina-|integrate",paste(db_f$Long.op.name,db_f$outcomes),ignore.case = T)

#Subcategorias de morbilidad
table(db_f$morbility_sdo) # check morbility

db_f$morbi_mat_inf_sdo<-with(db_f, morbility_sdo & grepl("infantil|mujeres|niños|niñez|infancia|neonatal-|embarazad-|matern-",paste(db_f$Long.op.name,db_f$objectives,db_f$outcomes),ignore.case = T))
db_f$morbi_ncd_sdo<-with(db_f, morbility_sdo & grepl("noncommunicable|ncd|cardiovasc-|cerebrovasc-|cancer|cáncer|niñez|infancia|hipertension|diabetes|HTA|DM|mental|respira-|musco-",paste(db_f$Long.op.name,db_f$objectives,db_f$outcomes),ignore.case = T))
db_f$morbi_cd_sdo<-with(db_f, covid_sdo|malaria_sdo|morbility_sdo & grepl("vih|hiv-|its|ets|malaria|covid|viruela|measles|leish-|-osis",paste(db_f$Long.op.name,db_f$objectives,db_f$outcomes),ignore.case = T))
db_f$morbi_ext_sdo<-with(db_f, morbility_sdo & grepl("transito|accident|accidente|its|ets|malaria|covid|viruela|measles|leish-|-osis",paste(db_f$Long.op.name,db_f$objectives,db_f$outcomes),ignore.case = T))

n_distinct(db_f$`Project Number`) #41 operations

#df %>%
#  group_by(db_f$`Project Number`) %>%
#  summarize(distinct_operations = n_distinct(outcomes))

#table(db_f$morbility,db_f$morbi_mat_inf) # Check morbi_mat_inf is T only if morbilidad is T

#Subcategorias calidad
table(db_f$quality_sdo) # check 

db_f$quality_resources_sdo<-with(db_f, quality_sdo & grepl("training|staff|equipo|medicament-|facili-|hospital-|capacity|capacidad|provisión",paste(db_f$Long.op.name,db_f$outcomes),ignore.case = T))
db_f$quality_improvement_sdo<-with(db_f, quality_sdo & grepl("estándar|standard|procesos|processes|fortalec-|strength-|aument-|mejor-|enhanc-|improv-",paste(db_f$Long.op.name,db_f$outcomes),ignore.case = T))
db_f$quality_opportunity_sdo<-with(db_f, quality_sdo & grepl("horas|time-|minimum",paste(db_f$Long.op.name,db_f$outcomes),ignore.case = T))
db_f$quality_responsivennes_sdo<-with(db_f, quality_sdo & grepl("satisfacción|satisfaction|choice|elección|communitary|comunidad",paste(db_f$Long.op.name,db_f$outcomes),ignore.case = T))

#Subcategirias efficiency
table(db_f$efficiency_sdo) # check 

db_f$efficiency_funding_sdo<-with(db_f, efficiency_sdo & grepl("beneficio|benefit|financing|brecha",paste(db_f$Long.op.name,db_f$outcomes),ignore.case = T))

#Subcategirias groups
db_f$group_women<-with(db_f, health_sdo & grepl("mujer-|embarazad-",paste(db_f$Long.op.name,db_f$outcomes),ignore.case = T))
db_f$group_child<-with(db_f, health_sdo & grepl("niñ-|infancia-|infatil",paste(db_f$Long.op.name,db_f$outcomes),ignore.case = T))
db_f$group_youth<-with(db_f, health_sdo & grepl("jov-|young-|youth",paste(db_f$Long.op.name,db_f$outcomes),ignore.case = T))
db_f$group_pwd<-with(db_f, health_sdo & grepl("discapacidad |young-|youth",paste(db_f$Long.op.name,db_f$outcomes),ignore.case = T))
db_f$group_aging<-with(db_f, health_sdo & grepl("adulto-|ltc|adultez|vejez|personas mayores",paste(db_f$Long.op.name,db_f$outcomes),ignore.case = T))

openxlsx::write.xlsx(db_f,file = paste0(path,"/db_classification.xlsx"))
write.csv(db_f,file = paste0(path,"/db_classification.csv"), fileEncoding="UTF-8")

openxlsx::write.xlsx(db_f,file = paste0(path_ext,"/db_classification.xlsx"))
write.csv(db_f,file = paste0(path_ext,"/db_classification.csv"), fileEncoding="UTF-8") #Path in OneDrive

