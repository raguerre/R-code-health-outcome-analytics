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
#path_ext <-setwd("C:/Users/ncber/OneDrive - Inter-American Development Bank Group/output") #OneDrive folder

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

# Leave the record of the first Convergence cycle only
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
profiles <-profiles[-c(1,2,3,6,7,10,15,20:21,23)] # delete non-important columns
profileS <-profiles[c(1,2,8,3:7,9:11)]
print(profiles)
View(profiles)

### General Development Objectives and Indicators ------------------------------

# Load Impacts (long term dev objectives)
file <- "/input/impact.csv" #Tab: General Development Objectives
impact <- readr::read_csv(paste0(path,file,sep=""), show_col_types = FALSE) #, encoding="UTF-8") # From tableau

# General Development Objective
g.ind <-unique(impact[c(2,4,8,9,11)])
rm(impact)

# Bring name of operation from profile and reorder
g.ind <-merge(g.ind, profiles[c(1,2)], by="Project Number", all.x = T)
#g.ind <-merge(g.ind, profiles[c(1,2)],by.x="Project Number",by.y="Project.Number",all.y = T)
g.ind$Long.op.name <-paste0(g.ind$`Project Number`," - ",g.ind$`Operation Name`)
g.ind <-g.ind[c(1,6,7,2,3,4,5)]
g.ind$indicator_unit <-paste0(g.ind$Indicators," - ",g.ind$`Unit of Measure`)

#g.ind <-merge(x=g.ind, y=profiles, by.x="Project Number", by.y="Project Number", fill= -9999)
#exporttab <- merge(x=dwd_nogap, y=dwd_gap, by.x='x1', by.y='x2', fill=-9999)

# Add plus sign to differentiate different lines when text in merged
g.ind$`General Development Objective` <-paste0("+",g.ind$`General Development Objective`)
g.ind$indicator_unit <-paste("+",g.ind$indicator_unit)

#freq_x <-table(g.ind$`Project Number`)
print(freq_x)

## collapse variables as text with newline within a cell

## alternative levels of aggregation
#g.ind_a <- data.frame(ddply(g.ind[c(1,4,8)], .(`Project Number`,`General Development Objective`), summarize, objectives_indicators_units = paste(indicator_unit, collapse = "\n")))
#g.ind_b <- data.frame(ddply(unique(g.ind[c(1,4)]), .(`Project Number`), summarize, objectives=paste(`General Development Objective`,collapse="\n")))
#g.ind_c <- data.frame(ddply(g.ind[c(1,4,8)], .(`Project Number`), summarize, objectives_indicators_units = paste(indicator_unit, collapse = "\n")))
#g.ind_d <- data.frame(ddply(g.ind[c(1,5)], .(`Project Number`), summarize, objectives_indicators = paste(Indicators, collapse = "\n")))
#g.ind_e <- data.frame(ddply(g.ind[c(1,6)], .(`Project Number`), summarize, objectives_units = paste(`Unit of Measure`, collapse = "\n")))

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
                x <- g.ind[c(1,7)]                
x$`Baseline Year` <-as.integer(x$`Baseline Year`)
      x$min.cycle <-ave(x$`Baseline Year`,x$`Project Number`,FUN = min)
                x <-x[x$`Baseline Year`==x$min.cycle,]
      x$min.cycle <-NULL
                x <-x[!duplicated(x),]

summary(x$`Baseline Year`) #freq max baseline year

# Add operation name and baseline year
g.ind_u <- merge(unique(g.ind[c(1,2,3)]),g.ind_u,by.x ="Project Number",by.y = "Project.Number")
#g.ind_u <- merge(y,g.ind_u,by ="Project Number",all.y = T) #Max baseline year
g.ind_u <- merge(x,g.ind_u,by ="Project Number",all.y = T) #Min baseline year

#rm(y)

# Drop everything and keep g.ind as main dataframe
g.ind <- g.ind_u
#rm(g.ind_u)

# Classify projects
g.ind$health     <-grepl("salud|health|saúde|morbi|morta|death|disease",paste(g.ind$Long.op.name,g.ind$objectives),ignore.case = T)
g.ind$malaria    <-grepl("malaria",paste(g.ind$Long.op.name,g.ind$objectives),ignore.case = T)
g.ind$covid      <-grepl("covid|coronavirus",g.ind$objectives,ignore.case = T)
g.ind$calidad    <-grepl("calidad|quality|qualidade",paste(g.ind$Long.op.name,g.ind$objectives),ignore.case = T)
g.ind$acceso     <-grepl("acceso|access|acesso",paste(g.ind$Long.op.name,g.ind$objectives),ignore.case = T)
g.ind$morbilidad <-grepl("mortalidad|mortality|mortalidade|morbilidad|morbidity|morbilidade",paste(g.ind$Long.op.name,g.ind$objectives),ignore.case = T)
g.ind$eficiencia <-grepl("eficiencia|efficiency|efficient",paste(g.ind$Long.op.name,g.ind$objectives),ignore.case = T)
g.ind$equidad    <-grepl("equidad|equity|equidade",paste(g.ind$Long.op.name,g.ind$objectives),ignore.case = T)

openxlsx::write.xlsx(g.ind,file = paste0(path,"/unique.g.indicators.xlsx"))
write.csv(g.ind,file = paste0(path,"/unique.g.indicators.csv"), fileEncoding="UTF-8")
#write.csv(g.ind,"unique.g.indicators1.csv", fileEncoding="UTF-8")

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
s.obj <-unique(outcomes[c(2,4)])
s.ind <-unique(outcomes[c(2,4,10,11,14)])
#rm(outcomes)

#s.ind <- merge(g.ind, profiles[c(1:7)],by.x = "Project Number",by.y="Project.Number", all.y = T)

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
s.ind  <-merge(s.ind,profiles[c(1,2)],by="Project Number",all.x = T)
s.ind$Long.op.name <-paste0(s.ind$`Project Number`," - ",s.ind$`Operation Name`)

s.ind$indicator_unit_means <-paste0(s.ind$`Indicator Definition`," - ",s.ind$`Unit of Measure`)

# Add plus sign to differentiate when text is wrapped
s.ind$`Specific Development objectives` <-paste0("+",s.ind$`Specific Development objectives`)
s.ind$`Indicator Definition`            <-paste0("+",s.ind$`Indicator Definition`)
s.ind$`Unit of Measure`                 <-paste0("+",s.ind$`Unit of Measure`)
s.ind$`Means of Verification`           <-paste0("+",s.ind$`Means of Verification`)

# alternative levels of aggregation
s.ind_a <- data.frame(ddply(s.ind[c(1,2,8)], .(`Project Number`,`Specific Development objectives`), summarize, outcomes_indicators_units = paste(indicator_unit_means, collapse = "\n")))
s.ind_b <- data.frame(ddply(unique(s.ind[c(1,2)]), .(`Project Number`), summarize, outcomes=paste(`Specific Development objectives`,collapse="\n"))) #Specific Development Objectives
s.ind_c <- data.frame(ddply(s.ind[c(1,3)], .(`Project Number`), summarize, outcomes_indicators = paste(`Indicator Definition`, collapse = "\n")))
s.ind_d <- data.frame(ddply(s.ind[c(1,4)], .(`Project Number`), summarize, outcomes_units = paste(`Unit of Measure`, collapse = "\n")))
s.ind_e <- data.frame(ddply(s.ind[c(1,5)], .(`Project Number`), summarize, outcomes_means = paste(`Means of Verification`, collapse = "\n")))

# keep one line per project
s.ind_u <-merge(s.ind_b,s.ind_c,by="Project.Number")
s.ind_u <-merge(s.ind_u,s.ind_d,by="Project.Number")
s.ind_u <-merge(s.ind_u,s.ind_e,by="Project.Number")

#rm(s.ind_a,s.ind_b,s.ind_c,s.ind_d,s.ind_e)

#View(g.ind_a[duplicated(g.ind_a[,1]),])

# Bring general objectives
s.ind_u <- merge(g.ind[c(1:7)],s.ind_u,by.x = "Project Number",by.y="Project.Number", all.y = T)

# Keep this files for exporting
s.ind <- s.ind_u  
rm(s.ind_u)

# Classify projects 
# Falta incluir los outcomes en la clasificación
s.ind$health     <-grepl("salud|health|saúde|morbi|morta|death|disease",paste(s.ind$Long.op.name,s.ind$objectives),ignore.case = T)
s.ind$malaria    <-grepl("malaria",paste(s.ind$Long.op.name,s.ind$objectives),ignore.case = T)
s.ind$covid      <-grepl("covid|coronavirus",s.ind$objectives,ignore.case = T)
s.ind$acceso     <-grepl("acceso|access|acesso",paste(s.ind$Long.op.name,s.ind$objectives),ignore.case = T)
s.ind$morbilidad <-grepl("mortalidad|mortality|mortalidade|morbilidad|morbidity|morbilidade",paste(s.ind$Long.op.name,s.ind$objectives),ignore.case = T)
s.ind$eficiencia <-grepl("eficiencia|efficiency|efficient",paste(s.ind$Long.op.name,s.ind$objectives),ignore.case = T)
s.ind$equidad    <-grepl("equidad|equity|equidade",paste(s.ind$Long.op.name,s.ind$objectives),ignore.case = T)

# Different options of quality classification by 
s.ind$calidad_name        <-grepl("calidad|quality|qualidade",s.ind$Long.op.name,ignore.case = T)
s.ind$calidad_g_objective <-grepl("calidad|quality|qualidade",s.ind$objectives,ignore.case = T)
s.ind$calidad_outcome     <-grepl("calidad|quality|qualidade",s.ind$outcomes,ignore.case = T)
s.ind$calidad_outcome_ind <-grepl("calidad|quality|qualidade",s.ind$outcomes_indicators,ignore.case = T)

dim(s.ind) # Dimension
str(s.ind) # df description

# Manual adjustment of the classification
s.ind$not.a.loan <-grepl("-J|-G|-U|RG",s.ind$`Project Number`)
summary(s.ind$not.a.loan) # check

s.ind$health[which(s.ind$`Project Number`=="EC-L1236")] <- FALSE # Este es discapacidad
s.ind$health[which(s.ind$`Project Number`=="EC-L1258")] <- FALSE # Este es DiT
s.ind$health[which(s.ind$`Project Number`=="PN-L1160")] <- FALSE # Este es Protección Social
s.ind$health[which(s.ind$`Project Number`=="VE-L1017")] <- FALSE # Este es Orquesta
s.ind$health[which(s.ind$`Project Number`=="JA-L1053")] <- FALSE # Este es Protección Social

# HO-L1194 es de proteccion social con un componente de salud. Lo dejo como proteccion social

#x<-s.ind[s.ind$health==T&s.ind$malaria==F&s.ind$not.a.loan==F,]

openxlsx::write.xlsx(s.ind,file = paste0(path,"/unique.outcomes.xlsx"))
write.csv(s.ind,file = paste0(path,"/unique.outcomes.csv"), fileEncoding="UTF-8")

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

### Classification & Subcategories para outcomes y objectives ------------------

file <- "/unique.outcomes.csv" #Database for analysis
#outcomes <- readxl::read_excel(paste0(path,file,sep=""))
data_base <- readr::read_csv(paste0(path,file,sep=""), show_col_types = FALSE) 

db_f <-data_base %>% filter(!not.a.loan) #Only loan operations
db_f <-data_base[!is.na(data_base$`Project Number`),]

# Classify projects 
# Falta incluir los outcomes en la clasificación
db_f$health    <-grepl("salud|health|saúde|diseas-|chronic|public|públic-|nutri-|illness",paste(db_f$Long.op.name,db_f$objectives,db_f$objectives_indicators,db_f$outcomes,db_f$outcomes_indicators),ignore.case = T)
db_f$morbility <-grepl("enferm-|mortalidad|mortality|mortalidade|morbilidad|morbidity|morbilidade|death|muerte",paste(db_f$Long.op.name,db_f$objectives,db_f$objectives_indicators,db_f$outcomes,db_f$outcomes_indicators),ignore.case = T)
db_f$malaria   <-grepl("malaria",paste(db_f$Long.op.name,db_f$objectives,db_f$objectives_indicators,db_f$outcomes,db_f$outcomes_indicators),ignore.case = T)
db_f$covid     <-grepl("covid|coronavirus",paste(db_f$Long.op.name,db_f$objectives,db_f$objectives_indicators,db_f$outcomes,db_f$outcomes_indicators),ignore.case = T)
db_f$acces     <-grepl("acceso|access|acesso|-cobertura|cobertura|atenção",paste(db_f$Long.op.name,db_f$objectives,db_f$objectives_indicators,db_f$outcomes,db_f$outcomes_indicators),ignore.case = T)
db_f$efficiency<-grepl("eficiencia|efficiency|efficient|efectividad",paste(db_f$Long.op.name,db_f$objectives,db_f$objectives_indicators,db_f$outcomes,db_f$outcomes_indicators),ignore.case = T)
db_f$equity    <-grepl("equidad|equity|equidade",paste(db_f$Long.op.name,db_f$objectives,db_f$objectives_indicators,db_f$outcomes,db_f$outcomes_indicators),ignore.case = T)
db_f$quality   <-grepl("calidad|quality|qualidade",paste(db_f$Long.op.name,db_f$objectives,db_f$objectives_indicators,db_f$outcomes,db_f$outcomes_indicators),ignore.case = T)
db_f$e_health  <-grepl("digital|tele-|technology|tecnolog-",paste(db_f$Long.op.name,db_f$objectives,db_f$objectives_indicators,db_f$outcomes,db_f$outcomes_indicators),ignore.case = T)
db_f$network   <-grepl("red|system|information|coordina-|integrate",paste(db_f$Long.op.name,db_f$objectives,db_f$objectives_indicators,db_f$outcomes,db_f$outcomes_indicators),ignore.case = T)

#Subcategorias de morbilidad
table(db_f$morbi_mat_inf) # check morbility

db_f$morbi_mat_inf<-with(db_f, morbility & grepl("infantil|mujeres|niños|niñez|infancia|neonatal-|embarazad-|matern-",paste(db_f$Long.op.name,db_f$objectives,db_f$objectives_indicators,db_f$outcomes,db_f$outcomes_indicators),ignore.case = T))
db_f$morbi_ncd<-with(db_f, morbility & grepl("noncommunicable|ncd|cardiovasc-|cerebrovasc-|cancer|cáncer|niñez|infancia|hipertension|diabetes|HTA|DM|mental|respira-|musco-",paste(db_f$Long.op.name,db_f$objectives,db_f$objectives_indicators,db_f$outcomes,db_f$outcomes_indicators),ignore.case = T))
db_f$morbi_cd<-with(db_f, covid|malaria|morbility & grepl("vih|hiv-|its|ets|malaria|covid|viruela|measles|leish-|-osis",paste(db_f$Long.op.name,db_f$objectives,db_f$objectives_indicators,db_f$outcomes,db_f$outcomes_indicators),ignore.case = T))
db_f$morbi_ext<-with(db_f, morbility & grepl("transito|accident|accidente|its|ets|malaria|covid|viruela|measles|leish-|-osis",paste(db_f$Long.op.name,db_f$objectives,db_f$objectives_indicators,db_f$outcomes,db_f$outcomes_indicators),ignore.case = T))

table(db_f$morbility,db_f$morbi_mat_inf) # Check morbi_mat_inf is T only if morbilidad is T
table(db_f$morbility,db_f$morbi_ecn) 
table(db_f$morbility,db_f$morbi_cd,db_f$malaria)

#Subcategorias calidad
table(db_f$quality) # check 

db_f$quality_resources<-with(db_f, quality & grepl("training|staff|equipo|medicament-|facili-|hospital-|capacity|capacidad|provisión",paste(db_f$Long.op.name,db_f$objectives,db_f$objectives_indicators,db_f$outcomes,db_f$outcomes_indicators),ignore.case = T))
db_f$quality_improvement<-with(db_f, quality & grepl("estándar|standard|procesos|processes|fortalec-|strength-|aument-|mejor-|enhanc-|improv-",paste(db_f$Long.op.name,db_f$objectives,db_f$objectives_indicators,db_f$outcomes,db_f$outcomes_indicators),ignore.case = T))
db_f$quality_opportunity<-with(db_f, quality & grepl("horas|time-|minimum",paste(db_f$Long.op.name,db_f$objectives,db_f$objectives_indicators,db_f$outcomes,db_f$outcomes_indicators),ignore.case = T))
db_f$quality_responsivennes<-with(db_f, quality & grepl("satisfacción|satisfaction|choice|elección|communitary|comunidad",paste(db_f$Long.op.name,db_f$objectives,db_f$objectives_indicators,db_f$outcomes,db_f$outcomes_indicators),ignore.case = T))

#Subcategirias efficiency
table(db_f$efficiency) # check 

db_f$efficiency_funding<-with(db_f, efficiency & grepl("beneficio|benefit|financing|brecha",paste(db_f$Long.op.name,db_f$objectives,db_f$objectives_indicators,db_f$outcomes,db_f$outcomes_indicators),ignore.case = T))

#Subcategirias groups
db_f$group_women<-with(db_f, health & grepl("mujer-|embarazad-",paste(db_f$Long.op.name,db_f$objectives,db_f$objectives_indicators,db_f$outcomes,db_f$outcomes_indicators),ignore.case = T))
db_f$group_child<-with(db_f, health & grepl("niñ-|infancia-|infatil",paste(db_f$Long.op.name,db_f$objectives,db_f$objectives_indicators,db_f$outcomes,db_f$outcomes_indicators),ignore.case = T))
db_f$group_youth<-with(db_f, health & grepl("jov-|young-|youth",paste(db_f$Long.op.name,db_f$objectives,db_f$objectives_indicators,db_f$outcomes,db_f$outcomes_indicators),ignore.case = T))
db_f$group_pwd<-with(db_f, health & grepl("discapacidad |young-|youth",paste(db_f$Long.op.name,db_f$objectives,db_f$objectives_indicators,db_f$outcomes,db_f$outcomes_indicators),ignore.case = T))
db_f$group_aging<-with(db_f, health & grepl("adulto- |ltc|adultez|vejez|personas mayores",paste(db_f$Long.op.name,db_f$objectives,db_f$objectives_indicators,db_f$outcomes,db_f$outcomes_indicators),ignore.case = T))

openxlsx::write.xlsx(db_f,file = paste0(path,"/db_classification.xlsx"))
write.csv(db_f,file = paste0(path,"/db_classification.csv"), fileEncoding="UTF-8")
write.csv(db_f,file = paste0(path_ext,"/db_classification.csv"), fileEncoding="UTF-8") #Path in OneDrive


### Classification & Subcategories para outcomes -------------------------------

file <- "/unique.outcomes.csv" #Database for analysis
#outcomes <- readxl::read_excel(paste0(path,file,sep=""))
data_base <- readr::read_csv(paste0(path,file,sep=""), show_col_types = FALSE) 

db_f <-data_base %>% filter(!not.a.loan) #Only loan operations
db_f <-data_base[!is.na(data_base$`Project Number`),]

# Classify projects 
# Falta incluir los outcomes en la clasificación
db_f$health    <-grepl("salud|health|saúde|diseas-|chronic|public|públic-|nutri-|illness",paste(db_f$Long.op.name,db_f$outcomes,db_f$outcomes_indicators),ignore.case = T)
db_f$morbility <-grepl("enferm-|mortalidad|mortality|mortalidade|morbilidad|morbidity|morbilidade|death|muerte",paste(db_f$Long.op.name,db_f$objectives,db_f$objectives_indicators,db_f$outcomes,db_f$outcomes_indicators),ignore.case = T)
db_f$malaria   <-grepl("malaria",paste(db_f$Long.op.name,db_f$objectives,db_f$objectives_indicators,db_f$outcomes,db_f$outcomes_indicators),ignore.case = T)
db_f$covid     <-grepl("covid|coronavirus",paste(db_f$Long.op.name,db_f$objectives,db_f$objectives_indicators,db_f$outcomes,db_f$outcomes_indicators),ignore.case = T)
db_f$acces     <-grepl("acceso|access|acesso|-cobertura|cobertura|atenção",paste(db_f$Long.op.name,db_f$objectives,db_f$objectives_indicators,db_f$outcomes,db_f$outcomes_indicators),ignore.case = T)
db_f$efficiency<-grepl("eficiencia|efficiency|efficient|efectividad",paste(db_f$Long.op.name,db_f$objectives,db_f$objectives_indicators,db_f$outcomes,db_f$outcomes_indicators),ignore.case = T)
db_f$equity    <-grepl("equidad|equity|equidade",paste(db_f$Long.op.name,db_f$objectives,db_f$objectives_indicators,db_f$outcomes,db_f$outcomes_indicators),ignore.case = T)
db_f$quality   <-grepl("calidad|quality|qualidade",paste(db_f$Long.op.name,db_f$objectives,db_f$objectives_indicators,db_f$outcomes,db_f$outcomes_indicators),ignore.case = T)
db_f$e_health  <-grepl("digital|tele-|technology|tecnolog-",paste(db_f$Long.op.name,db_f$objectives,db_f$objectives_indicators,db_f$outcomes,db_f$outcomes_indicators),ignore.case = T)
db_f$network   <-grepl("red|system|information|coordina-|integrate",paste(db_f$Long.op.name,db_f$objectives,db_f$objectives_indicators,db_f$outcomes,db_f$outcomes_indicators),ignore.case = T)

#Subcategorias de morbilidad
table(db_f$morbi_mat_inf) # check morbility

db_f$morbi_mat_inf<-with(db_f, morbility & grepl("infantil|mujeres|niños|niñez|infancia|neonatal-|embarazad-|matern-",paste(db_f$Long.op.name,db_f$objectives,db_f$objectives_indicators,db_f$outcomes,db_f$outcomes_indicators),ignore.case = T))
db_f$morbi_ncd<-with(db_f, morbility & grepl("noncommunicable|ncd|cardiovasc-|cerebrovasc-|cancer|cáncer|niñez|infancia|hipertension|diabetes|HTA|DM|mental|respira-|musco-",paste(db_f$Long.op.name,db_f$objectives,db_f$objectives_indicators,db_f$outcomes,db_f$outcomes_indicators),ignore.case = T))
db_f$morbi_cd<-with(db_f, covid|malaria|morbility & grepl("vih|hiv-|its|ets|malaria|covid|viruela|measles|leish-|-osis",paste(db_f$Long.op.name,db_f$objectives,db_f$objectives_indicators,db_f$outcomes,db_f$outcomes_indicators),ignore.case = T))
db_f$morbi_ext<-with(db_f, morbility & grepl("transito|accident|accidente|its|ets|malaria|covid|viruela|measles|leish-|-osis",paste(db_f$Long.op.name,db_f$objectives,db_f$objectives_indicators,db_f$outcomes,db_f$outcomes_indicators),ignore.case = T))

table(db_f$morbility,db_f$morbi_mat_inf) # Check morbi_mat_inf is T only if morbilidad is T
table(db_f$morbility,db_f$morbi_ecn) 
table(db_f$morbility,db_f$morbi_cd,db_f$malaria)

#Subcategorias calidad
table(db_f$quality) # check 

db_f$quality_resources<-with(db_f, quality & grepl("training|staff|equipo|medicament-|facili-|hospital-|capacity|capacidad|provisión",paste(db_f$Long.op.name,db_f$objectives,db_f$objectives_indicators,db_f$outcomes,db_f$outcomes_indicators),ignore.case = T))
db_f$quality_improvement<-with(db_f, quality & grepl("estándar|standard|procesos|processes|fortalec-|strength-|aument-|mejor-|enhanc-|improv-",paste(db_f$Long.op.name,db_f$objectives,db_f$objectives_indicators,db_f$outcomes,db_f$outcomes_indicators),ignore.case = T))
db_f$quality_opportunity<-with(db_f, quality & grepl("horas|time-|minimum",paste(db_f$Long.op.name,db_f$objectives,db_f$objectives_indicators,db_f$outcomes,db_f$outcomes_indicators),ignore.case = T))
db_f$quality_responsivennes<-with(db_f, quality & grepl("satisfacción|satisfaction|choice|elección|communitary|comunidad",paste(db_f$Long.op.name,db_f$objectives,db_f$objectives_indicators,db_f$outcomes,db_f$outcomes_indicators),ignore.case = T))

#Subcategirias efficiency
table(db_f$efficiency) # check 

db_f$efficiency_funding<-with(db_f, efficiency & grepl("beneficio|benefit|financing|brecha",paste(db_f$Long.op.name,db_f$objectives,db_f$objectives_indicators,db_f$outcomes,db_f$outcomes_indicators),ignore.case = T))

#Subcategirias groups
db_f$group_women<-with(db_f, health & grepl("mujer-|embarazad-",paste(db_f$Long.op.name,db_f$objectives,db_f$objectives_indicators,db_f$outcomes,db_f$outcomes_indicators),ignore.case = T))
db_f$group_child<-with(db_f, health & grepl("niñ-|infancia-|infatil",paste(db_f$Long.op.name,db_f$objectives,db_f$objectives_indicators,db_f$outcomes,db_f$outcomes_indicators),ignore.case = T))
db_f$group_youth<-with(db_f, health & grepl("jov-|young-|youth",paste(db_f$Long.op.name,db_f$objectives,db_f$objectives_indicators,db_f$outcomes,db_f$outcomes_indicators),ignore.case = T))
db_f$group_pwd<-with(db_f, health & grepl("discapacidad |young-|youth",paste(db_f$Long.op.name,db_f$objectives,db_f$objectives_indicators,db_f$outcomes,db_f$outcomes_indicators),ignore.case = T))
db_f$group_aging<-with(db_f, health & grepl("adulto- |ltc|adultez|vejez|personas mayores",paste(db_f$Long.op.name,db_f$objectives,db_f$objectives_indicators,db_f$outcomes,db_f$outcomes_indicators),ignore.case = T))

openxlsx::write.xlsx(db_f,file = paste0(path,"/db_classification.xlsx"))
write.csv(db_f,file = paste0(path,"/db_classification.csv"), fileEncoding="UTF-8")
write.csv(db_f,file = paste0(path_ext,"/db_classification.csv"), fileEncoding="UTF-8") #Path in OneDrive



### Filter profiles and save them ---------------------------------------------

#profiles <-profiles[profiles$`Project Number` %in% unique(s.ind$`Project Number`),]

# Save
#openxlsx::write.xlsx(profiles,file = paste0(path,"/unique.profiles.xlsx"))
#write.csv(s.ind,file = paste0(path,"/unique.profiles.csv"), fileEncoding="UTF-8")

# Load outputs
#outputs <- data.frame()

#list.files()
#list.files(path = "C:/Users/ncber/OneDrive/Documentos/R-code-health-outcome-analytics/input")

#file <- "/input/profiles.csv" #Tab: Operation Profile
#df_profile <- readr::read_csv(paste0(path,file,sep=""), show_col_types = FALSE) # From tableau

#file <- "/input/impact.csv" #Tab: General Development Objectives
#df_impact <- readr::read_csv(paste0(path,file,sep=""), show_col_types = FALSE) #, encoding="UTF-8") # From tableau

#file <- "/input/Outcomes.csv" #Tab: Specific Development Objectives
#df_outcomes <- readr::read_csv(paste0(path,file,sep=""), show_col_types = FALSE) # From tableau


#df  <-unique(df[c(2,7,8,9,11:13)])

# Attach component number to df
#df$`Component Statement` <- paste0("C",df$`Component Order Number`,"-",df$`Component Statement`)
#df$`Output Definition`   <- paste0("C",df$`Component Order Number`,"-",df$`Output Definition`)
#df$`Output UOM`          <- paste0("C",df$`Component Order Number`,"-",df$`Output UOM`)
#df$`Output MOV`          <- paste0("C",df$`Component Order Number`,"-",df$`Output MOV`)

#for (f in list.files(paste0(path,"/input/"))){
  
  #file <- "\\Outputs.ccb.xlsx"
  #file <- "\\Outputs.ccb.xlsx"
  #file <- "\\Outputs.ccb.xlsx"
  #file <- "\\Outputs.ccb.xlsx"
  
#  df  <-readr::read_csv(paste0(path,"/input/",f,sep=""))
  #df <-readr::read_csv(paste0(path,"",f,sep=""))
#  df  <-unique(df[c(2,7,8,9,11:13)])
  
  # Attach component number to df
#  df$`Component Statement` <- paste0("C",df$`Order Number`,"-",df$`Component Statement`)
#  df$`Output Definition`   <- paste0("C",df$`Order Number`,"-",df$`Output Definition`)
#  df$`Output UOM`          <- paste0("C",df$`Order Number`,"-",df$`Output UOM`)
#  df$`Output MOV`          <- paste0("C",df$`Order Number`,"-",df$`Output MOV`)
  
  # alternative levels of aggregation
#  components <-data.frame(ddply(unique(df[c(1,2)]),.(`Project Number`), summarize,components=paste(`Component Statement`,collapse = "\n")))
#  textputs   <-data.frame(ddply(unique(df[c(1,5)]),.(`Project Number`), summarize,outputs=paste(`Output Definition`,collapse = "\n")))
#  units      <-data.frame(ddply(unique(df[c(1,6)]),.(`Project Number`), summarize,output_units=paste(`Output UOM`,collapse = "\n")))
#  mov        <-data.frame(ddply(unique(df[c(1,7)]),.(`Project Number`), summarize,output_MoV=paste(`Output MOV`,collapse = "\n")))
  
  # keep one line per project
#  oput    <-merge(components,textputs,by="Project.Number")
#  oput    <-merge(oput,units,by="Project.Number")
#  oput    <-merge(oput,mov,by="Project.Number")
  
#  rm(components,textputs,units,mov)
  
#  outputs <- rbind(outputs,oput)
#  rm(df,oput)
#}
#rm(f)

# Merge
#output_prof    <- merge(outputs,profiles[c(1,2,4,5,8)],by.x = "Project Number",by.y="Project.Number", all.y = T)
#outcomes_outpu <- merge(output_prof,outcomes[c(1:4)],by.x = "Project Number",by.y="Project.Number", all.y = T)

#write.csv2(outcomes_outpu,"results.csv",row.names = F,fileEncoding = "UTF-8")

#output_rest <- merge(outcomes_outpu,impact,by="Project Number")
#results_int <-merge(results_imp,results_put,by="Project Number")
#results_int <- merge(results_int,results_come,by="Project Number",all = T)



#profiles <- readr::read_csv(paste0(path,file,sep=""), show_col_types = FALSE) # From tableau


#
#file_path <- "C:/Users/ncber/OneDrive/Documentos/R-code-health-outcome-analytics/input/Classification notes.txt"
#if (!file.exists(file_path)) {
#  stop("File does not exist: ", file_path)
#}

#files <- list.files(path = path)