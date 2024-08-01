# Path

#path <- "C:\\Users\\ramir\\OneDrive\ -\ Inter-American\ Development\ Bank Group\\Documents_IDB\\Action_Framework_SPH\\SPH_outputs_outcomes"

path <- "C:\\Users\\RAMIROG\\OneDrive\ -\ Inter-American\ Development\ Bank Group\\Documents_IDB\\Action_Framework_SPH\\SPH_outputs_outcomes" 

library(plyr)

# Load outputs and outcomes

file     <- "\\input\\Profiles.xlsx"                                                           # Operaciones SPH
profiles <- readxl::read_excel(paste0(path,file,sep=""))

profiles <-profiles[with(profiles,order(`Project Number`,`Cycle ID`)),]

# Leave the record of the latest Convergence cycle only
profiles$`Cycle ID` <-as.integer(profiles$`Cycle ID`)
profiles$max.cycle  <- ave(profiles$`Cycle ID`,profiles$`Project Number`,FUN = max)
profiles            <- profiles[profiles$`Cycle ID`==profiles$max.cycle,]
profiles$max.cycle  <-NULL

# Keep only health
profiles <- profiles[grepl("HEALTH",profiles$Sector,ignore.case = T),]

# Drop grants and policy loans
profiles <- profiles[profiles$`Operation Type`=="Loan Operation",]
profiles <- profiles[profiles$`Lending Instrument`=="Investment Loan",]

# Filter columns
profiles <-profiles[-c(1,2,3,6,7,8,9,10,15,20:21,23)]

profiles <-profiles[c(1,2,8,3:7,9:11)]


# General Development Objectives and Indicators -----------------------------------------------------
# Load Impacts (long term dev objectives)

file <- "\\input\\Impact.xlsx"
impact  <- readxl::read_excel(paste0(path,file,sep=""))

# General Development Objective
#g.obj <- unique(impact[c(2,4)])

g.ind  <-unique(impact[c(2,4,8,9,11)])
rm(impact)

# Bring name of operation from profile and reorder
g.ind  <-merge(g.ind,profiles[c(1,2)],by="Project Number",all.x = T)
g.ind$Long.op.name <-paste0(g.ind$`Project Number`," - ",g.ind$`Operation Name`)
g.ind <-g.ind[c(1,6,7,2,3,4,5)]
g.ind$indicator_unit <-paste0(g.ind$Indicators," - ",g.ind$`Unit of Measure`)

# Add plus sign to differentiate different lines when text in merged
g.ind$`General Development Objective` <-paste0("+",g.ind$`General Development Objective`)
g.ind$indicator_unit                  <-paste("+",g.ind$indicator_unit)

# collapse variables as text with newline within a cell


# alternaive levels of aggregation
g.ind_a <- data.frame(ddply(g.ind[c(1,4,8)], .(`Project Number`,`General Development Objective`), summarize, objectives_indicators_units = paste(indicator_unit, collapse = "\n")))
g.ind_b <- data.frame(ddply(unique(g.ind[c(1,4)]), .(`Project Number`), summarize, objectives=paste(`General Development Objective`,collapse="\n")))
g.ind_c <- data.frame(ddply(g.ind[c(1,4,8)], .(`Project Number`), summarize, objectives_indicators_units = paste(indicator_unit, collapse = "\n")))
g.ind_d <- data.frame(ddply(g.ind[c(1,5)], .(`Project Number`), summarize, objectives_indicators = paste(Indicators, collapse = "\n")))
g.ind_e <- data.frame(ddply(g.ind[c(1,6)], .(`Project Number`), summarize, objectives_units = paste(`Unit of Measure`, collapse = "\n")))

# keep one line per project
g.ind_u <-merge(g.ind_b,g.ind_d,by="Project.Number")
g.ind_u <-merge(g.ind_u,g.ind_e,by="Project.Number")

rm(g.ind_a,g.ind_b,g.ind_c,g.ind_d,g.ind_e)

# Create df to keep most recent baseline year for each project
y <- g.ind[c(1,7)]
y$`Baseline Year` <-as.integer(y$`Baseline Year`)
y$max.cycle       <-ave(y$`Baseline Year`,y$`Project Number`,FUN = max)
y                 <-y[y$`Baseline Year`==y$max.cycle,]
y$max.cycle       <-NULL
y                 <-y[!duplicated(y),]

# Add operation name and baseline year
g.ind_u <- merge(unique(g.ind[c(1,2,3)]),g.ind_u,by.x ="Project Number",by.y = "Project.Number")
g.ind_u <- merge(y,g.ind_u,by ="Project Number",all.y = T)
rm(y)

# Drop everything and keep g.ind as main dataframe

g.ind <- g.ind_u

rm(g.ind_u)

# Classify projects

g.ind$health      <-grepl("salud|health|saúde|morbi|morta|death|disease",paste(g.ind$Long.op.name,g.ind$objectives),ignore.case = T)
g.ind$malaria      <-grepl("malaria",paste(g.ind$Long.op.name,g.ind$objectives),ignore.case = T)

g.ind$covid        <-grepl("covid|coronavirus",g.ind$objectives,ignore.case = T)
g.ind$calidad      <-grepl("calidad|quality|qualidade",paste(g.ind$Long.op.name,g.ind$objectives),ignore.case = T)
g.ind$acceso       <-grepl("acceso|access|acesso",paste(g.ind$Long.op.name,g.ind$objectives),ignore.case = T)
g.ind$morbilidad   <-grepl("mortalidad|mortality|mortalidade|morbilidad|morbidity|morbilidade",paste(g.ind$Long.op.name,g.ind$objectives),ignore.case = T)
g.ind$eficiencia   <-grepl("eficiencia|efficiency|efficient",paste(g.ind$Long.op.name,g.ind$objectives),ignore.case = T)
g.ind$equidad      <-grepl("equidad|equity|equidade",paste(g.ind$Long.op.name,g.ind$objectives),ignore.case = T)

openxlsx::write.xlsx(g.ind,file = paste0(path,"\\unique.g.indicators.xlsx"))
#write.csv(g.ind,file = paste0(path,"\\unique.g.indicators.csv"))

# Outcomes -------------------------------------------------------------------

file <- "\\input\\Outcomes.xlsx"
outcomes <- readxl::read_excel(paste0(path,file,sep=""))

# Specific Development Objectives and Indicators
#s.obj <- unique(outcomes[c(2,4)])
s.ind <-unique(outcomes[c(2,4,10,11,14)])
rm(outcomes)

# old branch -------

# Bring name of operation from profile and reorder
#s.ind  <-merge(s.ind,profiles[c(1,2)],by="Project Number",all.x = T)
#s.ind$Long.op.name <-paste0(s.ind$`Project Number`," - ",s.ind$`Operation Name`)
#s.ind <-s.ind[c(1,6,2,3,4,5)]


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

s.ind$indicator_unit_means <-paste0(s.ind$`Indicator Definition`," - ",s.ind$`Unit of Measure`)

# Add plus sign to differentiate when text is wrapped
s.ind$`Specific Development objectives` <-paste0("+",s.ind$`Specific Development objectives`)
s.ind$`Indicator Definition`            <-paste0("+",s.ind$`Indicator Definition`)
s.ind$`Unit of Measure`                 <-paste0("+",s.ind$`Unit of Measure`)
s.ind$`Means of Verification`           <-paste0("+",s.ind$`Means of Verification`)

# alternative levels of aggregation
s.ind_a <- data.frame(ddply(s.ind[c(1,2,6)], .(`Project Number`,`Specific Development objectives`), summarize, outcomes_indicators_units = paste(indicator_unit_means, collapse = "\n")))
s.ind_b <- data.frame(ddply(unique(s.ind[c(1,2)]), .(`Project Number`), summarize, outcomes=paste(`Specific Development objectives`,collapse="\n")))

s.ind_c <- data.frame(ddply(s.ind[c(1,3)], .(`Project Number`), summarize, outcomes_indicators = paste(`Indicator Definition`, collapse = "\n")))
s.ind_d <- data.frame(ddply(s.ind[c(1,4)], .(`Project Number`), summarize, outcomes_units = paste(`Unit of Measure`, collapse = "\n")))
s.ind_e <- data.frame(ddply(s.ind[c(1,5)], .(`Project Number`), summarize, outcomes_means = paste(`Means of Verification`, collapse = "\n")))

# keep one line per project
s.ind_u <-merge(s.ind_b,s.ind_c,by="Project.Number")
s.ind_u <-merge(s.ind_u,s.ind_d,by="Project.Number")
s.ind_u <-merge(s.ind_u,s.ind_e,by="Project.Number")

rm(s.ind_a,s.ind_b,s.ind_c,s.ind_d,s.ind_e)

#View(g.ind_a[duplicated(g.ind_a[,1]),])

# Bring general objectives
s.ind_u <- merge(g.ind[c(1:7)],s.ind_u,by.x = "Project Number",by.y="Project.Number", all.y = T)

# Keep this files for exporting
s.ind <- s.ind_u  
rm(s.ind_u)

# Classifiy projects

s.ind$health       <-grepl("salud|health|saúde|morbi|morta|death|disease",paste(s.ind$Long.op.name,s.ind$objectives),ignore.case = T)
s.ind$malaria      <-grepl("malaria",paste(s.ind$Long.op.name,s.ind$objectives),ignore.case = T)
s.ind$covid        <-grepl("covid|coronavirus",s.ind$objectives,ignore.case = T)
s.ind$acceso       <-grepl("acceso|access|acesso",paste(s.ind$Long.op.name,s.ind$objectives),ignore.case = T)
s.ind$morbilidad   <-grepl("mortalidad|mortality|mortalidade|morbilidad|morbidity|morbilidade",paste(s.ind$Long.op.name,s.ind$objectives),ignore.case = T)
s.ind$eficiencia   <-grepl("eficiencia|efficiency|efficient",paste(s.ind$Long.op.name,s.ind$objectives),ignore.case = T)
s.ind$equidad      <-grepl("equidad|equity|equidade",paste(s.ind$Long.op.name,s.ind$objectives),ignore.case = T)

s.ind$calidad_name             <-grepl("calidad|quality|qualidade",s.ind$Long.op.name,ignore.case = T)
s.ind$calidad_g_objective      <-grepl("calidad|quality|qualidade",s.ind$objectives,ignore.case = T)
s.ind$calidad_outcome          <-grepl("calidad|quality|qualidade",s.ind$outcomes,ignore.case = T)
s.ind$calidad_outcome_ind      <-grepl("calidad|quality|qualidade",s.ind$outcomes_indicators,ignore.case = T)

# Manual adjustment of the classification
s.ind$not.a.loan <-grepl("-J|-G|-U|RG",s.ind$`Project Number`)

s.ind$health[which(s.ind$`Project Number`=="EC-L1236")] <-   FALSE  # Este es discapacidad
s.ind$health[which(s.ind$`Project Number`=="EC-L1258")] <-   FALSE  # Este es DiT
s.ind$health[which(s.ind$`Project Number`=="PN-L1160")] <-   FALSE  # Este es Protección Social
s.ind$health[which(s.ind$`Project Number`=="VE-L1017")] <-   FALSE  # Este es Orquesta
s.ind$health[which(s.ind$`Project Number`=="JA-L1053")] <-   FALSE  # Este es Protección Social

# HO-L1194 es de proteccion social con un componente de salud. Lo dejo como proteccion social

#x<-s.ind[s.ind$health==T&s.ind$malaria==F&s.ind$not.a.loan==F,]

openxlsx::write.xlsx(s.ind,file = paste0(path,"\\unique.outcomes.xlsx"))
#write.csv(s.ind,file = paste0(path,"\\unique.s.indicators.csv"))

# Filter profliles and save them ---------

profiles <-profiles[profiles$`Project Number` %in% unique(s.ind$`Project Number`),]

# Save
openxlsx::write.xlsx(profiles,file = paste0(path,"\\unique.profiles.xlsx"))
#write.csv(profiles,file = paste0(path,"\\unique.profiles.csv"))


# Load outputs

outputs <- data.frame()

for (f in list.files(paste0(path))){
  
#file <- "\\Outputs.ccb.xlsx"
#file <- "\\Outputs.ccb.xlsx"
#file <- "\\Outputs.ccb.xlsx"
#file <- "\\Outputs.ccb.xlsx"

df      <- readxl::read_excel(paste0(path,"\\input\\",f,sep=""))
df      <-unique(df[c(2,7,8,9,11:13)])

# Attach component number to df
df$`Component Statement` <- paste0("C",df$`Component Order Number`,"-",df$`Component Statement`)
df$`Output Definition`   <- paste0("C",df$`Component Order Number`,"-",df$`Output Definition`)
df$`Output UOM`          <- paste0("C",df$`Component Order Number`,"-",df$`Output UOM`)
df$`Output MOV`          <- paste0("C",df$`Component Order Number`,"-",df$`Output MOV`)

# alternative levels of aggregation
components  <-data.frame(ddply(unique(df[c(1,2)]),.(`Project Number`), summarize,components=paste(`Component Statement`,collapse = "\n")))
textputs    <-data.frame(ddply(unique(df[c(1,5)]),.(`Project Number`), summarize,outputs=paste(`Output Definition`,collapse = "\n")))
units       <-data.frame(ddply(unique(df[c(1,6)]),.(`Project Number`), summarize,output_units=paste(`Output UOM`,collapse = "\n")))
mov         <-data.frame(ddply(unique(df[c(1,7)]),.(`Project Number`), summarize,output_MoV=paste(`Output MOV`,collapse = "\n")))

# keep one line per project
oput    <-merge(components,textputs,by="Project.Number")
oput    <-merge(oput,units,by="Project.Number")
oput    <-merge(oput,mov,by="Project.Number")

rm(components,textputs,units,mov)

outputs <- rbind(outputs,oput)
rm(df,oput)
}
rm(f)

# Merge
output_prof         <- merge(outputs,profiles[c(1,2,4,5,8)],by="Project Number")
outcomes_outpu      <- merge(output_prof,outcomes[c(1:4)],by="Project Number")

write.csv2(outcomes_outpu,"results.csv",row.names = F,fileEncoding = "UTF-8")

output_rest         <- merge(outcomes_outpu,impact,by="Project Number")



results_int      <-merge(results_imp,results_put,by="Project Number")
results_int      <- merge(results_int,results_come,by="Project Number",all = T)


str(profiles)

# Classify  health - social protection poverty - ECD - other

# Identify covid response operations

profiles$Covid <- grepl("Covid|Coronavirus",profiles$`Operation Name`,ignore.case = T)

table(profiles$Sector)

# Exclude non SPH project

profiles <-profiles[!grepl("REFORM",profiles$Sector ),]

profiles$health <- grepl("health",profiles$Sector,ignore.case = T)
profiles$child  <- grepl("child",profiles$Sector,ignore.case = T)   
profiles$social <- grepl("social",profiles$Sector,ignore.case = T)


#df$`Cycle ID` <-as.integer(df$`Cycle ID`)
#df$max.cycle  <- ave(df$`Cycle ID`,df$`Project Number`,FUN = max)
#df            <- df[df$`Cycle ID`==df$max.cycle,]
#df$max.cycle  <-NULL
#
#df$year       <-as.integer(gsub("EOP ","",df$`Year Display`))
#df$max.year   <-ave(df$year,df$`Project Number`,FUN = max)
#df            <- df[df$year==df$max.year,]
#df$max.year   <-NULL

impact$`Cycle ID` <-as.integer(impact$`Cycle ID`)
impact$max.cycle  <- ave(impact$`Cycle ID`,impact$`Project Number`,FUN = max)
impact            <- impact[impact$`Cycle ID`==impact$max.cycle,]
impact$max.cycle  <-NULL

profiles            <- profiles[!grepl("Applied",profiles$`Cycle ID`),]             # Elminate redundant line

profiles            <-profiles[!is.na(profiles$`Project Number`),]