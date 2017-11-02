
# R CODE FOR IMPORTING, MANIPULATING, AND ANALYZING THE DATASETS USED IN ESPIN ET AL (Plos Biology 2017)
# The version of this code used in the article is frozen at Zenodo as v 1.0
# If modifying for analyses please cite as:
# The code is also available for improvement at 

# Note that these analyses use the following data packages available at #Dryad: 

# 1) Cho et al. (2014) Data from: Women are underrepresented on the editorial boards of journals in 
# environmental biology and natural resource management. Dryad Digital Repository. https://doi.org/10.5061/dryad.6jn86.2
# from the article Cho et al. (2014) Women are underrepresented on the editorial boards of journals in environmental 
# biology and natural resource management. PeerJ 2: e542. https://doi.org/10.7717/peerj.542
# and 
# 2) Espin et al. (2017) Data from: 
# from the article: Espin et al. (2017)

# If you use these datasets in publications please cite both the data packages and the original articles.
# If using this code please cite the Zenodo DOI...  


##############################################################
##############################################################
#
# R & Package versions:
# R version = 3.3.1
# vegan = 2.4.2
# tidyverse = 1.0
# MuMIn = 1.15.6
# nlme = 3.1.128
#
##############################################################
##############################################################

##############################################################
##############################################################
#
# GETTING STARTED
#
##############################################################
##############################################################

# Load required libraries
library(tidyverse)
library(vegan)
library(nlme)
library(MuMIn)
library(directlabels)
library(grid)
library(gridExtra)
library(RColorBrewer)




# Clear the environment 
rm(list=ls())

##############################################################
##############################################################
#
# UPLOAD & STANDARDIZE DATA ON EDITORS
#
##############################################################
##############################################################

# Data on Editors from Cho et al. 2014
Cho<-read.csv("./Data/Drayd.Cho.v2.csv", dec=".", header = TRUE, sep = ",", check.names=FALSE )
#minor corrections to Cho
Cho$COUNTRY<-as.character(Cho$COUNTRY)
Cho$COUNTRY[Cho$COUNTRY=="UK"] <- "United Kingdom"
Cho$COUNTRY[Cho$COUNTRY=="USSR"] <- "Russia"
Cho$COUNTRY[Cho$COUNTRY=="ITALY"] <- "Italy"
Cho$COUNTRY[Cho$COUNTRY=="United States"] <- "USA"
Cho$COUNTRY[Cho$COUNTRY=="Usa"] <- "USA"
Cho$COUNTRY[Cho$COUNTRY=="UsA"] <- "USA"
# Convert Country back to factor
Cho$COUNTRY<-as.factor(Cho$COUNTRY)
droplevels(Cho$COUNTRY)

# Data on Editors from Espin et al. 2017 
Espin<-read.csv("./Data/Drayd.Espin.v1.csv", dec=".", header = TRUE, sep = ",", check.names=FALSE )
#Corrections to Espin
Espin$COUNTRY<-as.character(Espin$COUNTRY)
Espin$COUNTRY[Espin$COUNTRY=="Czech Republicch Republic"] <- "Czech Republic"
Espin$COUNTRY[Espin$COUNTRY=="Brazil And UK"] <- "UK"
Espin$COUNTRY[Espin$COUNTRY=="Australiatralia"] <- "Australia" 
Espin$COUNTRY[Espin$COUNTRY=="P.R. China"] <- "China"
Espin$COUNTRY[Espin$COUNTRY=="IRAN"] <- "Iran"
Espin$COUNTRY[Espin$COUNTRY=="ITALY"] <- "Italy"
Espin$COUNTRY[Espin$COUNTRY=="MEXICO"] <- "Mexico"
Espin$COUNTRY[Espin$COUNTRY=="The Netherlands"] <- "Netherlands"
Espin$COUNTRY[Espin$COUNTRY=="NewZealand"] <- "New Zealand"
Espin$COUNTRY[Espin$COUNTRY=="PuertoRico"] <- "Puerto Rico"
Espin$COUNTRY[Espin$COUNTRY=="UK"] <- "United Kingdom"
Espin$COUNTRY[Espin$COUNTRY=="United States"] <- "USA"
Espin$COUNTRY[Espin$COUNTRY=="Usa"] <- "USA"
Espin$COUNTRY[Espin$COUNTRY=="UsA"] <- "USA"
Espin$COUNTRY[Espin$COUNTRY=="USSR"] <- "Russia"
Espin$COUNTRY<-as.factor(Espin$COUNTRY)
droplevels(Espin$COUNTRY)


#Bind the datasets together
ALLDATA<-bind_rows(Cho,Espin, id=NULL)

#Add the ISO Code for the country in which editors are based 
source("Country.Codes.R")
ALLDATA<-Country.Codes(ALLDATA)
levels(ALLDATA$geo.code)
levels(as.factor(ALLDATA$COUNTRY))
# Add the geographic region & national income category in which editors are based
source("AddIncomeRegion.R")
ALLDATA<-AddIncomeRegion(ALLDATA)

## Espin et al considered French Guiana = HIGH INCOME OECD income category and region = LATIN AMERICA/CARRIBBEAN 
# This is because scientists there have access to greater financial resources because they are 
# part of France. (NB: Puerto Rico is treated the same way but that is already done in function AddIncomeRegion)
ALLDATA$REGION[ALLDATA$geo.code == "GUF"]  <- "Latin America & Caribbean"
ALLDATA$INCOME_LEVEL[ALLDATA$geo.code == "GUF"]  <- "High income: OECD"

##############################################################
##############################################################
#
# UPLOAD & STANDARDIZE DATA ON AUTHORS
#
##############################################################
##############################################################

AuthorGeo_1985_2014<-read.csv("./Data/AuthorCountries_1985_2014.csv", dec=".", header = TRUE, sep = ",", check.names=FALSE, row.names = 1)
# if you want to see it looks ok use the following line:
# AuthorGeo_1985_2014

AuthorGeo_1985_2014$COUNTRY[AuthorGeo_1985_2014$COUNTRY=="FED REP GER"] <- "GERMANY"

levels(as.factor(AuthorGeo_1985_2014$COUNTRY))

#add country codes 
source("Country.Codes.R")
AuthorGeo_1985_2014<-Country.Codes(AuthorGeo_1985_2014)
levels(AuthorGeo_1985_2014$geo.code)
#add author region & income 
source("AddIncomeRegion.R")
AuthorGeo_1985_2014<-AddIncomeRegion(AuthorGeo_1985_2014)

# These don't have world bank information in the data file, so we added it ourselves.  
AuthorGeo_1985_2014$REGION[AuthorGeo_1985_2014$geo.code == "COK"]  <- "East Asia & Pacific" # COOK ISLANDS
AuthorGeo_1985_2014$INCOME_LEVEL[AuthorGeo_1985_2014$geo.code == "COK"]  <- "High income: OECD"# COOK ISLANDS

AuthorGeo_1985_2014$REGION[AuthorGeo_1985_2014$geo.code == "GLP"]  <- "Latin America & Caribbean" # GUADELOUPE
AuthorGeo_1985_2014$INCOME_LEVEL[AuthorGeo_1985_2014$geo.code == "GLP"]  <- "High income: OECD"# GUADELOUPE

AuthorGeo_1985_2014$REGION[AuthorGeo_1985_2014$geo.code == "GUF"]  <- "Latin America & Caribbean" # FRENCH GUIANA
AuthorGeo_1985_2014$INCOME_LEVEL[AuthorGeo_1985_2014$geo.code == "GUF"]  <- "High income: OECD" # FRENCH GUIANA

AuthorGeo_1985_2014$REGION[AuthorGeo_1985_2014$geo.code == "MTQ"]  <- "Latin America & Caribbean" #  MARTINIQUE
AuthorGeo_1985_2014$INCOME_LEVEL[AuthorGeo_1985_2014$geo.code == "MTQ"]  <- "High income: OECD" #  MARTINIQUE

AuthorGeo_1985_2014$REGION[AuthorGeo_1985_2014$geo.code == "REU"]  <- "Sub-Saharan Africa" # REUNION
AuthorGeo_1985_2014$INCOME_LEVEL[AuthorGeo_1985_2014$geo.code == "REU"]  <- "High income: OECD"# REUNION 

AuthorGeo_1985_2014$REGION[AuthorGeo_1985_2014$geo.code == "WLF"]  <- "East Asia & Pacific" # WALLIS & FUTUNA ISLANDS
AuthorGeo_1985_2014$INCOME_LEVEL[AuthorGeo_1985_2014$geo.code == "WLF"]  <- "High income: OECD"# WALLIS & FUTUNA ISLANDS

AuthorGeo_1985_2014$REGION[AuthorGeo_1985_2014$geo.code == "ANT"]  <- "Latin America & Caribbean" # Netherland Antilles
AuthorGeo_1985_2014$INCOME_LEVEL[AuthorGeo_1985_2014$geo.code == "ANT"]  <- "High income: OECD"# Netherland Antilles


##############################################################
##############################################################
#
# ANALYSES
#
##############################################################
##############################################################

# DEFINE THE TEMPORAL RANGE OF THE ANALYSES 
# Espin et al. Plos Biology analyzed editoral board composition 1985-2014
FirstYear=1985
LastYear=2014

# The analyses are conducted on a new dataframe ("AnalysisData") 
# spanning only the years of interest to allow one to change the time period
# of interest without reloading, rebinding, & restandardize the datasets
AnalysisData<-ALLDATA[ALLDATA$YEAR>=FirstYear & ALLDATA$YEAR<=LastYear,]

# SELECT THE EDITOR CATEGORIES FOR ANALYSIS
# Espin et al 2017: analyses were done with EIC, AE, SE, and Special Editors
AnalysisData <- AnalysisData[AnalysisData$CATEGORY %in% c('EIC', 'AE', 'SE', 'SPECIAL'),]

# delete unused columns 
AnalysisData<-AnalysisData %>% select(-NOTES,-GENDER, -VOLUME, -ISSUE, -TITLE)
# Convert editor_ID to type: factor
AnalysisData$editor_id<-as.factor(AnalysisData$editor_id)
# Convert COUNTRY to type: factor
AnalysisData$COUNTRY<-as.factor(AnalysisData$COUNTRY)
# Convert YEAR to type: numeric
AnalysisData$YEAR<-as.numeric(AnalysisData$YEAR)
# Change the names of the levels of the factors to abbreviations to make the figures cleaner 
# str(AnalysisData)
levels(AnalysisData$JOURNAL)
AnalysisData$JOURNAL<-as.factor(AnalysisData$JOURNAL)

# Use the following section to change the abbreviations for journals 
# into the names you want displayed in figures
AnalysisData$JOURNAL <-recode(AnalysisData$JOURNAL,'BITR'="Biotropica", 
                              'PLANTECOL'="Plant Ecology",
                              'OIKOS'="Oikos", 
                              'OECOL'="Oecologia",
                              'NEWPHYT'="New Phytologist",
                              'NAJFM'="N Am J Fisheries Manag",
                              'AREES'="Ann Rev Ecol, Evol, & Syst",
                              'JANE'="J Animal Ecology",
                              'JAPE'="J Applied Ecology",
                              'JECOL'="J Ecology",
                              'JZOOL'="J Zoology",
                              'LECO'="Landscape Ecology",
                              'FUNECOL'="Functional Ecology",
                              'FEM'="Forest Ecology & Manag",
                              'EVOL'="Evolution",
                              'ECOGRAPHY'="Ecography",
                              'JBIOG'="J Biogeography",
                              'AMNAT'="American Naturalist",
                              'BIOCON'="Biological Conservation",
                              'ECOLOGY'="Ecology",
                              'CONBIO'="Conservation Biology",
                              'AJB'="Am J Botany",
                              'AGRONOMY'="Agronomy Journal",
                              'JTE'="J Tropical Ecology")

# Use the following section to change the regions into how you want them displayed in figures
AnalysisData$REGION <-recode(AnalysisData$REGION,'North America'="N Amer", 
                             'Europe & Central Asia'="Eur & C Asia", 
                             'East Asia & Pacific'="E Asia & Pac",
                             'Latin America & Caribbean'="LatAm & Carib",
                             'Sub-Saharan Africa'="Sub-Sah Afr",
                             'South Asia'="S Asia",
                             'Middle East & North Africa'="M East & N Afr")

# Use the following section to change the income levels into how you want them displayed in figures
AnalysisData$INCOME_LEVEL <-recode(AnalysisData$INCOME_LEVEL,'High income: OECD'="High OECD", 
                                 'High income: nonOECD'="High nonOECD", 
                                 'Upper middle income'="Upper Middle",
                                 'Lower middle income'="Lower Middle",
                                 'Low income'="Low")

#############################################################




##############################################################
##############################################################
# Total Number of (Unique) Editors in the Community
eds<-AnalysisData %>% summarise(n_distinct(editor_id))
eds
##############################################################

##############################################################
# Total number of countries represented by Editors 
geocodes<-AnalysisData %>% summarise(n_distinct(geo.code))
geocodes
levels(AnalysisData$geo.code)

countries<-AnalysisData %>% summarise(n_distinct(COUNTRY))
countries
levels(AnalysisData$COUNTRY)
##############################################################

##############################################################
# Cumulative Geographic Richness: Cumulative Number of countries represented through year X
# Used in Figure 1A
# Use Rarefaction curves generated by vegan then convert back to tibble
editorAcum<-AnalysisData %>% group_by(YEAR, geo.code) %>% summarize(yr_tot = n_distinct(geo.code))
editorAcum<-spread(editorAcum, geo.code,yr_tot) 
editorAcum[is.na(editorAcum)] <- 0
editorAcum<-ungroup(editorAcum)
editorAcum<-select(editorAcum,-YEAR)
editorAcum<-specaccum(editorAcum, "collector")
editorAcum<-as_tibble(editorAcum$richness)
names(editorAcum)[1] <- "CumulativeRichness"
editorAcum$YEAR<-seq(FirstYear,LastYear,1)
editorAcum
##############################################################

##############################################################
# Annual Geographic Richness: Number of countries represented in year X 
# Used in Figure 1A
GEOperYR<-AnalysisData %>% group_by(YEAR) %>% summarize(AnnualRichness = n_distinct(geo.code)) 
GEOperYR
##############################################################

##############################################################
# Total Number of Editors (all jrnls pooled)  vs. Year
# Used in Figure 1B
EdsPerYr<-AnalysisData %>% group_by(YEAR) %>% summarize(TotalEditors = n_distinct(editor_id))
EdsPerYr
##############################################################

##############################################################
# Geographic Diversity (all journals pooled) 
# Used in Figure 1C
DivDataPooled<-AnalysisData %>% group_by(YEAR, geo.code) %>% summarize(Total = n_distinct(editor_id)) 
# DivDataPooled<-as.data.frame(EdsPerCountryPerJrnlPerYr.LONG)
DivDataPooled<-DivDataPooled %>% group_by(YEAR, geo.code) %>% summarise(Total_Eds=sum(Total))
DivDataPooled<-spread(DivDataPooled, geo.code, Total_Eds) 
DivDataPooled[is.na(DivDataPooled)] <- 0
DivDataPooled<-ungroup(DivDataPooled)
# 4: Geo Diverisity using Inverse Simpson's Index (expressed as 1/D)
IsimpDivTable <- diversity((DivDataPooled %>% select(-YEAR)), index="invsimpson") #Need to strip away the journal and year columns for vegan to do the analysis
# Table DIVERSITY with Results and Journals
IsimpDivTable <- data.frame(IsimpDivTable)
IsimpDivTable$YEAR <-DivDataPooled$YEAR #Add year as a column
IsimpDivTable<-rename(IsimpDivTable, InvSimpson=IsimpDivTable) #rename the columns
IsimpDivTable <- IsimpDivTable[c("YEAR","InvSimpson")] #reorder the columns
IsimpDivTable<-as_tibble(IsimpDivTable)


# THIS CALCLULATES THE SIMPSONS INDEX (expressed as 1-D)
simpDivTable <- diversity((DivDataPooled %>% select(-YEAR)), index="simpson") #Need to strip away the journal and year columns for vegan to do the analysis
# Table DIVERSITY with Results and Journals
simpDivTable <- data.frame(simpDivTable)
simpDivTable$YEAR <-DivDataPooled$YEAR #Add year as a column
simpDivTable<-rename(simpDivTable, Simpson=simpDivTable) #rename the columns
simpDivTable <- simpDivTable[c("YEAR","Simpson")] #reorder the columns
simpDivTable<-as_tibble(simpDivTable)


IsimpDivTable<-full_join(GEOperYR,IsimpDivTable, by="YEAR")
IsimpDivTable<-full_join(IsimpDivTable,simpDivTable, by="YEAR")

# 4: Geographic Evenness (all journals pooled). We do not present results for "Evenness" of the Editors
# but it is straightforward to calculate.
IsimpDivTable<-mutate(IsimpDivTable, Geo.Evenness = InvSimpson/AnnualRichness)
IsimpDivTable
##############################################################

##############################################################
# Number and Pcnt of Editors from Each Country (all journals and years pooled)
# Used for Fig 2A
Editor.Geo<-AnalysisData %>%  group_by(geo.code) %>% 
  summarize(N_editors = n_distinct(editor_id)) %>% 
  mutate(Pcnt_editors= (N_editors/sum(N_editors)*100)) %>% 
  arrange(desc(Pcnt_editors))
Editor.Geo
##############################################################

##############################################################
# Number & Percentage of Editors from Different Regions (all journals pooled)
# Used for Fig. 2b
RegionPlot<-AnalysisData %>% select(YEAR,editor_id,REGION,CATEGORY) %>% group_by(editor_id) 
RegionPlot<-distinct(RegionPlot, editor_id,YEAR, .keep_all = TRUE)
RegionPlot<-RegionPlot %>% group_by(YEAR,REGION) %>% count(YEAR,REGION)
RegionPlot<-RegionPlot %>% group_by(YEAR) %>% mutate(yr_tot=sum(n)) %>% mutate(Percent=n/yr_tot*100) 
RegionPlot
# RegionPlot %>%  group_by(YEAR) %>% mutate(sum=sum(Percent)) #checks that add up to 100%
##############################################################

##############################################################
# Number & Percentage of Editors from Different Income Levels, all journals pooled 
# USed for Fig. 2c
IncomePlot<-AnalysisData %>% select(YEAR,editor_id,INCOME_LEVEL,CATEGORY) %>% group_by(editor_id) 
IncomePlot<-distinct(IncomePlot, editor_id,YEAR, .keep_all = TRUE)
IncomePlot<-IncomePlot %>% group_by(YEAR,INCOME_LEVEL) %>% count(YEAR,INCOME_LEVEL)
IncomePlot<-IncomePlot %>% group_by(YEAR) %>% mutate(yr_tot=sum(n)) %>% mutate(Percent=n/yr_tot*100) 
IncomePlot
#IncomePlot %>%  group_by(YEAR) %>% mutate(sum=sum(Percent)) #checks that add up to 100%
##############################################################



######################################################
######################################################
#
# FIGURES AND TABLES
#
######################################################
######################################################
# 
# str(AnalysisData)
# # Convert Journal Codes to Journal Names (for figures and analyses)
# AnalysisData$JOURNAL[AnalysisData$JOURNAL=="BITR"] <- "Biotropica"
# AnalysisData$JOURNAL[AnalysisData$JOURNAL=="PLANTECOL"] <- "Plant Ecology"
# AnalysisData$JOURNAL[AnalysisData$JOURNAL=="AGRONOMY"] <- "Agronomy Journal"
# AnalysisData$JOURNAL[AnalysisData$JOURNAL=="AJB"] <- "American J. Botany"
# AnalysisData$JOURNAL[AnalysisData$JOURNAL=="CONBIO"] <- "Conservation Biology"
# AnalysisData$JOURNAL[AnalysisData$JOURNAL=="ECOLOGY"] <- "Ecology"
# AnalysisData$JOURNAL[AnalysisData$JOURNAL=="BIOCON"] <- "Biological Conservation"
# AnalysisData$JOURNAL[AnalysisData$JOURNAL=="JECOL"] <- "J. of Ecology"
# AnalysisData$JOURNAL[AnalysisData$JOURNAL=="JTE"] <- "J. Tropical Ecology"
# AnalysisData$JOURNAL[AnalysisData$JOURNAL=="AMNAT"] <- "American Naturalist"
# AnalysisData$JOURNAL[AnalysisData$JOURNAL=="JBIOG"] <- "J. Biogeography"
# AnalysisData$JOURNAL[AnalysisData$JOURNAL=="ECOGRAPHY"] <- "Ecography"
# AnalysisData$JOURNAL[AnalysisData$JOURNAL=="EVOL"] <- "Evolution"
# AnalysisData$JOURNAL[AnalysisData$JOURNAL=="FEM"] <- "Forest Ecology & Managment"
# AnalysisData$JOURNAL[AnalysisData$JOURNAL=="FUNECOL"] <- "Functional Ecology"
# AnalysisData$JOURNAL[AnalysisData$JOURNAL=="LECO"] <- "Landscape Ecology"
# AnalysisData$JOURNAL[AnalysisData$JOURNAL=="JZOOL"] <- "J. Zoology"
# AnalysisData$JOURNAL[AnalysisData$JOURNAL=="JAPE"] <- "J. Applied Ecology"
# AnalysisData$JOURNAL[AnalysisData$JOURNAL=="JANE"] <- "J. Animal Ecology"
# AnalysisData$JOURNAL[AnalysisData$JOURNAL=="NEWPHYT"] <- "New Phytologist"
# AnalysisData$JOURNAL[AnalysisData$JOURNAL=="OECOL"] <- "Oecologia"
# AnalysisData$JOURNAL[AnalysisData$JOURNAL=="OIKOS"] <- "Oikos"
# 
# 
# 


##############################################################
# TABLE 1
##############################################################

# Total Editors in each National Income Class
EdsByIncome<-AnalysisData %>% select(INCOME_LEVEL,editor_id)
EdsByIncome<-distinct(EdsByIncome) %>% group_by(INCOME_LEVEL) %>% summarize(Total = n_distinct(editor_id))
sum(EdsByIncome$Total)

# Total by Region
EdsByRegion<-AnalysisData %>% group_by(REGION) %>% summarize(Total = n_distinct(editor_id))
sum(EdsByRegion$Total)

EdsByRegion<-AnalysisData %>% select(REGION,editor_id)
EdsByRegion<-distinct(EdsByRegion) %>% group_by(REGION) %>% summarize(Total = n_distinct(editor_id))

#Number & Percentage of Editor Types from Different Regions, all years pooled
EdCat.region<-AnalysisData %>% group_by(CATEGORY, REGION)  %>%  summarize(N=n_distinct(editor_id))  %>% mutate(Pcnt=N/sum(N)*100) 
EdCat.region$Pcnt<-round(EdCat.region$Pcnt, digits=2)
sumsI<-EdCat.region %>% group_by(CATEGORY) %>% summarize(sum=sum(N))
EdCat.region<-EdCat.region %>% select(-N) %>% spread(CATEGORY, Pcnt)
EdCat.region[is.na(EdCat.region)] <- 0
EdCat.region

# Number & Percentage of Editor Types from Different Income Levels, all years pooled 
EdCat.income<-AnalysisData %>% group_by(CATEGORY, INCOME_LEVEL)  %>%  summarize(N=n_distinct(editor_id))  %>% mutate(Pcnt=N/sum(N)*100) 
EdCat.income$Pcnt<-round(EdCat.income$Pcnt, digits=2)
sumsR<-EdCat.income %>% group_by(CATEGORY) %>% summarize(sum=sum(N))
EdCat.income<-EdCat.income %>% select(-N) %>% spread(CATEGORY, Pcnt)
EdCat.income[is.na(EdCat.income)] <- 0

EdsByIncome
EdsByRegion
EdCat.income
EdCat.region



##############################################################
# Fig 1A: Total Number of Editors (all journals pooled)  vs. Year
##############################################################

plotTOTALEDSvYear<-ggplot(EdsPerYr, aes(x=YEAR, y=TotalEditors)) +
  #ylab("Number of Editors") +
  xlab("Year")+
  ylab("N")+
  geom_line(size=1, color="blue")+
  ggtitle('(A) Number of Editors') + 
  geom_point(color="black", shape=1)+
  scale_y_continuous(breaks=seq(0, 1500, 150))+
  scale_x_continuous(breaks=seq(FirstYear, LastYear+1, 5))

plotTOTALEDSvYear<-plotTOTALEDSvYear+theme_classic()+
  theme( axis.text=element_text(colour="black", size = 10),  
        # axis.title.y=element_text(colour="black", size = 14, vjust=2),            #sets y axis title size, style, distance from axis #add , face = "bold" if you want bold
        #axis.title.y=element_blank(),
        # axis.title.x=element_text(colour="black", size = 14, vjust=0),
        legend.text = element_text(color="black", size=10),  
        legend.position = c(0.9,0.8),
        legend.title = element_blank(),   #Removes the Legend title
        legend.background = element_rect(colour = 'black', size = 0.5, linetype='solid'))
        # plot.margin =unit(c(1,1.5,1,1), "cm")) #+  #plot margin - top, right, bottom, left
plotTOTALEDSvYear



##############################################################
# Fig. 1B: Cumulative Geo Richness
##############################################################

# PUT THE NECESSARY DATA IN ONE DATAFRAME 
jointGEOperYR<-GEOperYR
jointedAccDF<-editorAcum

jointGEOperYR<-rename(jointGEOperYR,Countries=AnnualRichness)
jointRichness<-full_join(jointGEOperYR, jointedAccDF, by = "YEAR")
jointRichness<-gather(jointRichness, "Richness","N", 2:3)
jointRichness[jointRichness=="Countries"]<-"Annual"
jointRichness[jointRichness=="CumulativeRichness"]<-"Cumulative"
rm(jointGEOperYR,jointedAccDF)

#plot cumulative and annual richness same plot
jointRichnessPlot<-ggplot(jointRichness, aes(x=YEAR, y=N, group = Richness, colour = Richness)) +
  geom_line(size=1) +
  scale_color_manual(values=c("blue", "red"))+
  geom_text(data = jointRichness[jointRichness$YEAR=="2012" & jointRichness$Richness=="Annual",], aes(label = Richness), hjust = 1.0, vjust = 2.5, size=3.5) +
  geom_text(data = jointRichness[jointRichness$YEAR=="2012" & jointRichness$Richness=="Cumulative",], aes(label = Richness), hjust = 2.0, vjust = 1.35, size=3.5) +
  #ylab("Number of Countries") +
  xlab("Year")+
  ylab("Richness")+
  ggtitle('(B) Editor Geographic Richness')+
  geom_point(color="black", shape=1)+
  # scale_y_continuous(breaks=seq(0, 1, 0.1))+
  scale_y_continuous(limits = c(25, 75))+
  scale_x_continuous(breaks=seq(FirstYear, LastYear+1, 5))

jointRichnessPlot<-jointRichnessPlot+theme_classic()+
  theme(axis.text=element_text(colour="black", size = 10),         #sets x axis title size, style, distance from axis #add , face = "bold" if you want bold
        #axis.title.y=element_text(colour="black", size = 14, vjust=2),            #sets y axis title size, style, distance from axis #add , face = "bold" if you want bold
        #axis.title.y=element_blank(),
        # axis.title.x=element_text(colour="black", size = 14, vjust=0),   
        legend.title = element_blank(),   #Removes the Legend title
        legend.text = element_text(color="black", size=10),  
        # legend.position = c(0.9,0.8),
        legend.position = ("none"),
        legend.background = element_rect(colour = 'black', size = 0.5, linetype='solid'))
        # plot.margin =unit(c(1,1.5,1,1), "cm")) #+  #plot margin - top, right, bottom, left
jointRichnessPlot

##############################################################
# Plot 1C: COMMUNITY (POOLED JOURNALS) LEVEL DIVERSITY
##############################################################

plotPOOLEDsimpdiv<-ggplot(IsimpDivTable, aes(x=YEAR, y=InvSimpson)) +
  geom_line(size=1, color="blue") + # Use hollow circles
  #ylab("Geographic Diversity") +
  xlab("Year")+
  ylab(bquote('D'[2]))+
  ggtitle('(C) Editor Geographic Diversity')+
  geom_point(color="black", shape=1)+
  # scale_y_continuous(breaks=seq(2, 5.5, 0.5))+
  scale_y_continuous(limits = c(0, 20))+
  # scale_y_continuous(limits=c(1,max(IsimpDivTable$AnnualRichness)))+
  scale_x_continuous(breaks=seq(FirstYear, LastYear+1, 5))
plotPOOLEDsimpdiv<-plotPOOLEDsimpdiv+theme_classic()+
  theme(axis.title.x=element_text(colour="black", size = 14, vjust=0),            #sets x axis title size, style, distance from axis #add , face = "bold" if you want bold
        #axis.title.y=element_text(colour="black", size = 14, vjust=2),            #sets y axis title size, style, distance from axis #add , face = "bold" if you want bold
        #axis.title.y=element_blank(),
        axis.text=element_text(colour="black", size = 10),                              #sets size and style of labels on axes
        legend.title = element_blank(),   #Removes the Legend title
        legend.text = element_text(color="black", size=10),  
        legend.position = c(0.9,0.8),
        legend.background = element_rect(colour = 'black', size = 0.5, linetype='solid'))
        # plot.margin =unit(c(1,1.5,1,1), "cm")) #+  #plot margin - top, right, bottom, left
plotPOOLEDsimpdiv


######################################################
# Binding these up to make Fig. 1
######################################################
source("multiplot.R")
# if you jiust want to take a quick look at the results...
Fig1<-multiplot(plotTOTALEDSvYear, jointRichnessPlot, plotPOOLEDsimpdiv, cols=1)
# to save the figure in format for submission
# for an explanation of why you need to do multiplot INSIDe of ggsave see: http://stackoverflow.com/questions/11721401/r-save-multiplot-to-file
ggsave("Fig1.eps", plot = multiplot(plotTOTALEDSvYear,jointRichnessPlot, plotPOOLEDsimpdiv, cols=1), device = "eps", scale = 1, width = NA, height = NA, units = c("in", "cm", "mm"), dpi = 300, limitsize = TRUE)
ggsave("Fig1_size_test.eps", plot = multiplot(plotTOTALEDSvYear,jointRichnessPlot, plotPOOLEDsimpdiv, cols=1), device = "eps", scale = 1, width = 13.2, height = 19, units = "cm", dpi = 300, limitsize = TRUE)
ggsave("Fig1_size_test.tiff", plot = multiplot(plotTOTALEDSvYear,jointRichnessPlot, plotPOOLEDsimpdiv, cols=1), device = "tiff", width = 13.2, height = 19, units = "cm", dpi = 500, limitsize = TRUE)

tiff("Fig1_size_test.tiff", width = 13.2, height = 19, units = 'cm', res = 300, compression = 'lzw')
multiplot(plotTOTALEDSvYear, jointRichnessPlot, plotPOOLEDsimpdiv, cols=1)
dev.off()


##############################################################
# Fig 2A: bar chart of countries with the most unique editors  
##############################################################
cutoff = 9 # This is how many countries you want on the chart, all the rest will be in "OTHER"
editor.Geo<-arrange(Editor.Geo, desc(Pcnt_editors)) %>% select(geo.code,N_editors,Pcnt_editors)
most.common.editors<-slice(editor.Geo, 1:cutoff)
least.common.editors<-slice(editor.Geo, (cutoff+1):nrow(editor.Geo)) 
least.common.editors$geo.code<-"OTHER"
least.common.editors<-least.common.editors %>% 
  mutate(sum(N_editors)) %>%
  mutate(sum(Pcnt_editors)) %>% 
  select(-N_editors) %>% 
  select(-Pcnt_editors) %>% 
  rename(N_editors = `sum(N_editors)`) %>% 
  rename(Pcnt_editors = `sum(Pcnt_editors)`) %>% 
  slice(1:1)
most.common.editors<-bind_rows(most.common.editors, least.common.editors)
most.common.editors$geo.code<-as.factor(most.common.editors$geo.code)
most.common.editors

# This is needed to put them in order in the plot with OTHER at the end of the graph
order<-seq(1:nrow(most.common.editors))
most.common.editors$geo.code <- factor(most.common.editors$geo.code,most.common.editors$geo.code[levels = order])
# levels(most.common.editors$geo.code)
rm(order,editor.Geo,least.common.editors)

CountriesED<-arrange(most.common.editors) %>%  ggplot(aes(x=geo.code, y=Pcnt_editors)) +
  geom_bar(colour="black", stat="identity")+
  ylab("Percent") +
  xlab("Country")+
  ggtitle('(A) Editors by Country')+
  scale_y_continuous(breaks=seq(0, 70, 5))
CountriesED<-CountriesED+theme_classic()+
  theme(axis.title.x=element_text(colour="black", size = 14, vjust=0),            #sets x axis title size, style, distance from axis #add , face = "bold" if you want bold
        axis.title.y=element_text(colour="black", size = 14, vjust=2),            #sets y axis title size, style, distance from axis #add , face = "bold" if you want bold
        axis.text=element_text(colour="black", size = 10),                              #sets size and style of labels on axes
        legend.title = element_blank(),   #Removes the Legend title
        legend.text = element_text(color="black", size=10),  
        legend.position = c(0.9,0.8),
        # plot.margin=unit(c(1,1,1,1),"lines"),
        #aspect.ratio=1,
        legend.background = element_rect(colour = 'black', size = 0.5, linetype='solid'))
CountriesED


######################################################
# Fig 2B: Prop of the EDITOR POOL in EACH YEAR by REGION
# Note: thisis not "the average proportion of each editorial board". 
# This takes the list of people serving as editors in a year, makes sure that each
# person is listed only once (i.e., if an editor is on 2 boards in one year they are
# counted only once), and then calculates the proportion of that pool from each Region
######################################################


RegionFig<-ggplot(data=RegionPlot, aes(x=YEAR, y=Percent, group=REGION, colour=REGION)) +
  geom_line(size=1)+
  geom_point(mapping=aes(shape=REGION), size=2) +
  scale_shape_manual(values=c(0,15,16,17,18,1,2))+
  ylab("Percent") +
  xlab("Year")+
  ggtitle('(B) Editors by Global Region')+
  guides(col = guide_legend(nrow = 7))+
  scale_y_continuous(limit = c(0, 100))+
  scale_color_brewer(palette="Paired")+
  scale_x_continuous(breaks=seq(FirstYear ,LastYear+1, 5))
# RegionFig<-RegionFig+scale_fill_discrete(labels=c("North America","Europe &\nCentral Asia","East Asia & Pacific","Latin America & Caribbean","Sub-Saharan Africa","South Asia","Middle East & North Africa"))
#   
RegionFig<-RegionFig+theme_classic()+
  theme(axis.title.x=element_text(colour="black", size = 14, vjust=0),            #sets x axis title size, style, distance from axis #add , face = "bold" if you want bold
        axis.title.y=element_text(colour="black", size = 14, vjust=2),            #sets y axis title size, style, distance from axis #add , face = "bold" if you want bold
        axis.text=element_text(colour="black", size = 10),                              #sets size and style of labels on axes
        legend.title = element_blank(),   #Removes the Legend title
        legend.text = element_text(color="black", size=8, lineheight=2),
        legend.key.height=unit(0.3,"cm"),
        # legend.position = c(0.5,0.9), 
        legend.position = "right",
        # plot.margin=unit(c(1,1,1,1),"lines"),
        legend.background = element_rect(colour = 'black', size = 0.5, linetype='solid'))
#plot.margin =unit(c(0,1,0,1.5), "cm")) #+  #plot margin - top, right, bottom, left
RegionFig


######################################################
# Fig 2C: Prop of the EDITORS in EACH YEAR by COUNTRY INCOME
# Note: thisis not "the average proportion of each editorial board". 
# This takes the list of people serving as editors in a year, makes sure that each person
# is listed only once (i.e., if an editor is on 2 boards in one year they are counted only
# once), and then calculates the proportion of that pool from each country income category 
######################################################

IncomeFig<-ggplot(data=IncomePlot, aes(x=YEAR, y=Percent, group=INCOME_LEVEL, colour=INCOME_LEVEL)) +
  geom_line(size=1)+
  geom_point(mapping=aes(shape=INCOME_LEVEL), size=2) +
  scale_shape_manual(values=c(0,15,16,17,18))+
  ylab("Percent") +
  xlab("Year")+
  ggtitle('(C) Editors by Gross National Income Category')+
  guides(col = guide_legend(nrow = 5))+
  scale_color_brewer(palette="Paired")+
  scale_y_continuous(limit = c(0, 100))+
  # scale_y_continuous(breaks=seq(0, 100, 10))+
  scale_x_continuous(breaks=seq(FirstYear, LastYear+1, 5))
  
  # # geom_dl(aes(label = INCOME_LEVEL), method = "last.points", cex = 0.8)  #Add to last points
  # geom_dl(aes(label = INCOME_LEVEL), method = list(dl.trans(x = x + 0.2), "last.points", cex = 0.8))
    
IncomeFig<-IncomeFig+theme_classic()+
  theme(axis.title.x=element_text(colour="black", size = 14, vjust=0),            #sets x axis title size, style, distance from axis #add , face = "bold" if you want bold
        axis.title.y=element_text(colour="black", size = 14, vjust=2),            #sets y axis title size, style, distance from axis #add , face = "bold" if you want bold
        axis.text=element_text(colour="black", size = 10), 
        legend.title = element_blank(),   #Removes the Legend title
        legend.text = element_text(color="black", size=8, lineheight=2),
        legend.key.height=unit(0.3,"cm"),
        # legend.position = c(0.5,0.5), 
        legend.position = "right",
        # plot.margin=unit(c(1,1,1,1),"lines"),
        legend.background = element_rect(colour = 'black', size = 0.5, linetype='solid'))

#plot.margin =unit(c(0,1,0,1.5), "cm")) #+  #plot margin - top, right, bottom, left
IncomeFig



######################################################
# BINDING THESE UP TO MAKE FIGURE 2
######################################################
# uses source(muliplot.R) loaded at start of code
source("multiplot.R")
Fig2<-multiplot(CountriesED, RegionFig, IncomeFig, cols=1)
ggsave("Fig2.eps", plot = multiplot(CountriesED, RegionFig, IncomeFig, cols=1), device = "eps", scale = 1, width = NA, height = NA, units = c("in", "cm", "mm"), dpi = 400, limitsize = TRUE)
ggsave("Fig2_size_test.eps", plot = multiplot(CountriesED, RegionFig, IncomeFig, cols=1), device = "eps", scale = 1, width = 13.2, height = 19, units = "cm", dpi = 400, limitsize = TRUE)


tiff("Fig2_size_test.tiff", width = 13.2, height = 19, units = 'cm', res = 400, compression = 'lzw')
multiplot(CountriesED, RegionFig, IncomeFig, cols=1)
dev.off()


######################################################



######################################################
# Fig 3: Cumlative Editors vs Cumulative Authors
######################################################
AuthorCountries<-AuthorGeo_1985_2014[AuthorGeo_1985_2014$YEAR>=FirstYear & AuthorGeo_1985_2014$YEAR<=LastYear,]
AuthorCountries$COUNTRY<-as.factor(AuthorCountries$COUNTRY)

AuPerCountryPerYr.LONG<-AuthorCountries %>% group_by(YEAR, geo.code) %>% summarize(Total = n_distinct(geo.code))
# AuPerCountryPerYr.LONG[is.na(AuPerCountryPerYr.LONG)] <- 0
AuCumulative<-AuthorCountries %>% ungroup() %>%  select(-Pcnt_Pubs, COUNTRY) %>% group_by(YEAR,geo.code) %>% summarize(yr_tot=sum(N_Articles))
# AuCumulativem$YEAR<-as.numeric(AuCumulativem$YEAR)
AuCumulative<-spread(AuCumulative, geo.code,yr_tot)
AuCumulative[is.na(AuCumulative)] <- 0
AuCumulative<-as_tibble(AuCumulative)
AuCumulativePlot<-specaccum(AuCumulative, "collector")

AuCumulativePlot<-as.data.frame(AuCumulativePlot$richness)
AuCumulativePlot$richness<-as.vector(AuCumulativePlot$richness)
names(AuCumulativePlot)[1] <- "CumulativeRichness"
AuCumulativePlot$YEAR<-seq(1985,2014,1)

EDvAuCumRich<-full_join(AuCumulativePlot, editorAcum, by = "YEAR")
EDvAuCumRich <- EDvAuCumRich %>% select(YEAR, CumulativeRichness.x, CumulativeRichness.y) #reorder columns
EDvAuCumRich<-gather(EDvAuCumRich, "CumulativeRichness.x","CumulativeRichness.x", 2:3) 
EDvAuCumRich[EDvAuCumRich=="CumulativeRichness.x"]<-"Authors"
EDvAuCumRich[EDvAuCumRich=="CumulativeRichness.y"]<-"Editors"
names(EDvAuCumRich)[2] <- "Category"
names(EDvAuCumRich)[3] <- "N"
# rm(AuPerCountryPerYr.LONG,AuthorCountries,AuCumulative,AuCumulativePlot)


#plot cumulative and annual richness same plot
EDvAuCumRichPlot<-ggplot(EDvAuCumRich, aes(x=YEAR, y=N, group = Category, colour = Category)) +
  geom_line(size=1) +
  scale_color_manual(values=c("blue", "red"))+
  geom_text(data = EDvAuCumRich[EDvAuCumRich$YEAR=="2012" & EDvAuCumRich$Category=="Editors",], aes(label = Category), hjust = 1, vjust = -1, size=5) +
  geom_text(data = EDvAuCumRich[EDvAuCumRich$YEAR=="2012" & EDvAuCumRich$Category=="Authors",], aes(label = Category), hjust = 1, vjust = -1, size=5) +
  ylab("Cumulative No. of Countries") +
  xlab("Year")+
  # ggtitle('Fig 3. Cumulative Geographic Richness from 1985-2014 of the editors for N = 24 environmental biology journals \n            and of the authors publishing in those journals during the same time perior (N = 113,816 articles).
  #         ')+
  geom_point(color="black", shape=1)+
  #scale_y_continuous(breaks = seq(20, 220,20))+
  scale_y_continuous(limits=c(1,max(EDvAuCumRich$N)+20))+
  scale_x_continuous(breaks=seq(FirstYear, LastYear+1, 5))

EDvAuCumRichPlot<-EDvAuCumRichPlot+theme_classic()+
  theme(axis.title.x=element_text(colour="black", size = 14, vjust=0),            #sets x axis title size, style, distance from axis #add , face = "bold" if you want bold
        axis.title.y=element_text(colour="black", size = 14, vjust=2),            #sets y axis title size, style, distance from axis #add , face = "bold" if you want bold
        axis.text=element_text(colour="black", size = 12),                              #sets size and style of labels on axes
        legend.title = element_blank(),   #Removes the Legend title
        legend.text = element_text(color="black", size=10),  
        # legend.position = c(0.9,0.8),
        legend.position = ("none"),
        legend.background = element_rect(colour = 'black', size = 0.5, linetype='solid'),
        plot.margin =unit(c(1,1.5,1,1), "cm")) #+  #plot margin - top, right, bottom, left
EDvAuCumRichPlot

ggsave("Fig3.eps", plot = EDvAuCumRichPlot, device = "eps", scale = 1, width = NA, height = NA, units = c("in", "cm", "mm"), dpi = 400, limitsize = TRUE)
ggsave("Fig3_size_test.eps", plot = EDvAuCumRichPlot, device = "eps", scale = 1, width = 13.2, height = 13.2, units = "cm", dpi = 400, limitsize = TRUE)
ggsave("Fig3_size_test_tiff", plot = EDvAuCumRichPlot, units="cm", scale = 1, width=13.2, height=13.2, dpi=400, device = "tiff")

tiff("Fig3_size_test.tiff", width = 13.2, height = 13.2, units = 'cm', res = 400, compression = 'lzw')
EDvAuCumRichPlot
dev.off()
######################################################
######################################################
#
# SUPPLEMENT: ANALYSES, TABLES, FIGURES: 
#
######################################################
######################################################

######################################################
#Table S1
######################################################

EdsFirstYr<-AnalysisData %>% 
  filter(YEAR == FirstYear) %>%  
  rbind(AnalysisData %>% filter(YEAR == 1987) %>% filter(JOURNAL%in% c("Conservation Biology" , "Functional Ecology", "Landscape Ecology"))) %>% 
  group_by(JOURNAL) %>%   #collect the data into groups by country
  summarize(EditorsFirstYr = n_distinct(editor_id)) %>% 
  arrange(JOURNAL)

CountriesFirstYr<-AnalysisData %>% 
  filter(YEAR == FirstYear) %>%  
  rbind(AnalysisData %>% filter(YEAR == 1987) %>% filter(JOURNAL%in% c("Conservation Biology" , "Functional Ecology", "Landscape Ecology"))) %>% 
  group_by(JOURNAL) %>%   #collect the data into groups by country
  summarize(CountriesFirstYr = n_distinct(geo.code)) %>% 
  arrange(JOURNAL)

EdsLastYr<-AnalysisData %>% 
  filter(YEAR == LastYear) %>% 
  group_by(JOURNAL) %>%   #collect the data into groups by country
  summarize(EditorsLastYr = n_distinct(editor_id))%>% 
  arrange(JOURNAL)

CountriesLastYr<-AnalysisData %>% 
  filter(YEAR == LastYear) %>%  
  group_by(JOURNAL) %>%   #collect the data into groups by country
  summarize(CountriesLastYr = n_distinct(geo.code)) %>% 
  arrange(JOURNAL)

EdsTotal<-AnalysisData %>% 
  group_by(JOURNAL) %>%   #collect the data into groups by country
  summarize(TotalEditors = n_distinct(editor_id)) %>% 
  arrange(JOURNAL)

CountriesTotal<-AnalysisData %>% 
  group_by(JOURNAL) %>%   #collect the data into groups by country
  summarize(TotalCountries = n_distinct(geo.code)) %>% 
  arrange(JOURNAL)

TABLE1<-full_join(EdsFirstYr,CountriesFirstYr, by = "JOURNAL")
TABLE1<-full_join(TABLE1,EdsLastYr, by = "JOURNAL")  
TABLE1<-full_join(TABLE1,CountriesLastYr, by = "JOURNAL") 
TABLE1<-full_join(TABLE1,EdsTotal, by = "JOURNAL") 
TABLE1<-full_join(TABLE1,CountriesTotal, by = "JOURNAL")
TABLE1<-mutate(TABLE1, CEratio=TotalEditors/TotalCountries)
TABLE1$Pcnt<-round(TABLE1$CEratio, digits=2)
TABLE1
rm(EdsFirstYr,CountriesFirstYr,EdsLastYr,CountriesLastYr,EdsTotal,CountriesTotal)
write.csv(TABLE1, file="TableS1.csv", row.names = F) #export it as a csv file

######################################################
#chi-sq test: are there differences in frequency by region and income level?

Chi.region<-AnalysisData %>% group_by(REGION)  %>%  summarize(Editors=n_distinct(editor_id)) %>% mutate(Pcnt=Editors/sum(Editors)*100)  
chisq.test(Chi.region$Editors)

Chi.income<-AnalysisData %>% group_by(INCOME_LEVEL)  %>%  summarize(Editors=n_distinct(editor_id))%>% mutate(Pcnt=Editors/sum(Editors)*100)
chisq.test(Chi.income$Editors)



######################################################
######################################################
# GLS with Temp Autocorrleation
######################################################
######################################################
#Bind up the data for the analyses
GLS.data<-full_join(IsimpDivTable, EdsPerYr,by="YEAR")
GLS.data<-rename(GLS.data, Countries=AnnualRichness, Editors=TotalEditors) #Shorter
# Preliminary: is there evidence for Autocorrelation 
# GR: Yes
acf(GLS.data$Countries,lag.max=10) 
plot(acf(GLS.data$Countries,type="p")) #partial autocorrelation

# GD YES
acf(GLS.data$InvSimpson,lag.max=10) 
plot(acf(GLS.data$InvSimpson,type="p"))

# GE YES
acf(GLS.data$Geo.Evenness,lag.max=10) 
plot(acf(GLS.data$Geo.Evenness,type="p"))

# RESPONSE<-"Countries"
# RESPONSE<-"InvSimpson"
# RESPONSE<-"Geo.Evenness"

# library(nlme)
# https://stats.stackexchange.com/questions/13859/finding-overall-p-value-for-gls-model
# REML to test the utility of corARMA term
# TESTING THE EFFECT OF THE CORRELATION STRUCTURE
mAC1.1 <- gls(Countries ~ 1, data = GLS.data,  na.action = na.omit) 
mAC1.2 <- gls(Countries ~ 1, data = GLS.data, correlation = corARMA(p = 1), na.action = na.omit)
mAC2.1 <- gls(Countries ~ Editors, data = GLS.data,  na.action = na.omit) 
mAC2.2 <- gls(Countries ~ Editors, data = GLS.data, correlation = corARMA(p = 1), na.action = na.omit)
mAC3.1 <- gls(Countries ~ YEAR, data = GLS.data,  na.action = na.omit)
mAC3.2 <- gls(Countries ~ YEAR, data = GLS.data, correlation = corARMA(p = 1), na.action = na.omit)
mAC4.1 <- gls(Countries ~ Editors+YEAR, data = GLS.data, na.action = na.omit)
mAC4.2 <- gls(Countries ~ Editors+YEAR, data = GLS.data, correlation = corARMA(p = 1), na.action = na.omit)
mAC5.1 <- gls(Countries ~ Editors*YEAR, data = GLS.data, na.action = na.omit)
mAC5.2 <- gls(Countries ~ Editors*YEAR, data = GLS.data, correlation = corARMA(p = 1), na.action = na.omit)
# 
summary(mAC1.2)
model.sel(mAC1.1,mAC1.2,mAC2.1,mAC2.2,mAC3.1,mAC3.2,mAC4.1,mAC4.2,mAC5.1,mAC5.2)


# ML to test main effects
# Countries
# InvSimpson

m1.MAIN <- gls(InvSimpson ~ 1, data = GLS.data, correlation = corARMA(p = 1),na.action = na.omit,method = "ML")
m2.MAIN <- gls(InvSimpson ~ YEAR, data = GLS.data, correlation = corARMA(p = 1), na.action = na.omit,method = "ML")
m3.MAIN <- gls(InvSimpson ~ Editors,  data = GLS.data,correlation = corARMA(p = 1),na.action = na.omit,method = "ML")
m4.MAIN <- gls(InvSimpson ~ YEAR + Editors, data = GLS.data, correlation = corARMA(p = 1), na.action = na.omit,method = "ML")
m5.MAIN <- gls(InvSimpson ~ Editors*YEAR, data = GLS.data, correlation = corARMA(p = 1), na.action = na.omit,method = "ML")

summary(m1.MAIN)
summary(m2.MAIN)
summary(m3.MAIN)
summary(m4.MAIN)
summary(m5.MAIN)
# https://stats.stackexchange.com/questions/13859/finding-overall-p-value-for-gls-model
anova(m1.MAIN,m2.MAIN)
anova(m1.MAIN, m3.MAIN)
anova(m2.MAIN,m4.MAIN)
anova(m3.MAIN,m4.MAIN)
anova(m3.MAIN,m5.MAIN)
anova(m4.MAIN,m5.MAIN)
# library(MuMIn)
model.sel(m1.MAIN,m2.MAIN,m3.MAIN,m4.MAIN,m5.MAIN)



######################################################
# S1 Text Fig 1: Countries in a Year vs. No of Editors in a Year (all journals pooled) 
######################################################

TotalEdsVGeo<-full_join(EdsPerYr,GEOperYR, by="YEAR")
plotTOTALedsVgeo<-ggplot(TotalEdsVGeo, aes(x=TotalEditors, y=AnnualRichness)) +
  # ggtitle('(D) Geographic Richness')+
  ylab("Editor Geographic Richness") +
  xlab("Total No. of Editors")+
  # geom_line(size=1, color="blue")+
  geom_point(color="black", shape=1)+
  geom_smooth(method='lm', se=FALSE)+
  scale_y_continuous(limit = c(30, 55))
  # scale_y_continuous(breaks=seq(30, 60, 5))
plotTOTALedsVgeo<-plotTOTALedsVgeo+theme_classic()+
  theme(axis.title.x=element_text(colour="black", size = 14, vjust=0),            #sets x axis title size, style, distance from axis #add , face = "bold" if you want bold
        axis.title.y=element_text(colour="black", size = 14, vjust=2),            #sets y axis title size, style, distance from axis #add , face = "bold" if you want bold
        axis.text=element_text(colour="black", size = 10),                              #sets size and style of labels on axes
        legend.title = element_blank(),   #Removes the Legend title
        legend.text = element_text(color="black", size=12),  
        legend.position = c(0.9,0.8),
        legend.background = element_rect(colour = 'black', size = 0.5, linetype='solid'))
#plot.margin =unit(c(0,1,0,1.5), "cm")) #+  #plot margin - top, right, bottom, left
plotTOTALedsVgeo

# to save the figure in format for submission
ggsave("S1_Text_FigA.eps", plot = plotTOTALedsVgeo, device = "eps", scale = 1, width = 5, height = 5, units = "in", dpi = 300, limitsize = TRUE)

ggsave("S1_Text_FigA_size_test.eps", plot = plotTOTALedsVgeo, device = "eps", scale = 1, width = 13.2, height = 13.2, units = "cm", dpi = 600, limitsize = TRUE)

tiff("S1_Text_FigA_size_test.tiff", width = 13.2, height = 13.2, units = 'cm', res = 400, compression = 'lzw')
plotTOTALedsVgeo
dev.off()

######################################################
# S1 Text Fig B: Zoom on editors per region and income <25%
######################################################

ZOOM=1984

##############################################################
# Number / Percentage of Editors from Different Income Levels, all journals pooled 
# USed for Fig. SB
IncomePlotZoom<-AnalysisData %>% select(YEAR,editor_id,INCOME_LEVEL,CATEGORY) %>% group_by(editor_id) 
IncomePlotZoom %>% filter(YEAR>=ZOOM)
IncomePlotZoom<-distinct(IncomePlotZoom, editor_id,YEAR, .keep_all = TRUE)
IncomePlotZoom<-IncomePlotZoom %>% group_by(YEAR,INCOME_LEVEL) %>% count(YEAR,INCOME_LEVEL)
IncomePlotZoom<-IncomePlotZoom %>% group_by(YEAR) %>% mutate(yr_tot=sum(n)) %>% mutate(Percent=n/yr_tot*100) 
# IncomePlotZoom<-IncomePlotZoom %>% filter (INCOME_LEVEL %in% c("High income: nonOECD","Upper middle income","Lower middle income","Low income"))
# IncomePlotZoom<-droplevels(IncomePlotZoom)
# levels(IncomePlotZoom$INCOME_LEVEL)
# summary(IncomePlotZoom)
#IncomePlot %>%  group_by(YEAR) %>% mutate(sum=sum(Percent)) checks that add up to 100%

#PLOT: INCOME
IncomeFigZoom<-ggplot(data=IncomePlotZoom, aes(x=YEAR, y=Percent, group=INCOME_LEVEL, colour=INCOME_LEVEL)) +
  geom_line(size=1)+
  geom_point(mapping=aes(shape=INCOME_LEVEL), size=2) +
  scale_shape_manual(values=c(0,15,16,17,18))+
  ylab("Percent") +
  xlab("Year")+
  ggtitle('(B) Editors by Gross National Income Category')+
  scale_color_brewer(palette="Paired")+
  scale_y_continuous(limit = c(0, 15))+
  # annotate("text", x=1986, y=20, label= "B", size = 6) +
  # scale_colour_discrete(drop=TRUE,limits = levels(IncomePlotZoom$INCOME_LEVEL))+
  # scale_y_continuous(breaks=seq(0, 100, 10))+
  scale_x_continuous(breaks=seq(ZOOM, LastYear+1, 5))
IncomeFigZoom<-IncomeFigZoom+theme_classic()+
  theme(axis.title.x=element_text(colour="black", size = 14, vjust=0),            #sets x axis title size, style, distance from axis #add , face = "bold" if you want bold
        axis.title.y=element_text(colour="black", size = 14, vjust=2),            #sets y axis title size, style, distance from axis #add , face = "bold" if you want bold
        axis.text=element_text(colour="black", size = 12), 
        legend.title = element_blank(),   #Removes the Legend title
        legend.text = element_text(color="black", size=10, lineheight=2.5),
        legend.key.height=unit(0.5,"cm"),
        # legend.position = c(0.9,0.8),
        legend.position = "right",
        plot.margin=unit(c(1,1,2,1),"lines"),
        legend.background = element_rect(colour = 'black', size = 0.5, linetype='solid'))
#plot.margin =unit(c(0,1,0,1.5), "cm")) #+  #plot margin - top, right, bottom, left
IncomeFigZoom
##############################################################

##############################################################
# Number / Percentage of Editors from Different Regions (all journals pooled)
# Used for Fig. Sb
RegionPlotZoom<-AnalysisData %>% select(YEAR,editor_id,REGION,CATEGORY) %>% group_by(editor_id)
RegionPlotZoom %>% filter(YEAR>=ZOOM) 
RegionPlotZoom<-distinct(RegionPlotZoom, editor_id,YEAR, .keep_all = TRUE)
RegionPlotZoom<-RegionPlotZoom %>% group_by(YEAR,REGION) %>% count(YEAR,REGION)
RegionPlotZoom<-RegionPlotZoom %>% group_by(YEAR) %>% mutate(yr_tot=sum(n)) %>% mutate(Percent=n/yr_tot*100) 
# RegionPlotZoom<-RegionPlotZoom%>% filter (REGION %in% c("East Asia & Pacific", "Latin America & Caribbean", "Sub-Saharan Africa", "South Asia", "Middle East & North Africa"))
# RegionPlotZoom<-droplevels(RegionPlotZoom)
levels(RegionPlotZoom$REGION)
#RegionPlot %>%  group_by(YEAR) %>% mutate(sum=sum(Percent)) checks that add up to 100%


# PLOT: REGION
RegionFigZoom<-ggplot(data=RegionPlotZoom, aes(x=YEAR, y=Percent, group=REGION, colour=REGION)) +
  geom_line(size=1)+
  geom_point(mapping=aes(shape=REGION), size=2) +
  scale_shape_manual(values=c(0,15,16,17,18,1,2))+
  ylab("Percent") +
  xlab("Year")+
  ggtitle('(A) Editors by Global Region')+
  scale_y_continuous(limit = c(0, 15))+
  # annotate("text", x=1986, y=20, label= "(A) Editors by Global Region", size = 6) +
  scale_colour_discrete(drop=TRUE,limits = levels(AnalysisData$REGION))+ #THIS maintains the color scheme order based on the original figure see https://stackoverflow.com/questions/6919025/how-to-assign-colors-to-categorical-variables-in-ggplot2-that-have-stable-mappin
  scale_color_brewer(palette="Paired")+
  scale_x_continuous(breaks=seq(ZOOM, LastYear+1, 5))
RegionFigZoom<-RegionFigZoom+theme_classic()+
  theme(axis.title.x=element_text(colour="black", size = 14, vjust=0),            #sets x axis title size, style, distance from axis #add , face = "bold" if you want bold
        axis.title.y=element_text(colour="black", size = 14, vjust=2),            #sets y axis title size, style, distance from axis #add , face = "bold" if you want bold
        axis.text=element_text(colour="black", size = 12),                              #sets size and style of labels on axes
        legend.title = element_blank(),   #Removes the Legend title
        legend.text = element_text(color="black", size=10, lineheight=2.5),
        legend.key.height=unit(0.5,"cm"),
        # legend.position = c(0.7,0.8),
        legend.position = "right",
        plot.margin=unit(c(1,1,2,1),"lines"),
        legend.background = element_rect(colour = 'black', size = 0.5, linetype='solid'))
#plot.margin =unit(c(0,1,0,1.5), "cm")) #+  #plot margin - top, right, bottom, left
RegionFigZoom
##############################################################


######################################################
# Binding these up to make Supplement Figure B
######################################################
source("multiplot.R")
# if you jiust want to take a quick look at the results...
FigSC<-multiplot(RegionFigZoom, IncomeFigZoom, cols=1)
# to save the figure in format for submission
# for an explanation of why you need to do multiplot INSIDe of ggsave see: http://stackoverflow.com/questions/11721401/r-save-multiplot-to-file
ggsave("S1_Text_FigB.eps", plot = multiplot(RegionFigZoom, IncomeFigZoom, cols=1), device = "eps", scale = .7, width = NA, height = NA, units = c("in", "cm", "mm"), dpi = 600, limitsize = TRUE)
# ggsave("S1_Text_FigB_size_test.eps", plot = multiplot(RegionFigZoom, IncomeFigZoom, cols=1), device = "eps", scale = 1, width = 13.2, height = 19, units = "cm", dpi = 600, limitsize = TRUE)
# 
# tiff("S1_Text_FigB_size_test.tiff", width = 13.2, height = 19, units = 'cm', res = 600, compression = 'lzw')
# multiplot(RegionFigZoom, IncomeFigZoom, cols=1)
# dev.off()


######################################################
# Fig S1: Countries: Changes in Percentage of editors over time
######################################################

rm(CountryCurves,foo,all.geo)
all.geo<-as_tibble(as.factor(levels(AnalysisData$geo.code)))
all.geo<-factor(levels(AnalysisData$geo.code))
all.geo<-as_tibble(rep(all.geo, each =30))
colnames(all.geo)[1]<-"geo.code"
all.geo$YEAR<-rep(seq(1985,2014,by=1), times =71)
all.geo$COUNTRY<-all.geo$geo.code
# all.geo$zero<-NA
source("AddIncomeRegion.R")
all.geo<-AddIncomeRegion(all.geo)

CountryCurves<-AnalysisData %>% group_by(YEAR, geo.code, REGION, INCOME_LEVEL) %>% summarize(n = n_distinct(editor_id))
str(CountryCurves)
CountryCurves<-full_join(CountryCurves,all.geo, by=c("YEAR","geo.code", "INCOME_LEVEL","REGION"))
# CountryCurves$n[is.na(CountryCurves$n)] <- 0
CountryCurves<-full_join(CountryCurves,EdsPerYr, by="YEAR")
# CountryCurves<-CountryCurves %>% mutate(Percent=n/TotalEditors*100)
# CountryCurves$Percent<-round(CountryCurves$Percent, digits=2)
Percent1985<-CountryCurves %>% ungroup(CountryCurves) %>%  filter(YEAR==1985) %>% mutate(Percent1985=n/TotalEditors*100) %>% select(Percent1985,geo.code)
Percent1985$Percent1985<-round(Percent1985$Percent1985, digits=2)
# Percent1985<-Percent1985 %>% select(-YEAR)
sum(Percent1985$Percent1985)
Percent2014<-CountryCurves %>% ungroup(CountryCurves) %>%  filter(YEAR==2014) %>% mutate(Percent2014=n/TotalEditors*100) %>% select(Percent2014,geo.code)
Percent2014$Percent2014<-round(Percent2014$Percent2014, digits=2)

# Percent2014<-CountryCurves %>% select(YEAR, Percent,geo.code) %>% filter(YEAR==2014) %>% group_by(geo.code) %>% mutate(Percent2014=sum(Percent)) %>% select(-Percent)
# Percent2014<-Percent2014 %>% select(-YEAR)

CountryCurves<-full_join(Percent1985,Percent2014,by="geo.code")
CountryCurves<-na.omit(CountryCurves)# CountryCurves$INCOME_LEVEL<-as.factor(CountryCurves$INCOME_LEVEL)
# CountryCurves$REGION<-as.factor(CountryCurves$REGION)

# CountryCurves<-full_join(CountryCurves,Percent2014, by="geo.code")
CountryCurves<-as.data.frame(CountryCurves)
str(CountryCurves)
CountryCurves<-CountryCurves%>% mutate(PercentChange85_14=(Percent2014-Percent1985))
# foo<-CountryCurves %>% filter(YEAR==FirstYear | YEAR==LastYear+1) 
# foo<-foo %>% filter(geo.code!="GBR" & geo.code!="USA")
# foo<-foo %>% filter(REGION=="North America" | REGION=="Europe & Central Asia"| REGION=="East Asia & Pacific")
sum(CountryCurves$Percent1985)
sum(CountryCurves$Percent2014)

# baseline<-CountryCurves %>% select(geo.code,Percent) %>% filter(YEAR==1985)
# CountryCurves<-full_join(CountryCurves,baseline, by="geo.code")
# CountryCurves$diff.percent<-CountryCurves$Percent.x-CountryCurves$Percent.y
levels(CountryCurves$geo.code)
# CountryCurves2<-CountryCurves %>% filter(REGION== "East Asia & Pacific")
plotdata<-CountryCurves %>% filter(PercentChange85_14 <= -1 | PercentChange85_14 >=1)   %>% arrange(PercentChange85_14)
# This is needed to put them in order in the plot with OTHER at the end of the graph

order2<-seq(1:nrow(plotdata))
plotdata$geo.code <- factor(plotdata$geo.code,plotdata$geo.code[levels = order2])
rm(order2)
# levels(plotdata$geo.code)
values = c("GBR" = "darkred","NOR" = "darkred", "CAN" = "navyblue", "USA" = "navyblue","CHN" = "navyblue","NLD" = "navyblue","FRA" = "navyblue","AUS" = "navyblue")
values = c("GBR" = "darkred","NOR" = "darkred", "CAN" = "darkred", "USA" = "darkred","CHN" = "navyblue","NLD" = "darkred","FRA" = "darkred","AUS" = "navyblue")

PercChangePlot<-arrange(plotdata) %>%  ggplot(aes(x=geo.code, y=PercentChange85_14)) +
  geom_bar(colour="black",fill=values, stat="identity")+
  ylab("Percent Change") +
  xlab("Country")+
  scale_color_brewer(palette="Paired")+
scale_y_continuous(breaks = seq(-10, 10, by=1))+
geom_hline(yintercept=0)
PercChangePlot<-PercChangePlot+theme_classic()+
  theme(axis.title.x=element_text(colour="black", size = 14, vjust=0),            #sets x axis title size, style, distance from axis #add , face = "bold" if you want bold
        axis.title.y=element_text(colour="black", size = 14, vjust=2),            #sets y axis title size, style, distance from axis #add , face = "bold" if you want bold
        axis.text=element_text(colour="black", size = 10),                              #sets size and style of labels on axes
        axis.text.x=element_text(angle=60, hjust=1),
        legend.title = element_blank(),   #Removes the Legend title
        legend.text = element_text(color="black", size=10),  
        legend.position = c(0.9,0.8),
        plot.margin=unit(c(1,1,2,1),"lines"),
        #aspect.ratio=1,
        legend.background = element_rect(colour = 'black', size = 0.5, linetype='solid'))
PercChangePlot

ggsave("S1_Fig.eps", plot =PercChangePlot , device = "eps", scale = 1, width = 12, height = 10, units = "cm", dpi = 400, limitsize = TRUE)
# ggsave("S1_Fig_size_test.eps", plot =PercChangePlot , device = "eps", scale = 1, width = 13.2, height = 19, units = "cm", dpi = 400, limitsize = TRUE)
# # 
# tiff("S1_Fig_size_test.tiff", width = 13.2, height = 19, units = 'cm', res = 400, compression = 'lzw')
# PercChangePlot
# dev.off()

######################################################
# S2 Fig: 2014 Authors v. Editors
######################################################


##############################################################
##############################################################
#
# DATA UPLOAD & STANDARDIZATION
#
##############################################################
##############################################################

GEO<-read.csv("./Data/Editor.Author.Geo_2014.csv", dec=".", header = TRUE, sep = ",", check.names=FALSE )
INCOME<-read.csv("./Data/Editor_Author_Income_2014.csv", dec=".", header = TRUE, sep = ",", check.names=FALSE )
REGION<-read.csv("./Data/Editor_Author_Region_2014.csv", dec=".", header = TRUE, sep = ",", check.names=FALSE )

############
# CHI SQ TEST REGION ED v AU
Region.tbl<- REGION %>% select(CATEGORY, REGION, n) 
Region.tbl<-spread(Region.tbl,CATEGORY, n)
str(Region.tbl)
A<-Region.tbl$Author
B<-Region.tbl$Editor
C<-cbind(A,B)
Region.tbl<-as.table(C)
chisq.test(Region.tbl) 
############

############
# CHI SQ TEST INCOME ED v AU
Income.tbl<- INCOME %>% select(CATEGORY, INCOME_LEVEL, n) 
Income.tbl<-spread(Income.tbl,CATEGORY, n)
str(Income.tbl)
A<-Income.tbl$Author
B<-Income.tbl$Editor
C<-cbind(A,B)
Income.tbl<-as.table(C)
chisq.test(Income.tbl) 
############

##############################################################
# bar chart of top autor/editor countries  
##############################################################
Author.Geo<-GEO %>% group_by(CATEGORY) %>% mutate(GeoRank = dense_rank(-N))
source("AddIncomeRegion.R")
Author.Geo$COUNTRY<-Author.Geo$geo.code
Author.Geo<-AddIncomeRegion(Author.Geo)
cutoff = 15 # This is how many countries you want on the chart, all the rest will be in "OTHER"
editors<-Author.Geo %>% filter(CATEGORY=="Editors") %>% select (CATEGORY,geo.code,N,Pcnt,GeoRank, INCOME_LEVEL, REGION)
authors<-Author.Geo %>% filter(CATEGORY=="Authors") %>% select (CATEGORY,geo.code,N,Pcnt,GeoRank, INCOME_LEVEL, REGION)

##Countries in author list not in editor list
AUbutnotEd<-anti_join(authors,editors,by="geo.code")
authors_sub<-slice(authors, 1:cutoff)
editors_sub <-semi_join(editors,authors_sub,by="geo.code")
ae_sub<-rbind(editors_sub,authors_sub)
ae_sub<-ae_sub %>% arrange(CATEGORY)

foo<-authors %>% group_by(INCOME_LEVEL) %>% summarise(sum(N))
foo2<-editors %>% group_by(INCOME_LEVEL) %>% summarise(sum(N))

foo3<-authors %>% group_by(REGION) %>% summarise(sum(N))
foo4<-editors %>% group_by(REGION) %>% summarise(sum(N))

ae_sub$order<-1:nrow(ae_sub)

# Add the geographic region and national income category in whihc editors are based
sum(AUbutnotEd$N)

foo<-AUbutnotEd %>% group_by(INCOME_LEVEL) %>% summarise(sum(N)/105*100)

CountriesED<-ae_sub %>% ggplot(aes(x=reorder(geo.code,order), y=Pcnt, fill=CATEGORY)) +
  geom_bar(colour="black", stat="identity",position="dodge")+
  geom_text(aes(label=GeoRank), position=position_dodge(width=0.9), vjust=-0.25, size=6)+
  scale_fill_manual(values=c("darkblue", "darkred"))+
  ylab("Percent") +
  xlab("Country")+
  # ggtitle('Editors and Authors by Country (2014)')+
  scale_y_continuous(breaks=seq(0, 55, 5))
CountriesED<-CountriesED+theme_classic()+
  theme(axis.title.x=element_text(colour="black", size = 20, vjust=0),            #sets x axis title size, style, distance from axis #add , face = "bold" if you want bold
        axis.title.y=element_text(colour="black", size = 20, vjust=0),            #sets y axis title size, style, distance from axis #add , face = "bold" if you want bold
        axis.text=element_text(colour="black", size = 16),                              #sets size and style of labels on axes
        legend.title = element_blank(),   #Removes the Legend title
        legend.text = element_text(color="black", size=18),  
        legend.position = c(0.85,0.5),
        #legend.position = "right",
        plot.margin=unit(c(1,1,1,1),"lines"),
        #aspect.ratio=1,
        legend.background = element_rect(colour = 'black', size = 0.5, linetype='solid'))
CountriesED
ggsave("S2_Fig_new.eps", plot = CountriesED, scale = 1, width = 18.05, height = 12,dpi = 400, limitsize = TRUE)
# ggsave("S2_Fig_new_size_test.eps", plot = CountriesED, scale = 1, width = 7, height = 4,units = "in",dpi = 1000, limitsize = TRUE)
# tiff("S2_Fig_new_size_test.tiff", width = 18.05, height = 12, units = 'cm', res = 400, compression = 'lzw')
# CountriesED
# dev.off()




# setEPS()
# postscript("S3_Fig.eps",width=19, height=22)
######################################################
# S3 Fig: Geo Richness of EDITORS EACH YEAR BY JOURNAL 
######################################################

# No. of editors on each journal's board in each year
EdsJrnlYr<-AnalysisData %>% group_by(JOURNAL, YEAR) %>% summarize(TotalEditors = n_distinct(editor_id))
# No. of countries on each journal's board in each year
CountriesJrnlYr<-AnalysisData %>% group_by(JOURNAL, YEAR) %>% summarize(TotalCountries = n_distinct(geo.code))
# bound into a single dataframe
JrnlData<-full_join(EdsJrnlYr,CountriesJrnlYr, by=c("JOURNAL", "YEAR"))
rm(EdsJrnlYr,CountriesJrnlYr)
JrnlData<-JrnlData %>% select(JOURNAL, YEAR,TotalCountries)


JrnlRichnessFig<-ggplot(data=JrnlData, aes(x=YEAR, y=TotalCountries)) +
  geom_line(size=1, color="blue")+
  #ggtitle('Appendix A: Geographic Richness of environmental biology journals')+
  facet_wrap(~JOURNAL, nrow=4, scales="fixed")+
  ylab("Geographic Richness") +
  xlab("Year")+
  scale_y_continuous(breaks=seq(0, 30, 5))+
  scale_color_brewer(palette="Dark2")+
  scale_x_continuous(breaks=seq(FirstYear, LastYear+1, 10))
JrnlRichnessFig<-JrnlRichnessFig+theme_bw()+
  theme(axis.title.x=element_text(colour="black", size = 10, vjust=0),            #sets x axis title size, style, distance from axis #add , face = "bold" if you want bold
        axis.title.y=element_text(colour="black", size = 10, vjust=0),            #sets y axis title size, style, distance from axis #add , face = "bold" if you want bold
        axis.text.y=element_text(colour="black", size = 8),
        axis.text.x=element_text(colour="black", size = 8,angle=45, hjust=1),
        panel.grid.minor = element_blank(),
        strip.background = element_blank(),
        panel.spacing.x = unit(0.75, "lines"),
        panel.spacing.y = unit(0.1, "lines"),
        # strip.background = element_rect(colour="black", fill="white"),
        panel.border = element_rect(colour = "black"),
        # legend.background = element_rect(colour = 'black', size = 0.5, linetype='solid'),
        legend.position="none",
        strip.text = element_text(face = "italic", size = 6))
JrnlRichnessFig
#plot.margin =unit(c(0,1,0,1.5), "cm")) #+  #plot margin - top, right, bottom, left
# uses source(muliplot.R) loaded at start of code
ggsave("S3_Fig.eps", JrnlRichnessFig, device = "eps", scale = 1, width = 19, height = 13.2, units = "cm", dpi = 400, limitsize = TRUE)
# ggsave("S3_Fig_size_test.eps", JrnlRichnessFig, device = "eps", scale = 1, width = 19, height = 13.2, units = "cm", dpi = 400, limitsize = TRUE)
# tiff("S3_Fig_size_test.tiff",width = 19, height = 13.2, units = 'cm', res = 400, compression = 'lzw')
# JrnlRichnessFig
# dev.off()



######################################################
# S4 Fig: Geo Diversity of EDITORS EACH YEAR split by JOURNAL
######################################################
EdsPerCountryPerJrnlPerYr.LONG<-AnalysisData %>% group_by(JOURNAL, YEAR, geo.code) %>% summarize(Total = n_distinct(editor_id))
EdsPerCountryPerJrnlPerYr.LONG[is.na(EdsPerCountryPerJrnlPerYr.LONG)] <- 0
DivDataJrnl<-as_tibble(EdsPerCountryPerJrnlPerYr.LONG)
DivDataJrnl<-spread(DivDataJrnl, geo.code, Total) 
DivDataJrnl[is.na(DivDataJrnl)] <- 0
DivDataJrnl<-ungroup(DivDataJrnl)
#Using simposns inverse
IsimpsonJRNL <- diversity((DivDataJrnl %>% select(-YEAR, - JOURNAL)), index="invsimpson") #Need to strip away the journal and year columns for vegan to do the analysis
# Table DIVERSITY with Results and Journals
JrnlIsimpDivTable <- data.frame(IsimpsonJRNL)
JrnlIsimpDivTable$YEAR <-DivDataJrnl$YEAR #Add year as a column
JrnlIsimpDivTable$JOURNAL <-DivDataJrnl$JOURNAL #Add year as a column
JrnlIsimpDivTable<-rename(JrnlIsimpDivTable, InvSimpson=IsimpsonJRNL) #rename the columns
JrnlIsimpDivTable <- JrnlIsimpDivTable[c("JOURNAL","YEAR","InvSimpson")] #reorder the columns
# ShannonDivTable<-arrange(ShannonDivTable, YEAR, desc(ShannonDiv)) # sort in descending order
JrnlIsimpDivTable

#computing evenness
GEOperYRJRNL<-EdsPerCountryPerJrnlPerYr.LONG %>% summarize(Countries = sum(n_distinct(geo.code)))
JrnlIsimpDivTable<-full_join(GEOperYRJRNL,JrnlIsimpDivTable, by=c("JOURNAL","YEAR"))
JrnlIsimpDivTable<-mutate(JrnlIsimpDivTable, Geo.Evenness = InvSimpson/Countries)

rm(GEOperYRJRNL, IsimpsonJRNL,DivDataJrnl,EdsPerCountryPerJrnlPerYr.LONG)


JrnlDiversityFig<-ggplot(data=JrnlIsimpDivTable, aes(x=YEAR, y=InvSimpson)) +
  geom_line(size=1, color="blue")+
  facet_wrap(~JOURNAL, nrow=4, scales="fixed")+
  # ylab("Geographic Diversity") +
  # ggtitle('Appendix B: Geographic Diversity of environmental biology journals')+
  ylab(bquote('Geographic Diversity (D'[2]*')'))+
  xlab("Year")+
  scale_color_brewer(palette="Dark2")+
  scale_y_continuous(breaks=seq(0, 18, 5))+   #####NEED TO SET MARGIN BY HIGHEST POSSIBLE VALUE FOR THAT JOURNAL???
  scale_x_continuous(breaks=seq(FirstYear, LastYear+1, 10))
JrnlDiversityFig<-JrnlDiversityFig+theme_bw()+
  theme(axis.title.x=element_text(colour="black", size = 10, vjust=0),            #sets x axis title size, style, distance from axis #add , face = "bold" if you want bold
        axis.title.y=element_text(colour="black", size = 10, vjust=0),            #sets y axis title size, style, distance from axis #add , face = "bold" if you want bold
        axis.text.y=element_text(colour="black", size = 8),
        axis.text.x=element_text(colour="black", size = 8,angle=45, hjust=1),
        panel.grid.minor = element_blank(),
        strip.background = element_blank(),
        panel.spacing.x = unit(0.75, "lines"),
        panel.spacing.y = unit(0.1, "lines"),
        # strip.background = element_rect(colour="black", fill="white"),
        panel.border = element_rect(colour = "black"),
        # legend.background = element_rect(colour = 'black', size = 0.5, linetype='solid'),
        legend.position="top",
        legend.text = element_text(color="black", size=8), 
        legend.title = element_blank(),   #Removes the Legend title
        legend.background = element_rect(colour = 'black', size = 0.5, linetype='solid'),
        strip.text = element_text(face = "italic", size = 6))
JrnlDiversityFig
# dev.off()

ggsave("S4_Fig.eps", JrnlDiversityFig, device = "eps", scale = 1, width = 19, height = 13.2, units ="cm", dpi = 400, limitsize = TRUE)
# ggsave("S4_Fig_size_test.eps", JrnlDiversityFig, device = "eps", scale = 1, width = 19, height = 13.2, units = "cm", dpi = 400, limitsize = TRUE)
# 
# tiff("S4_Fig_size_test.tiff", width = 19, height = 13.2, units = 'cm', res = 400, compression = 'lzw')
# JrnlDiversityFig
# dev.off()
# 
# 
# source("multiplot.R")
# # AppA<-multiplot(JrnlRichnessFig,JrnlDiversityFig,JrnlEvennessFig, cols=1)
# AppA<-multiplot(JrnlRichnessFig,JrnlDiversityFig, cols=1)
# 
# # ggsave("AppAggsave.eps", plot = multiplot(JrnlRichnessFig,JrnlDiversityFig,JrnlEvennessFig, cols=1), scale = 1, width=15,height=30,dpi = 300, limitsize = TRUE)
# ggsave("AppAggsave.eps", plot = multiplot(JrnlRichnessFig,JrnlDiversityFig, cols=1), scale = 1, width=19,height=22,dpi = 400, limitsize = TRUE)
# dev.off()
# 
# tiff("AppA_size_test.tiff", width = 19, height = 22, units = 'cm', res = 400, compression = 'lzw')
# multiplot(JrnlRichnessFig,JrnlDiversityFig, cols=1)
# dev.off()
# 
# 
# pdf(file="AppendixA.pdf")
# JrnlRichnessFig
# JrnlDiversityFig
# # JrnlEvennessFig
# dev.off()


######################################################
# S5 Fig: Prop of EDITORS EACH YEAR FROM EACH REGION  plit by JOURNAL
######################################################
# pdf(file="AppB.pdf", width=14, height=10)
# setEPS()
# postscript("S5_fig.eps",width=14, height=20)
RegionyrJRNL<-AnalysisData %>% group_by(JOURNAL, YEAR, REGION) %>% summarize(Total = n_distinct(editor_id))
RegionyrJRNL[is.na(RegionyrJRNL)] <- 0
RegionyrJRNL2<-RegionyrJRNL %>% group_by(JOURNAL, YEAR)  %>%  summarise(Editors=sum(Total))
RegionyrJRNL<-full_join(RegionyrJRNL,RegionyrJRNL2, by=c("JOURNAL","YEAR")) %>% mutate(pcnt=Total/Editors*100)
rm(RegionyrJRNL2)

RegionyrJRNLFig<-ggplot(data=RegionyrJRNL, aes(x=YEAR, y=pcnt, color=REGION)) +
  geom_line(size=1)+
  scale_color_brewer(palette="Dark2")+
  scale_x_continuous(breaks=seq(FirstYear, LastYear+1, 10))+
  facet_wrap(~JOURNAL, nrow=4, scales="fixed")+
  # facet_wrap(~JOURNAL, nrow=6, scales="free")+ #USE THIS ONE IF YOU USE NUMBERS INSTEAD OF % DUE TO UNEQUAL ED BOARD SIZES
  # ggtitle('Appendix C: Distribution of Editors by global region'+
  ylab("Percent") +
  xlab("Year")
  
RegionyrJRNLFig<-RegionyrJRNLFig+theme_bw()+
  theme(axis.title.x=element_text(colour="black", size = 10, vjust=0),            #sets x axis title size, style, distance from axis #add , face = "bold" if you want bold
        axis.title.y=element_text(colour="black", size = 10, vjust=0),            #sets y axis title size, style, distance from axis #add , face = "bold" if you want bold
        axis.text.y=element_text(colour="black", size = 6),
        axis.text.x=element_text(colour="black", size = 6,angle=45, hjust=1),
        panel.grid.minor = element_blank(),
        strip.background = element_blank(),
        panel.spacing.x = unit(0.5, "lines"),
        panel.spacing.y = unit(0.1, "lines"),
        # strip.background = element_rect(colour="black", fill="white"),
        panel.border = element_rect(colour = "black"),
        legend.position="top",
        legend.text = element_text(color="black", size=8,lineheight=1), 
        legend.title = element_blank(),   #Removes the Legend title
        legend.spacing=unit(0.75, "lines"),
        legend.key.size = unit(0.5, "lines"),
        legend.background = element_rect(colour = 'black', size = 0.5, linetype='solid'),
        strip.text = element_text(face = "italic", size = 6))

RegionyrJRNLFig

ggsave("S5_Fig.eps", RegionyrJRNLFig, device = "eps", scale = 1, width = 19, height = 13.2, units = "cm", dpi = 400, limitsize = TRUE)
# ggsave("S5_Fig_size_test.eps", RegionyrJRNLFig, device = "eps", scale = 1, width = 19, height = 13.2,units = "cm", dpi = 400, limitsize = TRUE)
# 
# 
# tiff("S5_Fig_size_test.tiff",width = 19, height = 13.2,units = 'cm', res = 400, compression = 'lzw')
# RegionyrJRNLFig
# dev.off()


#plot.margin =unit(c(0,1,0,1.5), "cm")) #+  #plot margin - top, right, bottom, left



######################################################
# S6 Fig: Geo Prop of EDITORS EACH YEAR FROM EACH INCOME CATEGORY  plit by JOURNAL
######################################################

INCOMEyrJRNL<-AnalysisData %>% group_by(JOURNAL, YEAR, INCOME_LEVEL) %>% summarize(Total = n_distinct(editor_id))
INCOMEyrJRNL[is.na(INCOMEyrJRNL)] <- 0
INCOMEyrJRNL2<-INCOMEyrJRNL %>% group_by(JOURNAL, YEAR)  %>%  summarise(Editors=sum(Total))
INCOMEyrJRNL<-full_join(INCOMEyrJRNL,INCOMEyrJRNL2, by=c("JOURNAL","YEAR")) %>% mutate(pcnt=Total/Editors*100)
rm(INCOMEyrJRNL2)

INCOMErJRNLFig<-ggplot(data=INCOMEyrJRNL, aes(x=YEAR, y=pcnt, color=INCOME_LEVEL)) +
  geom_line(size=1)+
  scale_color_brewer(palette="Dark2")+
  scale_x_continuous(breaks=seq(FirstYear, LastYear+1, 10))+
  facet_wrap(~JOURNAL, nrow=4, scales="fixed")+
  # facet_wrap(~JOURNAL, nrow=6, scales="free")+ #USE THIS ONE IF YOU USE NUMBERS INSTEAD OF % DUE TO UNEQUAL ED BOARD SIZES
  # ggtitle('Appendix D: Distribution of Editors by national income category')+
  ylab("Percent") +
  xlab("Year")
  
INCOMErJRNLFig<-INCOMErJRNLFig+theme_bw()+
  theme(axis.title.x=element_text(colour="black", size = 10, vjust=0),            #sets x axis title size, style, distance from axis #add , face = "bold" if you want bold
        axis.title.y=element_text(colour="black", size = 10, vjust=0),            #sets y axis title size, style, distance from axis #add , face = "bold" if you want bold
        axis.text.y=element_text(colour="black", size = 6),
        axis.text.x=element_text(colour="black", size = 6,angle=45, hjust=1),
        panel.grid.minor = element_blank(),
        strip.background = element_blank(),
        panel.spacing.x = unit(0.75, "lines"),
        panel.spacing.y = unit(0.1, "lines"),
        # strip.background = element_rect(colour="black", fill="white"),
        panel.border = element_rect(colour = "black"),
        # legend.background = element_rect(colour = 'black', size = 0.5, linetype='solid'),
        legend.position="top",
        legend.text = element_text(color="black", size=8), 
        legend.title = element_blank(),   #Removes the Legend title
        legend.background = element_rect(colour = 'black', size = 0.5, linetype='solid'),
        strip.text = element_text(face = "italic", size = 6))
INCOMErJRNLFig


ggsave("S6_Fig.eps", INCOMErJRNLFig, device = "eps", scale = 1, width = 19, height = 13.2, units = "cm", dpi = 400, limitsize = TRUE)
# ggsave("S6_Fig_size_test.eps", INCOMErJRNLFig, device = "eps", scale = 1, width = 19, height = 13.2, units = "cm", dpi = 400, limitsize = TRUE)
# # 
# # 
# tiff("S6_Fig_size_test.tiff", width = 19, height = 13.2,units = 'cm', res = 400, compression = 'lzw')
# INCOMErJRNLFig
# dev.off()
# 
# source("multiplot.R")
# AppB<-multiplot(RegionyrJRNLFig,INCOMErJRNLFig, cols=1)
# ggsave("AppBggsave.eps", plot = multiplot(RegionyrJRNLFig,INCOMErJRNLFig, cols=1), scale = 1, width=15,height=30,dpi = 300, limitsize = TRUE)
# dev.off()
# 
# 
# tiff("AppB_size_test.tiff", width = 19, height = 22, units = 'cm', res = 400, compression = 'lzw')
# multiplot(RegionyrJRNLFig,INCOMErJRNLFig, cols=1)
# dev.off()
# 


# tiff(file="AppA.tif", width=9, height=20)
# AppA<-multiplot(JrnlRichnessFig,JrnlDiversityFig, JrnlEvennessFig, cols=1)
# dev.off()



#plot.margin =unit(c(0,1,0,1.5), "cm")) #+  #plot margin - top, right, bottom, left
