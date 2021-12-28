
#devtools::install_github("vdeminstitute/vdemdata")
library(vdemdata)
library(tidyverse)
library(ggplot2)
library(here)
library(plm)
library(lmtest)
library(Hmisc)
library(corrplot)
library(readxl)
library(readr)

# Data import
# 1. Country economic statuses
FAD_Fiscal_Rules_Dataset <- read_excel("Data/FAD-Fiscal Rules Dataset Clear.xlsx", 
                                       sheet = "Clear")
FAD_Fiscal_Rules_Dataset <- FAD_Fiscal_Rules_Dataset %>% 
  select(c("year", "Country name", "Advanced", "Emerging", "Low-income", "Check")) %>% 
  filter(Check != 0) %>%
  select(-"Check") %>% gather("FiskalStatus", "FS", -c("Country name", "year")) %>% 
  filter(FS != 0)
table(FAD_Fiscal_Rules_Dataset$year)

# We extrapolate Fiscal status from the latest available to the net 4 years
FAD_Fiscal_Rules_Dataset %>% filter(year==2015) -> FAD_Fiscal_Rules_Dataset2016
FAD_Fiscal_Rules_Dataset %>% filter(year==2015) -> FAD_Fiscal_Rules_Dataset2017
FAD_Fiscal_Rules_Dataset %>% filter(year==2015) -> FAD_Fiscal_Rules_Dataset2018
FAD_Fiscal_Rules_Dataset %>% filter(year==2015) -> FAD_Fiscal_Rules_Dataset2019
FAD_Fiscal_Rules_Dataset2016[FAD_Fiscal_Rules_Dataset2016=="2015"]<-2016
FAD_Fiscal_Rules_Dataset2017[FAD_Fiscal_Rules_Dataset2017=="2015"]<-2017
FAD_Fiscal_Rules_Dataset2018[FAD_Fiscal_Rules_Dataset2018=="2015"]<-2018
FAD_Fiscal_Rules_Dataset2019[FAD_Fiscal_Rules_Dataset2019=="2015"]<-2019
FAD_Fiscal_Rules_Dataset <- rbind(FAD_Fiscal_Rules_Dataset,FAD_Fiscal_Rules_Dataset2016,FAD_Fiscal_Rules_Dataset2017,FAD_Fiscal_Rules_Dataset2018,FAD_Fiscal_Rules_Dataset2019)
table(FAD_Fiscal_Rules_Dataset$year)
rm(FAD_Fiscal_Rules_Dataset2016,FAD_Fiscal_Rules_Dataset2017,FAD_Fiscal_Rules_Dataset2018,FAD_Fiscal_Rules_Dataset2019)

# 2. V-Deme data set
V_Dem_CY_Full_Others_v11_1 <- read_csv("Data/Country_Year_V-Dem_Full+others_CSV_v11.1/V-Dem-CY-Full+Others-v11.1.csv")
vdem_data <- V_Dem_CY_Full_Others_v11_1 %>% select(c(country_name, country_text_id, year,
                                                     e_migdppc, #GDP per capita
                                                     e_migdpgro, #GDP growth
                                                     e_wb_pop, #Population
                                                     e_peaveduc, #Education 15+
                                                     e_wbgi_gee, #Government effectiveness
                                                     v2xps_party, #Party institutionalization index
                                                     v2exrescon, #Executive respects constitution
                                                     e_regionpol_6C, #Region
                                                     v2xcl_prpty,#Property rights
                                                     v2x_rule, #RuleOfLaw
                                                     e_cow_exports,#Ex
                                                     e_cow_imports,#Im
                                                     e_pelifeex,#Life expectancy
                                                     e_wbgi_cce,#Control of corruption
                                                     e_fh_status,#Status
                                                     e_fh_cl,#Civil liberties
                                                     v2smpolhate,#Political parties hate speech
                                                     e_wbgi_rle,#Rule of law — estimate
                                                     v2x_execorr,#Executive corruption index (D)
                                                     v2x_civlib,# Civil liberties
                                                     v2exrescon_ord# Executive respects constitution (D) 2 cat
))
colnames(vdem_data) <- c("Country", "Code", "Year", "GDP_p_capita", "GDP_growth", "Pop",
                         "Edu", "Gov_eff", "Party_institut_idx", "Exe_respect_constitution", "Region",
                         "P_rights", "RuleOfLaw", "Ex", "Im", "LifeEx", "ControlCorr", "Status", "Civil",
                         "Hate", "RuleOfLaw_Estimate","Exe_corr", "Civil_liberties", "Exe_respect_constitution_cat")


# Descriptive stats table
filter(vdem_data, Year==2014) %>% drop_na(Exe_respect_constitution_cat) %>% group_by(Exe_respect_constitution_cat) %>%
  summarise('Min (2014)'= min(Exe_respect_constitution),
            'Max (2014)' = max(Exe_respect_constitution)) %>%
  select(-Exe_respect_constitution_cat) -> Data2014

filter(vdem_data, Year==2000) %>% drop_na(Exe_respect_constitution_cat) %>% group_by(Exe_respect_constitution_cat) %>%
  summarise('Min (2000)'= min(Exe_respect_constitution),
            'Max (2000)' = max(Exe_respect_constitution)) %>%
  select(-Exe_respect_constitution_cat)-> Data2000

filter(vdem_data, Year==1985) %>% drop_na(Exe_respect_constitution_cat) %>% group_by(Exe_respect_constitution_cat) %>%
  summarise('Min (1985)'= min(Exe_respect_constitution),
            'Max (1985)' = max(Exe_respect_constitution)) -> Data1985

cbind(Data1985,Data2000,Data2014) -> DataAll
text <- c("Members of the executive violate the constitution whenever they want to, without legal consequences","Members of the executive violate most provisions of the constitution without legal consequences, but still must respect certain provisions.","Somewhere in between (1) and (3). Members of the executive would face legal consequences for violating most provisions of the constitution, but can disregard some provisions without any legal consequences.","Members of the executive rarely violate the constitution, and when it happens they face legal charges.","Members of the executive never violate the constitution.")

DataAll$Responses <- text
DataAll%>% select(c(-Exe_respect_constitution_cat))  %>% kable() %>% kable_styling()

vdem_data %>% drop_na(Exe_respect_constitution_cat) %>% group_by(Exe_respect_constitution_cat) %>%
  summarise('Min (1985-2014)'= min(Exe_respect_constitution),
            'Max (1985-2014)' = max(Exe_respect_constitution)) %>%
  select(-Exe_respect_constitution_cat) -> DataEXE
DataEXE$Responses <- text

# Descriptive stats table - export
DataEXE %>% select(Responses, 'Min (1985-2014)', 'Max (1985-2014)')  %>% kable() %>% kable_styling()%>%
  save_kable("tables/tableResponses.html",
             zoom = 2)

# Country names matching
FAD_Fiscal_Rules_Dataset[FAD_Fiscal_Rules_Dataset=="United States"]<-"United States of America"
FAD_Fiscal_Rules_Dataset[FAD_Fiscal_Rules_Dataset=="Slovak Republic"]<-"Slovakia"
FAD_Fiscal_Rules_Dataset[FAD_Fiscal_Rules_Dataset=="Congo"]<-"Democratic Republic of the Congo"
FAD_Fiscal_Rules_Dataset[FAD_Fiscal_Rules_Dataset=="Dominica"]<-"Dominican Republic"
FAD_Fiscal_Rules_Dataset[FAD_Fiscal_Rules_Dataset=="Guinea Bissau"]<-"Guinea-Bissau"
FAD_Fiscal_Rules_Dataset[FAD_Fiscal_Rules_Dataset=="Cote d'Ivoire"]<-"Ivory Coast"
FAD_Fiscal_Rules_Dataset[FAD_Fiscal_Rules_Dataset=="Hong Kong SAR"]<-"Hong Kong"

vdem_data <- inner_join(vdem_data, FAD_Fiscal_Rules_Dataset, by = c("Country" = "Country name", "Year" = "year")) %>%
  select(-c("FS"))

#Unmatched countries - 2 Low-income and 3 Emerging
table(vdem_data$Country) %>% 
  data.frame() %>%
  right_join(FAD_Fiscal_Rules_Dataset, by = c("Var1" = "Country name")) %>%
  filter(is.na(Freq)) %>% select(c(Var1, FiskalStatus)) %>%
  table()

# 3. FDI info
fdi_data <- read.csv(here("FDI", "API_BX.KLT.DINV.CD.WD_DS2_en_csv_v2_2055722.csv"), 
                     skip = 3)
names <- c("x", "Code", "y", "z",seq(1960,2020,1))

colnames(fdi_data) <- names
rm(names)
fdi_data <- fdi_data[,c(2,25:65)] #from 1980 to 2020

fdi_data <- fdi_data %>% gather("Year", "FDI", -Code)
fdi_data$Year <- as.numeric(fdi_data$Year)

FDI_dataset <- inner_join(vdem_data, fdi_data, by = c("Code" = "Code", "Year" = "Year")) %>% drop_na(FDI)
FDI_dataset <- FDI_dataset %>% filter(FDI>0)
FDI_dataset <- FDI_dataset %>% mutate(Trade = (Ex + Im))

data.frame(map(FDI_dataset, ~sum(is.na(.))))

colnames(FDI_dataset)
table(FDI_dataset$Year)


# 4. External Debt info
ExternalDebtStocks <- read_csv("Data/ExternalDebtStocks.csv")
names <- c("x", "y", "Country", "Code",seq(1971,2020,1))

colnames(ExternalDebtStocks) <- names
rm(names)
ExternalDebtStocks <- ExternalDebtStocks %>% filter(y=="DT.DOD.DECT.CD")
ExternalDebtStocks <- ExternalDebtStocks[,c(3:54)] #from 1971 to 2020

ExternalDebtStocks <- ExternalDebtStocks %>% gather("Year", "EDebt", -c("Country","Code"))
ExternalDebtStocks$Year <- as.numeric(ExternalDebtStocks$Year)
ExternalDebtStocks$EDebt <- as.numeric(ExternalDebtStocks$EDebt)

FDI_dataset <- inner_join(ExternalDebtStocks, FDI_dataset, by = c("Country"="Country", "Year" = "Year"))

FDI_dataset <- FDI_dataset %>% mutate(Code = Code.x) %>% select(-c(Code.x, Code.y))

# Country names matching and mutate new fields
GDP <- read_csv("Data/GDP/GDP.csv", skip = 4)

names <- c("Country", "Code","x", "y", seq(1960,2021,1))
colnames(GDP) <- names
rm(names)
GDP <- GDP[,c(1:2,5:65)] #from 1971 to 2020
GDP <- GDP %>% gather("Year", "GDP", -c("Country","Code"))
GDP$Year <- as.numeric(GDP$Year)

GDP[GDP=="United States"]<-"United States of America"
GDP[GDP=="Slovak Republic"]<-"Slovakia"
GDP[GDP=="Congo, Dem. Rep."]<-"Democratic Republic of the Congo"
GDP[GDP=="Cabo Verde"]<-"Cape Verde"
GDP[GDP=="Cote d'Ivoire"]<-"Ivory Coast"
GDP[GDP=="Hong Kong SAR, China"]<-"Hong Kong"
FDI_dataset <- inner_join(GDP, FDI_dataset, by = c("Country"="Country", "Year" = "Year"))

FDI_dataset <- FDI_dataset %>% mutate(Trade_op = Trade/GDP)
FDI_dataset <- FDI_dataset %>% mutate(Code = Code.x) %>% select(-c(Code.x, Code.y))

FDI_dataset$Exe_respect_constitution_cat <- as.factor(FDI_dataset$Exe_respect_constitution_cat)

# Data export
write.csv(FDI_dataset, here("Processed_Data", file = "FDI_dataset.csv"))
