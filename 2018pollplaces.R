library(dplyr)
library(readxl)

ALA <- read.csv("2018 Precincts/ALA20140616_PctPoll.txt", header = FALSE)
BAY <- read_xlsx("2018 Precincts/BAY20181015_PctPoll.xlsx")
BRA <- read_xlsx("2018 Precincts/BRA20120507_PctPoll.xlsx")
BRE <- read.delim("2018 Precincts/BRE20180726_PctPoll.txt", header = TRUE)
BRO <- read.delim("2018 Precincts/BRO20131121_PctPoll.txt", header = FALSE)
CAL <- read_xls("2018 Precincts/CAL20120622_PctPoll.xls")
CHA <- read.delim("2018 Precincts/CHA20160523_PctPoll.txt", header = TRUE)
CLA <- read.delim("2018 Precincts/CLA20180507_PctPoll.txt", header = TRUE)
CLL <- read.delim("2018 Precincts/CLL20180611_PctPoll.txt", header = FALSE)
CLM <- read.csv("2018 Precincts/CLM20160729_PctPoll.txt", header = FALSE)
DES <- read.delim("2018 Precincts/DES20180713_PctPoll.txt", header = FALSE)
DUV <- read_xls("2018 Precincts/DUV20120615_PctPoll.xls")
ESC <- read.delim("2018 Precincts/ESC20180727_PctPoll.txt", header = TRUE)
FRA <- read_xls("2018 Precincts/FRA20120705_PctPoll.xls")
GAD <- read_xlsx("2018 Precincts/GAD20181015_PctPoll.xlsx")
GLA <- read.delim("2018 Precincts/GLA20180608_PctPoll.txt", header = FALSE)
HAM <- read_xls("2018 Precincts/HAM20120627_PctPoll.xls")
HER <- read.delim("2018 Precincts/HER20180305_PctPoll.txt", header = TRUE)
HIG <- read.delim("2018 Precincts/HIG20180928_PctPoll.txt", header = TRUE)
HIL <- read.delim("2018 Precincts/HIL20181012_PctPoll.txt", header = FALSE)
HOL <- read_xlsx("2018 Precincts/HOL20120521_PctPoll.xlsx")
IND <- read.delim("2018 Precincts/IND20170630_PctPoll.txt", header = TRUE)
HOL <- read_xlsx("2018 Precincts/HOL20120521_PctPoll.xlsx")
LAK <- read.delim("2018 Precincts/LAK20171010_PctPoll.txt", header = TRUE)
LEE <- read.delim("2018 Precincts/LEE20180813_PctPoll.txt", header = FALSE)
LEO <- read.delim("2018 Precincts/LEO20180921_PctPoll.txt", header = TRUE)
LEV <- read.delim("2018 Precincts/LEV20180410_PctPoll.txt", header = FALSE)
MAN <- read.delim("2018 Precincts/MAN20180504_PctPoll.txt", header = FALSE)
MON <- read.delim("2018 Precincts/MON20181004_PctPoll.txt", header = TRUE)
MRN <- read.delim("2018 Precincts/MRN20160516_PctPoll.txt", header = FALSE)
MRT <- read.delim("2018 Precincts/MRT20180213_PctPoll.txt", header = FALSE)
NAS <- read.delim("2018 Precincts/NAS20180314_PctPoll.txt", header = TRUE)
ORA <- read.delim("2018 Precincts/ORA20181005_PctPoll.txt", header = FALSE)
OSC <- read.delim("2018 Precincts/OSC20180917_PctPoll.txt", header = TRUE)
PAS <- read.delim("2018 Precincts/PAS20181015_PctPoll.txt", header = TRUE)
PIN <- read.delim("2018 Precincts/PIN20160503_PctPoll.txt", header = FALSE)
POL <- read.delim("2018 Precincts/POL20180403_PctPoll.txt", header = FALSE)
PUT <- read.delim("2018 Precincts/PUT20160614_PctPoll.txt", header = TRUE)
SAN <- read.delim("2018 Precincts/SAN20180322_PctPoll.txt", header = TRUE)
SAR <- read.delim("2018 Precincts/SAR20180924_PctPoll.txt", header = TRUE)
SEM <- read.delim("2018 Precincts/SEM20120713_PctPoll.txt", header = FALSE)
STJ <- read.delim("2018 Precincts/STJ20180501_PctPoll.txt", header = TRUE)
STL <- read.delim("2018 Precincts/STL20180913_PctPoll.txt", header = TRUE)
SUM <- read.delim("2018 Precincts/SUM20170509_PctPoll.txt", header = TRUE)
VOL <- read.delim("2018 Precincts/VOL20170809_PctPoll.txt", header = TRUE)
WAL <- read.delim("2018 Precincts/WAL20150512_PctPoll.txt", header = TRUE)
WAS <- read_xlsx("2018 Precincts/WAS20181015_PctPoll.xlsx")


ALAscrape <- FLPollPlace %>%
  filter(abr == "ALA")

ALAclean <- ALA %>%
  mutate(address.inc = paste(as.character(V4), as.character(V5),"FL", sep = ", ")) %>%
  mutate(address = as.factor(paste(address.inc, substr(V6, start = 1, stop = 5)))) %>%
  mutate(pollname = V3, abr = V1, precinct = as.factor(V2), abrprecincts = as.factor(paste0(V1, V2))) %>%
  select(abr, precinct, address, pollname, abrprecincts)

BAYclean <- BAY %>%
  mutate(address.inc = paste(Address1, City,"FL", sep = ", ")) %>%
  mutate(address = as.factor(paste(address.inc, substr(Zip, start = 1, stop = 5)))) %>%
  mutate(pollname = as.factor(`Polling Location`), abr = as.factor(`County Code`), 
         precinct = as.factor(`Precinct Number`), 
         abrprecincts = as.factor(paste0(`County Code`, `Precinct Number`))) %>%
  select(abr, precinct, address, pollname, abrprecincts)

BRAclean <- BRA %>%
  mutate(address.inc = paste(Address1, City,"FL", sep = ", ")) %>%
  mutate(address = as.factor(paste(address.inc, substr(Zip, start = 1, stop = 5)))) %>%
  mutate(pollname = as.factor(`Polling Location`), abr = as.factor(`County Code`), 
         precinct = as.factor(`Precinct Number`), 
         abrprecincts = as.factor(paste0(`County Code`, `Precinct Number`))) %>%
  select(abr, precinct, address, pollname, abrprecincts)

BREclean <- BRE %>%
  mutate(address.inc = paste(Address1, City,"FL", sep = ", ")) %>%
  mutate(address = as.factor(paste(address.inc, substr(Zip, start = 1, stop = 5)))) %>%
  mutate(pollname = `Polling.Location`, abr = `County.Code`, 
         precinct = as.factor(substr(`Precinct.Number`, start = 1, stop = 3)), 
         abrprecincts = as.factor(paste0(`County.Code`, substr(`Precinct.Number`, start = 1, stop = 3)))) %>%
  select(abr, precinct, address, pollname, abrprecincts)

BROclean <- BRO %>%
  mutate(address.inc = paste(as.character(V4), as.character(V6),"FL", sep = ", ")) %>%
  mutate(address = as.factor(paste(address.inc, substr(V7, start = 1, stop = 5)))) %>%
  mutate(pollname = V3, abr = as.factor("BRO"), precinct = as.factor(V2), abrprecincts = as.factor(paste0("BRO", V2))) %>%
  select(abr, precinct, address, pollname, abrprecincts)

CALclean <- CAL %>%
  mutate(address.inc = paste(Address1, City,"FL", sep = ", ")) %>%
  mutate(address = as.factor(paste(address.inc, substr(Zip, start = 1, stop = 5)))) %>%
  mutate(pollname = as.factor(`PollingLocation`), abr = as.factor(`CountyCode`), 
         precinct = as.factor(`PrecinctNumber`), 
         abrprecincts = as.factor(paste0(`CountyCode`, `PrecinctNumber`))) %>%
  select(abr, precinct, address, pollname, abrprecincts)

CHAclean <- CHA %>%
  mutate(address.inc = paste(Address, City,"FL", sep = ", ")) %>%
  mutate(address = as.factor(paste(address.inc, substr(Zip, start = 1, stop = 5)))) %>%
  mutate(pollname = as.factor(Name), abr = as.factor("CHA"), 
         precinct = as.factor(Precincts), 
         abrprecincts = as.factor(paste0("CHA", Precincts))) %>%
  select(abr, precinct, address, pollname, abrprecincts)

CLAclean <- CLA %>%
  mutate(address.inc = paste(Address, City,"FL", sep = ", ")) %>%
  mutate(address = as.factor(paste(address.inc, substr(Zip, start = 1, stop = 5)))) %>%
  mutate(pollname = as.factor(Polling.Location), abr = as.factor(CLA), 
         precinct = as.factor(Pct), 
         abrprecincts = as.factor(paste0(CLA, Pct))) %>%
  select(abr, precinct, address, pollname, abrprecincts)


CLLclean <- CLL %>%
  mutate(address.inc = paste(V4, V6,"FL", sep = ", ")) %>%
  mutate(address = as.factor(paste(address.inc, substr(V7, start = 1, stop = 5)))) %>%
  mutate(pollname = as.factor(V3), abr = as.factor("CLL"), 
         precinct = as.factor(V2), 
         abrprecincts = as.factor(paste0("CLL", V2))) %>%
  select(abr, precinct, address, pollname, abrprecincts)


#For some reason this does not work there is an error message for pollname = as.factor(data$name)
#that I don't understand and can't seem to fix

pollclean <- function(data, county, pct, adrs, city, zip, name){
  clean <- data %>%
    mutate(address.inc = paste(data$adrs, data$city, "FL", sep = ", ")) %>%
    mutate(address = as.factor(paste(address.inc, substr(data$zip, start = 1, stop = 5)))) %>%
    mutate(pollname = as.factor(data$name), 
           abr = as.factor(data$county), 
           precinct = as.factor(data$pct), 
           abrprecincts = as.factor(paste0(data$county, data$pct))) %>%
    select(abr, precinct, address, pollname, abrprecincts)
  return(clean)
}

CLMclean <- CLM %>%
  mutate(address.inc = paste(V4, V5,"FL", sep = ", ")) %>%
  mutate(address = as.factor(paste(address.inc, substr(V6, start = 1, stop = 5)))) %>%
  mutate(pollname = as.factor(V3), abr = as.factor(V1), 
         precinct = as.factor(V2), 
         abrprecincts = as.factor(paste0(V1, V2))) %>%
  select(abr, precinct, address, pollname, abrprecincts)


DESclean <- DES %>%
  mutate(address.inc = paste(V3, V4, sep = ", ")) %>%
  mutate(address = as.factor(paste(address.inc, substr(V5, start = 1, stop = 5)))) %>%
  mutate(pollname = as.factor(V2), abr = as.factor("DES"), 
         precinct = as.factor(V1), 
         abrprecincts = as.factor(paste0("DES", V1))) %>%
  select(abr, precinct, address, pollname, abrprecincts)


DUVclean <- DUV %>%
  mutate(address.inc = paste(`Address 1`, City,"FL", sep = ", ")) %>%
  mutate(address = as.factor(paste(address.inc, substr(Zip, start = 1, stop = 5)))) %>%
  mutate(pollname = as.factor(`Polling Location`), abr = as.factor(`County Code`), 
         precinct = as.factor(`Precinct Number`), 
         abrprecincts = as.factor(paste0(`County Code`, `Precinct Number`))) %>%
  select(abr, precinct, address, pollname, abrprecincts)

ESCclean <- ESC %>%
  mutate(address.inc = paste(STREET.ADDRESS, CITY, sep = ", ")) %>%
  mutate(address = as.factor(paste(address.inc, substr(PHYSICAL.ZIP, start = 1, stop = 5)))) %>%
  mutate(pollname = as.factor(NAME.OF.POLLING.LOCATION), abr = as.factor("ESC"), 
         precinct = as.factor(PRECINCT), 
         abrprecincts = as.factor(paste0("ESC", PRECINCT))) %>%
  select(abr, precinct, address, pollname, abrprecincts)

FRAclean <- FRA %>%
  mutate(address.inc = paste(Address, City,"FL", sep = ", ")) %>%
  mutate(address = as.factor(paste(address.inc, substr(Zip, start = 1, stop = 5)))) %>%
  mutate(pollname = as.factor(`Polling Location`), abr = as.factor(`County`), 
         precinct = as.factor(`Precinct`), 
         abrprecincts = as.factor(paste0(`County`, 0, `Precinct`))) %>%
  select(abr, precinct, address, pollname, abrprecincts)

GADclean <- GAD %>%
  mutate(address.inc = paste(`Street Address`, City,"FL", sep = ", ")) %>%
  mutate(address = as.factor(paste(address.inc, substr(Zip, start = 1, stop = 5)))) %>%
  mutate(pollname = as.factor(`Name`), abr = as.factor("GAD"), 
         precinct = as.factor(`Number`), 
         abrprecincts = as.factor(paste0("GAD", `Number`))) %>%
  select(abr, precinct, address, pollname, abrprecincts)

GLAclean <- GLA %>%
  mutate(address.inc = paste(V4, V5, "FL", sep = ", ")) %>%
  mutate(address = as.factor(paste(address.inc, substr(V7, start = 1, stop = 5)))) %>%
  mutate(pollname = as.factor(V3), abr = as.factor(V1), 
         precinct = as.factor(V2), 
         abrprecincts = as.factor(paste0(V1, V2))) %>%
  select(abr, precinct, address, pollname, abrprecincts)

HAMclean <- HAM %>%
  mutate(address.inc = paste(`Address1`, City,"FL", sep = ", ")) %>%
  mutate(address = as.factor(paste(address.inc, substr(Zip, start = 1, stop = 5)))) %>%
  mutate(pollname = as.factor(`PollingLocation`), abr = as.factor(CountyCode), 
         precinct = as.factor(`PrecinctNumber`), 
         abrprecincts = as.factor(paste0(CountyCode, `PrecinctNumber`))) %>%
  select(abr, precinct, address, pollname, abrprecincts)

HERclean <- HER %>%
  mutate(address.inc = paste(`Address1`, City,"FL", sep = ", ")) %>%
  mutate(address = as.factor(paste(address.inc, substr(Zip, start = 1, stop = 5)))) %>%
  mutate(pollname = as.factor(`Polling.Location`), abr = as.factor(County.Code), 
         precinct = as.factor(`Precinct.Number`), 
         abrprecincts = as.factor(paste0(County.Code, `Precinct.Number`))) %>%
  select(abr, precinct, address, pollname, abrprecincts)

HIGclean <- HIG %>%
  mutate(address.inc = paste(`Address1`, City,"FL", sep = ", ")) %>%
  mutate(address = as.factor(paste(address.inc, substr(Zip, start = 1, stop = 5)))) %>%
  mutate(pollname = as.factor(`Polling.Location`), abr = as.factor("HIG"), 
         precinct = as.factor(`Precinct.Number`), 
         abrprecincts = as.factor(paste0("HIG", `Precinct.Number`))) %>%
  select(abr, precinct, address, pollname, abrprecincts)

HILclean <- HIL %>%
  mutate(address.inc = paste(V4, V6, "FL", sep = ", ")) %>%
  mutate(address = as.factor(paste(address.inc, substr(V7, start = 1, stop = 5)))) %>%
  mutate(pollname = as.factor(V3), abr = as.factor(V1), 
         precinct = as.factor(V2), 
         abrprecincts = as.factor(paste0(V1, V2))) %>%
  select(abr, precinct, address, pollname, abrprecincts)

HOLclean <- HOL %>%
  mutate(address.inc = paste(Address1, City,"FL", sep = ", ")) %>%
  mutate(address = as.factor(paste(address.inc, substr(Zip, start = 1, stop = 5)))) %>%
  mutate(pollname = as.factor(`Polling Location`), abr = as.factor("HOL"), 
         precinct = as.factor(`Precinct Number`), 
         abrprecincts = as.factor(paste0("HOL", `Precinct Number`))) %>%
  select(abr, precinct, address, pollname, abrprecincts)


INDclean <- IND %>%
  mutate(address.inc = paste(ADDRESS1, City, sep = ", ")) %>%
  mutate(address = as.factor(paste(address.inc, substr(ZIP, start = 1, stop = 5)))) %>%
  mutate(pollname = as.factor(POLLING.LOCATION), abr = as.factor(COUNTY.CODE), 
         precinct = as.factor(PRECINCT.NUMBER), 
         abrprecincts = as.factor(paste0(COUNTY.CODE, PRECINCT.NUMBER))) %>%
  select(abr, precinct, address, pollname, abrprecincts)

LAKclean <- LAK %>%
  mutate(address.inc = paste(`Address1`, City,"FL", sep = ", ")) %>%
  mutate(address = as.factor(paste(address.inc, substr(Zip, start = 1, stop = 5)))) %>%
  mutate(pollname = as.factor(`Polling_Location`), abr = as.factor("LAK"), 
         precinct = as.factor(`Precinct_Number`), 
         abrprecincts = as.factor(paste0("LAK", `Precinct_Number`))) %>%
  select(abr, precinct, address, pollname, abrprecincts)

LEEclean <- LEE %>%
  mutate(address.inc = paste(V4, V6, "FL", sep = ", ")) %>%
  mutate(address = as.factor(paste(address.inc, substr(V7, start = 1, stop = 5)))) %>%
  mutate(pollname = as.factor(V3), abr = as.factor("LEE"), 
         precinct = as.factor(V2), 
         abrprecincts = as.factor(paste0("LEE", V2))) %>%
  select(abr, precinct, address, pollname, abrprecincts)

LEOclean <- LEO %>%
  mutate(address.inc = paste(Address, City_Name,"FL", sep = ", ")) %>%
  mutate(address = as.factor(paste(address.inc, substr(X_Zip_Code, start = 1, stop = 5)))) %>%
  mutate(pollname = as.factor(Location), abr = as.factor(County.Code), 
         precinct = as.factor(Pct), 
         abrprecincts = as.factor(paste0(County.Code, Pct))) %>%
  select(abr, precinct, address, pollname, abrprecincts)

LEVclean <- LEV %>%
  mutate(address.inc = paste(V4, V6, "FL", sep = ", ")) %>%
  mutate(address = as.factor(paste(address.inc, substr(V7, start = 1, stop = 5)))) %>%
  mutate(pollname = as.factor(V3), abr = as.factor("LEV"), 
         precinct = as.factor(V2), 
         abrprecincts = as.factor(paste0("LEV", V2))) %>%
  select(abr, precinct, address, pollname, abrprecincts)

MANclean <- MAN %>%
  mutate(address.inc = paste(V4, V6, "FL", sep = ", ")) %>%
  mutate(address = as.factor(paste(address.inc, substr(V7, start = 1, stop = 5)))) %>%
  mutate(pollname = as.factor(V3), abr = as.factor(V1), 
         precinct = as.factor(V2), 
         abrprecincts = as.factor(paste0(V1, V2))) %>%
  select(abr, precinct, address, pollname, abrprecincts)

MONclean <- MON %>%
  mutate(address.inc = paste(Address.1, City,"FL", sep = ", ")) %>%
  mutate(address = as.factor(paste(address.inc, substr(Zip, start = 1, stop = 5)))) %>%
  mutate(pollname = as.factor(Poll.Location..name.), abr = as.factor(County.Code), 
         precinct = as.factor(Pr...), 
         abrprecincts = as.factor(paste0(County.Code, Pr...))) %>%
  select(abr, precinct, address, pollname, abrprecincts)


MRNclean <- MRN %>%
  mutate(address.inc = paste(V4, V6, "FL", sep = ", ")) %>%
  mutate(address = as.factor(paste(address.inc, substr(V7, start = 1, stop = 5)))) %>%
  mutate(pollname = as.factor(V3), abr = as.factor(V1), 
         precinct = as.factor(V2), 
         abrprecincts = as.factor(paste0(V1, V2))) %>%
  select(abr, precinct, address, pollname, abrprecincts)

MRTclean <- MRT %>%
  mutate(address.inc = paste(V4, V5, "FL", sep = ", ")) %>%
  mutate(address = as.factor(paste(address.inc, substr(V6, start = 1, stop = 5)))) %>%
  mutate(pollname = as.factor(V3), abr = as.factor(V1), 
         precinct = as.factor(V2), 
         abrprecincts = as.factor(paste0(V1, V2))) %>%
  select(abr, precinct, address, pollname, abrprecincts)

NASclean <- NAS %>%
  mutate(address.inc = paste(Address1, City,"FL", sep = ", ")) %>%
  mutate(address = as.factor(paste(address.inc, substr(Zip, start = 1, stop = 5)))) %>%
  mutate(pollname = as.factor(Polling.Location), abr = as.factor(Ctycode), 
         precinct = as.factor(Precinct.Number), 
         abrprecincts = as.factor(paste0(Ctycode, Precinct.Number))) %>%
  select(abr, precinct, address, pollname, abrprecincts)

ORAclean <- ORA %>%
  mutate(address.inc = paste(V4, V6, "FL", sep = ", ")) %>%
  mutate(address = as.factor(paste(address.inc, substr(V7, start = 1, stop = 5)))) %>%
  mutate(pollname = as.factor(V3), abr = as.factor(V1), 
         precinct = as.factor(V2), 
         abrprecincts = as.factor(paste0(V1, V2))) %>%
  select(abr, precinct, address, pollname, abrprecincts)


OSCclean <- OSC %>%
  mutate(address.inc = paste(ADDRESS1, CITY, sep = ", ")) %>%
  mutate(address = as.factor(paste(address.inc, substr(ZIP, start = 1, stop = 5)))) %>%
  mutate(pollname = as.factor(POLLING.LOCATION), abr = as.factor(COUNTY.CODE), 
         precinct = as.factor(PRECINCT.NUMBER), 
         abrprecincts = as.factor(paste0(COUNTY.CODE, PRECINCT.NUMBER))) %>%
  select(abr, precinct, address, pollname, abrprecincts)

PASclean <- PAS %>%
  mutate(address.inc = paste(paste(Street_Number, Street_Name, Street_Type), City_Name, State, sep = ", ")) %>%
  mutate(address = as.factor(paste(address.inc, substr(ZIP, start = 1, stop = 5)))) %>%
  mutate(pollname = as.factor(NAME), abr = as.factor("PAS"), 
         precinct = as.factor(PollingPlace), 
         abrprecincts = as.factor(paste0("PAS", PollingPlace))) %>%
  select(abr, precinct, address, pollname, abrprecincts)

PINclean <- PIN %>%
  mutate(address.inc = paste(V4, V6, "FL", sep = ", ")) %>%
  mutate(address = as.factor(paste(address.inc, substr(V7, start = 1, stop = 5)))) %>%
  mutate(pollname = as.factor(V3), abr = as.factor(V1), 
         precinct = as.factor(V2), 
         abrprecincts = as.factor(paste0(V1, V2))) %>%
  select(abr, precinct, address, pollname, abrprecincts)

POLclean <- POL %>%
  mutate(address.inc = paste(V4, V6, "FL", sep = ", ")) %>%
  mutate(address = as.factor(paste(address.inc, substr(V7, start = 1, stop = 5)))) %>%
  mutate(pollname = as.factor(V3), abr = as.factor(V1), 
         precinct = as.factor(V2), 
         abrprecincts = as.factor(paste0(V1, V2))) %>%
  select(abr, precinct, address, pollname, abrprecincts)

PUTclean <- PUT %>%
  mutate(address.inc = paste(Address1, City,"FL", sep = ", ")) %>%
  mutate(address = as.factor(paste(address.inc, substr(Zip, start = 1, stop = 5)))) %>%
  mutate(pollname = as.factor(`PollingLocation`), abr = as.factor(`CountyCode`), 
         precinct = as.factor(`PrecinctNumber`), 
         abrprecincts = as.factor(paste0(`CountyCode`, `PrecinctNumber`))) %>%
  select(abr, precinct, address, pollname, abrprecincts)

SANclean <- SAN %>%
  mutate(address.inc = paste(Address1, City,"FL", sep = ", ")) %>%
  mutate(address = as.factor(paste(address.inc, substr(Zip, start = 1, stop = 5)))) %>%
  mutate(pollname = as.factor(`PollingLocation`), abr = as.factor(`CountyCode`), 
         precinct = as.factor(`Precinct`), 
         abrprecincts = as.factor(paste0(`CountyCode`, `Precinct`))) %>%
  select(abr, precinct, address, pollname, abrprecincts)

SARclean <- SAR %>%
  mutate(address = paste(ADDRESS, CITY..STATE..ZIP, sep = ", ")) %>%
  mutate(pollname = as.factor(PRECINCT.NAME), abr = as.factor("SAR"), 
         precinct = as.factor(X.), 
         abrprecincts = as.factor(paste0("SAR", X.))) %>%
  select(abr, precinct, address, pollname, abrprecincts)

SEMclean <- SEM %>%
  mutate(address.inc = paste(V5, V7, "FL", sep = ", ")) %>%
  mutate(address = as.factor(paste(address.inc, substr(V8, start = 1, stop = 5)))) %>%
  mutate(pollname = as.factor(V4), abr = as.factor(V2), 
         precinct = as.factor(V3), 
         abrprecincts = as.factor(paste0(V2, V4))) %>%
  select(abr, precinct, address, pollname, abrprecincts)

STJclean <- STJ %>%
  mutate(address = paste(Location.Address, City..State..Zip, sep = ", ")) %>%
  mutate(pollname = as.factor(Polling.Place), abr = as.factor("STJ"), 
         precinct = as.factor(Precinct), 
         abrprecincts = as.factor(paste0("STJ", Precinct))) %>%
  select(abr, precinct, address, pollname, abrprecincts)

STLclean <- STL %>%
  mutate(address = Precinct.Address) %>%
  mutate(pollname = as.factor(Precinct.Name), abr = as.factor("STL"), 
         precinct = as.factor(Precinct.Number), 
         abrprecincts = as.factor(paste0("STL", Precinct.Number))) %>%
  select(abr, precinct, address, pollname, abrprecincts)


SUMclean <- SUM %>%
  mutate(address.inc = paste(Address.1, City,"FL", sep = ", ")) %>%
  mutate(address = as.factor(paste(address.inc, substr(Zip, start = 1, stop = 5)))) %>%
  mutate(pollname = `Polling.Location`, abr = `County.Code`, 
         precinct = as.factor(`Precinct.Number`), abrprecincts = as.factor(paste0(`County.Code`, `Precinct.Number`))) %>%
  select(abr, precinct, address, pollname, abrprecincts)

VOLclean <- VOL %>%
  mutate(address.inc = paste(ADDRESS1, CITY, sep = ", ")) %>%
  mutate(address = as.factor(paste(address.inc, substr(ZIP, start = 1, stop = 5)))) %>%
  mutate(pollname = as.factor(POLLING_LOCATION), abr = as.factor(COUNTY.CODE), 
         precinct = as.factor(PRECINCT_NUMBER), 
         abrprecincts = as.factor(paste0(COUNTY.CODE, PRECINCT_NUMBER))) %>%
  select(abr, precinct, address, pollname, abrprecincts)

WALclean <- WAL %>%
  mutate(address.inc = paste(`Address1`, City,"FL", sep = ", ")) %>%
  mutate(address = as.factor(paste(address.inc, substr(Zip, start = 1, stop = 5)))) %>%
  mutate(pollname = as.factor(`Polling.Location`), abr = as.factor(County.Code), 
         precinct = as.factor(`Precinct.Number`), 
         abrprecincts = as.factor(paste0(County.Code, `Precinct.Number`))) %>%
  select(abr, precinct, address, pollname, abrprecincts)

WASclean <- WAS %>%
  mutate(address.inc = paste(Address, City,"FL", sep = ", ")) %>%
  mutate(address = as.factor(paste(address.inc, substr(Zip, start = 1, stop = 5)))) %>%
  mutate(pollname = as.factor(`Location`), abr = as.factor("WAS"), 
         precinct = as.factor(`Precinct`), 
         abrprecincts = as.factor(paste0("WAS", `Precinct`))) %>%
  select(abr, precinct, address, pollname, abrprecincts)


FLPollPlace2 <- FLPollPlace %>%
  filter(abr != "ALA", abr != "BAY", abr != "BRA", abr != "BRE", abr != "BRO", abr != "CAL", 
         abr != "CHA", abr != "CLA", abr != "CLL", abr != "CLM", abr != "DES", abr != "DUV", 
         abr != "ESC", abr != "FRA", abr != "GAD", abr != "GLA", abr != "HAM", abr != "HER", 
         abr != "HIG", abr != "HIL", abr != "HOL", abr != "IND", abr != "LAK", abr != "LEE", 
         abr != "LEV", abr != "MAN", abr != "MON", abr != "MRN", abr != "MRT", abr != "NAS", 
         abr != "OSC", abr != "PAS", abr != "PIN", abr != "POL", abr != "PUT", abr != "SAN", 
         abr != "SEM", abr != "STJ", abr != "STL", abr != "SUM", abr != "VOL", abr != "WAL", 
         abr != "WAS") %>% 
  full_join(ALAclean) %>%
  full_join(BAYclean) %>%
  full_join(BRAclean) %>%
  full_join(BREclean) %>%
  full_join(BROclean) %>% 
  full_join(CALclean) %>%
  full_join(CHAclean) %>%
  full_join(CLAclean) %>%
  full_join(CLLclean) %>%
  full_join(CLMclean) %>%
  full_join(DESclean) %>%
  full_join(DUVclean) %>%
  full_join(ESCclean) %>%
  full_join(FRAclean) %>%
  full_join(GADclean) %>%
  full_join(GLAclean) %>%
  full_join(HAMclean) %>%
  full_join(HERclean) %>%
  full_join(HIGclean) %>% 
  full_join(HILclean) %>%
  full_join(HOLclean) %>%
  full_join(INDclean) %>%
  full_join(LAKclean) %>%
  full_join(LEEclean) %>%
  full_join(LEOclean) %>%
  full_join(LEVclean) %>%
  full_join(MANclean) %>%
  full_join(MONclean) %>%
  full_join(MRNclean) %>% 
  full_join(MRTclean) %>% 
  full_join(NASclean) %>%
  full_join(ORAclean) %>%
  full_join(OSCclean) %>%
  full_join(PASclean) %>%
  full_join(PINclean) %>%
  full_join(POLclean) %>%
  full_join(PUTclean) %>% 
  full_join(SANclean) %>%
  full_join(SARclean) %>%
  full_join(SEMclean) %>%
  full_join(STJclean) %>%
  full_join(STLclean) %>%
  full_join(SUMclean) %>%
  full_join(VOLclean) %>%
  full_join(WALclean) %>%
  full_join(WASclean)

write.csv(FLPollPlace2, "2018PollPlace")


geo2018 <- read.csv("Geocoded2018PollPlace.txt")
geo2018$geocode_rating <- as.numeric(as.character(geo2018$geocode_rating))

geo2018.goodmatch <- geo2018 %>%
  filter(geocode_rating < 40)


geo2018.badmatch <- geo2018 %>%
  filter(geocode_rating >= 40)


geo2018.goodmatch2 <- geo2018 %>%
  filter(geocode_rating <= 20)


geo2018.badmatch2 <- geo2018 %>%
  filter(geocode_rating > 20)
