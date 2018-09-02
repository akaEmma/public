############################################################################
# PROGRAM:  idata.R                                                     #
#                                                                          #
# PURPOSE:  THIS R PROGRAM CREATES A PERMANENT R DATASET                   #
#                FROM THE 2016 NIS-CHILD PUBLIC-USE ASCII FILE             #
#                                                                          #
# NOTES:    THIS PROGRAM IS DIVIDED INTO SEVEN STEPS AS FOLLOWS:           #
#                                                                          #
# STEP 1:   LOAD AN R PACKAGE AND ASSIGN FILE NAMES                        #
#                                                                          #
#       ONE R PACKAGE FOR TWO FUNCTIONS (CONTENTS, WTD.TABLE)              #
#       WILL BE LOADED                                                     #
#       THE USER CAN ENTER THE PATH TO THE ASCII DATA FILE AND             #
#       THE PATH WHERE THE R DATASET WILL BE CREATED                       #
#                                                                          #
# STEP 2:   CREATE FORMATS                                                 #
#                                                                          #
# STEP 3:   READ THE DATA FILE                                             #
#                                                                          #
# STEP 4:   ASSIGN VARIABLE LABELS                                         #
#                                                                          #
# STEP 5:   CREATE A PERMANENT R DATASET                                   #
#                                                                          #
# STEP 6:   ASSIGN FORMATS                                                 #
#                                                                          #
#     FORMATS ARE APPLICABLE ONLY TO CATEGORICAL VARIABLES IN R            #
#                                                                          #
# STEP 7:   CONTENTS AND STATISTICAL ESTIMATES(FREQUENCY) EXAMPLE          #
#                                                                          #
############################################################################

############################################################################
# Step 1:   LOAD R PACKAGES AND ASSIGN FILE NAMES                          #
############################################################################
#Users may need to install the Hmisc library before invoking it.
shhh <- suppressPackageStartupMessages
shhh(library(Hmisc)) #TO USE contents()
shhh(library(tidyverse))
shhh(library(labelled))

#---USE SLASH(/) TO SEPERATE A FILE PATH---#

# use this filename if you don't have the *.rds file available and need
# to start formatting the raw data from scratch.
#
# dir <- "./data/"
# 
# # Full data filename
# fname <- paste0(dir, "immunization_data_2016.csv")



############################################################################
# Step 2:   CREATE FORMATS                                                 #
############################################################################

AGEGRPlevels = c("1", "2", "3")
AGEGRPlabels = c("19 - 23 MONTHS", "24 - 29 MONTHS", "30 - 35 MONTHS")

LANGUAGElevels = c("1", "2", "3")
LANGUAGElabels = c("ENGLISH", "SPANISH", "OTHER")

YNDKRFlevels = c("1", "2", "77", "99")
YNDKRFlabels = c("YES", "NO", "DON'T KNOW", "REFUSED")

SHOTCOUNlevels = c("77","88", "99")
SHOTCOUNlabels = c("DON'T KNOW", "1+ BUT UNKNOWN NUMBER", "REFUSED")

YNlevels = c("1", "2")
YNlabels = c("YES", "NO")

Ylevels = c("1")
Ylabels = c("YES")

CHILDNMlevels = c("1", "2", "3", "77", "99")
CHILDNMlabels = c("ONE", "TWO OR THREE", "FOUR OR MORE", "DON'T KNOW", "REFUSED")

CWIClevels = c("1", "2", "3", "77", "99")
CWIClabels = c("YES", "NO", "NEVER HEARD OF WIC", "DON'T KNOW", "REFUSED")

EDUC1_levels = c("1", "2", "3", "4", "77", "99")
EDUC1_labels = c("< 12 YEARS", "12 YEARS", "> 12 YEARS, NON-COLLEGE GRAD", "COLLEGE GRAD", "DON'T KNOW", "REFUSED")

HISPlevels = c("1", "2", "3", "4", "5")
HISPlabels = c("HISPANIC", "NON-HISPANIC", "OTHER", "DON'T KNOW", "REFUSED")

MOBILlevels = c("1", "2", "77", "99")
MOBILlabels = c("MOVED FROM DIFFERENT STATE", "DID NOT MOVE FROM DIFFERENT STATE", "DON'T KNOW", "REFUSED")

SEXlevels = c("1", "2", "77", "99")
SEXlabels = c("MALE", "FEMALE", "DON'T KNOW", "REFUSED")

INCPOVlevels = c("1", "2", "3", "4")
INCPOVlabels = c("ABOVE POVERTY, > $75K", "ABOVE POVERTY, < =  $75K", "BELOW POVERTY", "UNKNOWN")

HASPDA2Flevels = c("1", "2")
HASPDA2Flabels = c("CHILD HAS ADEQUATE PROVIDER DATA OR ZERO VACCINATIONS", "CHILD DOES NOT HAVE ADEQUATE PROVIDER DATA")

PROVIDlevels = c("1", "2", "3", "4", "5", "6", "7")
PROVIDlabels = c("ALL PUBLIC FACILITIES", "ALL HOSPITAL FACILITIES", "ALL PRIVATE FACILITIES", "ALL MILITARY/OTHER FACILITIES", "MIXED", "TYPE OF PROVIDER UNKNOWN", "ALL WIC CLINIC PROVIDERS")

REGISTRYlevels = c("1", "2", "3", "4")
REGISTRYlabels = c("ALL PROVIDERS", "SOME BUT POSSIBLY OR DEFINITELY NOT ALL PROVIDERS", "NO PROVIDERS", "UNKNOWN/DON'T KNOW")

TYPElevels = c("","01","02","03","04","05","07","08","1L","1M","1N","20","21","22","30","31","32","33","43","44","60","70","71","72","73","74","BC","D3","DH","DK","FL","FM","FN","FO","H2","HA","HB","HG","HI","HM","HS","HY","MA","MB","MM","MP","NC","OT","RB",
"RG","RM","RO","TY","UN","VA","VM","VO","YF")
TYPElabels = c("MISSING", "DT", "DTP", "DTP-UNKNOWN", "DTAP", "DTP-HIB", "DTAP-HIB", "DTAP-HEPB-IPV", "H1N1 FLU-UNKNOWN", "H1N1 FLU SPRAY", "H1N1 FLU INJECTED", "OPV", "IPV", "POLIO-UNKNOWN", "MMR", "MEASLES ONLY", "MEASLES-MUMPS", "MEASLES-RUBELLA",
"HEPB-HIB", "HIB ONLY-UNKNOWN", "HEPB ONLY", "PCV CONJUGATE-UNKNOWN", "PCV POLYSACCHARIDE", "PCV-UNKNOWN", "PCV CONJUGATE-7", "PCV CONJUGATE-13", "BCG (TUBERCULOSIS)", "DTAP-IPV-HIB", "DTP-HEPB", "DON'T KNOW", "SEASONAL FLU-UNKNOWN", "SEASONAL FLU SPRAY",
"SEASONAL FLU INJECTED", "FOUR-IN-ONE", "HIB (SANOFI OR GLAXOSMITHKLINE)", "HEPA", "HEPB-UNKNOWN", "HIB (GLAXOSMITHKLINE)", "HIB-UNKNOWN", "HIB (MERCK)", "HIB (SANOFI)", "HIB-MENCY", "MALARIA", "MUMPS-RUBELLA", "MCV-UNKNOWN", "MUMPS", "NEVER CODABLE",
"OTHER", "RUBELLA", "ROTARIX (GSK)", "ROTATEQ (MERCK)", "ROTAVIRUS-UNKNOWN", "TYPHOID", "UNCODABLE", "VARICELLA-UNKNOWN", "MMR-VARICELLA", "VARICELLA-ONLY", "YELLOW FEVER")

HEPBRTlevels = c("1", "2")
HEPBRTlabels = c("AT LEAST ONE PROVIDER CHECKED GIVEN AT BIRTH", "NO PROVIDERS CHECKED GIVEN AT BIRTH")

HEPFLGlevels = c("1", "2")
HEPFLGlabels = c("HEPB BIRTH SHOT DATE IMPUTED FROM SHOTCARD", "HEPB BIRTH SHOT DATE IMPUTED FROM DISTRIBUTION OF BIRTH DOSE DATES")

UTDlevels = c("0", "1")
UTDlabels = c("NOT UTD", "UTD")

CENREGlevels = c("1", "2", "3", "4")
CENREGlabels = c("NORTHEAST", "MIDWEST", "SOUTH", "WEST")


STATElevels = c("1", "10", "11", "12", "13", "15", "16", "17", "18", "19", "2", "20", "21", "22", "23", "24", "25", "26", "27", "28", "29", "30", "31", "32", "33", "34", "35", "36", "37", "38", "39", "4", "40", "41", "42", "44", "45", "46", "47", "48", "49", "5", "50", "51", "53", "54", "55", "56", "6", "66", "72", "78", "8", "9")
STATElabels = c("ALABAMA", "DELAWARE", "DISTRICT OF COLUMBIA", "FLORIDA", "GEORGIA", "HAWAII", "IDAHO", "ILLINOIS", "INDIANA", "IOWA", "ALASKA", "KANSAS", "KENTUCKY", "LOUISIANA", "MAINE", "MARYLAND", "MASSACHUSETTS", "MICHIGAN", "MINNESOTA",
              "MISSISSIPPI", "MISSOURI", "MONTANA", "NEBRASKA", "NEVADA", "NEW HAMPSHIRE", "NEW JERSEY", "NEW MEXICO", "NEW YORK", "NORTH CAROLINA", "NORTH DAKOTA", "OHIO", "ARIZONA", "OKLAHOMA", "OREGON", "PENNSYLVANIA", "RHODE ISLAND", "SOUTH CAROLINA",
              "SOUTH DAKOTA", "TENNESSEE", "TEXAS", "UTAH", "ARKANSAS", "VERMONT", "VIRGINIA", "WASHINGTON", "WEST VIRGINIA", "WISCONSIN", "WYOMING", "CALIFORNIA", "GUAM", "PUERTO RICO", "U.S. VIRGIN ISLANDS", "COLORADO", "CONNECTICUT")


RACE_PUFlevels = c("1", "2", "3")
RACE_PUFlabels = c("WHITE ONLY", "BLACK ONLY", "OTHER + MULTIPLE RACE")

AGECPOXRlevels = c("1", "2", "3", "4")
AGECPOXRlabels = c("0 TO 6 MONTHS OLD", "7 TO 12 MONTHS OLD", "13 TO 18 MONTHS OLD", "19+ MONTHS OLD")

C1Rlevels = c("1", "2", "3", "4", "5", "6", "7", "8")
C1Rlabels = c("1", "2", "3", "4", "5", "6", "7", "8+")

C5Rlevels = c("1", "2", "3", "4", "77", "99")
C5Rlabels = c("MOTHER (STEP, FOSTER, ADOPTIVE) OR FEMALE GUARDIAN", "FATHER (STEP, FOSTER, ADOPTIVE) OR MALE GUARDIAN", "GRANDPARENT", "OTHER FAMILY MEMBER/FRIEND", "DON'T KNOW", "REFUSED")

INCQ298Alevels = c("10", "11", "12", "13", "14", "3", "4", "5", "6", "7", "77", "8", "9", "99")
INCQ298Alabels = c("$35001 - $40000", "$40001 - $50000", "$50001 - $60000", "$60001 - $75000", "$75001+", "$0 - $7500", "$7501 - $10000", "$10001 - $17500", "$17501 - $20000", "$20001 - $25000", "DON'T KNOW", "$25001 - $30000", "$30001 - $35000", "REFUSED")

RACEETHKlevels = c("1", "2", "3", "4")
RACEETHKlabels = c("HISPANIC", "NON-HISPANIC WHITE ONLY", "NON-HISPANIC BLACK ONLY", "NON-HISPANIC OTHER + MULTIPLE RACE")

D6Rlevels = c("0", "1", "2", "3")
D6Rlabels = c("0", "1", "2", "3+")

FRSTBRNlevels = c("1", "2", "77", "99")
FRSTBRNlabels = c("NO", "YES", "DON'T KNOW", "REFUSED")

CHARIDlevels = c( )
CHARIDlabels = c("MISSING")

BFFORM08Flevels = c("888")
BFFORM08Flabels = c("NEVER FED FORMULA")

RENTOWNlevels = c("1", "2", "3", "77", "99")
RENTOWNlabels = c("OWNED OR BEING BOUGHT", "RENTED", "OTHER ARRANGMENT", "DON'T KNOW", "REFUSED")

NUM_PHONlevels = c("1", "2", "3", "4", "77", "99")
NUM_PHONlabels = c("ONE", "TWO", "THREE OR MORE", "NONE", "DON'T KNOW", "REFUSED")

MAR_PUF2_levels = c("1", "2")
MAR_PUF2_labels = c("MARRIED", "NEVER MARRIED/WIDOWED/DIVORCED/SEPARATED/DECEASED/LIVING WITH PARTNER")

UTDPCVBlevels = c("1", "2", "3")
UTDPCVBlabels = c("4+ PCV7 PLUS 1+ PCV13", "4+ PCV7, NO FOLLOWING PCV13, WITH TYPE OF ALL VACCINES (IF ANY) FOLLOWING THE 4 PCV7 KNOWN", "ALL OTHERS WITH ADEQUATE PROVIDER DATA")

ESTGRANTlevels = c("1", "10", "11", "12", "13", "14", "16", "17", "18", "19", "2", "20", "22", "25", "27", "28", "29", "30", "31", "34", "35", "36", "38", "4", "40", "41", "44", "46", "47", "49", "5", "50", "51", "54", "55", "56", "57", "58", "59", "6", "60", "61", "62", "63", "64", "65", "66", "68", "7", "72", "73", "74", "75", "76", "77", "8")
ESTGRANTlabels = c("CT", "NY-REST OF STATE", "NY-CITY OF NEW YORK", "DC", "DE", "MD", "PA-REST OF STATE", "PA-PHILADELPHIA COUNTY", "VA", "WV", "MA", "AL", "FL", "GA", "KY", "MS", "NC", "SC", "TN", "IL-REST OF STATE", "IL-CITY OF CHICAGO", "IN", "MI", "ME",
"MN", "OH", "WI", "AR", "LA", "NM", "NH", "OK", "TX-REST OF STATE", "TX-CITY OF HOUSTON", "TX-BEXAR COUNTY", "IA", "KS", "MO", "NE", "RI", "CO", "MT", "ND", "SD", "UT", "WY", "AZ", "CA", "VT", "HI", "NV", "AK", "ID", "OR", "WA", "NJ")

INS_STAT_Ilevels = c("1", "2", "3", "4")
INS_STAT_Ilabels = c("PRIVATE INSURANCE", "ANY MEDICAID", "OTHER INSURANCE", "UNINSURED")

INS_BREAK_Ilevels = c("1", "2", "3", "4")
INS_BREAK_Ilabels = c("CURRENTLY INSURED BUT UNINSURED AT SOME POINT SINCE AGE 11", "CURRENTLY INSURED AND NEVER UNINSURED SINCE AGE 11", "CURRENTLY UNINSURED BUT INSURED AT SOME POINT SINCE AGE 11",
                    "CURRENTLY UNINSURED AND NEVER INSURED SINCE AGE 11")


ESTIAP16Flevels = c("1", "10", "105", "106", "11", "12", "13", "14", "16", "17", "18", "19", "2", "20", "22", "25", "27", "28", "29", "30", "31", "34", "35", "36", "38", "4", "40", "41", "44", "46", "47", "49", "5", "50", "51", "52", "53", "54", "55", "56", "57", "58", "59", "6", "60", "61", "62", "63", "64", "65", "66", "68", "7", "72", "73", "74", "75", "76", "77", "8", "95")

ESTIAP16Flabels = c("CT", "NY-REST OF STATE", "GUAM", "PUERTO RICO", "NY-CITY OF NEW YORK", "DC", "DE", "MD", "PA-REST OF STATE", "PA-PHILADELPHIA COUNTY", "VA", "WV", "MA", "AL", "FL", "GA", "KY", "MS", "NC", "SC", "TN", "IL-REST OF STATE",
"IL-CITY OF CHICAGO", "IN", "MI", "ME", "MN", "OH", "WI", "AR", "LA", "NM", "NH", "OK", "TX-REST OF STATE", "TX-DALLAS COUNTY", "TX-EL PASO COUNTY", "TX-CITY OF HOUSTON", "TX-BEXAR COUNTY", "IA", "KS", "MO", "NE", "RI", "CO", "MT", "ND", "SD", "UT", "WY",
"AZ", "CA", "VT", "HI", "NV", "AK", "ID", "OR", "WA", "NJ", "US VIRGIN ISLANDS")

MAGEGRP2_levels = c("1", "2", "77", "99")
MAGEGRP2_labels = c("< =  29 YEARS", "> =  30 YEARS", "DON'T KNOW", "REFUSED")

####################
# read file
####################

# use different command for raw data file. 

fname <- "./data/immunization_data_2016.rds"
idata <- read_rds(fname)

###################
# change some of the integers to characters
####################
source("format_char_vars.r")
idata <- format_char_vars(idata)


############################################################################
# Step 5:   CREATE A PERMANENT R DATASET                                   #
############################################################################

write_rds(idata, 
          "./data/immunization_data_2016.rds")

############################################################################
# Step 6:   Factor vars
############################################################################
source("assign_factor_vars.r")
idata <- assign_factor_vars(idata)

############################################################################
# ASSIGN VARIABLE LABELS                                         #
############################################################################
source("idata_labels.r")
idata <- get_idata_labels(idata)

############################################################################
# Step WHATEVER:   CREATE A PERMANENT R DATASET AGAIN #
############################################################################

write_rds(idata, 
          "./data/immunization_data_2016.rds")

##########################################################################
# STEP 7:   CONTENTS AND STATISTICAL ESTIMATES(FREQUENCY) EXAMPLE        #
##########################################################################

#---CONTENTS---#
idata.CONTENTS <- contents(idata)$contents

print(idata.CONTENTS)

#IN CASE THAT THE R CONSOLE CANNOT DISPLAY WHOLE CONTENTS,
#YOU CAN SAVE CONTENTS INTO A FILE
write.table(idata.CONTENTS, file = paste("./data/","toc",sep = ""))

#---UNWEIGHTED FREQUENCY---#
source("unwt_freq.r")

unwt_freq(idata$AGEGRP)

#---WEIGHTED FREQUENCY---#
WT <- idata$RDDWT_D     #INPUT A WEIGHT VARIABLE
WT.VAR <- idata$AGEGRP  #INPUT A VARIABLE OF INTEREST

wt.tab <- wtd.table(WT.VAR, weights =  WT, type = 'table')
wtd.freq <- data.frame(cbind(
    wt.tab, round(wt.tab/sum(wt.tab)*100,2),
    cumsum(wt.tab), cumsum(round(wt.tab/sum(wt.tab)*100,2))))
names(wtd.freq) <- c("Frequency", "Percent", "Cumulative Frequency", "Cumulative Percent")
wtd.title <- paste('2016 NIS PUBLIC USE FILE', 
                   'WEIGHTED FREQUENCIES (EXCLUDING TERRITORIES)',
                   var_label(WT.VAR), sep = "\n")
var_label(wtd.freq) <- wtd.title

print(wtd.freq)


