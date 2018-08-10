
##############
###R code to transpose and join AIMS, ESIS, participation fees, and premiums#####
###############


##1. 

###install packages if necessary
#readin the flat files from AIMS; 
###specify claims as the incident records with a CA number; 

#specify the packages of interest
packages = c("dplyr","stringr","tibble","reshape", "data.table","stringi", "tidyr")

#use this function to check if each package is on the local machine
#if a package is installed, it will be loaded
#if any are not, the missing package(s) will be installed and loaded
package.check <- lapply(packages, FUN = function(x) {
  if (!require(x, character.only = TRUE)) {
    install.packages(x, dependencies = TRUE)
    library(x, character.only = TRUE)
  }
})

#verify they are loaded
search()



##if testing on server, use this path set:
setwd("\\\\armsinc.local\\files\\ARMS\\Tableau\\Marsh_Working\\Old")
##if testing on personal machine, use this path set: 
##setwd("I:/009 IT/#Tableau Dashboards Secured Folder/data/AIMS/curr_files")

claim_transact = read.csv("ARMS_TRANS_CURRENT.CSV")
allincs = read.csv("ARMS_cLAIMS_CURRENT.csv")
tstamp = format(Sys.Date(), "%d%b%y")

rawclaims = allincs %>% select(CLAIM_ID, CLAIM, AFFILIATE_CODE,AFFILIATE_NAME, CENTER_STATE, OCC_STATUS, POLICY_DT,INCIDENT_DATE,CA_NUM, STATUS, CLOSE_DT,OUTCOME_DESC,TYPE_OF_SERVICE_DESC,TYPE_OF_SERVICE_DETAIL_DESC,INCIDENT_TYPE_DETAIL_DESC,CAUSE_DESC, COVERAGE) %>% filter(!is.na(CA_NUM) & POLICY_DT>0)# %>% rename(CLAIMSTATUS = STATUS)
rawclaims$COVERAGE = as.character(rawclaims$COVERAGE)
rawclaims =  rawclaims %>% dplyr::rename(CLAIMSTATUS = STATUS)
rawclaims$COVERAGE = ifelse(rawclaims$COVERAGE == "UMB", "GL", rawclaims$COVERAGE)

rawclaims$CLAIM = as.character(rawclaims$CLAIM)

##2. check that there are no WC claims in the AIMS data;

claim_policycov = rawclaims %>% group_by(COVERAGE,POLICY_DT) %>% summarise(claimcnt = n())
wc_aimscnt = nrow(claim_policycov %>% filter(COVERAGE == "WC") %>% summarise( n = n()))
##will abort of there are WC claims coming from the AIMS source.
stopifnot(wc_aimscnt == 0)


##3. clean up claims transactions;

claim_transact$CLAIM_NUMBER = as.character(claim_transact$CLAIM_NUMBER)
transact_formerge = claim_transact %>% select(-CLAIM_ID) %>% dplyr::rename(CLAIM = CLAIM_NUMBER )

rawclaims$rname <- rownames(rawclaims)

claim_transact2 = merge (rawclaims, transact_formerge, by = "CLAIM")
claim_transact2$TRANS_DT = as.vector(claim_transact2$TRANS_DT)

claim_transact2 = claim_transact2 %>% mutate(TRANS_DT = as.Date(TRANS_DT,"%d-%B-%y"))
claim_transact2 = claim_transact2 %>% mutate(INCIDENT_DATE = as.Date(INCIDENT_DATE,"%d-%B-%y"))
claim_transact2 = claim_transact2 %>% mutate(CLOSE_DT = as.Date(CLOSE_DT,"%m/%d/%Y"))

##4. 
##readin WC claims and match to the fields in the AIMS claim file;
#MUST HAVE ACCESS TO FOLDERS BELOW!!!!

#setwd("I:/009 IT/#Tableau Dashboards Secured Folder/data/ESIS")
setwd("\\\\armsinc.local\\files\\Arms\\009 IT\\#Tableau Dashboards Secured Folder\\data\\ESIS")

wc_rawclaim = read.csv("WC_claims_current.csv",sep = ",", dec = ".", stringsAsFactors = FALSE)
wc_claim = wc_rawclaim %>% dplyr::rename(CLAIM = Claim_Number) %>% mutate(CLAIMSTATUS = ifelse(Status == "CLOSED","F","O"), POLICY_DT = as.numeric(format(as.Date(Date_Reported, format = "%m/%d/%Y"),"%Y")), AFFILIATE_CODE = substr(Location_Site,8,12),COVERAGE = "WC", OCC_STATUS = ifelse(Litigation_Ind == "Yes","SUIT","CLM"),COVERAGE = "WC") %>%  select(CLAIM, CLAIMSTATUS,POLICY_DT,  AFFILIATE_CODE,State,OCC_STATUS,Cause,COVERAGE,Date_Closed_Reopened,Date_Reported,
                                                                                                                                                                                                                                                                                                                                                                  medbi_pay, comp_pay,exp_pay,totpay,recover_pay,net_pay,net_outstand,medbi_incurred,comp_incurred,incurr_exp,incurr,recover_incurr,net_incurr) %>% dplyr::rename(CENTER_STATE = State, CLOSE_DT = Date_Closed_Reopened,CAUSE_DESC = Cause, INCIDENT_DATE = Date_Reported)

wc_rawsum = wc_claim %>% summarise(medbi_pay = sum(medbi_pay), totpay = sum(totpay), comp_pay = sum(comp_pay), exp_pay = sum(exp_pay))


wc_claim$AFFILIATE_CODE = ifelse(wc_claim$AFFILIATE_CODE == "92000", "2000-00",wc_claim$AFFILIATE_CODE)
wc_claim$AFFILIATE_CODE = ifelse(wc_claim$AFFILIATE_CODE == "90840", "90860",wc_claim$AFFILIATE_CODE)


wc_claim = wc_claim %>% mutate(CLOSE_DT = as.Date(CLOSE_DT,"%m/%d/%Y"))

wc_claim = wc_claim %>% mutate(INCIDENT_DATE =  as.Date(INCIDENT_DATE,"%m/%d/%Y"), TRANS_DT = ifelse(CLAIMSTATUS == "F",CLOSE_DT,INCIDENT_DATE))

labels = c(
  'medbIcomp_pay',
  'medbi_pay',
  'comp_pay',
  'medbIcomp_res',
  'medbi_oustand',
  'comp_outstand',
  'exp_pay',
  'exp_outstand',
  'exp_res',
  'ind_pay',
  'ind_res',
  'comm_exp',
  'comm_exp_res',
  'recover_pay',
  'deduct_pay',
  'deduct_res',
  'comm_deduct_pay',
  'comm_deduct_res',
  'comm_ind_pay',
  'comm_ind_res'
)

codes = c('1P10',
          '1M10',
          '1C10',
          '1R1R',
          '2M2R',
          '2C2R',
          '2P20',
          '2S2R',
          '2R2R',
          '3P30',
          '3R3R',
          '4P40',
          '4R4R',
          '6Q60',
          '6P60',
          '6R6R',
          '7P70',
          '7R7R',
          '9P90',
          '9R9R'
)


sourcetype = c(
  'AIMS',
  'ESIS',
  'ESIS',
  'ESIS',
  'ESIS',
  'ESIS',
  'AIMS',
  'ESIS',
  'AIMS',
  'AIMS',
  'AIMS',
  'ESIS',
  'AIMS',
  'ESIS',
  'AIMS',
  'AIMS',
  'AIMS',
  'AIMS',
  'AIMS',
  'AIMS'
  
)
codelbls = cbind(codes,labels,sourcetype)


transactcodes = data.frame(codes,labels, stringsAsFactors = FALSE)

wc_claim2 <- reshape::melt(wc_claim, id=c("CLAIM","CLAIMSTATUS","POLICY_DT","AFFILIATE_CODE","CENTER_STATE","OCC_STATUS", "CAUSE_DESC","COVERAGE","CLOSE_DT","INCIDENT_DATE","TRANS_DT"))

class(wc_claim2$value)

wc_claim_sums = wc_claim2 %>% group_by(variable) %>% summarise(obscnt = n(), totdlr = sum(value))

to_exclude = c("incurr","net","tot")

wc_claimpaid_tranpose = wc_claim2 %>% filter(!grepl("incurr", variable, fixed = TRUE) & !grepl("net", variable, fixed = TRUE) & !grepl("tot", variable, fixed = TRUE)) %>% mutate(source = "ESIS",PAYER = "ESIS", SEQ = -1,TRANS_DT = CLOSE_DT, CA_NUM = "NNN",TYPE_OF_SERVICE_DESC = NA,TYPE_OF_SERVICE_DETAIL_DESC = NA,INCIDENT_TYPE_DETAIL_DESC = NA,OUTCOME_DESC = NA, TRANSACTION_STATUS = "F") %>% dplyr::rename(AMOUNT = value, TRANS_TYPE_LBL = variable) 

##apply the code labels based on the values in variable

setkey(setDT(wc_claimpaid_tranpose),TRANS_TYPE_LBL)
setkey(setDT(transactcodes),labels)

wc_claimpaid_tranpose[transactcodes, TRANS_TYPE := i.codes]

##add in affiliate names;
affillist = claim_transact2 %>% select(AFFILIATE_CODE,AFFILIATE_NAME) %>% group_by(AFFILIATE_CODE,AFFILIATE_NAME) %>% summarise(obscnt = n()) %>% select(-obscnt)

setkey(setDT(affillist),AFFILIATE_CODE)
setkey(setDT(wc_claimpaid_tranpose),AFFILIATE_CODE)

wc_claimpaid_tranpose[affillist,AFFILIATE_NAME:= i.AFFILIATE_NAME]

setkey(setDT(claim_transact2),TRANS_TYPE)
setkey(setDT(transactcodes),codes)

claim_transact2[transactcodes,TRANS_TYPE_LBL:= i.labels]

##4. join the WC data to AIMS data;



claim_transact3 = claim_transact2 %>% select(-rname, -CLAIM_ID, -TRANS_ID) %>% mutate(source = "AIMS")

wc_claimpaid_tranpose = wc_claimpaid_tranpose %>% mutate_if(is.factor, as.character)

claim_transact3 = claim_transact3 %>% mutate_if(is.factor, as.character)

wc_claimpaid_tranpose$POLICY_DT = as.integer(wc_claimpaid_tranpose$POLICY_DT)
wc_claimpaid_tranpose$OUTCOME_DESC = as.character(wc_claimpaid_tranpose$OUTCOME_DESC)
wc_claimpaid_tranpose$TYPE_OF_SERVICE_DESC = as.character(wc_claimpaid_tranpose$TYPE_OF_SERVICE_DESC)
wc_claimpaid_tranpose$TYPE_OF_SERVICE_DETAIL_DESC = as.character(wc_claimpaid_tranpose$TYPE_OF_SERVICE_DETAIL_DESC)
wc_claimpaid_tranpose$INCIDENT_TYPE_DETAIL_DESC = as.character(wc_claimpaid_tranpose$INCIDENT_TYPE_DETAIL_DESC)
wc_claimpaid_tranpose$INCIDENT_DATE = as.Date(wc_claimpaid_tranpose$INCIDENT_DATE,"%Y-%m-%d")
wc_claimpaid_tranpose$TRANS_DT = as.Date(wc_claimpaid_tranpose$TRANS_DT,"%Y-%m-%d")
wc_claimpaid_tranpose$CLOSE_DT = as.Date(wc_claimpaid_tranpose$CLOSE_DT,"%Y-%m-%d")
wc_claimpaid_tranpose$AMOUNT = as.integer(wc_claimpaid_tranpose$AMOUNT,digits = 2)

wc_claimpaid_tranpose$SEQ = as.numeric(wc_claimpaid_tranpose$SEQ)
claim_transact3$SEQ = as.numeric(claim_transact3$SEQ)

all_claim_transact = union(claim_transact3,wc_claimpaid_tranpose)

all_claim_transact <- all_claim_transact %>% mutate(PROGCOVERAGE = if_else(COVERAGE == "WC", "NWP",if_else(COVERAGE == "PROP","NPP","NIP")))


##5. sum participation fees to the affiliate level;

setwd("\\\\armsinc.local\\files\\Arms\\009 IT\\Megan\\AIMS\\finance")

#setwd("I:/009 IT/Megan/AIMS/finance")
affil_prems = read.csv("master_participationfees.csv", stringsAsFactors=FALSE )
affil_deduct = read.csv("WC_deductible.csv",stringsAsFactors=FALSE )

affil_prems2 <- reshape::melt(affil_prems, id=c("Affiliate_ID","Affiliate_Name","COVERAGE","feetype"))

affil_prems2$Affiliate_ID = ifelse(affil_prems2$Affiliate_ID == "1000", "3000-00", affil_prems2$Affiliate_ID)

affil_deduct2 <- reshape::melt(affil_deduct, id=c("Affiliate_ID","Affiliate_Name","COVERAGE","feetype"))

affil_deduct2 = affil_deduct2 %>% dplyr::rename(deductvalue = value) %>% select(-feetype)

affil_prems3 = dplyr::left_join(affil_prems2,affil_deduct2, by = c("Affiliate_ID" = "Affiliate_ID","Affiliate_Name" = "Affiliate_Name", "COVERAGE" = "COVERAGE", "variable" = "variable"))

affil_prems3$value = as.numeric(affil_prems3$value, digits = 2)

affil_prems3 <- affil_prems3 %>%
  mutate(value = ifelse(is.na(value),0,value),deductvalue = ifelse(is.na(deductvalue),0,deductvalue))

affil_prems3$netpremium = affil_prems3$value + affil_prems3$deductvalue

affil_prems3 <- affil_prems3 %>% mutate(POLICY_DT = substr(variable,8,12)) %>% select(-feetype,-variable -Affiliate_Name) %>% dplyr::rename(AFFILIATE_CODE = Affiliate_ID,premium = value)

affil_prems3 <- affil_prems3 %>% mutate(PROGCOVERAGE = if_else(COVERAGE == "WC", "NWP",if_else(COVERAGE == "PROP","NPP","NIP"))) 

##6. sum premiums to the coverage level;

setwd("\\\\armsinc.local\\files\\Arms\\009 IT\\Megan\\AIMS\\finance")

#setwd("I:/009 IT/Megan/AIMS/finance")
carrierprems = read.csv("carrierprems.csv", stringsAsFactors = FALSE)
carrierprems$COVERAGE = ifelse(carrierprems$COVERAGE == "UMB", "GL",carrierprems$COVERAGE)

carrierdat = carrierprems %>% filter(PremiumType == "Premium" & InsurType == "Commercial") %>% select(Policy_Year,COVERAGE,carrier_premium, carrier_deductible) %>% dplyr::rename(POLICY_DT = Policy_Year) %>% group_by(POLICY_DT,COVERAGE) %>% mutate(carrier_deductible == 0) %>% summarise(carrier_deductible = sum(carrier_deductible),carrier_premium = sum(carrier_premium))

carrierdat$POLICY_DT = as.character(carrierdat$POLICY_DT)
carrier_covlist = carrierdat %>% group_by(COVERAGE) %>% summarise(n = n())

##7. Join the claims and finance data together: 

all_claim_transact$POLICY_DT = as.character(all_claim_transact$POLICY_DT)
##this is where the first error is, find out what the issue is
setkey(setDT(affillist),AFFILIATE_CODE)
setkey(setDT(affil_prems3),AFFILIATE_CODE)

affil_prems3[affillist,AFFILIATE_NAME:= i.AFFILIATE_NAME]

all_claim_transact2 = full_join(all_claim_transact,affil_prems3, by = c
                                ("POLICY_DT" ="POLICY_DT", 
                                  "PROGCOVERAGE" = "PROGCOVERAGE",
                                  "COVERAGE" = "COVERAGE", 
                                  "AFFILIATE_CODE"= "AFFILIATE_CODE",
                                  "AFFILIATE_NAME" ="AFFILIATE_NAME"))

all_claim_transact2 = left_join(all_claim_transact2, carrierdat, by = c("POLICY_DT" = "POLICY_DT","COVERAGE" = "COVERAGE"))

all_claim_transact2 = all_claim_transact2 %>% mutate_if(is.numeric, funs(ifelse(is.na(.), 0, .)))

##to output to the I: Drive - 
#write.csv(all_claim_transact2, "I:/009 IT/#Tableau Dashboards Secured Folder/data/AIMS/curr_files/claim_payouts_curr.csv", row.names = FALSE)
#write.csv(affil_prems3, "I:/009 IT/#Tableau Dashboards Secured Folder/data/AIMS/curr_files/premiums_curr.csv", row.names = FALSE)
#write.csv(carrierdat, "I:/009 IT/#Tableau Dashboards Secured Folder/data/AIMS/curr_files/carrier_premiums_curr.csv", row.names = FALSE)

##to output to the server
write.csv(all_claim_transact2, "\\\\armsinc.local\\files\\ARMS\\Tableau\\Marsh\\claim_payouts_curr.csv", row.names = FALSE)
write.csv(affil_prems3, "\\\\armsinc.local\\files\\ARMS\\Tableau\\Marsh\\premiums_curr.csv", row.names = FALSE)
write.csv(carrierdat, "\\\\armsinc.local\\files\\ARMS\\Tableau\\Marsh\\carrier_premiums_curr.csv", row.names = FALSE)



