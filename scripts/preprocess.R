library(dplyr)
library(ggplot2)
library(here)

diagTable = read.csv(here("data-raw","download","Primary_Diagnosis.csv"), header=T, stringsAsFactors=FALSE)
surgTable = read.csv(here("data-raw","download","Surgery_for_Parkinson_Disease.csv"), header=T, stringsAsFactors=FALSE)
centerTable = read.csv(here("data-raw","download","Center-Subject_List.csv"), header=T, stringsAsFactors=FALSE)
drug_init = read.csv(here("data-raw","download","Initiation_of_PD_Medication-_incidents.csv"), header=T, stringsAsFactors=FALSE)
pd_diag= read.csv(here("data-raw","download", "PD_Features.csv"), header=T, stringsAsFactors=FALSE)

drug_init <-drug_init %>% select(PATNO, INITMDDT)
pd_diag <- pd_diag %>% select(PATNO, PDDXDT)
surgTable <- surgTable %>% filter(PDSURG == 1 & (PDSURGTP == 1 | PDSURGTP == 2)) %>% select(PATNO, PDSURG, PDSURGDT, PDSURGTP)
surgTable[,1] <- sapply(surgTable[,1], as.numeric)

# Join the date of diagnosis table
pd_df <-  diagTable %>% 
  filter(PRIMDIAG == 1) %>% 
  filter(!(EVENT_ID %in% c("PW", "ST", "SC"))) %>% 
  select(PATNO, EVENT_ID, PRIMDIAG, ORIG_ENTRY) %>% 
  left_join(centerTable, c("PATNO"))

pd_df <-  pd_df %>% left_join(pd_diag, c("PATNO"))
pd_df <- pd_df %>% left_join(drug_init, c("PATNO"))
pd_df <- pd_df %>% left_join(surgTable, c("PATNO"))

# Need a better way of dealing with duplicate files
pd_df <- pd_df %>% distinct(PATNO, EVENT_ID, .keep_all = TRUE)

pd_df <- pd_df %>% 
  mutate(dateDX = as.Date(paste0(PDDXDT, "-01"), "%b-%y-%d")) %>% 
  mutate(dateDrug =  as.Date(paste0(INITMDDT, "-01"), "%b-%y-%d")) %>% 
  mutate(dateSurg =  as.Date(paste0(PDSURGDT, "-01"), "%b-%y-%d")) %>%
  mutate(dateVisit = as.Date(paste0(ORIG_ENTRY, "-01"), "%m/%Y-%d")) %>%
  arrange(PATNO, dateVisit)

date_diff <- pd_df %>%
  mutate(visit_time= as.numeric(dateVisit - dateDX)) %>%
  mutate(drug_time= as.numeric(dateDrug - dateDX)) %>%
  mutate(surg_time= as.numeric(dateSurg - dateDX))

# Number of Disdattinct Patients with PD (PRIMDIAG = 1) (n = 413)
nrow(distinct(pd_df,PATNO))

# Number of patients who got DBS (n = 13)
nrow(distinct(date_diff %>% filter(PDSURG == 1),PATNO) )

# Calculate the UPDRS totals for each UPDRS part

updrs1Table = read.csv(here("data-raw","download","MDS_UPDRS_Part_I.csv"), header=T, stringsAsFactors=FALSE)
updrs2Table = read.csv(here("data-raw","download","MDS_UPDRS_Part_II__Patient_Questionnaire.csv"), header=T, stringsAsFactors=FALSE)
updrs3Table = read.csv(here("data-raw","download","MDS_UPDRS_Part_III__Post_Dose_.csv"), header=T, stringsAsFactors=FALSE)
updrs4Table= read.csv(here("data-raw","download", "MDS_UPDRS_Part_IV.csv"), header=T, stringsAsFactors=FALSE)

updrs1Table <- updrs1Table %>% mutate(total1=NP1COG + NP1HALL + NP1DPRS + NP1ANXS + NP1APAT + NP1DDS ) %>% 
  select(PATNO, EVENT_ID, total1) #Max score for part I is 24
updrs2Table <- updrs2Table %>% mutate(total2=NP2SPCH + NP2SALV + NP2SWAL +NP2EAT + NP2DRES + NP2HYGN + NP2HWRT+ NP2HOBB+ NP2TURN+ NP2TRMR + NP2RISE+ NP2WALK+ NP2FREZ) %>%
  select(PATNO, EVENT_ID, total2) #Max score for part I is 24
updrs3Table <- updrs3Table %>% mutate(total3=rowSums(select(.,contains("NP3")))) %>% 
  select(PATNO, EVENT_ID, total3) #Max score for part I is 24
updrs4Table <- updrs4Table %>% mutate(total4=NP4WDYSK + NP4DYSKI + NP4OFF +NP4FLCTI + NP4FLCTX + NP4DYSTN) %>% 
  select(PATNO, EVENT_ID, total4) #Max score for part I is 24
medTable = read.csv(here("data-raw","Use_of_PD_Medication.csv"), header=T)
updrsTable1 = read.csv(here("data-raw","MDS_UPDRS_PartI_score.csv"), header=T)
pd_initiation = read.csv(here("data-raw","Initiation_of_PD_Medication.csv"), header=T)
pd_diag= read.csv(here("data-raw","pd_features01.csv"), header=T)
pd_feat =read.csv(here("data-raw","pd_features.csv"), header=T)
demo = read.csv(here("data-raw","Demographics.csv"), header=T)
prodromal = read.csv(here("data-raw","Prodromal_Diagnostic_Questionnaire.csv"), header = T)
# Potential features to look out for 
gen_neuro = read.csv("ClinicalData/General_Neurological_Exam.csv")
gen_phys = read.csv("ClinicalData/General_Physical_Exam.csv")






# Need to convert each visit into a month, probably a better way to do this, but for now this is what I could figure out

joined3<- left_join(joined2, pd_diag, by = c("subj_id" = "SUBJ_ID")) %>% left_join(pd_feat, by = c("subj_id" = "SUBJ_ID")) %>% left_join(demo, by = c("subj_id" = "SUBJ_ID")) %>% left_join(pd_initiation, by = c("subj_id" = "SUBJ_ID"))
joined3 <- joined3 %>% mutate( dateDX = as.Date(paste0(Date.Of.Parkinson.Disease.Diagnosis..PDDXDT., "-01"), "%y-%b-%d")) %>% mutate( dateDrug = as.Date(paste0(INITMDDT, "-01"), "%b_%Y-%d")) %>% mutate(timeDiff = dateDrug - dateDX)
ggplot(joined3, aes(visnum, updrsT,col= primdiag )) + geom_line(alpha = 0.5,aes(group= factor(subj_id)))
require(RColorBrewer)
mycolors = c(brewer.pal(name="Dark2", n = 100))
