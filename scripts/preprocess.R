library(dplyr)
library(ggplot2)
library(here)

diagTable = read.csv(here("data-raw","download","Primary_Diagnosis.csv"), header=T, stringsAsFactors=FALSE)
surgTable = read.csv(here("data-raw","download","Surgery_for_Parkinson_Disease.csv"), header=T, stringsAsFactors=FALSE)
centerTable = read.csv(here("data-raw","download","Center-Subject_List.csv"), header=T, stringsAsFactors=FALSE)
drug_init = read.csv(here("data-raw","download","Initiation_of_PD_Medication-_incidents.csv"), header=T, stringsAsFactors=FALSE)
pd_diag= read.csv(here("data-raw","download", "PD_Features.csv"), header=T, stringsAsFactors=FALSE)
demog= read.csv(here("data-raw","download", "Screening___Demographics.csv"), header=T, stringsAsFactors=FALSE) %>%
  select(PATNO, BIRTHDT, GENDER)

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
# pd_df <- pd_df %>% distinct(PATNO, EVENT_ID, .keep_all = TRUE)

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
  select(PATNO, EVENT_ID, total2) #Max score for part II is 24
updrs3Table <- updrs3Table %>% mutate(total3=rowSums(select(.,contains("NP3")))) %>% 
  select(PATNO, EVENT_ID, total3) #Max score for part III is 24
updrs4Table <- updrs4Table %>% mutate(total4=NP4WDYSK + NP4DYSKI + NP4OFF +NP4FLCTI + NP4FLCTX + NP4DYSTN) %>% 
  select(PATNO, EVENT_ID, total4) #Max score for part IV is 24

final_df <- date_diff %>%
  left_join(updrs1Table, c("PATNO", "EVENT_ID")) %>%
  left_join(updrs2Table, c("PATNO", "EVENT_ID")) %>%
  left_join(updrs3Table, c("PATNO", "EVENT_ID")) %>%
  left_join(updrs4Table, c("PATNO", "EVENT_ID"))

# Convert NAs in these columns to zeros
final_df[,c("total1", "total2", "total3", "total4")][is.na(final_df[c("total1", "total2", "total3", "total4")])] <- 0

final_df <- final_df %>%
  mutate(updrsT = total1 + total2 + total3 + total4)

final_df[,c("PDSURG","PDSURGTP")][is.na(final_df[c("PDSURG","PDSURGTP")])] <- 0

final_df <- final_df %>%
  mutate(PATNO = factor(PATNO)) %>%
  mutate(CNO = factor(CNO)) %>%
  left_join(demog, c("PATNO")) %>%
  mutate(PATNO = factor(PATNO))
  

final_df <- final_df %>%
  mutate(age = as.numeric(dateDX - as.Date(paste0(BIRTHDT, "-01-01"), "%Y-%m-%d")) / 365)


final_df <- final_df %>%
  mutate(PDSURG = factor(PDSURG))
# Need a better way of dealing with duplicate files
final_df <- final_df %>% distinct(PATNO, EVENT_ID, .keep_all = TRUE)
ggplot(final_df, aes(visit_time, updrsT,col= PDSURG )) + geom_line(alpha = 0.5,aes(group= factor(PATNO))) + ggtitle("UPDRS totals over time")
ggplot(final_df, aes(visit_time, total1,col= PDSURG )) + geom_line(alpha = 0.5,aes(group= factor(PATNO))) + ggtitle("UPDRS PART I totals over time")
ggplot(final_df, aes(visit_time, total2,col= PDSURG )) + geom_line(alpha = 0.5,aes(group= factor(PATNO)))+ ggtitle("UPDRS PART II totals over time")
ggplot(final_df, aes(visit_time, total3,col= PDSURG )) + geom_line(alpha = 0.5,aes(group= factor(PATNO)))+ ggtitle("UPDRS PART III totals over time")

ggplot(final_df, aes(visit_time, updrsT,col= age )) + geom_line(alpha = 0.5,aes(group= factor(PATNO))) + ggtitle("UPDRS totals over time")
ggplot(final_df, aes(visit_time, updrsT,col= factor(GENDER) )) + geom_line(alpha = 0.5,aes(group= factor(PATNO))) + ggtitle("UPDRS totals over time")


# Need to convert each visit into a month, probably a better way to do this, but for now this is what I could figure out

joined3<- left_join(joined2, pd_diag, by = c("subj_id" = "SUBJ_ID")) %>% left_join(pd_feat, by = c("subj_id" = "SUBJ_ID")) %>% left_join(demo, by = c("subj_id" = "SUBJ_ID")) %>% left_join(pd_initiation, by = c("subj_id" = "SUBJ_ID"))
joined3 <- joined3 %>% mutate( dateDX = as.Date(paste0(Date.Of.Parkinson.Disease.Diagnosis..PDDXDT., "-01"), "%y-%b-%d")) %>% mutate( dateDrug = as.Date(paste0(INITMDDT, "-01"), "%b_%Y-%d")) %>% mutate(timeDiff = dateDrug - dateDX)
ggplot(joined3, aes(visnum, updrsT,col= primdiag )) + geom_line(alpha = 0.5,aes(group= factor(subj_id)))
require(RColorBrewer)
mycolors = c(brewer.pal(name="Dark2", n = 100))
