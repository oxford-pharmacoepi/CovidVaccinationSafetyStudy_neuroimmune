# printing numbers with 1 decimal place and commas 
nice.num<-function(x) {
  trimws(format(round(x,1),
         big.mark=",", nsmall = 1, digits=1, scientific=FALSE))}
# printing numbers with 2 decimal place and commas 
nice.num2<-function(x) {
  trimws(format(round(x,2),
         big.mark=",", nsmall = 2, digits=2, scientific=FALSE))}
# for counts- without decimal place
nice.num.count<-function(x) {
  trimws(format(x,
         big.mark=",", nsmall = 0, digits=0, scientific=FALSE))}


# data -----
load(here("data", "Network.patient.characteristcis.RData"))
load(here("data", "Network.Survival.summary.RData"))


load(here("data", "Network.IR.RData"))
#### NOT REMOVE events < 5 #####
# Network.IR<-Network.IR %>% 
#   filter(events>=5)

load(here("data", "COVID_PCR_background.pop.IRR.RData"))
# COVID_PCR_background.pop.IRR<-COVID_PCR_background.pop.IRR %>% 
#   filter(events_target>=5)

load(file = here("data", "COVID_PCR_background.pop.visit.IRR.RData"))
# COVID_PCR_background.pop.visit.IRR<-COVID_PCR_background.pop.visit.IRR %>% 
#   filter(events_target>=5)

# IRRS
load(here("data", "AZ_background.pop.IRR.RData"))
AZ_background.pop.IRR<-AZ_background.pop.IRR   # %>% 
#  filter(events_target>=5)
load(here("data", "AZ_background.pop.visit.IRR.RData"))
AZ_background.pop.visit.IRR<-AZ_background.pop.visit.IRR   # %>% 
#  filter(events_target>=5)
load(here("data", "Pf_background.pop.IRR.RData"))
Pf_background.pop.IRR<-Pf_background.pop.IRR  #  %>% 
#  filter(events_target>=5)
load(here("data", "Pf_background.pop.visit.IRR.RData"))
Pf_background.pop.visit.IRR<-Pf_background.pop.visit.IRR  #  %>% 
#  filter(events_target>=5)

# standardised results
load(here("data", "COVID_PCR_background.pop.IRS.RData"))
COVID_PCR_background.pop.IRS<-COVID_PCR_background.pop.IRS #%>% 
#  filter(events_target>=5)
load(here("data", "COVID_PCR_background.pop.visit.IRS.RData"))
COVID_PCR_background.pop.visit.IRS<-COVID_PCR_background.pop.visit.IRS #%>% 
#  filter(events_target>=5)
load(here("data", "AZ_background.pop.IRS.RData"))
AZ_background.pop.IRS<-AZ_background.pop.IRS #%>% 
#  filter(events_target>=5)
load(here("data", "AZ_background.pop.visit.IRS.RData"))
AZ_background.pop.visit.IRS<-AZ_background.pop.visit.IRS #%>% 
#  filter(events_target>=5)
load(here("data", "Pf_background.pop.IRS.RData"))
Pf_background.pop.IRS<-Pf_background.pop.IRS #%>% 
#  filter(events_target>=5)
load(here("data", "Pf_background.pop.visit.IRS.RData"))
Pf_background.pop.visit.IRS<-Pf_background.pop.visit.IRS #%>% 
#  filter(events_target>=5)


## Table 1 -----
table.data<-Network.patient.characteristcis %>%
  filter(prior.obs.required=="Yes") %>% 
  filter(pop.type=="All") %>% 
  filter(age_gr2=="All") %>% 
  select("var", 
         "Study population",
         "pop") %>% 
  filter(pop!="General population (index date: 1st December)")

table.data<-table.data %>% 
  pivot_wider(names_from = pop, 
              values_from = "Study population")

table.data<-bind_rows(
     table.data[c(1:10),],
    table.data[c(1),]  %>%
      mutate_at(vars(names(table.data)), ~ replace(., !is.na(.), NA)) %>%
      mutate(var="Comorbidities"),
     table.data[c(11:23),],
    table.data[c(1),]  %>%
      mutate_at(vars(names(table.data)), ~ replace(., !is.na(.), NA)) %>%
      mutate(var="Medication use (183 days prior to four days prior)"),
    table.data[c(24:35),]
    ) 


table.data<-table.data %>%
     mutate(var=ifelse(var=="Age.30 39", "Age: 30 to 39", var))  %>%
     mutate(var=ifelse(var=="Age.40 49", "Age: 40 to 49", var))  %>%
     mutate(var=ifelse(var=="Age.50 59", "Age: 50 to 59", var))  %>%
     mutate(var=ifelse(var=="Age.60 69", "Age: 60 to 69", var))  %>%
     mutate(var=ifelse(var=="Age.70 79", "Age: 70 to 79", var))  %>%
     mutate(var=ifelse(var=="Age.80u", "Age: 80 or older", var)) 


write.csv(table.data,
          here("SummariseResults", "table1.csv"))



## Table 1 30-44 -----
table.data<-Network.patient.characteristcis %>%
  filter(prior.obs.required=="Yes") %>% 
  filter(pop.type=="All") %>% 
  filter(age_gr2=="30-44") %>% 
  select("var", 
         "Study population",
         "pop") %>% 
  filter(pop!="General population (index date: 1st December)")

table.data<-table.data %>% 
  pivot_wider(names_from = pop, 
              values_from = "Study population")

table.data<-bind_rows(
     table.data[c(1:10),],
    table.data[c(1),]  %>%
      mutate_at(vars(names(table.data)), ~ replace(., !is.na(.), NA)) %>%
      mutate(var="Comorbidities"),
     table.data[c(11:23),],
    table.data[c(1),]  %>%
      mutate_at(vars(names(table.data)), ~ replace(., !is.na(.), NA)) %>%
      mutate(var="Medication use (183 days prior to four days prior)"),
    table.data[c(24:35),]
    ) 

table.data<-table.data %>%
     mutate(var=ifelse(var=="Copd", "COPD", var)) %>%
     mutate(var=ifelse(var=="Antiinflamatory and antirheumatic", "Non-steroidal anti-inflammatory drugs ", var)) %>%
     mutate(var=ifelse(var=="Coxibs", "Cox2 inhibitors ", var)) %>%
     mutate(var=ifelse(var=="Corticosteroids", "Systemic corticosteroids ", var)) %>%
     mutate(var=ifelse(var=="Antithrombotic", "Antithrombotic and anticoagulant therapies", var)) %>%
     mutate(var=ifelse(var=="Lipid modifying", "Lipid modifying agents ", var)) %>%
     mutate(var=ifelse(var=="Antineoplastic immunomodulating", "Antineoplastic and immunomodulating agents ", var)) %>%
     mutate(var=ifelse(var=="Hormonal contraceptives", "Hormonal contraceptives for systemic use ", var)) %>%
     mutate(var=ifelse(var=="Sex hormones modulators", "Sex hormones and modulators of the genital system", var))

table.data<-table.data %>%
     mutate(var=ifelse(var=="Age.30 39", "Age: 30 to 39", var))  %>%
     mutate(var=ifelse(var=="Age.40 49", "Age: 40 to 49", var))  %>%
     mutate(var=ifelse(var=="Age.50 59", "Age: 50 to 59", var))  %>%
     mutate(var=ifelse(var=="Age.60 69", "Age: 60 to 69", var))  %>%
     mutate(var=ifelse(var=="Age.70 79", "Age: 70 to 79", var))  %>%
     mutate(var=ifelse(var=="Age.80u", "Age: 80 or older", var)) 


write.csv(table.data,
          here("SummariseResults", "table1.30_44.csv"))




## Table 1 45-64 -----
table.data<-Network.patient.characteristcis %>%
  filter(prior.obs.required=="Yes") %>% 
  filter(pop.type=="All") %>% 
  filter(age_gr2=="45-64") %>% 
  select("var", 
         "Study population",
         "pop") %>% 
  filter(pop!="General population (index date: 1st December)")

table.data<-table.data %>% 
  pivot_wider(names_from = pop, 
              values_from = "Study population")

table.data<-bind_rows(
     table.data[c(1:10),],
    table.data[c(1),]  %>%
      mutate_at(vars(names(table.data)), ~ replace(., !is.na(.), NA)) %>%
      mutate(var="Comorbidities"),
     table.data[c(11:23),],
    table.data[c(1),]  %>%
      mutate_at(vars(names(table.data)), ~ replace(., !is.na(.), NA)) %>%
      mutate(var="Medication use (183 days prior to four days prior)"),
    table.data[c(24:35),]
    ) 

table.data<-table.data %>%
     mutate(var=ifelse(var=="Copd", "COPD", var)) %>%
     mutate(var=ifelse(var=="Antiinflamatory and antirheumatic", "Non-steroidal anti-inflammatory drugs ", var)) %>%
     mutate(var=ifelse(var=="Coxibs", "Cox2 inhibitors ", var))
table.data<-table.data %>%
     mutate(var=ifelse(var=="Age.30 39", "Age: 30 to 39", var))  %>%
     mutate(var=ifelse(var=="Age.40 49", "Age: 40 to 49", var))  %>%
     mutate(var=ifelse(var=="Age.50 59", "Age: 50 to 59", var))  %>%
     mutate(var=ifelse(var=="Age.60 69", "Age: 60 to 69", var))  %>%
     mutate(var=ifelse(var=="Age.70 79", "Age: 70 to 79", var))  %>%
     mutate(var=ifelse(var=="Age.80u", "Age: 80 or older", var)) 


write.csv(table.data,
          here("SummariseResults", "table1.45_64.csv"))





## Table 1 >=65 -----
table.data<-Network.patient.characteristcis %>%
  filter(prior.obs.required=="Yes") %>% 
  filter(pop.type=="All") %>% 
  filter(age_gr2==">=65") %>% 
  select("var", 
         "Study population",
         "pop") %>% 
  filter(pop!="General population (index date: 1st December)")

table.data<-table.data %>% 
  pivot_wider(names_from = pop, 
              values_from = "Study population")

table.data<-bind_rows(
     table.data[c(1:10),],
    table.data[c(1),]  %>%
      mutate_at(vars(names(table.data)), ~ replace(., !is.na(.), NA)) %>%
      mutate(var="Comorbidities"),
     table.data[c(11:23),],
    table.data[c(1),]  %>%
      mutate_at(vars(names(table.data)), ~ replace(., !is.na(.), NA)) %>%
      mutate(var="Medication use (183 days prior to four days prior)"),
    table.data[c(24:35),]
    ) 

table.data<-table.data %>%
     mutate(var=ifelse(var=="Age.30 39", "Age: 30 to 39", var))  %>%
     mutate(var=ifelse(var=="Age.40 49", "Age: 40 to 49", var))  %>%
     mutate(var=ifelse(var=="Age.50 59", "Age: 50 to 59", var))  %>%
     mutate(var=ifelse(var=="Age.60 69", "Age: 60 to 69", var))  %>%
     mutate(var=ifelse(var=="Age.70 79", "Age: 70 to 79", var))  %>%
     mutate(var=ifelse(var=="Age.80u", "Age: 80 or older", var)) 


write.csv(table.data,
          here("SummariseResults", "table1.65u.csv"))






## Table 2 , v1:  visit,  prior.obs.required=="Yes" -----
table.data<-bind_rows(AZ_background.pop.visit.IRS %>%
   mutate(pop="Vaccinated with ChAdOx1"),
Pf_background.pop.visit.IRS %>% 
   mutate(pop="Vaccinated with BNT162b2"),
COVID_PCR_background.pop.visit.IRS%>%
   mutate(pop="SARS-CoV-2 PCR positive test")) %>% 
  filter(outcome.name %in% 
           c("Myocarditis_Narrow","Myocarditis", 
             "Pericarditis_narrow", "Pericarditis")) %>% 
  mutate(outcome.name=str_remove_all(outcome.name,"\\(")) %>% 
  mutate(outcome.name=str_remove_all(outcome.name,"\\)")) %>% 
  mutate(outcome.name=str_replace_all(outcome.name,
    " - narrow",
    "")) %>% 
  mutate(outcome.name=factor(outcome.name,
                             levels=c(
                               "Myocarditis_Narrow",
                               "Myocarditis", 
                               "Pericarditis_narrow", 
                               "Pericarditis")))%>% 
  filter(target.time.window=="Full") %>% 
  filter(target.pop.type=="All")

# require hist for events except cvst
table.data<-bind_rows(
table.data %>%  filter(prior.obs.required=="No") %>%  filter(outcome.name=="Cerebral venous sinus thrombosis") , 
table.data %>%  filter(prior.obs.required=="Yes") %>%  filter(outcome.name!="Cerebral venous sinus thrombosis"))

table.data<-table.data %>% 
  # filter(isr.observed>=5) %>% 
    mutate(n_target=nice.num.count(n_target)) %>% 
    mutate(years_target=nice.num.count(years_target)) %>% 
    mutate(ir_100000.stand_p=
           paste0(nice.num((ir_100000.stand)),
                    " (",
                    nice.num((ir_100000_lower.stand)),
                    " to ",
                    nice.num((ir_100000_upper.stand)), ")")) %>% 
    mutate(irr_s=
           paste0(nice.num2((isr.sir)),
                    " (",
                    nice.num2((isr.sir_lower)),
                    " to ",
                    nice.num2((isr.sir_upper )), ")")) %>% 
  mutate(expected=nice.num(isr.expected)) %>% 
  mutate(observed=nice.num.count(isr.observed)) %>% 
  select( outcome.name,pop, 
          n_target,years_target,
          observed,expected,
          # ir_100000.stand_p,
           irr_s) %>% 
  arrange(outcome.name)

write.csv(table.data,
          here("SummariseResults", "table2.csv"))

tb2_v1 <- table.data %>% 
  mutate(visit.anchor="Yes", prior.obs.required="Yes")


## Table 2 , v2:  visit = no,  prior.obs.required=="Yes" -----
table.data<-bind_rows(AZ_background.pop.IRS %>%
                        mutate(pop="Vaccinated with ChAdOx1"),
                      Pf_background.pop.IRS %>% 
                        mutate(pop="Vaccinated with BNT162b2"),
                      COVID_PCR_background.pop.IRS%>%
                        mutate(pop="SARS-CoV-2 PCR positive test")) %>% 
  filter(outcome.name %in% 
           c("Myocarditis_Narrow","Myocarditis", 
             "Pericarditis_narrow", "Pericarditis")) %>% 
  mutate(outcome.name=str_remove_all(outcome.name,"\\(")) %>% 
  mutate(outcome.name=str_remove_all(outcome.name,"\\)")) %>% 
  mutate(outcome.name=str_replace_all(outcome.name,
                                      " - narrow",
                                      "")) %>% 
  mutate(outcome.name=factor(outcome.name,
                             levels=c(
                               "Myocarditis_Narrow",
                               "Myocarditis", 
                               "Pericarditis_narrow", 
                               "Pericarditis")))%>% 
  filter(target.time.window=="Full") %>% 
  filter(target.pop.type=="All")

# require hist for events except cvst
table.data<-  table.data %>%  filter(prior.obs.required=="Yes") 

table.data<-table.data %>% 
  # filter(isr.observed>=5) %>% 
  mutate(n_target=nice.num.count(n_target)) %>% 
  mutate(years_target=nice.num.count(years_target)) %>% 
  mutate(ir_100000.stand_p=
           paste0(nice.num((ir_100000.stand)),
                  " (",
                  nice.num((ir_100000_lower.stand)),
                  " to ",
                  nice.num((ir_100000_upper.stand)), ")")) %>% 
  mutate(irr_s=
           paste0(nice.num2((isr.sir)),
                  " (",
                  nice.num2((isr.sir_lower)),
                  " to ",
                  nice.num2((isr.sir_upper )), ")")) %>% 
  mutate(expected=nice.num(isr.expected)) %>% 
  mutate(observed=nice.num.count(isr.observed)) %>% 
  select( outcome.name,pop, 
          n_target,years_target,
          observed,expected,
          # ir_100000.stand_p,
          irr_s) %>% 
  arrange(outcome.name)

write.csv(table.data,
          here("SummariseResults", "table2_visitN_obsY.csv"))

tb2_v2 <- table.data %>% 
  mutate(visit.anchor="No", prior.obs.required="Yes")


## Table 2 , v3:  visit = Yes,  prior.obs.required=="No" -----
table.data<-bind_rows(AZ_background.pop.visit.IRS %>%
                        mutate(pop="Vaccinated with ChAdOx1"),
                      Pf_background.pop.visit.IRS %>% 
                        mutate(pop="Vaccinated with BNT162b2"),
                      COVID_PCR_background.pop.visit.IRS%>%
                        mutate(pop="SARS-CoV-2 PCR positive test")) %>% 
  filter(outcome.name %in% 
           c("Myocarditis_Narrow","Myocarditis", 
             "Pericarditis_narrow", "Pericarditis")) %>% 
  mutate(outcome.name=str_remove_all(outcome.name,"\\(")) %>% 
  mutate(outcome.name=str_remove_all(outcome.name,"\\)")) %>% 
  mutate(outcome.name=str_replace_all(outcome.name,
                                      " - narrow",
                                      "")) %>% 
  mutate(outcome.name=factor(outcome.name,
                             levels=c(
                               "Myocarditis_Narrow",
                               "Myocarditis", 
                               "Pericarditis_narrow", 
                               "Pericarditis")))%>% 
  filter(target.time.window=="Full") %>% 
  filter(target.pop.type=="All")

# require hist for events except cvst
table.data<-  table.data %>%  filter(prior.obs.required=="No") 

table.data<-table.data %>% 
  # filter(isr.observed>=5) %>% 
  mutate(n_target=nice.num.count(n_target)) %>% 
  mutate(years_target=nice.num.count(years_target)) %>% 
  mutate(ir_100000.stand_p=
           paste0(nice.num((ir_100000.stand)),
                  " (",
                  nice.num((ir_100000_lower.stand)),
                  " to ",
                  nice.num((ir_100000_upper.stand)), ")")) %>% 
  mutate(irr_s=
           paste0(nice.num2((isr.sir)),
                  " (",
                  nice.num2((isr.sir_lower)),
                  " to ",
                  nice.num2((isr.sir_upper )), ")")) %>% 
  mutate(expected=nice.num(isr.expected)) %>% 
  mutate(observed=nice.num.count(isr.observed)) %>% 
  select( outcome.name,pop, 
          n_target,years_target,
          observed,expected,
          # ir_100000.stand_p,
          irr_s) %>% 
  arrange(outcome.name)

write.csv(table.data,
          here("SummariseResults", "table2_visitY_obsN.csv"))

tb2_v3 <- table.data %>% 
  mutate(visit.anchor="Yes", prior.obs.required="No")



## Table 2 , v4:  visit = NO,  prior.obs.required=="No" -----
table.data<-bind_rows(AZ_background.pop.IRS %>%
                        mutate(pop="Vaccinated with ChAdOx1"),
                      Pf_background.pop.IRS %>% 
                        mutate(pop="Vaccinated with BNT162b2"),
                      COVID_PCR_background.pop.IRS%>%
                        mutate(pop="SARS-CoV-2 PCR positive test")) %>% 
  filter(outcome.name %in% 
           c("Myocarditis_Narrow","Myocarditis", 
             "Pericarditis_narrow", "Pericarditis")) %>% 
  mutate(outcome.name=str_remove_all(outcome.name,"\\(")) %>% 
  mutate(outcome.name=str_remove_all(outcome.name,"\\)")) %>% 
  mutate(outcome.name=str_replace_all(outcome.name,
                                      " - narrow",
                                      "")) %>% 
  mutate(outcome.name=factor(outcome.name,
                             levels=c(
                               "Myocarditis_Narrow",
                               "Myocarditis", 
                               "Pericarditis_narrow", 
                               "Pericarditis")))%>% 
  filter(target.time.window=="Full") %>% 
  filter(target.pop.type=="All")

# require hist for events except cvst
table.data<-  table.data %>%  filter(prior.obs.required=="No") 

table.data<-table.data %>% 
  # filter(isr.observed>=5) %>% 
  mutate(n_target=nice.num.count(n_target)) %>% 
  mutate(years_target=nice.num.count(years_target)) %>% 
  mutate(ir_100000.stand_p=
           paste0(nice.num((ir_100000.stand)),
                  " (",
                  nice.num((ir_100000_lower.stand)),
                  " to ",
                  nice.num((ir_100000_upper.stand)), ")")) %>% 
  mutate(irr_s=
           paste0(nice.num2((isr.sir)),
                  " (",
                  nice.num2((isr.sir_lower)),
                  " to ",
                  nice.num2((isr.sir_upper )), ")")) %>% 
  mutate(expected=nice.num(isr.expected)) %>% 
  mutate(observed=nice.num.count(isr.observed)) %>% 
  select( outcome.name,pop, 
          n_target,years_target,
          observed,expected,
          # ir_100000.stand_p,
          irr_s) %>% 
  arrange(outcome.name)

write.csv(table.data,
          here("SummariseResults", "table2_visitN_obsN.csv"))

tb2_v4 <- table.data %>% 
  mutate(visit.anchor="No", prior.obs.required="No")

table2_all <-  bind_rows(tb2_v1, tb2_v2,tb2_v3,tb2_v4)

write.csv(table2_all,
          here("SummariseResults", "table2_allversion.csv"))

##############################

## Table 2 all events by month-----
table.data<-bind_rows(AZ_background.pop.visit.IRS %>%
   mutate(pop="Vaccinated with ChAdOx1"),
Pf_background.pop.visit.IRS %>% 
   mutate(pop="Vaccinated with BNT162b2"),
COVID_PCR_background.pop.visit.IRS%>%
   mutate(pop="SARS-CoV-2 PCR positive test")) 

table.data<-table.data %>% 
  mutate(outcome.name=str_remove_all(outcome.name,"\\(")) %>% 
  mutate(outcome.name=str_remove_all(outcome.name,"\\)")) %>% 
  # mutate(outcome.name=str_replace_all(outcome.name,
  #   "with thrombocytopenia 10 days pre to 10 days post",
  #   "with thrombocytopenia")) %>% 
  # mutate(outcome.name=str_replace_all(outcome.name,
  #   " - narrow",
  #   "")) %>% 
  filter(target.time.window=="Full") %>% 
  filter(target.pop.type!="All")

# require hist for events except cvst
table.data<-bind_rows(
table.data %>%  filter(prior.obs.required=="No") %>%  filter(outcome.name=="Cerebral venous sinus thrombosis") , 
table.data %>%  filter(prior.obs.required=="Yes") %>%  filter(outcome.name!="Cerebral venous sinus thrombosis"))

table.data<-table.data %>% 
  # filter(isr.observed>=5) %>% 
    mutate(n_target=nice.num.count(n_target)) %>% 
    mutate(years_target=nice.num.count(years_target)) %>% 
    mutate(ir_100000.stand_p=
           paste0(nice.num((ir_100000.stand)),
                    " (",
                    nice.num((ir_100000_lower.stand)),
                    " to ",
                    nice.num((ir_100000_upper.stand)), ")")) %>% 
    mutate(irr_s=
           paste0(nice.num2((isr.sir)),
                    " (",
                    nice.num2((isr.sir_lower)),
                    " to ",
                    nice.num2((isr.sir_upper )), ")")) %>% 
  mutate(expected=nice.num(isr.expected)) %>% 
  mutate(observed=nice.num.count(isr.observed)) %>% 
  select( outcome.name,pop, target.pop.type,
          n_target,years_target,
          observed,expected,
          # ir_100000.stand_p,
           irr_s) %>% 
  arrange(outcome.name)

write.csv(table.data,
          here("SummariseResults", "table2_all.month.csv"))







## Table 2 all events, general pop 1st Jan-----
table.data<-bind_rows(AZ_background.pop.IRS %>%
   mutate(pop="Vaccinated with ChAdOx1"),
Pf_background.pop.IRS %>% 
   mutate(pop="Vaccinated with BNT162b2"),
COVID_PCR_background.pop.IRS%>%
   mutate(pop="SARS-CoV-2 PCR positive test")) 

table.data<-table.data %>% 
  mutate(outcome.name=str_remove_all(outcome.name,"\\(")) %>% 
  mutate(outcome.name=str_remove_all(outcome.name,"\\)")) %>% 
  # mutate(outcome.name=str_replace_all(outcome.name,
  #   "with thrombocytopenia 10 days pre to 10 days post",
  #   "with thrombocytopenia")) %>% 
  # mutate(outcome.name=str_replace_all(outcome.name,
  #   " - narrow",
  #   "")) %>% 
  filter(target.time.window=="Full") %>% 
  filter(target.pop.type=="All")

# no require hist 
table.data<-bind_rows(
table.data %>%  filter(prior.obs.required=="No") %>%  filter(outcome.name=="Cerebral venous sinus thrombosis") , 
table.data %>%  filter(prior.obs.required=="Yes") %>%  filter(outcome.name!="Cerebral venous sinus thrombosis"))

table.data<-table.data %>% 
  # filter(isr.observed>=5) %>% 
    mutate(n_target=nice.num.count(n_target)) %>% 
    mutate(years_target=nice.num.count(years_target)) %>% 
    mutate(ir_100000.stand_p=
           paste0(nice.num((ir_100000.stand)),
                    " (",
                    nice.num((ir_100000_lower.stand)),
                    " to ",
                    nice.num((ir_100000_upper.stand)), ")")) %>% 
    mutate(irr_s=
           paste0(nice.num2((isr.sir)),
                    " (",
                    nice.num2((isr.sir_lower)),
                    " to ",
                    nice.num2((isr.sir_upper )), ")")) %>% 
  mutate(expected=nice.num(isr.expected)) %>% 
  mutate(observed=nice.num.count(isr.observed)) %>% 
  select( outcome.name,pop, 
          n_target,years_target,
          observed,expected,
          # ir_100000.stand_p,
           irr_s) %>% 
  arrange(outcome.name)

write.csv(table.data,
          here("SummariseResults", "table2.gpop.date.csv"))


## Table 3 -----
table.data<-bind_rows(AZ_background.pop.IRS %>%
   mutate(pop="Vaccinated with ChAdOx1"),
Pf_background.pop.IRS %>% 
   mutate(pop="Vaccinated with BNT162b2"),
COVID_PCR_background.pop.visit.IRS%>%
   mutate(pop="SARS-CoV-2 PCR positive test")) %>% 
  filter(outcome.name %in% 
           c(
             "Myocarditis_Narrow",
             "Myocarditis", 
             "Pericarditis_narrow", 
             "Pericarditis" )) %>% 
  mutate(outcome.name=str_remove_all(outcome.name,"\\(")) %>% 
  mutate(outcome.name=str_remove_all(outcome.name,"\\)")) %>% 
  mutate(outcome.name=str_replace_all(outcome.name,
    "with thrombocytopenia 10 days pre to 10 days post",
    "with thrombocytopenia")) %>% 
  mutate(outcome.name=str_replace_all(outcome.name,
    " - narrow",
    "")) %>% 
  mutate(outcome.name=factor(outcome.name,
                             levels=c(
                               "Myocarditis_Narrow",
                               "Myocarditis", 
                               "Pericarditis_narrow", 
                               "Pericarditis"))) %>% 
  filter(prior.obs.required=="Yes") %>% 
  filter(target.pop.type=="All") %>% 
  filter(target.time.window=="Full")


table.data<-table.data %>% 
    mutate(n_target=nice.num.count(n_target)) %>% 
    mutate(years_target=nice.num.count(years_target)) %>% 
    mutate(ir_100000.stand_p=
           paste0(nice.num((ir_100000.stand)),
                    " (",
                    nice.num((ir_100000_lower.stand)),
                    " to ",
                    nice.num((ir_100000_upper.stand)), ")")) %>% 
    mutate(irr_s=
           paste0(nice.num2((isr.sir)),
                    " (",
                    nice.num2((isr.sir_lower)),
                    " to ",
                    nice.num2((isr.sir_upper )), ")")) %>% 
  mutate(expected=nice.num(isr.expected)) %>% 
  mutate(observed=nice.num.count(isr.observed)) %>% 
  select( outcome.name,pop, 
          n_target,years_target,
          observed,expected,
          # ir_100000.stand_p,
           irr_s) %>% 
  arrange(outcome.name)

write.csv(table.data,
          here("SummariseResults", "table3.csv"))

## Figure 2 -----
Network.IR <-  Network.IR %>% 
  mutate(pop = ifelse(pop== "Vaccinated with AstraZeneca", "Vaccinated with ChAdOx1",pop)) %>%
  mutate(pop = ifelse(pop== "Vaccinated with Pfizer-Biontech", "Vaccinated with BNT162b2",pop)) 

unique(Network.IR$pop)

# thrombosis 
plot.data<-Network.IR %>% 
  filter(events>=5) %>% 
  mutate(outcome.name=str_replace_all(outcome.name,
    " - narrow",
    "")) %>% 
  filter(outcome.name %in% 
           c(
             "Myocarditis_Narrow",
             "Myocarditis", 
             "Pericarditis_narrow", 
             "Pericarditis"
             )) %>% 
  mutate(outcome.name=ifelse(outcome.name=="Myocarditis_Narrow",
                             "Myocarditis\nNarrow",outcome.name
                             )) %>% 
  mutate(outcome.name=ifelse(outcome.name=="Pericarditis_narrow",
                             "Pericarditis\nnarrow", outcome.name
                             )) %>% 
  # mutate(outcome.name=ifelse(outcome.name=="Deep vein thrombosis",
  #                            "Deep vein\nthrombosis",outcome.name
  #                            )) %>% 
  # mutate(outcome.name=ifelse(outcome.name=="Venous thromboembolism",
  #                            "Venous\nthromboembolism",outcome.name
  #                            )) %>% 
  mutate(outcome.name=factor(outcome.name,
    levels=c(
              "Myocarditis\nNarrow",
              "Myocarditis", 
              "Pericarditis\nnarrow", 
              "Pericarditis"
                     ) )) %>% 
  filter(prior.obs.required=="Yes") %>% 
  filter(pop.type=="All") %>% 
  filter(time.window=="Full") %>% 
  filter(strata=="age_gr3_gender") %>% 
  filter(pop!="General population (index date: 1st December)") %>% 
  filter(pop!="SARS-CoV-2 PCR positive test")  %>% 
   mutate(pop=ifelse(pop=="General population (index date: first visit/ contact)", 
                     "General population", pop))


plot.data %>% 
  ggplot(aes(age_gr3,ir_100000, colour=pop)) +
  # facet_wrap(vars(outcome.name), #scales = "free_y",
  #            ncol=2)+
  facet_grid(outcome.name~ gender,scales = "free_y",  switch="y")+
  geom_errorbar(aes(ymin=ir_100000_lower,ymax=ir_100000_upper), width=0,
                position=position_dodge(width=0.6), size=0.75)+
  geom_point(position=position_dodge(width=0.6), size=2 )+ 
  theme_bw()+
  ylab("Incidence rate\nper 100,000 person-years\n")+
  xlab("Age")+
  theme(panel.spacing.x =  unit(1.5, "lines"),
          panel.spacing.y = unit(0.5, "lines"))+  
  scale_y_continuous(label=label_comma(accuracy= 1), trans='log2',
                     position = "right")+
  # scale_y_continuous(label=label_comma(accuracy= 1), #position = "right",
  #                    limits=c(0,NA))+
  theme_bw()+
  theme(panel.spacing = unit(0.6, "lines"),
        legend.title = element_blank(),
        axis.text=element_text(size=10),
        axis.title=element_text(size=12,face="bold"),
        strip.text = element_text(size=12, face="bold"),
        strip.text.y.left = element_text(angle = 0),
        strip.background = element_rect( fill="#f7f7f7"),
      #  axis.title.y.right =  element_text(angle = 0),
        legend.text=element_text(size=12), 
        legend.position = "top")+
   scale_color_manual(values=c("Vaccinated with ChAdOx1" = "#e41a1c",
                                 "Vaccinated with BNT162b2" = "#377eb8",
                               "General population (index date: 1st December)" = "#4daf4a",
                               "General population" = "#252525",
                               "SARS-CoV-2 PCR positive test"="#ff7f00",
                               "General population with visit" = "#69c967"))


ggsave(filename=here("SummariseResults", "IRs_myo.png"), 
       device = "png",
       width=12, height=8,  dpi=600)


## Figure 2 c -----
# thrombosis 
plot.data<-Network.IR %>% 
  filter(events>=5) %>% 
  mutate(outcome.name=str_replace_all(outcome.name,
    " - narrow",
    "")) %>% 
  filter(outcome.name %in% 
           c(
             "Myocarditis\nNarrow",
             "Myocarditis", 
             "Pericarditis\nnarrow", 
             "Pericarditis"
           )) %>% 
  mutate(outcome.name=ifelse(outcome.name=="Myocarditis_Narrow",
                             "Myocarditis\nNarrow",outcome.name
  )) %>% 
  mutate(outcome.name=ifelse(outcome.name=="Pericarditis_narrow",
                             "Pericarditis\nnarrow", outcome.name
  )) %>% 
  # mutate(outcome.name=ifelse(outcome.name=="Deep vein thrombosis",
  #                            "Deep vein\nthrombosis",outcome.name
  #                            )) %>% 
  # mutate(outcome.name=ifelse(outcome.name=="Venous thromboembolism",
  #                            "Venous\nthromboembolism",outcome.name
  #                            )) %>% 
  mutate(outcome.name=factor(outcome.name,
    levels=c("Myocarditis\nNarrow",
             "Myocarditis", 
             "Pericarditis\nnarrow", 
             "Pericarditis"
             ) )) %>% 
  filter(prior.obs.required=="Yes") %>% 
  filter(pop.type=="All") %>% 
  filter(time.window=="Full") %>% 
  filter(strata=="age_gr2") %>% 
  filter(pop!="General population (index date: 1st December)") %>% 
  # filter(pop!="SARS-CoV-2 PCR positive test")  %>% 
   mutate(pop=ifelse(pop=="General population (index date: first visit/ contact)", 
                     "General population", pop))


plot.data %>% 
  ggplot(aes(age_gr2,ir_100000, colour=pop)) +
  # facet_wrap(vars(outcome.name), #scales = "free_y",
  #            ncol=2)+
  facet_grid(outcome.name~ .,scales = "free_y",  switch="y")+
  geom_errorbar(aes(ymin=ir_100000_lower,ymax=ir_100000_upper), width=0,
                position=position_dodge(width=0.6), size=0.75)+
  geom_point(position=position_dodge(width=0.6), size=1.5 )+ 
  theme_bw()+
  ylab("Incidence rate\nper 100,000 person-years\n")+
  xlab("Age")+
  theme(panel.spacing.x =  unit(1.5, "lines"),
          panel.spacing.y = unit(0.5, "lines"))+  
  scale_y_continuous(label=label_comma(accuracy= 1), trans='log2',
                     position = "right")+
  # scale_y_continuous(label=label_comma(accuracy= 1), #position = "right",
  #                    limits=c(0,NA))+
  theme_bw()+
  theme(panel.spacing = unit(0.6, "lines"),
        legend.title = element_blank(),
        axis.text=element_text(size=10),
        axis.title=element_text(size=12,face="bold"),
        strip.text = element_text(size=12, face="bold"),
        strip.text.y.left = element_text(angle = 0),
        strip.background = element_rect( fill="#f7f7f7"),
      #  axis.title.y.right =  element_text(angle = 0),
        legend.text=element_text(size=12), 
        legend.position = "top")+
   scale_color_manual(values=c("Vaccinated with ChAdOx1" = "#e41a1c",
                                 "Vaccinated with BNT162b2" = "#377eb8",
                               "General population (index date: 1st December)" = "#4daf4a",
                               "General population" = "#984ea3",
                               "SARS-CoV-2 PCR positive test"="#ff7f00",
                               "General population with visit" = "#69c967"))


ggsave(filename=here("SummariseResults", "IRs_myo.cov.png"), 
       device = "png",
       width=12, height=8,  dpi=600)



## Figure 2 w covid -----
# thrombosis 
plot.data<-Network.IR %>% 
  filter(events>=5) %>% 
  mutate(outcome.name=str_replace_all(outcome.name,
    " - narrow",
    "")) %>% 
  filter(outcome.name %in% 
           c("Myocarditis_Narrow",
             "Myocarditis", 
             "Pericarditis_narrow", 
             "Pericarditis")) %>% 
  mutate(outcome.name=ifelse(outcome.name=="Myocarditis_Narrow",
                             "Myocarditis\nNarrow",outcome.name
  )) %>% 
  mutate(outcome.name=ifelse(outcome.name=="Pericarditis_narrow",
                             "Pericarditis\nnarrow", outcome.name
  )) %>% 
  # mutate(outcome.name=ifelse(outcome.name=="Deep vein thrombosis",
  #                            "Deep vein\nthrombosis",outcome.name
  #                            )) %>% 
  # mutate(outcome.name=ifelse(outcome.name=="Venous thromboembolism",
  #                            "Venous\nthromboembolism",outcome.name
  #                            )) %>% 
  mutate(outcome.name=factor(outcome.name,
    levels=c(
      "Myocarditis\nNarrow",
      "Myocarditis", 
      "Pericarditis\nnarrow", 
      "Pericarditis"
    ) )) %>% 
  filter(prior.obs.required=="Yes") %>% 
  filter(pop.type=="All") %>% 
  filter(time.window=="Full") %>% 
  filter(strata=="age_gr3_gender") %>% 
  filter(pop!="General population (index date: 1st December)") %>% 
  # filter(pop!="SARS-CoV-2 PCR positive test") %>% 
  mutate(pop=ifelse(pop=="General population (index date: first visit/ contact)", 
                    "General population", pop))


plot.data %>% 
  ggplot(aes(age_gr3,ir_100000, colour=pop)) +
  # facet_wrap(vars(outcome.name), #scales = "free_y",
  #            ncol=2)+
  facet_grid(outcome.name~ gender,  switch="y")+
  geom_errorbar(aes(ymin=ir_100000_lower,ymax=ir_100000_upper), width=0,
                position=position_dodge(width=0.6), size=0.75)+
  geom_point(position=position_dodge(width=0.6), size=1.5 )+ 
  theme_bw()+
  ylab("Incidence rate\nper 100,000 person-years\n")+
  xlab("Age")+
  theme(panel.spacing.x =  unit(1.5, "lines"),
          panel.spacing.y = unit(0.5, "lines"))+  
  scale_y_continuous(label=label_comma(accuracy= 1), trans='log2',
                     position = "right")+
  # scale_y_continuous(label=label_comma(accuracy= 1), #position = "right",
  #                    limits=c(0,NA))+
  theme_bw()+
  theme(panel.spacing = unit(0.6, "lines"),
        legend.title = element_blank(),
        axis.text=element_text(size=12),
        axis.title=element_text(size=12,face="bold"),
        strip.text = element_text(size=12, face="bold"),
        strip.text.y.left = element_text(angle = 0),
        strip.background = element_rect( fill="#f7f7f7"),
      #  axis.title.y.right =  element_text(angle = 0),
        legend.text=element_text(size=12), 
        legend.position = "top")+
   scale_color_manual(values=c("Vaccinated with ChAdOx1" = "#e41a1c",
                                 "Vaccinated with BNT162b2" = "#377eb8",
                               "General population (index date: 1st December)" = "#4daf4a",
                               "General population (index date: first visit/ contact)" = "#984ea3",
                               "General population" = "#984ea3",
                               "SARS-CoV-2 PCR positive test"="#ff7f00",
                               "General population with visit" = "#69c967"))

ggsave(filename=here("SummariseResults", "IRs_myo_w.covid.png"), 
       device = "png",
       width=12, height=8,  dpi=600)




######################################################################################
############################## FIGURE 3 ##############################################
## Figure TTS ------
## v1: visit YES, obs YES.
plot.data<-bind_rows(AZ_background.pop.visit.IRS %>%
   mutate(pop="Vaccinated with ChAdOx1"),
Pf_background.pop.visit.IRS %>% 
   mutate(pop="Vaccinated with BNT162b2"),
COVID_PCR_background.pop.visit.IRS%>%
   mutate(pop="SARS-CoV-2 PCR positive test")) %>% 
  filter(outcome.name %in% 
           c("Myocarditis_Narrow", "Myocarditis",  "Pericarditis_narrow"  # ,  "Pericarditis"
             )) %>% 
  mutate(outcome.name=str_replace_all(outcome.name,  " - narrow","")) %>% 
  mutate(outcome.name=str_replace_all(outcome.name,  " narrow","")) %>% 
  mutate(outcome.name=ifelse(outcome.name=="Myocarditis_Narrow",
                             "Myocarditis\nNarrow",outcome.name)) %>% 
  mutate(outcome.name=ifelse(outcome.name=="Pericarditis_narrow",
                             "Pericarditis\nnarrow", outcome.name )) %>% 
  filter(target.pop.type=="All") %>% 
  filter(target.time.window=="Full") 


# require hist for events except cvst
plot.data<- plot.data %>%  filter(prior.obs.required=="Yes") 


plot.data<-plot.data %>% 
  mutate(outcome.name=factor(outcome.name,
                               levels=c("Myocarditis\nNarrow", "Myocarditis",  
                                        "Pericarditis\nnarrow" )))

p1<-plot.data %>% 
  ggplot(aes(y=pop, fill=pop)) + 
  geom_errorbarh(aes(xmin=isr.observed,xmax=isr.expected), height=0, size=0.75,
                linetype="dotdash")+
  geom_point(aes(x=isr.observed),
             size=5.5, shape=21, alpha=0.75 ) +
  geom_point(aes(x=isr.expected), 
             size=5.5,  shape=22, alpha=0.75  )+ 
  facet_grid(outcome.name~., scales="free", switch = "y")+
  theme_bw()+
  ylab("")+
  xlab("Expected (square) versus observed (circle) events")+
  scale_y_discrete(position = "right",limits=rev)+
  scale_x_continuous( limits=c(0,NA))+
  theme_bw()+ my_theme1()+
   scale_fill_manual(values=c("Vaccinated with ChAdOx1" = "#e41a1c",
                                 "Vaccinated with BNT162b2" = "#377eb8",
                               "General population (index date: 1st December)" = "#4daf4a",
                               "General population (index date: first visit/ contact)" = "#984ea3",
                               "SARS-CoV-2 PCR positive test"="#ff7f00",
                              "General population with visit" = "#69c967"))+
   scale_colour_manual(values=c("Vaccinated with ChAdOx1" = "#e41a1c",
                                 "Vaccinated with BNT162b2" = "#377eb8",
                               "General population (index date: 1st December)" = "#4daf4a",
                               "General population (index date: first visit/ contact)" = "#984ea3",
                               "SARS-CoV-2 PCR positive test"="#ff7f00",
                               "General population with visit" = "#69c967"))+
  scale_x_continuous(label=label_comma(accuracy= 1), trans='log2')

p1

grobs <- ggplotGrob(p1)$grobs
legend <- grobs[[which(sapply(grobs, function(x) x$name) == "guide-box")]]
p1<-p1+   theme(legend.position = "none")


p2<-plot.data %>% 
  ggplot(aes(y=pop, colour=pop)) + 
  geom_errorbarh(aes(xmin=isr.sir_lower,xmax=isr.sir_upper), height=0, size=1)+
  geom_point(aes(x=isr.sir),
             size=2.5) +
  facet_grid(outcome.name~., scales="free", switch = "y")+
  theme_bw()+
  ylab("")+
  geom_vline(xintercept = 1
             )+
  xlab("SIR (95% CI)")+
  scale_y_discrete(position = "right",limits=rev)+
  scale_x_continuous( limits=c(0,NA))+
  theme_bw()+ my_theme2()+
    scale_color_manual(values=c("Vaccinated with ChAdOx1" = "#e41a1c",
                                 "Vaccinated with BNT162b2" = "#377eb8",
                               "General population (index date: 1st December)" = "#4daf4a",
                               "General population (index date: first visit/ contact)" = "#984ea3",
                               "SARS-CoV-2 PCR positive test"="#ff7f00"))+
  scale_x_continuous(label=label_comma(accuracy= 1), trans='log2')
  
pg<-cowplot::plot_grid(p1,p2, rel_widths = c(0.75,0.25))
cowplot::plot_grid(legend,pg, rel_heights =  c(0.25, 3), ncol = 1)

ggsave(filename=here("SummariseResults", "StIRs_visitY_obsY.png"), 
       device = "png",
       width=12, height=8,  dpi=600)

############ theme as function ############ 

my_theme1 <- function (){
  theme(panel.spacing = unit(0, "lines"),
        legend.title = element_blank(),
        axis.text.x = element_text(size=14),
        axis.text.y.right = element_blank(),
        axis.ticks.length =  unit(0, "lines"),
        axis.title=element_text(size=14,face="bold"),
        strip.text = element_text(size=14, face="bold"),
        strip.text.y.left = element_text(angle = 0),
        strip.background = element_rect( fill="#f7f7f7"),
        axis.title.y.right =  element_text(angle = 0),
        legend.text=element_text(size=14), 
        legend.position = "top")
}

my_theme2 <- function(){
  theme(strip.background = element_blank(), 
        strip.text = element_blank(),
        panel.spacing = unit(0, "lines"),
        legend.title = element_blank(),
        axis.text.x = element_text(size=14),
        axis.text.y.right = element_blank(),
        axis.ticks.length =  unit(0, "lines"),
        axis.title=element_text(size=14,face="bold"),
        axis.title.y.right =  element_text(angle = 0),
        legend.text=element_text(size=14), 
        legend.position = "none")
}

####################################################

## v2: visit YES, obs NO.
plot.data<-bind_rows(AZ_background.pop.visit.IRS %>%
                       mutate(pop="Vaccinated with ChAdOx1"),
                     Pf_background.pop.visit.IRS %>% 
                       mutate(pop="Vaccinated with BNT162b2"),
                     COVID_PCR_background.pop.visit.IRS%>%
                       mutate(pop="SARS-CoV-2 PCR positive test")) %>% 
  filter(outcome.name %in% 
           c("Myocarditis_Narrow", "Myocarditis",  "Pericarditis_narrow"  # ,  "Pericarditis"
           )) %>% 
  mutate(outcome.name=str_replace_all(outcome.name,  " - narrow","")) %>% 
  mutate(outcome.name=str_replace_all(outcome.name,  " narrow","")) %>% 
  mutate(outcome.name=ifelse(outcome.name=="Myocarditis_Narrow",
                             "Myocarditis\nNarrow",outcome.name)) %>% 
  mutate(outcome.name=ifelse(outcome.name=="Pericarditis_narrow",
                             "Pericarditis\nnarrow", outcome.name )) %>% 
  filter(target.pop.type=="All") %>% 
  filter(target.time.window=="Full") 


# require hist for events except cvst
plot.data<- plot.data %>%  filter(prior.obs.required=="No") 


plot.data<-plot.data %>% 
  mutate(outcome.name=factor(outcome.name,
                             levels=c("Myocarditis\nNarrow", "Myocarditis",  
                                      "Pericarditis\nnarrow" )))

p1<-plot.data %>% 
  ggplot(aes(y=pop, fill=pop)) + 
  geom_errorbarh(aes(xmin=isr.observed,xmax=isr.expected), height=0, size=0.75,
                 linetype="dotdash")+
  geom_point(aes(x=isr.observed),
             size=5.5, shape=21, alpha=0.75 ) +
  geom_point(aes(x=isr.expected), 
             size=5.5,  shape=22, alpha=0.75  )+ 
  facet_grid(outcome.name~., scales="free", switch = "y")+
  theme_bw()+
  ylab("")+
  xlab("Expected (square) versus observed (circle) events")+
  scale_y_discrete(position = "right",limits=rev)+
  scale_x_continuous( limits=c(0,NA))+
  theme_bw()+ my_theme1()+
  scale_fill_manual(values=c("Vaccinated with ChAdOx1" = "#e41a1c",
                             "Vaccinated with BNT162b2" = "#377eb8",
                             "General population (index date: 1st December)" = "#4daf4a",
                             "General population (index date: first visit/ contact)" = "#984ea3",
                             "SARS-CoV-2 PCR positive test"="#ff7f00",
                             "General population with visit" = "#69c967"))+
  scale_colour_manual(values=c("Vaccinated with ChAdOx1" = "#e41a1c",
                               "Vaccinated with BNT162b2" = "#377eb8",
                               "General population (index date: 1st December)" = "#4daf4a",
                               "General population (index date: first visit/ contact)" = "#984ea3",
                               "SARS-CoV-2 PCR positive test"="#ff7f00",
                               "General population with visit" = "#69c967"))+
  scale_x_continuous(label=label_comma(accuracy= 1), trans='log2')

p1

grobs <- ggplotGrob(p1)$grobs
legend <- grobs[[which(sapply(grobs, function(x) x$name) == "guide-box")]]
p1<-p1+   theme(legend.position = "none")


p2<-plot.data %>% 
  ggplot(aes(y=pop, colour=pop)) + 
  geom_errorbarh(aes(xmin=isr.sir_lower,xmax=isr.sir_upper), height=0, size=1)+
  geom_point(aes(x=isr.sir),
             size=2.5) +
  facet_grid(outcome.name~., scales="free", switch = "y")+
  theme_bw()+
  ylab("")+
  geom_vline(xintercept = 1
  )+
  xlab("SIR (95% CI)")+
  scale_y_discrete(position = "right",limits=rev)+
  scale_x_continuous( limits=c(0,NA))+
  theme_bw()+ my_theme2()+
  scale_color_manual(values=c("Vaccinated with ChAdOx1" = "#e41a1c",
                              "Vaccinated with BNT162b2" = "#377eb8",
                              "General population (index date: 1st December)" = "#4daf4a",
                              "General population (index date: first visit/ contact)" = "#984ea3",
                              "SARS-CoV-2 PCR positive test"="#ff7f00"))+
  scale_x_continuous(label=label_comma(accuracy= 1), trans='log2')

pg<-cowplot::plot_grid(p1,p2, rel_widths = c(0.75,0.25))
cowplot::plot_grid(legend,pg, rel_heights =  c(0.25, 3), ncol = 1)

ggsave(filename=here("SummariseResults", "StIRs_visitY_obsN.png"), 
       device = "png",
       width=12, height=8,  dpi=600)

## v3: visit No, obs YES.
plot.data<-bind_rows(AZ_background.pop.IRS %>%
                       mutate(pop="Vaccinated with ChAdOx1"),
                     Pf_background.pop.IRS %>% 
                       mutate(pop="Vaccinated with BNT162b2"),
                     COVID_PCR_background.pop.IRS%>%
                       mutate(pop="SARS-CoV-2 PCR positive test")) %>% 
  filter(outcome.name %in% 
           c("Myocarditis_Narrow", "Myocarditis",  "Pericarditis_narrow"  # ,  "Pericarditis"
           )) %>% 
  mutate(outcome.name=str_replace_all(outcome.name,  " - narrow","")) %>% 
  mutate(outcome.name=str_replace_all(outcome.name,  " narrow","")) %>% 
  mutate(outcome.name=ifelse(outcome.name=="Myocarditis_Narrow",
                             "Myocarditis\nNarrow",outcome.name)) %>% 
  mutate(outcome.name=ifelse(outcome.name=="Pericarditis_narrow",
                             "Pericarditis\nnarrow", outcome.name )) %>% 
  filter(target.pop.type=="All") %>% 
  filter(target.time.window=="Full") 


# require hist for events except cvst
plot.data<- plot.data %>%  filter(prior.obs.required=="Yes") 


plot.data<-plot.data %>% 
  mutate(outcome.name=factor(outcome.name,
                             levels=c("Myocarditis\nNarrow", "Myocarditis",  
                                      "Pericarditis\nnarrow" )))

p1<-plot.data %>% 
  ggplot(aes(y=pop, fill=pop)) + 
  geom_errorbarh(aes(xmin=isr.observed,xmax=isr.expected), height=0, size=0.75,
                 linetype="dotdash")+
  geom_point(aes(x=isr.observed),
             size=5.5, shape=21, alpha=0.75 ) +
  geom_point(aes(x=isr.expected), 
             size=5.5,  shape=22, alpha=0.75  )+ 
  facet_grid(outcome.name~., scales="free", switch = "y")+
  theme_bw()+
  ylab("")+
  xlab("Expected (square) versus observed (circle) events")+
  scale_y_discrete(position = "right",limits=rev)+
  scale_x_continuous( limits=c(0,NA))+
  theme_bw()+ my_theme1()+
  scale_fill_manual(values=c("Vaccinated with ChAdOx1" = "#e41a1c",
                             "Vaccinated with BNT162b2" = "#377eb8",
                             "General population (index date: 1st December)" = "#4daf4a",
                             "General population (index date: first visit/ contact)" = "#984ea3",
                             "SARS-CoV-2 PCR positive test"="#ff7f00",
                             "General population with visit" = "#69c967"))+
  scale_colour_manual(values=c("Vaccinated with ChAdOx1" = "#e41a1c",
                               "Vaccinated with BNT162b2" = "#377eb8",
                               "General population (index date: 1st December)" = "#4daf4a",
                               "General population (index date: first visit/ contact)" = "#984ea3",
                               "SARS-CoV-2 PCR positive test"="#ff7f00",
                               "General population with visit" = "#69c967"))+
  scale_x_continuous(label=label_comma(accuracy= 1), trans='log2')

p1

grobs <- ggplotGrob(p1)$grobs
legend <- grobs[[which(sapply(grobs, function(x) x$name) == "guide-box")]]
p1<-p1+   theme(legend.position = "none")


p2<-plot.data %>% 
  ggplot(aes(y=pop, colour=pop)) + 
  geom_errorbarh(aes(xmin=isr.sir_lower,xmax=isr.sir_upper), height=0, size=1)+
  geom_point(aes(x=isr.sir),
             size=2.5) +
  facet_grid(outcome.name~., scales="free", switch = "y")+
  theme_bw()+
  ylab("")+
  geom_vline(xintercept = 1
  )+
  xlab("SIR (95% CI)")+
  scale_y_discrete(position = "right",limits=rev)+
  scale_x_continuous( limits=c(0,NA))+
  theme_bw()+ my_theme2()+
  scale_color_manual(values=c("Vaccinated with ChAdOx1" = "#e41a1c",
                              "Vaccinated with BNT162b2" = "#377eb8",
                              "General population (index date: 1st December)" = "#4daf4a",
                              "General population (index date: first visit/ contact)" = "#984ea3",
                              "SARS-CoV-2 PCR positive test"="#ff7f00"))+
  scale_x_continuous(label=label_comma(accuracy= 1), trans='log2')

pg<-cowplot::plot_grid(p1,p2, rel_widths = c(0.75,0.25))
cowplot::plot_grid(legend,pg, rel_heights =  c(0.25, 3), ncol = 1)

ggsave(filename=here("SummariseResults", "StIRs_visitN_obsY.png"), 
       device = "png",
       width=12, height=8,  dpi=600)


## v4: visit No, obs No.
plot.data<-bind_rows(AZ_background.pop.IRS %>%
                       mutate(pop="Vaccinated with ChAdOx1"),
                     Pf_background.pop.IRS %>% 
                       mutate(pop="Vaccinated with BNT162b2"),
                     COVID_PCR_background.pop.IRS%>%
                       mutate(pop="SARS-CoV-2 PCR positive test")) %>% 
  filter(outcome.name %in% 
           c("Myocarditis_Narrow", "Myocarditis",  "Pericarditis_narrow"  # ,  "Pericarditis"
           )) %>% 
  mutate(outcome.name=str_replace_all(outcome.name,  " - narrow","")) %>% 
  mutate(outcome.name=str_replace_all(outcome.name,  " narrow","")) %>% 
  mutate(outcome.name=ifelse(outcome.name=="Myocarditis_Narrow",
                             "Myocarditis\nNarrow",outcome.name)) %>% 
  mutate(outcome.name=ifelse(outcome.name=="Pericarditis_narrow",
                             "Pericarditis\nnarrow", outcome.name )) %>% 
  filter(target.pop.type=="All") %>% 
  filter(target.time.window=="Full") 


# require hist for events except cvst
plot.data<- plot.data %>%  filter(prior.obs.required=="No") 


plot.data<-plot.data %>% 
  mutate(outcome.name=factor(outcome.name,
                             levels=c("Myocarditis\nNarrow", "Myocarditis",  
                                      "Pericarditis\nnarrow" )))

p1<-plot.data %>% 
  ggplot(aes(y=pop, fill=pop)) + 
  geom_errorbarh(aes(xmin=isr.observed,xmax=isr.expected), height=0, size=0.75,
                 linetype="dotdash")+
  geom_point(aes(x=isr.observed),
             size=5.5, shape=21, alpha=0.75 ) +
  geom_point(aes(x=isr.expected), 
             size=5.5,  shape=22, alpha=0.75  )+ 
  facet_grid(outcome.name~., scales="free", switch = "y")+
  theme_bw()+
  ylab("")+
  xlab("Expected (square) versus observed (circle) events")+
  scale_y_discrete(position = "right",limits=rev)+
  scale_x_continuous( limits=c(0,NA))+
  theme_bw()+ my_theme1()+
  scale_fill_manual(values=c("Vaccinated with ChAdOx1" = "#e41a1c",
                             "Vaccinated with BNT162b2" = "#377eb8",
                             "General population (index date: 1st December)" = "#4daf4a",
                             "General population (index date: first visit/ contact)" = "#984ea3",
                             "SARS-CoV-2 PCR positive test"="#ff7f00",
                             "General population with visit" = "#69c967"))+
  scale_colour_manual(values=c("Vaccinated with ChAdOx1" = "#e41a1c",
                               "Vaccinated with BNT162b2" = "#377eb8",
                               "General population (index date: 1st December)" = "#4daf4a",
                               "General population (index date: first visit/ contact)" = "#984ea3",
                               "SARS-CoV-2 PCR positive test"="#ff7f00",
                               "General population with visit" = "#69c967"))+
  scale_x_continuous(label=label_comma(accuracy= 1), trans='log2')

p1

grobs <- ggplotGrob(p1)$grobs
legend <- grobs[[which(sapply(grobs, function(x) x$name) == "guide-box")]]
p1<-p1+   theme(legend.position = "none")


p2<-plot.data %>% 
  ggplot(aes(y=pop, colour=pop)) + 
  geom_errorbarh(aes(xmin=isr.sir_lower,xmax=isr.sir_upper), height=0, size=1)+
  geom_point(aes(x=isr.sir),
             size=2.5) +
  facet_grid(outcome.name~., scales="free", switch = "y")+
  theme_bw()+
  ylab("")+
  geom_vline(xintercept = 1
  )+
  xlab("SIR (95% CI)")+
  scale_y_discrete(position = "right",limits=rev)+
  scale_x_continuous( limits=c(0,NA))+
  theme_bw()+ my_theme2()+
  scale_color_manual(values=c("Vaccinated with ChAdOx1" = "#e41a1c",
                              "Vaccinated with BNT162b2" = "#377eb8",
                              "General population (index date: 1st December)" = "#4daf4a",
                              "General population (index date: first visit/ contact)" = "#984ea3",
                              "SARS-CoV-2 PCR positive test"="#ff7f00"))+
  scale_x_continuous(label=label_comma(accuracy= 1), trans='log2')

pg<-cowplot::plot_grid(p1,p2, rel_widths = c(0.75,0.25))
cowplot::plot_grid(legend,pg, rel_heights =  c(0.25, 3), ncol = 1)

ggsave(filename=here("SummariseResults", "StIRs_visitN_obsN.png"), 
       device = "png",
       width=12, height=8,  dpi=600)


###############################














