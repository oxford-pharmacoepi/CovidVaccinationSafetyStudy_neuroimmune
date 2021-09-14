##### CREATE OUTCOME COHORTS ... #####################

# instantiate outcome tables -----
cohort.sql<-list.files(here("OutcomeCohorts", "sql"))
cohort.sql<-cohort.sql[cohort.sql!="CreateCohortTable.sql"]
outcome.cohorts<-tibble(id=1:length(cohort.sql),
                        file=cohort.sql,
                        name=str_replace(cohort.sql, ".sql", ""))
outcome.cohorts$outcome_name <- gsub(" ", "_", outcome.cohorts$name)


if(create.outcome.cohorts=="FALSE"){
  print(paste0("- Skipping creating outcome cohorts"))
} else {
  print(paste0("- Getting outcomes"))


  conn <- connect(connectionDetails)
  # create empty cohorts table
  print(paste0("Create empty cohort table"))
  sql<-readSql(here("OutcomeCohorts", "sql","CreateCohortTable.sql"))
  sql<-SqlRender::translate(sql, targetDialect = targetDialect)
  renderTranslateExecuteSql(conn=conn,
                            sql,
                            cohort_database_schema =  results_database_schema,
                            cohort_table = cohortTableOutcomes)
  rm(sql)

  for(cohort.i in 1:length(outcome.cohorts$id)){

    working.id<-outcome.cohorts$id[cohort.i]

    print(paste0("- Getting outcome: ",
                 outcome.cohorts$name[cohort.i],
                 " (", cohort.i, " of ", length(outcome.cohorts$name), ")"))
    sql<-readSql(here("OutcomeCohorts", "sql",
                      outcome.cohorts$file[cohort.i]))
    sql <- sub("BEGIN: Inclusion Impact Analysis - event.*END: Inclusion Impact Analysis - person", "", sql)

    sql<-SqlRender::translate(sql, targetDialect = targetDialect)
    renderTranslateExecuteSql(conn=conn,
                              sql,
                              cdm_database_schema = cdm_database_schema,
                              vocabulary_database_schema = vocabulary_database_schema,
                              target_database_schema = results_database_schema,
                              results_database_schema = results_database_schema,
                              target_cohort_table = cohortTableOutcomes,
                              target_cohort_id = working.id)
  }
  disconnect(conn)
}


# CREATE EXPOSURE COHORTS ... ########

if(run.vax.cohorts=="TRUE"){
#####  generate exposure cohorts  ########

# instantiate exposure tables -----
cohort.sql<-list.files(here("VaccinatedCohorts", "sql"))
cohort.sql<-cohort.sql[cohort.sql!="CreateCohortTable.sql"]
vaccinated.cohorts<-tibble(id=1:length(cohort.sql),
                           file=cohort.sql,
                           name=str_replace(cohort.sql, ".sql", ""))



if(create.exposure.cohorts=="FALSE"){
  print(paste0("- Skipping creating exposure cohorts"))
} else {
  print(paste0("- Getting exposures"))

conn <- connect(connectionDetails)
# create empty cohorts table
print(paste0("Create empty cohort table"))
sql<-readSql(here("VaccinatedCohorts", "sql","CreateCohortTable.sql"))
sql<-SqlRender::translate(sql, targetDialect = targetDialect)
renderTranslateExecuteSql(conn=conn,
                          sql,
                          cohort_database_schema =  results_database_schema,
                          cohort_table = cohortTableExposures)
rm(sql)

for(cohort.i in 1:length(vaccinated.cohorts$id)){

  working.id<-vaccinated.cohorts$id[cohort.i]

  print(paste0("- Getting vaccine exposure: ",
               vaccinated.cohorts$name[cohort.i],
               " (", cohort.i, " of ", length(vaccinated.cohorts$name), ")"))
  sql<-readSql(here("VaccinatedCohorts", "sql",
                    vaccinated.cohorts$file[cohort.i]))
  sql <- sub("BEGIN: Inclusion Impact Analysis - event.*END: Inclusion Impact Analysis - person", "", sql)

  sql<-SqlRender::translate(sql, targetDialect = targetDialect)
  renderTranslateExecuteSql(conn=conn,
                            sql,
                            cdm_database_schema = cdm_database_schema,
                            vocabulary_database_schema = vocabulary_database_schema,
                            target_database_schema = results_database_schema,
                            results_database_schema = results_database_schema,
                            target_cohort_table = cohortTableExposures,
                            target_cohort_id = working.id)
}
disconnect(conn)
}

##########  CHECK THE COHORTS CREATED...  ###################

conn <- connect(connectionDetails)

print("Outcome cohorts and count... ")
print(outcome.cohorts)
sql <- "select cohort_definition_id, count(distinct subject_id) as patiend_ct, count( subject_id) as outcome_ct
        from @results_database_schema.@cohortTableOutcomes
        group by cohort_definition_id ;"
temp.table <- DatabaseConnector::renderTranslateQuerySql(conn, sql,
                                           results_database_schema = results_database_schema,
                                           cohortTableOutcomes = cohortTableOutcomes)
print(temp.table)

print("Exposure cohorts and count... ")
print(vaccinated.cohorts)
sql <- "select cohort_definition_id, count(distinct subject_id) as patiend_ct, count( subject_id) as exposure_ct
        from @results_database_schema.@cohortTableExposures
        group by cohort_definition_id ;"
temp.table <- DatabaseConnector::renderTranslateQuerySql(conn, sql,
                                           results_database_schema = results_database_schema,
                                           cohortTableExposures = cohortTableExposures)
disconnect(conn)

print(temp.table)
######################################################
########## RUN SCCS MODELS ... #######################


#### select exposure id ########
if(run.main.analysis=="TRUE"){
  PF_exposureid <- vaccinated.cohorts %>% filter(name == pfizer_cohort_name ) %>% select(id)  ##### SELECT the exposure cohort to run on #####
  AZ_exposureid <- vaccinated.cohorts %>% filter(name == AZ_cohort_name ) %>% select(id)
  output.folder <- paste0(output.folder.name, "/MainAna")
  print("Running the main analysis... ")

} else if(run.sens1=="TRUE"){
  PF_exposureid <- vaccinated.cohorts %>% filter(name == "Vaccinated with Pfizer-Biontech" ) %>% select(id)  ##### SELECT the exposure cohort to run on #####
  AZ_exposureid <- vaccinated.cohorts %>% filter(name == "Vaccinated with AstraZeneca" ) %>% select(id)
  output.folder <- paste0(output.folder.name, "/Sens1_wo_1yr")
  print("Running the Sensitivity analysis 1: NOT require 1 yr observation prior to index date... ")


} else if(run.sens2=="TRUE"){
  PF_exposureid <- vaccinated.cohorts %>% filter(name == "Pfizer_req_obs_1y28d" ) %>% select(id)  ##### SELECT the exposure cohort to run on #####
  AZ_exposureid <- vaccinated.cohorts %>% filter(name == "AZ_req_obs_1y28d" ) %>% select(id)
  output.folder <- paste0(output.folder.name, "/Sens2_1yr_28d")
  print("Running the Sensitivity analysis 2: Require 1 yr observation prior to index date AND 28 days post-vaccination.... ")

} else{
  print("No analysis was selected to run.. Please check the setting. ")
}


PF_exposureid <- as.numeric(PF_exposureid$id)
AZ_exposureid <- as.numeric(AZ_exposureid$id)


## neuro-immune outcomes ###

###################################################################

## select outcome of interest to run model ##
outcome.list <- outcome.cohorts %>%
  filter(name %in% c("BellsPalsy"                     ###### select the outcomes to be included  #####
                     )) %>%            ####  , "Encephalomyelitis" ,"GuillainBarreSyndrome","TransverseMyelitis"
select(id)
outcome.list <- as.numeric(outcome.list$id)
outcome.list

## Pfizer ##########
## get sccsData
sccsData_PF <- SelfControlledCaseSeries::getDbSccsData(connectionDetails = connectionDetails,
                                                       cdmDatabaseSchema = cdm_database_schema,
                                                       #       oracleTempSchema = oracleTempSchema,
                                                       outcomeDatabaseSchema = results_database_schema,
                                                       outcomeTable = cohortTableOutcomes,
                                                       outcomeIds = c(1:length(outcome.cohorts$outcome_name)),
                                                       exposureDatabaseSchema = results_database_schema,
                                                       exposureTable = cohortTableExposures,
                                                       exposureIds = PF_exposureid,
                                                       studyStartDate = "20170101",  ######   2017  ########
                                                       cdmVersion = cdmVersion)

summary(sccsData_PF)

saveSccsData(sccsData_PF, paste0(output.folder,"/PF_full/VaxPF_SccsData.zip" ))

sccsData_PF <- loadSccsData(paste0(output.folder,"/PF_full/VaxPF_SccsData.zip" ))

PF_power_mdrr <- list()

start0<-Sys.time()

for(working.outcome in c(outcome.list)){   #  2,4,6,7

  PF_sccs_studypop <- list()
  PF_sccs_model <- list()
  PF_sccs_model_simple <- list()
  PF_sccs_model_1yrP <- list()

  #   working.outcome = 2 #7
  model.num <- paste0("outcome_",working.outcome)
  model.name <- paste0(outcome.cohorts[outcome.cohorts$id==working.outcome,]$outcome_name)
  print(paste0("running for ",model.num, " : ", model.name))

  studyPop <- createStudyPopulation(sccsData = sccsData_PF,
                                    outcomeId = c(working.outcome), ##  c(3,4,5)  ###########
                                    firstOutcomeOnly = TRUE
                                    # minAge = 30           ########   minimun age   ##########
                                    # ,
                                    #  naivePeriod = 365
  )

  # plotExposureCentered(studyPop, sccsData_PF, exposureEraId = 2)
  # plotAgeSpans(studyPop)
  # plotEventObservationDependence(studyPop)
  # plotEventToCalendarTime(studyPop)

  print(getAttritionTable(studyPop))
  PF_sccs_studypop[[model.name]] <- studyPop

  # Defining  model setting
  covarVaxPF <- createEraCovariateSettings(label = "vaccine Pfizer",
                                           startAnchor = "era start" ,
                                           start = 1,
                                           end = 28,
                                           endAnchor = "era start",
                                           firstOccurrenceOnly = TRUE)

  covarPreVaxPF <- createEraCovariateSettings(label = "Pre-exposure",
                                              start = -28,
                                              end = -1,
                                              endAnchor = "era start")

  covarPre1yVaxPF <- createEraCovariateSettings(label = "Pre-exposure",
                                                start = -365,
                                                end = -1,
                                                endAnchor = "era start")

  ## Including age and seasonality
  ageCovariateSettings <- createAgeCovariateSettings(ageKnots = 5)
  seasonalityCovariateSettings <- createSeasonalityCovariateSettings(seasonKnots = 4)

  start<-Sys.time()
  sccsIntervalData_ageseason <- createSccsIntervalData(studyPopulation = studyPop,
                                                       sccsData = sccsData_PF,
                                                       eraCovariateSettings = list(covarVaxPF,
                                                                                   covarPreVaxPF),
                                                       ageCovariateSettings = ageCovariateSettings,
                                                       seasonalityCovariateSettings = seasonalityCovariateSettings)

  PF_power_mdrr[[model.name]] <-  computeMdrr(sccsIntervalData_ageseason,1000)

  sccsIntervalData_ageseason_1yrP <- createSccsIntervalData(studyPopulation = studyPop,
                                                            sccsData = sccsData_PF,
                                                            eraCovariateSettings = list(covarVaxPF,
                                                                                        covarPre1yVaxPF),
                                                            ageCovariateSettings = ageCovariateSettings,
                                                            seasonalityCovariateSettings = seasonalityCovariateSettings)

  sccsIntervalData_simple <- createSccsIntervalData(studyPopulation = studyPop,
                                                    sccsData = sccsData_PF,
                                                    eraCovariateSettings = list(covarVaxPF,
                                                                                covarPreVaxPF))

  rm(studyPop)
  gc()
  model <- fitSccsModel(sccsIntervalData_ageseason,
                        control = createControl(threads = parallel::detectCores()-1))
  PF_sccs_model[[model.name]] <- model

  gc()

  model_1yrP <- fitSccsModel(sccsIntervalData_ageseason_1yrP)
  PF_sccs_model_1yrP[[model.name]] <- model_1yrP
  gc()

  model_simple <- fitSccsModel(sccsIntervalData_simple)
  print(Sys.time()-start)
  PF_sccs_model_simple[[model.name]] <- model_simple
  # rm(studyPop)
  gc()

  rm(sccsIntervalData_ageseason)
  rm(sccsIntervalData_ageseason_1yrP)


  # model
  # plotAgeEffect(model)
  # plotSeasonality(model)
  # getModel(model)

  filename <- paste0(output.folder,"/PF_full/",model.name,".RData")
  save(PF_sccs_studypop, PF_sccs_model,PF_sccs_model_1yrP, PF_sccs_model_simple,outcome.cohorts, file = filename)
  rm(PF_sccs_studypop)
  rm(PF_sccs_model)
  rm(PF_sccs_model_1yrP)
  rm(PF_sccs_model_simple)
}

print(paste("Total run time for Pfizer is: ",Sys.time()-start0 ) )

PF_power_mdrr_tb <- bind_rows(PF_power_mdrr,.id = "outcome")

filename <- paste0(output.folder,"/PF_full/PF_power_mdrr.RData")
save(PF_power_mdrr_tb, file = filename)


#######
#######
#######


## AZ ##########
## get sccsData
sccsData_AZ <- SelfControlledCaseSeries::getDbSccsData(connectionDetails = connectionDetails,
                                                       cdmDatabaseSchema = cdm_database_schema,
                                                       #       oracleTempSchema = oracleTempSchema,
                                                       outcomeDatabaseSchema = results_database_schema,
                                                       outcomeTable = cohortTableOutcomes,
                                                       outcomeIds = c(1:length(outcome.cohorts$outcome_name)),
                                                       exposureDatabaseSchema = results_database_schema,
                                                       exposureTable = cohortTableExposures,
                                                       exposureIds = AZ_exposureid,
                                                       studyStartDate = "20170101",  ######   2017  ######
                                                       cdmVersion = cdmVersion)

#sccsData_AZ
summary(sccsData_AZ)

saveSccsData(sccsData_AZ, paste0(output.folder,"/AZ_full/VaxAZ_SccsData.zip" ))
sccsData_AZ <- loadSccsData(paste0(output.folder,"/AZ_full/VaxAZ_SccsData.zip" ))


AZ_power_mdrr <- list()

start0<-Sys.time()

for(working.outcome in c(outcome.list)){   #  2,4,6,7

  AZ_sccs_studypop <- list()
  AZ_sccs_model <- list()
  AZ_sccs_model_simple <- list()
  AZ_sccs_model_1yrP <- list()


  #   working.outcome = 2 #7
  model.num <- paste0("outcome_",working.outcome)
  model.name <- paste0(outcome.cohorts[outcome.cohorts$id==working.outcome,]$outcome_name)
  print(paste0("running for ",model.num, " : ", model.name))

  studyPop <- createStudyPopulation(sccsData = sccsData_AZ,
                                    outcomeId = c(working.outcome), ##  c(3,4,5)  ###########
                                    firstOutcomeOnly = TRUE
                                    # minAge = 30           ########   minimun age   ##########
                                    # ,
                                    #  naivePeriod = 365
  )

  # plotExposureCentered(studyPop, sccsData_AZ, exposureEraId = 2)
  # plotAgeSpans(studyPop)
  # plotEventObservationDependence(studyPop)
  # plotEventToCalendarTime(studyPop)

  print(getAttritionTable(studyPop))
  AZ_sccs_studypop[[model.name]] <- studyPop

  # Defining  model setting
  covarVaxAZ <- createEraCovariateSettings(label = "vaccine AZ",
                                           #    includeEraIds = VaxAZ,
                                           startAnchor = "era start" ,
                                           start = 1,
                                           end = 28,
                                           endAnchor = "era start",
                                           firstOccurrenceOnly = TRUE)

  covarPreVaxAZ <- createEraCovariateSettings(label = "Pre-exposure",
                                              #      includeEraIds = diclofenac,
                                              start = -28,
                                              end = -1,
                                              endAnchor = "era start")

  covarPre1yVaxAZ <- createEraCovariateSettings(label = "Pre-exposure",
                                                start = -365,
                                                end = -1,
                                                endAnchor = "era start")


  ## Including age and seasonality
  ageCovariateSettings <- createAgeCovariateSettings(ageKnots = 5)
  seasonalityCovariateSettings <- createSeasonalityCovariateSettings(seasonKnots = 4)

  start<-Sys.time()
  sccsIntervalData_ageseason <- createSccsIntervalData(studyPopulation = studyPop,
                                                       sccsData = sccsData_AZ,
                                                       eraCovariateSettings = list(covarVaxAZ,
                                                                                   covarPreVaxAZ),
                                                       ageCovariateSettings = ageCovariateSettings,
                                                       seasonalityCovariateSettings = seasonalityCovariateSettings)

  sccsIntervalData_ageseason_1yrP <- createSccsIntervalData(studyPopulation = studyPop,
                                                            sccsData = sccsData_AZ,
                                                            eraCovariateSettings = list(covarVaxAZ,
                                                                                        covarPre1yVaxAZ),
                                                            ageCovariateSettings = ageCovariateSettings,
                                                            seasonalityCovariateSettings = seasonalityCovariateSettings)

  AZ_power_mdrr[[model.name]] <-  computeMdrr(sccsIntervalData_ageseason,1000)

  sccsIntervalData_simple <- createSccsIntervalData(studyPopulation = studyPop,
                                                    sccsData = sccsData_AZ,
                                                    eraCovariateSettings = list(covarVaxAZ,
                                                                                covarPreVaxAZ))

  rm(studyPop)
  gc()
  model <- fitSccsModel(sccsIntervalData_ageseason,
                        control = createControl(threads = parallel::detectCores()-1))
  # print(Sys.time()-start)
  AZ_sccs_model[[model.name]] <- model

  gc()


  model_1yrP <- fitSccsModel(sccsIntervalData_ageseason_1yrP)
  AZ_sccs_model_1yrP[[model.name]] <- model_1yrP
  gc()

  model_simple <- fitSccsModel(sccsIntervalData_simple)
  print(Sys.time()-start)
  AZ_sccs_model_simple[[model.name]] <- model_simple
  # rm(studyPop)
  gc()

  rm(sccsIntervalData_ageseason)
  rm(sccsIntervalData_ageseason_1yrP)


  # model
  # plotAgeEffect(model)
  # plotSeasonality(model)
  # getModel(model)

  filename <- paste0(output.folder,"/AZ_full/",model.name,".RData")
  save(AZ_sccs_studypop, AZ_sccs_model,AZ_sccs_model_1yrP, AZ_sccs_model_simple,outcome.cohorts, file = filename)
  rm(AZ_sccs_studypop)
  rm(AZ_sccs_model)
  rm(AZ_sccs_model_1yrP)
  rm(AZ_sccs_model_simple)
}

print(paste("Total run time for AZ is: ",Sys.time()-start0 ) )

AZ_power_mdrr_tb <- bind_rows(AZ_power_mdrr,.id = "outcome")

filename <- paste0(output.folder, "/AZ_full/AZ_power_mdrr",".RData")
save(AZ_power_mdrr_tb, file = filename)


# # zip results
zipName <- paste0(output.folder, "/sccs_neuro_VAX_full.zip")

files<-c(paste0(output.folder,"/AZ_full"),paste0(output.folder,"/PF_full"))

files <- files[file.exists(files)==TRUE]
zip(zipfile = zipName, files = files)

}else if(run.covid.cohorts=="TRUE"){
###########

## power calculation with sccsIntervalData
#
#
# # AZ ##
# sccsData_AZ <- loadSccsData("tempdata/AZ_full/VaxAZ_SccsData.zip")
#
# AZ_power_mdrr <- list()
#
# for(working.outcome in c(outcome.list)){   #  2,4,6,7
#   #   working.outcome = 2 #7
#   model.num <- paste0("outcome_",working.outcome)
#   model.name <- paste0(outcome.cohorts[outcome.cohorts$id==working.outcome,]$outcome_name)
#   print(paste0("running for ",model.num, " : ", model.name))
#
#   studyPop <- createStudyPopulation(sccsData = sccsData_AZ,
#                                     outcomeId = c(working.outcome), ##  c(3,4,5)  ###########
#                                     firstOutcomeOnly = TRUE ,
#                                     minAge = 30           ########   minimun age   ##########
#   )
#
#   # plotExposureCentered(studyPop, sccsData_AZ, exposureEraId = 2)
#   # plotAgeSpans(studyPop)
#   # plotEventObservationDependence(studyPop)
#   # plotEventToCalendarTime(studyPop)
#
#   print(getAttritionTable(studyPop))
#   #AZ_sccs_studypop[[model.name]] <- studyPop
#
#   # Defining  model setting
#   covarVaxAZ <- createEraCovariateSettings(label = "vaccine AZ",
#                                            #    includeEraIds = VaxAZ,
#                                            startAnchor = "era start" ,
#                                            start = 1,
#                                            end = 28,
#                                            endAnchor = "era start",
#                                            firstOccurrenceOnly = TRUE)
#
#   covarPreVaxAZ <- createEraCovariateSettings(label = "Pre-exposure",
#                                               #      includeEraIds = diclofenac,
#                                               start = -28,
#                                               end = -1,
#                                               endAnchor = "era start")
#
#   ## Including age and seasonality
#   ageCovariateSettings <- createAgeCovariateSettings(ageKnots = 5)
#   seasonalityCovariateSettings <- createSeasonalityCovariateSettings(seasonKnots = 4)
#
#   #start<-Sys.time()
#   sccsIntervalData_ageseason <- createSccsIntervalData(studyPopulation = studyPop,
#                                                        sccsData = sccsData_AZ,
#                                                        eraCovariateSettings = list(covarVaxAZ,
#                                                                                    covarPreVaxAZ),
#                                                        ageCovariateSettings = ageCovariateSettings,
#                                                        seasonalityCovariateSettings = seasonalityCovariateSettings)
#
#   AZ_power_mdrr[[model.name]] <-  computeMdrr(sccsIntervalData_ageseason,1000)
#   rm(sccsIntervalData_ageseason)
# }
#
# AZ_power_mdrr_tb <- bind_rows(AZ_power_mdrr,.id = "outcome")
#
# filename <- paste0("tempdata/AZ_full/AZ_power_mdrr",".RData")
# save(AZ_power_mdrr_tb, file = filename)
#
#
# ## Pfizer ##
#
# sccsData_PF <- loadSccsData("tempdata/PF_full/VaxPF_SccsData.zip")
#
# PF_power_mdrr <- list()
#
# for(working.outcome in c(outcome.list)){   #  2,4,6,7
#   #   working.outcome = 2 #7
#   model.num <- paste0("outcome_",working.outcome)
#   model.name <- paste0(outcome.cohorts[outcome.cohorts$id==working.outcome,]$outcome_name)
#   print(paste0("running for ",model.num, " : ", model.name))
#
#   studyPop <- createStudyPopulation(sccsData = sccsData_PF,
#                                     outcomeId = c(working.outcome), ##  c(3,4,5)  ###########
#                                     firstOutcomeOnly = TRUE ,
#                                     minAge = 30           ########   minimun age   ##########
#   )
#
#   # plotExposureCentered(studyPop, sccsData_PF, exposureEraId = 2)
#   # plotAgeSpans(studyPop)
#   # plotEventObservationDependence(studyPop)
#   # plotEventToCalendarTime(studyPop)
#
#   print(getAttritionTable(studyPop))
#   #PF_sccs_studypop[[model.name]] <- studyPop
#
#   # Defining  model setting
#   covarVaxPF <- createEraCovariateSettings(label = "vaccine PF",
#                                            #    includeEraIds = VaxPF,
#                                            startAnchor = "era start" ,
#                                            start = 1,
#                                            end = 28,
#                                            endAnchor = "era start",
#                                            firstOccurrenceOnly = TRUE)
#
#   covarPreVaxPF <- createEraCovariateSettings(label = "Pre-exposure",
#                                               #      includeEraIds = diclofenac,
#                                               start = -28,
#                                               end = -1,
#                                               endAnchor = "era start")
#
#   ## Including age and seasonality
#   ageCovariateSettings <- createAgeCovariateSettings(ageKnots = 5)
#   seasonalityCovariateSettings <- createSeasonalityCovariateSettings(seasonKnots = 4)
#
#   #start<-Sys.time()
#   sccsIntervalData_ageseason <- createSccsIntervalData(studyPopulation = studyPop,
#                                                        sccsData = sccsData_PF,
#                                                        eraCovariateSettings = list(covarVaxPF,
#                                                                                    covarPreVaxPF),
#                                                        ageCovariateSettings = ageCovariateSettings,
#                                                        seasonalityCovariateSettings = seasonalityCovariateSettings)
#
#   PF_power_mdrr[[model.name]] <-  computeMdrr(sccsIntervalData_ageseason,1000)
#   rm(sccsIntervalData_ageseason)
# }
#
# PF_power_mdrr_tb <- bind_rows(PF_power_mdrr,.id = "outcome")
#
# filename <- paste0("tempdata/PF_full/PF_power_mdrr",".RData")
# save(PF_power_mdrr_tb, file = filename)


######################################
## neuro-immune otucomes ###

############## covid.cohorts   #######################################

  # instantiate exposure tables -----
  cohort.sql<-list.files(here("CovidCohorts", "sql"))
  cohort.sql<-cohort.sql[cohort.sql!="CreateCohortTable.sql"]
  covid.cohorts<-tibble(id=1:length(cohort.sql),
                             file=cohort.sql,
                             name=str_replace(cohort.sql, ".sql", ""))



  if(create.exposure.cohorts=="FALSE"){
    print(paste0("- Skipping creating exposure cohorts"))
  } else {
    print(paste0("- Getting exposures"))

    conn <- connect(connectionDetails)
    # create empty cohorts table
    print(paste0("Create empty cohort table"))
    sql<-readSql(here("CovidCohorts", "sql","CreateCohortTable.sql"))
    sql<-SqlRender::translate(sql, targetDialect = targetDialect)
    renderTranslateExecuteSql(conn=conn,
                              sql,
                              cohort_database_schema =  results_database_schema,
                              cohort_table = cohortTableExposures)
    rm(sql)

    for(cohort.i in 1:length(covid.cohorts$id)){

      working.id<-covid.cohorts$id[cohort.i]

      print(paste0("- Getting covid exposure: ",
                   covid.cohorts$name[cohort.i],
                   " (", cohort.i, " of ", length(covid.cohorts$name), ")"))
      sql<-readSql(here("CovidCohorts", "sql",
                        covid.cohorts$file[cohort.i]))
      sql <- sub("BEGIN: Inclusion Impact Analysis - event.*END: Inclusion Impact Analysis - person", "", sql)

      sql<-SqlRender::translate(sql, targetDialect = targetDialect)
      renderTranslateExecuteSql(conn=conn,
                                sql,
                                cdm_database_schema = cdm_database_schema,
                                vocabulary_database_schema = vocabulary_database_schema,
                                target_database_schema = results_database_schema,
                                results_database_schema = results_database_schema,
                                target_cohort_table = cohortTableExposures,
                                target_cohort_id = working.id)
    }
    disconnect(conn)
  }


##########  CHECK THE COHORTS CREATED...  ###################

  conn <- connect(connectionDetails)

  print("Outcome cohorts and count... ")
  print(outcome.cohorts)
  sql <- "select cohort_definition_id, count(distinct subject_id) as patiend_ct, count( subject_id) as outcome_ct
        from @results_database_schema.@cohortTableOutcomes
        group by cohort_definition_id ;"
  temp.table <- DatabaseConnector::renderTranslateQuerySql(conn, sql,
                                                           results_database_schema = results_database_schema,
                                                           cohortTableOutcomes = cohortTableOutcomes)
  print(temp.table)

  print("Exposure cohorts and count... ")
  print(covid.cohorts)
  sql <- "select cohort_definition_id, count(distinct subject_id) as patiend_ct, count( subject_id) as exposure_ct
        from @results_database_schema.@cohortTableExposures
        group by cohort_definition_id ;"
  temp.table <- DatabaseConnector::renderTranslateQuerySql(conn, sql,
                                                           results_database_schema = results_database_schema,
                                                           cohortTableExposures = cohortTableExposures)
  disconnect(conn)

  print(temp.table)

#####################################################
######### RUN SCCS MODELS ... #######################


#### select exposure id ########
# for the covid cohort, only one cohort created and the exposureid = 1.
  output.folder <- paste0(output.folder.name, "/MainAna")
  print("Running the main analysis... ")


## select outcome of interest to run model ##
outcome.list <- outcome.cohorts %>% filter(name %in% c("BellsPalsy", "Encephalomyelitis","GuillainBarreSyndrome","TransverseMyelitis")) %>%
  select(id)
outcome.list <- as.numeric(outcome.list$id)
outcome.list


## Pfizer ##########
## get sccsData
sccsData_covid <- SelfControlledCaseSeries::getDbSccsData(connectionDetails = connectionDetails,
                                                          cdmDatabaseSchema = cdm_database_schema,
                                                          #       oracleTempSchema = oracleTempSchema,
                                                          outcomeDatabaseSchema = results_database_schema,
                                                          outcomeTable = cohortTableOutcomes,
                                                          outcomeIds = c(1:length(outcome.cohorts$outcome_name)),
                                                          exposureDatabaseSchema = results_database_schema,
                                                          exposureTable = cohortTableExposures,
                                                          exposureIds = 1,
                                                          studyStartDate = "20170101",  ######   2019  ######
                                                          cdmVersion = cdmVersion)

summary(sccsData_covid)

saveSccsData(sccsData_covid, paste0(output.folder,"/covid_full/covid_SccsData.zip"))
sccsData_covid <- loadSccsData(paste0(output.folder,"/covid_full/covid_SccsData.zip"))

covid_power_mdrr <- list()

start0<-Sys.time()

for(working.outcome in c(outcome.list)){   #  2,4,6,7

  covid_sccs_studypop <- list()
  covid_sccs_model <- list()
  covid_sccs_model_simple <- list()
  covid_sccs_model_sp <- list()

  #   working.outcome = 2 #7
  model.num <- paste0("outcome_",working.outcome)
  model.name <- paste0(outcome.cohorts[outcome.cohorts$id==working.outcome,]$outcome_name)
  print(paste0("running for ",model.num, " : ", model.name))

  studyPop <- createStudyPopulation(sccsData = sccsData_covid,
                                    outcomeId = c(working.outcome), ##  c(3,4,5)  ###########
                                    firstOutcomeOnly = TRUE
                                   # minAge = 30            ########   minimun age   ##########
                                    # ,
                                    #  naivePeriod = 365
  )

  # plotExposureCentered(studyPop, sccsData_covid, exposureEraId = 2)
  # plotAgeSpans(studyPop)
  # plotEventObservationDependence(studyPop)
  # plotEventToCalendarTime(studyPop)

  print(getAttritionTable(studyPop))
  covid_sccs_studypop[[model.name]] <- studyPop

  # Defining  model setting
  covarCovid <- createEraCovariateSettings(label = "SARS-CoV-2 PCR positive test",
                                           #    includeEraIds = VaxAZ,
                                           startAnchor = "era start" ,
                                           start = 1,
                                           end = 90,
                                           endAnchor = "era start",
                                           firstOccurrenceOnly = TRUE)

  covarPreCovid <- createEraCovariateSettings(label = "Pre-exposure",
                                              start = -28,
                                              end = -1,
                                              endAnchor = "era start")

  ## Including age and seasonality
  ageCovariateSettings <- createAgeCovariateSettings(ageKnots = 5)
  seasonalityCovariateSettings <- createSeasonalityCovariateSettings(seasonKnots = 4)

  start<-Sys.time()
  sccsIntervalData_ageseason <- createSccsIntervalData(studyPopulation = studyPop,
                                                       sccsData = sccsData_covid,
                                                       eraCovariateSettings = list(covarCovid, covarPreCovid),
                                                       ageCovariateSettings = ageCovariateSettings,
                                                       seasonalityCovariateSettings = seasonalityCovariateSettings)

  covid_power_mdrr[[model.name]] <-  computeMdrr(sccsIntervalData_ageseason,1000)

  sccsIntervalData_simple <- createSccsIntervalData(studyPopulation = studyPop,
                                                    sccsData = sccsData_covid,
                                                    eraCovariateSettings = list(covarCovid, covarPreCovid))

  rm(studyPop)
  gc()
  model <- fitSccsModel(sccsIntervalData_ageseason,
                        control = createControl(threads = parallel::detectCores()-1))
  # print(Sys.time()-start)
  covid_sccs_model[[model.name]] <- model
  # covid_bells_model[["yr19_30old"]] <- model

  gc()

  # model <- fitSccsModel(sccsIntervalData_ageseason_sp)
  # # print(Sys.time()-start)
  # sccs_model_sp[[model.name]] <- model
  # gc()

  model_simple <- fitSccsModel(sccsIntervalData_simple)
  print(Sys.time()-start)
  covid_sccs_model_simple[[model.name]] <- model_simple
  # rm(studyPop)
  gc()

  rm(sccsIntervalData_ageseason)


  # model
  # plotAgeEffect(model)
  # plotSeasonality(model)
  # getModel(model)

  filename <- paste0(output.folder, "/covid_full/",model.name,".RData")
  save(covid_sccs_studypop, covid_sccs_model,covid_sccs_model_simple,outcome.cohorts, file = filename)
  rm(covid_sccs_studypop)
  rm(covid_sccs_model)
  rm(covid_sccs_model_simple)
}

covid_power_mdrr <- bind_rows(covid_power_mdrr,.id = "outcome")

filename <- paste0(output.folder,"/covid_full/PF_power_mdrr.RData")
save(covid_power_mdrr, file = filename)

print(paste("Total run time for Covid is: ",Sys.time()-start0 ) )

# # zip results
zipName <- paste0(output.folder, "/sccs_neuro_covid_full.zip")

files<-c(paste0(output.folder,"/covid_full"))

files <- files[file.exists(files)==TRUE]
zip(zipfile = zipName, files = files)

}



