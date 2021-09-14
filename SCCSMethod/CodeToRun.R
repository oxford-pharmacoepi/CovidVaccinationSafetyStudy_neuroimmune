
#install.packages("renv") # if not already installed, install renv from CRAN
# renv::restore() # this should prompt you to install the various packages required for the study
# renv::activate()
# renv::init()
# renv::status()
# renv::snapshot()

# packages
library(SqlRender)
# install.packages("remotes")
library(remotes)
# remotes::install_github("ohdsi/DatabaseConnector", ref = "develop")
library(DatabaseConnector)
library(FeatureExtraction)
library(here)
library(lubridate)
library(stringr)
library(ggplot2)
library(DBI)
library(dbplyr)
library(dplyr)
library(tidyr)
library(kableExtra)
# remotes::install_github("rstats-db/RSQLite")
# install.packages("RSQLite")
library(RSQLite)
#library(rmarkdown)
#library(tableone)
library(scales)
library(forcats)
#library(epiR)
library(RPostgreSQL)
library(SelfControlledCaseSeries)
library(Andromeda)

# Optional: specify where the temporary files (used by the Andromeda package) will be created:
options(andromedaTempFolder = "/home/xli/VaxSafetySCCS/andromedaTemp")

## Get the number of available gigabytes:
getAndromedaTempDiskSpace()/ 1024^3

# connect to db

output.folder.name<-here::here("output")
# the path to a folder (that exists) where the results from this analysis will be saved

oracleTempSchema<-NULL

# If you haven´t already, save database details to .Renviron by running:
# usethis::edit_r_environ()

# p20_000211_cdm_aurum is db with vaccinated cohorts
# p20_059_cdm_aurum is db with covid cohorts
# cdmgold202007 with general population

## select only ONE ####
run.vax.cohorts<-TRUE
run.covid.cohorts<-FALSE

if(run.vax.cohorts){
    server<-Sys.getenv("DB_SERVER_p20_000211_cdm_aurum")
    server_dbi<-Sys.getenv("DB_SERVER_p20_000211_cdm_aurum_dbi")
    #server_dbi = "p20_000211_cdm_aurum"
    db.name<-"CPRD AURUM_vaccinated"
} else if(run.covid.cohorts){
    server<-Sys.getenv("DB_SERVER_p20_059_cdm_aurum")
    server_dbi<-Sys.getenv("DB_SERVER_p20_059_cdm_aurum_dbi")
    db.name<-"CPRD AURUM_covid"
}


user<-Sys.getenv("DB_USER")
password<- Sys.getenv("DB_PASSWORD")
port<-Sys.getenv("DB_PORT")
host<-Sys.getenv("DB_HOST")
connectionDetails <-DatabaseConnector::downloadJdbcDrivers("postgresql", here::here())
connectionDetails <- DatabaseConnector::createConnectionDetails(dbms = "postgresql",
                                                                server =server,
                                                                user = user,
                                                                password = password,
                                                                port = port ,
                                                                pathToDriver = here::here())

db <- dbConnect(RPostgreSQL::PostgreSQL(),
                dbname = server_dbi,
                port = port,
                host = host,
                user = user,
                password = password)

#dbListTables(db)


# The OHDSI DatabaseConnector connection details
targetDialect <-"postgresql"
# This is your sql dialect used with the OHDSI SqlRender package

cdm_database_schema<-"public"
# This is the name of the schema that contains the OMOP CDM with patient-level data
vocabulary_database_schema<-"vocabulary"
# This is the name of the schema that contains the vocabularies

results_database_schema<-"results"
# This is the name of the schema where a results table will be created
cdmVersion <- "5"

cohortTableExposures<-"CovVaxExposures_XL_SCCS"
cohortTableOutcomes <-"CovVaxOutcomes_XL_SCCS"
cohortTableProfiles<-"CovVaxProfiles_XL"
# These are the tables to be created in your results schema for this analysis
# You can keep the above names or change them
# Note, any existing tables in your results schema with the same name will be overwritten


conn <- connect(connectionDetails)

db.name<-"CPRD AURUM"
# This is the name/ acronym for your database (to be used in the titles of reports, etc)

create.outcome.cohorts<-FALSE  #TRUE  #FALSE
# if you have already created the outcome cohorts, you can set this to FALSE to skip instantiating these cohorts again
create.exposure.cohorts <- TRUE

# set which analysis to run. Only ONE per time, only applied to Vaccine cohorts.
run.main.analysis <- TRUE  # Main analysis : require 1 yr observation prior to index date
run.sens1 <- FALSE  # Sensitivity analysis 1: NOT require 1 yr observation prior to index date
run.sens2 <- FALSE  # Sensitivity analysis 2: Require 1 yr observation prior to index date AND 28 days post-vaccination.


# run the analysis
startAll<-Sys.time()
source(here("runSCCS.R"))
Sys.time()-startAll


dbDisconnect(con)
disconnect(conn)


# remove temp files
tempdir()
file.remove(list.files(tempdir(), full.names = TRUE) )
