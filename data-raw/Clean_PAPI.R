library(haven)
library(xlsx)
library(dplyr)
library(ggplot2)
library(vietnamcode)
library(vietnamdata)

setwd("/media/minh/Dropbox (MIT)/Documents/Works/Vietnam Elections/Data/PAPI/")
#source("../../Code/Functions.R")

#### Import PAPI Data
# file name
file.papi <- "PAPI_2011-2016.xlsx"

# read in PAPI data for every year
# raw data is already consistently formatted
papi <- do.call(bind_rows, lapply(1:6, function(i) {

    papi <- read.xlsx2(file.papi, sheetIndex=i, startRow=3, endRow = 65, colIndex = c(3:32),
                       header=F, colClasses = c("character", rep("numeric",29)), stringsAsFactors=F)

    # in 2016 theere were some changes in indicators
    if(i < 6) {
        names(papi) <- c("prov",
                         "papi_participation", "papi_participation_knowledge", "papi_participation_opportunities", "papi_participation_elections", "papi_participation_contributions",
                         "papi_transparency", "papi_transparency_povertylists", "papi_transparency_budgetexpenditure", "papi_transparency_landuse",
                         "papi_accountability", "papi_accountability_interaction", "papi_accountability_inspection", "papi_accountability_investment",
                         "papi_corruption", "papi_corruption_public", "papi_corruption_service", "papi_corruption_employment", "papi_corruption_willingness",
                         "papi_procedures", "papi_procedures_certification", "papi_procedures_construction", "papi_procedures_landuse", "papi_procedures_personal",
                         "papi_service", "papi_service_health", "papi_service_primary", "papi_service_infrastructure", "papi_service_law",
                         "papi_unweighted")
    } else {
        names(papi) <- c("prov",
                         "papi_participation", "papi_participation_knowledge", "papi_participation_opportunities", "papi_participation_elections", "papi_participation_contributions",
                         "papi_transparency", "papi_transparency_povertylists", "papi_transparency_budgetexpenditure", "papi_transparency_landuse",
                         "papi_accountability", "papi_accountability_interaction", "papi_accountability_responsive", "papi_accountability_inspection",
                         "papi_corruption", "papi_corruption_public", "papi_corruption_service", "papi_corruption_employment", "papi_corruption_willingness",
                         "papi_procedures", "papi_procedures_certification", "papi_procedures_construction", "papi_procedures_landuse", "papi_procedures_personal",
                         "papi_service", "papi_service_health", "papi_service_primary", "papi_service_infrastructure", "papi_service_law",
                         "papi_unweighted")
    }

    yearraw <- c(2011:2016)
    papi$year <- yearraw[i]

    return(papi)
}))

#### Cleaning

# clean up province names using the Vietnam Code function
papi$prov <- vietnamcode(papi$prov, origin = "province_name", destination = "province_name")

# arrange variables
papi <- papi %>%
    select(prov, year,
           papi_participation, papi_transparency, papi_accountability, papi_corruption, papi_procedures, papi_service, papi_unweighted,
           papi_participation_knowledge, papi_participation_opportunities, papi_participation_elections, papi_participation_contributions,
           papi_transparency_povertylists, papi_transparency_budgetexpenditure, papi_transparency_landuse,
           papi_accountability_interaction, papi_accountability_inspection, papi_accountability_investment, papi_accountability_responsive,
           papi_corruption_public, papi_corruption_service, papi_corruption_employment, papi_corruption_willingness,
           papi_procedures_certification, papi_procedures_construction, papi_procedures_landuse, papi_procedures_personal,
           papi_service_health, papi_service_primary, papi_service_infrastructure, papi_service_law) %>%
    arrange(prov, year)
