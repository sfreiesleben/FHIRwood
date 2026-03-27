# Author: Sherry Freiesleben
# Email: sherry.freiesleben@med.uni-rostock.de
# Creation date (YYYY-MM-DD): 2026-03-19
# License: Attribution-NonCommercial 4.0 International (CC BY-NC 4.0)
# Script purpose:
# 1) runs the source scripts to create the necessary data for the dashboard
# Notes:
# 1) This script is part of an RProject

################################################################################
# set global options
################################################################################

options(scipen=999) #remove scientific notation

################################################################################
# packages to install
################################################################################

source("./packages_to_install.R")

################################################################################
# obtain information from Json files
################################################################################

source("./obtain_Json.R")

################################################################################
# CREATE THE DASHBOARD PLOTS
################################################################################

################################################################################
# obtain plots based on Condition FHIR resources
################################################################################

source("./Plot/Plot_condition.R")

################################################################################
# obtain plots based on Consent FHIR resources
################################################################################

source("./Plot/Plot_consent.R")

################################################################################
# obtain plots based on Diagnostic Report: Laboratory FHIR resources
################################################################################

#source("./Plot/Plot_diagnosticReportLab.R")

################################################################################
# obtain plots based on Diagnostic Report: Pathology FHIR resources
################################################################################

#source("./Plot/Plot_diagnosticReportPatho.R")

################################################################################
# obtain plots based on Encounter FHIR resources
################################################################################

#source("./Plot/Plot_encounter.R")

################################################################################
# obtain plots based on Medication Administration FHIR resources
################################################################################

source("./Plot/Plot_medicationAdministration.R")

################################################################################
# obtain plots based on Medication List FHIR resources
################################################################################

source("./Plot/Plot_medicationList.R")

################################################################################
# obtain plots based on Medication Request FHIR resources
################################################################################

source("./Plot/Plot_medicationRequest.R")

################################################################################
# obtain plots based on Medication Statement FHIR resources
################################################################################

source("./Plot/Plot_medicationStatement.R")

################################################################################
# obtain plots based on Observation FHIR resources
################################################################################

#source("./Plot/Plot_observation.R")

################################################################################
# obtain plots based on Patient FHIR resources
################################################################################

source("./Plot/Plot_patient.R")

################################################################################
# obtain plots based on Procedure FHIR resources
################################################################################

#source("./Plot/Plot_procedure.R")

################################################################################
# obtain plots based on Specimen FHIR resources
################################################################################

#source("./Plot/Plot_specimen.R")

################################################################################
# obtain plots of missing values
################################################################################

source("./Plot/Plot_missing_values.R")
