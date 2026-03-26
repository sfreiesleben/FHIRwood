# Author: Sherry Freiesleben
# Email: sherry.freiesleben@med.uni-rostock.de
# Creation date (YYYY-MM-DD): 2026-03-23
# License: Attribution-NonCommercial 4.0 International (CC BY-NC 4.0)
# Script purpose:
# 1) contains the instructions needed to build the dashboard
# Notes:
# 1) This script is part of an RProject
# 2) This file needs to be in the working directory
# 3) Objects (infoboxes, plots, ...) that are not needed at a specific time point are commented out!
# 4) It is not possible to reuse the same object twice, hence versions a, b, c, ...

function(input, output, session) {
  
  #########################################################
  ###### create info- and valueboxes containing big numbers
  #########################################################
  
  ###Condition infoboxes
  output$conditionBoxa <- renderInfoBox({
    infoBox(
      "Condition: Number of values in initial population", as.numeric(df.initialPopBox$Condition[3]) %>%
        prettyNum(big.mark = ".", decimal.mark = ","),
      icon = icon("users"),
      color = "light-blue"
    )
  })
  
  ###Consent infoboxes
  output$consentBoxa <- renderInfoBox({
    infoBox(
      "Consent: Number of values in initial population", as.numeric(df.initialPopBox$Consent[3]) %>%
        prettyNum(big.mark = ".", decimal.mark = ","),
      icon = icon("users"),
      color = "light-blue"
    )
  })
  
  ###DiagnosticReportLab infoboxes
  output$DRLabBoxa <- renderInfoBox({
    infoBox(
      "Diagnostic report laboratory: Number of values in initial population", as.numeric(df.initialPopBox$DiagnosticReportLab[3]) %>%
        prettyNum(big.mark = ".", decimal.mark = ","),
      icon = icon("users"),
      color = "light-blue"
    )
  })
  
  ###DiagnosticReportPatho infoboxes
  output$DRPathoBoxa <- renderInfoBox({
    infoBox(
      "Diagnostic report pathology: Number of values in initial population", as.numeric(df.initialPopBox$DiagnosticReportPatho[3]) %>%
        prettyNum(big.mark = ".", decimal.mark = ","),
      icon = icon("users"),
      color = "light-blue"
    )
  })
  
  ###Encounter infoboxes
  output$encounterBoxa <- renderInfoBox({
    infoBox(
      "Encounter: Number of values in initial population", as.numeric(df.initialPopBox$Encounter[3]) %>%
        prettyNum(big.mark = ".", decimal.mark = ","),
      icon = icon("users"),
      color = "light-blue"
    )
  })
  
  ###MedicationAdministration infoboxes
  output$medAdminBoxa <- renderInfoBox({
    infoBox(
      "MedicationAdministration: Number of values in initial population", as.numeric(df.initialPopBox$MedicationAdministration[3]) %>%
        prettyNum(big.mark = ".", decimal.mark = ","),
      icon = icon("users"),
      color = "light-blue"
    )
  })
  
  ###MedicationList infoboxes
  output$medListBoxa <- renderInfoBox({
    infoBox(
      "MedicationList: Number of values in initial population", as.numeric(df.initialPopBox$MedicationList[3]) %>%
        prettyNum(big.mark = ".", decimal.mark = ","),
      icon = icon("users"),
      color = "light-blue"
    )
  })
  
  ###MedicationRequest infoboxes
  output$medReBoxa <- renderInfoBox({
    infoBox(
      "MedicationList: Number of values in initial population", as.numeric(df.initialPopBox$MedicationRequest[3]) %>%
        prettyNum(big.mark = ".", decimal.mark = ","),
      icon = icon("users"),
      color = "light-blue"
    )
  })
  
  ###MedicationStatement infoboxes
  output$medStatBoxa <- renderInfoBox({
    infoBox(
      "MedicationList: Number of values in initial population", as.numeric(df.initialPopBox$MedicationStatement[3]) %>%
        prettyNum(big.mark = ".", decimal.mark = ","),
      icon = icon("users"),
      color = "light-blue"
    )
  })
  
  ###Observation infoboxes
  output$observationBoxa <- renderInfoBox({
    infoBox(
      "Observation: Number of values in initial population", as.numeric(df.initialPopBox$Observation[3]) %>%
        prettyNum(big.mark = ".", decimal.mark = ","),
      icon = icon("users"),
      color = "light-blue"
    )
  })
  
  ###Patient infoboxes
  output$patientBoxa <- renderInfoBox({
    infoBox(
      "Patient: Number of values in initial population", as.numeric(df.initialPopBox$Patient[3]) %>%
        prettyNum(big.mark = ".", decimal.mark = ","),
      icon = icon("users"),
      color = "light-blue"
    )
  })
  
  ###Procedure infoboxes
  output$procedureBoxa <- renderInfoBox({
    infoBox(
      "Procedure: Number of values in initial population", as.numeric(df.initialPopBox$Procedure[3]) %>%
        prettyNum(big.mark = ".", decimal.mark = ","),
      icon = icon("users"),
      color = "light-blue"
    )
  })
  
  ###Specimen infoboxes
  output$SpecimenBoxa <- renderInfoBox({
    infoBox(
      "Specimen: Number of values in initial population", as.numeric(df.initialPopBox$Specimen[3]) %>%
        prettyNum(big.mark = ".", decimal.mark = ","),
      icon = icon("users"),
      color = "light-blue"
    )
  })

  ###################
  ###### create plots
  ###################
  
  ###plot condition dqa
  output$cond.grid <- renderPlot({
    p.cond.grid
  }, height = 700)
  
  ###plot consent dqa
  output$cons.grid <- renderPlot({
    p.cons.grid
  }, height = 700)
  
  ###plot diagnosticReportLab dqa
  #for the time being, only plot missing values
  output$drl.mv <- renderPlot({
    p.drl.mv
  })
  
  ###plot encounter dqa
  output$enc.grid <- renderPlot({
    p.enc.grid
  }, height = 1000)
  
  ###plot medicationAdministration dqa
  output$medAdmin.grid <- renderPlot({
    p.medAdmin.grid
  }, height = 700)
  
  ###plot medicationList dqa
  output$medList.grid <- renderPlot({
    p.medList.grid
  }, height = 700)
  
  ###plot medicationRequest dqa
  output$medRe.grid <- renderPlot({
    p.medRe.grid
  }, height = 750)
  
  ###plot medicationStatement dqa
  output$medStat.grid <- renderPlot({
    p.medStat.grid
  }, height = 750)
  
  ###plot observation dqa
  output$obs.grid <- renderPlot({
    p.obs.grid
  }, height = 1000)
  
  ###plot patient dqa
  output$pat.grid <- renderPlot({
      p.pat.grid
  }, height = 500)
  
  ###plot procedure dqa
  output$pro.mv <- renderPlot({
    p.pro.mv
  }, height = 500)
  
  ###plot specimen dqa
  output$spe.grid <- renderPlot({
    p.spe.grid
  }, height = 700)
  
  ###plot percent of missing values
  output$mv.combined <- renderPlot({
    p.mv.combined
  }, height = 800)

  ####################
  ###### create tables
  ####################
  
  ### table overall json files
  #the dataframe for this section can be found in: obtain_Json.R
  #output$jsontable <- DT::renderDataTable({
  #  DT::datatable(data)
  #})
  
  
  ### table for initial population count
  #the dataframe for this section can be found in: obtain_Json.R
  output$initialPop <- DT::renderDataTable({
    DT::datatable(df.initialPop)
  })
  
  ### table for the complete list of DQA checks that have been performed
  #the dataframe for this section can be found in: obtain_Json.R
  output$DQACheckNames <- DT::renderDataTable({
    DT::datatable(df.dqa.check.names.all.u)
  })
  
  #the dataframe for this section can be found in: obtain_Json.R
  #output$duplicatedDQA <- DT::renderDataTable({
  #  DT::datatable(df.duplicated)
  #})
  
}
