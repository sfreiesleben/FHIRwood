# Author: Sherry Freiesleben
# Email: sherry.freiesleben@med.uni-rostock.de
# Creation date (YYYY-MM-DD): 2026-03-23
# License: Attribution-NonCommercial 4.0 International (CC BY-NC 4.0)
# Script purpose:
# 1) controls the layout and appearance of the dashboard
# Notes:
# 1) This script is part of an RProject
# 2) This file needs to be in the working directory

################################################################################
#sidebar menu chunk
################################################################################

sidebar_menu = sidebarMenu(
  id = "sidebarID",
  menuItem("About FHIRwood", tabName = "tab_about", icon = icon("info")),
  menuItem("Resource", tabName = "tab_dashboard", icon = icon("fire"),
           menuSubItem("Condition", tabName = "tab_condition", icon = icon("user-injured")),
           menuSubItem("Consent", tabName = "tab_consent", icon = icon("person-circle-check")),
           menuSubItem("Diagnostic Report: Laboratory", tabName = "tab_DRLab", icon = icon("notes-medical")),
           menuSubItem("Diagnostic Report: Pathology", tabName = "tab_DRPatho", icon = icon("notes-medical")),
           menuSubItem("Encounter", tabName = "tab_encounter", icon = icon("hospital")),
           menuSubItem("Medication Administration", tabName = "tab_medAdmin", icon = icon("pills")),
           menuSubItem("Medication List", tabName = "tab_medList", icon = icon("pills")),
           menuSubItem("Medication Request", tabName = "tab_medRe", icon = icon("pills")),
           menuSubItem("Medication Statement", tabName = "tab_medStat", icon = icon("pills")),
           menuSubItem("Observation", tabName = "tab_observation", icon = icon("flask-vial")),
           menuSubItem("Patient", tabName = "tab_patient", icon = icon("users")),
           menuSubItem("Procedure", tabName = "tab_procedure", icon = icon("bed-pulse")),
           menuSubItem("Specimen", tabName = "tab_specimen", icon = icon("vials"))#,
           #For lists of available icons, see http://fontawesome.io/icons/ and http://getbootstrap.com/components/#glyphicons
  ),
  menuItem("Overall DQA Scores", tabName = "tab_overall", icon = icon("star")),
  menuItem("Missing Values", tabName = "tab_missingValues", icon = icon("circle-exclamation"))#,
)

################################################################################
#tab items chunk
################################################################################

tab_items <- tabItems(
  
  ###tab_about content
  tabItem(tabName = "tab_about",
          tags$head(tags$style(HTML("a {color: #bc6b3c}"))),
          
          h1("Welcome"),
          
          h3("The DQA-CQL Dashboard is an R-based tool to retrieve Json files from the DQA-CQL GitHub repository and to visualize the content of the Json files via a dashboard."),
          
          p("This is a reminder that there should be a small description that goes here explaining the different parts of the dashboard."),
          
          br(),#this adds vertical space
          
          #fluidPage(
          #  HTML("<p>The DQA-CQL Dashboard is an R-based tool to retrieve Json files from the DQA-CQL GitHub repository and to visualize the content of the Json files via a dashboard. 
          #       <!-- There should here be a small description that goes a bit more in detail as to what the tool does and give a bit of DQ background -->
          #       </p>")
          #),

          fluidPage(
            HTML("<p>This following table shows how many resources are available per resource type in the initial population. 
                 The row name corresponds to the resource type. 
                 The column <i>resourceName</i> corresponds to the name of the resource types as it appears in the <i>reports</i> folder of the DQA-CQL repository. 
                 The column <i>code</i> corresponds to the type of population.
                 The column <i>count</i> corresponds to the number of elements that belong to the type of population.
                 </p>")
          ),

          fluidPage(
            DT::dataTableOutput("initialPop"),
          ),
          
          br(),#this adds vertical space

          fluidPage(
            HTML("<p>In this section I should explain what the following table does. To this table, I should add what resources contains what checks
                 </p>")
          ),
          
          fluidPage(
            DT::dataTableOutput("DQACheckNames"),
          ),
  ),
  
  
  ###tab_condition content
  tabItem(tabName = "tab_condition",
          #h2("Condition tab content"),
          
          fluidRow(
            infoBoxOutput("conditionBoxa")
          ),
          
          fluidRow(
            box(plotOutput("cond.grid"),
                #height = 500,
                width = "100%"
            ),
          ),
  ),
  
  ###tab_consent content
  tabItem(tabName = "tab_consent",
          #h2("consent tab content"),
          
          fluidRow(
            infoBoxOutput("consentBoxa")
          ),
          
          fluidRow(
            box(plotOutput("cons.grid"),
                #height = 500,
                width = "100%"
            ),
          ),
  ),
 
  ###tab_DRLab content
  tabItem(tabName = "tab_DRLab",
          #h2("Condition tab content"),
          
          fluidRow(
            infoBoxOutput("DRLabBoxa")
          ),
          
          fluidRow(
            box(plotOutput("drl.mv"),
                #height = 500,
                width = "100%"
            ),
          ),
  ),

  ###tab_DRPatho content
  tabItem(tabName = "tab_DRPatho",
          
          fluidRow(
            infoBoxOutput("DRPathoBoxa")
          ),
          
          h3("There is no DQA data available for this resource type"),
          
  ),
  
  
  ###tab_encounter content
  tabItem(tabName = "tab_encounter",
          
          fluidRow(
            infoBoxOutput("encounterBoxa")
          ),
          
          fluidRow(
            box(plotOutput("enc.grid"),
                #height = 500,
                width = "100%"
            ),
          ),
  ),
  
  ###tab_medAdmin content
  tabItem(tabName = "tab_medAdmin",
          
          fluidRow(
            infoBoxOutput("medAdminBoxa")
          ),
          
          fluidRow(
            box(plotOutput("medAdmin.grid"),
                #height = 500,
                width = "100%"
            ),
          ),
  ),
  
  ###tab_medList content
  tabItem(tabName = "tab_medList",
          
          fluidRow(
            infoBoxOutput("medListBoxa")
          ),
          
          fluidRow(
            box(plotOutput("medList.grid"),
                #height = 500,
                width = "100%"
            ),
          ),
  ),
  
  ###tab_medRe content
  tabItem(tabName = "tab_medRe",
          
          fluidRow(
            infoBoxOutput("medReBoxa")
          ),
          
          fluidRow(
            box(plotOutput("medRe.grid"),
                #height = 500,
                width = "100%"
            ),
          ),
  ),
  
  ###tab_medStat content
  tabItem(tabName = "tab_medStat",
          
          fluidRow(
            infoBoxOutput("medStatBoxa")
          ),
          
          fluidRow(
            box(plotOutput("medStat.grid"),
                #height = 500,
                width = "100%"
            ),
          ),
  ),
  
  #tab_observation content
  tabItem(tabName = "tab_observation",
          
          fluidRow(
            infoBoxOutput("observationBoxa")
          ),
          
          fluidRow(
            box(plotOutput("obs.grid"),
                #height = 500,
                width = "100%"
            ),
          ),
  ),
  
  #tab_patient content
  tabItem(tabName = "tab_patient",

          fluidRow(
            infoBoxOutput("patientBoxa")
          ),

          fluidRow(
            box(plotOutput("pat.grid"),
                #height = 500,
                width = "100%"
            ),
          ),
  ),

  #tab_procedure content
  tabItem(tabName = "tab_procedure",
          
          fluidRow(
            infoBoxOutput("procedureBoxa")
          ),
          
          fluidRow(
            box(plotOutput("pro.mv"),
                #height = 500,
                width = "100%"
            ),
          ),
  ),
  
  #tab_specimen content
  tabItem(tabName = "tab_specimen",
          
          fluidRow(
            infoBoxOutput("SpecimenBoxa")
          ),
          
          fluidRow(
            box(plotOutput("spe.grid"),
                #height = 500,
                width = "100%"
            ),
          ),
  ),

  ###tab_overall content
  tabItem(tabName = "tab_overall",
          h2("The overall DQA scores should be here"),
          
  ),
  
  ###tab_missingValues content
  tabItem(tabName = "tab_missingValues",
          #h2("For all resources, the DQ missing values should be displayed here"),
          
          fluidRow(
            box(plotOutput("mv.combined"),
                #height = 500,
                width = "100%"
            ),
          ),
          
  )#,
)

################################################################################
#dashboard page chunk
################################################################################

###add image to the title and hyperlink the title in the header
title <- tags$a(href = "https://github.com/sfreiesleben/FHIRwood",
                tags$img(src="FHIRwood.png", height = "45"),
                "FHIRwood"
                )

dashboardPage(
  title = "FHIRwood", #title shown on internet site tab
  header = dashboardHeader(title = title, titleWidth = 300),
  skin = "black",
  
  ###Dashboard sidebar content
  sidebar = dashboardSidebar(
    sidebarMenu(sidebar_menu)
  ),
  
  ###Dashboard body content
  body = dashboardBody(
    tab_items
  )
)
