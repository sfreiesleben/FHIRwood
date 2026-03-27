# Author: Sherry Freiesleben
# Email: sherry.freiesleben@med.uni-rostock.de
# Creation date (YYYY-MM-DD): 2026-03-19
# License: Attribution-NonCommercial 4.0 International (CC BY-NC 4.0)
# Script purpose:
# 1) obtain Json files form GitHub repository
# 2) store data in dataframes
# 3) retrieve the names of all the DQA checks that have been performed
# Notes: This script is part of an RProject

################################################################################
# create list of files of raw Json files directly from saved repository
################################################################################

#path <- "./DQA-CQL/reports/"
#files <- dir(path, pattern = "*.json")

#data <- files %>%
#  map_df(~jsonlite::fromJSON(file.path(path, .), flatten = TRUE))

################################################################################
# create list of files of raw Json files directly from saved repository
################################################################################

#l.path <- paste0("./DQA-CQL/reports/", list.files("../DQA-CQL/reports"))

#l.path <- l.path[grep(".json", l.path)]

path <- "./DQA-CQL/reports/"
files <- dir(path, pattern = "*.json")

l.path <- paste0(path, files)

#l.name <- gsub("./DQA-CQL/reports/", "", l.path)
#l.name <- gsub(".json", "", l.name)

for(i in 1:length(files)){
  name <- gsub(".json", "", files[i])
  name <- paste0("j.", name)
  
  assign(name, jsonlite::fromJSON(l.path[i]))
}

################################################################################
# create data frame with initial population count
################################################################################

c.name <- c("resourceName",
            "code",
            "count"#,
            #"true",
            #"false",
            #"null"
            )

r.name <- gsub(".json", "", files)

df.initialPop <- data.frame(matrix(ncol = length(c.name),
                                   nrow = length(r.name),
                                   dimnames = list(r.name, c.name)))

dqa.check.names.all <- c()

################################################################################
# create DQ data frames for every resource type
################################################################################

for(i in 1:length(files)){
  #get json list
  jl <- jsonlite::fromJSON(l.path[i], flatten = FALSE)
  
  #create name for resulting dqa data frame
  r.name <- gsub(".json", "", files[i])
  df.name <- paste0("df.", r.name)
  #df.name <- files[i]
  
  #get resource name as saved in the Json file
  resource <- unlist(jl$group$code, recursive = FALSE, use.names = FALSE)
  
  #get resource code and count
  jl.pop <- unlist(jl$group$population, recursive=FALSE)
  jl.code <- unlist(jl.pop$code$coding)[2]
  jl.count <- jl.pop$count
  
  #fill data frame regarding overall population count
  df.initialPop$resourceName[i] <- resource
  df.initialPop$code[i] <- jl.code
  df.initialPop$count[i] <- jl.count
  
  #get stratifier DQA check names
  stratifier <- unlist(jl$group$stratifier, recursive = FALSE)
  #All DQA check names
  stratifier.code <- unlist(stratifier$code, use.names = FALSE)
  
  #add all stratifier.code to vector to, get all available DQ checks
  dqa.check.names.all <- c(dqa.check.names.all, stratifier.code)
  
  #create df to store dqa data
  c.name <- c("dqaName",
              "text",
              "count")
    
  r.name <- c()
  
  df.dqa <- data.frame(matrix(ncol = length(c.name),
                              nrow = length(r.name),
                              dimnames = list(r.name, c.name)))
  
  #start retrieving  DQA data
  if(is.null(stratifier$stratum)){
    #this indicates that the are no values associated to the DQ checks (initial population=0)
    #stratum part of stratifier does not exist
    #print(paste(i, j, k, collapse = ", "))
    
    #save empty data frame
    assign(df.name, df.dqa)
    
    next
  }
  
  #fill df.dqa (all DQA checks) with population counts
  for(j in 1:length(stratifier$stratum)){
   stratum <- unlist(stratifier$stratum[j], use.names = FALSE)

   df.stratum <- as.data.frame(stratifier$stratum[j])

   for(k in 1:nrow(df.stratum)){
     stratum.text <- unlist(df.stratum$value$text, use.names = FALSE)[k]
     
     #if stratum text does not exist meaning that information is stored differently
     #data is stored in stratum$value instead
     if(is.null(stratum.text)){
       df.stratum.value <- as.data.frame(df.stratum$value)
       #print(paste(i, j, k, collapse = ", "))
       
       for(l in 1:nrow(df.stratum.value)){
         stratum.value.coding <- paste(unlist(df.stratum.value$coding[l], use.names = FALSE), collapse = ";")
         stratum.value.poupulation <- unlist(df.stratum$population[l], use.names = FALSE)[3]

         df.info <- data.frame("dqaName" = stratifier.code[j],
                               "text" = stratum.value.coding,
                               "count" = stratum.value.poupulation)
         
         df.dqa <- rbind(df.dqa, df.info)
       }
       
       next
     }
     
     stratum.count <- unlist(df.stratum$population, use.names = FALSE)[k*3]
     
     df.info <- data.frame("dqaName" = stratifier.code[j],
                           "text" = stratum.text,
                           "count" = stratum.count)

     df.dqa <- rbind(df.dqa, df.info)
     
   }
  }
  
  assign(df.name, df.dqa)
} 

################################################################################
# format df.initialPop for infoboxes
################################################################################

df.initialPopBox <- as.data.frame(t(df.initialPop))
colnames(df.initialPopBox)<- rownames(df.initialPop)
#df.initialPopBox <- df.initialPopBox[-c(1:3),]

################################################################################
# create DQA categories
################################################################################

#these vectors are needed for the plots
dqa.check.names.all <- sort(dqa.check.names.all)
dqa.check.names.all.u <- unique(sort(dqa.check.names.all))

#write csv of DQA check names
write.table(dqa.check.names.all.u,
            file = "DQA_check_names.csv",
            sep = ",",
            fileEncoding = "UTF-8",
            row.names = FALSE,
            col.names = FALSE,
            quote = FALSE)


df.dqa.check.names.all.u <- data.frame(dqa.check.names.all.u)
colnames(df.dqa.check.names.all.u) <- c("DQA check names")

#dqa.categories <- c("Inadmissible categorical values",
#                    "Missing values",
#                    "Other")



#check for duplicated dqa checks
#dqa.check.names.all.duplicated <- sort(unique(dqa.check.names.all[duplicated(dqa.check.names.all)]))
#df.duplicated <- as.data.frame(dqa.check.names.all.duplicated)
#colnames(df.duplicated) <- c("DQA checks avaiable for multiple resource types")
#
#create data frame for dqa checks that occur in more than 1 resource type
#dqa.check.names.all.unique <- sort(unique(dqa.check.names.all))
#df.unique <- as.data.frame(dqa.check.names.all.unique)
#colnames(df.unique) <- c("List of all available DQA checks")
#
#format the initial population table for the dashboard
#df.names <- data.frame("Resource Name in Json file" = row.names(df.initialPop))
#rownames(df.names) <- row.names(df.initialPop)
#
#for the initial population table present in the dashboard, rename the columns
#colnames(df.initialPop) <- c("Resource name",
#                             "Population type",
#                             "Count")
#
#df.initialPop <- cbind(df.names, df.initialPop)
#rownames(df.initialPop) <- 1:nrow(df.initialPop) 
#
#colnames(df.initialPop) <- c("Resource Name in Json file",
#                             "Resource name",
#                             "Population type",
#                             "Count")

################################################################################
# create list to chose a DQA check
################################################################################


#test <- dqa.check.names.all

#mv <- grep("Missing values", dqa.check.names.all)
#inad <- grep("Inadmissible categorical values", dqa.check.names.all)
#effect <- grep("Effective type", dqa.check.names.all)
#status <- grep("Status observed", dqa.check.names.all)
#iden <- grep("identifierType observed", dqa.check.names.all)
#weg <- grep("Weg der Anwendung observed", dqa.check.names.all)

#other <- seq(1:length(dqa.check.names.all))[-c(mv,  inad,  effect,  status,  iden, weg)]





#list.dqa <- list(`Missing values` = list(dqa.check.names.all[mv]),
#                 `Inadmissible categorical values` = list(dqa.check.names.all[inad]),
#                 `Effective type` = list(dqa.check.names.all[effect]),
#                 `Status observed` = list(dqa.check.names.all[status]),
#                 `identifierType observed` = list(dqa.check.names.all[iden]),
#                 `Weg der Anwendung observed` = list(dqa.check.names.all[weg]),
#                 `Other` = list(dqa.check.names.all[other])
#                 )





#list.dqa <- list(`Inadmissible categorical values` = list(dqa.check.names.all[effect]),
#  
#  
#  
#  `Missing values` = list(dqa.check.names.all[mv]),
#                 `Inadmissible categorical values` = list(dqa.check.names.all[inad]),
#                 `Status observed` = list(dqa.check.names.all[status]),
#                 `identifierType observed` = list(dqa.check.names.all[iden]),
#                 `Weg der Anwendung observed` = list(dqa.check.names.all[weg]),
#                 `Other` = list(dqa.check.names.all[other])
#)














