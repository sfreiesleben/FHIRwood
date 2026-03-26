# Author: Sherry Freiesleben
# Email: sherry.freiesleben@med.uni-rostock.de
# Creation date (YYYY-MM-DD): 2026-03-20
# License: Attribution-NonCommercial 4.0 International (CC BY-NC 4.0)
# Script purpose:
# 1) store DQA data from jsonfiles in dataframes
# Notes: This script is part of an RProject


#get stratifier
stratifier <- unlist(j.Condition$group$stratifier , recursive = FALSE)

#get all DQA check names
stratifier.code <- unlist(stratifier$code, use.names = FALSE)

#create df to store dqa data
df.name <- paste0("df.", resource)

c.name <- c()

r.name <- c("text",
            "count"
)

df.dqa <- data.frame(matrix(ncol = length(c.name),
                            nrow = length(r.name),
                            dimnames = list(r.name, c.name)))

#fill df.dqa (all DQA checks) with population counts
for(j in 1:length(stratifier$stratum)){
  #get information associated to 1 dqa check/stratum
  stratum <- unlist(stratifier$stratum[j], use.names = FALSE)
  df.stratum <- as.data.frame(stratifier$stratum[j])
  
  for(k in 1:nrow(df.stratum)){
    #get value text of check
    stratum.text <- unlist(df.stratum$value$text, use.names = FALSE)[k]
    
    #get count associated to value text
    stratum.count <- unlist(df.stratum$population, use.names = FALSE)[k*3]
    
    #create data frame to append to dqa data frame   
    info <- c(stratum.text, stratum.count)
    df.info <- data.frame(info, row.names = c("text", "count"))
    colnames(df.info) <- paste0(gsub(" ", ".", stratifier.code[j]), ".", k)
    
    df.dqa <- cbind(df.dqa, df.info)
  }
}

assign("df.Condition", df.dqa)
 

