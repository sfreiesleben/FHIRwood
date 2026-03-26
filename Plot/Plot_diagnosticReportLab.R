# Author: Sherry Freiesleben
# Email: sherry.freiesleben@med.uni-rostock.de
# Creation date (YYYY-MM-DD): 2026-03-25
# License: Attribution-NonCommercial 4.0 International (CC BY-NC 4.0)
# Script purpose:
# 1) create plots related to the diagnostic report laboratory resource
# Notes:
# 1) This script is part of an RProject
# 2) the ui.R and server.R files need to be in the working directory 

################################################################################
# create data frames needed for plots
################################################################################

#create df containing all info
dqa.check.names <- sort(unique(df.DiagnosticReportLab$dqaName))

c.name <- c("dqaName",
            "text",
            "count")

r.name <- c()

df.DiagnosticReportLab.info <- data.frame(matrix(ncol = length(c.name),
                                     nrow = length(r.name),
                                     dimnames = list(r.name, c.name)))

for(i in 1:length(dqa.check.names)){
  name <- dqa.check.names[i]
  name <- paste0("^", name, "$")
  
  rows <- grep(name, df.DiagnosticReportLab$dqaName)
  
  #print(paste0(i, ": ", rows))
  
  if(length(rows) == 1){
    df.info <- df.DiagnosticReportLab[rows,]
    
    if(df.info$text == "true"){
      df.info.f <- data.frame("dqaName" = df.info$dqaName,
                              "text" = "false",
                              "count"= 0)
      
      df.DiagnosticReportLab.info <- rbind(df.DiagnosticReportLab.info, df.info, df.info.f)
    }
    
    if(df.info$text == "false"){
      df.info.t <- data.frame("dqaName" = df.info$dqaName,
                              "text" = "true",
                              "count"= 0)
      
      df.DiagnosticReportLab.info <- rbind(df.DiagnosticReportLab.info, df.info.t,  df.info)
    }
    
    
    if((df.info$text!="true") && (df.info$text!="false")){
      #print(paste0(i, ": ", rows))
      next
    }
    
  }
  
  if(length(rows) == 2){
    df.info <- df.DiagnosticReportLab[rows,]
    dt <- grep("dateTime", df.info$text)
    #print(paste0(i, ": ", rows))
    
    if(length(dt)> 0){
      next
    }
    
    df.DiagnosticReportLab.info <- rbind(df.DiagnosticReportLab.info, df.info)
  }
  
  if((length(rows) != 1) && (length(rows) != 2)){
    #print(paste0(i, ": ", rows))
    next
  }
}

#create df containing missing values
df.DiagnosticReportLab.mv <- df.DiagnosticReportLab.info %>%
  filter(grepl("Missing values", dqaName)) %>%
  mutate(mv_name = gsub("Missing values ", "", dqaName)) %>%
  mutate(mv_name = gsub("\\.", " ", mv_name)) %>%
  mutate(mv_name = gsub(":", ": ", mv_name)) %>%
  mutate(count = as.numeric(count))

df.DiagnosticReportLab.mv$mv_name <- factor(df.DiagnosticReportLab.mv$mv_name, levels = sort(unique(as.character(df.DiagnosticReportLab.mv$mv_name)), decreasing = TRUE))

#create df for overall statistic for missing values
s.true <- sum(df.DiagnosticReportLab.mv$count[grep("true", df.DiagnosticReportLab.mv$text)])
s.false <- sum(df.DiagnosticReportLab.mv$count[grep("false", df.DiagnosticReportLab.mv$text)])
total <- s.true + s.false

df.DiagnosticReportLab.mv.overall <- data.frame("sumTrue" = s.true,
                                    "percentTrue"= round((s.true/total*100), digits = 0),
                                    "sumFalse" = s.false,
                                    "percentFalse"= round((s.false/total*100), digits = 0),
                                    "total" = total,
                                    row.names = "DiagnosticReportLab")

#create df for remain DQ check in df.DiagnosticReportLab.info
df.DiagnosticReportLab.rest <- df.DiagnosticReportLab.info %>%
  filter(!grepl("Missing values", dqaName)) %>%
  replace_na(list(text = "Not available")) %>%
  mutate(rest_name = dqaName) %>%
  mutate(count = as.numeric(count))

df.DiagnosticReportLab.rest$rest_name <- factor(df.DiagnosticReportLab.rest$rest_name, levels = sort(unique(as.character(df.DiagnosticReportLab.rest$rest_name)), decreasing = TRUE))

################################################################################
# plot: missing values blue hue
################################################################################

p.drl.mv <- ggplot(df.DiagnosticReportLab.mv, aes(x = mv_name, y = count, fill = text)) + 
  geom_bar(stat = "identity") + coord_flip() +
  
  ggtitle("Missing Values") +
  ylab("Number of values") +
  xlab(" ") +
  
  theme_minimal() +
  
  theme(
    plot.title = element_text(face = "bold", size=15, hjust=0.5),
    #axis.title.x = element_text(margin = margin(b=7), hjust = 0.5, vjust = -4),
    axis.title.y = element_text(margin = margin(b=7), hjust = 0.5, vjust = 3),
    axis.text.x = element_text(size = 9, angle = 45, vjust = 1, hjust=1),
    axis.text.y = element_text(size = 11),
    axis.title.x = element_blank(),
    legend.title = element_blank()
    #legend.position = "none" # Removes the legend
  ) +
  scale_fill_manual(labels = c("False", "True"), #make sure this is correct!
                    values = c("#C5DFED","#3C8DBC")) +
  
  #guides(fill = guide_legend(title = "Value")) +
  
  scale_y_continuous(#"Gesamtzahl der Fälle pro DiagnosticReportLab",
    limits = c(0, ceiling(max(df.DiagnosticReportLab.mv$count)/10)*10),
    #minor_breaks = NULL,
    breaks = seq(0, ceiling(max(df.DiagnosticReportLab.mv$count)/10)*10, 1)
  )

#p.drl.mv

################################################################################
# plot: df.diagnosticReportLab.rest values
################################################################################

#p.drl.rest <- ggplot(df.DiagnosticReportLab.rest, aes(x = rest_name, y = count, fill = text)) + 
#  geom_bar(stat = "identity") + coord_flip() +
#  
#  ggtitle("Additional Data Quality Checks") +
#  ylab("Number of values") +
#  xlab(" ") +
#  
#  theme_minimal() +
#  
#  theme(
#    plot.title = element_text(face = "bold", size=15, hjust=0.5),
#    axis.title.x = element_text(margin = margin(b=7), hjust = 0.5, vjust = -4),
#    axis.title.y = element_text(margin = margin(b=7), hjust = 0.5, vjust = 3),
#    axis.text.x = element_text(size = 9, angle = 45, vjust = 1, hjust=1),
#    axis.text.y = element_text(size = 11)
#    #legend.position = "none" # Removes the legend
#  ) +
#  scale_fill_manual(labels = c("Not available", "Null"),
#                    values = c("#C5DFED","#3C8DBC")) +
#  
#  guides(fill = guide_legend(title = "Value")) +
#  
#  scale_y_continuous(#"Gesamtzahl der Fälle pro DiagnosticReportLab",
#    limits = c(0, ceiling(max(df.DiagnosticReportLab.mv$count)/10)*10),
#    #minor_breaks = NULL,
#    breaks = seq(0, ceiling(max(df.DiagnosticReportLab.mv$count)/10)*10, 1)
#  )
#
#p.drl.rest

################################################################################
# add 2 plots in grid
################################################################################

#p.drl.grid <- list(p.drl.mv, p.drl.icv)

#p.drl.grid <- plot_grid(plotlist = p.drl.grid,
#                        ncol=1,
#                        align="hv",
#                        rel_heights = c(3.5/5, 1.5/5))

#use package cowplot
#p.drl.grid <- plot_grid(p.drl.mv, p.drl.rest,
#                         align = "hv", ncol = 1, rel_heights = c(9.5/12, 2.5/12))

#p.drl.grid






