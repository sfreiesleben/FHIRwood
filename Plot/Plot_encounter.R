# Author: Sherry Freiesleben
# Email: sherry.freiesleben@med.uni-rostock.de
# Creation date (YYYY-MM-DD): 2026-03-25
# License: Attribution-NonCommercial 4.0 International (CC BY-NC 4.0)
# Script purpose:
# 1) create plots related to the encounter resource
# Notes:
# 1) This script is part of an RProject
# 2) the ui.R and server.R files need to be in the working directory 

################################################################################
# create data frames needed for plots
################################################################################

#create df containing all info
dqa.check.names <- sort(unique(df.Encounter$dqaName))

c.name <- c("dqaName",
            "text",
            "count",
            "percent",
            "percent_text")

r.name <- c()

df.Encounter.info <- data.frame(matrix(ncol = length(c.name),
                                     nrow = length(r.name),
                                     dimnames = list(r.name, c.name)))

for(i in 1:length(dqa.check.names)){
  name <- dqa.check.names[i]
  name <- paste0("^", name, "$")
  
  rows <- grep(name, df.Encounter$dqaName)
  
  #print(paste0(i, ": ", rows))
  
  if(length(rows) == 1){
    df.info <- df.Encounter[rows,]
    
    #if true exist implies that false does not exist and it has a 0 count
    #please check this in the future if all checks are true/false paired, this information is not available at the moment
    if(df.info$text == "true"){
      df.info.f <- data.frame("dqaName" = df.info$dqaName,
                              "text" = "false",
                              "count"= 0,
                              "percent" = 0,
                              "percent_text" = "0%")
      df.info$percent = 100
      df.info$percent_text = "100%"
      
      df.Encounter.info <- rbind(df.Encounter.info, df.info, df.info.f)
    }
    
    #if false exist implies that true does not exist and it has a 0 count
    #please check this in the future if all checks are true/false paired, this information is not available at the moment
    if(df.info$text == "false"){
      df.info.t <- data.frame("dqaName" = df.info$dqaName,
                              "text" = "true",
                              "count"= 0,
                              "percent" = 0,
                              "percent_text" = "0%")
      
      df.info$percent = 100
      df.info$percent_text = "100%"
      
      df.Encounter.info <- rbind(df.Encounter.info, df.info.t, df.info)
    }
    
    
    if((df.info$text!="true") && (df.info$text!="false")){
      #print(paste0(i, ": ", rows))
      next
    }
    
  }
  
  if(length(rows) == 2){
    df.info <- df.Encounter[rows,]
    df.info$count <- as.numeric(df.info$count)
    
    df.info <- df.info %>%
      mutate(percent = round(count/sum(count)*100, digits = 0)) %>%
      mutate(percent_text = paste0(percent, "%"))
    
    dt <- grep("dateTime", df.info$text)
    #print(paste0(i, ": ", rows))
    
    if(length(dt)> 0){
      next
    }
    
    df.Encounter.info <- rbind(df.Encounter.info, df.info)
  }
  
  if((length(rows) != 1) && (length(rows) != 2)){
    #print(paste0(i, ": ", rows))
    next
  }
}
#create df containing missing values
df.Encounter.mv <- df.Encounter.info %>%
  filter(grepl("Missing values", dqaName)) %>%
  mutate(mv_name = gsub("Missing values ", "", dqaName)) %>%
  mutate(mv_name = gsub("\\.", " ", mv_name)) %>%
  mutate(mv_name = gsub(":", ": ", mv_name)) %>%
  mutate(count = as.numeric(count))

df.Encounter.mv$mv_name <- factor(df.Encounter.mv$mv_name, levels = sort(unique(as.character(df.Encounter.mv$mv_name)), decreasing = TRUE))

#create df for overall statistic for missing values
s.true <- sum(df.Encounter.mv$count[grep("true", df.Encounter.mv$text)])
s.false <- sum(df.Encounter.mv$count[grep("false", df.Encounter.mv$text)])
total <- s.true + s.false

df.Encounter.mv.overall <- data.frame("sumTrue" = s.true,
                                    "percentTrue"= round((s.true/total*100), digits = 0),
                                    "sumFalse" = s.false,
                                    "percentFalse"= round((s.false/total*100), digits = 0),
                                    "total" = total,
                                    row.names = "Encounter")

#create df for remain DQ check in df.Encounter.info
df.Encounter.rest <- df.Encounter.info %>%
  filter(!grepl("Missing values|Inadmissible categorical values", dqaName)) %>%
  replace_na(list(text = "Not available")) %>%
  mutate(rest_name = dqaName) %>%
  mutate(count = as.numeric(count))

df.Encounter.rest$rest_name <- factor(df.Encounter.rest$rest_name, levels = sort(unique(as.character(df.Encounter.rest$rest_name)), decreasing = TRUE))

#create df containing inadmissible categorical values
df.Encounter.icv <- df.Encounter.info %>%
  filter(grepl("Inadmissible categorical values", dqaName)) %>%
  mutate(icv_name = gsub("Inadmissible categorical values", "", dqaName)) %>%
  mutate(icv_name = gsub("\\.", " ", icv_name)) %>%
  mutate(icv_name = gsub(":", ": ", icv_name)) %>%
  mutate(count = as.numeric(count))

df.Encounter.icv$icv_name <- factor(df.Encounter.icv$icv_name, levels = sort(unique(as.character(df.Encounter.icv$icv_name)), decreasing = TRUE))

################################################################################
# plot: missing values blue hue
################################################################################

p.enc.mv <- ggplot(df.Encounter.mv, aes(x = mv_name, y = count, fill = text)) + 
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
    axis.text.y = element_text(size = 13),
    axis.title.x = element_blank(),
    legend.title = element_blank()
    #legend.position = "none" # Removes the legend
  ) +
  scale_fill_manual(labels = c("False", "True"), #make sure this is correct!
                    values = c("#C5DFED","#3C8DBC")) +
  
  #guides(fill = guide_legend(title = "Value")) +
  
  geom_text(aes(label = ifelse(text == "true", percent_text, "")), 
            position = position_dodge(width = 0), vjust = 0.5, hjust = -0.1, size = 5) +
  
  scale_y_continuous(#"Gesamtzahl der Fälle pro Encounter",
    limits = c(0, floor(max(df.Encounter.mv$count)/1000)*1000+1000),
    #minor_breaks = NULL,
    breaks = seq(0, floor(max(df.Encounter.mv$count)/1000)*1000+1000, 1000)
  )

#p.enc.mv

################################################################################
# plot: df.encounter.rest values
################################################################################

p.enc.rest <- ggplot(df.Encounter.rest, aes(x = rest_name, y = count, fill = text)) + 
  geom_bar(stat = "identity") + coord_flip() +
  
  ggtitle("Additional Data Quality Checks") +
  ylab("Number of values") +
  xlab(" ") +
  
  theme_minimal() +
  
  theme(
    plot.title = element_text(face = "bold", size=15, hjust=0.5),
    #axis.title.x = element_text(margin = margin(b=7), hjust = 0.5, vjust = -4),
    axis.title.y = element_text(margin = margin(b=7), hjust = 0.5, vjust = 3),
    axis.text.x = element_text(size = 9, angle = 45, vjust = 1, hjust=1),
    axis.text.y = element_text(size = 13),
    axis.title.x = element_blank(),
    legend.title = element_blank()
    #legend.position = "none" # Removes the legend
  ) +
  scale_fill_manual(labels = c("False", "True"), #make sure this is correct!
                    values = c("#C5DFED","#3C8DBC")) +
  
  guides(fill = guide_legend(title = "Value")) +
  
  scale_y_continuous(#"Gesamtzahl der Fälle pro Encounter",
    limits = c(0, floor(max(df.Encounter.rest$count)/1000)*1000+1000),
    #minor_breaks = NULL,
    breaks = seq(0, floor(max(df.Encounter.rest$count)/1000)*1000+1000, 1000)
  )

#p.enc.rest


################################################################################
# add 2 plots in grid
################################################################################

#use package cowplot
p.enc.grid <- plot_grid(p.enc.mv, p.enc.rest,
                        align = "hv", ncol = 1, rel_heights = c(10.5/12, 1.5/12))

#p.enc.grid





