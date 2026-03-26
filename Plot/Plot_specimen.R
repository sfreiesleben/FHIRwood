# Author: Sherry Freiesleben
# Email: sherry.freiesleben@med.uni-rostock.de
# Creation date (YYYY-MM-DD): 2026-03-23
# License: Attribution-NonCommercial 4.0 International (CC BY-NC 4.0)
# Script purpose:
# 1) create plots related to the specimen resource
# Notes:
# 1) This script is part of an RProject
# 2) the ui.R and server.R files need to be in the working directory 

################################################################################
# create data frames needed for plots
################################################################################

#create df containing all info
dqa.check.names <- sort(unique(df.Specimen$dqaName))

c.name <- c("dqaName",
            "text",
            "count")

r.name <- c()

df.Specimen.info <- data.frame(matrix(ncol = length(c.name),
                                     nrow = length(r.name),
                                     dimnames = list(r.name, c.name)))

for(i in 1:length(dqa.check.names)){
  name <- dqa.check.names[i]
  name <- paste0("^", name, "$")
  
  rows <- grep(name, df.Specimen$dqaName)
  
  #print(paste0(i, ": ", rows))
  
  if(length(rows) == 1){
    df.info <- df.Specimen[rows,]
    
    if(df.info$text == "true"){
      df.info.f <- data.frame("dqaName" = df.info$dqaName,
                              "text" = "false",
                              "count"= 0)
      
      df.Specimen.info <- rbind(df.Specimen.info, df.info, df.info.f)
    }
    
    if(df.info$text == "false"){
      df.info.t <- data.frame("dqaName" = df.info$dqaName,
                              "text" = "true",
                              "count"= 0)
      
      df.Specimen.info <- rbind(df.Specimen.info, df.info.t,  df.info)
    }
    
    if((df.info$text!="true") && (df.info$text!="false")){
      #print(paste0(i, ": ", rows))
      #it is unclear at the moment what to do with these
    }
    
  }
  
  if(length(rows) == 2){
    df.info <- df.Specimen[rows,]
    df.Specimen.info <- rbind(df.Specimen.info, df.info)
  }
  
  if((length(rows) != 1) && (length(rows) != 2)){
    #print(paste0(i, ": ", rows))
    #it is not yet clear what to do with these at the moment.
  }
}

#create df containing missing values
df.Specimen.mv <- df.Specimen.info %>%
  filter(grepl("Missing values", dqaName)) %>%
  mutate(mv_name = gsub("Missing values: ", "", dqaName)) %>%
  mutate(mv_name = gsub("\\.", " ", mv_name)) %>%
  mutate(mv_name = gsub(":", ": ", mv_name)) %>%
  mutate(count = as.numeric(count))

df.Specimen.mv$mv_name <- factor(df.Specimen.mv$mv_name, levels = sort(unique(as.character(df.Specimen.mv$mv_name)), decreasing = TRUE))

#create df for overall statistic for missing values
s.true <- sum(df.Specimen.mv$count[grep("true", df.Specimen.mv$text)])
s.false <- sum(df.Specimen.mv$count[grep("false", df.Specimen.mv$text)])
total <- s.true + s.false

df.Specimen.mv.overall <- data.frame("sumTrue" = s.true,
                                    "percentTrue"= round((s.true/total*100), digits = 0),
                                    "sumFalse" = s.false,
                                    "percentFalse"= round((s.false/total*100), digits = 0),
                                    "total" = total,
                                    row.names = "Specimen")

#create df containing inadmissible categorical values
df.Specimen.icv <- df.Specimen.info %>%
  filter(grepl("Inadmissible categorical values", dqaName)) %>%
  mutate(icv_name = gsub("Inadmissible categorical values: ", "", dqaName)) %>%
  mutate(icv_name = gsub("\\.", " ", icv_name)) %>%
  mutate(icv_name = gsub(":", ": ", icv_name)) %>%
  mutate(count = as.numeric(count))

df.Specimen.icv$icv_name <- factor(df.Specimen.icv$icv_name, levels = sort(unique(as.character(df.Specimen.icv$icv_name)), decreasing = TRUE))

#create df for remain DQ check in df.Specimen.info
df.Specimen.rest <- df.Specimen.info %>%
  filter(!grepl("Missing values|Inadmissible categorical values", dqaName)) %>%
  replace_na(list(text = "Not available")) %>%
  mutate(rest_name = dqaName) %>%
  mutate(count = as.numeric(count))

df.Specimen.rest$rest_name <- factor(df.Specimen.rest$rest_name, levels = sort(unique(as.character(df.Specimen.rest$rest_name)), decreasing = TRUE))

################################################################################
# plot: missing values blue hue
################################################################################

p.spe.mv <- ggplot(df.Specimen.mv, aes(x = mv_name, y = count, fill = text)) + 
  geom_bar(stat = "identity") + coord_flip() +
  
  ggtitle("Missing Values") +
  #ylab("Number of values") +
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
  scale_fill_manual(labels = c("False", "True"),
    values = c("#C5DFED","#3C8DBC")) +
  
  #guides(fill = guide_legend(title = "Value")) +
  
  scale_y_continuous(#"Gesamtzahl der Fälle pro Specimen",
                     limits = c(0, floor(max(df.Specimen.mv$count)/10)*10+2),
                     #minor_breaks = NULL,
                     breaks = seq(0, floor(max(df.Specimen.mv$count)/10)*10+2, 2)
                     )

#p.spe.mv

################################################################################
# plot: df.specimen.rest values
################################################################################

p.spe.rest <- ggplot(df.Specimen.rest, aes(x = rest_name, y = count, fill = text)) + 
  geom_bar(stat = "identity") + coord_flip() +
  
  ggtitle("Additional Data Quality Checks") +
  ylab("Number of values") +
  xlab(" ") +
  
  theme_minimal() +
  
  theme(
    plot.title = element_text(face = "bold", size=15, hjust=0.5),
    axis.title.x = element_text(margin = margin(b=7), hjust = 0.5, vjust = -4),
    axis.title.y = element_text(margin = margin(b=7), hjust = 0.5, vjust = 3),
    axis.text.x = element_text(size = 9, angle = 45, vjust = 1, hjust=1),
    axis.text.y = element_text(size = 11)
    #legend.position = "none" # Removes the legend
  ) +
  scale_fill_manual(labels = c("Not available", "Null"), #make sure this is correct!
                    values = c("#C5DFED","#3C8DBC")) +
  
  guides(fill = guide_legend(title = "Value")) +
  
  scale_y_continuous(#"Gesamtzahl der Fälle pro Specimen",
    limits = c(0, floor(max(df.Specimen.rest$count)/10)*10+2),
    #minor_breaks = NULL,
    breaks = seq(0, floor(max(df.Specimen.rest$count)/10)*10+2, 2)
  )

#p.spe.rest

################################################################################
# add 2 plots in grid
################################################################################

#use package cowplot
p.spe.grid <- plot_grid(p.spe.mv, p.spe.rest,
                        align = "hv", ncol = 1, rel_heights = c(3.9/5, 1.1/5))




p.spe.grid


