# Author: Sherry Freiesleben
# Email: sherry.freiesleben@med.uni-rostock.de
# Creation date (YYYY-MM-DD): 2026-03-23
# License: Attribution-NonCommercial 4.0 International (CC BY-NC 4.0)
# Script purpose:
# 1) create plots related to the patient resource
# Notes:
# 1) This script is part of an RProject
# 2) the ui.R and server.R files need to be in the working directory 

################################################################################
# create data frames needed for plots
################################################################################

#create df containing all info
dqa.check.names <- sort(unique(df.Patient$dqaName))

c.name <- c("dqaName",
            "text",
            "count",
            "percent",
            "percent_text")

r.name <- c()

df.Patient.info <- data.frame(matrix(ncol = length(c.name),
                                       nrow = length(r.name),
                                       dimnames = list(r.name, c.name)))

for(i in 1:length(dqa.check.names)){
  name <- dqa.check.names[i]
  name <- paste0("^", name, "$")
  
  rows <- grep(name, df.Patient$dqaName)
  
  #print(paste0(i, ": ", rows))
  
  if(length(rows) == 1){
    df.info <- df.Patient[rows,]
    
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
      
      df.Patient.info <- rbind(df.Patient.info, df.info, df.info.f)
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
      
      df.Patient.info <- rbind(df.Patient.info, df.info.t, df.info)
    }
    
    
    if((df.info$text!="true") && (df.info$text!="false")){
      #print(paste0(i, ": ", rows))
      next
    }
    
  }
  
  if(length(rows) == 2){
    df.info <- df.Patient[rows,]
    df.info$count <- as.numeric(df.info$count)
    
    df.info <- df.info %>%
      mutate(percent = round(count/sum(count)*100, digits = 0)) %>%
      mutate(percent_text = paste0(percent, "%"))
    
    dt <- grep("dateTime", df.info$text)
    #print(paste0(i, ": ", rows))
    
    if(length(dt)> 0){
      next
    }
    
    df.Patient.info <- rbind(df.Patient.info, df.info)
  }
  
  if((length(rows) != 1) && (length(rows) != 2)){
    #print(paste0(i, ": ", rows))
    next
  }
}


#create df containing missing values
df.Patient.mv <- df.Patient.info %>%
  filter(grepl("Missing values", dqaName)) %>%
  mutate(mv_name = gsub("Missing values: ", "", dqaName)) %>%
  mutate(mv_name = gsub("\\.", " ", mv_name)) %>%
  mutate(mv_name = gsub(":", ": ", mv_name)) %>%
  mutate(count = as.numeric(count))

df.Patient.mv$mv_name <- factor(df.Patient.mv$mv_name, levels = sort(unique(as.character(df.Patient.mv$mv_name)), decreasing = TRUE))

#create df for overall statistic for missing values
s.true <- sum(df.Patient.mv$count[grep("true", df.Patient.mv$text)])
s.false <- sum(df.Patient.mv$count[grep("false", df.Patient.mv$text)])
total <- s.true + s.false

df.Patient.mv.overall <- data.frame("sumTrue" = s.true,
                                    "percentTrue"= round((s.true/total*100), digits = 0),
                                    "sumFalse" = s.false,
                                    "percentFalse"= round((s.false/total*100), digits = 0),
                                    "total" = total,
                                    row.names = "Patient")

#create df containing inadmissible categorical values
df.Patient.icv <- df.Patient.info %>%
  filter(grepl("Inadmissible categorical values", dqaName)) %>%
  mutate(icv_name = gsub("Inadmissible categorical values: ", "", dqaName)) %>%
  mutate(icv_name = gsub("\\.", " ", icv_name)) %>%
  mutate(icv_name = gsub(":", ": ", icv_name)) %>%
  mutate(count = as.numeric(count))

df.Patient.icv$icv_name <- factor(df.Patient.icv$icv_name, levels = sort(unique(as.character(df.Patient.icv$icv_name)), decreasing = TRUE))

################################################################################
# plot: missing values blue hue
################################################################################

p.pat.mv <- ggplot(df.Patient.mv, aes(x = mv_name, y = count, fill = text)) + 
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
    axis.text.y = element_text(size = 13),
    axis.title.x = element_blank(),
    legend.title = element_blank()
    #legend.position = "none" # Removes the legend
  ) +
  scale_fill_manual(labels = c("False", "True"),
    values = c("#C5DFED","#3C8DBC")) +
  
  geom_text(aes(label = ifelse(text == "true", percent_text, "")), 
            position = position_dodge(width = 0), vjust = 0.5, hjust = -0.1, size = 5) +
  
  #guides(fill = guide_legend(title = "Value")) +
  
  scale_y_continuous(#"Gesamtzahl der Fälle pro Patient",
                     limits = c(0, floor(max(df.Patient.mv$count)/100000)*100000+150000),
                     #minor_breaks = NULL,
                     breaks = seq(0, floor(max(df.Patient.mv$count)/100000)*100000+150000, 50000)
                     )

#p.pat.mv

################################################################################
# plot: missing values red green
################################################################################

#p.pat.mv.rg <- ggplot(df.Patient.mv, aes(x = mv_name, y = count, fill = text)) + 
#  geom_bar(stat = "identity") + coord_flip() +
#  
#  ggtitle("Missing Values") +
#  ylab("Count") +
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
#  scale_fill_manual(labels = c("False", "True"),
#                    values = c("red2", "green3")) +
#  
#  guides(fill = guide_legend(title = "Value")) +
#  
#  scale_y_continuous(#"Gesamtzahl der Fälle pro Patient",
#    limits = c(0, floor(max(df.Patient.mv$count)/1000)*1000+500),
#    #minor_breaks = NULL,
#    breaks = seq(0, floor(max(df.Patient.mv$count)/1000)*1000+500, 1000)
#  )
#
#p.pat.mv.rg

################################################################################
# plot: Inadmissible categorical values
################################################################################

p.pat.icv <- ggplot(df.Patient.icv, aes(x = icv_name, y = count, fill = text)) + 
  geom_bar(stat = "identity") + coord_flip() +
  
  ggtitle("Inadmissible Categorical Values") +
  ylab("Number of values") +
  xlab(" ") +
  
  theme_minimal() +
  
  theme(
    plot.title = element_text(face = "bold", size=15, hjust=0.5),
    axis.title.x = element_text(margin = margin(b=7), hjust = 0.5, vjust = -4),
    axis.title.y = element_text(margin = margin(b=7), hjust = 0.5, vjust = 3),
    axis.text.x = element_text(size = 9, angle = 45, vjust = 1, hjust=1),
    axis.text.y = element_text(size = 11),
    legend.title = element_blank()
    #legend.position = "none" # Removes the legend
  ) +
  scale_fill_manual(labels = c("False", "True"),
                    values = c("#C5DFED","#3C8DBC")) +
  
  #guides(fill = guide_legend(title = "Value")) +
  
  scale_y_continuous(#"Gesamtzahl der Fälle pro Patient",
    limits = c(0, floor(max(df.Patient.mv$count)/100000)*100000+150000),
    #minor_breaks = NULL,
    breaks = seq(0, floor(max(df.Patient.mv$count)/100000)*100000+150000, 50000)
  )

#p.pat.icv


################################################################################
# add 2 plots in grid
################################################################################

#p.pat.grid <- list(p.pat.mv, p.pat.icv)

#p.pat.grid <- plot_grid(plotlist = p.pat.grid,
#                        ncol=1,
#                        align="hv",
#                        rel_heights = c(3.5/5, 1.5/5))

#use package cowplot
p.pat.grid <- plot_grid(p.pat.mv, p.pat.icv,
                        align = "hv", ncol = 1, rel_heights = c(3.9/5, 1.1/5))




#p.pat.grid


