# Author: Sherry Freiesleben
# Email: sherry.freiesleben@med.uni-rostock.de
# Creation date (YYYY-MM-DD): 2026-03-24
# License: Attribution-NonCommercial 4.0 International (CC BY-NC 4.0)
# Script purpose:
# 1) create plots related to the condition resource
# Notes:
# 1) This script is part of an RProject
# 2) the ui.R and server.R files need to be in the working directory 

################################################################################
# create data frames needed for plots
################################################################################

#create df containing all info
dqa.check.names <- sort(unique(df.Condition$dqaName))

c.name <- c("dqaName",
            "text",
            "count")

r.name <- c()

df.Condition.info <- data.frame(matrix(ncol = length(c.name),
                                     nrow = length(r.name),
                                     dimnames = list(r.name, c.name)))

for(i in 1:length(dqa.check.names)){
  name <- dqa.check.names[i]
  name <- paste0("^", name, "$")
  
  rows <- grep(name, df.Condition$dqaName)
  
  #print(paste0(i, ": ", rows))
  
  if(length(rows) == 1){
    df.info <- df.Condition[rows,]
    
    if(df.info$text == "true"){
      df.info.f <- data.frame("dqaName" = df.info$dqaName,
                              "text" = "false",
                              "count"= 0)
      
      df.Condition.info <- rbind(df.Condition.info, df.info, df.info.f)
    }
    
    if(df.info$text == "false"){
      df.info.t <- data.frame("dqaName" = df.info$dqaName,
                              "text" = "true",
                              "count"= 0)
      
      df.Condition.info <- rbind(df.Condition.info, df.info.t,  df.info)
    }

    
    if((df.info$text!="true") && (df.info$text!="false")){
      #print(paste0(i, ": ", rows))
      next
    }
    
  }
  
  if(length(rows) == 2){
    df.info <- df.Condition[rows,]
    dt <- grep("dateTime", df.info$text)
    #print(paste0(i, ": ", rows))
    
    if(length(dt)> 0){
      next
    }
    
    df.Condition.info <- rbind(df.Condition.info, df.info)
  }
  
  if((length(rows) != 1) && (length(rows) != 2)){
    #print(paste0(i, ": ", rows))
    next
  }
}

#create df containing missing values
df.Condition.mv <- df.Condition.info %>%
  filter(grepl("Missing values", dqaName)) %>%
  mutate(mv_name = gsub("Missing values ", "", dqaName)) %>%
  mutate(mv_name = gsub("\\.", " ", mv_name)) %>%
  mutate(mv_name = gsub(":", ": ", mv_name)) %>%
  mutate(count = as.numeric(count))

df.Condition.mv$mv_name <- factor(df.Condition.mv$mv_name, levels = sort(unique(as.character(df.Condition.mv$mv_name)), decreasing = TRUE))

#create df for overall statistic for missing values
s.true <- sum(df.Condition.mv$count[grep("true", df.Condition.mv$text)])
s.false <- sum(df.Condition.mv$count[grep("false", df.Condition.mv$text)])
total <- s.true + s.false

df.Condition.mv.overall <- data.frame("sumTrue" = s.true,
                                    "percentTrue"= round((s.true/total*100), digits = 0),
                                    "sumFalse" = s.false,
                                    "percentFalse"= round((s.false/total*100), digits = 0),
                                    "total" = total,
                                    row.names = "Condition")

#create df for remain DQ check in df.Condition.info
df.Condition.rest <- df.Condition.info %>%
  filter(!grepl("Missing values", dqaName)) %>%
  replace_na(list(text = "Not available")) %>%
  mutate(rest_name = dqaName) %>%
  mutate(count = as.numeric(count))

df.Condition.rest$rest_name <- factor(df.Condition.rest$rest_name, levels = sort(unique(as.character(df.Condition.rest$rest_name)), decreasing = TRUE))

#create df containing inadmissible categorical values
#df.Condition.icv <- df.Condition.info %>%
#  filter(grepl("Inadmissible categorical values", dqaName)) %>%
#  mutate(icv_name = gsub("Inadmissible categorical values: ", "", dqaName)) %>%
#  mutate(icv_name = gsub("\\.", " ", icv_name)) %>%
#  mutate(icv_name = gsub(":", ": ", icv_name)) %>%
#  mutate(count = as.numeric(count))
#
#df.Condition.icv$icv_name <- factor(df.Condition.icv$icv_name, levels = sort(unique(as.character(df.Condition.icv$icv_name)), decreasing = TRUE))


################################################################################
# plot: missing values blue hue
################################################################################

p.cond.mv <- ggplot(df.Condition.mv, aes(x = mv_name, y = count, fill = text)) + 
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
  scale_fill_manual(labels = c("False", "True"),
                    values = c("#C5DFED","#3C8DBC")) +
  
  #guides(fill = guide_legend(title = "Value")) +
  
  scale_y_continuous(#"Gesamtzahl der Fälle pro Condition",
    limits = c(0, floor(max(df.Condition.mv$count)/1000)*1000+500),
    #minor_breaks = NULL,
    breaks = seq(0, floor(max(df.Condition.mv$count)/1000)*1000+500, 1000)
  )

#p.cond.mv

################################################################################
# plot: df.condition.rest values
################################################################################

p.cond.rest <- ggplot(df.Condition.rest, aes(x = rest_name, y = count, fill = text)) + 
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
    axis.text.y = element_text(size = 11),
    legend.title = element_blank()
    #legend.position = "none" # Removes the legend
  ) +
  scale_fill_manual(labels = c("Not available", "Null"),
                    values = c("#C5DFED","#3C8DBC")) +
  
  #guides(fill = guide_legend(title = "Value")) +
  
  scale_y_continuous(#"Gesamtzahl der Fälle pro Condition",
    limits = c(0, floor(max(df.Condition.rest$count)/1000)*1000+500),
    #minor_breaks = NULL,
    breaks = seq(0, floor(max(df.Condition.rest$count)/1000)*1000+500, 1000)
  )

#p.cond.rest


################################################################################
# add 2 plots in grid
################################################################################

#use package cowplot
p.cond.grid <- plot_grid(p.cond.mv, p.cond.rest,
                        align = "hv", ncol = 1, rel_heights = c(8/12, 4/12))

#p.cond.grid





