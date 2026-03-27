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
# obtain missing values data frames from environment
################################################################################

#combine data frames containing missing values overall information
list.mv_overall <- Filter(function(x) is(x, "data.frame"), mget(ls(pattern = ".mv\\.overall")))

df.mv_overall <- bind_rows(list.mv_overall, .id = "total")

#percent.mv <- df.mv.overall$percentTrue
#percen.mv <- paste0(percent.mv, "%")

#create data frame to store data of missing values
c.name <- c("name",
            "percent",
            "count")

r.name <- c()

df.mv.combined <- data.frame(matrix(ncol = length(c.name),
                                      nrow = length(r.name),
                                      dimnames = list(r.name, c.name)))

#store data pertaining to missing values
for(i in 1:nrow(df.mv_overall)){
  name <- rownames(df.mv_overall)[i]
  ptrue <- df.mv_overall$percentTrue[i]
  pfalse <- df.mv_overall$percentFalse[i]
  
  df.info.t <- data.frame("name" = name,
                          "percent" = "Percent True",
                          "count"= ptrue)
  
  df.info.f <- data.frame("name" = name,
                          "percent" = "Percent False",
                          "count"= pfalse)
  
  df.mv.combined <- rbind(df.mv.combined, df.info.t,  df.info.f)
}


df.mv.combined$name <- factor(df.mv.combined$name, levels = sort(unique(as.character(df.mv.combined$name)), decreasing = TRUE))

df.mv.combined <- df.mv.combined %>%
  mutate(percent_text = paste0(count, "%"))

################################################################################
# plot: missing values blue hue
################################################################################

p.mv.combined <- ggplot(df.mv.combined, aes(x = name, y = count, fill = percent)) + 
  geom_bar(stat = "identity") + coord_flip() +
  
  ggtitle("Percent of Missing Values") +
  #ylab("Number of values") +
  xlab(" ") +
  
  theme_minimal() +
  
  theme(
    plot.title = element_text(face = "bold", size=20, hjust=0.5),
    #axis.title.x = element_text(margin = margin(b=7), hjust = 0.5, vjust = -4),
    axis.title.y = element_text(margin = margin(b=7), hjust = 0.5, vjust = 3),
    axis.text.x = element_text(size = 11, angle = 45, vjust = 1, hjust=1),
    axis.text.y = element_text(size = 16),
    axis.title.x = element_blank(),
    legend.title = element_blank()
    #legend.position = "none" # Removes the legend
  ) +
  
  scale_fill_manual(labels = c("False", "True"),
                    values = c("#C5DFED","#3C8DBC")) +
  
  #guides(fill = guide_legend(title = "Value")) +
  
  geom_text(aes(label = ifelse(percent == "Percent True", percent_text, "")), 
            position = position_dodge(width = 0), vjust = 0.5, hjust = 1.2, size = 7) +
  
  scale_y_continuous(#"Gesamtzahl der Fälle pro Specimen",
    limits = c(0, 100),
    #minor_breaks = NULL,
    breaks = seq(0, 100, 5)
  )

#p.mv.combined
