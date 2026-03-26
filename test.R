







################################################################################
# test
################################################################################


#method 1

l.path <- paste0("../DQA-CQL/reports/", list.files("../DQA-CQL/reports"))

l.path <- l.path[grep(".json", l.path)]

l.name <- gsub("../DQA-CQL/reports/", "", l.path)
l.name <- gsub(".json", "", l.name)

for(i in 1:length(l.name)){
  name <- l.name[i]
  name <- paste0("j.", name)
  
  assign(name, fromJSON(l.path[i]))
}



























test <- data$group$population
class(test)

df %>% 
  unnest(test) %>% 
  mutate()




df %>% 
  unnest(test) %>% 
  mutate(
    count = test.count,
    code = test.codingcode,
  ) %>% 
  select(-test)





################################################################################
# create list of files of raw Json files directly from online repository
# NOTE: this does not work
################################################################################

l.condition <- "https://raw.githubusercontent.com/medizininformatik-initiative/DQA-CQL/refs/heads/main/reports/Condition.json?token=GHSAT0AAAAAADWXXDR3U5TPUHRSSQ3HUZJC2N3U7WA"

l.consent <- "https://raw.githubusercontent.com/medizininformatik-initiative/DQA-CQL/refs/heads/main/reports/Consent.json?token=GHSAT0AAAAAADWXXDR2E4S6FY3STWZMAWDO2N3VASQ"

l.diagnosticReportLab <- "https://raw.githubusercontent.com/medizininformatik-initiative/DQA-CQL/refs/heads/main/reports/DiagnosticReportLab.json?token=GHSAT0AAAAAADWXXDR2XCYLV64V7EXYLEQA2N3VDKQ"

l.diagnosticReportPatho <- "https://raw.githubusercontent.com/medizininformatik-initiative/DQA-CQL/refs/heads/main/reports/DiagnosticReportPatho.json?token=GHSAT0AAAAAADWXXDR34MD3C3VNN7FBECJI2N3VEBQ"

l.encounter <- "https://raw.githubusercontent.com/medizininformatik-initiative/DQA-CQL/refs/heads/main/reports/Encounter.json?token=GHSAT0AAAAAADWXXDR3N2C5ZTH7OM7BL7US2N3VEJA"

l.medicationAdministration <- "https://raw.githubusercontent.com/medizininformatik-initiative/DQA-CQL/refs/heads/main/reports/MedicationAdministration.json?token=GHSAT0AAAAAADWXXDR3A4PT2LJB7G7IXB7M2N3VESA"

l.medicationList <- "https://raw.githubusercontent.com/medizininformatik-initiative/DQA-CQL/refs/heads/main/reports/MedicationList.json?token=GHSAT0AAAAAADWXXDR3HD5IPAANIXFUGZ4K2N3VFAQ"

l.medicationRequest <- "https://raw.githubusercontent.com/medizininformatik-initiative/DQA-CQL/refs/heads/main/reports/MedicationRequest.json?token=GHSAT0AAAAAADWXXDR3JRZJXQH6XEGPCTZG2N3VFJA"

l.medicationStatement <- "https://raw.githubusercontent.com/medizininformatik-initiative/DQA-CQL/refs/heads/main/reports/MedicationStatement.json?token=GHSAT0AAAAAADWXXDR3NEPB2K4MINYYZNPO2N3VFQQ"

l.observation <- "https://raw.githubusercontent.com/medizininformatik-initiative/DQA-CQL/refs/heads/main/reports/Observation.json?token=GHSAT0AAAAAADWXXDR22PAP2U6SZNYNENRI2N3VFXQ"

l.patient <- "https://raw.githubusercontent.com/medizininformatik-initiative/DQA-CQL/refs/heads/main/reports/Patient.json?token=GHSAT0AAAAAADWXXDR2WIFIAVHKJXAG6XKM2N3VGGQ"

l.procedure <- "https://raw.githubusercontent.com/medizininformatik-initiative/DQA-CQL/refs/heads/main/reports/Procedure.json?token=GHSAT0AAAAAADWXXDR36C3BFEWRBQAB5BXY2N3VGOA"

l.specimen <- "https://raw.githubusercontent.com/medizininformatik-initiative/DQA-CQL/refs/heads/main/reports/Specimen.json?token=GHSAT0AAAAAADWXXDR2VDQVQQ2ZQWLJM3DC2N3VGTQ"





