# Extract demographic information per respondent
# This script extracts age, gender, and education level for each respondent
library(tidyverse)

# Read the survey data
survey_data <- read.csv("raw_data/Questionario Lab (Risposte) - Risposte del modulo 1.csv", 
                        stringsAsFactors = FALSE,
                        fileEncoding = "UTF-8")

# Rimuovi le righe dove la risposta alla SCELTA 5 è "Codice - Lento - Sufficiente - Alta - 25€"
choice5_col <- "Specializzazione...Velocità...Qualità...Privacy...Costo...SCELTA.5."
invalid_response <- "Codice - Lento - Sufficiente - Alta - 25€"

# Usa grepl per un matching più robusto
rows_to_keep <- !grepl(invalid_response, survey_data[[choice5_col]], fixed = TRUE)
rows_before <- nrow(survey_data)
survey_data <- survey_data[rows_to_keep, ]
rows_removed <- rows_before - nrow(survey_data)

if (rows_removed > 0) {
  cat("Rimosse", rows_removed, "righe con risposta non valida alla SCELTA 5\n")
}

# Extract demographic columns
demographic_info <- survey_data %>%
  select(
    Timestamp = 1,  # First column is timestamp
    Age = 2,        # "A quale fascia d'età appartieni?"
    Gender = 3,     # "Con quale genere ti identifichi?"
    Education = 4   # "Qual è il tuo titolo di studio (o che stai conseguendo)?"
  ) %>%
  mutate(
    Respondent_ID = row_number()
  ) %>%
  select(Respondent_ID, Timestamp, Age, Gender, Education)

# Rename columns to English for clarity
colnames(demographic_info) <- c(
  "Respondent_ID",
  "Timestamp",
  "Age_Group",
  "Gender",
  "Education_Level"
)

# Display summary
cat("Total respondents:", nrow(demographic_info), "\n\n")

# Preview the data
print(head(demographic_info, 10))

# Save to CSV file
write.csv(demographic_info, 
          "data/demographic_information.csv", 
          row.names = FALSE,
          fileEncoding = "UTF-8")

