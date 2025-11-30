# Extract demographic information per respondent
# This script extracts age, gender, and education level for each respondent
library(tidyverse)

# Read the survey data
survey_data <- read.csv("Questionario Lab (Risposte) - Risposte del modulo 1.csv", 
                        stringsAsFactors = FALSE,
                        fileEncoding = "UTF-8")

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
cat("=== DEMOGRAPHIC INFORMATION SUMMARY ===\n\n")

cat("Total respondents:", nrow(demographic_info), "\n\n")

cat("Age distribution:\n")
print(table(demographic_info$Age_Group))
cat("\n")

cat("Gender distribution:\n")
print(table(demographic_info$Gender))
cat("\n")

cat("Education level distribution:\n")
print(table(demographic_info$Education_Level))
cat("\n")

# Preview the data
cat("=== FIRST 10 RESPONDENTS ===\n")
print(head(demographic_info, 10))

# Save to CSV file
write.csv(demographic_info, 
          "demographic_information.csv", 
          row.names = FALSE,
          fileEncoding = "UTF-8")

cat("\n=== File 'demographic_information.csv' created successfully! ===\n")
