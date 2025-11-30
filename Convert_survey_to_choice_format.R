# Script per convertire i dati dal Google Form al formato per Choice-Based Conjoint Analysis
library(tidyverse)

# Funzione per convertire i dati del questionario al formato choice-based
convert_survey_to_choice_format <- function(input_file, output_file) {
  
  # Leggi il file CSV del questionario
  survey_data <- read.csv(input_file, sep = ",", header = TRUE, check.names = FALSE)
  
  # Numero di choice set (colonne con le scelte - escludi le prime 4 colonne demografiche)
  choice_columns <- 5:ncol(survey_data)
  n_sets <- length(choice_columns)
  
  # Definizione del design (10 set, 3 alternative ciascuno)
  design <- data.frame(
    set = c(rep(1, 3), rep(2, 3), rep(3, 3), rep(4, 3), rep(5, 3), 
            rep(6, 3), rep(7, 3), rep(8, 3), rep(9, 3), rep(10, 3)),
    alt = rep(c("alt1", "alt2", "alt3"), 10),
    spec = c("Assistente", "Codice", "Content",
             "Content", "Assistente", "Codice",
             "Codice", "Content", "Assistente",
             "Assistente", "Codice", "Content",
             "Codice", "Assistente", "Codice",
             "Codice", "Content", "Assistente",
             "Codice", "Assistente", "Content",
             "Codice", "Content", "Assistente",
             "Codice", "Assistente", "Content",
             "Assistente", "Content", "Content"),
    vel = c("Lento", "Lento", "Veloce",
            "Veloce", "Veloce", "Lento",
            "Lento", "Lento", "Veloce",
            "Lento", "Veloce", "Veloce",
            "Lento", "Lento", "Veloce",
            "Lento", "Veloce", "Veloce",
            "Lento", "Lento", "Veloce",
            "Veloce", "Lento", "Veloce",
            "Veloce", "Lento", "Lento",
            "Veloce", "Lento", "Lento"),
    qual = c("Ottimale", "Sufficente", "Ottimale",
             "Ottimale", "Ottimale", "Sufficente",
             "Sufficente", "Ottimale", "Sufficente",
             "Sufficente", "Sufficente", "Ottimale",
             "Sufficente", "Sufficente", "Ottimale",
             "Ottimale", "Ottimale", "Sufficente",
             "Ottimale", "Ottimale", "Sufficente",
             "Ottimale", "Ottimale", "Sufficente",
             "Sufficente", "Ottimale", "Sufficente",
             "Sufficente", "Ottimale", "Sufficente"),
    priv = c("Bassa", "Alta", "Bassa",
             "Bassa", "Bassa", "Alta",
             "Bassa", "Alta", "Alta",
             "Alta", "Bassa", "Alta",
             "Alta", "Bassa", "Alta",
             "Bassa", "Bassa", "Alta",
             "Bassa", "Alta", "Alta",
             "Alta", "Alta", "Bassa",
             "Bassa", "Bassa", "Alta",
             "Bassa", "Bassa", "Alta"),
    cost = c(25, 20, 15,
             25, 20, 15,
             20, 15, 25,
             20, 15, 25,
             25, 15, 20,
             15, 20, 25,
             25, 25, 20,
             25, 20, 15,
             25, 20, 15,
             15, 20, 25)
  )
  
  # Mappa per convertire le risposte del form alle alternative
  create_choice_map <- function(spec, vel, qual, priv, cost) {
    # Converti i nomi come appaiono nel form
    spec_form <- ifelse(spec == "Content", "Prod. di contenuti", 
                       ifelse(spec == "Assistente", "Assistente",
                              ifelse(spec == "Codice", "Codice", spec)))
    qual_form <- ifelse(qual == "Sufficente", "Sufficiente", qual)
    
    paste(spec_form, vel, qual_form, priv, paste0(cost, "€"), sep = " - ")
  }
  
  # Crea una mappa per identificare le alternative
  design$response_text <- create_choice_map(
    design$spec, 
    design$vel, 
    design$qual, 
    design$priv, 
    design$cost
  )
  
  # Inizializza il dataframe finale
  choice_data <- data.frame()
  
  # Per ogni rispondente
  for (resp_id in 1:nrow(survey_data)) {
    
    # Per ogni set di scelta
    for (set_id in 1:n_sets) {
      
      # Estrai la risposta per questo set
      response <- survey_data[resp_id, choice_columns[set_id]]
      
      # Trova le 3 alternative per questo set
      set_alternatives <- design[design$set == set_id, ]
      
      # Per ogni alternativa nel set
      for (alt_num in 1:3) {
        alt_data <- set_alternatives[alt_num, ]
        
        # Determina se questa alternativa è stata scelta
        # Normalizza la risposta per gestire variazioni di formato (spazi multipli, etc)
        response_normalized <- gsub("\\s+", " ", trimws(as.character(response)))
        alt_text_normalized <- gsub("\\s+", " ", trimws(alt_data$response_text))
        
        choice <- ifelse(response_normalized == alt_text_normalized, 1, 0)
        
        # Crea la riga per questo record
        row_data <- data.frame(
          resp.id = resp_id,
          ques = set_id,
          alt = alt_data$alt,
          spec = alt_data$spec,
          vel = alt_data$vel,
          qual = alt_data$qual,
          priv = alt_data$priv,
          cost = alt_data$cost,
          choice = choice
        )
        
        choice_data <- rbind(choice_data, row_data)
      }
    }
  }
  
  # Scrivi il file di output
  write.csv2(choice_data, output_file, row.names = FALSE, quote = FALSE)
  
  # Statistiche per il log
  cat("Conversione completata!\n")
  cat("Numero di rispondenti:", nrow(survey_data), "\n")
  cat("Numero di set di scelta:", n_sets, "\n")
  cat("Numero totale di osservazioni:", nrow(choice_data), "\n")
  
  # Verifica delle scelte per rispondente
  choices_per_resp <- choice_data %>%
    filter(choice == 1) %>%
    group_by(resp.id) %>%
    summarise(n_choices = n())
  
  cat("\nRiepilogo scelte per rispondente:\n")
  print(summary(choices_per_resp$n_choices))
  
  return(choice_data)
}

# Esegui la conversione
# input_file da sovrascrivere con il file scaricato dal google form
# output_file è il file di destinazione (viene sovrascrtitto ogni volta)
choice_data <- convert_survey_to_choice_format(
  input_file = "Questionario Lab (Risposte) - Risposte del modulo 1.csv", 
  output_file = "Choice_Data_Converted.csv"
)

# Visualizza le prime righe
head(choice_data, 30)

# Controllo con barplot dei rispondenti per vedere che non ci siano bug nella conversione 
barplot(table(choice_data$resp.id), 
        main = "Frequenza per Respondent", 
        xlab = "resp.id", 
        ylab = "Frequenza",
        las = 2)
