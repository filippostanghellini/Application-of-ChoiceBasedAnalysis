# Application of Choice-Based Conjoint Analysis

Questo repository contiene l'implementazione di un'analisi Choice-Based Conjoint per studiare le preferenze degli utenti riguardo alle funzionalità di assistenti AI (es. ChatGPT).

## Descrizione del Progetto

Il progetto utilizza la metodologia Choice-Based Conjoint Analysis per analizzare le preferenze degli utenti su cinque attributi principali di un assistente AI:

- **Specializzazione**: Assistente, Codice, Content
- **Velocità**: Lento, Veloce  
- **Qualità**: Sufficiente, Ottimale
- **Privacy**: Bassa, Alta
- **Costo**: 15€, 20€, 25€

## Struttura del Repository

### Script R
- `Design_Project.R`: Script per la generazione del design sperimentale ottimale
- `Convert_survey_to_choice_format.R`: Script per convertire i dati dal Google Form al formato per l'analisi
- `Extract_info_per_respondent.R`: Script per estrarre le informazioni demografiche dei rispondenti

### Dati
- `raw_data/`: Cartella contenente i dati grezzi
  - `Questionario Lab (Risposte) - Risposte del modulo 1.csv`: Risposte raccolte dal Google Form
- `data/`: Cartella contenente i dati elaborati per l'analisi
  - `Choice_Data_Converted.csv`: File delle risposte convertite per eseguire l'analisi
  - `demographic_information.csv`: Informazioni demografiche dei rispondenti (età, genere, titolo di studio)

## Contributors

- [@filippostanghellini](https://github.com/filippostanghellini)
- [@MolteniF](https://github.com/MolteniF)
- [@01andreabattaglia](https://github.com/01andreabattaglia)
- [@matteo-massari](https://github.com/matteo-massari)
- [@TommasoCestari](https://github.com/TommasoCestari)
