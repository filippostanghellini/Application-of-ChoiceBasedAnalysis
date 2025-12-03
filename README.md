# Application of Choice-Based Conjoint Analysis

Questo repository contiene l'implementazione di un'analisi Choice-Based Conjoint per studiare le preferenze degli utenti riguardo alle funzionalità di assistenti AI (es. ChatGPT).

## Descrizione del Progetto

Il progetto utilizza la metodologia Choice-Based Conjoint Analysis per analizzare le preferenze degli utenti su cinque attributi principali di un assistente AI:

| Attributo | Livelli |
|-----------|---------|
| **Specializzazione** | Assistente, Codice, Content |
| **Velocità** | Lento, Veloce |
| **Qualità** | Sufficiente, Ottimale |
| **Privacy** | Bassa, Alta |
| **Costo** | 15€, 20€, 25€ |

## Struttura del Repository

```
├── Analisi.R                           # Script principale per l'analisi
├── Design_Project.R                    # Generazione del design sperimentale
├── Convert_survey_to_choice_format.R   # Conversione dati Google Form
├── Extract_info_per_respondent.R       # Estrazione info demografiche
├── data/
│   ├── Choice_Data_Converted.csv       # Dati convertiti per l'analisi
│   └── demographic_information.csv     # Info demografiche rispondenti
└── raw_data/
    └── Questionario Lab (Risposte).csv # Risposte grezze dal Google Form
```

### Script R

| Script | Descrizione |
|--------|-------------|
| `Analisi.R` | Script principale per l'analisi dei dati con modelli Multinomial Logit e Mixed Multinomial Logit |
| `Design_Project.R` | Generazione del design sperimentale ottimale |
| `Convert_survey_to_choice_format.R` | Conversione dei dati dal Google Form al formato per l'analisi |
| `Extract_info_per_respondent.R` | Estrazione delle informazioni demografiche dei rispondenti |

### Dati

- **`raw_data/`**: Dati grezzi raccolti dal Google Form
- **`data/`**: Dati elaborati pronti per l'analisi
  - `Choice_Data_Converted.csv`: Risposte convertite nel formato richiesto per la Conjoint Analysis
  - `demographic_information.csv`: Informazioni demografiche (età, genere, titolo di studio)

## Contributors

- [@filippostanghellini](https://github.com/filippostanghellini)
- [@MolteniF](https://github.com/MolteniF)
- [@01andreabattaglia](https://github.com/01andreabattaglia)
- [@matteo-massari](https://github.com/matteo-massari)
- [@TommasoCestari](https://github.com/TommasoCestari)
