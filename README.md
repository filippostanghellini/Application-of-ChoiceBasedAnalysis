# Application of Choice-Based Conjoint Analysis

This repository contains the implementation of a Choice-Based Conjoint Analysis to study user preferences regarding AI assistant features (e.g., ChatGPT).

## Project Description

The project uses the Choice-Based Conjoint Analysis methodology to analyze user preferences across five main attributes of an AI assistant:

| Attribute | Levels |
|-----------|--------|
| **Specialization** | Assistant, Code, Content |
| **Speed** | Slow, Fast |
| **Quality** | Sufficient, Optimal |
| **Privacy** | Low, High |
| **Cost** | 15€, 20€, 25€ |

## Repository Structure

```
├── Analisi.R                           # Main analysis script
├── Design_Project.R                    # Experimental design generation
├── Convert_survey_to_choice_format.R   # Google Form data conversion
├── Extract_info_per_respondent.R       # Demographic info extraction
├── data/
│   ├── Choice_Data_Converted.csv       # Converted data for analysis
│   └── demographic_information.csv     # Respondent demographic info
└── raw_data/
    └── Questionario Lab (Risposte).csv # Raw responses from Google Form
```

### R Scripts

| Script | Description |
|--------|-------------|
| `Analisi.R` | Main data analysis script using Multinomial Logit and Mixed Multinomial Logit models |
| `Design_Project.R` | Optimal experimental design generation |
| `Convert_survey_to_choice_format.R` | Conversion of Google Form data into the format required for analysis |
| `Extract_info_per_respondent.R` | Extraction of respondent demographic information |

### Data

- **`raw_data/`**: Raw data collected from the Google Form
- **`data/`**: Processed data ready for analysis
  - `Choice_Data_Converted.csv`: Responses converted into the format required for Conjoint Analysis
  - `demographic_information.csv`: Demographic information (age, gender, education level)

## Contributors

- [@filippostanghellini](https://github.com/filippostanghellini)
- [@MolteniF](https://github.com/MolteniF)
- [@01andreabattaglia](https://github.com/01andreabattaglia)
- [@matteo-massari](https://github.com/matteo-massari)
- [@TommasoCestari](https://github.com/TommasoCestari)
