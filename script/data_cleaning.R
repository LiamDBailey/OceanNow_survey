library(readr)
library(dplyr)
library(ggplot2)

# CREATE QUESTION DICTIONARY ####

#Create a dictionary to combine german and english answers
dictionary <- data.frame(name = c("Have you seen the exhibition?", "Haben Sie die Ausstellung schon besichtigt?",
                                  "How long did you spend in the exhibition (minutes)?", "Wie lange haben Sie sich die Ausstellung angeschaut (in Minuten)?",
                                  "Did you scan the QR code at the exhibition?", "Haben Sie einen QR Code von der Ausstellung gescannt?",
                                  "Microplastic pollution is a major problem that needs to be addressed", "Die Verschmutzung durch Mikroplastik ist ein großes Problem, dass angegangen  werden muss.",
                                  "We as humans are able to act against microplastic pollution", "Wir Menschen haben die Möglichkeit, etwas gegen die Verschmutzung durch Mikroplastik zu tun.",
                                  "I have a personal responsibility to change my behaviour to avoid microplastic pollution", "Ich habe die Verantwortung, mein Verhalten zu ändern, um die Verschmutzung durch Mikroplastik zu vermeiden.",
                                  "Politicians have a responsibility to implement legislation to avoid microplastic pollution", "Die Politiker*innen sind dafür verantwortlich, Gesetze zu erlassen um die Verschmutzung durch Mikroplastik zu verhindern.",
                                  "Climate change is the most important problem facing our society", "Der Klimawandel ist das wichtigste Problem unserer Gesellschaft.",
                                  "I would be willing to pay 50% more for products that do not contain microplastics", "Ich wäre bereit, 50% mehr zu bezahlen für Produkte, die kein Mikroplastik enthalten.",
                                  "I would be willing to participate in collective actions to address microplastic pollution (e.g. demonstrations)", "Ich wäre bereit, an gemeinschaftlichen  Aktionen teilzunehmen, die sich gegen die Verschmutzung durch Mikroplastik richten (z.B. an einer Demonstration).",
                                  "Year of birth", "In welchem Jahr sind Sie geboren?",
                                  "Gender", "Mit welchem Geschlecht identifizieren Sie sich?",
                                  "What is your gender?", "Andere (individuell eingeben):...44",
                                  "What further actions would you be willing to take to reduce microplastic pollution?/Sign a petition about microplastic pollution", "Sind Sie bereit, sich weiter für das Reduzieren von Mikroplastik einzusetzen? (mehrere Antworten möglich)/Ich möchte eine Petition zum Verbot von Mikroplastik in Kosmetika unterschreiben.",
                                  "What further actions would you be willing to take to reduce microplastic pollution?/Join the Ocean. Now! newsletter to learn more", "Sind Sie bereit, sich weiter für das Reduzieren von Mikroplastik einzusetzen? (mehrere Antworten möglich)/Ich möchte den Newsletter von Ocean. Now! erhalten, um mehr zu erfahren.",
                                  "Code 1: First letter of your own first name (e.g. \"M\" for Mary)", "Code 1: Erster Buchstabe Ihres Vornamens (z.B. „P“ für Petra)",
                                  "Code 2: First letter of your mother's first name (e.g. \"P\" for Penelope; please put \"X\" if your mother's name is unknown)", "Code 2: Erster Buchstabe des Vornamens Ihrer Mutter (z.B. „E“ für Eva. Falls unbekannt, wählen „X“.)",
                                  "Code 3: First letter of your father's first name (e.g. \"T\" for Thomas; please put \"X\" if your mother's name is unknown)", "Code 3: Erster Buchstabe des Vornamens Ihres Vaters (z.B. „H“ für Harald. Falls unbekannt, wählen „X“.)",
                                  "Code 4: First letter of your place of birth (e.g. \"L\" for London; please put \"Z\" if your place of birth is unknown)", "Code 4: Erster Buchstage Ihres Geburtsortes (z.B. „B“ für Berlin. Falls unbekannt, wählen „X“.)"),
                         Q_language = c("ENG", "DEU",
                                        "ENG", "DEU",
                                        "ENG", "DEU",
                                        "ENG", "DEU",
                                        "ENG", "DEU",
                                        "ENG", "DEU",
                                        "ENG", "DEU",
                                        "ENG", "DEU",
                                        "ENG", "DEU",
                                        "ENG", "DEU",
                                        "ENG", "DEU",
                                        "ENG", "DEU",
                                        "ENG", "DEU",
                                        "ENG", "DEU",
                                        "ENG", "DEU",
                                        "ENG", "DEU",
                                        "ENG", "DEU",
                                        "ENG", "DEU",
                                        "ENG", "DEU"),
                         code = c("Treatment", "Treatment",
                                  "Time", "Time",
                                  "QR", "QR",
                                  "Microplastic_importance_LK", "Microplastic_importance_LK",
                                  "Viable_action_LK", "Viable_action_LK",
                                  "Personal_responsibility_LK", "Personal_responsibility_LK",
                                  "Political_responsibility_LK", "Political_responsibility_LK",
                                  "Climate_importance_LK", "Climate_importance_LK",
                                  "Willingness_price_LK", "Willingness_price_LK",
                                  "Willingness_community_LK", "Willingness_community_LK",
                                  "BirthYear", "BirthYear",
                                  "Gender", "Gender",
                                  "Gender_optional", "Gender_optional",
                                  "Petition", "Petition",
                                  "Newsletter", "Newsletter",
                                  "PersonalCode1", "PersonalCode1",
                                  "PersonalCode2", "PersonalCode2",
                                  "PersonalCode3", "PersonalCode3",
                                  "PersonalCode4", "PersonalCode4"))

# BERLIN DATA ####

(berlin_data <- readr::read_delim(here::here("I:/pwind/Nextcloud/Datascience/OceanNow_data/Ocean._Now_Berlin_-_all_versions_-_labels_-_2021-11-02-15-19-29.csv"), delim = ";",
                                  col_types = list(
                                    start = col_datetime(),
                                    end = col_datetime(),
                                    .default = col_character())) %>% 
    #Remove unneeded columns
    #Remove the columns with just text
    #Remove the columns that show 0/1 for each column. These cause problems between Kiel and Berlin
    dplyr::select(!contains("Thank") & !contains("Vielen Dank") &
                  !contains("Your questionnaire will receive") & !contains("Ihr Fragebogen wird") &
                  !contains(match = "What were the reasons you came to the exhibition?/") &
                  !contains(match = "Warum sind Sie zur Ausstellung gekommen?/")) %>% 
    #Convert 'Please choose' into language column
    dplyr::mutate(language = dplyr::case_when(stringr::str_detect(`Please choose/Bitte wählen Sie:`, "^I want") ~ "ENG",
                                              stringr::str_detect(`Please choose/Bitte wählen Sie:`, "^Ich") ~ "DEU")) %>% 
    #Pivot longer to deal with questions more easily and filter out Qs in wrong language
    tidyr::pivot_longer(cols = -c(`_id`, `_validation_status`, `_submission_time`, start, end, language)) %>% 
    #Add standard column codes
    dplyr::left_join(dictionary, by = "name") %>% 
    #Filter only relevant Qs for the language used
    dplyr::filter(language == Q_language & !is.na(code)) %>%
    dplyr::select(-name, -Q_language) %>% 
    tidyr::pivot_wider(names_from = code, values_from = value) %>% 
    #Convert Likert scale to values
    #Cols using Likert scale have suffix _LK
    dplyr::mutate(across(.cols = matches("_LK$"), .fns = function(answer){
      
      dplyr::case_when(answer %in% c("Strongly agree", "Stimme voll und ganz zu") ~ "7",
                       answer %in% c("Strongly disagree", "Stimme überhaupt nicht zu") ~ "1",
                       TRUE ~ answer) %>% 
        as.numeric()
      
    })) %>%
   #Personal code with NA should be X so not included in paste
   dplyr::mutate(across(.cols = contains("PersonalCode"), .fns = ~tidyr::replace_na(., replace = "X"))) %>% 
   dplyr::mutate(BirthYear = as.numeric(BirthYear),
                 Treatment = dplyr::case_when(Treatment %in% c("No", "Nein") ~ FALSE,
                                              Treatment %in% c("Yes", "Ja") ~ TRUE),
                 Gender = dplyr::case_when(Gender %in% c("Male", "Mann") ~ "Male",
                                           Gender %in% c("Female", "Frau") ~ "Female",
                                           TRUE ~ "Other"),
                 QR = dplyr::case_when(QR %in% c("No", "Nein") ~ FALSE,
                                       QR %in% c("Yes", "Ja") ~ TRUE),
                 Location = "BERLIN",
                 PersonalCode = paste0(PersonalCode1, PersonalCode2, PersonalCode3, PersonalCode4)) %>% 
   dplyr::select(-PersonalCode1:-PersonalCode4))

# KIEL DATA ####
#In Kiel data there are data collected from an old version of the survey (pre-deployment)
#These end up with different column titles because the names changed a bit over time (STUPID!)
#We can overcome this by unchecking 'include fields from all X deployed version'

(kiel_data <- readr::read_delim(here::here("I:/pwind/Nextcloud/Datascience/OceanNow_data/Ocean._Now_Kiel_-_latest_version_-_labels_-_2021-11-17-17-50-39.csv"), delim = ";",
                                  col_types = list(
                                    start = col_datetime(),
                                    end = col_datetime(),
                                    .default = col_character())) %>% 
   #We need to remove inputs that were before the start of the exhibition (Aug 1st)
   dplyr::filter(start >= lubridate::ymd("2021-08-01")) %>% 
   #Remove unneeded columns
   #Remove the columns with just text
   #Remove the columns that show 0/1 for each column. These cause problems between Kiel and Berlin
   dplyr::select(!contains("Thank") & !contains("Vielen Dank") &
                   !contains("Your questionnaire will receive") & !contains("Ihr Fragebogen wird") &
                   !contains(match = "What were the reasons you came to the exhibition?/") &
                   !contains(match = "Warum sind Sie zur Ausstellung gekommen?/")) %>% 
   #Convert 'Please choose' into language column
   dplyr::mutate(language = dplyr::case_when(stringr::str_detect(`Please choose/Bitte wählen Sie:`, "^I want") ~ "ENG",
                                             stringr::str_detect(`Please choose/Bitte wählen Sie:`, "^Ich") ~ "DEU")) %>% 
   #Pivot longer to deal with questions more easily and filter out Qs in wrong language
   tidyr::pivot_longer(cols = -c(`_id`, `_validation_status`, `_submission_time`, start, end, language)) %>% 
   #Add standard column codes
   dplyr::left_join(dictionary, by = "name") %>% 
   #Filter only relevant Qs for the language used
   dplyr::filter(language == Q_language & !is.na(code)) %>%
   dplyr::select(-name, -Q_language) %>% 
   tidyr::pivot_wider(names_from = code, values_from = value) %>% 
   #Convert Likert scale to values
   #Cols using Likert scale have suffix _LK
   dplyr::mutate(across(.cols = matches("_LK$"), .fns = function(answer){
     
     dplyr::case_when(answer %in% c("Strongly agree", "Stimme voll und ganz zu") ~ "7",
                      answer %in% c("Strongly disagree", "Stimme überhaupt nicht zu") ~ "1",
                      TRUE ~ answer) %>% 
       as.numeric()
     
   })) %>%
   #Personal code with NA should be X so not included in paste
   dplyr::mutate(across(.cols = contains("PersonalCode"), .fns = ~tidyr::replace_na(., replace = "X"))) %>% 
   dplyr::mutate(BirthYear = as.numeric(BirthYear),
                 Treatment = dplyr::case_when(Treatment %in% c("No", "Nein") ~ FALSE,
                                              Treatment %in% c("Yes", "Ja") ~ TRUE),
                 Gender = dplyr::case_when(Gender %in% c("Male", "Mann") ~ "Male",
                                           Gender %in% c("Female", "Frau") ~ "Female",
                                           TRUE ~ "Other"),
                 QR = dplyr::case_when(QR %in% c("No", "Nein") ~ FALSE,
                                       QR %in% c("Yes", "Ja") ~ TRUE),
                 Location = "KIEL",
                 PersonalCode = paste0(PersonalCode1, PersonalCode2, PersonalCode3, PersonalCode4)) %>% 
  dplyr::select(-PersonalCode1:-PersonalCode4))

# COMBINE DATA ####

clean_data <- dplyr::bind_rows(berlin_data,
                             kiel_data)

# write.csv(clean_data, file = here::here("data/clean_data.csv"))

## Age grouping (Peter)
clean_data <- mutate(Age = 2021 - BirthYear, clean_data)

clean_data <- clean_data %>% mutate(Age_group = case_when(
   Age <= 30            ~ "0-30",
   Age > 30 & Age <= 45 ~ "31-45",
   Age > 45             ~ "> 45"
),
Gen_Age_group = case_when(
   Age <= 24 ~ "Gen_Z",
   Age > 24 & Age <= 40 ~ "Millenials",
   Age > 40 ~ "Gen_X_Bommer")
)

table(clean_data$Age, clean_data$Age_group, useNA = "ifany")
table(clean_data$Age, clean_data$Gen_Age_group, useNA = "ifany")

prop.table(table(clean_data$Age_group))
prop.table(table(clean_data$Gen_Age_group)) # Worked

# Gender to factor
table(clean_data$Gender, useNA = "always")
class(clean_data$Gender)

clean_data <- clean_data %>% mutate(Gender = factor(Gender))

class(clean_data$Gender)
table(clean_data$Gender, useNA = "always")

# Save the Data
# write.csv(clean_data, file = here::here("I:/pwind/Nextcloud/Datascience/OceanNow_data/grouped_data.csv"))

