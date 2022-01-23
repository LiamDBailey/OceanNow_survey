# SUMMARY PLOTS ####
clean_data <- read.csv(here::here("I:/pwind/Nextcloud/Datascience/OceanNow_data/clean_data.csv"))

## HOW MANY SURVEYS CONDUCTED AT EACH SITE? ####
plot_data <- clean_data %>% 
  group_by(Location) %>% 
  mutate(label = paste0(Location, " (", n(), ")"))

location_perc <- plot_data %>%
  group_by(Location) %>% 
  summarise(N = n()) %>% 
  arrange(N) %>%
  mutate(perc = (N/sum(N)) * 100,
         lagN = tidyr::replace_na(lag(N), 0)) %>% 
  mutate(y_coord = lagN + N/2)

ggplot(plot_data) +
  geom_bar(aes(x = 1, fill = label), colour = "black", size = 1) +
  geom_text(data = location_perc, aes(x = 1.15, y = y_coord, label = paste0(round(perc), "%")), size = 12) +
  coord_polar("y", start = 0) +
  labs(title = paste("Total number of surveys:", nrow(plot_data))) +
  scale_fill_manual(name = "", values = c("#ca3c25", "#33658a")) +
  theme_void() +
  theme(plot.title = element_text(size = 30))

ggplot2::ggsave(here::here("plot/full_sample.jpeg"), width = 9.24, height = 5.6)

## HOW MANY *INDIVIDUALS* SURVEYED AT EACH SITE ACCOUNTING FOR PERSONAL CODE ####

kiel_indv <- kiel_data %>% 
  #If individual is XXXX then make it unique
  tibble::rownames_to_column(var = "rownumber") %>% 
  mutate(PersonalCode = case_when(PersonalCode == "XXXX" ~ paste0(PersonalCode, rownumber),
                                  TRUE ~ PersonalCode)) %>% 
  group_by(PersonalCode) %>% 
  summarise(n = n()) %>% 
  group_by(n) %>% 
  mutate(label = case_when(n == 1 ~ paste0("KIEL \nONE SURVEY", " (", n(), ")"),
                           n == 2 ~ paste0("KIEL \nTWO SURVEY", " (", n(), ")")))

berlin_indv <- berlin_data %>% 
  select(-PersonalCode) %>% 
  tibble::rownames_to_column(var = "PersonalCode") %>% 
  group_by(PersonalCode) %>% 
  summarise(n = n()) %>% 
  mutate(label = paste0("BERLIN", " (", n(), ")"))

indv_data_combo <- kiel_indv %>% 
  dplyr::bind_rows(berlin_indv)

location_perc <- indv_data_combo %>%
  group_by(label) %>% 
  summarise(N = n()) %>% 
  arrange(N) %>%
  mutate(perc = (N/sum(N)) * 100,
         lagN = tidyr::replace_na(lag(N), 0)) %>% 
  mutate(y_coord = lagN + N/2)

ggplot(indv_data_combo) +
  geom_bar(aes(x = 1, fill = label), colour = "black", size = 1) +
  geom_text(data = location_perc, aes(x = 1.15, y = y_coord, label = paste0(round(perc), "%")), size = 6) +
  coord_polar("y", start = 0) +
  scale_fill_manual(name = "", values = c("#ca3c25", "#33658a", "#8ea4d2")) +
  labs(title = paste("Total number of individuals:", nrow(indv_data_combo))) +
  theme_void() +
  theme(plot.title = element_text(size = 30))

ggplot2::ggsave(here::here("plot/full_sample_individual.jpeg"), width = 9.24, height = 5.6)

## WHAT IS THE SEX RATIO? ####

kiel_indv <- kiel_data %>% 
  #If individual is XXXX then make it unique
  tibble::rownames_to_column(var = "rownumber") %>% 
  mutate(PersonalCode = case_when(PersonalCode == "XXXX" ~ paste0(PersonalCode, rownumber),
                                  TRUE ~ PersonalCode)) %>% 
  group_by(PersonalCode) %>% 
  summarise(Sex = first(Gender)) %>% 
  group_by(Sex) %>% 
  summarise(n = n(),
            Location = "KIEL")

berlin_indv <- berlin_data %>% 
  select(-PersonalCode) %>% 
  tibble::rownames_to_column(var = "PersonalCode") %>% 
  group_by(PersonalCode) %>% 
  summarise(Sex = first(Gender)) %>% 
  group_by(Sex) %>% 
  summarise(n = n(),
            Location = "BERLIN")

sex_data_combo <- kiel_indv %>% 
  dplyr::bind_rows(berlin_indv) %>% 
  mutate(label = paste0(Location, " (", Sex, ")"))

location_perc <- sex_data_combo %>%
  arrange(Location, n) %>%
  group_by(Location) %>% 
  mutate(perc = (n/sum(n)) * 100,
         lagN = tidyr::replace_na(lag(n), 0)) %>% 
  mutate(y_coord = lagN + n/2)

ggplot(sex_data_combo) +
  geom_col(aes(x = Location, y = n, fill = Sex), colour = "black", size = 1) +
  scale_fill_manual(name = "", values = c("#5FAD56", "#F78154", "grey75")) +
  theme_classic()+
  scale_y_continuous(expand = c(0, 0)) +
  labs(x = "", y = "Number responses")

## LOOK AT IT ANOTHER WAY, HOW MANY IN BERLIN AND KIEL ARE FROM EACH TREATMENT? ####
plot_data <- clean_data %>% 
  mutate(bar_fill = paste(Location, Treatment, sep = "_"))

location_perc <- plot_data %>%
  group_by(bar_fill) %>% 
  summarise(N = n(),
            Location = first(Location)) %>% 
  group_by(Location) %>% 
  arrange(desc(bar_fill)) %>%
  mutate(perc = (N/sum(N)) * 100,
         lagN = tidyr::replace_na(lag(N), 0)) %>% 
  mutate(y_coord = lagN + N/2)

ggplot(plot_data) + 
  geom_bar(aes(x = Location, fill = Treatment), colour = "black", size = 1) +
  geom_text(data = location_perc, aes(x = Location, y = y_coord, label = N), size = 10) +
  scale_fill_manual(values = c(rgb(202, 60, 37, max = 255, alpha = 255),
                               rgb(202, 60, 37, max = 255, alpha = 255*0.45)),
                    labels = c("Pre-treatment",
                               "Post-treatment"),
                    name = "") +
  scale_y_continuous(expand = c(0, 0)) +
  labs(x = "", y = "Number responses") +
  theme_classic() +
  theme(axis.text = element_text(colour = "black", size = 12))

ggplot2::ggsave(here::here("plot/sample_treatment.jpeg"), width = 9.24, height = 5.6)

## PEOPLE WHO SAW THE EXHIBITION USED QR OR SPENT TIME? ####
plot_data <- clean_data %>% 
  dplyr::filter(.data$Treatment)

location_perc <- plot_data %>%
  group_by(QR) %>% 
  summarise(N = n()) %>% 
  arrange(N) %>%
  mutate(perc = (N/sum(N)) * 100,
         lagN = tidyr::replace_na(lag(N), 0)) %>% 
  mutate(y_coord = lagN + N/2)
  
ggplot(plot_data) +
  geom_bar(aes(x = 1, fill = QR), colour = "black", size = 1) +
  geom_text(data = location_perc, aes(x = 1.1, y = y_coord, label = paste0(round(perc), "%\n", QR)), size = 9) +
  coord_polar("y", start = 0) +
  labs(title = "Number of attendees that \nscanned QR code") +
  scale_fill_manual(name = "", values = c("#ca3c25", "#33658a")) +
  theme_void() +
  theme(plot.title = element_text(size = 25),
        legend.position = "none")

ggplot2::ggsave(here::here("plot/QR_code.jpeg"), width = 9.24, height = 5.6)

time_data <- clean_data %>% 
  dplyr::filter(.data$Treatment)

mean(time_data$Time)

ggplot(time_data) +
  geom_histogram(aes(x = .data$Time), bins = 30, colour = "black", fill = "grey75") +
  labs(y = "", x = "Time (minutes)", title = "Time spent at exhibition") +
  theme_classic() +
  theme(axis.text.y = element_blank(),
        axis.title.y = element_blank(),
        axis.ticks.y = element_blank(),
        axis.text.x = element_text(colour = "black"))

ggplot2::ggsave(here::here("plot/time_hist.jpeg"), width = 9.24, height = 5.6)
