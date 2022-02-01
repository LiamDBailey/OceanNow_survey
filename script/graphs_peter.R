### Preparations

library(readr)
library(tidyverse)
library(ggplot2)
library(psych)

clean_data <- read.csv(here::here("data/clean_data.csv"))

### Responses Male vs Female
names(clean_data)

table(clean_data$Treatment, useNA = "ifany")
table(as.numeric(clean_data$Treatment), useNA = "ifany")
dim(clean_data)

pre_treatment_data <- clean_data %>% filter(Treatment == 0)
dim(pre_treatment_data)
names(pre_treatment_data)

pre_treatment_data <- pre_treatment_data %>% select(Microplastic_importance_LK, 
                                                    Viable_action_LK, Personal_responsibility_LK,
                                                    Political_responsibility_LK, Gender)

names(pre_treatment_data) # Selecting the needed data worked

pre_treatment_data %>% describe()
percentages <- pre_treatment_data %>% sapply(table, pre_treatment_data$Gender) %>% sapply(prop.table, 2)

# Prepare percentage value
scale_strongly_agree <- function(x) {
  new_value <- if_else(x == 7, 1, 0)
}

table(pre_treatment_data$Microplastic_importance_LK)
table(scale_strongly_agree(pre_treatment_data$Microplastic_importance_LK))

pre_treatment_data <- pre_treatment_data %>% mutate(across(!Gender, scale_strongly_agree))

percentages
pre_treatment_data %>% sapply(table, pre_treatment_data$Gender) %>% sapply(prop.table, 2) # Worked

# Remove Other Gender
table(pre_treatment_data$Gender)
class(pre_treatment_data$Gender)

pre_treatment_data <- pre_treatment_data %>% filter(Gender != "Other")
pre_treatment_data <- pre_treatment_data %>% mutate(Gender = if_else(Gender == "Female", 0, 1))

table(pre_treatment_data$Gender) # Worked

## Create the graph
# Preparations
shares <- aggregate(pre_treatment_data,by=list(pre_treatment_data$Gender),mean)
shares

shares <- shares[,2:length(shares)]

higher_perc <- function(x){
  percentage <- x * 100
  r_perc <- round(percentage, 2)
  return(r_perc)
}

shares[1,2]
higher_perc(shares[1,2]) # transformation function works

shares <- shares %>% mutate(across(!Gender, higher_perc))
shares

shares_long <- shares %>% pivot_longer(!Gender, names_to = "Variable", values_to = "Highly_agree")
shares_long

ggplot(shares_long,aes(x=Variable,y=Highly_agree,fill=factor(Gender), label = Highly_agree))+
  geom_bar(stat="identity",position="dodge", width = 0.8, alpha = 0.8)+
  scale_fill_manual(name="Gender",
                      breaks=c(1, 0),
                      labels=c("Male", "Female"),
                      values = c( "blue2", "grey48"))+
  geom_text(size = 8, position = position_dodge(width = .8), hjust = "right", colour = "white") +
  scale_x_discrete("Variable", labels = c("Microplastic \n pollution \n is a major 
                                          problem",
                                          "I have a \n personal \n responsibility \n to act against \n microplastic pollution", 
                                          "Politicians have a \n responsibility \n to act against \n microplastic pollution", 
                                          "We are able \n to act against \n microplastic \n pollution"))+
  ylab("Strongly Agree (in %)") +
  ggtitle("Men agree less with the Key Messages of\nOcean. Now! than Women before seeing the Exhibition!") +
  coord_flip() + 
  theme_classic() +
  theme(axis.title=element_text(size=12,face="bold"), axis.text = element_text(size = 12),
        legend.title = element_text(size=12,face="bold"), text = element_text(size = 12))

ggplot2::ggsave(here::here("plot/gender_dif.jpeg"), width = 9.24, height = 5.6)

## Newsletter VS Petition
# Preparation
names(clean_data)

dim(clean_data)
act_data <- clean_data %>% select(Treatment, Petition, Newsletter)
dim(act_data)

# Inspect the data
act_data %>% sapply(table, useNA = "ifany")
act_data %>% sapply(table, act_data$Treatment, useNA = "ifany")
table(act_data$Petition, act_data$Newsletter, useNA = "ifany") 
# NAs always exist for both petition and Newsletter in parallel
table(act_data$Treatment, act_data$Newsletter, useNA = "ifany")
table(act_data$Treatment, act_data$Petition, useNA = "ifany") 
# They are also quite equally across treatment and no treatment

# Remove NA
nrow(act_data)
act_data <- act_data %>% na.omit()
nrow(act_data)
table(act_data$Petition, act_data$Newsletter, useNA = "ifany") # All NAs were successfully removed

# Prepare the dataset for the graph
act_graph_long <- act_data %>% pivot_longer(!Treatment, names_to = "Variable", values_to = "Value")
head(act_graph_long)
act_graph <- act_graph_long %>% group_by(Treatment, Variable, Value) %>% tally()
act_graph$Value <- as.logical(act_graph$Value)
act_graph$Perc_Var <- act_graph %>% group_by(Treatment, Variable) %>% 
  summarise(Sub_count = sum(n), Percentage_Var = n / Sub_count) %>% pull(Percentage_Var)
act_graph # Looks good

# Test_Graphs
ggplot(act_graph, aes(x = Variable, y = n, fill = Value)) +
  geom_bar(stat = "identity", position = "fill")

ggplot(act_graph, aes(x = Treatment, y = n, fill = factor(Value), label = paste0(round(Perc_Var, 2)*100,"%"))) +
  geom_bar(stat = "identity", position = "fill", alpha = 0.8) +
  scale_fill_manual(name="Participation",
                    breaks=c(FALSE, TRUE),
                    labels=c("No", "Yes"),
                    values = c("grey48", "blue4"))+
  facet_wrap(~Variable, scales = "free_x", strip.position = "bottom") + 
  geom_text(position = position_fill(vjust =0.5), size = 5, color = "white") +
  ggtitle("People are much more likely to participate\nin Petitions than subscribing to Newsletter!") +
  xlab("The Exhibition") +
  scale_x_discrete(labels = c("Didn't see", "Have seen")) +
  scale_y_continuous(labels = scales::percent) +
  theme_classic() +  
  theme(strip.background = element_blank(), strip.text = element_text(size=14,face="bold"),
        plot.title =  element_text(size=19,face="bold"), axis.title.y = element_blank(), 
        axis.title.x = element_text(size=14,face="bold"), 
        axis.text = element_text(size = 11), axis.line = element_blank(), 
        legend.title = element_text(size=12,face="bold"), legend.text = element_text(size=12))

ggplot2::ggsave(here::here("plot/action_plot.jpeg"), width = 9.24, height = 5.6)
