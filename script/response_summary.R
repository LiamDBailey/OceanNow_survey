# SUMMARY PLOTS (RESPONSES) ####
clean_data <- read.csv(here::here("data/clean_data.csv"))

## MICROPLASTIC POLLUTION LK ####

ggplot(data = clean_data) +
  geom_bar(aes(x = Microplastic_importance_LK, y = ..count../sum(..count..)),
           colour = "black", size = 1) +
  scale_x_continuous(limits = c(0.5, 7.5),
                     breaks = seq(1, 7, 1),
                     labels = c("Strongly\ndisagree", seq(2, 6, 1), "Strongly\nagree")) +
  scale_y_continuous(limits = c(0, 1)) +
  labs(y = "Proportion of responses",
       x = "",
       title = "Microplastic pollution is a major problem that needs to be addressed") +
  theme_classic() +
  theme(axis.text = element_text(colour = "black"))

ggplot2::ggsave(here::here("plot/microplastic_lk.jpeg"), width = 9.24, height = 5.6)

## ACTION IS VIABLE LK ####

ggplot(data = clean_data) +
  geom_bar(aes(x = Viable_action_LK, y = ..count../sum(..count..)),
           colour = "black", size = 1) +
  scale_x_continuous(limits = c(0.5, 7.5),
                     breaks = seq(1, 7, 1),
                     labels = c("Strongly\ndisagree", seq(2, 6, 1), "Strongly\nagree")) +
  scale_y_continuous(limits = c(0, 1)) +
  labs(y = "Proportion of responses",
       x = "",
       title = "We as humans are able to act against microplastic pollution") +
  theme_classic() +
  theme(axis.text = element_text(colour = "black"))

ggplot2::ggsave(here::here("plot/viableaction_lk.jpeg"), width = 9.24, height = 5.6)

## PERSONAL RESPONSIBILITY LK ####

ggplot(data = clean_data) +
  geom_bar(aes(x = Personal_responsibility_LK, y = ..count../sum(..count..)),
           colour = "black", size = 1) +
  scale_x_continuous(limits = c(0.5, 7.5),
                     breaks = seq(1, 7, 1),
                     labels = c("Strongly\ndisagree", seq(2, 6, 1), "Strongly\nagree")) +
  scale_y_continuous(limits = c(0, 1)) +
  labs(y = "Proportion of responses",
       x = "",
       title = "I have a personal responsibility to change my behaviour to avoid microplastic pollution") +
  theme_classic() +
  theme(axis.text = element_text(colour = "black"))

ggplot2::ggsave(here::here("plot/personalresponsibility_lk.jpeg"), width = 9.24, height = 5.6)

## POLITICIANS LK ####

ggplot(data = clean_data) +
  geom_bar(aes(x = Political_responsibility_LK, y = ..count../sum(..count..)),
           colour = "black", size = 1) +
  scale_x_continuous(limits = c(0.5, 7.5),
                     breaks = seq(1, 7, 1),
                     labels = c("Strongly\ndisagree", seq(2, 6, 1), "Strongly\nagree")) +
  scale_y_continuous(limits = c(0, 1)) +
  labs(y = "Proportion of responses",
       x = "",
       title = "Politicians have a responsibility to implement legislation to avoid microplastic pollution") +
  theme_classic() +
  theme(axis.text = element_text(colour = "black"))

ggplot2::ggsave(here::here("plot/politicianresponsibility_lk.jpeg"), width = 9.24, height = 5.6)

## CLIMATE CHANGE LK ####

ggplot(data = clean_data) +
  geom_bar(aes(x = Climate_importance_LK, y = ..count../sum(..count..)),
           colour = "black", size = 1) +
  scale_x_continuous(limits = c(0.5, 7.5),
                     breaks = seq(1, 7, 1),
                     labels = c("Strongly\ndisagree", seq(2, 6, 1), "Strongly\nagree")) +
  scale_y_continuous(limits = c(0, 1)) +
  labs(y = "Proportion of responses",
       x = "",
       title = "Climate change is the most important problem facing our society") +
  theme_classic() +
  theme(axis.text = element_text(colour = "black"))

ggplot2::ggsave(here::here("plot/climatechange_lk.jpeg"), width = 9.24, height = 5.6)

## Willing to pay LK ####

ggplot(data = clean_data) +
  geom_bar(aes(x = Willingness_price_LK, y = ..count../sum(..count..)),
           colour = "black", size = 1) +
  scale_x_continuous(limits = c(0.5, 7.5),
                     breaks = seq(1, 7, 1),
                     labels = c("Strongly\ndisagree", seq(2, 6, 1), "Strongly\nagree")) +
  scale_y_continuous(limits = c(0, 1)) +
  labs(y = "Proportion of responses",
       x = "",
       title = "I would be willing to pay 50% more for products that do not contain microplastics") +
  theme_classic() +
  theme(axis.text = element_text(colour = "black"))

ggplot2::ggsave(here::here("plot/willingpaymore_lk.jpeg"), width = 9.24, height = 5.6)

## Collective action LK ####

ggplot(data = clean_data) +
  geom_bar(aes(x = Willingness_community_LK, y = ..count../sum(..count..)),
           colour = "black", size = 1) +
  scale_x_continuous(limits = c(0.5, 7.5),
                     breaks = seq(1, 7, 1),
                     labels = c("Strongly\ndisagree", seq(2, 6, 1), "Strongly\nagree")) +
  scale_y_continuous(limits = c(0, 1)) +
  labs(y = "Proportion of responses",
       x = "",
       title = "I would be willing to participate in collective actions to address microplastic pollution (e.g. demonstrations)") +
  theme_classic() +
  theme(axis.text = element_text(colour = "black"))

ggplot2::ggsave(here::here("plot/collectiveaction_lk.jpeg"), width = 9.24, height = 5.6)

## WILL TAKE ACTION! ####

plot_data <- clean_data %>% 
  dplyr::mutate(across(.cols = c(Petition, Newsletter), ~tidyr::replace_na(., replace = 0))) %>% 
  dplyr::select(Petition, Newsletter) %>% 
  dplyr::mutate(noaction = (Petition + Newsletter) == 0) %>% 
  tidyr::pivot_longer(cols = c(Petition, Newsletter, noaction))

ggplot(plot_data) +
  geom_col(aes(x = name, y = value)) +
  scale_x_discrete(labels = c("Newsletter", "No action", "Sign petition"),
                   name = "") +
  scale_y_continuous(name = "") +
  labs(title = "Actions people are willing to take") +
  theme_classic()

ggplot2::ggsave(here::here("plot/willingactions.jpeg"), width = 9.24, height = 5.6)