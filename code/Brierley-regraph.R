################################################################################

# Regraph Brierley PLoS Biol 2022

## article: https://doi.org/10.1371/journal.pbio.3001285
## data & code: https://zenodo.org/records/6281828

################################################################################

# Load libraries ----
library(tidyverse)
library(here)


# Fig 2C: figure changes between preprint & published article ----

## import data
main_body_changes <- read_csv(here("data", "Brierley_main_body_changes.csv"))


## explore the 2 variables that matter most: covid_preprint and change_outcomes
unique(main_body_changes$covid_preprint)
## based on code on Zenodo, T == COVID article, F == non-COVID article

unique(main_body_changes$change_outcomes)
## based on code on Zenodo, 0:4 corresponds to labels below
figure_change_category <- c("No real change", "Figures rearranged", 
                            "Significant content added", "Significant content removed", 
                            "Content added and removed")
figure_change_color <- c("#17BECF", "#9EDAE5", "grey80", "grey90", "grey99")
names(figure_change_color) <- figure_change_category


## wrangle data
figure_change_data <- main_body_changes %>%
  mutate(covid_preprint = if_else(covid_preprint == T, "COVID article", "non-COVID article"),
         change_outcomes = factor(change_outcomes, levels = c(0:4), labels = figure_change_category)) %>%
  count(covid_preprint, change_outcomes) %>%
  group_by(covid_preprint) %>%
  mutate(total = sum(n), 
         proportion = n / total *100) %>%
  ungroup() 
## note, 87 covid + 97 non-covid matches reporting in methods about 184 prepreint-paper pairs used in analysis


## reproduce Brierley Fig 2C
ggplot(data = figure_change_data,
       mapping = aes(x = change_outcomes, y = proportion, fill = covid_preprint)) +
  geom_col(position = "dodge", color = "grey50", size = 0.25) +
  theme_minimal() +
  theme(legend.position = "bottom") +
  labs(x = "Classification of change", y = "Percentage of articles") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
  guides(color = FALSE) 


## regraph Fig 2C into stacked bar graphs
ggplot(data = figure_change_data,
       mapping = aes(x = proportion, y = covid_preprint, 
                     fill = forcats::fct_rev(change_outcomes))) +
  geom_col(color = "grey50", linewidth = 0.25, width = 0.65) +
  scale_fill_manual(values = figure_change_color) +
  coord_fixed(ratio = 35) +
  theme_bw() +
  theme(legend.position="right",
        legend.title = element_blank()) +
  labs(x = "Percentage of articles", y = "")

ggsave(filename = "Brierley_Figure 2C_regraph.png", path = here("figure"),
       height = 3, width = 7.5, dpi = 300, units = "in")




# Fig 3C: changes in abstracts ----

## import and clean data 
abstract_scoring <- read_csv(here("data", "Brierley_abstract_scoring.csv")) %>% 
  # retain only non-excluded abstracts
  filter(exclude == "keep") %>%
  mutate(calendar_date = as.numeric(as.Date("30/4/2020", format="%d/%m/%Y") - as.Date(posted_date, format="%d/%m/%Y"))) %>% # Calculate number of days each preprint had been online by latest preprint posting date
  select(-X1.x) 

nrow(abstract_scoring)
## 184 entries, matching the sample size reported in Brierley Fig 3C


## explore the Highest_change variable
unique(abstract_scoring$Highest_change)
## based on code on Zenodo, 0:2 corresponds to labels below
abstract_change_level <- c("No Change", "Strengthening/softening, minor", "Major conclusion change")
abstract_change_color <- c("#795BB7", "#B5A7D5", "#F2F3F4")
names(abstract_change_color) <- abstract_change_level


## wrangle data
abstract_change_data <- abstract_scoring %>% 
  mutate(covid_preprint = if_else(covid_preprint == T, "COVID article", "non-COVID article"),
         Highest_change = factor(Highest_change, levels = 0:2, labels = abstract_change_level)) %>% 
  count(Highest_change, covid_preprint) %>% 
  group_by(covid_preprint) %>%
  mutate(total = sum(n), 
         proportion = n / total *100) %>%
  ungroup() 


## reproduce Brierley Fig 3C  
ggplot(data = abstract_change_data, 
       mapping = aes(x = Highest_change, y = n, fill = covid_preprint)) +
  geom_col(position = "dodge") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
  theme(legend.position = "none") +
  labs(x = "Scored change", y = "Number of scored abstracts") 


# regraph fig 3C into stacked bar graph, also using percentage instead of number
ggplot(data = abstract_change_data,
       mapping = aes(x = proportion, y = covid_preprint, 
                     fill = forcats::fct_rev(Highest_change))) +
  geom_col(color = "grey50", linewidth = 0.25, width = 0.65) +
  scale_fill_manual(values = abstract_change_color) +
  coord_fixed(ratio = 35) +
  theme_bw() +
  theme(legend.position="right",
        legend.title = element_blank()) +
  labs(x = "Percentage of articles", y = "")

ggsave(filename = "Brierley_Figure 3C_regraph.png", path = here("figure"),
       height = 3, width = 7.5, dpi = 300, units = "in")




