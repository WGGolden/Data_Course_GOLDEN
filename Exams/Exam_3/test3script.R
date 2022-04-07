# Loading Packages ####
library(tidyverse)
library(ggplot2)
library(janitor)
library(modelr)
library(broom)

# Faculty Salaries ####

df <- read.csv("FacultySalaries_1995.csv")

df <- clean_names(df)

df <- df[!df$tier == "VIIB", ]

# Clean Data ####

#assist

assist <- df %>% 
  select(starts_with("avg_assist"),-ends_with("comp"),state,tier)

assist$rank <- "assist"

assist <- assist %>% rename(.,salary=avg_assist_prof_salary)

# assoc

assoc <- df %>% 
  select(starts_with("avg_assoc"),-ends_with("comp"),state,tier)

assoc$rank <- "assoc"

assoc <- assoc %>% rename(.,salary=avg_assoc_prof_salary)


# Full

full <- df %>% 
  select(starts_with("avg_full"),-ends_with("comp"),state,tier)

full$rank <- "full"

full <- full %>% rename(.,salary=avg_full_prof_salary)


# Combine data

clean <- full_join(assoc,assist) %>% 
  full_join(full)


# Graph Data ####

clean %>% 
  ggplot(aes(x=rank,y=salary, fill=rank))+ 
  geom_boxplot()+
  facet_wrap(~tier)+
  theme_bw()+
  theme(strip.background = element_blank())+
  theme(axis.text.x = element_text(angle = 50, hjust = 1))


# ANOVA model and summary ####

mod <- glm(data=clean,
           formula=salary~state+tier+rank)

test <- aov(mod)
summary(test)


# Juniper Oil Data ####

# Load dataset

oil <- read.csv("Juniper_Oils.csv")

# tidy up to show Years since burn, concentration, and chemical ID ####

oil <- clean_names(oil)

cleanoil <- select(oil,"alpha_pinene","para_cymene","alpha_terpineol",
              "cedr_9_ene","alpha_cedrene","beta_cedrene",
              "cis_thujopsene","alpha_himachalene",
              "beta_chamigrene","cuparene","compound_1",
              "alpha_chamigrene","widdrol","cedrol","beta_acorenol"
              ,"alpha_acorenol","gamma_eudesmol","beta_eudesmol"
              ,"alpha_eudesmol","cedr_8_en_13_ol","cedr_8_en_15_ol",
              "compound_2","thujopsenal", "years_since_burn") %>% 
  rename("alpha-acorenol"="alpha_acorenol",
             "alpha-pinene" ="alpha_pinene",
             "para-cymene" ="para_cymene",
             "alpha-terpineol" ="alpha_terpineol",
             "cedr-9-ene" ="cedr_9_ene",
             "alpha-cedrene"="alpha_cedrene",
             "beta-cedrene"="beta_cedrene",
             "cis-thujopsene" ="cis_thujopsene",
             "alpha-himachalene"="alpha_himachalene",
             "beta-chamigrene"="beta_chamigrene",
             "compound 1"="compound_1",
             "alpha-chamigrene" ="alpha_chamigrene",
             "beta-acorenol" ="beta_acorenol",
             "alpha-acorenol"="alpha_acorenol",
             "gamma-eudesmol" ="gamma_eudesmol",
             "beta-eudesmol" ="beta_eudesmol",
             "alpha-eudesmol"="alpha_eudesmol",
             "cedr-8-en-13-ol" ="cedr_8_en_13_ol",
             "cedr-8-en-15-ol" ="cedr_8_en_15_ol",
             "compound 2" ="compound_2",
             "YearsSinceBurn" ="years_since_burn")


cleanoil <- cleanoil %>% 
  pivot_longer(c("alpha-pinene","para-cymene","alpha-terpineol",
                    "cedr-9-ene","alpha-cedrene","beta-cedrene",
                    "cis-thujopsene","alpha-himachalene",
                    "beta-chamigrene","cuparene","compound 1",
                    "alpha-chamigrene","widdrol","cedrol","beta-acorenol"
                    ,"alpha-acorenol","gamma-eudesmol","beta-eudesmol"
                    ,"alpha-eudesmol","cedr-8-en-13-ol","cedr-8-en-15-ol",
                    "compound 2","thujopsenal"),
               names_to = "ChemicalID",
               values_to = "Concentration")
               
# Make graph ####

cleanoil %>% 
  ggplot(aes(x = YearsSinceBurn,y = Concentration))+
  geom_smooth()+
  facet_wrap(~ChemicalID, scales = "free")

# Make a model ####

cleanoil %>% glimpse

mod2 <- glm(data=cleanoil,
            formula = Concentration~ChemicalID*YearsSinceBurn)
summary(mod2)    

m <- tidy(mod2)

m %>% filter(m$p.value<0.05)
