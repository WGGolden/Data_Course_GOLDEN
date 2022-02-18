library(tidyverse)
library(dplyr)
library(gganimate)

dat <- read_csv("../../Data/BioLog_Plate_Data.csv")

longdat <- dat %>% pivot_longer(starts_with("Hr_"),
                     names_to = "Time",
                     values_to = "Absorbance",names_prefix = "Hr_") %>% 
  mutate(Time = as.numeric(Time)) %>% 
  mutate(SampleType = case_when(`Sample ID` == "Clear_Creek" ~ "Water",
                                `Sample ID` == "Wast_water" ~ "Water",
                                TRUE ~ "Soil"))

longdat %>% 
  filter(Dilution == 0.1) %>% 
  ggplot(longdat, mapping = aes(x = Time, y = Absorbance, color = SampleType))+
  geom_smooth(se = FALSE)+
  facet_wrap(~Substrate)+
  labs(title = "just dilution 0.1")

longdat %>% 
  group_by(Substrate, Time, `Sample ID`,Dilution) %>% 
  summarize(meanabs = mean(Absorbance)) %>% 
  filter(Substrate == "Itaconic Acid") %>% 
  ggplot(aes(x= Time, y = meanabs, color = `Sample ID`))+
  geom_line()+
  transition_reveal(Time)+
  facet_wrap(~Dilution)
