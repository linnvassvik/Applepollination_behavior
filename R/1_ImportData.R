library(readxl)
library(tidyverse)
library(tidylog)
library(ggplot2)
library(lme4)
library(emmeans)
library(vegan)

###### INSECT CAUGHT IN PAN AND VANE TRAPS

# INSECT CAUGHT IN PAN AND VANE TRAPS -------------------------------------


PanVane2023 <- read_excel("Data/PanVane2023.xlsx") %>% 
  select(-Sex, -Who)

unique_species <- PanVane2023 %>%
  select(Species) %>%
  distinct()

#Remove non pollinating genus
OnlyPollinators <- PanVane2023 %>% 
  filter(Genus != 'Sphecodes', Genus != 'Nomada')# %>% 
  #filter(Where == 'Inside')






# APPLE DATA FOR 2023 ONLY ------------------------------------------------


AppleDataB1 <- read_excel("Data/AQ_22+23.xlsx") %>% 
  filter(Year == '2023') %>% 
  filter(Treatment == 'N') %>% 
  mutate(seed_fail = Seeds_partially_developed + Seeds_not_developed) %>% 
  select(-Height, -Diameter, -Shape, -Seeds_partially_developed, -Seeds_not_developed, -ID, -ID_Year) %>% 
  rename(seed_success = Seeds_fully_developed) #%>% 
  # select(-Region, -Year)

AppleDataB <- AppleDataB1 %>% 
  group_by(Year, Region, Location, Apple_variety) %>%
  summarize(avg_weight = mean(Weight, na.rm = TRUE),
            avg_seed_success = mean(seed_success, na.rm = TRUE),
            avg_damage = mean(Damage, na.rm = TRUE),
            avg_seed_fail = mean(seed_fail, na.rm = TRUE))

#average per variety+location+treatment (remove HP and C)

FruitSetDataB <- read_excel("Data/ClusterFruit_22+23_RemovedUnsure.xlsx") %>%
  filter(Year_wrong == '2023') %>% 
  filter(Treatment == 'N') %>% 
  select(-Kommentar) %>% 
  rename(Year = Year_wrong) %>% 
  mutate(Tree = factor(substring(ID, 7,8))) %>% 
  relocate(Tree, .after = ID) %>% 
  mutate(Tree = as.character(Tree)) %>% 
  select(-ID) %>% 
  group_by(Year, Region, Apple_variety, Location, Treatment, Tree) %>%
  mutate(flowers = case_when(
    Apple_variety == "Summerred" ~ clusters * 5.3,
    Apple_variety == "Discovery" ~ clusters * 5.8,
    Apple_variety == "Aroma" ~ clusters * 5.1)) %>% 
  mutate(apple_fail = flowers - apples) %>% 
  select(-clusters, -flowers) %>% 
  rename(apple_success = apples) 

FruitSetDataB <- FruitSetDataB %>% 
  group_by(Year, Region, Location, Apple_variety) %>%
  summarize(avg_apple_success = mean(apple_success, na.rm = TRUE),
            avg_apple_fail = mean(apple_fail, na.rm = TRUE))

appledf <- AppleDataB %>% 
  left_join(FruitSetDataB, by = c("Year", "Region", "Apple_variety", "Location")) %>% 
  rename(weight = avg_weight, 
         seed_success = avg_seed_success,
         seed_fail = avg_seed_fail,
         apple_success = avg_apple_success,
         apple_fail = avg_apple_fail) %>% 
  select(-avg_damage) %>% 
  mutate(seedset = seed_success/(seed_fail + seed_success),
         fruitset = apple_success/(apple_success + apple_fail)) 

appledfB <- appledf %>% 
  select(-seed_success, -seed_fail, -apple_fail, -apple_success)

trap_types <- c("PanTrap", "VaneTrap")

appledfB_TrapType <- appledfB %>%
  expand_grid(Trap_type = trap_types) %>%
  arrange(Location, Apple_variety)




# MERGE DATAFRAMES FOR APPLES AND POLLINATORS -----------------------------


Poll <- OnlyPollinators %>%
  group_by(Year, Region, Location, Apple_variety, Trap_type, Trap_color, Species) %>% 
  summarise(count = n()) %>% 
  pivot_wider(names_from = Species, values_from = count, values_fill = 0) 

Poll2 <- OnlyPollinators %>%
  group_by(Location, Apple_variety, Species) %>% 
  summarise(count = n()) %>% 
  pivot_wider(names_from = Species, values_from = count, values_fill = 0) 


TestSpecies <- OnlyPollinators %>% 
  select(-c("Date_Start", "Date_Stop/Collected", "Date_ID", "ID", "Region", "Year")) %>% 
  group_by(Location, Apple_variety, Where, Trap_type, Trap_color) %>% 
  summarise(Nspecies = n_distinct(Species), .groups = 'drop')

Abundance <- OnlyPollinators %>% 
  select(-Genus, -Group) %>%
  group_by(Location, Apple_variety, Trap_type, Where, Species) %>% 
  summarise(count = n()) %>% 
  pivot_wider(names_from = Species, values_from = count, values_fill = 0) %>% 
  rowwise() %>%
  mutate(across(`Andrena cineraria`:`Hoplitis claviventris`, ~ ifelse(. >= 1, 1, 0))) %>% #check if this line should be there
  mutate(Species_sum = sum(c_across(`Andrena cineraria`:`Hoplitis claviventris`)))

Abundance_NoTrap <- OnlyPollinators %>% 
  select(-Genus, -Group) %>%
  #filter(Where == 'Inside') %>% 
  group_by(Location, Apple_variety, Species) %>% 
  summarise(count = n()) %>% 
  pivot_wider(names_from = Species, values_from = count, values_fill = 0) %>% 
  rowwise() %>%
  mutate(Species_sum = sum(c_across(`Andrena cineraria`:`Hoplitis claviventris`)))

Abundance_Genus <- OnlyPollinators %>% 
  select(-Species, -Group) %>%
  filter(Where == 'Inside') %>% 
  group_by(Location, Apple_variety, Genus) %>% 
  summarise(count = n()) %>% 
  pivot_wider(names_from = Genus, values_from = count, values_fill = 0) %>% 
  rowwise()

Abundance_Group <- OnlyPollinators %>% 
  select(-Species, -Genus) %>%
  filter(Where == 'Inside') %>% 
  group_by(Location, Apple_variety, Group) %>% 
  summarise(count = n()) %>% 
  pivot_wider(names_from = Group, values_from = count, values_fill = 0) %>% 
  rowwise()







# # Unsure what this is for. Probably NMDS which we cut out -----------------
# 
# 
# 
# PollSpecies <- left_join(appledfB, Poll2, 
#                        by = c("Location", "Apple_variety")) %>% 
#   ungroup() %>% 
#   select(-Year, -Region, -weight, -seedset, -fruitset)
# 
# Poll_Species2 <- PollSpecies %>% 
#   select(-Apple_variety, Location) %>% 
#   mutate(Location_numeric = as.numeric(factor(Location))) %>%
#   relocate(Location_numeric, .after = 2) 
# 
# data_1 <- Poll_Species2 %>% 
#   select(-Location, -Location_numeric) %>% 
#   select(-'Andrena', -'Andrena tibiailis', -'Andrena bicolor', -'Andrena lathyri', -'Andrena semilaevis', -'Andrena ruficrus', -'Bombus sylvestris', -"Bombus sylvarum", -'Osmia bicornis', -'Osmia parietina', -'Osmia bicolor', -'Hylaeus sp.', -'Hylaeus incongruus', -'Hoplitis claviventris') #remove with 2 or less registrations
# 
# data_1a <- Poll_Species2 %>% 
#   select(-Location, -Location_numeric) %>% 
#   select(-'Andrena', -'Andrena tibiailis', -'Andrena bicolor', -'Osmia bicornis', -'Osmia parietina', -'Hoplitis claviventris', -'Bombus ruderarius', -'Andrena subopaca', -'Bombus lapidarius') #remove with 2 or less registrations
# 
# # -'Bombus sylvarum', 
# 
# 
# 
# 

# Difference between vanetrap and pantrap ---------------------------------



PollTrap <- OnlyPollinators %>%
  group_by(Trap_type, Species) %>%
  summarise(count = n()) %>%
  pivot_wider(names_from = Species, values_from = count, values_fill = 0)


species_data <- Poll %>%
  ungroup() %>%
  select(-Trap_type)

species_data2 <- Poll %>%
  ungroup() %>%
  select(-Trap_color)
# dissimilarity <- vegdist(species_data, method = "bray")
# print(dissimilarity)

presence_absence <- species_data %>%
  mutate_all(~ ifelse(. > 0, 1, 0))

shared_species <- colnames(presence_absence)[colSums(presence_absence) == 2]
unique_to_pantrap <- colnames(presence_absence)[presence_absence[1, ] == 1 & presence_absence[2, ] == 0]
unique_to_vanetrap <- colnames(presence_absence)[presence_absence[1, ] == 0 & presence_absence[2, ] == 1]

max_length <- max(length(shared_species), length(unique_to_pantrap), length(unique_to_vanetrap))

species_df <- data.frame(
  Shared = c(shared_species, rep(NA, max_length - length(shared_species))),
  Unique_PanTrap = c(unique_to_pantrap, rep(NA, max_length - length(unique_to_pantrap))),
  Unique_VaneTrap = c(unique_to_vanetrap, rep(NA, max_length - length(unique_to_vanetrap))),
  stringsAsFactors = FALSE
)



# Behaviour ---------------------------------------------------------------

Behaviour <- read_excel("Data/Behaviour_Jan25_both.xlsx") %>%
  select(-Comment, -Comment2, -Length)

Behaviour %>%
  group_by(Subject) %>%  # Group by Subject (bee type)
  summarise(unique_IDs = n_distinct(ID))

Behaviour <- Behaviour %>%
  mutate(Location = case_when(
    substr(ID, 3, 3) == "B" ~ "Berle",
    substr(ID, 3, 3) == "H" ~ "Høyen",
    substr(ID, 3, 3) == "S" ~ "Sando",
    substr(ID, 3, 3) == "L" ~ "Lofthus",
    substr(ID, 3, 3) == "U" ~ "Urheim",
    substr(ID, 3, 3) == "D" ~ "Djønno",
    TRUE ~ NA_character_  # For any other cases, assign NA
  ))

Behaviour_stigma <- Behaviour %>%
  group_by(ID, Subject, Location) %>%
  summarize(no_stigma_contact = sum(Behavior == "No stigma contact"),
            foraging_count = sum(Behavior == "Foraging"),
            Percent_stigma_contact = (foraging_count / (no_stigma_contact + foraging_count)) * 100,
            .groups = "drop") %>%
  mutate(stigma_contact = foraging_count - no_stigma_contact)

Behaviour_stigma2 <- Behaviour %>%
  separate(Date, into = c("Date", "Time"), sep = " ") %>% 
  group_by(ID, Subject, Location, Date, Time) %>%
  mutate(Time = round_date(ymd_hms(paste(Date, Time)), unit = "hour"),
    Time = format(Time, "%H:%M:%S")) %>%  
  summarize(no_stigma_contact = sum(Behavior == "No stigma contact"),
            foraging_count = sum(Behavior == "Foraging"),
            Percent_stigma_contact = (foraging_count / (no_stigma_contact + foraging_count)) * 100,
            .groups = "drop") %>%
  mutate(stigma_contact = foraging_count - no_stigma_contact)

Behaviour_stigma2 <- Behaviour_stigma2 %>%
  mutate(Date = ymd(Date),
         DOY = yday(Date),
         Time = str_extract(Time, "^[0-9]{1,2}:[0-9]{2}"),
         Time_numeric = if_else(!is.na(Time), hour(hm(Time)) + minute(hm(Time)) / 60, NA_real_))



# Behaviour_stigma %>%
#   group_by(Subject) %>%
#   summarize(Average_Percent_Stigma_Contact = mean(Percent_stigma_contact, na.rm = TRUE))

Behaviour_foraging <- Behaviour %>%
  filter(Behavior == 'Foraging')

Behaviour_distance <- Behaviour %>%
  filter(Category == 'Travel distance')

BORIS_distance2 <- Behaviour %>%
  filter(Category == 'Travel distance') %>%
  group_by(Subject) %>%
  mutate(total_count = n()) %>%  # Calculate total count per Bee
  ungroup() %>%
  group_by(Behavior, Subject) %>%
  summarise(count = n(), .groups = 'drop') %>%
  left_join(Behaviour_distance %>% group_by(Subject) %>% summarise(total_count = n()), by = "Subject") %>%
  mutate(percentage = (count / total_count) * 100)

BORIS_distance2 <- BORIS_distance2 %>%
  mutate(Behavior = factor(
    Behavior,
    levels = c("New flower same tree", "New tree", "New row"),
    labels = c("New flower same tree", "New tree", "New row/further")
  ))





Behaviour3 <- Behaviour %>%
  filter(Category == 'Travel distance') %>%
  group_by(Subject, Location) %>%
  mutate(total_count = n()) %>%  # Calculate total count per Bee
  ungroup() %>%
  group_by(Behavior, Subject, ID, Location) %>%
  summarise(count = n(), .groups = 'drop')


Behaviour4 <- Behaviour %>%
  filter(Category == 'Travel distance') %>%
  group_by(Subject, Location) %>%
  mutate(total_count = n()) %>%  # Calculate total count per Bee
  ungroup() %>%
  group_by(Behavior, Subject, Location) %>%
  summarise(count = n(), .groups = 'drop') %>%
  left_join(Behaviour_distance %>% group_by(Subject) %>% summarise(total_count = n()), by = "Subject") %>%
  mutate(percentage = (count / total_count) * 100)


# Orchard design and tree placement data ------------------------------

appledfB_Tot <- appledf %>%
  arrange(Location, Apple_variety) %>%
  ungroup() %>%
  select(-Year, -fruitset, -weight, -apple_success , -apple_fail) %>%
  mutate(seed_total = seed_success + seed_fail) %>%
  mutate(seed_potential = seed_success / seed_total) %>%
  mutate(Orchard_structure = case_when((Location %in% c("Djønno", "Høyen") & Apple_variety %in% c("Aroma", "Discovery", "Summerred")) | (Location == "Urheim" & Apple_variety == "Summerred") ~ "block", TRUE ~ "mixed"))

AppleDF <- AppleDataB1 %>%
  select(-Weight, -Damage, -Year) %>%
  mutate(seed_total = seed_success + seed_fail) %>%
  mutate(seed_potential = seed_success / seed_total) %>%
  mutate(Orchard_structure = case_when((Location %in% c("Djønno", "Høyen") & Apple_variety %in% c("Aroma", "Discovery", "Summerred")) | (Location == "Urheim" & Apple_variety == "Summerred") ~ "block", TRUE ~ "mixed"))

CenterEdge <- read_excel("Data/CenterEdge.xlsx") %>%
  mutate(Tree = as.character(Tree))

AppleDF$seed_success <- round(AppleDF$seed_success)
AppleDF$seed_fail <- round(AppleDF$seed_fail)
AppleDF$seed_total <- round(AppleDF$seed_total)


AppleDF1 <- AppleDF %>%
  left_join(CenterEdge, by = c("Location", "Apple_variety", "Tree")) %>%
  rename(Placement = "Center/Edge")


# Combine Seed set data and species richness and abundance per loc --------

AppleDF2 <- AppleDF1 %>%
  group_by(Apple_variety, Location) %>%
  summarise(across(where(is.numeric), mean, na.rm = TRUE), .groups = "drop") %>%
  ungroup()

#For analysis
StasSpecies <- OnlyPollinators %>% 
  select(-Year, -Date_Start, -Date_ID, -ID) %>% 
  rename(date = `Date_Stop/Collected`)

StasSpecies %>%
  group_by(Location) %>%
  summarise(
    Species_Richness = n_distinct(Species),
    Abundance = n()) %>%
  ungroup()


species_RA <- StasSpecies %>%
  group_by(Location, Where, Trap_color, date, Apple_variety) %>%
  summarise(
    Species_Richness = n_distinct(Species), 
    Abundance = n()) %>%
  ungroup()  

SpeciesDF2 <- species_RA %>%
  group_by(Apple_variety, Location) %>%
  summarise(across(where(is.numeric), mean, na.rm = TRUE), .groups = "drop") %>%
  ungroup()

AppleSpecies <- SpeciesDF2 %>%
  left_join(AppleDF2, by = c("Apple_variety", "Location")) %>%
  mutate(Location = as.factor(Location))



#Check what is needed here

# appledfB_Tot <- appledf %>%
#   arrange(Location, Apple_variety) %>%
#   select(-Year, -fruitset, -weight, -Region, -apple_success , -apple_fail) %>%
#   mutate(seed_total = seed_success + seed_fail) %>%
#   mutate(seed_potential = seed_success / seed_total)
# 
# 
# PollinatorsTot <- OnlyPollinators %>%
#   #filter(Where == 'Inside') %>%
#   select(-Date_Start, -`Date_Stop/Collected`, -Date_ID, -Group, -Genus, -Trap_color, -Year, -ID, -Trap_type, -Region, -Where)

#Abundance
PollTot <- Abundance_NoTrap %>%
  left_join(appledfB_Tot, by = c("Location", "Apple_variety"))

PollTotNoSpecies <- PollTot %>%
  select(-c(3:39)) %>%
  #select(-Region, -Year) %>%
  mutate(Species_sum = as.double(Species_sum))


#Richness
TestSpecies3 <- OnlyPollinators %>% 
  #filter(Where == 'Inside') %>% 
  select(-c("Date_Start", "Date_Stop/Collected", "Date_ID", "ID", "Region", "Year")) %>% 
  group_by(Location, Apple_variety, Where) %>% 
  summarise(Nspecies = n_distinct(Species),
            NApis = sum(Genus == "Apis"),
            .groups = 'drop') %>% 
  ungroup()


PollTot2 <- TestSpecies3 %>% 
  left_join(appledfB_Tot, by = c("Location", "Apple_variety"))


PollTot_RA <- PollTot2 %>% 
  left_join(PollTotNoSpecies, by = c("Location", "Apple_variety", "Region", "Orchard_structure", "seedset", "seed_success", "seed_fail", "seed_total", "seed_potential")) %>% 
  mutate(Wildbee_sum = Species_sum - NApis)

# #Genus
# GenusPoll <- Abundance_Genus %>% 
#   left_join(appledfB_Tot, by = c("Location", "Apple_variety")) %>% 
#   select(-Year, -Region)
# 
# #Group
# GroupPoll <- Abundance_Group %>% 
#   left_join(appledfB_Tot, by = c("Location", "Apple_variety")) %>% 
#   select(-Year, -Region)

