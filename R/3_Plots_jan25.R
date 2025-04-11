source("R/1_ImportData.R")


#library(broom) #?
#library(purrr) #?
library(ggplot2) #plots
library(ggforce) #sina in ggplot
library(ggpubr) #ggarrange
library(ggvenn) #Venn diagram



# Venn diagram per trap ---------------------------------------------
VennDiagram <- read_excel("Data/PanVane2023.xlsx") %>%
  select(-c(Date_Start, `Date_Stop/Collected`, Date_ID, ID, Region, Sex, Who, Where))


VennDiagram2 <- VennDiagram %>%
  filter(!(Genus == "Sphecodes")) %>% 
  filter(!(Genus == "Nomada")) %>%
  select(-Genus, -Group) %>%
  group_by(Year, Location, Apple_variety, Trap_type, Trap_color, Species) %>% 
  summarise(count = n()) 


Species_Trap <- VennDiagram2 %>% 
  group_by((Trap_color)) %>% 
  summarise(Species = list(unique(Species))) %>%
  deframe()


VennTraps2 <- ggvenn(Species_Trap, 
                     fill_color = c("#6699CC", "#DDC1B6", "#FFCC00", "#868686FF"),
                     show_percentage = FALSE,
                     set_name_size = 0)


# Species accumulation curve for different traps ------------------------------
OnlyPollinators <- OnlyPollinators %>%
  mutate(
    `Date_Stop/Collected` = case_when(
      `Date_Stop/Collected` == "29.03.23" ~ "29.05.23",
      TRUE ~ `Date_Stop/Collected`),
    Date_Start = case_when(
      Date_Start == "26.03.23" ~ "26.05.23",
      TRUE ~ Date_Start)) %>%
  filter(`Date_Stop/Collected` != "No date on bag") %>%
  distinct()

# Prepare the dataset: Define a sample
OnlyPollinators3 <- OnlyPollinators %>%
  mutate(Date = `Date_Stop/Collected`,
         DOY = yday(dmy(Date))) %>% 
  ungroup() %>% 
  select(-Year, -Date_Start, -`Date_Stop/Collected`, -Date_ID, -ID, -Region, -Apple_variety, -Where, -Trap_type, -Genus, -Group, -Date)

OnlyPollinators5 <- OnlyPollinators3 %>%
  group_by(Species, DOY, Trap_color) %>%
  summarize(total_species = n(), .groups = "drop") %>% 
  pivot_wider(names_from = Species, values_from = total_species, values_fill = list(total_species = 0))

OnlyPollinators5 <- OnlyPollinators5 %>%
  mutate(Sample = case_when(
    DOY == 136 ~ 1,
    DOY == 139 ~ 2,
    DOY == 143 ~ 3,
    DOY == 146 ~ 4,
    DOY == 149 ~ 5,
    DOY == 152 ~ 6,
    DOY == 155 ~ 7,
    TRUE ~ NA_real_  # for any unexpected DOY values
  ))


# Create a list of data frames, one per trap color
trap_list <- split(OnlyPollinators5, OnlyPollinators5$Trap_color)

# Make specaccum for each
accum_list <- map(trap_list, ~ {
  species <- .x %>% select(-DOY, -Trap_color)
  specaccum(as.data.frame(species), method = "random")
})

# Convert to data frame for ggplot
accum_df <- map2_df(accum_list, names(accum_list), ~ data.frame(
  Sites = .x$sites,
  Richness = .x$richness,
  SD = .x$sd,
  Trap_color = .y
))


accum_df$Trap_color <- factor(accum_df$Trap_color, levels = c("BlueYellow", "Blue", "Yellow", "White"))


Species_accumulation <- accum_df %>% 
  ggplot(aes(x = Sites, y = Richness, color = Trap_color, fill = Trap_color)) +
  geom_smooth() +
  scale_x_continuous(breaks = 1:7,
                     labels = c(136, 139, 143, 146, 149, 152, 155)) +
  scale_color_manual(values = c("BlueYellow" = "#868686FF", "Blue" = "#6699CC", "Yellow" = "#FFCC00", "White" = "#DDC1B6"),
                     labels = c("BlueYellow" = "Vane trap", "Blue" = "Blue pan trap", "Yellow" = "Yellow pan trap", "White" = "White pan trap")) +
  scale_fill_manual(values = c("BlueYellow" = "#868686FF", "Blue" = "#6699CC", "Yellow" = "#FFCC00", "White" = "#DDC1B6"),
                    labels = c("BlueYellow" = "Vane trap", "Blue" = "Blue pan trap", "Yellow" = "Yellow pan trap", "White" = "White pan trap")) +
  theme_minimal() +
  labs(x = "DOY (day of the year)",
       y = "Species richness",
       color = "", fill = "") +
  geom_vline(xintercept = 5, color = "black", linetype = "solid", size = 1) +
  geom_vline(xintercept = 6, color = "black", linetype = "dashed", size = 1) +
  geom_vline(xintercept = 7, color = "black", linetype = "dotted", size = 1) +
  annotate("text", x = 5, y = 1, label = "Summerred", angle = 0, hjust = 1.1, vjust = 1, size = 6, color = "black") +
  annotate("text", x = 6, y = 1, label = "Discovery", angle = 0, hjust = 1.1, vjust = 1, size = 6, color = "black") +
  annotate("text", x = 7, y = 1, label = "Aroma", angle = 0, hjust = 1.1, vjust = 1, size = 6, color = "black") +
  theme_minimal(base_size = 20) + 
  theme(legend.position = "bottom",
        axis.text = element_text(size = 14),
        axis.title = element_text(size = 17))


SpeciesData <- ggarrange(VennTraps2, Species_accumulation, widths = c(3.5, 5))
ggsave(SpeciesData, filename = "Figures/SpeciesData.jpeg", height = 10, width = 16)

# Behaviour ---------------------------------------------------------------

## Stigma contact
Stigma <- Behaviour_stigma %>% 
  ggplot(aes(x = Subject, y = Percent_stigma_contact, fill = Subject, color = Subject)) + 
  #geom_jitter(size = 2, width = 0.1, alpha = 0.6, show.legend = FALSE) +  
  geom_sina() +
  geom_violin(alpha = 0.5) +  
  scale_fill_manual(values = c("#FFAC81", "#B74F6F", "#FEC3A6")) +
  scale_color_manual(values = c("#FFAC81","#B74F6F", "#FEC3A6")) +
  stat_summary(fun = mean, geom = "point", color = "black",size = 3) +
  labs(y = "Stigma contact (%)", x = "", fill = "Bee", title = "b") +
  scale_x_discrete(labels = c("Honeybee" = "Honeybees",
                              "Bumblebee" = "Bumblebees",
                              "Wild bee" = "Solitary bees"),
                   limits = c("Honeybee", "Bumblebee", "Wild bee")) + 
  theme_minimal() +
  theme(legend.position = "none", 
        axis.text = element_text(size = 14),
        axis.title = element_text(size = 17),
        plot.title = element_text(face = "bold", size = 22))


Behaviour_foraging$Forage_time_s <- as.numeric(Behaviour_foraging$Forage_time_s)

Foraging <- Behaviour_foraging %>% 
  ggplot(aes(x = Subject, y = Forage_time_s, fill = Subject, color = Subject)) + 
  #geom_jitter(size = 2, width = 0.1, alpha = 0.6, show.legend = FALSE) +
  geom_sina() +
  geom_violin(alpha = 0.5) +
  scale_fill_manual(values = c("#FFAC81", "#B74F6F", "#FEC3A6")) +
  scale_color_manual(values = c("#FFAC81", "#B74F6F", "#FEC3A6")) +
  stat_summary(fun = mean, geom = "point", color = "black",size = 3) + 
  labs(y = "Foraging time (s)", x = "", fill = "Bee", title = "a") +
  scale_x_discrete(labels = c("Honeybee" = "Honeybees",
                              "Bumblebee" = "Bumblebees",
                              "Wild bee" = "Solitary bees"),
                   limits = c("Honeybee", "Bumblebee", "Wild bee")) + 
  theme_minimal() +
  theme(legend.position = "none", 
        axis.text = element_text(size = 14),
        axis.title = element_text(size = 17),
        plot.title = element_text(face = "bold", size = 22))

Behaviour1 <- ggarrange(Foraging, Stigma)

#Flight distance
Distance <- BORIS_distance2 %>% 
  mutate(Subject = factor(Subject, levels = c("Honeybee", "Bumblebee", "Wild bee"))) %>%
  ggplot(aes(x = Behavior, y = percentage, fill = Subject)) +
  geom_bar(stat = "identity", position = "dodge") +
  labs(y = "Foraging time (s)", x = "", fill = "Bee", title = "c") +
  scale_fill_manual(
    values = c("#B74F6F", "#FFAC81", "#FEC3A6"),
    labels = c("Honeybee" = "Honeybees", "Bumblebee" = "Bumblebees", "Wild bee" = "Solitary bees")) + 
  labs(y = "Percentage of total observations", x = "", fill = "") +
  theme_minimal() +
  theme(axis.text = element_text(size = 14),
        axis.title = element_text(size = 17),
        legend.text = element_text(size = 15),
        plot.title = element_text(face = "bold", size = 22))




Behaviour2 <- ggarrange(ggarrange(Foraging, Stigma, ncol = 2),
                        Distance,
                        nrow = 2,
                        heights = c(1, 1))

ggsave(Behaviour2, filename = "Figures/Behaviour2.jpeg", height = 10, width = 16)



# Stigma contact and DOY --------------------------------------------------

HBDOY <- Behaviour_stigma2 %>% 
  filter(Subject == 'Honeybee') %>% 
  ggplot(aes(x = DOY, y = Percent_stigma_contact, fill = Subject, color = Subject)) + 
  geom_smooth(method = "lm") +
  geom_jitter(size = 2, width = 0.1, show.legend = FALSE) +  
  scale_fill_manual(values = c("#B74F6F")) +
  scale_color_manual(values = c("#B74F6F")) +
  labs(title = "Honeybee", y = "Stigma contact (%)", x = "", fill = "") +
  theme_minimal() +
  scale_y_continuous(limits = c(50, 100), oob = scales::squish) +
  scale_x_continuous(limits = c(135, 154)) +
  theme(legend.position = "none", 
      axis.text = element_text(size = 14),
      axis.title = element_text(size = 17),
      plot.title = element_text(hjust = 0.5, size = 22))


SBDOY <- Behaviour_stigma2 %>% 
  filter(Subject == 'Wild bee') %>% 
  ggplot(aes(x = DOY, y = Percent_stigma_contact, fill = Subject, color = Subject)) + 
  geom_smooth(method = "lm") +
  geom_jitter(size = 2, width = 0.1, show.legend = FALSE) +  
  scale_fill_manual(values = c("#FEC3A6")) +
  scale_color_manual(values = c("#FEC3A6")) +
  labs(title = "Solitary bee", y = "", x = "", fill = "") +
  theme_minimal() +
  scale_y_continuous(limits = c(50, 100), oob = scales::squish) +
  scale_x_continuous(limits = c(135, 154)) +
  theme(legend.position = "none", 
        axis.text = element_text(size = 14),
        axis.title = element_text(size = 17),
        plot.title = element_text(hjust = 0.5, size = 22))

BBDOY <- Behaviour_stigma2 %>% 
  filter(Subject == 'Bumblebee') %>% 
  ggplot(aes(x = DOY, y = Percent_stigma_contact, fill = Subject, color = Subject)) + 
  geom_smooth(method = "lm") +
  geom_jitter(size = 2, width = 0.1, show.legend = FALSE) +  
  scale_fill_manual(values = c("#FFAC81")) +
  scale_color_manual(values = c("#FFAC81")) +
  labs(title = "Bumblebee", y = "", x = "DOY (day of the year)", fill = "") +
  theme_minimal() +
  scale_y_continuous(limits = c(50, 100), oob = scales::squish) +
  scale_x_continuous(limits = c(135, 154)) +
  theme(legend.position = "none", 
        axis.text = element_text(size = 14),
        axis.title = element_text(size = 17),
        plot.title = element_text(hjust = 0.5, size = 22))

StigmaDOY <- ggarrange(HBDOY, BBDOY, SBDOY, nrow = 1)
ggsave(StigmaDOY, filename = "Figures/StigmaDOY.png", height = 10, width = 14)




# Species and Genus data  -------------------------------------------------

# some stats
StasSpecies <- OnlyPollinators %>% 
  select(-Year, -Date_Start, -`Date_Stop/Collected`, -Date_ID, -ID)

StasSpecies %>%
  group_by(Genus) %>%
  tally() %>%
  mutate(Percentage = n / sum(n) * 100) %>%
  ungroup() %>%
  arrange(desc(Percentage))

StasSpecies %>%
  group_by(Location) %>%
  summarise(
    Species_Richness = n_distinct(Species),  # Number of unique species
    Abundance = n()  # Total number of individuals
  ) %>%
  ungroup() # Remove grouping




######### PLOTS OVER SPECIES AND ABUNDANCE ################


genus_colors2 <- c("Andrena" = "#8c510a", "Hylaeus" = "#80cdc1", "Apis" = "#bf812d", 
                   "Osmia" = "#01665e", "Lasioglossum" = "#35978f", "Bombus" = "#f6e8c3", 
                   "Hoplitis" = "#c7eae5")


### GENUS

SpeciesBerle <- OnlyPollinators %>%
  filter(Location == 'Berle') %>% 
  count(Genus, name = "count") %>%
  mutate(Genus = factor(Genus, levels = c("Halictus", "Hylaeus", "Hoplitis", "Osmia", "Lasioglossum", "Andrena", "Bombus", "Apis"))) %>%
  ggplot(aes(x = count, y = Genus, fill = Genus)) +
  geom_col() +
  labs(
    x = "",
    y = "",
    fill = "", 
    title = "Berle") +
  scale_fill_manual(values = genus_colors2) +
  theme_minimal() +                                      
  theme(legend.position = "none",
        axis.text = element_text(size = 14),
        axis.title = element_text(size = 17),
        plot.title = element_text(size = 18, face = "bold")) +
  xlim(0,110)

SpeciesHøyen <- OnlyPollinators %>%
  filter(Location == 'Høyen') %>% 
  count(Genus, name = "count") %>%
  mutate(Genus = factor(Genus, levels = c("Halictus", "Hylaeus", "Hoplitis", "Osmia", "Lasioglossum", "Andrena", "Bombus", "Apis"))) %>% 
  ggplot(aes(x = count, y = Genus, fill = Genus)) +
  geom_col() +
  labs(
    x = "",
    y = "",
    fill = "", 
    title = "Høyen") +
  scale_fill_manual(values = genus_colors2) +
  theme_minimal() +                                      
  theme(legend.position = "none",
        axis.text = element_text(size = 14),
        axis.title = element_text(size = 17),
        plot.title = element_text(size = 18, face = "bold")) +
  xlim(0,110)


SpeciesSando <- OnlyPollinators %>%
  filter(Location == 'Sando') %>% 
  count(Genus, name = "count") %>%
  mutate(Genus = factor(Genus, levels = c("Halictus", "Hylaeus", "Hoplitis", "Osmia", "Lasioglossum", "Andrena", "Bombus", "Apis"))) %>%
  ggplot(aes(x = count, y = Genus, fill = Genus)) +
  geom_col() +
  labs(
    x = "Number of individuals",
    y = "",
    fill = "", 
    title = "Sando") +
  scale_fill_manual(values = genus_colors2) +
  theme_minimal() +                                      
  theme(legend.position = "none",
        legend.text = element_text(size = 18),
        axis.text = element_text(size = 14),
        axis.title = element_text(size = 17),
        plot.title = element_text(size = 18, face = "bold")) +
  xlim(0,110)

SpeciesLofthus <- OnlyPollinators %>%
  filter(Location == 'Lofthus') %>% 
  count(Genus, name = "count") %>%
  mutate(Genus = factor(Genus, levels = c("Halictus", "Hylaeus", "Hoplitis", "Osmia", "Lasioglossum", "Andrena", "Bombus", "Apis"))) %>%
  ggplot(aes(x = count, y = Genus, fill = Genus)) +
  geom_col() +
  labs(
    x = "",
    y = "",
    fill = "", 
    title = "Lofthus") +
  scale_fill_manual(values = genus_colors2) +
  theme_minimal() +                                      
  theme(legend.position = "none",
        axis.text = element_text(size = 14),
        axis.title = element_text(size = 17),
        plot.title = element_text(size = 18, face = "bold")) +
  xlim(0,110)

SpeciesUrheim <- OnlyPollinators %>%
  filter(Location == 'Urheim') %>% 
  count(Genus, name = "count") %>%
  mutate(Genus = factor(Genus, levels = c("Halictus", "Hylaeus", "Hoplitis", "Osmia", "Lasioglossum", "Andrena", "Bombus", "Apis"))) %>%
  ggplot(aes(x = count, y = Genus, fill = Genus)) +
  geom_col() +
  labs(
    x = "",
    y = "",
    fill = "", 
    title = "Urheim") +
  scale_fill_manual(values = genus_colors2) +
  theme_minimal() +                                      
  theme(legend.position = "none",
        axis.text = element_text(size = 14),
        axis.title = element_text(size = 17),
        plot.title = element_text(size = 18, face = "bold")) +
  xlim(0,110)

SpeciesDjønno <- OnlyPollinators %>%
  filter(Location == 'Djønno') %>% 
  count(Genus, name = "count") %>%
  mutate(Genus = factor(Genus, levels = c("Halictus", "Hylaeus", "Hoplitis", "Osmia", "Lasioglossum", "Andrena", "Bombus", "Apis"))) %>%
  ggplot(aes(x = count, y = Genus, fill = Genus)) +
  geom_col() +
  labs(
    x = "Number of individuals",
    y = "",
    fill = "", 
    title = "Djønno") +
  scale_fill_manual(values = genus_colors2) +
  theme_minimal() +                                      
  theme(legend.position = "none",
        axis.text = element_text(size = 14),
        axis.title = element_text(size = 17),
        plot.title = element_text(size = 18, face = "bold"))


SpeciesLocation <- ggarrange(SpeciesBerle, SpeciesLofthus, SpeciesHøyen, SpeciesUrheim, SpeciesSando, SpeciesDjønno,
                             ncol = 2, nrow = 3)

ggsave(SpeciesLocation, filename = "Figures/SpeciesLocation.jpeg", height = 15, width = 18)





### SPECIES

SpeciesBerle <- OnlyPollinators %>%
  filter(Location == 'Berle', !is.na(Species)) %>%  
  count(Species, Genus, name = "count") %>%
  ungroup() %>%
  mutate(count = count + 1) %>%  # Offset to prevent log(0) issues
  mutate(Species = fct_reorder(Species, count, .desc = TRUE)) %>%
  ggplot(aes(x = count, y = fct_reorder(Species, count), fill = Genus)) +
  geom_col() +
  scale_x_log10(breaks = c(1, 5, 10, 50, 100, 500)) +
  expand_limits(x = 1) +  # Ensures values of 1 are visible
  labs(
    x = "",  
    y = "",
    fill = "", 
    title = "Berle") +
  scale_fill_manual(values = genus_colors2) +
  theme_minimal() +                                      
  theme(legend.position = "none",
        axis.text.y = element_text(size = 18),
        axis.text.x = element_text(size = 18),
        plot.title = element_text(size = 18, face = "bold"))



SpeciesHøyen <- OnlyPollinators %>%
  filter(Location == 'Høyen', !is.na(Species)) %>% 
  count(Species, Genus, name = "count") %>%
  ungroup() %>%
  mutate(count = count + 1) %>%
  mutate(Species = fct_reorder(Species, count, .desc = TRUE)) %>%
  ggplot(aes(x = count, y = fct_reorder(Species, count), fill = Genus)) +
  geom_col() +
  scale_x_log10(breaks = c(1, 5, 10, 50, 100, 500)) +
  expand_limits(x = 1) +
  labs(
    x = "",
    y = "",
    fill = "", 
    title = "Høyen") +
  scale_fill_manual(values = genus_colors2) +
  theme_minimal() +                                      
  theme(legend.position = "none",
        axis.text.y = element_text(size = 18),
        axis.text.x = element_text(size = 18),
        plot.title = element_text(size = 18, face = "bold"))



SpeciesSando <- OnlyPollinators %>%
  filter(Location == 'Sando', !is.na(Species)) %>% 
  count(Species, Genus, name = "count") %>%
  ungroup() %>%
  mutate(count = count + 1) %>%
  mutate(Species = fct_reorder(Species, count, .desc = TRUE)) %>%
  ggplot(aes(x = count, y = fct_reorder(Species, count), fill = Genus)) +
  geom_col() +
  scale_x_log10(breaks = c(1, 5, 10, 50, 100, 500)) +
  expand_limits(x = 1) + 
  labs(
    x = "log (species count)",
    y = "",
    fill = "", 
    title = "Sando") +
  scale_fill_manual(values = genus_colors2) +
  theme_minimal() +                                      
  theme(legend.position = "none", #change to "right" if you want the legend
        axis.text.y = element_text(size = 18),
        legend.text = element_text(size = 18),
        axis.text.x = element_text(size = 18),
        axis.title.x = element_text(size = 18),
        plot.title = element_text(size = 18, face = "bold"))

SpeciesLofthus <- OnlyPollinators %>%
  filter(Location == 'Lofthus', !is.na(Species)) %>% 
  count(Species, Genus, name = "count") %>%
  ungroup() %>%
  mutate(count = count + 1) %>%
  mutate(Species = fct_reorder(Species, count, .desc = TRUE)) %>%
  ggplot(aes(x = count, y = fct_reorder(Species, count), fill = Genus)) +
  geom_col() +
  scale_x_log10(breaks = c(1, 5, 10, 50, 100, 500)) +
  expand_limits(x = 1) + 
  labs(
    x = "",
    y = "",
    fill = "", 
    title = "Lofthus") +
  scale_fill_manual(values = genus_colors2) +
  theme_minimal() +                                      
  theme(legend.position = "none",
        axis.text.y = element_text(size = 18),
        axis.text.x = element_text(size = 18),
        plot.title = element_text(size = 18, face = "bold"))

SpeciesUrheim <- OnlyPollinators %>%
  filter(Location == 'Urheim', !is.na(Species)) %>% 
  count(Species, Genus, name = "count") %>%
  ungroup() %>%
  mutate(count = count + 1) %>%
  mutate(Species = fct_reorder(Species, count, .desc = TRUE)) %>%
  ggplot(aes(x = count, y = fct_reorder(Species, count), fill = Genus)) +
  geom_col() +
  scale_x_log10(breaks = c(1, 5, 10, 50, 100, 500)) +
  expand_limits(x = 1) + 
  labs(
    x = "",
    y = "",
    fill = "", 
    title = "Urheim") +
  scale_fill_manual(values = genus_colors2) +
  theme_minimal() +                                      
  theme(legend.position = "none",
        axis.text.y = element_text(size = 18),
        axis.text.x = element_text(size = 18),
        plot.title = element_text(size = 18, face = "bold"))

SpeciesDjønno <- OnlyPollinators %>%
  filter(Location == 'Djønno', !is.na(Species)) %>% 
  count(Species, Genus, name = "count") %>%
  ungroup() %>%
  mutate(count = count + 1) %>%
  mutate(Species = fct_reorder(Species, count, .desc = TRUE)) %>%
  ggplot(aes(x = count, y = fct_reorder(Species, count), fill = Genus)) +
  geom_col() +
  scale_x_log10(breaks = c(1, 5, 10, 50, 100, 500)) +
  expand_limits(x = 1) + 
  labs(
    x = "log (species count)",
    y = "",
    fill = "", 
    title = "Djønno") +
  scale_fill_manual(values = genus_colors2) +
  theme_minimal() +                                      
  theme(legend.position = "none",
        axis.text.y = element_text(size = 18),
        axis.text.x = element_text(size = 18),
        axis.title.x = element_text(size = 18),
        plot.title = element_text(size = 18, face = "bold"))





# Combine the plots using ggarrange, with common.legend set to TRUE
SpeciesLocation <- ggarrange(SpeciesBerle, SpeciesLofthus, SpeciesHøyen, SpeciesUrheim, SpeciesSando, SpeciesDjønno,
                             ncol = 2, nrow = 3)

ggsave(SpeciesLocation, filename = "Figures/SpeciesLocation.jpeg", height = 15, width = 18)




# Seed set ----------------------------------------------------------------

# Compute mean seed set ratio per Location
mean_seedset <- AppleDF %>%
  group_by(Location) %>%
  summarise(mean_seed = mean(seed_success / seed_total, na.rm = TRUE)) %>%
  arrange(mean_seed)

AppleDF <- AppleDF %>%
  left_join(mean_seedset, by = "Location")

# Use computed mean for reordering in ggplot
SeedSetLocation <- AppleDF %>%
  ggplot(aes(y = (seed_success/seed_total), 
             x = fct_reorder(Location, mean_seed),  # Order by mean seed set
             fill = Region, color = Region)) +
  geom_violin(alpha = 0.5) +
  geom_sina() +
  theme_minimal() +
  stat_summary(fun = mean, geom = "point", color = "black", size = 3) +
  scale_fill_manual(values = c("Svelvik" = "#9DC780", "Ullensvang" = "#304828")) +
  scale_color_manual(values = c("Svelvik" = "#9DC780", "Ullensvang" = "#304828")) +
  labs(y = "Seed set (ratio)", x = "", title = "a") +
  theme(axis.text = element_text(size = 14),
        axis.title = element_text(size = 17),
        plot.title = element_text(face = "bold", size = 22))


RicnessLocation <- PollTot_RA %>% 
  ggplot(aes(y = Nspecies, x = fct_relevel(Location, "Djønno", "Høyen", "Lofthus", "Urheim", "Berle", "Sando"), fill = Region, color = Region)) +
  geom_sina() +
  geom_violin(alpha = 0.5) +
  theme_minimal() +
  stat_summary(fun = mean, geom = "point", color = "black",size = 3) +
  scale_fill_manual(values = c("Svelvik" = "#C3D7A4", "Ullensvang" = "#52854C")) +
  scale_color_manual(values = c("Svelvik" = "#C3D7A4", "Ullensvang" = "#52854C")) +
  labs(y = "Species richness", x = "", title = "b")+
  theme(axis.text = element_text(size = 14),
        axis.title = element_text(size = 17),
        plot.title = element_text(face = "bold", size = 22))

AbundanceLocation <- PollTot_RA %>% 
  ggplot(aes(y = Species_sum, x = fct_relevel(Location, "Djønno", "Høyen", "Lofthus", "Urheim", "Berle", "Sando"), fill = Region, color = Region)) +
  geom_sina() +
  geom_violin(alpha = 0.5) +
  theme_minimal() +
  stat_summary(fun = median, geom = "point", color = "black",size = 3) +
  scale_fill_manual(values = c("Svelvik" = "#C3D7A4", "Ullensvang" = "#52854C")) +
  scale_color_manual(values = c("Svelvik" = "#C3D7A4", "Ullensvang" = "#52854C")) +
  labs(y = "Relative abundance", x = "", title = "c") +
  theme(axis.text = element_text(size = 14),
        axis.title = element_text(size = 17),
        plot.title = element_text(face = "bold", size = 22))


SeedSetSpecies <- ggarrange(SeedSetLocation, RicnessLocation, AbundanceLocation, nrow=3, common.legend = TRUE, legend = "bottom")
ggsave(SeedSetSpecies, filename = "Figures/SeedSetSpecies.jpeg", height = 14, width = 12)


OrchardStructurePlacement <- AppleDF1 %>% 
  ggplot(aes(y = seed_potential, x = Orchard_structure, fill = Placement, color = Placement)) +
  geom_sina() +
  geom_violin(alpha = 0.5, position = position_dodge(width = 0.9)) +
  theme_minimal() +
  stat_summary(fun = median, geom = "point", aes(group = Placement), position = position_dodge(width = 0.9), color = "black", size = 3) +
  scale_fill_manual(values = c("C" = "#660000", "E" = "#CC9966"),
    labels = c("C" = "Centre of orchard", "E" = "Edge of orchard")) +
  scale_color_manual(values = c("C" = "#660000", "E" = "#CC9966"),
    labels = c("C" = "Centre of orchard", "E" = "Edge of orchard")) +
  scale_x_discrete(labels = c("block" = "Block orchard design", "mixed" = "Integrated orchard design")) +
  labs(fill = "Tree placement", color = "Tree placement", y = "Seed set (ratio)", x = "") +
  theme(axis.text.x = element_text(size = 14),
        axis.text.y = element_text(size = 14),
        axis.title.y = element_text(size = 17))

ggsave(OrchardStructurePlacement, filename = "Figures/OrchardStructurePlacement.jpeg", height = 10, width = 12)














