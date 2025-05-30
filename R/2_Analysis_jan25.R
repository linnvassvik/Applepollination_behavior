source("R/1_ImportData.R")

library(glmmTMB)
library(performance)
#library(writexl)


# Species richness and abundance between locations ------------------------
richness_model <- glmmTMB(Species_Richness ~ Location, 
                          data = species_RA, 
                          family = poisson)

richness_model2 <- glmmTMB(Species_Richness ~ 1, 
                          data = species_RA, 
                          family = poisson)

summary(richness_model)
anova(richness_model, richness_model2)
#create a null model, and compare the two models. anova(model1, model2)

abundance_model <- glmmTMB(Abundance ~ Location, 
                           data = species_RA, 
                           family = poisson)

abundance_model2 <- glmmTMB(Abundance ~ 1, 
                           data = species_RA, 
                           family = poisson)

summary(abundance_model)
anova(abundance_model, abundance_model2)

# Use emmeans to compute the marginal means
emm <- emmeans(richness_model, ~ Location)
pairwise_comparisons <- contrast(emm, method = "pairwise", adjust = "tukey")
summary(pairwise_comparisons)



# Species richness and abundance between trap types and placement  --------
richness_model2 <- glmmTMB(Species_Richness ~ Where + Trap_color + (1|Location), 
                           data = species_RA, 
                           family = poisson)

richness_model2a <- glmmTMB(Species_Richness ~ Where + (1|Location), 
                           data = species_RA, 
                           family = poisson)

richness_model2b <- glmmTMB(Species_Richness ~ Trap_color + (1|Location), 
                            data = species_RA, 
                            family = poisson)

summary(richness_model2)
anova(richness_model2, richness_model2b)

abundance_model2 <- glmmTMB(Abundance ~ Where + Trap_color + (1|Location), 
                            data = species_RA, 
                            family = poisson)

abundance_model2a <- glmmTMB(Abundance ~ Where + (1|Location), 
                            data = species_RA, 
                            family = poisson)

abundance_model2b <- glmmTMB(Abundance ~ Trap_color + (1|Location), 
                            data = species_RA, 
                            family = poisson)

summary(abundance_model2)
anova(abundance_model2, abundance_model2b)

# Use emmeans to compute the marginal means
emm <- emmeans(richness_model2, ~  Where)
pairwise_comparisons <- contrast(emm, method = "pairwise", adjust = "tukey")
summary(pairwise_comparisons)



# Behaviour ---------------------------------------------------------------

# Foraging time (s) per flower

Behaviour_foraging <- Behaviour_foraging %>%
  mutate(Forage_time_s = as.numeric(as.character(Forage_time_s)))

ForagingTime <- glmmTMB(log(Forage_time_s) ~ Subject + (1|Location), 
                        data = Behaviour_foraging, 
                        family = gaussian)

ForagingTime2 <- glmmTMB(log(Forage_time_s) ~ 1 + (1|Location), 
                        data = Behaviour_foraging, 
                        family = gaussian)

anova(ForagingTime, ForagingTime2)

summary(ForagingTime)

emm <- emmeans(ForagingTime, ~ Subject)
pairwise_comparisons <- contrast(emm, method = "pairwise", adjust = "tukey")
summary(pairwise_comparisons)


# Percentage of flower visits with stigma contact per observation 

StigmaContact <- glmmTMB(cbind(stigma_contact, no_stigma_contact) ~ Subject + (1|Location),
                         family = betabinomial(link = "logit"),
                         data = Behaviour_stigma)

StigmaContact2 <- glmmTMB(cbind(stigma_contact, no_stigma_contact) ~ 1 + (1|Location),
                         family = betabinomial(link = "logit"),
                         data = Behaviour_stigma)

anova(StigmaContact,StigmaContact2)

summary(StigmaContact)
check_model(StigmaContact)


emm <- emmeans(StigmaContact, ~ Subject)
pairwise_comparisons <- contrast(emm, method = "pairwise", adjust = "tukey")
summary(pairwise_comparisons)



# Percentage of flower visits with stigma contact per observation accross the flowering season

StigmaContact2 <- glmmTMB(cbind(stigma_contact, no_stigma_contact) ~ Subject * DOY + (1|Location), 
                          family = betabinomial(link = "logit"),
                          data = Behaviour_stigma2)


summary(StigmaContact2)

emtrends(StigmaContact2, ~ Subject, var = "DOY")
test(emtrends(StigmaContact2, ~ Subject, var = "DOY"))

emm <- emmeans(StigmaContact2, ~ Subject)
pairwise_comparisons <- contrast(emm, method = "pairwise", adjust = "tukey")
summary(pairwise_comparisons)


#Flight distance between each foraging visit

Distance <- glmmTMB(log(percentage) ~ Behavior * Subject + (1|Location), 
                     data = Behaviour5, 
                     family = gaussian)

Distance2 <- glmmTMB(log(percentage) ~  Subject + (1|Location), 
                    data = Behaviour5, 
                    family = gaussian)

anova(Distance, Distance2)

summary(Distance)


emm <- emmeans(Distance, ~ Behavior)
pairwise_comparisons <- contrast(emm, method = "pairwise", adjust = "tukey")
summary(pairwise_comparisons)




# Seed set varies between locations, orchard design and tree placement  --------

LocationSS <- glmmTMB(cbind(seed_success, seed_fail) ~ Location,
                      family = betabinomial(link = "logit"),
                      data = AppleDF1)

LocationSS2 <- glmmTMB(cbind(seed_success, seed_fail) ~ 1,
                      family = betabinomial(link = "logit"),
                      data = AppleDF1)

anova(LocationSS, LocationSS2)

summary(LocationSS)

emmeans_results3 <- emmeans(LocationSS, ~ Location)
summary(emmeans_results3)
pairwise_comparisons3 <- pairs(emmeans_results3)
summary(pairwise_comparisons3)

#Seed set varies with orchard structure and tree placement
OrchardSS2 <- glmmTMB(cbind(seed_success, seed_fail) ~ Orchard_structure * Placement + (1|Location/Apple_variety),
                     family = betabinomial(link = "logit"),
                     data = AppleDF1)


summary(OrchardSS2)

emmeans_results3 <- emmeans(OrchardSS2, ~ Orchard_structure * Placement)
summary(emmeans_results3)
pairwise_comparisons3 <- pairs(emmeans_results3)
summary(pairwise_comparisons3)


# Seed set and species richness and abundance -----------------------------
# AppleSpecies$seed_success <- round(AppleSpecies$seed_success)
# AppleSpecies$seed_fail <- round(AppleSpecies$seed_fail)
# AppleSpecies$seed_total <- round(AppleSpecies$seed_total)

#only wild bees

RichAbSS1 <- glmmTMB(cbind(seed_success, seed_fail) ~ Species_Richness * Abundance + (1|Location),
                      family = betabinomial(link = "logit"),
                      data = RichnessSeedSet2)



summary(RichAbSS1)




# #only wild bee abundance
# RichAbSS2 <- glmmTMB(cbind(seed_success, seed_fail) ~ Abundance + (1|Location),
#                       family = betabinomial(link = "logit"),
#                       data = RichnessSeedSet2)


#only honeybee
RichAbSS3 <- glmmTMB(cbind(seed_success, seed_fail) ~ Abundance + (1|Location),
                      family = betabinomial(link = "logit"),
                      data = RichnessSeedSet3)


summary(RichAbSS3)




# General  ----------------------------------------------------------------

Behaviour %>%
  group_by(Subject) %>% 
  summarise(unique_IDs = n_distinct(ID))

#Number of visits per bee group
Behaviour %>%
  filter(Category == "Foraging") %>%
  group_by(Subject, ID) %>% 
  summarise(n_events = n(), .groups = "drop") %>% 
  group_by(Subject) %>%
  summarise(mean_events_per_visit = mean(n_events),
    se_events_per_visit = sd(n_events) / sqrt(n()),
    .groups = "drop")


#Foraging time per observation per bee group
Behaviour %>%
  filter(Category == "Foraging") %>%
  mutate(Forage_time_s = as.numeric(Forage_time_s)) %>%
  group_by(Subject) %>%
  summarise(mean_forage_time_s = mean(Forage_time_s, na.rm = TRUE),
    se_forage_time_s = sd(Forage_time_s, na.rm = TRUE) / sqrt(sum(!is.na(Forage_time_s))),
    .groups = "drop")


#Number of stigma contact per bee group
Behaviour_stigma %>%
  mutate(stigma_contact = foraging_count - no_stigma_contact) %>% 
  group_by(Subject) %>%
  summarise(
    stigma_contact = sum(stigma_contact),
    total_foraging = sum(foraging_count),
    prop_stigma_contact = stigma_contact / total_foraging,
    se_stigma_contact = sqrt((prop_stigma_contact * (1 - prop_stigma_contact)) / total_foraging),
    .groups = "drop"
  )


#Movement between flowers, trees and rows/further
Behaviour %>%
  filter(Category == "Travel distance") %>%
  group_by(Subject, Behavior) %>%
  summarise(count = n(), .groups = "drop") %>%
  group_by(Subject) %>%
  mutate(
    total = sum(count),
    percentage = (count / total) * 100,
    se = sqrt((percentage * (100 - percentage)) / total))

## mean seed set, abundance and richness per location

RichnessSeedSet %>%
  mutate(seed_ratio = seed_success / seed_total) %>%      
  group_by(Location) %>%                                 
  summarise(mean_seed_ratio = mean(seed_ratio, na.rm = TRUE))

RichnessSeedSet %>%    
  group_by(Location) %>%                                 
  summarise(mean_Abundance  = mean(Abundance, na.rm = TRUE))

RichnessSeedSet %>%    
  group_by(Location) %>%                                 
  summarise(mean_Species_Richness   = mean(Species_Richness , na.rm = TRUE))

OnlyPollinators %>%
  group_by(Location) %>%
  summarise(total_species = n_distinct(Species))
