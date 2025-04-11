source("R/1_ImportData.R")

library(glmmTMB)
library(performance)
#library(writexl)


# Species richness and abundance between locations ------------------------
richness_model <- glmmTMB(Species_Richness ~ Location, 
                          data = species_RA, 
                          family = poisson)

summary(richness_model)

abundance_model <- glmmTMB(Abundance ~ Location, 
                           data = species_RA, 
                           family = poisson)

summary(abundance_model)

# Use emmeans to compute the marginal means
emm <- emmeans(abundance_model, ~ Location)
pairwise_comparisons <- contrast(emm, method = "pairwise", adjust = "tukey")
summary(pairwise_comparisons)



# Species richness and abundance between trap types and placement  --------
richness_model2 <- glmmTMB(Species_Richness ~ Where + Trap_color + (1|Location), 
                           data = species_RA, 
                           family = poisson)
summary(richness_model2)

abundance_model2 <- glmmTMB(Abundance ~ Where + Trap_color + (1|Location), 
                            data = species_RA, 
                            family = poisson)
summary(abundance_model2)

# Use emmeans to compute the marginal means
emm <- emmeans(abundance_model2, ~ Where)
pairwise_comparisons <- contrast(emm, method = "pairwise", adjust = "tukey")
summary(pairwise_comparisons)



# Behaviour ---------------------------------------------------------------

# Foraging time (s) per flower

Behaviour_foraging <- Behaviour_foraging %>%
  mutate(Forage_time_s = as.numeric(as.character(Forage_time_s)))

ForagingTime <- glmmTMB(log(Forage_time_s) ~ Subject + (1|Location), 
                        data = Behaviour_foraging, 
                        family = gaussian())
summary(ForagingTime)

emm <- emmeans(ForagingTime, ~ Subject)
pairwise_comparisons <- contrast(emm, method = "pairwise", adjust = "tukey")
summary(pairwise_comparisons)


# Percentage of flower visits with stigma contact per observation 

StigmaContact <- glmmTMB(cbind(stigma_contact, no_stigma_contact) ~ Subject + (1|Location),
                         family = betabinomial(link = "logit"),
                         data = Behaviour_stigma)


summary(StigmaContact)


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
                     data = Behaviour4, 
                     family = gaussian())

summary(Distance)

emm <- emmeans(Distance, ~ Behavior * Subject)
pairwise_comparisons <- contrast(emm, method = "pairwise", adjust = "tukey")
summary(pairwise_comparisons)



# Seed set and location, orchard design and tree placement --------------------------
#Seed set varies between locations
LocationSS <- glmmTMB(cbind(seed_success, seed_fail) ~ Location,
                      family = betabinomial(link = "logit"),
                      data = AppleDF1)


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

emmeans_results3 <- emmeans(OrchardSS, ~ Orchard_structure * Placement)
summary(emmeans_results3)
pairwise_comparisons3 <- pairs(emmeans_results3)
DesignTree <- summary(pairwise_comparisons3)


# Seed set and species richness and abundance -----------------------------
AppleSpecies$seed_success <- round(AppleSpecies$seed_success)
AppleSpecies$seed_fail <- round(AppleSpecies$seed_fail)
AppleSpecies$seed_total <- round(AppleSpecies$seed_total)

RichAbSS <- glmmTMB(cbind(seed_success, seed_fail) ~ Species_Richness * Abundance + (1|Location),
                  family = binomial(link = "logit"),
                  data = AppleSpecies)



summary(RichAbSS)
check_model(RichAbSS2)
