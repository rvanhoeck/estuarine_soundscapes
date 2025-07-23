


# load data
toadfishModData = read.csv("data/clean_data/ToadfishModData.csv", header = TRUE)


### v1 Modeling toadfish - Avg call rate ####
# included in original manuscript, needs to be corrected to percent presence

TmetricModP_avg = lm(BoatAvgPresent ~ richness_comb + total_cover, data = toadfishModData)
TmetricModL_avg = lm(BoatAvgPresent ~ oyster_perimeter + saltmarsh_area, data = toadfishModData)
TmetricModPL_avg = lm(BoatAvgPresent ~ richness_comb + total_cover + oyster_perimeter + saltmarsh_area, data = toadfishModData)

model.sel(TmetricModPL_avg, TmetricModP_avg, TmetricModL_avg)
#AICc: L,P indistinguishable between L & P mod alone, used PL to capture variation from both

# Variance partitioning 
Tpatch_avg = summary(TmetricModPL_avg)$r.squared - summary(TmetricModL_avg)$r.squared
Tlandscape_avg = summary(TmetricModPL_avg)$r.squared - summary(TmetricModP_avg)$r.squared
Ttotal_avg = summary(TmetricModPL_avg)$r.squared 






### v2 Modeling toadfish - Percent Presence (as percent, with linear model) ####

TmetricModP_lm = lm(BoatPercentPresence ~ richness_comb + total_cover, data = toadfishModData)
TmetricModL_lm = lm(BoatPercentPresence ~ oyster_perimeter + saltmarsh_area, data = toadfishModData)
TmetricModPL_lm = lm(BoatPercentPresence ~ richness_comb + total_cover + oyster_perimeter + saltmarsh_area, data = toadfishModData)

model.sel(TmetricModPL_lm, TmetricModP_lm, TmetricModL_lm)
#AICc: L 

# No variance partioning, landscape alone was best model fit
Tlandscape_lm = summary(TmetricModL_lm)$r.squared 
Ttotal_lm = summary(TmetricModL_lm)$r.squared





### v3 Modeling toadfish - percent presence (as proportion - binomial glm) ####

TmetricModP = glm(BoatProportionPresence ~ richness_comb + total_cover, data = toadfishModData, family = binomial())
TmetricModL = glm(BoatProportionPresence ~ oyster_perimeter + saltmarsh_area, data = toadfishModData, family = binomial())
TmetricModPL = glm(BoatProportionPresence ~ richness_comb + total_cover + oyster_perimeter + saltmarsh_area, data = toadfishModData, family = binomial())

model.sel(TmetricModPL, TmetricModP, TmetricModL)
#AICc: tie between P and L, use PL to capture variation from both

# binomial model with nfiles as weights - much higher AIC for all 3 models. Moving forward with binomial without weights
# TmetricModP_w = glm(BoatProportionPresence ~ richness_comb + total_cover, data = ecometrics, family = binomial(), weights = nFiles)
# TmetricModL_w = glm(BoatProportionPresence ~ oyster_perimeter + saltmarsh_area, data = ecometrics, family = binomial(), weights = nFiles)
# TmetricModPL_w = glm(BoatProportionPresence ~ richness_comb + total_cover + oyster_perimeter + saltmarsh_area, data = ecometrics, family = binomial(), weights = nFiles)

# Variance partitioning requires a psuedo r-squared - using Nagelkerke's as its most comparable to lm r squared
#install.packages("fmsb")
library(fmsb)
Tpatch = NagelkerkeR2(TmetricModPL)$R2 - NagelkerkeR2(TmetricModL)$R2
Tlandscape = NagelkerkeR2(TmetricModPL)$R2 - NagelkerkeR2(TmetricModP)$R2
Ttotal = NagelkerkeR2(TmetricModPL)$R2 





#### Summarize 3 model outputs in table analagous to Table 1 ####
mods = c("AvgPresent", "PercentPresenceLM", "ProportionPresentBinomial")
patchScale = c(Tpatch_avg, NA, Tpatch)
landscapeScale = c(Tlandscape_avg, Tlandscape_lm, Tlandscape)
totalExp = c(Ttotal_avg, Ttotal_lm, Ttotal)

toadfishModOutput = as.data.frame(rbind(patchScale, landscapeScale, totalExp))
colnames(toadfishModOutput) = mods
