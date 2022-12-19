####################################
# TxTRS Normal Cost/Benefit Model #
####################################

rm(list = ls())
library("readxl")
library(tidyverse)
library(zoo)
library(profvis)

#### Start the Timing
#profvis({


FileName <- 'TxTRS_BM_Inputs.xlsx'


ModelPeriod <- 100    #Projection period (typically 30 years)
MinAge <- 20          #Age of the typical youngest member
MaxAge <- 120         #Max age from mortality assumptions
YearStart <- 2021     #Year of the latest val report
MinYear <- 1980       #No hard rule about this. Should get back to about 40 years from now.   
MaxYear <- YearStart + ModelPeriod + MaxAge - MinAge

EntryYear <- MinYear:(YearStart + ModelPeriod)  
Years <- MinYear:MaxYear
Age <- MinAge:MaxAge
YOS <- 0:70
RetirementAge <- Age

#Assigning individual  Variables
model_inputs <- read_excel(FileName, sheet = 'Main')

for(i in 1:nrow(model_inputs)){
  if(!is.na(model_inputs[i,2])){
    assign(as.character(model_inputs[i,2]),as.double(model_inputs[i,3]))
  }
}


#Import key data tables
SurvivalRates <- read_excel(FileName, sheet = 'Mortality Rates')
MaleMP <- read_excel(FileName, sheet = 'MP-2018_Male') 
FemaleMP <- read_excel(FileName, sheet = 'MP-2018_Female')

SalaryGrowthYOS <- read_excel(FileName, sheet = "Salary Growth YOS")
SalaryMatrix <- read_excel(FileName, sheet = "Salary Matrix") 
HeadCountMatrix <- read_excel(FileName, sheet = "Head Count Matrix")

TerminationRateAfter10 <- read_excel(FileName, sheet = 'Termination Rates after 10')
TerminationRateBefore10 <- read_excel(FileName, sheet = 'Termination Rates before 10')
RetirementRates <- read_excel(FileName, sheet = 'Retirement Rates')

ReducedGFT <- read_excel(FileName, sheet = "Reduced GFT")
ReducedOthers <- read_excel(FileName, sheet = "Reduced Others")

#Joining headcount data, salary data, and salary growth data
SalaryMatrix_long <- SalaryMatrix %>% 
  pivot_longer(cols = -1, names_to = "YOS", values_to = "Salary")
HeadCountMatrix_long <- HeadCountMatrix %>% 
  pivot_longer(cols = -1, names_to = "YOS", values_to = "Count")

SalaryHeadCountData <- SalaryMatrix_long %>%
  left_join(HeadCountMatrix_long) %>% 
  replace(is.na(.), 0) %>% 
  mutate(YOS = as.numeric(YOS),
         CurrentYear = YearStart,
         entry_age = Age - YOS,
         EntryYear = CurrentYear - YOS) %>% 
  filter(Salary > 0, entry_age >= 18)

#Salary entry is the starting salary data 
SalaryEntry <- SalaryHeadCountData %>%  
  filter(YOS == 2) %>% 
  select(entry_age, CurrentYear, Salary) %>% 
  rename(start_sal = Salary)


#Custom function to calculate cumulative future values  
cumFV <- function(interest, cashflow){
  cumvalue <- double(length = length(cashflow))
  for (i in 2:length(cumvalue)) {
    cumvalue[i] <- cumvalue[i - 1]*(1 + interest) + cashflow[i - 1]
  }
  return(cumvalue)
}


################
# Main rule: Retirement Eligibility
################

#Grandfathered criteria
#To be grandfathered, one must have met at least one of the following conditions on or before 2005:
#1. at least 50 years old
#2. Age plus YOS >= 70
#3. YOS >= 25
IsGrandfathered <- function(Age, YOS, EntryYear, EntryAge){
  YOS_2005 <- pmin(YOS, 2005 - EntryYear)
  Age_2005 <- pmin(Age, 2005 - EntryYear + EntryAge)
  Check <- ifelse(EntryYear <= 2005 & 
                    (Age_2005 >= 50 |
                       Age_2005 + YOS_2005 >= 70 |
                       YOS_2005 >= 25), TRUE, FALSE)
  return(Check)
}

#Determine retirement eligibility
IsRetirementEligible_Normal <- function(Age, YOS, EntryYear){
  Check <- ifelse((Age >= NormalRetAgeI & YOS >= NormalYOSI) |
                    (EntryYear <= 2007 & YOS + Age >= NormalRetRule & YOS >= NormalYOSI) |
                    (EntryYear > 2007 & EntryYear <= 2009 & YOS + Age >= NormalRetRule & Age >= NormalRetRuleAgeI & YOS >= NormalYOSI) |   #for those hired after 2007 and vested as of 2014
                    (EntryYear > 2009 & YOS + Age >= NormalRetRule & Age >= NormalRetRuleAgeII & YOS >= NormalYOSI), TRUE, FALSE)          #for those not vested as of 2014
  return(Check)
}

IsRetirementEligible_Early <- function(Age, YOS, EntryYear){
  Check <- ifelse((Age >= ReduceRetAge & YOS >= NormalYOSI) |
                    (YOS >= EarlyRetYOS) |
                    (EntryYear > 2007 & YOS + Age >= NormalRetRule & YOS >= NormalYOSI), TRUE, FALSE)
  return(Check)
}


IsRetirementEligible <- function(Age, YOS, EntryYear){
  Check <- ifelse(IsRetirementEligible_Normal(Age, YOS, EntryYear) == T | IsRetirementEligible_Early(Age, YOS, EntryYear) == T, TRUE, FALSE)
  return(Check)
}


#Determine retirement type
RetirementType <- function(Age, YOS, EntryYear){
  Check = ifelse(IsRetirementEligible_Normal(Age, YOS, EntryYear) == T, "Normal",
                 ifelse(IsRetirementEligible_Early(Age, YOS, EntryYear) == T, "Early", "None"))
  
  return(Check)
}


##Transform base mortality rates and mortality improvement rates
#Add age 19 and 18 to the mortality improvement tables (with the same values as age 20)
# MaleMP_20 <- as.numeric(as.vector(MaleMP[1,]))[-1]
# MaleMP_18 <- c(18, MaleMP_20)
# MaleMP_19 <- c(19, MaleMP_20)
# MaleMP <- rbind(MaleMP, MaleMP_18, MaleMP_19) %>% arrange(Age)
# 
# FemaleMP_20 <- as.numeric(as.vector(FemaleMP[1,]))[-1]
# FemaleMP_18 <- c(18, FemaleMP_20)
# FemaleMP_19 <- c(19, FemaleMP_20)
# FemaleMP <- rbind(FemaleMP, FemaleMP_18, FemaleMP_19) %>% arrange(Age)

#Turn MP tables into long format and impute values for years after the max year in the MP tables
MaleMP <- MaleMP %>% 
  pivot_longer(-Age, names_to = "Years", values_to = "MP_male") %>% 
  mutate(Years = as.numeric(Years))

MaleMP_ultimate <- MaleMP %>%        #ultimate rates = rates for the last year in the MP table
  filter(Years == max(Years)) %>% 
  rename(MP_ultimate_male = MP_male) %>% 
  select(-Years)

MaleMP_final <- expand_grid(Age, Years = 1951:MaxYear) %>% 
  left_join(MaleMP, by = c("Age", "Years")) %>% 
  left_join(MaleMP_ultimate, by = "Age") %>% 
  mutate(
    # MP_final_male = ifelse(Years > max(MaleMP$Years), MP_ultimate_male, MP_male)   
    #Since the plan assumes "immediate convergence" of MP rates, the "ultimate rates" are used for all years
    MP_final_male = MP_ultimate_male) %>% 
  group_by(Age) %>% 
  mutate(MPcumprod_male_raw = cumprod(1 - MP_final_male),
         MPcumprod_male_adj = MPcumprod_male_raw / MPcumprod_male_raw[Years == 2014]) %>%   #Adjust the mort improvement rates for the 2014 base year
  ungroup()


FemaleMP <- FemaleMP %>% 
  pivot_longer(-Age, names_to = "Years", values_to = "MP_female") %>% 
  mutate(Years = as.numeric(Years))

FemaleMP_ultimate <- FemaleMP %>% 
  filter(Years == max(Years)) %>% 
  rename(MP_ultimate_female = MP_female) %>% 
  select(-Years)

FemaleMP_final <- expand_grid(Age, Years = 1951:MaxYear) %>% 
  left_join(FemaleMP, by = c("Age", "Years")) %>% 
  left_join(FemaleMP_ultimate, by = "Age") %>% 
  mutate(
    # MP_final_female = ifelse(Years > max(FemaleMP$Years), MP_ultimate_female, MP_female)
    #Since the plan assumes "immediate convergence" of MP rates, the "ultimate rates" are used for all years
    MP_final_female = MP_ultimate_female
  ) %>%
  group_by(Age) %>% 
  mutate(MPcumprod_female_raw = cumprod(1 - MP_final_female),
         MPcumprod_female_adj = MPcumprod_female_raw / MPcumprod_female_raw[Years == 2014]) %>% 
  ungroup()

##Mortality calculations
#Mortality initial setup
MortalityTable_int <- expand_grid(EntryYear, Age, Years, YOS)

MortalityTable_int <- MortalityTable_int %>% 
  mutate(term_year = EntryYear + YOS,
         entry_age = Age - (Years - EntryYear)) %>% 
  #Years here is the same as retirement/refund years
  filter(term_year <= Years,
         entry_age %in% SalaryEntry$entry_age) %>% 
  arrange(EntryYear, entry_age, YOS, Age)


#Join base mortality table with mortality improvement table and calculate the final mortality rates
MortalityTable <- MortalityTable_int %>% 
  left_join(SurvivalRates, by = "Age") %>% 
  left_join(MaleMP_final, by = c("Age", "Years")) %>% 
  left_join(FemaleMP_final, by = c("Age", "Years")) %>% 
  
  #MPcumprod is the cumulative product of (1 - MP rates), starting from 2011. We use it later so make life easy and calculate now
  mutate(
    RetirementCond = IsRetirementEligible(Age, YOS, EntryYear),
    mort_male = ifelse(RetirementCond == T, RP_2014_ann_employee_male, 
                       RP_2014_employee_male * ScaleMultiple) * MPcumprod_male_adj, 
    mort_female = ifelse(RetirementCond == T, RP_2014_ann_employee_female, 
                         RP_2014_employee_female * ScaleMultiple) * MPcumprod_female_adj,
    mort = (mort_male + mort_female)/2)

#filter out the necessary variables
MortalityTable <- MortalityTable %>% 
  select(EntryYear, term_year, Years, entry_age, Age, YOS, mort)

######################
######################

#Separation Rates (only apply to active members)
#Because separation rates are subject to retirement eligibility, which depends on entry years (tiers), the separation table needs to take entry years into account
SeparationRates <- expand_grid(EntryYear, Age, YOS) %>% 
  mutate(entry_age = Age - YOS) %>% 
  filter(entry_age %in% SalaryEntry$entry_age) %>% 
  arrange(EntryYear, entry_age, Age) %>% 
  #Calculate the years from normal retirement
  group_by(EntryYear, entry_age) %>% 
  mutate(normal_retire = ifelse(RetirementType(Age, YOS, EntryYear) == "Normal", Age, 0),
         first_retire = min(normal_retire[normal_retire != 0]),
         YearsFromNR = pmax(first_retire - Age, 0)) %>% 
  left_join(TerminationRateBefore10, by = "YOS") %>% 
  left_join(TerminationRateAfter10, by = "YearsFromNR") %>% 
  left_join(RetirementRates, by = "Age") %>% 
  replace(is.na(.), 0) %>%    
  
  #If retirement eligible, use the retirement rates, or else check if YOS < 10 and use the "before 10" term rates, or else use the "after 10" term rates.
  mutate(retirement_type = RetirementType(Age, YOS, EntryYear),
         SepRateMale = ifelse(retirement_type == "Normal", NormalMale,
                              ifelse(retirement_type == "Early", ReducedMale,
                                     ifelse(YOS < 10, TermBefore10Male, TermAfter10Male))),
         
         SepRateFemale = ifelse(retirement_type == "Normal", NormalFemale,
                                ifelse(retirement_type == "Early", ReducedFemale,
                                       ifelse(YOS < 10, TermBefore10Female, TermAfter10Female))),
         
         SepRate = ((SepRateMale + SepRateFemale)/2),
         
         RemainingProb = cumprod(1 - lag(SepRate, default = 0)),
         SepProb = lag(RemainingProb, default = 1) - RemainingProb) %>% 
  ungroup()

#Filter out unecessary values
SeparationRates <- SeparationRates %>% 
  select(EntryYear, entry_age, Age, YOS, SepRate, RemainingProb, SepProb)

##Salary Projection
#Create a long-form table of EntryYear, Age, YOS and merge with salary data
SalaryData <- expand_grid(EntryYear, Age, YOS) %>% 
  mutate(entry_age = Age - YOS,
         Years = EntryYear + YOS) %>% 
  filter(entry_age %in% SalaryEntry$entry_age) %>% 
  arrange(EntryYear, entry_age, YOS) %>% 
  select(EntryYear, entry_age, Age, YOS, Years) %>% 
  left_join(SalaryEntry, by = "entry_age") %>% 
  left_join(SalaryGrowthYOS, by = "YOS") %>% 
  group_by(EntryYear, entry_age) %>% 
  mutate(GFT = IsGrandfathered(Age, YOS, EntryYear, entry_age),
         FinAvgSalaryYears = ifelse(GFT == T, FinAvgSalaryYears_gft, FinAvgSalaryYears_current),
         Salary = start_sal * cumprod(1 + lag(salary_increase, default = 0)) * (1 + payroll_growth)^(Years - YOS - YearStart),
         FinalAvgSalary = rollmean(lag(Salary), k = FinAvgSalaryYears, fill = NA, align = "right"),
         EEContrib = DB_EE_cont * Salary,
         DBEEBalance = cumFV(Interest, EEContrib),
         CumulativeWage = cumFV(ARR, Salary)) %>% 
  ungroup()

#Reduced Factor to account for early retirement
#Convert Reduced Factor table for the grandfathered into long form and impute the rates with YOS > 30
ReducedGFT_long <- ReducedGFT %>% 
  pivot_longer(cols = -1, names_to = "Age", values_to = "RedGFT") %>% 
  mutate(Age = as.numeric(Age))

ReducedGFT_mod <- expand_grid(YOS, Age = 55:60) %>% 
  left_join(ReducedGFT_long) %>% 
  group_by(Age) %>% 
  mutate(RedGFT2 = ifelse(YOS > 30, max(RedGFT, na.rm = T), RedGFT)) %>% 
  select(YOS, Age, RedGFT2) %>% 
  ungroup()

ReducedFactor <- expand_grid(EntryYear, entry_age = SalaryEntry$entry_age, Age, YOS) %>% 
  filter(entry_age + YOS <= Age) %>% 
  left_join(ReducedGFT_mod) %>% 
  left_join(ReducedOthers) %>% 
  mutate(RetType = RetirementType(Age, YOS, EntryYear),
         RF = ifelse(RetType == "Normal", 1,
                     ifelse(RetType == "Early", 
                            ifelse(EntryYear <= 2007 & YOS >= 30, 1 - 0.02*(50 - Age),
                                   ifelse(IsGrandfathered(Age, YOS, EntryYear, entry_age) == T & YOS >= 20, RedGFT2,
                                          ifelse(EntryYear > 2007 & EntryYear <= 2009 & (YOS + Age >= NormalRetRule | YOS >= 30), 1 - 0.05*(60 - Age),
                                                 ifelse(EntryYear > 2009 & (YOS + Age >= NormalRetRule | YOS >= 30), 1 - 0.05*(62 - Age), RedOthers)))),
                            0))) %>% 
  rename(RetirementAge = Age)


#Survival Probability and Annuity Factor
AnnFactorData <- MortalityTable %>% 
  group_by(EntryYear, entry_age, YOS) %>% 
  mutate(surv = cumprod(1 - lag(mort, default = 0)),
         surv_DR = surv/(1+ARR)^(Age - min(Age)),
         surv_DR_COLA = surv_DR * (1+COLA)^(Age - min(Age)),
         AnnuityFactor = rev(cumsum(rev(surv_DR_COLA)))/surv_DR_COLA) %>% 
  ungroup()

#Benefits and Present Value
BenefitsTable <- AnnFactorData %>% 
  mutate(term_age = entry_age + YOS) %>% 
  left_join(SalaryData, by = c("EntryYear", "term_year" = "Years", "entry_age", "term_age" = "Age", "YOS")) %>% 
  left_join(ReducedFactor, by = c("EntryYear", "entry_age", "Age" = "RetirementAge", "YOS")) %>% 
  mutate(AnnFactorAdj = AnnuityFactor * surv_DR,
         PensionBenefit = RF * BenMult * FinalAvgSalary * YOS,
         PensionBenefit = ifelse(RF == 1, pmax(PensionBenefit, 150 * 12), PensionBenefit),   #normal retirement benefit = greater of standard annuity, or $150 per month
         PresentValue = PensionBenefit * AnnFactorAdj)


#For a given combination of entry age and termination age, the member is assumed to choose the retirement age that maximizes the PV of future retirement benefits. That value is the "optimum benefit". 
OptimumBenefit <- BenefitsTable %>% 
  group_by(EntryYear, entry_age, term_age) %>% 
  summarise(MaxBenefit = max(PresentValue)) %>%
  mutate(MaxBenefit = ifelse(is.na(MaxBenefit), 0, MaxBenefit)) %>% 
  ungroup()

# Benefit Accrual & Normal Cost #
#### Real Pension Wealth = Pension Wealth adjusted for inflation
#### Actuarial PV of Pension Wealth = Pension Wealth discounted back to entry age, multiplied by separation probability
#Combine optimal benefit with employee balance and calculate the PV of future benefits and salaries 
#####################################
FinalData <- SalaryData %>% 
  left_join(OptimumBenefit, by = c("EntryYear", "entry_age", "Age" = "term_age")) %>% 
  left_join(SeparationRates, by = c("EntryYear", "entry_age", "Age", "YOS")) %>%
  mutate(PenWealth = pmax(DBEEBalance,MaxBenefit),        #Members are assumed to elect the option with the greatest PV between a refund with interest and a deferred benefit
         RealPenWealth = PenWealth/(1 + assum_infl)^YOS,
         PVPenWealth = PenWealth/(1 + ARR)^YOS * SepProb,
         PVCumWage = CumulativeWage/(1 + ARR)^YOS * SepProb)

#Calculate normal cost rate for each entry age in each entry year
NormalCost <- FinalData %>% 
  group_by(EntryYear, entry_age) %>% 
  summarise(normal_cost = sum(PVPenWealth)/sum(PVCumWage)) %>% 
  ungroup()

#Calculate the aggregate normal cost
NC_aggregate <- NormalCost %>% 
  left_join(SalaryHeadCountData, by = c("EntryYear", "entry_age")) %>% 
  filter(!is.na(CurrentYear)) %>% 
  summarise(normal_cost_aggregate = sum(normal_cost * Salary * Count) / sum(Salary * Count)) %>% 
  pull()





# 
# 
# 
# 
# ####### DC Account Balance 
# #SalaryData1.2 <- SalaryData %>% filter(entry_age ==27 & Age < 81)
# ####### DC Account Balance 
# if(isTRUE(DC)){
#   
#   SalaryData2 <- SalaryData %>% filter(entry_age == e.age & Age < 81)
#   
#   SalaryData2 <- SalaryData2 %>% 
#     #filter(entry_age == HiringAge) %>% 
#     select(Age, YOS, entry_age, start_sal, salary_increase, Salary, RemainingProb, PVPenWealth,YearsFirstRetire) %>% 
#     mutate(DC_EEContrib = Salary * DC_EE_cont,
#            DC_ERContrib = Salary * DC_ER_cont,
#            DC_Contrib = DC_EEContrib + DC_ERContrib,
#            DC_balance = cumFV(DC_return, DC_Contrib),
#            RealDC_balance = DC_balance/(1 + assum_infl)^YOS) %>% 
#     left_join(SalaryData %>% select(Age, YOS, RealPenWealth), by = c("Age", "YOS")) %>% 
#     mutate(RealHybridWealth = RealDC_balance + RealPenWealth)
#   
#   SalaryData2 <- data.frame(SalaryData2)
#   SalaryData2$entry_age <- as.numeric(SalaryData2$entry_age)
# }else{SalaryData2 <- SalaryData %>% select(entry_age,Age, YOS, PVPenWealth, RealPenWealth, YearsFirstRetire)}
# 
# ######### Graphing SINGLE ENTRY AGE + RETENTION
# 
# # #View(SalaryData2)
# 
# 
# ########## Normal Cost #######
# #Calculate the aggregate normal cost
# 
# if(isTRUE(NCost)){
#   
#   #Calculate normal cost rate for each entry age
#   NormalCost <- SalaryData %>% 
#     group_by(entry_age) %>% 
#     summarise(normal_cost = sum(PVPenWealth)/sum(PVCumWage)) %>% 
#     ungroup()
#   
#   #View(NormalCost)
#   
#   #Calculate the aggregate normal cost
#   NC_aggregate <- sum(NormalCost$normal_cost * SalaryEntry$start_sal * SalaryEntry$count_start)/
#     sum(SalaryEntry$start_sal * SalaryEntry$count_start)
#   
#   NC_aggregate}else{SalaryData2}
# 
# #Blend: 10.86% (Class II: 11.09%)
# #Teachers: 11.16%
# #General: 10.85%
# #Blend Class II/III: 11.09%/10.86%
# 
# ###
# #Class II & III payroll (5,235 & 4,253)
# #(10.86*5235 + 11.09*4253)/(5235+4253)
# 
# #Weighted: 10.96%
# #Val: 10.77%
# 
# #Adjustment:
# #10.77/10.96
# #0.9826642
# 
# #0.9826642*11.09
# #Class II: 10.90%
# 
# #0.9826642*10.86
# #Class III: 10.67%
# 
# 
# #### End the Timing
# #})
# 
# #### BenefitModel ####
# 
# SalaryData2 <- data.frame(
#   BenefitModel(employee = "Blend", #"Teachers", "General"
#                tier = 3, #tier 2 for Legacy
#                NCost = FALSE, #(TRUE -- calculates GNC on original SalaryData)
#                DC = TRUE, #(TRUE -- calculates DC using e.age)
#                e.age = 27, #for DC
#                ARR.base =  ARR , #can set manually
#                COLA.base = COLA, #can set manually
#                BenMult.base = BenMult , #can set manually
#                DC_EE_cont =  0.08, #can set manually
#                DC_ER_cont = 0.06, #can set manually
#                DC_return = 0.06)
# )
# ################################
# 
# #View(SalaryData2)
# #data <- SalaryData2 %>% select(entry_age, Age, YOS, RealPenWealth)
# 
# #Save outputs
# #write_csv(data, "SCRS_Benefits_all_Ages.csv")
# 
# #########
# # View(SalaryData2 %>% filter(YOS == 20) %>%
# #        select(entry_age, Age, YOS, RealPenWealth)) 
# 
# # View(SalaryData2 %>% filter(Age + YOS >= 90 & Age + YOS < 92) %>%
# #         select(entry_age, Age, YOS, RealPenWealth))
# 
# ## Graphing PWealth accrual [ALL ENTRY AGES]
# 
# # ggplot(SalaryData, aes(Age,RealPenWealth/1000, group = entry_age, col = as.factor(entry_age)))+
# #   geom_line(size = 1)+
# #   theme_bw()+
# #   scale_x_continuous(breaks = seq(0, 80, by = 10),labels = function(x) paste0(x), 
# #                      name = "Age (Entry age at 27)", expand = c(0,0)) + 
# #   scale_y_continuous(breaks = seq(0, 5000, by = 100),labels = function(x) paste0("$",x), 
# #                      name = "Present Value of Pension Wealth ($Thousands)", expand = c(0,0)) 
# ##################################
# 
# #write_csv(SalaryData2, "SCRS_BModel.csv")
# ######### Graphing SINGLE ENTRY AGE + RETENTION
# 
# palette_reason <- list(Orange="#FF6633",
#                        LightOrange="#FF9900",
#                        DarkGrey="#333333",
#                        LightGrey= "#CCCCCC",
#                        SpaceGrey ="#A69FA1",
#                        DarkBlue="#0066CC",
#                        GreyBlue= "#6699CC",
#                        Yellow= "#FFCC33",
#                        LightBlue = "#66B2FF",
#                        SatBlue = "#3366CC",
#                        Green = "#669900",LightGreen = "#00CC66", Red = "#CC0000",LightRed="#FF0000")
# 
# ##############
# #
# EntryAge <- 27
# SalaryData2 <- SalaryData2 %>% filter(entry_age == EntryAge)
# SalaryData2 <- SalaryData2 %>% filter(Age < 81)
# SalaryData2 <- SalaryData2 %>% mutate(PVPenWealth = RealPenWealth)
# SalaryData2$PVPenWealth <- as.numeric(SalaryData2$RealPenWealth)
# y_max <- max(SalaryData2$PVPenWealth)
# 
# ############## Chart
# 
# pwealth <- ggplot(SalaryData2, aes(Age,PVPenWealth/1000, fill = "DB Accrual Pattern"))+
#   geom_line(aes(group = 1,
#                 text = paste0("Age: ", Age,
#                               "<br>DB Pension Wealth: $",round(PVPenWealth/1000,1), " Thousands")),size = 1.25, color = palette_reason$SatBlue)+
#   geom_line(aes(Age, RealDC_balance/1000,
#                 group = 2,
#                 text = paste0("Age: ", Age,
#                               "<br>DC Wealth at ", DC_return*100,"% : $", round(RealDC_balance/1000,1), " Thousands"),fill = "DC Accrual Pattern"), size = 1, color = palette_reason$Orange)+
#   geom_line(aes(Age, RemainingProb* (y_max/1000),
#                 group = 3,
#                 text = paste0("Age: ", Age,
#                               "<br>Members Remaining: ", round(RemainingProb*100,1), "%"),fill = "Share or Members Remaining"), size = 1, color = palette_reason$LightBlue, linetype = "dashed")+
#   scale_x_continuous(breaks = seq(0, 80, by = 10),labels = function(x) paste0(x),
#                      name = paste0("Age (Entry age at 27)"), expand = c(0,0)) +
#   
#   scale_y_continuous(breaks = seq(0, y_max/1000, by = 100),limits = c(0, y_max/1000*1.1), labels = function(x) paste0("$",x),
#                      sec.axis = ggplot2::sec_axis(~./(y_max/100),
#                                                   breaks = scales::pretty_breaks(n = 10), name = "Percent of Members Remaining",
#                                                   labels = function(b) paste0(round(b, 0), "%")),
#                      name = "Present Value of Pension Wealth ($Thousands)", expand = c(0,0)) +
#   theme_bw()+
#   theme(   #panel.grid.major = element_blank(),
#     panel.grid.minor = element_blank(),
#     axis.line = element_line(colour = "black"),
#     plot.margin = margin(0.5, 0.5,0.5,0.5, "cm"),
#     axis.text.y = element_text(size=11, color = "black"),
#     axis.text.y.right = element_text(size=11, color = "black"),
#     axis.text.y.left = element_text(size=11, color = "black"),
#     axis.text.x = element_text(size=11, color = "black"),
#     legend.title = element_text(size = 9, colour = "black", face = "bold"),
#     legend.text = element_text(size = 9),
#     legend.position = "bottom")
# 
# #### Secondary axis
# 
# ax2 <- list(
#   overlaying = "y",
#   side = "right",
#   showticklabels = TRUE,
#   range = c(0,110),
#   title = "Percent of Employees Remaining (%)",
#   automargin = T,
#   showgrid = FALSE,
#   titlefont = list(size = 15),
#   tickvals = seq(0, 110, by = 25),
#   tickfont = list(size = 15)) # I added this line
# 
# library(plotly)
# ggplotly(pwealth, tooltip = c("text")) %>%
#   add_markers(yaxis = "y2") %>% # new line
#   layout(
#     yaxis2 = ax2)
# 
# 
# 
# ###########
# ###########
# 
# # pwealth <- ggplot(SalaryData2, aes(Age,PVPenWealth/1000, fill = "SCRS DB Accrual Pattern"))+
# #   geom_line(aes(group = 1,
# #                 text = paste0("Age: ", Age,
# #                               "<br>DB Pension Wealth: $",round(PVPenWealth/1000,1), " Thousands")),size = 1.25, color = palette_reason$SatBlue)+
# #   geom_line(aes(Age, RealDC_balance/1000,
# #                 group = 2,
# #                 text = paste0("Age: ", Age,
# #                               "<br>DC Wealth at ", DC_return*100,"% : $", round(RealDC_balance/1000,1), " Thousands"),fill = "SCRS DC Accrual Pattern"), size = 1.25, color = palette_reason$Orange)+
# #   #geom_line(aes(Age, RemainingProb* (y_max/1000),
# #   #              group = 3,
# #   #              text = paste0("Age: ", Age,
# #   #                            "<br>Members Remaining: ", round(RemainingProb*100,1), "%"),fill = "Share or Members Remaining"), size = 1, color = palette_reason$LightBlue, linetype = "dashed")+
# #   scale_x_continuous(breaks = seq(0, 80, by = 10),labels = function(x) paste0(x),
# #                      name = paste0("Age (Entry age at 27)"), expand = c(0,0)) +
# # 
# #   scale_y_continuous(breaks = seq(0, y_max*1.15/1000, by = 100),limits = c(0, y_max/1000*1.15), labels = function(x) paste0("$",x),
# #                      #sec.axis = ggplot2::sec_axis(~./(y_max/100),
# #                      #breaks = scales::pretty_breaks(n = 10), name = "Percent of Members Remaining",
# #                      #labels = function(b) paste0(round(b, 0), "%")),
# #                      name = "Present Value of Pension Wealth ($Thousands)", expand = c(0,0)) +
# #   theme_bw()+
# #   theme(   #panel.grid.major = element_blank(),
# #     panel.grid.minor = element_blank(),
# #     axis.line = element_line(colour = "black"),
# #     plot.margin = margin(0.5, 0.5,0.5,0.5, "cm"),
# #     axis.text.y = element_text(size=11, color = "black"),
# #     axis.text.y.right = element_text(size=11, color = "black"),
# #     axis.text.y.left = element_text(size=11, color = "black"),
# #     axis.text.x = element_text(size=11, color = "black"),
# #     legend.title = element_text(size = 9, colour = "black", face = "bold"),
# #     legend.text = element_text(size = 9),
# #     legend.position = "bottom")+
# #   labs(caption = paste("reason.org/pensions"))
# # 
# # #### Secondary axis
# # 
# # # ax2 <- list(
# # #   overlaying = "y",
# # #   side = "right",
# # #   showticklabels = TRUE,
# # #   range = c(0,110),
# # #   title = "Percent of Employees Remaining (%)",
# # #   automargin = T,
# # #   showgrid = FALSE,
# # #   titlefont = list(size = 15),
# # #   tickvals = seq(0, 110, by = 25),
# # #   tickfont = list(size = 15)) # I added this line
# # 
# # library(plotly)
# # ggplotly(pwealth, tooltip = c("text"))
# # # %>%
# # #   add_markers(yaxis = "y2") %>% # new line
# # #   layout(
# # #     yaxis2 = ax2)
# # 
# # pretention <- ggplot(SalaryData2, aes(Age, RemainingProb*100,fill = "Share of New SCRS Members Remaining",
# #                                       group = 3,
# #                                       text = paste0("Age: ", Age,
# #                                                     "<br>Members Remaining: ", round(RemainingProb*100,1), "%")))+
# #   # geom_line(aes(group = 1,
# #   #               text = paste0("Age: ", Age,
# #   #                             "<br>DB Pension Wealth: $",round(PVPenWealth/1000,1), " Thousands")),size = 1.25, color = palette_reason$SatBlue)+
# #   # geom_line(aes(Age, RealDC_balance/1000,
# #   #               group = 2,
# #   #               text = paste0("Age: ", Age,
# #   #                             "<br>DC Wealth at ", DC_return*100,"% : $", round(RealDC_balance/1000,1), " Thousands"),fill = "SCRS DC Accrual Pattern"), size = 1, color = palette_reason$Orange)+
# #   geom_line(size = 0.75, color = palette_reason$LightBlue, linetype = "dashed")+
# #   scale_x_continuous(breaks = seq(0, 80, by = 10),labels = function(x) paste0(x),
# #                      name = paste0("Age (Entry age at 27)"), expand = c(0,0)) +
# # 
# #   scale_y_continuous(#breaks = seq(0, y_max*1.15/1000, by = 100),limits = c(0, y_max/1000*1.15), labels = function(x) paste0("$",x),
# #     #sec.axis = ggplot2::sec_axis(~./(y_max/100),
# #     breaks = seq(0, 100, by = 10),limits = c(0,100),
# #     labels = function(b) paste0(round(b, 0), "%"),
# #     name = "Percent of SCRS Employees Remaining",
# #     expand = c(0,0)) +
# #   theme_bw()+
# #   theme(   #panel.grid.major = element_blank(),
# #     panel.grid.minor = element_blank(),
# #     axis.line = element_line(colour = "black"),
# #     plot.margin = margin(0.5, 0.5,0.5,0.5, "cm"),
# #     axis.text.y = element_text(size=11, color = "black"),
# #     axis.text.y.right = element_text(size=11, color = "black"),
# #     axis.text.y.left = element_text(size=11, color = "black"),
# #     axis.text.x = element_text(size=11, color = "black"),
# #     legend.title = element_text(size = 9, colour = "black", face = "bold"),
# #     legend.text = element_text(size = 9))+
# #     labs(caption = paste("reason.org/pensions"))
# # 
# # #### Secondary axis
# # 
# # # ax2 <- list(
# # #   overlaying = "y",
# # #   side = "right",
# # #   showticklabels = TRUE,
# # #   range = c(0,110),
# # #   title = "Percent of Employees Remaining (%)",
# # #   automargin = T,
# # #   showgrid = FALSE,
# # #   titlefont = list(size = 15),
# # #   tickvals = seq(0, 110, by = 25),
# # #   tickfont = list(size = 15)) # I added this line
# # 
# # ###Pension wealth
# # library(plotly)
# # pwealth
# 
# ###Retention
# #ggplotly(pretention, tooltip = c("text"))
# #pdf = 4.8 x 7.8
# 
# #######################

