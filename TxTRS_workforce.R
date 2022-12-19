source("TxTRS_R_BModel revised.R")
library(data.table)

#Initialize empty workforce projection arrays
ea <- SalaryEntry$entry_age
age <- Age
year <- YearStart:2130   #test now, fix this later
term_year <- year
retire_year <- year


active_dim <- c(length(ea), length(age), length(year))
active_dim_names <- list(ea, age, year)

term_dim <- c(length(ea), length(age), length(year), length(term_year))
term_dim_names <- list(ea, age, year, term_year)

retire_dim <- c(length(ea), length(age), length(year), length(term_year), length(retire_year))
retire_dim_names <- list(ea, age, year, term_year, retire_year)

wf_active <- array(0, dim = active_dim, dimnames = active_dim_names)
wf_term <- array(0, dim = term_dim, dimnames = term_dim_names)
wf_refund <- wf_term
wf_retire <- array(0, dim = retire_dim, dimnames = retire_dim_names)

#Initial population
active_int_pop <- c(2000, 8000, 8000, 7000, 8000, 9000, 8000, 7000, 6000, 5000)

active_int_ea <- data.frame(ea = SalaryEntry$entry_age, age = SalaryEntry$entry_age, n.active = active_int_pop)

active_int <- expand_grid(ea, age) %>% 
  left_join(active_int_ea) %>% 
  replace(is.na(.), 0) %>% 
  pivot_wider(names_from = age, values_from = n.active) %>% 
  select(-1)

wf_active[,,1] <- as.matrix(active_int)


##Create probability array

#Mortality probability array (4 dimensions)
mort_df_term <- expand_grid(ea, age, year, term_year) %>% 
  left_join(MortalityTable, by = c("ea" = "entry_age", "age" = "Age", "year" = "Years", "term_year")) %>% 
  mutate(mort = ifelse(is.na(mort), 0, mort))

mort_array_term <- xtabs(mort ~ ea + age + year + term_year, mort_df_term)

#Separation probability array (3 dimensions): 
sep_df <- expand_grid(ea, age, year) %>% 
  mutate(ey = year - (age - ea)) %>% 
  left_join(SeparationRates, by = c("ea" = "entry_age", "age" = "Age", "ey" = "EntryYear")) %>% 
  select(ea, age, year, SepRate) %>% 
  mutate(SepRate = ifelse(is.na(SepRate), 0, SepRate))

sep_array <- xtabs(SepRate ~ ea + age + year, sep_df)

#Refund and retirement probability arrays
#Determine the optimal retirement age
optimal_retire <- OptimumBenefit %>% 
  left_join(BenefitsTable %>% filter(PresentValue > 0), by = c("EntryYear", "entry_age", "term_age", "MaxBenefit" = "PresentValue")) %>% 
  select(EntryYear, entry_age, term_age, Age, MaxBenefit) %>% 
  rename(retire_age = Age) %>% 
  left_join(FinalData, by = c("EntryYear", "entry_age", "term_age" = "Age", "MaxBenefit")) %>% 
  select(EntryYear, entry_age, term_age, YOS, retire_age, MaxBenefit, DBEEBalance) %>% 
  mutate(ben_decision = ifelse(YOS == 0, NA, ifelse(DBEEBalance > MaxBenefit, "refund", "retire")),
         refund = case_when(ben_decision == "refund" ~ 1,     #use case_when instead of ifselse to handle NA values better
                            TRUE ~ 0),
         retire = case_when(ben_decision == "retire" ~ 1,
                            TRUE ~ 0),
         refund_age = term_age)

#Retire probability array (4 dimensions)
retire_df <- expand_grid(ea, age, year, term_year) %>% 
  mutate(entry_year = year - (age - ea),
         term_age = age - (year - term_year),
         YOS = term_age - ea) %>% 
  filter(year - term_year >= 0, YOS >= 0) %>% 
  left_join(optimal_retire, by = c("ea" = "entry_age",
                                   "age" = "retire_age",
                                   "entry_year" = "EntryYear",
                                   "term_age",
                                   "YOS")) %>% 
  mutate(retire = ifelse(is.na(retire), 0, retire))

retire_array <- xtabs(retire ~ ea + age + year + term_year, retire_df) 

#Refund probability array (4 dimensions). Note that employees get refunds in the same year they get terminated. 
refund_df <- expand_grid(ea, age, year, term_year) %>% 
  mutate(entry_year = year - (age - ea),
         term_age = age - (year - term_year),
         YOS = term_age - ea) %>% 
  filter(year - term_year >= 0, YOS >= 0) %>% 
  left_join(optimal_retire, by = c("ea" = "entry_age",
                                   "age" = "refund_age",
                                   "entry_year" = "EntryYear",
                                   "term_age",
                                   "YOS")) %>% 
  mutate(refund = ifelse(is.na(refund), 0, refund))

refund_array <- xtabs(refund ~ ea + age + year + term_year, refund_df)

#Transition matrix to shift the population to the right by 1 age after 1 year
TM <-  diag(length(age) + 1)[-1, -(length(age) + 1)] 

#Workforce projection
for (i in 2:length(year)) {
  active2term <- wf_active[,,i-1] * sep_array[,,i-1]   #calculate the # of newly terminated actives. 2-dimensional array
  
  wf_active[,,i] <- (wf_active[,,i-1] - active2term) %*% TM  #add new entrants later
  
  term2death <- wf_term[,,i-1,] * mort_array_term[,,i-1,] #3-dimensional array
  
  wf_term[,,i,] <- apply(wf_term[,,i-1,] - term2death, 3, function(x) x %*% TM) %>% array(term_dim[-3]) 
  
  wf_term[,,i,i] <- active2term %*% TM   #add newly terminated members the term population
  
  term2refund <- wf_term[,,i,i] * refund_array[,,i,i]  #calculate the # of newly refunded members. 2-dimensional array
  
  wf_term[,,i,i] <- wf_term[,,i,i] - term2refund
  
  wf_refund[,,i,i] <- term2refund
  
  term2retire <- wf_term[,,i,] * retire_array[,,i,]  #calculate the # of newly retired members. 3-dimensional array
  
  wf_term[,,i,] <- wf_term[,,i,] - term2retire
  
  retire2death <- apply(wf_retire[,,i-1,,], 4, function(x) x * mort_array_term[,,i-1,]) %>% array(retire_dim[-3])   #4-dimensional array
  
  wf_retire[,,i,,] <- apply(wf_retire[,,i-1,,] - retire2death, c(3,4), function(x) x %*% TM) %>% array(retire_dim[-3])
  
  wf_retire[,,i,,i] <- term2retire
  
}


#####Convert the multidimensional arrays into data frames 
wf_active_df <- data.frame(expand.grid(ea = ea, age = age, year = year), n.active = as.vector(wf_active)) %>% filter(age >= ea)

wf_term_df <- data.frame(expand.grid(ea = ea, age = age, year = year, term_year = term_year), n.term = as.vector(wf_term)) %>% 
  filter(age >= ea, year >= term_year)

wf_refund_df <- data.frame(expand.grid(ea = ea, age = age, year = year, term_year = term_year), n.refund = as.vector(wf_refund)) %>% 
  filter(age >= ea, year >= term_year)

#Since the wf_retire array is too big to handle using the above method, we need to split it into smaller parts for processing
wf_retire_list <- list()  #empty list to save retire workforce data in the for loop

for (i in seq_along(SalaryEntry$entry_age)) {
  wf_retire_name <- paste0("wf_retire_", SalaryEntry$entry_age[i])
  assign(wf_retire_name, wf_retire[i,,,,])
  wf_retire_i <- data.table(CJ(retire_year, term_year, year, age), n.retire = as.vector(get(wf_retire_name)))[n.retire > 0,] %>% 
    mutate(ea = SalaryEntry$entry_age[i])
  assign(wf_retire_name, wf_retire_i)   #do this to save memory space
  wf_retire_list <- append(wf_retire_list, list(get(wf_retire_name)))
}

#Combine all retire data frames from the retire list into one retire data frame 
wf_retire_df <- rbindlist(wf_retire_list) %>% 
  select(ea, age, year, term_year, retire_year, n.retire)

#Join wf active table with normal cost and salary table to calculate the overall payroll and normal costs each year
wf_active_df_final <- wf_active_df %>% 
  mutate(entry_year = year - (age - ea)) %>% 
  left_join(NormalCost, by = c("ea" = "entry_age", "entry_year" = "EntryYear")) %>% 
  left_join(SalaryData, by = c("ea" = "entry_age", "age" = "Age", "year" = "Years", "entry_year" = "EntryYear")) %>% 
  select(ea, age, year, entry_year, n.active, normal_cost, Salary) %>% 
  replace(is.na(.), 0) %>% 
  group_by(year) %>% 
  summarise(payroll = sum(Salary * n.active),
            nc_rate = sum(normal_cost * Salary * n.active) / sum(Salary * n.active)) %>% 
  ungroup() %>% 
  replace(is.na(.), 0)
  

#Join wf refund table with benefit table to calculate the overall refunds each year
wf_refund_df_final <- wf_refund_df %>% 
  left_join(BenefitsTable, by = c("ea" = "entry_age", "age" = "Age", "year" = "Years", "term_year")) %>% 
  select(ea, age, year, term_year, n.refund, DBEEBalance) %>% 
  replace(is.na(.), 0) %>% 
  group_by(year) %>% 
  summarise(refund = sum(DBEEBalance * n.refund)) %>% 
  ungroup()

#Join wf retire table with benefit table to calculate the overall retirement benefits each year
wf_retire_df_final <- wf_retire_df %>% 
  mutate(entry_year = year - (age - ea)) %>%    
  left_join(BenefitsTable, by = c("ea" = "entry_age", "entry_year" = "EntryYear", "term_year", "retire_year" = "Years")) %>% 
  select(ea, age, year, term_year, retire_year, n.retire, PensionBenefit) %>% 
  rename(base_benefit = PensionBenefit) %>% #let's ignore COLA for now 
  group_by(year) %>% 
  replace(is.na(.), 0) %>% 
  summarise(retire_ben = sum(base_benefit * n.retire)) %>% 
  ungroup()


#Mini funding model
funding_df <- wf_active_df_final %>% 
  left_join(wf_refund_df_final) %>% 
  left_join(wf_retire_df_final) %>% 
  replace(is.na(.), 0)


write.csv(funding_df, "funding_df_TexsTRS.csv")
