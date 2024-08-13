pins_indicator <- main_merged %>%
  #Select only the previously calculated indicators
  select(
    c(
      #Sex (all members)
      MH_4,
      #Age1 years (all members)
      age1, 
      #Age2 months (all members)
      age2, 
      # pregnant women
      NUT_D1_Q1,
      #Food Security
      IND_FS_max,
      #Health
      IND_HE_max,
      #Humanitarian Transport
      IND_HT_max,
      #Integration
      IND_INT_D1_max,
      IND_INT_D2_max,
      IND_INT_D3_max,
      #Nutrition
      IND_NUT_D1,
      IND_NUT_D2,
      IND_NUT_D3,
      #Protection
      IND_PRO_D1_max,
      IND_PRO_D2_max,
      IND_PRO_D3_max,
      IND_PRO_CP_max,
      IND_PRO_GBV_max,
      IND_PRO_HTS_max,
      #Shelter
      IND_SHE_D1_max,
      IND_SHE_D2_max,
      IND_SHE_D3_max,
      #Wash
      IND_WA_D1_max,
      IND_WA_D2_max,
      IND_WA_D3_max,
      #Education
      IND_EDU_D1_max,
      IND_EDU_D2_max))%>%
      mutate(age1_rounded = floor(age1),
         age2_rounded = floor(age2),
         child_edu_cp=case_when(
           age1_rounded>=0& age1_rounded<18~"Yes",
           TRUE~"No"),
         child_0_5=case_when(
           age1_rounded>=0& age1_rounded<5~"Yes",
           TRUE~"No"),
         woman_preg_lact=case_when(MH_4=="women" & age1_rounded>=14 & (NUT_D1_Q1=="breastfeeding" |NUT_D1_Q1=="pregnant")~"Yes",
                                   TRUE~"No"))

# read indicators sheet with weights

#df_weights <- read_excel("df_weights.xlsx")

#Multiply each column of pins_indicator by the respective weight in df_weights

weighted_pins_indicator <- pins_indicator |>
  mutate(across(c(IND_FS_max,
                  IND_HE_max,
                  IND_HT_max, 
                  IND_INT_D1_max,IND_INT_D2_max, IND_INT_D3_max,
                  IND_NUT_D1,IND_NUT_D2,IND_NUT_D3,
                  IND_PRO_D1_max,IND_PRO_D2_max,IND_PRO_D3_max,IND_PRO_CP_max,IND_PRO_GBV_max,IND_PRO_HTS_max,
                  IND_SHE_D1_max, IND_SHE_D2_max,IND_SHE_D3_max,
                  IND_WA_D1_max, IND_WA_D2_max,IND_WA_D3_max,
                  IND_EDU_D1_max, IND_EDU_D2_max), ~ . * 
                  df_weights$ind_weight_per_sector[match(cur_column(), df_weights$ind_per_sector)]))

#Calculate sectorial deprivation score ds suming weighted indicators per sector

ds_per_sector <- weighted_pins_indicator %>%
  mutate(
    Food_security_ds = IND_FS_max,
    Health_ds =  IND_HE_max,
    Humanitarian_transport_ds = IND_HT_max,
    Integration_ds = rowSums(weighted_pins_indicator[, c("IND_INT_D1_max", "IND_INT_D2_max", "IND_INT_D3_max")], na.rm = TRUE),
    Nutrition_ds = rowSums(weighted_pins_indicator[, c("IND_NUT_D1","IND_NUT_D2", "IND_NUT_D3" )], na.rm = TRUE),
    Nutrition_ds_women = rowSums(weighted_pins_indicator[, c("IND_NUT_D1" )], na.rm = TRUE),
    Nutrition_ds_baby = rowSums(weighted_pins_indicator[, c("IND_NUT_D2", "IND_NUT_D3" )], na.rm = TRUE),
    Protection_ds = rowSums(weighted_pins_indicator[, c("IND_PRO_D1_max", "IND_PRO_D2_max", "IND_PRO_D3_max", "IND_PRO_CP_max", "IND_PRO_GBV_max","IND_PRO_HTS_max")], na.rm = TRUE),
    Child_protection_ds = IND_PRO_CP_max,
    Gender_based_violence_ds = IND_PRO_GBV_max,
    HT_S_ds = IND_PRO_HTS_max,
    Shelter_ds = rowSums(weighted_pins_indicator[, c("IND_SHE_D1_max", "IND_SHE_D2_max", "IND_SHE_D3_max")], na.rm = TRUE),
    Wash_ds = rowSums(weighted_pins_indicator[, c("IND_WA_D1_max","IND_WA_D2_max", "IND_WA_D3_max" )], na.rm = TRUE),
    Education_ds = rowSums(weighted_pins_indicator[, c("IND_EDU_D1_max", "IND_EDU_D2_max")], na.rm = TRUE)
  )%>%
  rename()

### Step 2: Calculate the intersectorial MPI using the scores from each sector 
#and using the 33.3% cutoff to identify those to the right of the distribution.
#Exclude subsectors - CP, HT and GBV (only calculate intersectoral MPI with 9 dimensions)

df_pin <- ds_per_sector %>%
  mutate(intersector_ds = rowSums(select(., c(
    "Food_security_ds",
    "Health_ds",
    "Humanitarian_transport_ds",
    "Integration_ds",
    "Nutrition_ds",
    "Protection_ds",
    "Shelter_ds",
    "Wash_ds",
    "Education_ds"
  )), na.rm = TRUE)) %>%
  # Calculate if included in intersector_pin with the threshold of 33.3% of intersector ds
  mutate(intersector_pin = ifelse(intersector_ds > 0.333, 1, 0))
  # ggplot of intersector_ds to see distribution
  
ggplot(df_pin, aes(x = intersector_ds)) +
  geom_density(fill = "blue", alpha = 0.9) +
  labs(title = "Density Plot of Intersector IPM Distribution",
       x = "Intersector ds",
       y = "Density") +
  theme_minimal()
  
pin_intersector <- sum(df_pin$"intersector_pin") / nrow(df_pin)

### Step 3: Identify individuals with deprivations in each sector. 
#Now, having the figure of the Intersectoral MPI, we can identify who is part of the MPI of each sector. 
#This ensures that, although a person may have all the deprivations in a sector, they will
#only be part of the MPI if they have more than 33.3% of the total deprivations.

df_sectoral_pin <- df_pin %>%
  select(-intersector_ds) %>%
  #include in sectorial pins if included in intersectorial pin (intersector== 1) 
  #and there is a privation in the specific sector (sector_ds>0)
  mutate(across(
    c(
      "Food_security_ds",
      "Health_ds",
      "Humanitarian_transport_ds",
      "Integration_ds",
      "Protection_ds",
      "Shelter_ds",
      "Wash_ds"
    ),
    ~ ifelse(intersector_pin == 1 & . > 0, 1, 0)
  )) |>
  rename_with(~ str_replace(., "_ds$", "_pin"),
              c(
                "Food_security_ds",
                "Health_ds",
                "Humanitarian_transport_ds",
                "Integration_ds",
                "Protection_ds",
                "Shelter_ds",
                "Wash_ds")) %>%
  mutate(across(
    c(
      "Gender_based_violence_ds",
      "HT_S_ds"
    ),
    ~ ifelse(Protection_pin == 1 & . > 0, 1, 0)
  ))|>
  #rename ds to pin
  rename_with( ~ str_replace(., "_ds$", "_pin")) 


#Education

df_sectoral_pin_edu <- df_pin %>%
  filter(child_edu_cp=="Yes")%>%
  select(-intersector_ds, Education_ds) %>%
  #include in sectorial pins if included in intersectorial pin (intersector== 1) 
  #and there is a privation in the specific sector (sector_ds>0)
  mutate(across(
    c(
      "Education_ds"
    ),
    ~ ifelse(intersector_pin == 1 & . > 0, 1, 0)
  )) |>
  #rename ds to pin
  rename_with( ~ str_replace(., "_ds$", "_pin")) 

#final Education pin
pins_final_edu <- data.frame(  
  Sectors = c(
   "Education"
  ),
  pins = round(colSums(select(df_sectoral_pin_edu, Education_pin)) / nrow(df_sectoral_pin_edu) *
                 100, 1)
)


#Nutrition women
df_sectoral_pin_nut_w <- df_pin %>%
  filter(woman_preg_lact=="Yes")%>%
  select(-intersector_ds, Nutrition_ds_women) %>%
  #include in sectorial pins if included in intersectorial pin (intersector== 1) 
  #and there is a privation in the specific sector (sector_ds>0)
  mutate(across(
    c(
      "Nutrition_ds_women"
    ),
    ~ ifelse(intersector_pin == 1 & . > 0, 1, 0)
  )) |>
  #rename ds to pin
  rename_with( ~ str_replace(., "_ds_", "_pin_")) 

#final Nutrition women pin
pins_final_nut_w <- data.frame(  
  Sectors = c(
    "Nutrition Woman"
  ),
  pins = round(colSums(select(df_sectoral_pin_nut_w, Nutrition_pin_women)) / nrow(df_sectoral_pin_nut_w) *
                 100, 1)
)


#Nutrition 0-5 years
df_sectoral_pin_nut_b <- df_pin %>%
  filter(child_0_5=="Yes")%>%
  select(-intersector_ds, Nutrition_ds_baby) %>%
  #include in sectorial pins if included in intersectorial pin (intersector== 1) 
  #and there is a privation in the specific sector (sector_ds>0)
  mutate(across(
    c(
      "Nutrition_ds_baby"
    ),
    ~ ifelse(intersector_pin == 1 & . > 0, 1, 0)
  )) |>
  #rename ds to pin
  rename_with( ~ str_replace(., "_ds_", "_pin_")) 

#final Nutrition baby pin
pins_final_nut_b <- data.frame(  
  Sectors = c(
    "Nutrition Baby"
  ),
  pins = round(colSums(select(df_sectoral_pin_nut_b, Nutrition_pin_baby)) / nrow(df_sectoral_pin_nut_b) *
                 100, 1)
)

#Child Protection
df_sectoral_pin_cp <- df_sectoral_pin %>%
  filter(child_edu_cp=="Yes")%>%
  #include in sectorial pins if included in intersectorial pin (intersector== 1) 
  #and there is a privation in the specific sector (sector_ds>0)
  mutate(across(
    c(
      "Child_protection_pin"
    ),
    ~ ifelse(Protection_pin == 1 & . > 0, 1, 0)
  )) |>
  #rename ds to pin
  rename_with( ~ str_replace(., "_ds$", "_pin")) 

#final Nutrition baby pin
pins_final_cp <- data.frame(  
  Sectors = c(
    "Child Protection"
  ),
  pins = round(colSums(select(df_sectoral_pin_cp, Child_protection_pin)) / nrow(df_sectoral_pin_cp) *
                 100, 1)
)


#final PIN table
pins_final <- data.frame(  
  Sectors = c(
    "Food_security",
    "Health",
    "Humanitarian_transport",
    "Integration",
    "Protection",
    "Gender_based_violence",
    "Human_Traficking_&_Smuggling",
    "Shelter",
    "Wash",
    "Intersector"
  ),
  pins = round(colSums(select(df_sectoral_pin, Food_security_pin, Health_pin, 
                              Humanitarian_transport_pin, Integration_pin,
                              Protection_pin, Gender_based_violence_pin,
                              HT_S_pin,
                              Shelter_pin, Wash_pin, intersector_pin)) / nrow(df_sectoral_pin) *
                 100, 1)
)

#merge EDU, NUT, CP and final

#merge

pins_final_final <- rbind(pins_final, pins_final_edu, pins_final_nut_w, pins_final_nut_b, pins_final_cp)

write_xlsx(pins_final_final, "pins_final_final.xlsx")


write_xlsx(pins_final, "rmna_pin2025.xlsx")
