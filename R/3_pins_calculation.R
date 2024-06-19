###Step 1: Identify individuals with deprivations in each indicator (first cut based on predefined thresholds)

pins_indicator <- main_merged |>
  #Select only the previously calculated indicators
  select(
    c(
      #Food Security
      FS_CARI,
      #Health
      HE_D1,
      HE_D4,
      #Humanitarian Transport
      HT_D1,
      #Integration
      INT_D1,
      INT_D2,
      INT_D3,
      INT_D4,
      #Nutrition
      NUT_D1,
      NUT_D4,
      NUT_D5,
      NUT_D8,
      NUT_D10,
      #Protection
      PRO_D1,
      PRO_D2,
      PRO_D3,
      PRO_D4,
      PRO_D5,
      #Child Protection
      CP_D1,
      #Gender Based Violence
      GBV_D1,
      GBV_D3,
      #Human Traficking & Smuggling
      HTS_D1,
      HTS_D2,
      #Shelter
      SHE_D1,
      SHE_D2,
      SHE_D3,
      SHE_D4,
      #Wash
      WA_D1,
      WA_D2,
      WA_D4,
      WA_D6,
      WA_D8,
      WA_D11,
      #Education
      EDU_D1,
      EDU_D2,
      EDU_D3
    )
  )


# read indicators sheet with weights

df_weights <- read_excel("df_weights.xlsx")

#Multiply each column of pins_indicator by the respective weight in df_weights

weighted_pins_indicator <- pins_indicator |>
  mutate(across(everything(), ~ . * df_weights$ind_weight_per_sector[match(cur_column(), df_weights$ind_per_sector)]))

#Calculate sectorial deprivation score ds suming weighted indicators per sector

ds_per_sector <- weighted_pins_indicator |>
  mutate(
    Food_security_ds = rowSums(weighted_pins_indicator[, c("FS_CARI")], na.rm = TRUE),
    Health_ds =  rowSums(weighted_pins_indicator[, c("HE_D1", "HE_D4")], na.rm = TRUE),
    Humanitarian_transport_ds = rowSums(weighted_pins_indicator[, c("HT_D1")], na.rm = TRUE),
    Integration_ds = rowSums(weighted_pins_indicator[, c("INT_D1", "INT_D2", "INT_D3", "INT_D4")], na.rm = TRUE),
    Nutrition_ds = rowSums(weighted_pins_indicator[, c("NUT_D1", "NUT_D4", "NUT_D5", "NUT_D8", "NUT_D10")], na.rm = TRUE),
    Protection_ds = rowSums(weighted_pins_indicator[, c("PRO_D1", "PRO_D2", "PRO_D3", "PRO_D4", "PRO_D5")], na.rm = TRUE),
    Child_protection_ds = rowSums(weighted_pins_indicator[, c("CP_D1")], na.rm = TRUE),
    Gender_based_violence_ds = rowSums(weighted_pins_indicator[, c("GBV_D1", "GBV_D3")], na.rm = TRUE),
    HT_S_ds = rowSums(weighted_pins_indicator[, c("HTS_D1", "HTS_D2")], na.rm = TRUE),
    Shelter_ds = rowSums(weighted_pins_indicator[, c("SHE_D1", "SHE_D2", "SHE_D3", "SHE_D4")], na.rm = TRUE),
    Wash_ds = rowSums(weighted_pins_indicator[, c("WA_D1", "WA_D2", "WA_D4", "WA_D6", "WA_D8", "WA_D11")], na.rm = TRUE),
    Education_ds = rowSums(weighted_pins_indicator[, c("EDU_D1", "EDU_D2", "EDU_D3")], na.rm = TRUE)
  )

### Step 2: Calculate the intersectorial MPI using the scores from each sector and using the 33.3% cutoff to identify those to the right of the distribution.


df_pin <- ds_per_sector |>
  select(
    #select only ds columns
    c(
      Food_security_ds,
      Health_ds,
      Humanitarian_transport_ds,
      Integration_ds,
      Nutrition_ds,
      Protection_ds,
      Child_protection_ds,
      Gender_based_violence_ds,
      HT_S_ds,
      Shelter_ds,
      Wash_ds,
      Education_ds
    )
  )


#sum sectorial ds to intersectorial ds
df_pin <- df_pin |>
  mutate(intersector_ds = rowSums(df_pin[, c(
    "Food_security_ds",
    "Health_ds",
    "Humanitarian_transport_ds",
    "Integration_ds",
    "Nutrition_ds",
    "Protection_ds",
    "Child_protection_ds",
    "Gender_based_violence_ds",
    "HT_S_ds",
    "Shelter_ds",
    "Wash_ds",
    "Education_ds"
  )], na.rm = TRUE)) |>
  #calculate if included in intersector pin with the threshold of 33.3% of intersector ds
mutate(intersector_pin = ifelse(intersector_ds > 0.333, 1, 0))
  # ggplot of intersector_ds to see distribution
  
ggplot(df_pin, aes(x = intersector_ds)) +
  geom_density(fill = "blue", alpha = 0.9) +
  labs(title = "Density Plot of Intersector IPM Distribution",
       x = "Intersector ds",
       y = "Density") +
  theme_minimal()
  
  



pin_intersector <- sum(df_pin$"intersector_pin") / nrow(df_pin)


### Step 3: Identify individuals with deprivations in each sector. Now, having the figure of the Intersectoral MPI, we can identify who is part of the MPI of each sector. This ensures that, although a person may have all the deprivations in a sector, they will only be part of the MPI if they have more than 33.3% of the total deprivations.


df_sectoral_pin <- df_pin |>
  select(-intersector_ds) |>
  #include in sectorial pins if included in intersectorial pin (intersector== 1) and there is a privation in the specific sector (sector_ds>0)
  mutate(across(
    c(
      "Food_security_ds",
      "Health_ds",
      "Humanitarian_transport_ds",
      "Integration_ds",
      "Nutrition_ds",
      "Protection_ds",
      "Child_protection_ds",
      "Gender_based_violence_ds",
      "HT_S_ds",
      "Shelter_ds",
      "Wash_ds",
      "Education_ds"
    ),
    ~ ifelse(intersector_pin == 1 & . > 0, 1, 0)
  )) |>
  #rename ds to pin
  rename_with( ~ str_replace(., "_ds$", "_pin"))

#final PIN table
pins_final <- data.frame(
  Sectors = c(
    "Food_security",
    "Health",
    "Humanitarian_transport",
    "Integration",
    "Nutrition",
    "Protection",
    "Child_protection",
    "Gender_based_violence",
    "Human_Traficking_&_Smuggling",
    "Shelter",
    "Wash",
    "Education",
    "Intersector"
  ),
  pins = round(colSums(df_pin_intersector) / nrow(df_pin_intersector) *
                 100, 1)
)

write_xlsx(pins_final, "rmna_pin2025.xlsx")
