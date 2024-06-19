###Indicators calculation

main_merged <- main_merged %>%
  
  # food security
  mutate(
    FCS = (
      FS_D1_Q1 * 2 + FS_D1_Q5 * 1 +
        FS_D1_Q8 * 1 + FS_D1_Q4 * 4 +
        FS_D1_Q3 * 4 + FS_D1_Q2 * 3 +
        FS_D1_Q9 * .5 + FS_D1_Q10 * .5
    )
  ) %>%
  mutate(FCG = case_when(FCS <= 28.0 ~ 1, FCS >= 28.5 &
                           FCS <= 42.0 ~ 2, FCS >= 42.5 ~ 3)) %>%
  mutate(FCS_4pt = case_when(FCG == 1 ~ 4, FCG == 2 ~ 3, FCG == 3 ~ 1)) %>%
  mutate(FS_D1 = FCS_4pt) %>%
  mutate(R_CSI = (FS_D2_Q1 * 1 + FS_D2_Q2 * 2 +
                    FS_D2_Q3 * 1 + FS_D2_Q4 * 3 +
                    FS_D2_Q5 * 1)) %>%
  mutate(r_CSI_categories = case_when(R_CSI <= 4 ~ 1, R_CSI > 4 &
                                        R_CSI <= 18 ~ 2, ... =
                                        R_CSI >= 19 ~ 3))%>%
  mutate(
    FCS_rCSI = ifelse(FCS_4pt %in% c(1, 3, 4), FCS_4pt, NA),
    FCS_rCSI = ifelse(FCS_4pt == 1 &
                        R_CSI > 4, 2, FCS_rCSI)
  ) %>%
  mutate(FS_D2 = FCS_rCSI) %>%
  mutate(
    FES = FS_D3_Q1 / (FS_D3_Q1 + FS_D3_Q2),
    Foodexp_4pt = case_when(
      FES <= .4999999 ~ 1,
      FES >= .5 & FES <= .64999999 ~ 2,
      FES >= .65 & FES <= .74999999 ~ 3,
      FES >= .75 ~ 4
    )
  ) %>%
  mutate(FS_D3 = Foodexp_4pt) %>%
  mutate(
    stress_coping = ifelse(
      FS_D4_Q1 == "already_used_12" |
        FS_D4_Q1 == "yes" |
        FS_D4_Q2 == "already_used_12" |
        FS_D4_Q2 == "yes" |
        FS_D4_Q3 == "already_used_12" |
        FS_D4_Q3 == "yes",
      1,
      0
    )
  ) %>%
  mutate(
    crisis_coping = ifelse(
      FS_D4_Q4 == "already_used_12" |
        FS_D4_Q4 == "yes" |
        FS_D4_Q5 == "already_used_12" |
        FS_D4_Q5 == "yes" |
        FS_D4_Q6 == "already_used_12" |
        FS_D4_Q6 == "yes" |
        FS_D4_Q8 == "already_used_12" |
        FS_D4_Q8 == "yes",
      1,
      0
    )
  ) %>%
  mutate(
    emergency_coping = ifelse(
      FS_D4_Q7 == "already_used_12" |
        FS_D4_Q7 == "yes" |
        FS_D4_Q9 == "already_used_12" |
        FS_D4_Q9 == "yes" |
        FS_D4_Q10 == "already_used_12" |
        FS_D4_Q10 == "yes",
      1,
      0
    )
  ) %>%
  mutate(
    stress_coping = recode(stress_coping, "0" = 0, "1" = 2),
    crisis_coping = recode(crisis_coping, "0" = 0, "1" = 3),
    emergency_coping = recode(emergency_coping, "0" = 0, "1" = 4),
    Max_coping_behaviour = pmax(stress_coping, crisis_coping, emergency_coping),
    Max_coping_behaviour = recode(Max_coping_behaviour, "0" = 1)
  ) %>%
  mutate(FS_D4 = Max_coping_behaviour) %>%
  mutate(
    Mean_coping_capacity_FES = rowMeans(across(c(
      Max_coping_behaviour, Foodexp_4pt
    )), na.rm = TRUE),
    
    CARI_unrounded_FES = rowMeans(across(c(
      FCS_rCSI, Mean_coping_capacity_FES
    )), na.rm = TRUE),
    
    CARI_FES = round(CARI_unrounded_FES)
  ) |>
  mutate(FS_CARI = ifelse(CARI_FES > 2, 1, 0)) %>%
  
  # health
  group_by(id_hogar)%>%
  mutate(HE_D1 = ifelse(any(HE_D1_Q1 == "yes" &
                          HE_D1_Q2 == "no",na.rm = TRUE), 1, 0),
         HE_D4 = ifelse(any(INT_D2_Q2 == "no", na.rm = TRUE), 1,0)) %>%
  
  # humanitarian transportation
  mutate(HT_D1 = ifelse(
    (HT_D1_Q1_walking == 1 |
       HT_D1_Q1_bike == 1) & HT_D1_Q2 == "30m-plus",
    1,
    0
  )) %>%
  
  # integration

  group_by(id_hogar)%>%
  mutate(
    INT_D1 = ifelse(any(INT_D1_Q1 == "finding_work", na.rm = TRUE), 1, 0),
    INT_D2 = ifelse(any(INT_D2_Q1 == "no" |INT_D2_Q2 == "no" | INT_D2_Q3 == "no", na.rm = TRUE), 1, 0))%>%
  ungroup()%>%
  mutate(
    INT_D3 = ifelse(
      INT_D3_Q1_nationality == 1,
      1,
      0
    ),
    INT_D4 = ifelse(INT_D4_Q1_none == 1, 1, 0)
  )%>%

  
  # nutrition
  
  group_by(id_hogar) %>%
  mutate(
    NUT_D1 = ifelse(any(
      (NUT_D1_Q1 == "pregnant" | NUT_D1_Q1 == "breastfeeding") &
        (NUT_D1_Q2_nutritional_evaluation == 0 |
           NUT_D1_Q2_nutritional_counceling == 0 |
           NUT_D1_Q2_micronutrient_delivery == 0), na.rm = TRUE), 1, 0),
    NUT_D4 = ifelse(any(
      NUT_D4_Q1_nutritional_evaluation == 0 |
        NUT_D4_Q1_lactation_counseling == 0 |
        NUT_D4_Q1_non_lactation_counseling == 0, na.rm = TRUE), 1, 0),
    NUT_D5 = ifelse(any(NUT_D5_Q1 == "no" | NUT_D5_Q2 != "none", na.rm = TRUE), 1, 0),
    NUT_D8 = case_when(
      age2 < 24 ~ ifelse(any(
        NUT_D8_Q1_nutritional_assessment_weight_height_arm_measurement == 0 |
          NUT_D8_Q1_counseling_support_breastfeeding_evaluation_positions_difficulties == 0 |
          NUT_D8_Q1_counseling_support_non_breastfed_babies_formula_preparation_use_cleaning_feeding_utensils == 0 |
          NUT_D8_Q1_counseling_trained_personnel_feeding_solid_foods_diversity_preparation_feeding_children == 0 |
          NUT_D8_Q1_delivery_vitamin_mineral_supplements_iron_vitamin_a_powder_drops_syrups == 0, na.rm = TRUE ), 1, 0),
      age2 > 23 ~ ifelse(any(
        NUT_D8_Q1_nutritional_assessment_weight_height_arm_measurement == 0 |
          NUT_D8_Q1_counseling_trained_personnel_feeding_solid_foods_diversity_preparation_feeding_children == 0 |
          NUT_D8_Q1_delivery_vitamin_mineral_supplements_iron_vitamin_a_powder_drops_syrups == 0, na.rm = TRUE), 1, 0)),
    NUT_D10 = ifelse(any(
      sum(
        NUT_D10_Q1_breastmilk,
        NUT_D10_Q1_grains_roots,
        NUT_D10_Q1_legumes,
        NUT_D10_Q1_lacteo_products,
        NUT_D10_Q1_animal_protein,
        NUT_D10_Q1_eggs,
        NUT_D10_Q1_dark_leaf_vegetables,
        NUT_D10_Q1_other_vegetables,
        NUT_D10_Q1_other_fruits) < 5, na.rm = TRUE), 1, 0)
  ) %>%
  ungroup()%>%
  
  # protection
  mutate(
    PRO_D1 = ifelse(PROT_D1_Q1 != "none", 1, 0),
    PRO_D2	= ifelse(PROT_D2_Q1 != "none", 1, 0),
    PRO_D3 = ifelse(PROT_D3_Q1 == "yes" & PROT_D3_Q3 == "no", 1, 0),
    PRO_D4 = ifelse(MH_6_no_valid_document == 1, 1, 0),
    PRO_D5 = ifelse((PROT_D5_Q1 == "yes" |
                       PROT_D5_Q1 == "prefer_not_answer") & (
                         PROT_D5_Q2_threat == 1 |
                           PROT_D5_Q2_extortion == 1 |
                           PROT_D5_Q2_general_violence == 1 |
                           PROT_D5_Q2_insecurity == 1 |
                           PROT_D5_Q2_afraid_armed_group == 1 |
                           PROT_D5_Q2_persecuted_assaulted_discrim == 1 |
                           PROT_D5_Q2_recruited == 1
                       ) ,
                    1,
                    0
    )
  ) %>%
  
  # child protection
  mutate(CP_D1 = ifelse(
    (
      CP_D1_Q1 != "none" |
        CP_D1_Q1 != "dont_know" | CP_D1_Q1 != "prefer_not_answer"
    ) &
      CP_D1_Q2 == "no",
    1,
    0
  )) %>%
  
  # gender based violence
  group_by(id_hogar) %>%
  mutate(
    GBV_D1 = ifelse(any(GBV_D1_Q1 == "yes",na.rm = TRUE), 1, 0),
    GBV_D3 = ifelse(any(GBV_D3_Q1 == 4 | GBV_D3_Q1 == 5,na.rm = TRUE), 1, 0)
  ) %>%
  
  # T&T
  mutate(
    HTS_D1 = ifelse(HTS_D1_Q1 == "yes" | HTS_D1_Q2 == "yes", 1, 0),
    HTS_D2 = ifelse(HTS_D2_Q1 != "none" |
                      HTS_D2_Q1 != "not_applicable", 1, 0)
  )%>%
  
  # shelter
  mutate(
    SHE_D1 = ifelse(
      SHE_D1_Q1 == "street" |
        SHE_D1_Q1 == "shared_house" |
        SHE_D1_Q1 == "foster_family" |
        SHE_D1_Q1 == "rooming_house" |
        SHE_D1_Q1 == "hotel" |
        SHE_D1_Q1 == "shelters" |
        SHE_D1_Q1 == "colective" |
        SHE_D1_Q1 == "spontaneous" |
        SHE_D1_Q1 == "prefer_not_answer" |
        SHE_D1_Q2_house_unprotected_insecure == 1 |
        SHE_D1_Q2_neighboor_unprotected_insecure == 1 |
        SHE_D1_Q2_privacy == 1 |
        SHE_D1_Q2_natural_disasters == 1 |
        SHE_D1_Q2_wash_issues == 1 |
        SHE_D1_Q3_sewerage == 1 |
        SHE_D1_Q3_gas == 1 |
        SHE_D1_Q3_garbage_collection == 1 |
        SHE_D1_Q3_electricity == 1 |
        SHE_D1_Q3_acueduct == 1,
      1,
      0
    ),
    SHE_D2 = ifelse(MH_1 / SHE_D2_Q1 > 3 , 1, 0),
    SHE_D3 = ifelse(SHE_D3_Q1_none == 0, 1, 0),
    SHE_D4 = ifelse(
      SHE_D4_Q1 == "some_eviction_risk" |
        SHE_D4_Q1 == "evicted" |
        SHE_D4_Q1 == "prefer_not_answer",
      1,
      0
    )
  ) %>%
  
  # wash
  mutate(
    WA_D1 = ifelse(
      (WASH_D1_Q1 == "rain_water" |
          WASH_D1_Q1 == "bottled_water" |
          WASH_D1_Q1 == "water_supply" |
          WASH_D1_Q1 == "water_kiosk" |
          WASH_D1_Q1 == "unprotected_well_spring" |
          WASH_D1_Q1 == "no_pump_well" |
          WASH_D1_Q1 == "surface_water" |
          WASH_D1_Q1 == "none") &
        WASH_D1_Q2 == "30min_high",1,0),
    WA_D2 = ifelse(
      WASH_D2_Q1 == 1 |
        (WASH_D2_Q2 != "yes" | WASH_D2_Q2 != "not_aplicable"),
      1,
      0
    ),
    WA_D4 = ifelse(
      (
        WASH_D4_Q1 == "improvised_latrine" |
          WASH_D4_Q1 == "hanging_latrine" |
          WASH_D4_Q1 == "no_sanitation_service_free_access" |
          WASH_D4_Q1 == "no_sanitation_service_pay_access" |
          WASH_D4_Q1 == "open_defecation"
      ) |
        WASH_D4_Q2 == "yes",
      1,
      0
    ),
    WA_D6 = ifelse(
      WASH_D6_Q1 == "river_stream" |
        WASH_D6_Q1 == "patio_lot" |
        WASH_D6_Q1 == "burnt" |
        WASH_D6_Q1 == "dont_know",
      1,
      0
    ),
    WA_D8 = ifelse(
      WASH_D8_Q1 == "water" |
        WASH_D8_Q1 == "soap" |
        WASH_D8_Q1 == "cant_handwash" |
        WASH_D8_Q1 == "antibacterial_gel" ,
      1,
      0
    ))%>%
    group_by(id_hogar) %>%
      mutate(
    WA_D11 = ifelse(any(
      WASH_D11_Q1 == "no" |
        WASH_D11_Q1 == "panty_liners" |
        WASH_D11_Q1 == "cloth_fabric" |
        WASH_D11_Q1 == "toilet_paper" |
        WASH_D11_Q1 == "underwear_layers" |
        WASH_D11_Q1 == "not_applicable" |
        WASH_D11_Q1 == "other", na.rm = TRUE),
      1,
      0
    )
  ) %>%
  
  # education
    
  group_by(id_hogar) %>%
  mutate(
    EDU_D1 = ifelse(any(EDU_D1_Q1 == "no", na.rm = TRUE), 1, 0),
    EDU_D2 = ifelse(any(EDU_D2_Q1 == "no" &
                          EDU_D2_Q2 != "in_hh_parents", na.rm = TRUE), 1, 0),
    EDU_D3 = ifelse(any(EDU_D3_Q1 < 5, na.rm = TRUE), 1, 0)
  ) %>%
  ungroup()



write_xlsx(main_merged, "main_merged.xlsx")
