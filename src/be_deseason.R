# DESEASON
be_deseason_m <- function(df_met, ...){
  
  # Deseason monthly temperature data
  df_ta_mm <- aggregate(df_met$Ta_200, by = list(df_met$g_pm), 
                        FUN = mean, na.rm = TRUE)
  colnames(df_ta_mm) <- c("g_pm", "Ta_200_mm")
  df_met <- merge(df_met, df_ta_mm, by = "g_pm")
  df_met$Ta_200_mm_ds <- df_met$Ta_200 - df_met$Ta_200_mm
  
  # Deseason monthly rainfall data
  df_p_mm <- aggregate(df_met$P_RT_NRT, by = list(df_met$g_pm), FUN = mean, 
                       na.rm=TRUE)
  colnames(df_p_mm) <- c("g_pm", "P_RT_NRT_mm")
  df_met <- merge(df_met, df_p_mm, by = "g_pm")
  df_met$P_RT_NRT_ms_ds <- df_met$P_RT_NRT - df_met$P_RT_NRT_mm
  return(df_met)
}


be_deseason_a <- function(df_met, ...){
  
  # Deseason annual temperature data
  df_ta_mm <- aggregate(df_met$Ta_200, by = list(df_met$g_pa), 
                        FUN = mean, na.rm = TRUE)
  colnames(df_ta_mm) <- c("g_pa", "Ta_200_mm")
  df_met <- merge(df_met, df_ta_mm, by = "g_pa")
  df_met$Ta_200_mm_ds <- df_met$Ta_200 - df_met$Ta_200_mm
  
  # Deseason annual rainfall data
  df_p_mm <- aggregate(df_met$P_RT_NRT, by = list(df_met$g_pa), 
                       FUN = mean, na.rm=TRUE)
  colnames(df_p_mm) <- c("g_pa", "P_RT_NRT_am")
  df_met <- merge(df_met, df_p_mm, by = "g_pa")
  df_met$P_RT_NRT_as_ds <- df_met$P_RT_NRT - df_met$P_RT_NRT_am
  return(df_met)
}
