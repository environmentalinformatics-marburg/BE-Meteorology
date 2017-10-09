# DESEASON
be_deseason_m <- function(data_m, ...){
  
  # Deseason monthly temperature data
  df_ta_mm <- aggregate(data_m$Ta_200, by = list(data_m$g_pm), 
                        FUN = mean, na.rm = TRUE)
  colnames(df_ta_mm) <- c("g_pm", "Ta_200_avgmm")
  data_m <- merge(data_m, df_ta_mm, by = "g_pm")
  data_m$Ta_200_mm_ds <- data_m$Ta_200 - data_m$Ta_200_avgmm
  
#   df_ta_max_mm <- aggregate(data_m$Ta_200_max, by = list(data_m$g_pm), 
#                         FUN = mean, na.rm = TRUE)
#   colnames(df_ta_max_mm) <- c("g_pm", "Ta_200_max_avgmm")
#   data_m <- merge(data_m, df_ta_max_mm, by = "g_pm")
#   data_m$Ta_200_max_mm_ds <- data_m$Ta_200_max - data_m$Ta_200_max_avgmm
#   
#   df_ta_min_mm <- aggregate(data_m$Ta_200_min, by = list(data_m$g_pm), 
#                             FUN = mean, na.rm = TRUE)
#   colnames(df_ta_min_mm) <- c("g_pm", "Ta_200_min_avgmm")
#   data_m <- merge(data_m, df_ta_min_mm, by = "g_pm")
#   data_m$Ta_200_min_mm_ds <- data_m$Ta_200_min - data_m$Ta_200_min_avgmm
  
  # Deseason monthly rainfall data
  df_p_mm <- aggregate(data_m$P_RT_NRT, by = list(data_m$g_pm), FUN = mean, 
                       na.rm=TRUE)
  colnames(df_p_mm) <- c("g_pm", "P_RT_NRT_avgmm")
  data_m <- merge(data_m, df_p_mm, by = "g_pm")
  data_m$P_RT_NRT_ms_ds <- data_m$P_RT_NRT - data_m$P_RT_NRT_avgmm
  return(data_m)
}


be_deseason_a <- function(data_a, ...){

  # Deseason annual temperature data
  df_ta_am <- aggregate(data_a$Ta_200, by = list(data_a$g_pa), 
                        FUN = mean, na.rm = TRUE)
  colnames(df_ta_am) <- c("g_pa", "Ta_200_avgam")
  data_a <- merge(data_a, df_ta_am, by = "g_pa")
  data_a$Ta_200_am_ds <- data_a$Ta_200 - data_a$Ta_200_avgam
  
  # Deseason annual rainfall data
  df_p_am <- aggregate(data_a$P_RT_NRT, by = list(data_a$g_pa), 
                       FUN = mean, na.rm=TRUE)
  colnames(df_p_am) <- c("g_pa", "P_RT_NRT_avgam")
  data_a <- merge(data_a, df_p_am, by = "g_pa")
  data_a$P_RT_NRT_as_ds <- data_a$P_RT_NRT - data_a$P_RT_NRT_avgam
  return(data_a)
}
