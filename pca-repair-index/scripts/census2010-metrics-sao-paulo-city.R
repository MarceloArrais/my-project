# 1 - LIBRARIES ========================================================================================================

lst_packages <- c('tidyverse', 'dbplyr', 'bigrquery', 'DBI', 'rgdal', 'raster','lodown')

if(sum(as.numeric(!lst_packages %in% installed.packages())) != 0){
  installer <- lst_packages[!lst_packages %in% installed.packages()]
  for(i in 1:length(installer)) {
    install.packages(installer, dependencies = T)
    break()}
  sapply(lst_packages, require, character = T) 
} else {
  sapply(lst_packages, require, character = T) 
}


# 2 - DATA SOURCE ======================================================================================================

## 2.1 - Shapefiles ----------------------------------------------------------------------------------------------------

### 2.1.1 -  by sector -------------------------------------------------------------------------------------------------

sf_sector_raw <- readOGR(dsn = 'data/shapefiles/sp_census_sector', layer = '35SEE250GC_SIR', encoding = 'latin1')

df_sector <- sf_sector_raw@data %>% filter(CD_GEOCODM == '3550308') %>% dplyr::select(CD_GEOCODI, CD_GEOCODD)

df_sector

### 2.2.2 - by distrits ------------------------------------------------------------------------------------------------

sf_distrit_raw <- readOGR(dsn = 'data/shapefiles/sp_distrits', layer = '35DSE250GC_SIR', encoding = 'latin1')

df_distrit <- sf_distrit_raw[grepl('(3550308)([0-9]*)', sf_distrit_raw@data$CD_GEOCODD), ]@data %>% 
  dplyr::select(CD_GEOCODD, NM_DISTRIT)


## 2.2 - Census2010 ----------------------------------------------------------------------------------------------------
### 2.2.1 - from connection (Use this part only if you lost the census2010-sao-paulo-city-raw.csv file) ----------------
# 
# # connection Settings 
# str_bigquery_billing <- rstudioapi::askForPassword('Enter your bigquery project billing')
# con <- dbConnect(
#   bigrquery::bigquery(),
#   project = 'basedosdados',
#   dataset = 'br_ibge_censo_demografico',
#   billing = str_bigquery_billing
# )
# 
# # sql
# sql_census2010 <-
# "
#   SELECT
#     ger.id_setor_censitario id_sector
#     , ger.v002 qty_property_all
#     , ger.v003 qty_property_house
#     , ger.v005 qty_property_apartment
#     , ger.v008 qty_property_rented
#     , ger.v012 qty_property_water
#     , ger.v017 qty_property_swage
#     , ger.v035 qty_property_trash
#     , ger.v043 qty_property_electricity
#     , ger.v050 + ger.v051 qty_property_people_1_2
#     , ger.v052 + ger.v053 qty_property_people_3_4
#     , ger.v054 + ger.v055 + ger.v056 + ger.v057 + ger.v058 + ger.v059 qty_property_people_5_p
#     , ren.v014 + ren.v005 + ren.v006 + ren.v007 + ren.v008 qty_property_income_0_1
#     , ren.v009 + ren.v010 + ren.v011 qty_property_income_1_5
#     , ren.v012 qty_property_income_5_10
#     , ren.v013 qty_property_income_10_p
#     , mor.v002 qty_residents_all
#     , ida.v048 + ida.v049 + ida.v050 + ida.v051 + ida.v052 + ida.v053 + ida.v054 + ida.v055 + ida.v056 + ida.v057 +
#       ida.v058 + ida.v059 + ida.v060 + ida.v061 + ida.v062 + ida.v063 + ida.v064 + ida.v065 + ida.v066 + ida.v067 +
#       ida.v068 + ida.v069 + ida.v070 + ida.v071 + ida.v072 + ida.v073 + ida.v074 + ida.v075 + ida.v076 + ida.v077 +
#       ida.v078 + ida.v079 + ida.v080 + ida.v081 + ida.v082 + ida.v083 + ida.v084 + ida.v085 + ida.v086 + ida.v087 +
#       ida.v088 + ida.v089 + ida.v090 + ida.v091 + ida.v092 + ida.v093 + ida.v094 + ida.v095 + ida.v096 + ida.v097 +
#       ida.v098 + ida.v099 + ida.v100 + ida.v101 + ida.v102 + ida.v103 + ida.v104 + ida.v105 + ida.v106 + ida.v107 +
#       ida.v108 + ida.v109 + ida.v110 + ida.v111 + ida.v112 + ida.v113 + ida.v114 + ida.v115 + ida.v116 + ida.v117 +
#       ida.v118 + ida.v119 + ida.v120 + ida.v121 + ida.v122 + ida.v123 + ida.v124 + ida.v125 + ida.v126 + ida.v127 +
#       ida.v128 + ida.v129 + ida.v130 + ida.v131 + ida.v132 + ida.v133 + ida.v134 qty_residents_14_p
#     , alf.v011 + alf.v012 + alf.v013 + alf.v014 + alf.v015 + alf.v016 + alf.v017 + alf.v018 + alf.v019 + alf.v020 +
#       alf.v021 + alf.v022 + alf.v023 + alf.v024 + alf.v025 + alf.v026 + alf.v027 + alf.v028 + alf.v029 + alf.v030 +
#       alf.v031 + alf.v032 + alf.v033 + alf.v034 + alf.v035 + alf.v036 + alf.v037 + alf.v038 + alf.v039 + alf.v040 +
#       alf.v041 + alf.v042 + alf.v043 + alf.v044 + alf.v045 + alf.v046 + alf.v047 + alf.v048 + alf.v049 + alf.v050 +
#       alf.v051 + alf.v052 + alf.v053 + alf.v054 + alf.v055 + alf.v056 + alf.v057 + alf.v058 + alf.v059 + alf.v060 +
#       alf.v061 + alf.v062 + alf.v063 + alf.v064 + alf.v065 + alf.v066 + alf.v067 + alf.v068 + alf.v069 + alf.v070 +
#       alf.v071 + alf.v072 + alf.v073 + alf.v074 + alf.v075 + alf.v076 + alf.v077 qty_residents_14_p_literate
#   FROM
#     setor_censitario_domicilio_caracteristicas_gerais_2010 ger
#   LEFT JOIN 
#     basedosdados.br_ibge_censo_demografico.setor_censitario_domicilio_moradores_2010 mor 
#       ON ger.id_setor_censitario = mor.id_setor_censitario
#   LEFT JOIN 
#     basedosdados.br_ibge_censo_demografico.setor_censitario_domicilio_renda_2010 ren
#       ON ger.id_setor_censitario = ren.id_setor_censitario
#   LEFT JOIN 
#     basedosdados.br_ibge_censo_demografico.setor_censitario_idade_total_2010 ida
#       ON ger.id_setor_censitario = ida.id_setor_censitario 
#   LEFT JOIN 
#     basedosdados.br_ibge_censo_demografico.setor_censitario_alfabetizacao_total_2010 alf
#       ON ger.id_setor_censitario = alf.id_setor_censitario
#   WHERE
#     CAST(ger.id_setor_censitario AS STRING) LIKE '3550308%'
# "
# 
# df_raw_census2010 <- dbGetQuery(con, sql_census2010)
# 
# df_raw_census2010 %>% 
#   write.csv(paste(getwd(), 'data/source/raw/raw-sao-paulo-city-census2010.csv', sep = '/'), row.names = F)

### 2.2.2 - from csv file ----------------------------------------------------------------------------------------------

df_raw_census2010 <- read.csv(paste(getwd(), 'data/csv/census2010-sao-paulo-city-raw.csv', sep = '/')) %>% 
  dplyr::rename(CD_GEOCODI = 1) %>% 
  mutate(CD_GEOCODI = as.character(CD_GEOCODI))


# 3 - WRANGLING ========================================================================================================

df_metrics <- df_sector %>% 
  left_join(df_distrit, by = 'CD_GEOCODD') %>% 
  left_join(df_raw_census2010, by = 'CD_GEOCODI') %>%
  group_by(CD_GEOCODD) %>% 
  summarise(n_ppt_all = sum(qty_property_all, na.rm = T), 
            n_ppt_house = sum(qty_property_house, na.rm = T), 
            n_ppt_apartment = sum(qty_property_apartment, na.rm = T),
            n_ppt_rented = sum(qty_property_rented, na.rm = T), 
            n_ppt_water = sum(qty_property_water, na.rm = T), 
            n_ppt_swage = sum(qty_property_swage, na.rm = T),
            n_ppt_trash = sum(qty_property_trash, na.rm = T), 
            n_ppt_electricity = sum(qty_property_electricity, na.rm = T), 
            n_ppt_people_1_2 = sum(qty_property_people_1_2, na.rm = T),
            n_ppt_people_3_4 = sum(qty_property_people_3_4, na.rm = T),
            n_ppt_people_5_p = sum(qty_property_people_5_p, na.rm = T),
            n_ppt_income_0_1 = sum(qty_property_income_0_1, na.rm = T),
            n_ppt_income_1_5 = sum(qty_property_income_1_5, na.rm = T),
            n_ppt_income_5_10 = sum(qty_property_income_5_10, na.rm = T),
            n_ppt_income_10_p = sum(qty_property_income_10_p, na.rm = T),
            n_res_all = sum(qty_residents_all, na.rm = T), 
            n_res_14_p = sum(qty_residents_14_p, na.rm = T),
            n_res_14_p_literate = sum(qty_residents_14_p_literate, na.rm = T)) %>% 
  mutate(perc_ppt_house = round(n_ppt_house / n_ppt_all, 4), 
         perc_ppt_apartment = round(n_ppt_apartment / n_ppt_all, 4),
         perc_ppt_rented = round(n_ppt_rented / n_ppt_all, 4), 
         perc_ppt_water = round(n_ppt_water / n_ppt_all, 4),
         perc_ppt_swage = round(n_ppt_swage / n_ppt_all, 4),
         perc_ppt_trash = round(n_ppt_trash / n_ppt_all, 4),
         perc_ppt_electricity = round(n_ppt_electricity / n_ppt_all, 4), 
         perc_ppt_people_1_2 = round(n_ppt_people_1_2 / n_ppt_all, 4),
         perc_ppt_people_3_4 = round(n_ppt_people_3_4 / n_ppt_all, 4),
         perc_ppt_people_5_p = round(n_ppt_people_5_p / n_ppt_all, 4),
         perc_ppt_income_0_1 = round(n_ppt_income_0_1 / n_ppt_all, 4),
         perc_ppt_income_1_5 = round(n_ppt_income_1_5 / n_ppt_all, 4),
         perc_ppt_income_5_10 = round(n_ppt_income_5_10 / n_ppt_all, 4),
         perc_ppt_income_10_p = round(n_ppt_income_10_p / n_ppt_all, 4),
         perc_res_14_p = round(n_res_14_p / n_res_all, 4),
         perc_res_14_p_literate = round(n_res_14_p_literate / n_res_14_p, 4)) %>% 
  dplyr::select(CD_GEOCODD, perc_ppt_house:perc_res_14_p_literate)

# 4- SAVE --------------------------------------------------------------------------------------------------------------

df_metrics %>% 
  write.csv(paste(getwd(), 'data/csv/census2010-metrics-sao-paulo-city.csv', sep = '/'), row.names = F)



# Some information 
# Metadata 
## Sector Census 2010 <> basedosdados.org/dataset/br-ibge-censo-demografico?bdm_table=setor_censitario_basico_2010 
