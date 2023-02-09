# 1 - LIBRARIES ========================================================================================================

lst_packages <- c('tidyverse', 'bigrquery', 'DBI', 'rgdal', 'raster','lodown', 'sf', 'lubridate', 
                  'spdep')

if(sum(as.numeric(!lst_packages %in% installed.packages())) != 0){
  installer <- lst_packages[!lst_packages %in% installed.packages()]
  for(i in 1:length(installer)) {
    install.packages(installer, dependencies = T)
    break()}
  sapply(lst_packages, require, character = T) 
} else {
  sapply(lst_packages, require, character = T) 
}

# 2 - GLOBAL VARIABLES =================================================================================================

min_date <- '2021-06-01' 

# 3 - DATA SOURCE ======================================================================================================

## 3.1 - Sao Paulo Polygons --------------------------------------------------------------------------------------------

sf_distrit <- readOGR(dsn = 'data/shapefiles/sp_distrits', layer = '35DSE250GC_SIR', encoding = 'latin1')

sf_distrit_sp <- sf_distrit[grepl('(3550308)([0-9]*)', sf_distrit@data$CD_GEOCODD), ]

sp_polygons <- st_transform(st_as_sf(sf_distrit_sp), 4326)

## 3.2 - Ongrepairs Points ---------------------------------------------------------------------------------------------

df_raw_ongrepairs <- read.csv(paste(getwd(), 'data/csv/lgpd-ongrepairs-raw.csv', sep = '/'), na.strings = '')

df_ongrepairs <- df_raw_ongrepairs %>% 
  filter(as.Date(dt_repair_requested) >= as.Date('2022-01-01'), 
         as.Date(dt_repair_requested) <= as.Date('2022-12-31')) %>% 
  mutate(flg_area = ifelse(repair_category_name %in% c('Área interna', 'Áreas externas'), 1, 0), 
         flg_eletric = ifelse(repair_category_name == 'Elétrico', 1, 0), 
         flg_hidraulic = ifelse(repair_category_name == 'Hidráulico', 1, 0),
         flg_gas = ifelse(repair_category_name == 'Gás', 1, 0))


sapply(df_raw_ongrepairs, function(x){sum(is.na(x))})
df_ongrepairs$dt_repair_requested %>% min()
df_ongrepairs$dt_repair_requested %>% max()
df_ongrepairs$repair_category_name %>% unique()

ongrepairs_points <- st_as_sf(df_ongrepairs, coords = c('long', 'lat'), crs = 4326)

df_ong <- st_join(ongrepairs_points, sp_polygons) %>% 
  as.data.frame() %>% 
  filter(!is.na(CD_GEOCODD)) %>% 
  group_by(CD_GEOCODD) %>% 
  summarise(n_requests = n_distinct(sk_request), 
            n_ongrepairs = sum(!is.na(sk_request)),
            n_area = sum(flg_area),
            n_eletric = sum(flg_eletric), 
            n_hidraulic = sum(flg_hidraulic),
            n_gas = sum(flg_gas))

## 3.3 - Offrepairs Points ---------------------------------------------------------------------------------------------

df_raw_offrepairs <- read.csv(paste(getwd(), 'data/csv/lgpd-offrepairs-raw.csv', sep = '/'), na.strings = '')

lapply(df_raw_offrepairs, function(x){sum(is.na(x))})

df_offrepairs <- df_raw_offrepairs %>% 
  filter(!is.na(repair_resolution), 
         as.Date(ts_created) >= as.Date('2022-01-01'),
         as.Date(ts_created) <= as.Date('2022-12-31')) %>% 
  mutate(flg_repair = ifelse(repair_resolution == 'NO_REPAIR_NEEDED', 1, 0)) %>% 
  dplyr::select(sk_contract, long, lat, flg_repair)

lapply(df_offrepairs, function(x){sum(is.na(x))})

offrepairs_points <- st_as_sf(df_offrepairs, coords = c('long', 'lat'), crs = 4326)

df_off <- st_join(offrepairs_points, sp_polygons) %>% 
  as.data.frame() %>% 
  filter(!is.na(CD_GEOCODD)) %>% 
  group_by(CD_GEOCODD) %>%
  summarise(n_terminations = sum(!is.na(sk_contract)), 
            n_offrepairs = sum(flg_repair))

## 3.4 - Contracts Point -----------------------------------------------------------------------------------------------

df_raw_contracts <- read.csv(paste(getwd(), 'data/csv/lgpd-contracts-raw.csv', sep = '/'), na.strings = '')

lapply(df_raw_contracts, function(x){sum(is.na(x))})

df_contracts <- df_raw_contracts %>% 
  mutate(dt_annulment = coalesce(dt_annulment, as.character(Sys.Date())), 
         age_months = interval(dt_entrance, dt_annulment) %/% months(1)) %>% 
  filter(!status %in% c('Cancelado', 'PreAssinaturas', 'Minuta', ''),
         as.Date(dt_entrance) <= as.Date(dt_annulment),
         coalesce(as.Date(dt_annulment), Sys.Date()) > as.Date('2022-01-01'),
         as.Date(dt_entrance) <= as.Date('2022-12-31'))

lapply(df_contracts, function(x){sum(is.na(x))})

contracts_points <- st_as_sf(df_contracts, coords = c('long', 'lat'), crs = 4326)

df_cont <- st_join(contracts_points, sp_polygons) %>% 
  as.data.frame() %>% 
  filter(!is.na(CD_GEOCODD)) %>% 
  group_by(CD_GEOCODD) %>% 
  summarise(avg_age_months = mean(age_months),
            n_contracts = n_distinct(sk_contract), 
            n_houses = n_distinct(sk_house))

df_cont


## 3.5 - Neighbourhood -------------------------------------------------------------------------------------------------

# Getting Neighbour
neighbour_queen <- poly2nb(sf_distrit_sp, queen = TRUE, row.names = sf_distrit_sp@data$CD_GEOCODD)

plot(sf_distrit_sp, border = ' lightgray')
plot(neighbour_queen, coordinates(sf_distrit_sp), add=TRUE, col = '#33638DFF')

matrix_neighbour_queen <- nb2mat(neighbours = neighbour_queen, style = 'B', zero.policy = TRUE)
colnames(matrix_neighbour_queen) <- sf_distrit_sp@data$CD_GEOCODD

df_neighbours <- matrix_neighbour_queen %>% 
  as.data.frame() %>% 
  rownames_to_column('CD_GEOCODD') %>% 
  pivot_longer(cols = starts_with('3550308'), names_to = 'CD_GEOCODD_NB', values_to = 'flg_neighbour') %>% 
  filter(flg_neighbour == 1)

## 3.6 - All Joined  ---------------------------------------------------------------------------------------------------

df_metrics0 <- sf_distrit_sp@data %>% 
  left_join(df_cont, by = 'CD_GEOCODD') %>% 
  left_join(df_off, by = 'CD_GEOCODD') %>% 
  left_join(df_ong, by = 'CD_GEOCODD') %>% 
  mutate(offrepairs_rate = round(n_offrepairs / n_terminations, 4), 
         ongrepairs_rate = round(n_ongrepairs / n_houses, 4), 
         ongarea_rate = round(n_area / n_ongrepairs, 4),
         ongeletric_rate = round(n_eletric / n_ongrepairs, 4),
         onghidraulic_rate = round(n_hidraulic / n_ongrepairs, 4),
         onggas_rate = round(n_gas / n_ongrepairs, 4)) %>% 
  dplyr::select(CD_GEOCODD:avg_age_months, offrepairs_rate:onggas_rate)
  
df_metrics_nb <- df_neighbours %>% 
  left_join(df_metrics0, by = c('CD_GEOCODD_NB' = 'CD_GEOCODD')) %>% 
  group_by(CD_GEOCODD) %>% 
  summarise(avg_age_months_nb = round(mean(avg_age_months, na.rm = TRUE), 4),
            offrepairs_rate_nb = round(mean(offrepairs_rate, na.rm = TRUE), 4), 
            ongrepairs_rate_nb = round(mean(ongrepairs_rate, na.rm = TRUE), 4), 
            ongarea_rate_nb = round(mean(ongarea_rate, na.rm = TRUE), 4),
            ongeletric_rate_nb = round(mean(ongeletric_rate, na.rm = TRUE), 4),
            onghidraulic_rate_nb = round(mean(onghidraulic_rate, na.rm = TRUE), 4),
            onggas_rate_nb = round(mean(onggas_rate, na.rm = TRUE), 4))


df_metrics <- df_metrics0 %>% 
  left_join(df_metrics_nb, by = 'CD_GEOCODD') %>% 
  mutate(avg_age_months = coalesce(avg_age_months,
                                   avg_age_months_nb,
                                   mean(df_metrics0$avg_age_months, na.rm = TRUE)),
         offrepairs_rate = coalesce(offrepairs_rate, 
                                    offrepairs_rate_nb, 
                                    mean(df_metrics0$avg_age_months, na.rm = TRUE)), 
         ongrepairs_rate = coalesce(ongrepairs_rate,
                                    ongrepairs_rate_nb,
                                    mean(df_metrics0$ongrepairs_rate, na.rm = TRUE)) , 
         ongarea_rate = coalesce(ongarea_rate,
                                 ongarea_rate_nb,
                                 mean(df_metrics0$ongarea_rate, na.rm = TRUE)),
         ongeletric_rate = coalesce(ongeletric_rate,
                                    ongeletric_rate_nb,
                                    mean(df_metrics0$ongeletric_rate, na.rm = TRUE)) ,
         onghidraulic_rate = coalesce(onghidraulic_rate,
                                      onghidraulic_rate_nb,
                                      mean(df_metrics0$onghidraulic_rate, na.rm = TRUE)),
         onggas_rate = coalesce(onggas_rate,
                                onggas_rate_nb,
                                mean(df_metrics0$onggas_rate, na.rm = TRUE))) %>% 
  dplyr::select(CD_GEOCODD:onggas_rate, -NM_DISTRIT)

sapply(df_metrics, function(x){sum(is.na(x))})

# 4 - SAVE -------------------------------------------------------------------------------------------------------------

df_metrics %>% write.csv(paste(getwd(), 'data/csv/repair-metrics-sao-paulo-city.csv', sep = '/'), row.names = F)

