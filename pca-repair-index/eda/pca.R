# Information ==========================================================================================================

# São Paulo City Identification <> id_municipio = 3550308

# 1 Packages ===========================================================================================================

# vct_packages <- c("rgdal", "raster", "tmap", "maptools", "tidyverse", "broom", "knitr", "kableExtra", "RColorBrewer",
#                   "sp", "googleway", "ggmap", "revgeo", "bigrquery")

vct_packages <- c('tidyverse', 'psych', 'PerformanceAnalytics', 'rgdal', 'raster', 'reshape',
                  'broom', 'spdep')

if(sum(as.numeric(!vct_packages %in% installed.packages())) != 0){
  installer <- vct_packages[!vct_packages %in% installed.packages()]
  for(i in 1:length(installer)) {
    install.packages(installer, dependencies = T)
    break()}
  sapply(vct_packages, require, character = T) 
} else {
  sapply(vct_packages, require, character = T) 
}

# 2 DATA SOURCE ========================================================================================================

## 2.1 Repair Metrics --------------------------------------------------------------------------------------------------
df_repair_metrics <- read.csv(paste(getwd(), 'data/csv/repair-metrics-sao-paulo-city.csv', sep = '/'), 
                              na.strings = 'NA')

## 2.2 Census2010 Metrics ----------------------------------------------------------------------------------------------
df_census_metrics <- read.csv(paste(getwd(), 'data/csv/census2010-metrics-sao-paulo-city.csv', sep = '/'), 
                              na.strings = '')

## 2.3 Distrits --------------------------------------------------------------------------------------------------------
sf_distrit_raw <- readOGR(dsn = 'data/shapefiles/sp_distrits', layer = '35DSE250GC_SIR', encoding = 'latin1')

sf_distrit <- sf_distrit_raw[grepl('(3550308)([0-9]*)', sf_distrit_raw@data$CD_GEOCODD), ]

# 3 WRANGLING ==========================================================================================================

df_metrics <- df_census_metrics %>% left_join(df_repair_metrics, by = 'CD_GEOCODD')

# 4 PCA ================================================================================================================

## 4.1 Exploration ----------------------------------------------------------------------------------------------------- 

vct_remove_cols <- c('CD_GEOCODD', 'perc_res_14_p', 'perc_ppt_house')

#Summary
df_metrics %>% dplyr::select(-vct_remove_cols) %>% summary()


## 4.2 Correlation ---------------------------------------------------------------------------------------------------------

# Normalizing data 
df_zscore <- df_metrics %>% 
  # dplyr::select(-CD_GEOCODD, -perc_res_14_p, -perc_ppt_house, -perc_ppt_income_10_p, -ongarea_rate) %>% 
  dplyr::select(-vct_remove_cols) %>% 
  scale()

# Data 
rho_zscore <- df_zscore %>% corr.test(method = 'spearman')

# Heat Map Values
rho_zscore$r %>% melt() %>% 
  ggplot(aes(x = X1, y = X2)) + 
  geom_tile(aes(fill = value), colour = '#666666', alpha = 0.8) + 
  geom_text(aes(label = round(x = value, digits = 2))) +
  labs(title = 'Matriz de Correlação', 
       subtitle = 'Spearman', 
       x = NULL, 
       y = NULL) +
  scale_fill_gradient2(name = 'Scale', 
                       low = '#0000FF', 
                       mid = '#ffffff', 
                       high = '#ff9800', 
                       midpoint = 0) + 
  theme(plot.title = element_text(size = 15, face = 'bold'),
        axis.text.x = element_text(face = 'bold', angle = 90), 
        axis.text.y = element_text(face = 'bold'))

# Heat Map of Significance
rho_zscore$p %>% 
  melt() %>% 
  ggplot(aes(x = X1, y = X2)) + 
  geom_tile(aes(fill = value), colour = '#666666', alpha = 0.8) + 
  geom_text(aes(label = round(x = value, digits = 2))) +
  labs(title = 'Matrix de Significância', 
       subtitle = 'Spearman', 
       x = NULL, 
       y = NULL) +
  scale_fill_gradient2(name = 'Scale', 
                       low = '#0000FF', 
                       mid = '#ffffff', 
                       high = '#ff9800', 
                       midpoint = 0) + 
  theme(plot.title = element_text(size = 15, face = 'bold'),
        axis.text.x = element_text(face = 'bold', angle = 90), 
        axis.text.y = element_text(face = 'bold'))

# Summary 
chart.Correlation(df_zscore, histogram = TRUE, pch = '+')

## 4.3 Sphericity Test -------------------------------------------------------------------------------------------------
cortest.bartlett(df_zscore, n = NULL, diag = TRUE)

## 4.4 Principals ------------------------------------------------------------------------------------------------------
principal_raw <- principal(df_zscore, nfactors = ncol(df_zscore), rotate = 'none', scores = TRUE)

principal_raw$Vaccounted %>% as.data.frame() %>% slice(1:3)

eigen_value_raw <- principal_raw$values %>% round(4)
eigen_value_raw

# Applying Kaiser Criteria
pca_result_kaiser <- principal(df_zscore, nfactors = sum(eigen_value_raw > 1), rotate = 'none', scores = TRUE)

# Shared Variance
df_shared_variance <- pca_result_kaiser$Vaccounted %>% as.data.frame() %>% slice(1:3)
df_shared_variance

# Factorial Scores
df_fscore <- pca_result_kaiser$weights %>% as.data.frame()

df_fscore %>% 
  rownames_to_column('metric') %>% 
  pivot_longer(!metric, names_to = 'principal', values_to = 'factorial_score')%>% 
  ggplot(aes(x = metric, y = factorial_score, fill = metric)) +
  geom_bar(stat = 'identity') +
  facet_wrap(~principal) + 
  scale_fill_viridis_d() + 
  theme_bw() + 
  theme(legend.position = 'none',
        axis.text.x = element_text(size = 11, face = 'bold', angle = 90))

## 4.5 Communality -----------------------------------------------------------------------------------------------------
df_communality <- pca_result_kaiser$communality %>% 
  as.data.frame() %>% 
  rownames_to_column() %>% 
  dplyr::rename(metric = 1, communality = 2) 

df_communality %>% 
  ggplot(aes(x = communality, y = fct_reorder(metric, communality))) +
  geom_col(fill = '#3388FF', color = '#000000', alpha = 0.5) +
  geom_text(aes(label = paste(round(communality * 100, 2), '%', sep = '')), hjust = + 1.1) + 
  labs(title = 'Communality', x = NULL, y = NULL) +
  theme(panel.background = element_blank(), 
        plot.title = element_text(face = 'bold', size = 15, hjust = 0.05), 
        axis.text.x = element_blank(),
        axis.ticks.x = element_blank(),
        axis.text.y = element_text(face = 'bold'))

## 4.6 Weighted Factorial Scores -------------------------------------------------------------------------------------------
factorial_score_matrix <- df_fscore %>% as.matrix()
shared_variance_matrix <- df_shared_variance[2, ] %>% t() %>% as.matrix()

df_weighted_fscore <- (factorial_score_matrix %*% shared_variance_matrix) %>% 
  as.data.frame() %>% 
  rownames_to_column() %>% 
  dplyr::rename(metric = 1, weigthed_factorial_score = 2) 

df_weighted_fscore %>% 
  mutate(ggplot_fill = ifelse(weigthed_factorial_score < 0, 'neg', 'pos')) %>% 
  ggplot(aes(x = weigthed_factorial_score, 
             y = fct_reorder(metric, weigthed_factorial_score),
             fill = ggplot_fill)) +
  geom_col(color = '#000000', alpha = 0.5) +
  geom_text(aes(label = round(weigthed_factorial_score, 4)),
            position = position_stack(vjust = 0.5),
            size = 4) +
  scale_fill_manual(values = c('#ff9800', '#0000FF')) +
  labs(title = 'Wheighted Factorial Scores', x = NULL, y = NULL) +
  theme(panel.background = element_blank(), 
        plot.title = element_text(face = 'bold', size = 15, hjust = 0.25), 
        axis.text.x = element_blank(),
        axis.ticks.x = element_blank(),
        axis.text.y = element_text(face = 'bold'), 
        legend.position = 'none')

# 5 Results ============================================================================================================

df_rank_scores <- (df_zscore %>% as.matrix()) %*% (df_weighted_fscore %>% dplyr::select(2) %>% as.matrix()) %>% 
  as.data.frame()

df_rank <- df_metrics %>% dplyr::select(CD_GEOCODD) %>% cbind(df_rank_scores)

sf_distrits_ranked <- merge(x = sf_distrit, y = df_rank, by = 'CD_GEOCODD')

sf_distrits_ranked@data

df_georef <- tidy(sf_distrits_ranked, region = 'CD_GEOCODD') %>% 
  dplyr::rename(CD_GEOCODD = 7) %>% 
  left_join(sf_distrits_ranked@data, by = 'CD_GEOCODD')


centroid <- aggregate(cbind(long, lat) ~ NM_DISTRIT, df_georef, FUN = mean)
df_georef %>% 
  ggplot() + 
  geom_polygon(aes(x = long, y = lat, group = group, fill = weigthed_factorial_score),colour = '#666666',alpha = 0.7) +
  geom_text(data = centroid, aes(x = long, y = lat, label = NM_DISTRIT), size = 2.5) + 
  scale_fill_viridis_c() +
  theme_bw()

df_georef %>% 
  dplyr::select(NM_DISTRIT, weigthed_factorial_score) %>% 
  distinct() %>% 
  arrange(weigthed_factorial_score %>% desc()) 
