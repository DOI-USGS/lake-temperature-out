#### Lindsay's code ####

# library(readr)
# library(dplyr)
# library(ggplot2)
# 
# scmake("3_summarize/out/rmse_pb0.csv")
# scmake("3_summarize/out/rmse_pgdl.csv")
# pb0_rmse <- read_csv("3_summarize/out/rmse_pb0.csv")
# pgdl_rmse <- read_csv("3_summarize/out/rmse_pgdl.csv")
# obs <- read_csv("2_process/out/combined_obs_toha.csv")
# 
# #### PB0 ####
# 
# pb0_rmse_reshape <- pb0_rmse %>% 
#   # Reshape to get one column of RMSE and then one column of the type
#   pivot_longer(cols = c(rmse_opti_hab, rmse_therm_hab, rmse_opti_therm_hab), 
#                names_to = "hab_type", names_pattern = "rmse_(.*)", values_to = "RMSE")
# 
# ggplot(pb0_rmse_reshape, aes(x = hab_type, y=RMSE)) + geom_boxplot()
# ggplot(pb0_rmse_reshape, aes(x = RMSE, y=..density..)) + geom_density() + facet_grid(. ~ hab_type)
# 
# # Plot obs with RMSE scatter plot
# pb0_scatter <- obs %>% 
#   select(site_id, date, opti_hab, therm_hab, opti_therm_hab) %>% 
#   left_join(pb0_rmse, by = c("site_id", "date")) %>% 
#   mutate(month = as.numeric(format(date, "%m")),
#          season = ifelse(month %in% 4:5, "Spring", 
#                          ifelse(month %in% 6:8, "Summer",
#                                 ifelse(month %in% 9:10, "Fall", "Winter"))))
# 
# ggplot(pb0_scatter, aes(x = opti_hab, y = rmse_opti_hab)) + geom_point() 
# ggplot(pb0_scatter, aes(x = therm_hab, y = rmse_therm_hab)) + geom_point()
# ggplot(pb0_scatter, aes(x = opti_therm_hab, y = rmse_opti_therm_hab)) + geom_point()
# 
# # Now by season
# ggplot(pb0_scatter, aes(x = opti_hab, y = rmse_opti_hab)) + geom_point() + facet_grid(season ~ .)
# ggplot(pb0_scatter, aes(x = therm_hab, y = rmse_therm_hab)) + geom_point() + facet_grid(season ~ .)
# ggplot(pb0_scatter, aes(x = opti_therm_hab, y = rmse_opti_therm_hab)) + geom_point() + facet_grid(season ~ .)

#### Alison's code ####

library(tidyverse)

read_obs <- function(zipfile) {
  unzipped_obs_file <- unzip(zipfile = zipfile, overwrite = TRUE, exdir = tempdir())
  readr::read_csv(unzipped_obs_file, col_types = cols()) %>%
    mutate(model='obs') %>%
    select(site_id, date, depth, temp, model)
}
read_preds <- function(zipfile) {
  unzipped_obs_file <- unzip(zipfile = zipfile, overwrite = TRUE, exdir = tempdir())
  readr::read_csv(unzipped_obs_file, col_types = cols())  %>%
    mutate(model=str_match(string=zipfile, pattern='/([[:alnum:]]+)_matched')[1,2]) %>%
    select(site_id, date, depth, temp=pred, model)
}
predobs <- bind_rows(
  read_obs("1_fetch/out/temperature_observations.zip"),
  read_preds("1_fetch/out/pb0_matched_to_observations.zip"),
  read_preds("1_fetch/out/pgdl_matched_to_observations.zip"))
# scmake('1_fetch/out/lake_metadata.csv')
lake_metadata <- read_csv('1_fetch/out/lake_metadata.csv', col_types=cols())
obs_dates <- predobs %>%
  filter(model == 'obs') %>%
  left_join(select(lake_metadata, site_id, lake_depth=depth), by='site_id') %>%
  group_by(site_id, date) %>%
  # Jordan suggested density_0_5 = 1, density_5_10 = 0.5, density_gt10 = 0.25
  summarize(
    num_obs = length(depth),
    lake_depth = lake_depth[1],
    density_tot = length(depth) / lake_depth,
    density_0_5 = length(which(depth < 5)) / min(5, lake_depth),
    density_5_10 = length(which(depth >= 5 & depth < 10)) / (min(10, lake_depth) - 5),
    density_gt10 = length(which(depth >= 10)) / (lake_depth - 10)
  ) %>%
  mutate(
    min_obs = num_obs > 1,
    hi_obs =
      num_obs > 1 &
      density_0_5 >= 1 &
      (lake_depth < 6 | density_5_10 >= 0.5) &
      (lake_depth < 11 | density_gt10 >= 0.25))

# >  obs_dates %>% ungroup %>% select(min_obs, hi_obs) %>% summarize(min_obs=sum(min_obs), hi_obs=sum(hi_obs))
# # A tibble: 1 x 2
# min_obs hi_obs
# <int>  <int>
#   1   64451  37924

read_toha <- function(filename) {
  read_csv(filename, col_types = cols()) %>% 
    tidyr::pivot_longer(cols=ends_with('hab'), names_to='habitat', values_to='area') %>%
    mutate(model = str_match(string=filename, pattern='combined_([[:alnum:]]+)_')[1,2])
}
all_zs_df <- purrr::map_df(
  c("2_process/out/combined_obs_toha.csv",
    "2_process/out/combined_pb0_matched2obs_toha.csv",
    "2_process/out/combined_pgdl_matched2obs_toha.csv"),
  read_toha) %>%
  mutate(area = area / 1e6) # convert from m2 to km2
all_ha_df <- all_zs_df %>% 
  select(site_id, date, habitat, area, model) %>%
  tidyr::pivot_wider(names_from='model', values_from='area') %>%
  filter(!is.na(pb0) | !is.na(pgdl)) %>%
  mutate(err_pb0 = pb0 - obs, err_pgdl = pgdl - obs)
# pick out some highly observed sites for exploring patterns anecdotally
hi_obs_sites <- all_ha_df %>% filter(!is.na(pgdl)) %>% group_by(site_id) %>% tally %>% arrange(desc(n)) %>% pull(site_id)

# check n's
filter(all_ha_df, habitat=='opti_hab', err_pb0 != 0 | err_pgdl != 0) # optical habitat shouldn't differ among obs/models

# now ignore optical habitat errors
tha_df <- filter(all_ha_df, habitat=='therm_hab') %>%
  left_join(select(obs_dates, site_id, date, num_obs, hi_obs), by=c('site_id','date'))

tha_anecdotes <- filter(tha_df, site_id %in% hi_obs_sites[1:16])
tha_anecdote_maxes <- tha_anecdotes %>%
  group_by(site_id) %>%
  summarize(max_ha = max(c(obs, pb0, pgdl), na.rm=TRUE))
tha_anecdotes %>%
  filter(hi_obs) %>%
  pivot_longer(cols=c('pb0','pgdl'), names_to='model', values_to='pred') %>%
  ggplot(aes(x=obs, y=pred, color=model, shape=model)) +
  geom_point(alpha=0.5) +
  geom_point(data=tha_anecdote_maxes, aes(x=max_ha, y=max_ha), color='black', shape=3) +
  scale_color_manual('Model', values=c(pb0='orange1', pgdl='orange4')) +
  scale_shape_discrete('Model') +
  facet_wrap(~site_id, scales='free') +
  theme_minimal() +
  theme(legend.position='bottom') +
  xlab('Observations') + ylab('Predictions') +
  ggtitle('Thermal habitat examples (highly observed sites, highly observed dates), Pred vs Obs')
  # ggtitle('Thermal habitat examples (highly observed sites), Pred vs Obs')
ggsave('3_summarize/out/eg_tha_predobs_hiobs.png', width=10, height=10)
# ggsave('3_summarize/out/eg_tha_predobs.png', width=10, height=10)

library(lubridate)
anecdote_date <- tibble(site_id = 'nhdhr_120018114', date = ymd('2010-08-25'))
tha_one_anecdote <- tha_anecdotes %>%
  filter(site_id == 'nhdhr_120018114',
         between(date, ymd('2008-01-01'), ymd('2010-12-31'))) %>%
  pivot_longer(cols=c('obs','pb0','pgdl'), names_to='model', values_to='THA') %>%
  mutate(THA = 100*THA/max(THA)) # assume max = 100%
g <- tha_one_anecdote %>% 
  ggplot(aes(x=date, y=THA, color=model)) +
  scale_color_manual('', values=c(obs='gray', pb0='#1b9e77', pgdl='#7570b3'), 
                     labels=c(obs='Observed', pb0='CPB', pgdl='PGDL')) +
  geom_point() +
  xlab(NULL) +
  ylab('Thermal habitat area (%)') +
  theme_minimal() +
  theme(legend.position='bottom')
g
ggsave('3_summarize/out/tha_one_anecdote.png', width=5, height=3)
g + geom_point(data=filter(tha_one_anecdote, date == anecdote_date$date), color='red', shape=21, size=3)
ggsave('3_summarize/out/tha_one_anecdote_circled.png', width=5, height=3)
  

anecdote_date %>%
  left_join(predobs, by=c('site_id', 'date')) %>%
  ggplot(aes(color=model, shape=model)) +
  # annotate('rect', xmin=5, xmax=11, ymin=-Inf, ymax=Inf, fill='#0000ff', alpha=0.1, color=NA) +
  annotate('rect', xmin=11, xmax=25, ymin=-Inf, ymax=Inf, fill='#fc0000', alpha=0.1, color=NA) +
  geom_point(aes(x=temp, y=depth)) + geom_line(aes(x=temp, y=depth), orientation = 'y') +
  xlab('Temperature (C)') + ylab('Depth (m)') +
  coord_cartesian(xlim=c(9,27)) +
  scale_y_reverse() +
  scale_shape_discrete('', labels=c(obs='Observed', pb0='CPB', pgdl='PGDL'), guide='none') +
  scale_color_manual('', values=c(obs='gray', pb0='#1b9e77', pgdl='#7570b3'), 
                     labels=c(obs='Observed', pb0='CPB', pgdl='PGDL'), guide='none') +
  theme_minimal()
ggsave('3_summarize/out/tha_one_anecdote_profile.png', width=4, height=3)

ggplot(tha_anecdotes,
       aes(x=pb0, y=pgdl)) +
  geom_point(color='orange2', alpha=0.5) +
  geom_point(data=tha_anecdote_maxes, aes(x=max_ha, y=max_ha), color='black', shape=3) +
  facet_wrap(~site_id, scales='free') +
  theme_minimal() +
  theme(legend.position='bottom') +
  xlab('PB0') + ylab('PGDL') +
  ggtitle('Thermal habitat examples (highly observed sites), PGDL vs PB0')
ggsave('3_summarize/out/eg_tha_pb0pgdl.png', width=10, height=10)


plot_anecdote_dates <- function(anecdote_dates) {
  tha_anecdote_zs <- anecdote_dates %>%
    left_join(all_zs_df, by=c('site_id','date')) %>%
    filter(!is.na(area)) %>%
    filter(habitat == 'therm_hab') %>%
    select(-starts_with('opti'), -habitat) %>%
    crossing(ribbon_x=c(0,1)) %>%
    mutate(ribbon_x=ribbon_x+as.integer(as.factor(model)))
  anecdote_dates %>%
    left_join(predobs, by=c('site_id', 'date')) %>%
    ggplot(aes(color=model, shape=model)) +
    geom_vline(aes(xintercept=11), color='gray') + geom_vline(aes(xintercept=25), color='gray') +
    geom_point(aes(x=temp, y=depth)) + geom_line(aes(x=temp, y=depth), orientation = 'y') +
    geom_ribbon(data=tha_anecdote_zs, aes(x=ribbon_x, color=model, ymax=therm_Z1, ymin=therm_Z2, fill=model), color=NA, alpha=0.2) +
    scale_y_reverse() +
    facet_wrap(~site_id+date, scales='free') +
    theme_minimal() +
    theme(legend.position='bottom')
}
plot_anecdote_dates(
  tha_anecdotes %>% left_join(tha_anecdote_maxes, by='site_id') %>%
    filter(pgdl == 0 & pb0 == max_ha) %>% select(site_id, date) %>%
    group_by(site_id) %>% slice(1) %>% ungroup()) +
  ggtitle('pgdl == 0 & pb0 == max_ha')
ggsave('3_summarize/out/eg_tha_anecdates_pglo_pbhi.png', width=10, height=9)
plot_anecdote_dates(
  tha_anecdotes %>% left_join(tha_anecdote_maxes, by='site_id') %>%
    filter(pb0 == 0 & pgdl == max_ha) %>% select(site_id, date) %>%
    group_by(site_id) %>% slice(1) %>% ungroup())
ggsave('3_summarize/out/eg_tha_anecdates_pghi_pblo.png', width=10, height=9)

# irr_thresh = c(0.0762, 0.6476), 
# wtr_thresh = c(11,25),

toha_df <- filter(all_ha_df, habitat=='opti_therm_hab')

tha_toha_df <- filter(all_ha_df, habitat!='opti_hab')
