library(tidyverse)
library(patchwork)

#-------------------------------
# Create the 3 figures showing federal employment among graduate degree holders.
#-------------------------------

filter_stuff = function(df){
  # there's literally hundreds of pay plans, so just keep the primary
  # ones I see a lot
  df %>%
    mutate(keep = case_when(
      str_detect(pay_plan, 'GS') ~ TRUE, 
      str_detect(pay_plan, 'ZP') ~ TRUE,  # decent amount of ZP positions with NOAA and NIST 
      TRUE ~ FALSE                        
    )) %>%
  filter(keep) %>%
  # Only keep the relevant social science once. droping things like "intelligence" and "workforce development"
  mutate(keep = case_when(
    occupation_desc == '0170-HISTORY' ~ TRUE,
    occupation_desc == '0190-GENERAL ANTHROPOLOGY' ~ TRUE,
    occupation_desc == '0188-RECREATION SPECIALIST' ~ TRUE,
    occupation_desc == '0150-GEOGRAPHY' ~ TRUE,
    occupation_desc == '0193-ARCHEOLOGY' ~ TRUE,
    occupation_desc == '0110-ECONOMIST' ~ TRUE,
    occupation_desc == '0184-SOCIOLOGY' ~ TRUE,
    str_detect(occupation_desc, '01\\d{2}') ~ FALSE,
    TRUE ~ TRUE,
  )) %>%
  filter(keep) %>%
  select(-keep)
}

fed_data = read_csv('./data/processed_data.csv.gz', col_types = cols(agency_acronym=col_character())) %>%
  filter_stuff() %>%
  mutate(agency_and_acronym = paste0(agency, ' (',agency_acronym,')')) %>%
  mutate(agency_and_acronym = ifelse(agency=='Other','Other',agency_and_acronym))

#-------------------------------
# Bar Plot of MS/PhD positions in GS-9/10/11/12 for 18-20
#-------------------------------
summary_fig = fed_data %>%
  filter(!is.na(perm_status)) %>%
  filter(!agency_acronym %in% c('Mil','CBBP','VA','NSF')) %>% 
  filter(education %in% c('Masters','PhD')) %>%
  filter(pay_plan  %in% c('GS-09','GS-11','GS-12','ZP-02','ZP-03')) %>%
  filter(year %in% 2018:2020) %>%
  count(year, agency_and_acronym, pay_plan, perm_status) %>%
  group_by(agency_and_acronym, pay_plan, perm_status) %>%
  summarise(n = mean(n)) %>%
  ungroup() %>% 
  complete(agency_and_acronym,pay_plan,perm_status, fill=list(n=0)) %>% 
  ggplot(aes(x=pay_plan, y=n, fill=perm_status)) +
  geom_col(position = position_dodge()) +
  scale_fill_manual(values=c('#009e73','grey30')) + 
  facet_wrap(~agency_and_acronym, scales='free', ncol=3) +
  theme_bw(10) +
  theme(legend.position =  c(0.6,0.03),
        #legend.direction = 'vertical',
        legend.title = element_text(size=22),
        legend.text = element_text(size=18),
        #legend.key.size = unit(10,'mm'),
        axis.text =  element_text(size=10, color='black'),
        axis.title.y = element_text(size=16),
        axis.title.x = element_blank(),
        plot.subtitle = element_text(size=20),
        strip.background = element_blank(),
        strip.text       = element_text(size=11,hjust=0)) +
  labs(x='', y='Average Number of Employees 2018-2020', fill='Permananent Status',
       subtitle = 'GS 9/11/12 and ZP 2/3 Social/Physical/Natural Science employees\nwith an MS or PhD',
       caption='') +
  guides(fill=guide_legend(title.position = 'top', direction = 'horizontal', keywidth = unit(15,'mm')))

ggsave('./summary_agency_count_figure.png', plot=summary_fig, width=28, height=40, units='cm', dpi=200)



#-------------------------------
# Large plot with specific occupations per agency
#-------------------------------
compress_some_occupations = function(df){
  low_count_occupations = c('1372-GEODESY','0405-PHARMACOLOGY','1384-TEXTILE TECHNOLOGY',
                            '0459-IRRIGATION SYSTEM OPERATION','1321-METALLURGY')
  df %>%
    mutate(occupation_desc = case_when(
      occupation_desc == '1371-CARTOGRAPHIC TECHNICIAN' ~ '1370-CARTOGRAPHY',
      occupation_desc == '1316-HYDROLOGIC TECHNICIAN'   ~ '1315-HYDROLOGY',
      occupation_desc == '1399-PHYSICAL SCIENCE STUDENT TRAINEE' ~ '1301-GENERAL PHYSICAL SCIENCE',
      occupation_desc == '1311-PHYSICAL SCIENCE TECHNICIAN' ~ '1301-GENERAL PHYSICAL SCIENCE',
      occupation_desc == '0401-GENERAL NATURAL RESOURCES MANAGEMENT AND BIOLOGICAL SCIENCES' ~ '0401-GENERAL BIOLOGY/NATURAL RESOURCES',
      occupation_desc == '0404-BIOLOGICAL SCIENCE TECHNICIAN' ~ '0401-GENERAL BIOLOGY/NATURAL RESOURCES', # 404/401 are pretty much the same
      occupation_desc == '0499-BIOLOGICAL SCIENCE STUDENT TRAINEE' ~ '0401-GENERAL BIOLOGY/NATURAL RESOURCES',
      occupation_desc == '0455-RANGE TECHNICIAN' ~ '0454-RANGELANDS',
      occupation_desc == '0454-RANGELAND MANAGEMENT' ~ '0454-RANGELANDS',
      occupation_desc == '1316-HYDROLOGIC TECHNICIAN' ~ '1315-HYDROLOGY',
      occupation_desc == '1341-METEOROLOGICAL TECHNICIAN' ~ '1340-METEOROLOGY',
      occupation_desc == '0480-FISH AND WILDLIFE ADMINISTRATION' ~ '0480-FISH/WILDLIFE MGMT',
      occupation_desc == '0485-WILDLIFE REFUGE MANAGEMENT' ~ '0480-FISH/WILDLIFE MGMT',
      occupation_desc == '0458-SOIL CONSERVATION TECHNICIAN' ~ '0457-SOIL CONSERVATION',
      occupation_desc == '0462-FORESTRY TECHNICIAN' ~ '0460-FORESTRY',
      occupation_desc %in% low_count_occupations   ~ 'Other',
      TRUE ~ occupation_desc
    ))
}

occupation_agency_totals = fed_data %>%
  filter(!is.na(perm_status)) %>%
  filter(!agency_acronym %in% c('Mil','CBBP','VA','NSF')) %>% 
  filter(education %in% c('Masters','PhD')) %>%
  filter(pay_plan  %in% c('GS-09','GS-11','GS-12','ZP-02','ZP-03')) %>%
  #filter(agency %in% c('NIST','NOAA')) %>%
  compress_some_occupations() %>%
  filter(year %in% 2018:2020) %>%
  count(year, agency,agency_and_acronym, occupation_desc,perm_status) %>%
  group_by(agency,agency_and_acronym, occupation_desc,perm_status) %>%
  summarise(n = mean(n)) %>%
  ungroup() %>%
  #complete(agency,occupation_desc,perm_status, fill=list(n=0)) %>%
  as_tibble()


build_occupation_figure = function(agency_list, legend_position, title, x_axis){
  
  occupation_agency_totals %>%
    filter(agency %in% agency_list) %>%
  ggplot(aes(y=str_wrap(occupation_desc,25), x=n, fill=perm_status)) +
  geom_col() +
  scale_fill_manual(values=c('#009e73','grey30')) +
  facet_grid(~str_wrap(agency_and_acronym,20), scales='free_x') +
  theme_bw() +
  theme(legend.position =  legend_position,
        legend.direction = 'horizontal',
        axis.text.x =  element_text(size=9, color='black'),
        axis.text.y = element_text(size=7, color='black'),
        strip.background = element_blank(),
        strip.text       = element_text(hjust=0, size=11)) +
  labs(x=x_axis, y='', fill='Permananent Status',
       subtitle = title,
       caption=waiver())
}

agencies = unique(occupation_agency_totals$agency)
top = build_occupation_figure(agencies[1:5], legend_position = 'none', title = 'GS 9/11/12 and ZP 2/3 Social/Physical/Natural Science\nemployees with an MS or PhD',
                              x_axis = '') +
  theme(plot.subtitle = element_text(size=15))

middle1 = build_occupation_figure(agencies[6:10], legend_position = 'none', title = waiver(),
                              x_axis = '') 
middle2 = build_occupation_figure(agencies[11:15], legend_position = 'none', title = waiver(),
                                  x_axis = '') 
middle3 = build_occupation_figure(agencies[16:20], legend_position = 'none', title = waiver(),
                                  x_axis = '') 

bottom = build_occupation_figure(agencies[21:25], legend_position = 'bottom', title = waiver(),
                        x_axis = 'Average Number of Employees 2018-2020') +
  theme(axis.title.x = element_text(size=15))

giant_figure = top + middle1 + middle2 + middle3 + bottom + plot_layout(ncol=1)

ggsave('./giant_occupation_count_figure.png', plot=giant_figure, width=25, height=110, units='cm', dpi=150)

# as separate figures
n_splits = 5
for(i in 1:n_splits){
  # agency groupings 1-5, 6-10, etc
  end =  i*n_splits
  start = end - n_splits + 1
  fig = build_occupation_figure(agencies[start:end], legend_position = 'bottom', title = 'GS 9/11/12 and ZP 2/3 Social/Physical/Natural Science\nemployees with an MS or PhD',
                          x_axis = '') +
        theme(plot.subtitle = element_text(size=15))
  
  fig_filename = paste0('./occupation_figure_',i,'_of_5.png')
  ggsave(fig_filename, plot=fig, width=25, height=22, units='cm', dpi=100)
  
}

#-------------------------------
# map figure
#-------------------------------
library(sf)

states = st_read('./data/gis/final_us_composite.geojson') %>%
  mutate(state = tolower(name)) %>%
  select(state) %>%
  mutate(state = ifelse(state == 'united states virgin islands','virgin islands',state))

state_outlier_boxes = st_read('./data/gis/outlier_bounding_boxes.geojson') %>%
  mutate(state = tolower(name))%>%
  mutate(state = ifelse(state == 'united states virgin islands','virgin islands',state))

location_counts = fed_data %>%
  filter(!is.na(perm_status)) %>%
  filter(!agency_acronym %in% c('Mil','CBBP','VA','NSF')) %>% 
  filter(education %in% c('Masters','PhD')) %>%
  filter(pay_plan  %in% c('GS-09','GS-11','GS-12','ZP-02','ZP-03')) %>%
  filter(year %in% 2018:2020) %>%
  count(year, agency_and_acronym, location) %>%
  group_by(agency_and_acronym,location) %>%
  summarise(n = mean(n)) %>%
  ungroup() %>%  
  complete(agency_and_acronym,location, fill=list(n=0)) %>% 
  mutate(state = tolower(location))

map_data = inner_join(states, location_counts, by='state')

map_figure = ggplot(map_data) +
  geom_sf(aes(fill=n), color='black', size=0.1) +
  geom_sf(data=state_outlier_boxes, fill=NA, color='grey40', size=0.08) + 
  scale_fill_viridis_c(limits=c(1,200), na.value = 'white', breaks=c(50,100,150,200), labels=c('50','100','150','200+')) +
  facet_wrap(~str_wrap(agency_and_acronym,25), ncol=3) +
  theme(axis.text = element_blank(),
        axis.ticks = element_blank(),
        panel.border = element_blank(),
        panel.background = element_blank(),
        strip.background = element_blank(),
        strip.text = element_text(color='black', size=10, hjust=0.1),
        panel.grid = element_blank(),
        plot.title = element_text(size=24, face='bold', hjust=0.5),
        legend.title = element_text(size=14),
        legend.text = element_text(size=12),
        legend.position = c(0.7, 0.05),
        legend.direction = 'horizontal') +
  labs(subtitle = 'GS 9/11/12 and ZP 2/3 Social/Physical/Natural Science employees with an MS or PhD') +
  guides(fill=guide_colorbar(title = 'Average Number of Employees 2018-2020', title.position = 'top',
                             barwidth = unit(85,'mm')))


ggsave('./map_figure.png', plot=map_figure, width=18, height=40, units='cm', dpi=150)
