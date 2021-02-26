library(tidyverse)
library(patchwork)


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

fed_data = read_csv('./data/processed_data.csv.gz') %>%
  filter_stuff()

#-------------------------------
# Bar Plot of MS/PhD positions in GS-9/10/11/12 for 18-20
#-------------------------------
summary_fig = fed_data %>%
  filter(!is.na(perm_status)) %>%
  filter(!agency %in% c('Military','VA','NSF')) %>%
  filter(education %in% c('Masters','PhD')) %>%
  filter(pay_plan  %in% c('GS-09','GS-11','GS-12','ZP-02','ZP-03')) %>%
  filter(year %in% 2018:2020) %>%
  count(year, agency, pay_plan, perm_status) %>%
  group_by(agency, pay_plan, perm_status) %>%
  summarise(n = mean(n)) %>%
  ungroup() %>%  
  complete(agency,pay_plan,perm_status, fill=list(n=0)) %>% 
  ggplot(aes(x=pay_plan, y=n, fill=perm_status)) +
  geom_col(position = position_dodge()) +
  scale_fill_manual(values=c('#009e73','grey30')) + 
  facet_wrap(~agency, scales='free', ncol=3) +
  theme_bw(10) +
  theme(legend.position =  'bottom',
        legend.direction = 'horizontal',
        legend.title = element_text(size=12),
        legend.text = element_text(size=10),
        axis.text =  element_text(size=10, color='black'),
        axis.title.y = element_text(size=16),
        axis.title.x = element_blank(),
        plot.subtitle = element_text(size=16),
        strip.background = element_blank(),
        strip.text       = element_text(size=14,hjust=0)) +
  labs(x='', y='Average Number of Employees 2018-2020', fill='Permananent\nStatus',
       subtitle = 'GS 9/11/12 and ZP 2/3 Physical & Natural Science employees\nwith an MS or PhD',
       caption='')

ggsave('./summary_agency_count_figure.png', plot=summary_fig, width=25, height=40, units='cm', dpi=200)



#-------------------------------
# MS/PhD positions over time in GS-9/10/11/12
#-------------------------------
fed_data %>%
  filter(!is.na(perm_status)) %>%
  filter(!agency %in% c('Military','VA','NSF')) %>%
  #filter(education %in% c('Masters','PhD')) %>%
  filter(pay_plan  %in% c('GS-09','GS-10','GS-11','GS-12')) %>%
  count(year, agency, pay_plan, perm_status) %>%
  ggplot(aes(x=year, y=n, color=pay_plan)) +
  geom_line(aes(linetype=perm_status)) + 
  geom_point() + 
  #scale_color_manual(values=c('black','#009e73')) + 
  scale_color_brewer(palette = 'Dark2') +
  scale_x_continuous(breaks=seq(2008,2020,2)) + 
  facet_wrap(~agency, scales='free_y') +
  theme_bw(15) +
  theme(legend.position = c(0.7,0.1),
        legend.direction = 'vertical',
        legend.box       = 'horizontal',
        axis.text.x =  element_text(size=10, angle=45, hjust=1),
        strip.background = element_blank(),
        strip.text       = element_text(hjust=0)) +
  labs(x='', y='Number of employees',
       subtitle = 'Physical & Natural Science Positions with an MS or PhD and at GS 9-12',
       caption='*Does not account for promotions beyond G12 or self-reporting of education level') +
  guides(linetype = guide_legend('Permananent Status', override.aes = list(size=2), keywidth = unit(40,'mm')),
         color    = guide_legend(title=element_blank(),override.aes = list(size=2)))


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
  filter(!agency %in% c('Military','VA','NSF')) %>%
  filter(education %in% c('Masters','PhD')) %>%
  filter(pay_plan  %in% c('GS-09','GS-11','GS-12','ZP-02','ZP-03')) %>%
  #filter(agency %in% c('NIST','NOAA')) %>%
  compress_some_occupations() %>%
  filter(year %in% 2018:2020) %>%
  count(year, agency, occupation_desc,perm_status) %>%
  group_by(agency, occupation_desc,perm_status) %>%
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
  facet_grid(~agency, scales='free') +
  theme_bw() +
  theme(legend.position =  legend_position,
        legend.direction = 'horizontal',
        axis.text.x =  element_text(size=5, color='black'),
        axis.text.y = element_text(size=6, color='black'),
        strip.background = element_blank(),
        strip.text       = element_text(hjust=0, size=15)) +
  labs(x=x_axis, y='', fill='Permananent Status',
       subtitle = title,
       caption=waiver())
}

agencies = sample(unique(occupation_agency_totals$agency))
top = build_occupation_figure(agencies[1:6], legend_position = 'none', title = 'GS 9/11/12 and ZP 2/3 Physical & Natural Science\nemployees with an MS or PhD',
                              x_axis = '') +
  theme(plot.subtitle = element_text(size=15))
middle = build_occupation_figure(agencies[7:12], legend_position = 'none', title = waiver(),
                              x_axis = '') 
bottom = build_occupation_figure(agencies[13:20], legend_position = 'bottom', title = waiver(),
                        x_axis = 'Average Number of Employees 2018-2020') +
  theme(axis.title.x = element_text(size=15))

giant_figure = top + middle + bottom + plot_layout(ncol=1)

ggsave('./giant_occupation_count_figure.png', plot=giant_figure, width=20, height=75, units='cm', dpi=200)
