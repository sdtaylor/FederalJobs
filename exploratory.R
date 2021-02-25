library(tidyverse)



filter_stuff = function(df){
  # there's literally hundreds of pay plans, so just keep the primary
  # ones I see a lot
  df %>%
    mutate(keep = case_when(
      str_detect(pay_plan, 'GS') ~ TRUE, 
      str_detect(pay_plan, 'ZP') ~ FALSE,  # decent amount of ZP positions with NOAA and NIST but they are just too confusing to 
      TRUE ~ FALSE                         # compare with GS
    )) %>%
  filter(keep) %>%
  select(-keep)
}

fed_data = read_csv('./data/processed_data.csv.gz') %>%
  filter_stuff()

# Bar Plot of MS/PhD positions in GS-9/10/11/12 for 09-20
fed_data %>%
  filter(!is.na(perm_status)) %>%
  filter(!agency %in% c('Military','VA','NSF')) %>%
  filter(education %in% c('Masters','PhD')) %>%
  filter(pay_plan  %in% c('GS-09','GS-11','GS-12')) %>%
  filter(year %in% 2018:2020) %>%
  count(year, agency, pay_plan, perm_status) %>%
  group_by(agency, pay_plan, perm_status) %>%
  summarise(n = mean(n)) %>%
  ungroup() %>%
  complete(agency,pay_plan,perm_status, fill=list(n=0)) %>%
  ggplot(aes(x=pay_plan, y=n, fill=perm_status)) +
  geom_col(position = position_dodge()) +
  scale_fill_manual(values=c('#009e73','grey30')) + 
  facet_wrap(~agency, scales='free') +
  theme_bw(10) +
  theme(legend.position =  'right',
        legend.direction = 'vertical',
        legend.box       = 'horizontal',
        axis.text =  element_text(size=10, color='black'),
        axis.title.x = element_blank(),
        strip.background = element_blank(),
        strip.text       = element_text(hjust=0)) +
  labs(x='', y='Average Number of Employees 2008-2020', fill='Permananent\nStatus',
       subtitle = 'GS 9/11/12 Physical & Natural Science employees with an MS or PhD',
       caption='')




# MS/PhD positions over time in GS-9/10/11/12
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
