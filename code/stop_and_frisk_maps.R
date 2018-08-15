base_map <- function(data) {
  
  ggplot(data,aes(x=long,y=lat)) +
    geom_polygon(data=tract_polygons,aes(x=long,y=lat,group=factor(CensusTract)),fill="grey",color="black") 
  
}

sf_map <- geospatial_sf_data %>%
  base_map +
  geom_jitter() +
  scale_x_continuous(breaks=NULL) +
  scale_y_continuous(breaks=NULL) +
  labs(title='Washington, DC - Map of Stop and Frisk Incidents',
       subtitle='2010 - 2017',
       caption="Source: Metropolitan Police Department",
       x="",y=""
  ) +
  theme(
    legend.position="bottom",
    panel.background = element_blank()
  )

sf_map

sf_age_map <- geospatial_sf_data %>%
  filter(!is.na(age)) %>%
  base_map +
  geom_jitter(aes(color=as.numeric(age))) +
  scale_x_continuous(breaks=NULL) +
  scale_y_continuous(breaks=NULL) +
  labs(title='Washington, DC - Map of Stop and Frisk Incidents',
       subtitle='Stops By Age',
       caption="Source: Metropolitan Police Department",
       x="",y=""
  ) +
  theme(
    legend.position="bottom",
    panel.background = element_blank()
  )

sf_age_map

sf_sex_map <- geospatial_sf_data %>%
  filter(!is.na(sex)) %>%
  base_map +
  geom_jitter(aes(color=sex))  +
  scale_x_continuous(breaks=NULL) +
  scale_y_continuous(breaks=NULL) +
  labs(title='Washington, DC - Map of Stop and Frisk Incidents',
       subtitle='Stops By Age',
       caption="Source: Metropolitan Police Department",
       x="",y=""
  ) +
  theme(
    legend.position="bottom",
    panel.background = element_blank()
  )

sf_sex_map

sf_race_map <- geospatial_sf_data %>%
  filter(!is.na(race)) %>%
  base_map +
  geom_jitter(aes(color=race)) +
  scale_x_continuous(breaks=NULL) +
  scale_y_continuous(breaks=NULL) +
  labs(title='Washington, DC - Map of Stop and Frisk Incidents',
       subtitle='Stops By Race',
       caption="Source: Metropolitan Police Department",
       x="",y=""
  ) +
  theme(
    legend.position="bottom",
    panel.background = element_blank()
  )

sf_race_map

sf_age_sex_map <- geospatial_sf_data %>%
  filter(!is.na(age),!is.na(sex)) %>%
  base_map +
  geom_jitter(aes(color=as.numeric(age))) +
  facet_wrap(~sex) +
  scale_x_continuous(breaks=NULL) +
  scale_y_continuous(breaks=NULL) +
  labs(title='Washington, DC - Map of Stop and Frisk Incidents',
       subtitle='Stops By Age & Sex',
       caption="Source: Metropolitan Police Department",
       x="",y=""
  ) +
  theme(
    legend.position="bottom",
    panel.background = element_blank()
  )

sf_age_sex_map

sf_race_sex_map <- geospatial_sf_data %>%
  filter(!is.na(race),!is.na(sex)) %>%
  base_map +
  geom_jitter(aes(color=race)) +
  facet_wrap(~sex) +
  scale_x_continuous(breaks=NULL) +
  scale_y_continuous(breaks=NULL) +
  labs(title='Washington, DC - Map of Stop and Frisk Incidents',
       subtitle='Stops By Race & Sex',
       caption="Source: Metropolitan Police Department",
       x="",y=""
  ) +
  theme(
    legend.position="bottom",
    panel.background = element_blank()
  )

sf_race_sex_map

sf_time_map <- geospatial_sf_data %>%
  base_map +
  geom_jitter() +
  facet_wrap(~time_six_hour) +
  scale_x_continuous(breaks=NULL) +
  scale_y_continuous(breaks=NULL) +
  labs(title='Washington, DC - Map of Stop and Frisk Incidents',
       subtitle='Stops By Time of Day',
       caption="Source: Metropolitan Police Department",
       x="",y=""
  ) +
  theme(
    legend.position="bottom",
    panel.background = element_blank()
  )

sf_time_map

sf_age_time_map <- geospatial_sf_data %>%
  base_map +
  geom_jitter(aes(color=as.numeric(age))) +
  facet_wrap(~time_six_hour) +
  scale_x_continuous(breaks=NULL) +
  scale_y_continuous(breaks=NULL) +
  labs(title='Washington, DC - Map of Stop and Frisk Incidents',
       subtitle='Stops By Age & Time of Day',
       caption="Source: Metropolitan Police Department",
       x="",y=""
  ) +
  theme(
    legend.position="bottom",
    panel.background = element_blank()
  )

sf_age_time_map