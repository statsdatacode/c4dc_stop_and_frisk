base_map <- function(data) {
  
  ggplot(data,aes(x=long,y=lat)) +
    geom_polygon(data=tract_polygons,aes(x=long,y=lat,group=factor(CensusTract)),fill="grey",color="black") +
    scale_x_continuous(breaks=NULL) +
    scale_y_continuous(breaks=NULL) +
    labs(title='Washington, DC - Map of Stop and Frisk Incidents',
       caption="Source: Metropolitan Police Department",
       x="",y=""
    ) +
    theme(
      legend.position="bottom",
      panel.background = element_blank()
    )
  
}

sf_map <- geospatial_sf_data %>%
  base_map +
  geom_jitter() +
  labs(subtitle="Stops By Location: 2010-2017")

sf_map

sf_age_map <- geospatial_sf_data %>%
  filter(!is.na(age)) %>%
  base_map +
  geom_jitter(aes(color=as.numeric(age))) +
  labs(
    subtitle="Stops By Age & Location: 2010-2017",
    color='Age'
  )

sf_age_map

sf_sex_map <- geospatial_sf_data %>%
  filter(!is.na(sex)) %>%
  base_map +
  geom_jitter(aes(color=sex)) +
  labs(
    subtitle="Stops By Sex & Location: 2010 through 2017",
    color="Sex"
  )

sf_sex_map

sf_race_map <- geospatial_sf_data %>%
  filter(!is.na(race)) %>%
  base_map +
  geom_jitter(aes(color=race)) +
  labs(
    subtitle="Stops By Race & Location: 2010 through 2017",
    color="Race"
  )

sf_race_map

sf_age_sex_map <- geospatial_sf_data %>%
  filter(!is.na(age),!is.na(sex)) %>%
  base_map +
  geom_jitter(aes(color=as.numeric(age), shape=as.factor(sex))) +
  labs(
    subtitle="Stops By Age, Sex & Location: 2010 through 2017",
    color="Age",
    shape="Sex"
  )

sf_age_sex_map

sf_race_sex_map <- geospatial_sf_data %>%
  filter(!is.na(race),!is.na(sex)) %>%
  base_map +
  geom_jitter(aes(color=race,shape=as.factor(sex))) +
  labs(
    subtitle="Stops By Race, Sex & Location: 2010 through 2017",
    color="Race",
    shape="Sex"
  )

sf_race_sex_map