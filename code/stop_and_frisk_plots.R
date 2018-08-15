raw_time <- sf_data %>%
  ggplot(aes(x=hms(time_min))) +
  geom_bar() +
  scale_x_time(
    name='Time of Day',
    breaks=c(hms("04:00:00"),hms("08:00:00"),hms("12:00:00"),hms("16:00:00"),hms("20:00:00")),
    labels=c('4am','8am','12pm','4pm','8pm')
  ) +
  labs(title='Washington, DC - Stop and Frisk Incidents',
       subtitle="Stops By Time of Day (1 minute increments)",
       caption="Source: Metropolitan Police Department",
       x="Time of Day"
  )

save_plot(raw_time, "stop_and_frisk_1_min.png")

qtr_hour_time <- sf_data %>%
  ggplot(aes(x=hms(time_qtr_hour))) +
  geom_bar() +
  scale_x_time(
    name='Time of Day',
    breaks=c(hms("04:00:00"),hms("08:00:00"),hms("12:00:00"),hms("16:00:00"),hms("20:00:00")),
    labels=c('4am','8am','12pm','4pm','8pm')
  ) +
  labs(title='Washington, DC - Stop and Frisk Incidents',
       subtitle="Stops By Time of Day (15 minute increments)",
       caption="Source: Metropolitan Police Department",
       x="Time of Day"
  )

save_plot(qtr_hour_time, "stop_and_frisk_15_min.png")

hlf_hour_time <- sf_data %>%
  ggplot(aes(x=hms(time_hlf_hour))) +
  geom_bar() +
  scale_x_time(
    name='Time of Day',
    breaks=c(hms("04:00:00"),hms("08:00:00"),hms("12:00:00"),hms("16:00:00"),hms("20:00:00")),
    labels=c('4am','8am','12pm','4pm','8pm')
  ) +
  labs(title='Washington, DC - Stop and Frisk Incidents',
       subtitle="Stops By Time of Day (30 minute increments)",
       caption="Source: Metropolitan Police Department",
       x="Time of Day"
  )

save_plot(hlf_hour_time, "stop_and_frisk_30_min.png")

hour_time <- sf_data %>%
  ggplot(aes(x=hms(time_hour))) +
  geom_bar() +
  scale_x_time(
    name='Time of Day',
    breaks=c(hms("04:00:00"),hms("08:00:00"),hms("12:00:00"),hms("16:00:00"),hms("20:00:00")),
    labels=c('4am','8am','12pm','4pm','8pm')
  ) +
  labs(title='Washington, DC - Stop and Frisk Incidents',
       subtitle="Stops By Time of Day (1 hour increments)",
       caption="Source: Metropolitan Police Department",
       x="Time of Day"
  )

save_plot(hour_time, "stop_and_frisk_1_hour.png")

three_hour_time <- sf_data %>%
  mutate(time_three_hour=ifelse(time_three_hour == "0:00:00", "24:00:00",time_three_hour)) %>%
  ggplot(aes(x=hms(time_three_hour))) +
  geom_bar() +
  scale_x_time(
    name='Time of Day',
    breaks=c(hms("03:00:00"),hms("06:00:00"),hms("09:00:00"),hms("12:00:00"),hms("15:00:00"),hms("18:00:00"),hms("21:00:00"),hms("24:00:00")),
    labels=c('2am - 5am','5am - 8am','8am - 11am','11am - 2pm','2pm - 5pm', '5pm - 8pm', '8pm - 11pm', '11pm - 2am')
  ) +
  labs(title='Washington, DC - Stop and Frisk Incidents',
       subtitle="Stops By Time of Day (3 hour increments)",
       caption="Source: Metropolitan Police Department",
       x="Time of Day"
  )

save_plot(three_hour_time, "stop_and_frisk_3_hour.png")

six_hour_time <- sf_data %>%
  mutate(time_six_hour=ifelse(time_six_hour == '0:00:00','24:00:00',time_six_hour)) %>%
  ggplot(aes(x=hms(time_six_hour))) +
  geom_bar() +
  scale_x_time(
    name='Time of Day',
    breaks=c(hms("06:00:00"),hms("12:00:00"),hms("18:00:00"),hms('24:00:00')),
    labels=c('3am - 9am','9am - 3pm','3pm - 9pm', '9pm - 3am')
  ) +
  labs(title='Washington, DC - Stop and Frisk Incidents',
       subtitle="Stops By Time of Day (6 hour increments)",
       caption="Source: Metropolitan Police Department",
       x="Time of Day"
  )

save_plot(six_hour_time, "stop_and_frisk_6_hour.png")

raw_date <- sf_data %>%
  filter(date >= mdy("01/02/2012")) %>%
  ggplot(aes(x=date)) +
  geom_bar() +
  scale_x_date(
    name='Date',
    limits = c(mdy('01/02/2012'),mdy('12/31/2017'))
  ) +
  labs(title='Washington, DC - Stop and Frisk Incidents',
       subtitle="Stops Over Time (January 2nd, 2012 through December 31st, 2017)",
       caption="Source: Metropolitan Police Department",
       x="Date"
  )

raw_date

save_plot(raw_date, "stop_and_frisk_date.png")

month_date <- sf_data %>%
  filter(date >= as.Date("1/2/2012", "%m/%d/%Y")) %>%
  mutate(date_month = as.Date(paste0(month,'/1/',year), "%m/%d/%Y")) %>%
  ggplot(aes(x=date_month)) +
  geom_bar() +
  scale_x_date(
    name='Date',
    limits = c(mdy('01/02/2012'),mdy('12/31/2017'))
  ) +
  labs(title='Washington, DC - Stop and Frisk Incidents',
       subtitle="Stops Each Month (January 2nd, 2012 through December 31st, 2017)",
       caption="Source: Metropolitan Police Department",
       x="Date"
  )

save_plot(month_date, "stop_and_frisk_monthly_over_time.png")

raw_month <- sf_data %>%
  filter(date >= as.Date("1/2/2012", "%m/%d/%Y")) %>%
  ggplot(aes(x=fmonth)) +
  geom_bar() +
  labs(title='Washington, DC - Stop and Frisk Incidents',
       subtitle="Stops By Month",
       caption="Source: Metropolitan Police Department",
       x="Month"
  )

save_plot(raw_month, "stop_and_frisk_by_month.png")

raw_season <- sf_data %>%
  filter(date >= as.Date("1/2/2012", "%m/%d/%Y"), date <= as.Date("12/31/2017", "%m/%d/%Y")) %>%
  ggplot(aes(x=season)) +
  geom_bar() +
  labs(title='Washington, DC - Stop and Frisk Incidents',
       subtitle="Stops By Season (January 2nd, 2012 through December 31st, 2017)",
       caption="Source: Metropolitan Police Department",
       x="Season"
  )

save_plot(raw_season, "stop_and_frisk_by_season.png")

raw_ward <- sf_data %>%
  ggplot(aes(x=ward)) +
  geom_bar()

raw_ward  