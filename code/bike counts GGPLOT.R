bikecounts
library(ggfortify)
library(directlabels)

ggplot(data = bikecounts) + 
  geom_point(mapping = aes(x = date, y = total, color = name))  + 
  facet_wrap(~ name, nrow = 2) +
  geom_line(mapping = aes(x = date, y = total, linetype = name, alpha = 1/5, position = ""))

ggplot(data = bikecounts) + 
  geom_point(mapping = aes(x = date, y = total,color = name))  + 
  facet_wrap(~ name, nrow = 2) +
  geom_smooth(mapping = aes(x = date, y = total, linetype = name, alpha = 2/5, group = westbound))

ggplot(data = bikecounts) + 
  geom_bar(mapping = aes(x = date, y = total, group = westbound))

bikecounts %>% 
  mutate("WB-EB" = westbound - eastbound)

bikecounts

ggplot(data = bikecounts %>% 
         mutate("WB-EB" = westbound - eastbound)) + 
  geom_point(mapping = aes(x = date, y = 'WB-EB', color = name))

ggplot(data = bikecounts) + 
  stat_summary(
    mapping = aes(x = date, y = total, linetype = name, alpha = 3/5),
    fun.ymin = min,
    fun.ymax = max,
    fun.y = median
  )

bikecounts %>% 
  group_by(name) %>% 
  summarize(monthly_total=sum(total)) %>% 
  ggplot() + 
  geom_line(mapping = aes(x = date, y = monthly_total, group = name))

bikecounts %>% 
  mutate(DATE=as_date(date)) %>% 
  left_join(weather) %>%
  ggplot() + 
  geom_col(aes(x = DATE, y = total, color = name)) +
  geom_line(aes(x=DATE, y=TMIN * 100))

ggplot() +
  geom_col(data=bikecounts, aes(x = as_date, y = total, color = name)) +
  geom_line(data=weather, aes(x=DATE, y=TMIN * 100))

bikecounts_weather <- bikecounts %>% 
  mutate(DATE=as_date(date)) %>% 
  left_join(weather)