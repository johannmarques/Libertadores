library(tidyverse)
library(rvest)

html_page <- read_html('https://www.worldfootball.net/winner/copa-libertadores/')

tables <- html_page %>%
  html_elements('table')

data <- tables[[1]] %>%
  html_table() %>%
  select(Year, Winner, Country) %>%
  arrange(Year) %>%
  mutate(Win = 1)

ev_df <- expand.grid(Year = min(data$Year):max(data$Year),
                     Country = data %>%
                       distinct(Country) %>%
                       {.$Country}) %>%
  left_join(data) %>%
  replace_na(list(Win = 0)) %>%
  group_by(Country) %>%
  summarise(Year, Won = cumsum(Win)) %>%
  ungroup() %>%
  mutate(VAR = factor(ifelse(Year >= 2018, 'VAR', 'Pre VAR')))

preVAR <- ev_df[ev_df$VAR=='Pre VAR',] %>%
  group_by(Country) %>%
  mutate(Won = fitted(loess(Won ~ Year))) %>%
  ungroup()

postVAR <- ev_df[ev_df$VAR=='VAR',] %>%
  group_by(Country) %>%
  mutate(Won = fitted(loess(Won ~ Year))) %>%
  ungroup()

countries_colors <- c(
  'Argentina' = '#6CACE4',
  'Uruguay' = '#0035AA',
  'Brazil' = '#0CA030',
  'Chile' = '#6C2E60',
  'Colombia' = '#7F7C44',
  'Ecuador' = '#8c80a4',
  'Paraguay' = '#D62718'
)

ev_df %>%
  ggplot(aes(x = Year, y = Won, color = Country), size = 2) +
  geom_line(data = preVAR, aes(x = Year, y = Won, color = Country), size = 12, alpha = 0.2, lineend='round') +
  geom_line(data = postVAR, aes(x = Year, y = Won, color = Country), size = 12, alpha = 0.2, lineend='round') +
  geom_step() + geom_vline(xintercept = 2018, linetype = 'dashed') +
  annotate("text", x = 2021.5, y = 12, label = "VAR implemented", size = 5) +
  scale_color_manual(values = countries_colors) +
  geom_curve(
    aes(x = 2021.5, y = 11.2, xend = 2018.4, yend = 10.5),
    #data = .,
    arrow = arrow(length = unit(0.03, "npc"), angle = 25), curvature = -0.45, color = 'black'
  ) +
  labs(y = 'Libertadores trophies', x = 'Year', color = '',
       title = 'Libertadores trophies over time',
       subtitle = 'Argentina stucks after VAR implementation') +
  theme(panel.background = element_rect(fill = 'white', colour = '#4c3818'),
        panel.grid = element_blank(),
        title = element_text(colour = '#4c3818', size = 18, family = 'Go Medium'),
        panel.grid.major.y = element_line(colour = 'grey75'),
        axis.text = element_text(size = 14, family = 'Go Medium', color = '#4c3818'),
        legend.key = element_blank(),
        legend.text = element_text(size = 14, family = 'Go Medium', color = '#4c3818'))
  grid::grid.raster(png::readPNG("sign.png"), x = 0.995, y = 0.001, just = c('right', 'bottom'), width = unit(2, 'inches'))

