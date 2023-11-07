setwd('/Users/fsp5/OneDrive - University of Pittsburgh/work/athletics/sports_science/mentorship/teaching/lectures/force_plates')

df <- readr::read_csv('cmj_force.csv')

df %>%
  filter(`Time (s)` > 2) %>%
  ggplot(aes(x = `Time (s)`, y = `Combined (N)`)) +
  geom_line(group = 1,
            color = pitt_fill_colors[2],
            size = 2) +
  labs(y = "Force (N)",
       x = "Time") +
  theme(axis.text.x = element_blank(),
        axis.ticks = element_blank(),
        axis.text.y = element_text(size = 12),
        axis.title = element_text(size = 14, face = "bold"),
        panel.background = element_blank(),
        axis.line = element_line(color = "black"))

ggsave('force.png',
       width = 5,
       height = 3.5,
       units = "in",
       dpi = 300)

df <- readr::read_csv('cmj_velocity.csv')

df %>%
  filter(`Time (s)` > 2) %>%
  ggplot(aes(x = `Time (s)`, y = `Velocity (m/s)`)) +
  geom_line(group = 1,
            color = pitt_fill_colors[2],
            size = 2) +
  labs(y = "Velocity (m/s2)",
       x = "Time") +
  theme(axis.text.x = element_blank(),
        axis.ticks = element_blank(),
        axis.text.y = element_text(size = 12),
        axis.title = element_text(size = 14, face = "bold"),
        panel.background = element_blank(),
        axis.line = element_line(color = "black"))

ggsave('velocity.png',
       width = 5,
       height = 3.5,
       units = "in",
       dpi = 300)

df <- readr::read_csv('cmj_displacement.csv')

df %>%
  filter(`Time (s)` > 2) %>%
  ggplot(aes(x = `Time (s)`, y = `Displacement (m)`)) +
  geom_line(group = 1,
            color = pitt_fill_colors[2],
            size = 2) +
  labs(y = "Displacement (m)",
       x = "Time") +
  theme(axis.text.x = element_blank(),
        axis.ticks = element_blank(),
        axis.text.y = element_text(size = 12),
        axis.title = element_text(size = 14, face = "bold"),
        panel.background = element_blank(),
        axis.line = element_line(color = "black"))

ggsave('displacement.png',
       width = 5,
       height = 3.5,
       units = "in",
       dpi = 300)


df <- readr::read_csv('cmj_power.csv')

df %>%
  filter(`Time (s)` > 2) %>%
  ggplot(aes(x = `Time (s)`, y = `Power (W)`)) +
  geom_line(group = 1,
            color = pitt_fill_colors[2],
            size = 2) +
  labs(y = "Power (W)",
       x = "Time") +
  theme(axis.text.x = element_blank(),
        axis.ticks = element_blank(),
        axis.text.y = element_text(size = 12),
        axis.title = element_text(size = 14, face = "bold"),
        panel.background = element_blank(),
        axis.line = element_line(color = "black"))

ggsave('power.png',
       width = 5,
       height = 3.5,
       units = "in",
       dpi = 300)
