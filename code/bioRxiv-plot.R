################################################################################

# Plot bioRxiv data

################################################################################


# Load libraries ----
library(tidyverse)
library(here)
library(lubridate)

# Import data ----
biorxiv_newpaper <- readRDS(file = "data/bioRxiv-new-paper-stats.rds")
biorxiv_newpaper_H <- readRDS(file = "data/bioRxiv-new-paper-Harvard.rds")
biorxiv_published <- readRDS(file = "data/bioRxiv-published.rds")

# Graph: montly new bioRxiv preprints ----
ggplot(data = biorxiv_newpaper,
       mapping = aes(x = year.month, y = new_papers)) +
  geom_col(width = 0.85, fill = "#F7B6D2", alpha = 0.5) +
  scale_y_continuous(limits = c(0, 4000), expand = c(0.01, 0)) +
  facet_grid(~ year, scales="free", space="free_x", switch="both") +
  theme_bw() +
  theme(panel.grid.major.y = element_line(color = "grey90", linewidth = 0.15),
        panel.grid.minor.y = element_line(color = "grey90", linewidth = 0.15),
        panel.spacing.x = unit(0, "cm"),                   # plot facet panels together
        panel.grid.major.x = element_blank(),
        axis.text.x = element_blank(),
        axis.ticks.x = element_blank(),
        strip.background = element_blank(),
        panel.border = element_rect(colour = "grey70", linewidth = 0.25)) +
  labs(x = "", y = "New preprint deposited")

ggsave(filename = "bioRxiv_newpreprint_monthly_all.png", path = here("figure"),
       height = 3.5, width = 10, dpi = 300, units = "in")

ggsave(filename = "bioRxiv_newpreprint_monthly_all_shorter.png", path = here("figure"),
       height = 2.5, width = 10, dpi = 300, units = "in")


# Graph: montly new bioRxive preprints by Harvard researchers ----
## NOTE bc how the data was obtained, this only accounts for preprints whose
## corresponding author has an Harvard Affiliation
ggplot(data = biorxiv_newpaper_H,
       mapping = aes(x = year.month, y = h.preprint)) +
  geom_col(width = 0.85, fill = "#a51c30") +
  scale_y_continuous(limits = c(0, 90), breaks = seq(from = 0, to = 90, by = 30),
                     expand = c(0.01, 0.01)) +
  facet_grid(~ year, scales="free", space="free_x", switch="both") +
  theme_bw() +
  theme(panel.grid.major.y = element_line(color = "grey90", linewidth = 0.15),
        panel.grid.minor.y = element_line(color = "grey90", linewidth = 0.15),
        panel.spacing.x = unit(0, "cm"),                   # plot facet panels together
        panel.grid.major.x = element_blank(),
        axis.text.x = element_blank(),
        axis.ticks.x = element_blank(),
        strip.background = element_blank(),
        panel.border = element_rect(colour = "grey70", linewidth = 0.25)) +
  labs(x = "", y = "New preprint\ndeposited")

ggsave(filename = "bioRxiv_newpreprint_monthly_Harvard.png", path = here("figure"),
       height = 1.5, width = 9.9, dpi = 300, units = "in")


# Graph: bioRxiv preprints that gets published ----
## by month they were deposited
## different approach than https://api.biorxiv.org/reports/publication_summary, 
## which calculates cumulative publication rate

biorxiv_publish_rate <- biorxiv_published %>%
  count(year, year.month, name = "no_published") %>% 
  left_join(biorxiv_newpaper, by = c("year", "year.month")) %>%
  select(year, year.month, no_published, new_papers) %>%
  mutate(rate_published = no_published / new_papers * 100)

mean_rate <- mean(biorxiv_publish_rate$rate_published)
mean_rate

median_rate <- median(biorxiv_publish_rate$rate_published)
median_rate

ggplot() +
  geom_col(data = biorxiv_publish_rate, mapping = aes(x = year.month, y = rate_published),
           width = 0.85, fill = "#B8DEE6") +
  geom_hline(yintercept = median_rate, color = "#E377C2", alpha = 0.85, linewidth = 0.4) + 
  scale_y_continuous(limits = c(0, 100), breaks = seq(from = 0, to = 100, by = 25),
                     expand = c(0.01, 0.01)) +
  facet_grid(~ year, scales="free", space="free_x", switch="both") +
  theme_bw() +
  theme(panel.grid.major.y = element_line(color = "grey90", linewidth = 0.15),
        panel.grid.minor.y = element_line(color = "grey90", linewidth = 0.15),
        panel.spacing.x = unit(0, "cm"),                   # plot facet panels together
        panel.grid.major.x = element_blank(),
        axis.text.x = element_blank(),
        axis.ticks.x = element_blank(),
        strip.background = element_blank(),
        panel.border = element_rect(colour = "grey70", linewidth = 0.25)) +
  labs(x = "", y = "% preprint published")

ggsave(filename = "bioRxiv_publish_rate.png", path = here("figure"),
       height = 2.5, width = 9.9, dpi = 300, units = "in")


# Graph: time between preprinting and publishing ----
biorxiv_publish_delay <- biorxiv_published %>%
  mutate(diff = published_date - preprint_date)

## quick look at summary statistics
summary(biorxiv_publish_delay$diff)
fivenum(biorxiv_publish_delay$diff)
### there are preprinting dates that come after published dates

median_delay <- median(biorxiv_publish_delay$diff)

ggplot() +
  geom_histogram(data = biorxiv_publish_delay, mapping = aes(x = diff), binwidth = 30,
                 fill = "#B8DEE6", color = "grey50", linewidth = 0.25) +
  geom_vline(xintercept = median_delay, color = "#E377C2", alpha = 0.85, linewidth = 0.4) +
  scale_x_continuous(limits = c(-40, 1200)) +
  theme_bw() +
  theme(panel.grid.major.y = element_line(color = "grey90", linewidth = 0.15),
        panel.grid.minor.y = element_line(color = "grey90", linewidth = 0.15),
        panel.border = element_rect(colour = "grey25", linewidth = 0.25)) +
  labs(x = "delay between publish vs preprint date",
       y = "number of articles")

ggsave(filename = "bioRxiv_publish_delay.png", path = here("figure"),
       height = 4, width = 6, dpi = 300, units = "in")



