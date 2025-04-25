# Scope: Toekomstverkenning

# A1. Load Libraries. ----------------------------------------------------------

# install.packages("devtools")
#devtools::install_github("kylebutts/tailwindr")
#devtools::install_github("lgnbhl/aniview")

library(ggplot2)
library(gganimate)
library(tidyverse)

source("~/Data/Projecten/KNVB-voetbalmonitor/code/id_to_season.R")

folder_path <- "~/KNVB/OND-Onderzoek-Toekomstverkenning KNVB2 - Documenten/data"

knvb_cols <- list(
  `donkeroranje_nieuw`    = "#FF6000",
  `donkeroranje_nieuw_1`  = "#FF8D4D",
  `donkeroranje_nieuw_2`  = "#FFB68D",
  `donkeroranje_nieuw_3`  = "#FFD6C0",
  `blauwgroen`            = "#00D6B7",
  `blauwgroen_1`          = "#00E5C9",
  `blauwgroen_2`          = "#52EFD9",
  `blauwgroen_3`          = "#A4EDE1",
  `mist_white_core`       = "#89C9C1",
  `mist_white_1`          = "#AFD5D2",
  `mist_white_2`          = '#CAE3E1',
  `mist_white_3`          = "#D8EBE6",
  `white`                 = "#ffffff",
  `groen`                 = '#008000',
  `rood`                  = '#ff0000',
  `licht grijs`           = "#F2F2F2",
  `donker grijs`          = "#7F7F7F"
)


grid_col <- axis_col <- "#505053"
def_color <- "#505053"
base_family <- "Arial"
base_size <- 10

library(extrafont)
ret <-
  ggplot2::theme_minimal(base_family = "Arial", base_size = 10) +
  theme(
    # Legend
    legend.background = element_blank(),
    legend.title.align = 1,
    legend.box.just = "left",
    legend.box.spacing =  unit(0.15, "cm"),
    legend.spacing.x =  unit(0.15, "cm"),
    legend.text = element_text(lineheight = 1, size = rel(.75), ),
    legend.title = element_text(lineheight = 2.5, size = rel(.75)),
    legend.position = "top",
    legend.direction = "horizontal",
    legend.key.size = unit(0.25, "cm"),
    strip.background = element_blank(),
    strip.placement = "outside"
  ) + theme(panel.grid.minor.y = element_blank()) + theme(panel.grid = element_blank()) +
  theme(axis.ticks = element_blank()) +
  theme(axis.ticks.x = element_blank()) +
  theme(axis.ticks.y = element_blank()) +
  theme(# Axis lineaxis.line = element_line(
    linetype = "solid",
    colour = axis_col,
    size = 0.05)  +
  theme(# Text
        text = element_text(
          family = "Arial",
          colour = "#3C3C3C",
          size = base_size
        )) + theme(
          axis.title.x = element_text(
            family = "Arial",
            colour = "#3C3C3C",
            size = rel(1),
            hjust = 0.5,
            vjust = -2,
            margin = margin(unit(c(5, 0, 0, 0), "cm"))
          ),
          axis.title.y = element_text(
            family = "Arial",
            colour = "#3C3C3C",
            size = rel(1),
            hjust = 0.5,
            margin = margin(unit(c(5, 20, 0, 20), "cm"))
          )
        )  +
  theme(
    axis.text.x = element_text(
      family = "Arial",
      colour = "#3C3C3C",
      size = rel(1)
    ),
    axis.text.y = element_text(
      family = "Arial",
      colour = "#3C3C3C",
      size = rel(1)
    )
  ) +
  theme(
    # Text titel, subtitel, caption
    plot.title = element_text(
      family = "Arial Black",
      colour =  "#3C3C3C",
      size = rel(2.5)
    ),
    plot.subtitle = element_text(
      family = "Arial",
      size = rel(1.6),
      colour = "#A0A0A3"
    ),
    plot.caption = element_text(
      family = "Arial",
      colour = "#3C3C3C",
      size = rel(0.8),
      vjust = -8.5
    ),
    strip.text = element_text(
      family = "Arial",
      colour = "#3C3C3C",
      size = rel(.75)
    ),
    plot.tag = element_text(
      family = "Arial",
      colour = "#3C3C3C",
      size = rel(0.8),
      vjust = -8.5
    )
  ) +
  theme(
    panel.background =  element_rect(fill = NA, color = NA),
    plot.background = element_rect(fill = NA, color = NA),
    panel.border = element_blank(),
    # t = 0, r = 0, b = 0, l = 0
    plot.margin = unit(c(1.5, 1, 1, 0.5), "cm")
  )


# B1. Bevolkingsprognose CBS. --------------------------------------------------

## Prepare the data ----
cbs_bevolking <- read_csv(paste0(folder_path, "/ruw/verwachte bevolkingsontwikkeling.csv")) %>%
  janitor::clean_names() %>%
  mutate(
    waarneming = ifelse(
      waarneming <= 10000,
      sprintf("%.2f", waarneming / 100),
      sprintf("%.2f", waarneming / 1000)
    ),
    prognose_2024_2070 = case_when(
      prognose_2024_2070 <= 1000 ~ sprintf("%.2f", prognose_2024_2070 / 10),
      prognose_2024_2070 <= 10000 ~ sprintf("%.2f", prognose_2024_2070 / 100),
      T ~ sprintf("%.2f", prognose_2024_2070 / 1000)
    )
  )  %>%
  separate(
    prognose_interval_95_percent,
    into = c('lower_95', 'upper_95'),
    sep = ' – ', convert = T
  ) %>%
  mutate(
    lower_95 = case_when(
      as.numeric(gsub(",", "",lower_95)) <= 1000 ~ sprintf("%.2f", as.numeric(gsub(",", "",lower_95))/10),
      as.numeric(gsub(",", "",lower_95)) <= 10000 ~ sprintf("%.2f", as.numeric(gsub(",", "",lower_95)) / 100),
      T ~ sprintf("%.2f", as.numeric(gsub(",", "",lower_95)) / 1000)
    ),
    upper_95 = case_when(
      as.numeric(gsub(",", "",upper_95)) <= 1000 ~ sprintf("%.2f", as.numeric(gsub(",", "",upper_95))/10),
      as.numeric(gsub(",", "",upper_95)) <= 10000 ~ sprintf("%.2f", as.numeric(gsub(",", "",upper_95)) / 100),
      T ~ sprintf("%.2f", as.numeric(gsub(",", "",upper_95)) / 1000)
    )
  ) %>% mutate_at(vars("waarneming", "prognose_2024_2070",
                       "lower_95", "upper_95"), function (x) as.numeric(gsub(",", "",x)))

## Visualiseer ----
static_plot <- ggplot(cbs_bevolking) +
  # waarneming
  geom_line(aes(x = jaar, y = waarneming), color = knvb_cols$donkeroranje_nieuw) +
  geom_ribbon(
    aes(ymin = lower_95, ymax = upper_95, x = jaar),
    alpha = 0.2,
    fill = knvb_cols$mist_white_core
  ) +
  geom_line(
    aes(x = jaar, y = prognose_2024_2070),
    color = knvb_cols$donkeroranje_nieuw_1,
    linetype = 2
  ) +
  scale_y_continuous(limits = c(0, 25), name = "Bevolking op 1 januari [x Mln]") +
  scale_x_continuous(
    limits = c(1980, 2070),
    breaks = seq(1980, 2070, 10),
    name  = NULL
  ) +
  geom_vline(aes(xintercept = 2025),
             linetype = 2,
             color = knvb_cols$`donker grijs`) +
  ret + theme(panel.background = element_blank()) + 
  geom_point(aes(x = jaar, y = waarneming),
             color = knvb_cols$donkeroranje_nieuw_1, size = 1.5)

animated_plot <-
  static_plot +
  transition_reveal(jaar)

animate(
  animated_plot,
  fps = 50,
  nframes = 250,
  width = 16,
  height = 9,
  res = 300,
  units = "in",
  dpi = 300,
  renderer = gifski_renderer(file = "cbs_bevolking.gif", loop = FALSE), 
  bg = 'transparent'
)

# B2. Football Benchmark Deloitte. ---------------------------------------------

deloitte_money <- read_csv(paste0(folder_path, "/ruw/deloitte-money-league.csv")) %>%
  janitor::clean_names() %>% filter(season >= 2000)

ani_plot <- ggplot(deloitte_money) + geom_bar(aes(x = as.factor(season),
                                      y = revenue_eur_million, group = club,
                                      fill = country),
                                  stat = "identity") + ret +
  scale_y_continuous(
    name = "Omzet top-20 clubs [in Mln]",
    label = scales::dollar_format(
      prefix = "€",
      suffix = "",
      big.mark = "."
    )
  ) +
  scale_x_discrete(labels = function(x) id_to_season(as.numeric(x)),
                   name = "Seizoen") +
  scale_fill_discrete(name = "Competitie") + transition_layers(
    layer_length = 1, transition_length = 2) +
  enter_drift(x_mod = 0, y_mod = -max(deloitte_money$revenue_eur_million))
  
