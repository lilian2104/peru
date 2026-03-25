library(tidyverse)
library(provenance)
library(readxl)
library(deeptime)
library(showtext)
library(ggtext)

font_add_google(name = "Kalam", family = "Kalam")
font_add(family = "InkFree", regular = "provenance/fonts/Inkfree.ttf")
showtext_auto()
u_pb <- read_csv("data/U-Pb.csv") |>
  janitor::clean_names() |>
  rownames_to_column() |>
  pivot_longer(-rowname, names_to = "sample", values_to = "age")

ar9_colours <- c("#bfd200", "#046c9a", "#972D15")
plot_title <- glue::glue("Age Density Distribution for Samples <i style= 'font-family: Kalam; color:{ar9_colours[1]}; font-size: 60px;'> AR 16</i>, ",
                         "<i style='font-family: Kalam; color:{ar9_colours[2]}; font-size: 60px;'>E03</i>, ",
                         "and <b><i style = 'font-family:Kalam; color:{ar9_colours[3]}; font-size: 60px;'>E06</i></b>")


u_pb |>
  filter(sample %in% c("ar9", "ar16", "eo6", "eo3", "dc_04_5_2")) |>
  #filter(sample %in% c("ar9", "dc_04_5_2")) |>
  filter(sample %in% c("ar16", "eo6", "eo3")) |>
  ggplot(aes(age, colour = sample, fill = sample)) +
    geom_density(alpha = 0.1, size = 2, show.legend = F) +
  geom_rug(show.legend = F) +
  coord_geo(dat = "eras") +
  labs(title = plot_title,
       x = "Age (Myr)") +
  scale_fill_manual(values = ar9_colours) +
  scale_colour_manual(values = ar9_colours) +
  theme_minimal() +
  theme(plot.title = element_markdown(),
        text = element_text(family = "InkFree", size = 40),
        axis.text.y = element_blank(),
        axis.title.y = element_blank())


ar16_colours <- c("#046c9a", "#972D15")
plot_title <- glue::glue("Age Density Distribution for Samples <i style= 'font-family: Kalam; color:{ar16_colours[1]}; font-size: 60px;'> AR 16</i>, ",
                         "and <b><i style = 'font-family:Kalam; color:{ar16_colours[2]}; font-size: 60px;'>DC 04-5-2</i></b>")


u_pb |>
  filter(sample %in% c("ar9", "ar16", "eo6", "eo3", "dc_04_5_2")) |>
  filter(sample %in% c("ar9", "dc_04_5_2")) |>
  #filter(sample %in% c("ar16", "eo6", "eo3")) |>
  ggplot(aes(age, colour = sample, fill = sample)) +
  geom_density(alpha = 0.1, size = 2, show.legend = F) +
  geom_rug(show.legend = F) +
  coord_geo(dat = "eras") +
  labs(title = plot_title,
       x = "Age (Myr)") +
  scale_fill_manual(values = ar16_colours) +
  scale_colour_manual(values = ar16_colours) +
  theme_minimal() +
  theme(plot.title = element_markdown(),
        text = element_text(family = "InkFree", size = 40),
        axis.text.y = element_blank(),
        axis.title.y = element_blank())

my_file <- "provenance/data/peru/U-Pb in other literature.csv.xlsx"
my_sheets <- excel_sheets(my_file)
z <- read_excel(my_file, my_sheets[1])
mds_upb <-