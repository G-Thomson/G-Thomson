# https://github.com/sharlagelfand/mylifeinmonths
library(tidyverse)
library(waffle)
library(hrbrthemes)

# Create the data-----
life_data <- expand_grid(
  month = month.name,
  year = 1990:2020) %>%
  mutate(month = fct_relevel(month, month.name)) %>%
  arrange(year, month) %>%
  group_by(year) %>%
  mutate(month_number = row_number()) %>%
  ungroup() #%>%
  #filter(!(year == 1990 & month_number < 6))

# Add "eras" to be coloured ----
life_data <- life_data %>%
  rowwise() %>%
  mutate(year_month = paste0(c(year, month_number), collapse = ",")) %>%
  ungroup() %>%
  mutate(era = case_when(
    year_month == "1990,1" ~ "blank",
    year_month == "1990,6" ~ "Childhood",
    year_month == "1995,6" ~ "Primary School",
    year_month == "2002,2" ~ "High School (High School)",
    year_month == "2004,1" ~ "High School",
    year_month == "2009,3" ~ "Undergraduate",
    year_month == "2015,3" ~ "Grad School",
    year_month == "2019,1" ~ "Hiking",
    year_month == "2019,2" ~ "Inbetween",
    year_month == "2019,5" ~ "Traveling",
    year_month == "2019,7" ~ "Postdoc"
  )) %>%
  fill(era) %>%
  mutate(era = fct_inorder(era))

# Waffle chart-----
life_in_months_base <- life_data %>%
  count(era) %>% ## the count of each era is the number of months in that era
  ggplot(aes(fill = era, values = n)) +
  geom_waffle(color = "#FFFFFF", n_rows = 12, size = 1, flip = FALSE) + ## make each row a year/12 months
  # scale_fill_manual(name = "", values = c("#EF476F", "#FCA311", "#FFD166", "#0EAD69", "#4ECDC4", "#118AB2")) + ## assign colors to the eras
  coord_equal() +
  # scale_y_continuous(breaks = c(1, 4, 7, 10), labels = c("June", "September", "December", "March")) +
  scale_x_continuous(limits = c(-0.5, 37.5)) +
  scale_y_continuous(limits = c(-2.5, 15)) +
  scale_fill_manual(values = c("#00000000","#00AFBB", "#33B191", "#E62946", "#E9AC00", 
                               "#F76505", "#E9AC00", "#00AFBB", "#E9AC00", "#00AFBB", "#E62946")) +
  labs(y = NULL, x = NULL) +
  theme_ipsum(grid = "") +
  theme(
    text = element_text(family = "IBM Plex Mono", face = "italic"),
    legend.position = "none",
    #plot.background = element_rect(fill = "#F7F7F7", color = "#F7F7F7"),
    axis.text.x = element_blank(),
    axis.text.y = element_blank()
  )

annotation_base_size <- 10
annotation_lineheight <- 1
initial_annotations_font_family <- "IBM Plex Mono"
initial_annotations_colour <- "#666666"

life_in_months_initial_annotations <- life_in_months_base +
  annotate("text", x = 0, y = 6.5, label = "1 year", angle = 90, family = initial_annotations_font_family, fontface = "italic", size = annotation_base_size, colour = initial_annotations_colour) +
  geom_segment(aes(x = 0, xend = 0, y = 1, yend = 5), colour = initial_annotations_colour) +
  geom_segment(aes(x = -0.25, xend = 0.25, y = 1, yend = 1), colour = initial_annotations_colour) +
  geom_segment(aes(x = 0, xend = 0, y = 8, yend = 12), colour = initial_annotations_colour) +
  geom_segment(aes(x = -0.25, xend = 0.25, y = 12, yend = 12), colour = initial_annotations_colour) +
  annotate("text", x = 1, y = 14.5, label = "1 square = 1 month", family = initial_annotations_font_family, fontface = "italic", size = annotation_base_size * 0.8, lineheight = annotation_lineheight, hjust = 0.4, colour = initial_annotations_colour) +
  geom_curve(aes(x = 0, xend = 1, y = 14, yend = 12), arrow = arrow(length = unit(0.0175, "npc")), colour = initial_annotations_colour) +
  annotate("text", x = 0.5, y = 0, label = "age", family = initial_annotations_font_family, fontface = "italic", hjust = 0, size = annotation_base_size, colour = initial_annotations_colour) +
  geom_segment(aes(x = 2, xend = 4, y = 0, yend = 0), arrow = arrow(length = unit(0.0175, "npc")), colour = initial_annotations_colour) +
  annotate("text", x = 32, y = 6.5, label = "my life\nin months", hjust = 0, family = "Azo Sans", fontface = "bold", lineheight = 1, size = annotation_base_size * 1.75)

role_annotations_font_family <- "Nimbus Mono PS"

role_annotations_y <- -0.25
roles_size <- annotation_base_size * 1.5

life_in_months_role_annotations <-  life_in_months_initial_annotations +
  # annotate("text", x = 4, y = role_annotations_y + 13.5, label = "childhood", family = role_annotations_font_family, size = roles_size, colour = "#00AFBB") +
  annotate("text", x = 9, y = role_annotations_y, label = "primary school", family = role_annotations_font_family, size = roles_size, colour = "#33B191") +
  
  annotate("text", x = 9, y = role_annotations_y - 1.25, label = "highschool", family = role_annotations_font_family, size = roles_size, colour = "#E62946") +
  annotate("text", x = 9, y = role_annotations_y - 2.25, label = "(leeds, uk)", family = role_annotations_font_family, size = roles_size * 0.75, colour = "#E62946") +
  geom_curve(aes(x = 12.5, xend = 14, y = -1.5, yend = 0.35), curvature = 0.4, arrow = arrow(length = unit(0.0175, "npc")), colour = "#E62946") +
  
  annotate("text", x = 18, y = role_annotations_y, label = "highschool", family = role_annotations_font_family, size = roles_size, colour = "#E9AC00") +
  annotate("text", x = 19, y = role_annotations_y - 1.25, label = "undergrad", family = role_annotations_font_family, size = roles_size, colour = "#F76505") +
  annotate("text", x = 19, y = role_annotations_y - 2.25, label = "(bio & english)", family = role_annotations_font_family, size = roles_size * 0.75, colour = "#F76505") +
  geom_curve(aes(x = 22, xend = 22, y = -1.5, yend = 0.35), curvature = 0.4, arrow = arrow(length = unit(0.0175, "npc")), colour = "#F76505") +
  annotate("text", x = 28, y = role_annotations_y, label = "PhD", family = role_annotations_font_family, size = roles_size, colour = "#E9AC00") +
  annotate("text", x = 28, y = role_annotations_y - 1, label = "(plant bio)", family = role_annotations_font_family, size = roles_size * 0.75, colour = "#E9AC00") +
  annotate("text", x = 34, y = 1.5, label = "Postdoc", family = role_annotations_font_family, lineheight = annotation_lineheight - 0.25, size = roles_size, colour = "#E62946") +
  # annotate("text", x = 35, y = 0.5, label = "(ct, usa)", family = role_annotations_font_family, size = roles_size * 0.75, colour = "#E62946") +
  geom_curve(aes(x = 32.75, xend = 31, y = 0.5, yend = 0.35), curvature = -0.3, arrow = arrow(length = unit(0.0175, "npc")), colour = "#E62946")

location_colour <- "#8c8c8c"

life_in_months_final <- life_in_months_role_annotations +
  annotate("text", x = 6, y = 13.1, label = "auckland, nz", family = role_annotations_font_family, size = annotation_base_size, colour = location_colour) +
  geom_segment(aes(x = 1, xend = 3, y = 13, yend = 13), colour = location_colour) +
  geom_segment(aes(x = 9, xend = 12, y = 13, yend = 13), colour = location_colour) +
  geom_segment(aes(x = 1, xend = 1, y = 12.75, yend = 13.25), colour = location_colour) +
  geom_segment(aes(x = 12, xend = 12, y = 12.75, yend = 13.25), colour = location_colour) +

  annotate("text", x = 22, y = 13.1, label = "auckland, nz", family = role_annotations_font_family, size = annotation_base_size, colour = location_colour) +
  geom_segment(aes(x = 15, xend = 19, y = 13, yend = 13), colour = location_colour) +
  geom_segment(aes(x = 25, xend = 29, y = 13, yend = 13), colour = location_colour) +
  geom_segment(aes(x = 15, xend = 15, y = 12.75, yend = 13.25), colour = location_colour) +
  geom_segment(aes(x = 29, xend = 29, y = 12.75, yend = 13.25), colour = location_colour) +

  annotate("text", x = 21, y = 14, label = "moved to leeds, uk", family = role_annotations_font_family, size = annotation_base_size, lineheight = annotation_lineheight, hjust = 0.75, colour = location_colour) +
  geom_curve(aes(x = 15, xend = 13, y = 14, yend = 12.6), curvature = 0.4, arrow = arrow(length = unit(0.0175, "npc")), colour = location_colour) +
  annotate("text", x = 30, y = 14.75, label = "moved to new haven, usa", family = role_annotations_font_family, size = annotation_base_size, lineheight = annotation_lineheight, colour = location_colour) +
  geom_curve(aes(x = 33, xend = 32, y = 14, yend = 12.6), curvature = -0.2, arrow = arrow(length = unit(0.0175, "npc")), colour = location_colour)

ggsave("life_in_months.png", plot = life_in_months_final, device = "png", type = "cairo", width = 25, height = 15, dpi = 300)
