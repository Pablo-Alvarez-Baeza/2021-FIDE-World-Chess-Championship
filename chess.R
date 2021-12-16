library(pacman)
p_load(tidyverse, lubridate, bigchess, janitor, reshape2, ggbeeswarm, ggtext, showtext, sysfonts, stringi, readxl, stringr)

font_add_google("Lato")

df_raw <- read.pgn("wch21.pgn") |> 
  clean_names()

df <- df_raw |> 
  select(date, round, white, black, result, movetext)

df_text <-  df |> 
  mutate(date = ymd(date),
         year = year(date),
         month = month(date, label = TRUE),
         day = day(date),
         date_text = paste(month, day, year),
         round_text = paste("Game", round),
         white_text = if_else(white == "Nepomniachtchi, Ian", "Nepo", "Carlsen"),
         black_text = if_else(black == "Nepomniachtchi, Ian", "Nepo", "Carlsen"),
         color_text = paste(white_text, black_text),
         result_text = case_when(result == "1/2-1/2" ~ "—",
                                 result == "1-0" ~ "|",
                                 result == "0-1" ~ "||") ,
         plot_label = paste(round_text, date_text, color_text, result_text, sep="\n")) |> 
  select(plot_label)
   
df <- df_raw |> 
  select(date, round, white, black, result, movetext) |> 
  mutate(round = is.numeric(round))

moves <- str_extract_all(df$movetext, "[A-Za-z]\\S+") |> 
  melt(moves, value.name = "move") |> 
  rename(round = L1) |> 
  as.data.frame() |> 
  group_by(round) |>  
  mutate(move_color = factor(if_else(row_number() %% 2 == 1, "white", "black"),
                             levels = c("white", "black"), ordered = TRUE),
         move_dist_x = if_else(row_number() %% 2 == 1, 1, 1.1),
         move_dist_y = as.numeric(rep(1:n(), each =2)[1:n()])) |> 
  ungroup() |> 
  mutate(move_capture = str_detect(move, "x"),
         move_dist_x_sep = as.numeric(case_when(round == 2 & move_dist_x == 1 ~ 1.3,
                               round == 2 & move_dist_x == 1.1 ~ 1.4,
                               round == 3 & move_dist_x == 1 ~ 1.6,
                               round == 3 & move_dist_x == 1.1 ~ 1.7,
                               round == 4 & move_dist_x == 1 ~ 1.9,
                               round == 4 & move_dist_x == 1.1 ~ 2,
                               round == 5 & move_dist_x == 1 ~ 2.2,
                               round == 5 & move_dist_x == 1.1 ~ 2.3,
                               round == 6 & move_dist_x == 1 ~ 2.5,
                               round == 6 & move_dist_x == 1.1 ~ 2.6,
                               round == 7 & move_dist_x == 1 ~ 2.8,
                               round == 7 & move_dist_x == 1.1 ~ 2.9,
                               round == 8 & move_dist_x == 1 ~ 3.1,
                               round == 8 & move_dist_x == 1.1 ~ 3.2,
                               round == 9 & move_dist_x == 1 ~ 3.4,
                               round == 9 & move_dist_x == 1.1 ~ 3.5,
                               round == 10 & move_dist_x == 1 ~ 3.7,
                               round == 10 & move_dist_x == 1.1 ~ 3.8,
                               round == 11 & move_dist_x == 1 ~ 4,
                               round == 11 & move_dist_x == 1.1 ~ 4.1,
                               TRUE ~ move_dist_x))) 
  
# Max mov in total = 136
moves |> 
  slice_max(move_dist_y, n = 1) |> View()

# Basic plot --------------------------------------------------------------
plot <- moves |> 
ggplot(aes(move_dist_x_sep, move_dist_y, label = move, color = move_color)) +
  geom_text(aes(fontface = ifelse(move_capture == TRUE, 2, 1)),
            hjust = 0.5,
            size = 1.8,
            show.legend = FALSE,
            ) +
  labs(x = NULL,
       y = NULL,
       title = "FIDE CHESS WORLD CHAMPIONSHIP",
       subtitle = "DUBAI 2021",
       caption = "Visualization by Pablo Alvarez") +
  scale_x_continuous(limits = c(0.95, 4.2),
                     breaks = seq(0.95, 4.2, by = 0.1)) 
  

# Final plot --------------------------------------------------------------


plot +
  coord_cartesian(clip = "off") +
  # Adding horizontal bars below each round
  annotate("segment", x = 0.950, y = 0, xend = 1.150, yend = 0, color = "black") +
  annotate("segment", x = 1.250, y = 0, xend = 1.450, yend = 0, color = "black") +
  annotate("segment", x = 1.550, y = 0, xend = 1.750, yend = 0, color = "black") +
  annotate("segment", x = 1.850, y = 0, xend = 2.050, yend = 0, color = "black") +
  annotate("segment", x = 2.150, y = 0, xend = 2.350, yend = 0, color = "black") +
  annotate("segment", x = 2.450, y = 0, xend = 2.650, yend = 0, color = "black") +
  annotate("segment", x = 2.750, y = 0, xend = 2.950, yend = 0, color = "black") +
  annotate("segment", x = 3.050, y = 0, xend = 3.250, yend = 0, color = "black") +
  annotate("segment", x = 3.350, y = 0, xend = 3.550, yend = 0, color = "black") +
  annotate("segment", x = 3.650, y = 0, xend = 3.850, yend = 0, color = "black") +
  annotate("segment", x = 3.950, y = 0, xend = 4.150, yend = 0, color = "black") +
  # Annotating game details
  # Game 1 Nov 26 2021 Nepo Carlsen —
  annotate(geom = "richtext",
           label = "**Round 1**<br>Nov 26, 2021<br><span style='color:white'>Nepo</span>
           <span>Carlsen</span><br>**—**",
           x = 1.050, y = -2.5, fill = NA, label.color = NA, family = "Lato", size = 2,
           lineheight = 1.5) +
  # Game 2 Nov 27 2021 Carlsen Nepo —
  annotate(geom = "richtext",
           label = "**Round 2**<br>Nov 27, 2021<br><span style='color:white'>Carlsen</span>
           <span>Nepo</span><br>**—**",
           x = 1.350, y = -2.5, fill = NA, label.color = NA, family = "Lato", size = 2,
           lineheight = 1.5) +
  # Game 3 Nov 28 2021 Nepo Carlsen —
  annotate(geom = "richtext",
           label = "**Round 3**<br>Nov 28, 2021<br><span style='color:white'>Nepo</span>
           <span>Carlsen</span><br>**—**",
           x = 1.650, y = -2.5, fill = NA, label.color = NA, family = "Lato", size = 2,
           lineheight = 1.5) +
  # Game 4 Nov 30 2021 Carlsen Nepo —
  annotate(geom = "richtext",
           label = "**Round 4**<br>Nov 27, 2021<br><span style='color:white'>Carlsen</span>
           <span>Nepo</span><br>**—**",
           x = 1.950, y = -2.5, fill = NA, label.color = NA, family = "Lato", size = 2,
           lineheight = 1.5) +
  # Game 5 Dec 1 2021 Nepo Carlsen —
  annotate(geom = "richtext",
           label = "**Round 5**<br>Nov 26, 2021<br><span style='color:white'>Nepo</span>
           <span>Carlsen</span><br>**—**",
           x = 2.250, y = -2.5, fill = NA, label.color = NA, family = "Lato", size = 2,
           lineheight = 1.5) +
  # Game 6 Dec 3 2021 Carlsen Nepo |
  annotate(geom = "richtext",
           label = "**Round 6**<br>Dec 3, 2021<br><span style='color:white'>**Carlsen**</span>
           <span>Nepo</span><br><span style='color:white'>**|**<span>",
           x = 2.550, y = -2.5, fill = NA, label.color = NA, family = "Lato", size = 2,
           lineheight = 1.5) +
  # Game 7 Dec 4 2021 Nepo Carlsen —
  annotate(geom = "richtext",
           label = "**Round 7**<br>Dec 4, 2021<br><span style='color:white'>Nepo</span>
           <span>**Carlsen**</span><br>**—**",
           x = 2.850, y = -2.5, fill = NA, label.color = NA, family = "Lato", size = 2,
           lineheight = 1.5) +
  # Game 8 Dec 5 2021 Carlsen Nepo —
  annotate(geom = "richtext",
           label = "**Round 8**<br>Dec 8, 2021<br><span style='color:white'>Carlsen</span>
           <span>Nepo</span><br>**—**",
           x = 3.150, y = -2.5, fill = NA, label.color = NA, family = "Lato", size = 2,
           lineheight = 1.5) +
  # Game 9 Dec 7 2021 Nepo Carlsen ||
  annotate(geom = "richtext",
           label = "**Round 9**<br>Dec 9, 2021<br><span style='color:white'>Nepo</span>
           <span>**Carlsen**</span><br><span style='color:black'>**|**<span>",
           x = 3.450, y = -2.5, fill = NA, label.color = NA, family = "Lato", size = 2,
           lineheight = 1.5) +
  # Game 10 Dec 8 2021 Carlsen Nepo —
  annotate(geom = "richtext",
           label = "**Round 10**<br>Dec 8, 2021<br><span style='color:white'>Carlsen</span>
           <span>Nepo</span><br>**—**",
           x = 3.750, y = -2.5, fill = NA, label.color = NA, family = "Lato", size = 2,
           lineheight = 1.5) +
  # Game 11 Dec 10 2021 Nepo Carlsen —
  annotate(geom = "richtext",
           label = "**Round 11**<br>Dec 10, 2021<br><span style='color:white'>Nepo</span>
           <span>**Carlsen**</span><br><span style='color:black'>**|**<span>",
           x = 4.050, y = -2.5, fill = NA, label.color = NA, family = "Lato", size = 2,
           lineheight = 1.5) +
  # Theme
    scale_color_manual(values = c("white", "black")) +
    theme_void(base_family = "Lato") +
    theme(
      plot.margin = margin(1, 1, 1, 1),
      panel.background = element_rect(color = "grey50", fill = "grey50"),
      plot.background = element_rect(color = "grey50", fill = "grey50"),
      # Customize title appearance
      plot.title = element_markdown(
        color = "black", 
        size = 20, 
        face = "bold",
        hjust = 0,
        margin = margin(t = 40, l = 20)
      ),
      # Customize subtitle appearance
      plot.subtitle = element_markdown(
        color = "black", 
        size = 14, 
        hjust = 0,
        margin = margin(t = 5, l = 20)
      ),
      # Customize caption appearance
      plot.caption = element_markdown(
        color = "black", 
        size = 6.5,
        hjust = 0.5,
        margin = margin(t = -20, b = 30),
        family = "Lato"
      )
    )
 


ggsave("fide_chess.png", width = 10, height = 20, units = "in", dpi = 320)
 