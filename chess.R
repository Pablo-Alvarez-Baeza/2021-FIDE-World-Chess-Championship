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
         result_text = case_when(result == "1/2-1/2" ~ "|",
                                 result == "1-0" ~ "⚪",
                                 result == "0-1" ~ "⚫") ,
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
  

# Basic plot --------------------------------------------------------------
plot <- moves |> 
ggplot(aes(move_dist_x_sep, move_dist_y, label = move, color = move_color)) +
  geom_text(aes(fontface = ifelse(move_capture == TRUE, 2, 1)),
            hjust = 0,
            size = 1.8,
            show.legend = FALSE,
            ) +
  labs(x = NULL,
       y = NULL) +
  scale_x_continuous(limits = c(1, 4.2),
                     breaks = seq(1, 4.2, by = 0.1))
  
plot +
  coord_cartesian(clip = "off") +
  annotate("segment", x = 1, y = 0, xend = 1.2, yend = 0, color = "black") +
  annotate("segment", x = 1.3, y = 0, xend = 1.5, yend = 0, color = "black") +
  annotate("segment", x = 1.6, y = 0, xend = 1.8, yend = 0, color = "black") +
  annotate("segment", x = 1.9, y = 0, xend = 2.1, yend = 0, color = "black") +
  annotate("segment", x = 2.2, y = 0, xend = 2.4, yend = 0, color = "black") +
  annotate("segment", x = 2.5, y = 0, xend = 2.7, yend = 0, color = "black") +
  annotate("segment", x = 2.8, y = 0, xend = 3, yend = 0, color = "black") +
  annotate("segment", x = 3.1, y = 0, xend = 3.3, yend = 0, color = "black") +
  annotate("segment", x = 3.4, y = 0, xend = 3.6, yend = 0, color = "black") +
  annotate("segment", x = 3.7, y = 0, xend = 3.9, yend = 0, color = "black") +
  annotate("segment", x = 4, y = 0, xend = 4.2, yend = 0, color = "black") +
  scale_color_manual(values = c("white", "black")) +
  theme_void(base_family = "Lato") +
  theme(
    plot.margin = margin(20, 40, 20, 30),
    panel.background = element_rect(color = "grey50", fill = "grey50")
  ) +
  annotate("text", x = 1.1, y = -2, label = df_text$plot_label[1],
           size = 2,
           family = "Lato",
           fontface = "bold") +
  annotate("text", x = 1.4, y = -2, label = df_text$plot_label[2],
           size = 2,
           family = "Lato",
           fontface = "bold") +
  annotate("text", x = 1.7, y = -2, label = df_text$plot_label[3],
           size = 2,
           family = "Lato",
           fontface = "bold") +
  annotate("text", x = 2, y = -2, label = df_text$plot_label[4],
           size = 2,
           family = "Lato",
           fontface = "bold") +
  annotate("text", x = 2.3, y = -2, label = df_text$plot_label[5],
           size = 2,
           family = "Lato",
           fontface = "bold") +
  annotate("text", x = 2.6, y = -2, label = df_text$plot_label[6],
           size = 2,
           family = "Lato",
           fontface = "bold") +
  annotate("text", x = 2.9, y = -2, label = df_text$plot_label[7],
           size = 2,
           family = "Lato",
           fontface = "bold") +
  annotate("text", x = 3.2, y = -2, label = df_text$plot_label[8],
           size = 2,
           family = "Lato",
           fontface = "bold") +
  annotate("text", x = 3.5, y = -2, label = df_text$plot_label[9],
           size = 2,
           family = "Lato",
           fontface = "bold") +
  annotate("text", x = 3.8, y = -2, label = df_text$plot_label[10],
           size = 2,
           family = "Lato",
           fontface = "bold") +
  annotate("text", x = 4.1, y = -2, label = df_text$plot_label[11],
           size = 2,
           family = "Lato",
           fontface = "bold")
  
  
  
  ggsave("delete.png", width = 10, height = 20, units = "in", dpi = 320)
  
  #  Max n moves 
  moves |> 
    slice_max(move_n, n = 1) |> 
    select(move_n)
  
  scale_color_manual(values = c("white", "black")) +
  labs(x = NULL,
       y = NULL) +
  coord_cartesian(clip = "off") +
  theme(
    panel.grid = element_blank(),
    plot.background = element_rect(color = "grey50", fill = "grey50"),
    panel.background = element_rect(color = "grey50", fill = "grey50"),
    plot.margin = margin(c(20, 40, 20, 40)),
    axis.ticks = element_blank(),
    axis.text = element_blank(),
    legend.position = "none"
  )

rep(1:87, times = 1, each = 2, length.out = nrow(data_char))

moves |> View()

test |> 
  mutate(move_n = rep(1:n(moves), times = 1, each = 2)) |> View()

         data_char$move_n <- factor(rep(1:87, times = 1, each = 2, length.out = nrow(data_char)))
         
         
moves |> 
  ggplot(aes(round, move, label = moves, color = color_m)) +
  geom_text() +
  scale_color_manual(values = c("black", "white")) 

moves|> View()

d |> View()


df <- data.frame(value = 1:23)

df |> 
  mutate(group = case_when(value < 4 ~ "A",
                           value >= 4 &  value < 18 ~ "B",
                           value >= 18 & value <= 23 ~ "C"))
TRUE ~ value)))

moves <- moves |> 
  group_by(game) |> 
  mutate(move_n = 1:n(),
         moves_total = n()) |> 
  ungroup() 


example <- data.frame(moves = c(1:50))
moves_total2 <- moves |> 
  distinct(game, moves_total) |> 
  select(moves_total) |> 
  as.vector()
         
test <- moves |> 
  filter(game %in% c(1))

data.frame(move = c(1:150)) |> 
  mutate(game = case_when(move < 50 ~ "1",
                            move >= 50, move < 140 ~ "2",
                            move >=140 ~ 3))

rep(c("white", "black"),  times = 5, each = 1)

test |> 
  mutate(color = rep(c("white", "black"), times = n())) |> View()

test |> 
  mutate(color = rep(c("white", "black"), each = 1, times = c(89, 116)) |> View()
         
         moves |> 
  mutate(moves_color = rep(c("White", "Black"), each = 2, times = c(89, 116, 81, 65, 85, 271, 81, 91, 78, 81, 98)))
moves_color rep(c("White", "Black"), each = 2, times = c(10, 20, 30))) |> View()


df |> 
  mutate(movetext_cleaned = strsplit(movetext, "[^1-9.a-zA-Z]"),
         movetext_cleaned = case_when(movetext == "O" ~ "O-O",
                                      TRUE ~ movetext))
,
         movetext_cleaned = str_replace_all(movetext, "^[1-9].", " ")) |> View()
pgn |> View()
pgn |> colnames()



text <- "1. e4 e5 2. Nf3 Nf6 3. Nxe5 d6 4. Nd3 Nxe4 5. Qe2 Qe7 6. Nf4 Nf6 7. d4 Nc6 8. c3 d5 9. Nd2 Nd8 10. Nf3 Qxe2+ 11. Bxe2 Bd6 12. O-O O-O 13. Bd3 Re8 14. Re1 Rxe1+ 15. Nxe1 Ne6 16. Nxe6 Bxe6 17. g3 g6 18. Ng2 Re8 19. f3 Nh5 20. Kf2 c6 21. g4 Ng7 22. Bf4 Bxf4 23. Nxf4 g5 24. Ne2 f5 25. h3 Kf7 26. Rh1 h6 27. f4 fxg4 28. hxg4 Bxg4 29. Rxh6 Bf5 30. Bxf5 Nxf5 31. Rh7+ Ng7 32. fxg5 Kg6 33. Rh3 Kxg5 34. Rg3+ Kf6 35. Rf3+ Ke7 36. Nf4 Kd6 37. Ng6 Re6 38. Ne5 Ne8 39. Rf7 Rf6+ 40. Rxf6+ Nxf6 41. Ke3"
text2 <- c("10. Nf3 Qxe2+ 11. Bxe2 Bd6 12. O-O")
text_outcome <- strsplit(text, "[^1-9.a-zA-Z]")[[1]]

str_remove_all(text, "[1-9]\\. ")[[1]]
str_remove_all(text2, "[[1-9]\\. ][0-9]\\. ")[[1]] 
str_extract_all(text, "[A-Za-z]\\S+")[[1]]


text_outcome |> 
  as.data.frame() |> 
  mutate(text_cleaned = case_when(text_outcome == "O" ~ "O-O",
                       TRUE ~ text_outcome),
         text_cleaned2 = str_replace_all(text_cleaned, "^[:digit:]\\.", " ")) |> 
  filter(!text_cleaned2 == "", !text_cleaned2 == ".") |> View()

text_outcome$text_cleaned2 

text_outcome |> pull()
df <- pgn[1,8]

df |> View()
str_trim(df) |> View()
stri_replace_all_fixed(df, "[1.-9.]", "")
str_replace_all(df, " ")


data_char <- strsplit(df,"[^0-9.a-zA-Z]")[[1]]

data_char  <- str_subset(data_char, "[a-zA-Z]") |> 
  as.data.frame() |> 
  rename(move = starts_with("str"))


df_char <- df[]
strsplit(Movetext,"[^1-9.a-zA-Z]")[[1]]

df |> 
  select(Movetext) |> 
  mutate(moves = strsplit(Movetext, ", ")) 
  as.data.frame() |> View()
  
\
  str_remove("[^1-9]")
  
pgn |> View()

data_raw <- read_excel("Nepomniachtchi-Carlsen_1.xls")

data <- data_raw[12, 1] |> pull()

# Split strings that start with a number followed by a letter
data_char <- strsplit(data,"[^1-9.a-zA-Z]")[[1]]

# Extract only alpha characters
data_char  <- str_subset(data_char, "[a-zA-Z]") |> 
  as.data.frame() |> 
  rename(move = starts_with("str"))
 

rep(1:87, times = 1, each = 2, length.out = nrow(data_char))

# Using length.out for succesfully applying rep with uneven observations
data_char$move_n <- factor(rep(1:87, times = 1, each = 2, length.out = nrow(data_char)))
data_char$move_color <- factor(rep(c("white", "black"), length.out = nrow(data_char)),
                               levels = c("white", "black"),
                               ordered = TRUE)
data_char$move_dist_y <- rep(c(1, 1.05), length.out = nrow(data_char))
data_char <- data_char |> 
  mutate(move_capture = str_detect(move, "x"))
  
t <- moves |> 
  as.data.frame(moves) |> 
  mutate(move = factor(rep(1:3, times=1, each=2)),
         color_m = factor(rep(c("white", "black"), times = 3)),
         game = factor(rep(1:1.05, times = 3)))

rbind(t, row3 = apply(t, 2, paste0, collapse = "-"))

t |> 
  ggplot(aes(game, move, label = moves, color = color_m)) +
  geom_text() +
  scale_color_manual(values = c("black", "white")) 

data_char |> 
  ggplot(aes(move_dist_y, move_n, label = move, color = move_color)) +
  geom_text(aes(fontface = ifelse(move_capture == TRUE, 2, 1)),
            hjust = 0,
            size = 2) +
  annotate("segment", x = 1, y = 0, xend = 1.1, yend = 0, color = "black") +
  geom_richtext(x = 1, y = 0,
                fill = "grey50",
                label.color = NA,
                label = "<span style='color:white'>Carlsen</span> <span style='color:black'>Nepo</span><br><span style='color:grey'>Game 1</span>",
                label.padding = unit(c(0.25, 0, 0, 0), "lines"),
                size = 2,
                color = "grey66",
                hjust = 0,
                family = "Lato") +
  scale_x_continuous(limits = c(1, 2),
                     breaks = seq(1, 2, by = 1)) +
  scale_color_manual(values = c("white", "black")) +
  labs(x = NULL,
       y = NULL) +
  coord_cartesian(clip = "off") +
  theme(
    panel.grid = element_blank(),
    plot.background = element_rect(color = "grey50", fill = "grey50"),
    panel.background = element_rect(color = "grey50", fill = "grey50"),
    plot.margin = margin(c(20, 40, 20, 40)),
    axis.ticks = element_blank(),
    axis.text = element_blank(),
    legend.position = "none"
  )
  
