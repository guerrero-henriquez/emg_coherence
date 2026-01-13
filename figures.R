# FIG 2A (beta_mean) — estilo tipo fc_hipotesis:
# barras = media ± SD, puntos individuales + líneas por sujeto (perfectamente conectadas),
# bracket SOLO NN vs 48-h HH (p=0.029) + Δ(48-h HH – NN)=... (solo significativo).

library(readxl)
library(dplyr)
library(tidyr)
library(stringr)
library(ggplot2)

# ----------------------------
# 0) Ruta del Excel
# ----------------------------
file_xlsx <- "C:/Users/jguer/Dropbox/linea_investigacion/borradores/coherencia/estadistica.xlsx"

# ----------------------------
# 1) Helpers (coma decimal + asterisco SPSS)
# ----------------------------
to_num_comma <- function(x){
  x <- as.character(x)
  x <- str_replace_all(x, "\\*", "")
  x <- str_replace(x, "^-,", "-0,")
  x <- str_replace(x, "^,", "0,")
  as.numeric(str_replace_all(x, ",", "."))
}
fmt_p <- function(p){
  if (is.na(p)) return("")
  if (p < 0.001) "p<0.001" else paste0("p=", formatC(p, format="f", digits=3))
}
fmt_delta <- function(x, digits = 3){
  out <- formatC(x, format="f", digits=digits)
  str_replace(out, "\\.", ",")
}
y_labels <- function(x){
  str_replace(format(x, nsmall = 2, trim = TRUE), "\\.", ",")
}

# ----------------------------
# 2) Datos individuales (sheet: ind) -> beta_mean
# ----------------------------
df <- read_excel(file_xlsx, sheet = "ind") %>%
  mutate(
    Condition = case_when(
      condition == 1 ~ "NN",
      condition == 2 ~ "acute HH",
      condition == 3 ~ "48-h HH",
      TRUE ~ as.character(condition)
    ),
    Condition = factor(Condition, levels = c("NN", "acute HH", "48-h HH"))
  ) %>%
  group_by(Condition) %>%
  mutate(Subject = row_number()) %>%   # asume mismo orden de sujetos por condición (1..10)
  ungroup() %>%
  transmute(Condition, Subject, y = beta_mean)

# ----------------------------
# 2.1) FIX: jitter constante por sujeto (líneas y puntos calzan perfecto)
# ----------------------------
set.seed(123)  # reproducible
df <- df %>% mutate(x = as.numeric(Condition))    # 1..3

jit <- df %>%
  distinct(Subject) %>%
  mutate(j = runif(n(), -0.06, 0.06))             # amplitud del jitter

df <- df %>%
  left_join(jit, by = "Subject") %>%
  mutate(xj = x + j)

# ----------------------------
# 3) Resumen (media ± SD)
# ----------------------------
sumdf <- df %>%
  group_by(Condition) %>%
  summarise(
    x    = unique(x),
    mean = mean(y, na.rm = TRUE),
    sd   = sd(y, na.rm = TRUE),
    .groups = "drop"
  )

# ----------------------------
# 4) Comparaciones múltiples (sheet: comparaciones multiples)
#    IMPORTANTE: usamos tu evidencia textual: SOLO NN vs 48-h HH es significativo (p=0.029)
# ----------------------------
comp <- read_excel(file_xlsx, sheet = "comparaciones multiples")
names(comp) <- c("measure","I","J","mean_diff","se","p","ci_low","ci_high")[seq_len(ncol(comp))]

comp_beta <- comp %>%
  mutate(measure = as.character(measure),
         I = as.character(I),
         J = as.character(J)) %>%
  fill(measure) %>%
  filter(measure == "beta_media") %>%
  mutate(p = to_num_comma(p)) %>%
  filter(!is.na(I), !is.na(J)) %>%
  distinct(I, J, .keep_all = TRUE)

# ---- FORZAR SOLO el contraste significativo reportado ----
# (evita que SPSS te traiga pares duplicados o direcciones raras)
comp_sig <- tibble(I = "NN", J = "48-h HH", p = 0.029)

# ----------------------------
# 5) Estética (académica)
# ----------------------------
pal <- c(
  "NN" = "#3B74A6",
  "acute HH" = "#9E9E9E",
  "48-h HH" = "#6F6F6F"
)

# ----------------------------
# 6) Plot base
# ----------------------------
p <- ggplot() +
  geom_col(
    data = sumdf,
    aes(x = x, y = mean, fill = Condition),
    width = 0.72,
    color = "black",
    linewidth = 0.7,
    alpha = 0.95
  ) +
  geom_errorbar(
    data = sumdf,
    aes(x = x, ymin = mean - sd, ymax = mean + sd),
    width = 0.16,
    linewidth = 0.9,
    color = "black"
  ) +
  geom_line(
    data = df,
    aes(x = xj, y = y, group = Subject),
    color = "grey35",
    linewidth = 0.6,
    alpha = 0.7
  ) +
  geom_point(
    data = df,
    aes(x = xj, y = y),
    shape = 21,
    fill = "white",
    color = "black",
    size = 3,
    stroke = 0.8,
    alpha = 0.95
  ) +
  scale_fill_manual(values = pal, guide = "none") +
  scale_x_continuous(breaks = 1:3, labels = c("NN", "acute HH", "48-h HH")) +
  scale_y_continuous(labels = y_labels) +
  labs(x = "Condition", y = "Mean coherence (a.u.)") +
  theme_bw(base_size = 20) +
  theme(
    panel.grid.major = element_line(color = "grey88", linewidth = 0.7),
    panel.grid.minor = element_line(color = "grey93", linewidth = 0.5),
    axis.title = element_text(size = 26),
    axis.text  = element_text(size = 20),
    axis.line  = element_line(linewidth = 1.1, color = "black")
  )

# ----------------------------
# 7) Bracket + p + Δ (SOLO NN vs 48-h HH)
#    p arriba del bracket; Δ abajo (un solo triángulo)
# ----------------------------
cond_x <- c("NN" = 1, "acute HH" = 2, "48-h HH" = 3)

y_top <- max(c(df$y, sumdf$mean + sumdf$sd), na.rm = TRUE)
yb    <- y_top * 1.10
step  <- y_top * 0.09

I <- comp_sig$I[1]
J <- comp_sig$J[1]
pval <- comp_sig$p[1]
x1 <- cond_x[[I]]
x2 <- cond_x[[J]]

mean_I <- sumdf$mean[sumdf$Condition == I]
mean_J <- sumdf$mean[sumdf$Condition == J]

# Δ(48-h HH – NN) como en tu redacción
delta <- mean_J - mean_I

p <- p +
  annotate("segment", x = x1, xend = x2, y = yb, yend = yb, linewidth = 0.9) +
  annotate("segment", x = x1, xend = x1, y = yb, yend = yb - step*0.25, linewidth = 0.9) +
  annotate("segment", x = x2, xend = x2, y = yb, yend = yb - step*0.25, linewidth = 0.9) +
  # p arriba
  annotate("text", x = (x1+x2)/2, y = yb + step*0.80, label = fmt_p(pval), size = 6) +
  # Δ abajo (triángulo único)
  annotate("text",
           x = (x1+x2)/2, y = yb - step*0.55,
           label = paste0("\u0394(48-h HH\u2013NN)=", fmt_delta(delta, 3)),
           size = 5.2)

p

# ----------------------------
# 8) Guardar (opcional)
# ----------------------------
# ggsave("Fig2A_beta_mean.png", p, width = 8.6, height = 6.0, dpi = 600)



######################################################################################################################################
# FIG 3A (theta_mean) — estilo tipo fc_hipotesis:
# barras = media ± SD, puntos + líneas por sujeto (alineadas),
# brackets SOLO NN vs acute HH (p=0.044) y NN vs 48-h HH (p=0.041),
# p ARRIBA del bracket y Δ ABAJO del bracket (sin superposición),
# con: bajar p del bracket inferior + subir Δ del bracket superior + más separación vertical.

library(readxl)
library(dplyr)
library(tidyr)
library(stringr)
library(ggplot2)

# ----------------------------
# 0) Ruta del Excel
# ----------------------------
file_xlsx <- "C:/Users/jguer/Dropbox/linea_investigacion/borradores/coherencia/estadistica.xlsx"

# ----------------------------
# 1) Helpers
# ----------------------------
to_num_comma <- function(x){
  x <- as.character(x)
  x <- str_replace_all(x, "\\*", "")
  x <- str_replace(x, "^-,", "-0,")
  x <- str_replace(x, "^,", "0,")
  as.numeric(str_replace_all(x, ",", "."))
}
fmt_delta <- function(x, digits = 3){
  out <- formatC(x, format="f", digits=digits)
  str_replace(out, "\\.", ",")
}
y_labels <- function(x){
  str_replace(format(x, nsmall = 2, trim = TRUE), "\\.", ",")
}

# ----------------------------
# 2) Datos individuales (sheet: ind) -> theta_mean
# ----------------------------
df <- read_excel(file_xlsx, sheet = "ind") %>%
  mutate(
    Condition = case_when(
      condition == 1 ~ "NN",
      condition == 2 ~ "acute HH",
      condition == 3 ~ "48-h HH",
      TRUE ~ as.character(condition)
    ),
    Condition = factor(Condition, levels = c("NN", "acute HH", "48-h HH"))
  ) %>%
  group_by(Condition) %>%
  mutate(Subject = row_number()) %>%
  ungroup() %>%
  transmute(Condition, Subject, y = theta_mean)

# ----------------------------
# 3) Jitter fijo por sujeto (alineación perfecta líneas/puntos)
# ----------------------------
set.seed(123)
df <- df %>% mutate(x = as.numeric(Condition))

jit <- df %>%
  distinct(Subject) %>%
  mutate(j = runif(n(), -0.06, 0.06))

df <- df %>%
  left_join(jit, by = "Subject") %>%
  mutate(xj = x + j)

# ----------------------------
# 4) Resumen (media ± SD)
# ----------------------------
sumdf <- df %>%
  group_by(Condition) %>%
  summarise(
    x    = unique(x),
    mean = mean(y, na.rm = TRUE),
    sd   = sd(y, na.rm = TRUE),
    .groups = "drop"
  )

# ----------------------------
# 5) Estética
# ----------------------------
pal <- c("NN"="#3B74A6","acute HH"="#9E9E9E","48-h HH"="#6F6F6F")

# ----------------------------
# 6) Plot base
# ----------------------------
p2 <- ggplot() +
  geom_col(
    data = sumdf,
    aes(x = x, y = mean, fill = Condition),
    width = 0.72,
    color = "black",
    linewidth = 0.7,
    alpha = 0.95
  ) +
  geom_errorbar(
    data = sumdf,
    aes(x = x, ymin = mean - sd, ymax = mean + sd),
    width = 0.16,
    linewidth = 0.9,
    color = "black"
  ) +
  geom_line(
    data = df,
    aes(x = xj, y = y, group = Subject),
    color = "grey35",
    linewidth = 0.6,
    alpha = 0.7
  ) +
  geom_point(
    data = df,
    aes(x = xj, y = y),
    shape = 21,
    fill = "white",
    color = "black",
    size = 3,
    stroke = 0.8,
    alpha = 0.95
  ) +
  scale_fill_manual(values = pal, guide = "none") +
  scale_x_continuous(breaks = 1:3, labels = c("NN","acute HH","48-h HH")) +
  scale_y_continuous(labels = y_labels) +
  labs(x = "Condition", y = "Mean coherence (a.u.)") +
  theme_bw(base_size = 20) +
  theme(
    panel.grid.major = element_line(color = "grey88", linewidth = 0.7),
    panel.grid.minor = element_line(color = "grey93", linewidth = 0.5),
    axis.title = element_text(size = 26),
    axis.text  = element_text(size = 20),
    axis.line  = element_line(linewidth = 1.1, color = "black")
  )

# ----------------------------
# 7) Brackets con offsets controlados
#   p_off: cuánto sube el p sobre el bracket (MENOR = más abajo)
#   d_off: cuánto baja el Δ bajo el bracket (MENOR = más arriba)
# ----------------------------
add_bracket <- function(p_obj, x1, x2, yb, step, p_txt, delta_txt,
                        p_off = 0.90, d_off = 0.70){
  p_obj +
    annotate("segment", x = x1, xend = x2, y = yb, yend = yb, linewidth = 0.9) +
    annotate("segment", x = x1, xend = x1, y = yb, yend = yb - step*0.25, linewidth = 0.9) +
    annotate("segment", x = x2, xend = x2, y = yb, yend = yb - step*0.25, linewidth = 0.9) +
    # p arriba del bracket
    annotate("text", x = (x1+x2)/2, y = yb + step*p_off, label = p_txt, size = 6) +
    # Δ abajo del bracket
    annotate("text", x = (x1+x2)/2, y = yb - step*d_off, label = delta_txt, size = 5.2)
}

# alturas base
y_top <- max(c(df$y, sumdf$mean + sumdf$sd), na.rm = TRUE)
y_br0 <- y_top * 1.08
step  <- y_top * 0.12  # un poco más de "unidad" para mover textos

m_NN    <- sumdf$mean[sumdf$Condition=="NN"]
m_acute <- sumdf$mean[sumdf$Condition=="acute HH"]
m_48h   <- sumdf$mean[sumdf$Condition=="48-h HH"]

# Bracket inferior: NN vs acute HH
# -> BAJAR p: usar p_off más chico
delta1 <- m_acute - m_NN
p2 <- add_bracket(
  p_obj     = p2,
  x1        = 1, x2 = 2,
  yb        = y_br0,
  step      = step,
  p_txt     = "p=0.044",
  delta_txt = paste0("\u0394(acute HH\u2013NN)=", fmt_delta(delta1, 3)),
  p_off     = 0.55,  # <- p más abajo (más pegado al bracket)
  d_off     = 0.85   # <- Δ un poquito más abajo (para despejar la zona)
)

# Bracket superior: NN vs 48-h HH
# -> SUBIR Δ: usar d_off más chico (menos distancia hacia abajo)
# -> MÁS separación vertical: subir el bracket superior (yb mayor)
delta2 <- m_48h - m_NN
p2 <- add_bracket(
  p_obj     = p2,
  x1        = 1, x2 = 3,
  yb        = y_br0 + step*2.30,  # <- más arriba (más separación entre brackets)
  step      = step,
  p_txt     = "p=0.041",
  delta_txt = paste0("\u0394(48-h HH\u2013NN)=", fmt_delta(delta2, 3)),
  p_off     = 0.95,  # p normal arriba
  d_off     = 0.45   # <- Δ más arriba (más cerca del bracket superior)
)

# aire arriba total
p2 <- p2 + coord_cartesian(ylim = c(0, y_br0 + step*3.8))

p2

# ggsave("Fig3A_theta_mean.png", p2, width = 8.6, height = 6.0, dpi = 600)

###########################################################################################################################################


# FIG (theta_max) — estilo tipo fc_hipotesis:
# barras = media ± SD, puntos + líneas por sujeto (alineación perfecta),
# bracket SOLO NN vs 48-h HH (p=0.036),
# p-valor ARRIBA del bracket y Δ ABAJO del bracket.

library(readxl)
library(dplyr)
library(tidyr)
library(stringr)
library(ggplot2)

# ----------------------------
# 0) Ruta del Excel
# ----------------------------
file_xlsx <- "C:/Users/jguer/Dropbox/linea_investigacion/borradores/coherencia/estadistica.xlsx"

# ----------------------------
# 1) Helpers
# ----------------------------
fmt_delta <- function(x, digits = 3){
  out <- formatC(x, format="f", digits=digits)
  str_replace(out, "\\.", ",")
}
y_labels <- function(x){
  str_replace(format(x, nsmall = 2, trim = TRUE), "\\.", ",")
}

# ----------------------------
# 2) Datos individuales (sheet: ind) -> theta_max
# ----------------------------
df <- read_excel(file_xlsx, sheet = "ind") %>%
  mutate(
    Condition = case_when(
      condition == 1 ~ "NN",
      condition == 2 ~ "acute HH",
      condition == 3 ~ "48-h HH",
      TRUE ~ as.character(condition)
    ),
    Condition = factor(Condition, levels = c("NN", "acute HH", "48-h HH"))
  ) %>%
  group_by(Condition) %>%
  mutate(Subject = row_number()) %>%   # asume mismo orden por condición
  ungroup() %>%
  transmute(Condition, Subject, y = theta_max)  # <- si tu columna se llama distinto, cámbiala aquí

# ----------------------------
# 3) Jitter fijo por sujeto (líneas conectan EXACTO los puntos)
# ----------------------------
set.seed(123)
df <- df %>% mutate(x = as.numeric(Condition))

jit <- df %>%
  distinct(Subject) %>%
  mutate(j = runif(n(), -0.06, 0.06))

df <- df %>%
  left_join(jit, by = "Subject") %>%
  mutate(xj = x + j)

# ----------------------------
# 4) Resumen (media ± SD)
# ----------------------------
sumdf <- df %>%
  group_by(Condition) %>%
  summarise(
    x    = unique(x),
    mean = mean(y, na.rm = TRUE),
    sd   = sd(y, na.rm = TRUE),
    .groups = "drop"
  )

# ----------------------------
# 5) Estética
# ----------------------------
pal <- c("NN"="#3B74A6","acute HH"="#9E9E9E","48-h HH"="#6F6F6F")

# ----------------------------
# 6) Plot base
# ----------------------------
p <- ggplot() +
  geom_col(
    data = sumdf,
    aes(x = x, y = mean, fill = Condition),
    width = 0.72,
    color = "black",
    linewidth = 0.7,
    alpha = 0.95
  ) +
  geom_errorbar(
    data = sumdf,
    aes(x = x, ymin = mean - sd, ymax = mean + sd),
    width = 0.16,
    linewidth = 0.9,
    color = "black"
  ) +
  geom_line(
    data = df,
    aes(x = xj, y = y, group = Subject),
    color = "grey35",
    linewidth = 0.6,
    alpha = 0.7
  ) +
  geom_point(
    data = df,
    aes(x = xj, y = y),
    shape = 21,
    fill = "white",
    color = "black",
    size = 3,
    stroke = 0.8,
    alpha = 0.95
  ) +
  scale_fill_manual(values = pal, guide = "none") +
  scale_x_continuous(breaks = 1:3, labels = c("NN","acute HH","48-h HH")) +
  scale_y_continuous(labels = y_labels) +
  labs(x = "Condition", y = "Maximum coherence (a.u.)") +
  theme_bw(base_size = 20) +
  theme(
    panel.grid.major = element_line(color = "grey88", linewidth = 0.7),
    panel.grid.minor = element_line(color = "grey93", linewidth = 0.5),
    axis.title = element_text(size = 26),
    axis.text  = element_text(size = 20),
    axis.line  = element_line(linewidth = 1.1, color = "black")
  )

# ----------------------------
# 7) Bracket NN vs 48-h HH (p=0.036) + Δ
# ----------------------------
add_bracket <- function(p_obj, x1, x2, yb, step, p_txt, delta_txt,
                        p_off = 0.95, d_off = 0.55){
  p_obj +
    annotate("segment", x = x1, xend = x2, y = yb, yend = yb, linewidth = 0.9) +
    annotate("segment", x = x1, xend = x1, y = yb, yend = yb - step*0.25, linewidth = 0.9) +
    annotate("segment", x = x2, xend = x2, y = yb, yend = yb - step*0.25, linewidth = 0.9) +
    annotate("text", x = (x1+x2)/2, y = yb + step*p_off, label = p_txt, size = 6) +
    annotate("text", x = (x1+x2)/2, y = yb - step*d_off, label = delta_txt, size = 5.2)
}

y_top <- max(c(df$y, sumdf$mean + sumdf$sd), na.rm = TRUE)
y_br0 <- y_top * 1.08
step  <- y_top * 0.12

m_NN  <- sumdf$mean[sumdf$Condition=="NN"]
m_48h <- sumdf$mean[sumdf$Condition=="48-h HH"]
delta <- m_48h - m_NN

p <- add_bracket(
  p_obj     = p,
  x1        = 1, x2 = 3,
  yb        = y_br0,
  step      = step,
  p_txt     = "p=0.036",
  delta_txt = paste0("\u0394(48-h HH\u2013NN)=", fmt_delta(delta, 3)),
  p_off     = 0.95,  # p bien arriba del bracket
  d_off     = 0.55   # Δ debajo del bracket
)

p <- p + coord_cartesian(ylim = c(0, y_br0 + step*2.2))

p

# ggsave("Fig_theta_max.png", p, width = 8.6, height = 6.0, dpi = 600)



#####################################################################################################################################
# FIG (theta consistency area index) — theta_A_c
# barras = media ± SD, puntos individuales + líneas por sujeto (sin desalineación),
# bracket NN vs 48-h HH (p=0.023) + Δ(48-h HH–NN) abajo.

library(readxl)
library(dplyr)
library(stringr)
library(ggplot2)

file_xlsx <- "C:/Users/jguer/Dropbox/linea_investigacion/borradores/coherencia/estadistica.xlsx"

fmt_delta <- function(x, digits = 3){
  out <- formatC(x, format="f", digits=digits)
  str_replace(out, "\\.", ",")
}
y_labels <- function(x){
  str_replace(format(x, nsmall = 2, trim = TRUE), "\\.", ",")
}

# ----------------------------
# 1) Datos individuales (sheet: ind) -> theta_A_c
# ----------------------------
df <- read_excel(file_xlsx, sheet = "ind") %>%
  mutate(
    Condition = case_when(
      condition == 1 ~ "NN",
      condition == 2 ~ "acute HH",
      condition == 3 ~ "48-h HH",
      TRUE ~ as.character(condition)
    ),
    Condition = factor(Condition, levels = c("NN", "acute HH", "48-h HH"))
  ) %>%
  group_by(Condition) %>%
  mutate(Subject = row_number()) %>%
  ungroup() %>%
  transmute(Condition, Subject, y = theta_A_c)

# ----------------------------
# 2) Jitter fijo por sujeto (líneas conectan EXACTO los puntos)
# ----------------------------
set.seed(123)
df <- df %>% mutate(x = as.numeric(Condition))
jit <- df %>% distinct(Subject) %>% mutate(j = runif(n(), -0.06, 0.06))
df <- df %>% left_join(jit, by = "Subject") %>% mutate(xj = x + j)

# ----------------------------
# 3) Resumen (media ± SD)
# ----------------------------
sumdf <- df %>%
  group_by(Condition) %>%
  summarise(
    x    = unique(x),
    mean = mean(y, na.rm = TRUE),
    sd   = sd(y, na.rm = TRUE),
    .groups = "drop"
  )

# ----------------------------
# 4) Estética
# ----------------------------
pal <- c("NN"="#3B74A6","acute HH"="#9E9E9E","48-h HH"="#6F6F6F")

p <- ggplot() +
  geom_col(
    data = sumdf,
    aes(x = x, y = mean, fill = Condition),
    width = 0.72, color = "black", linewidth = 0.7, alpha = 0.95
  ) +
  geom_errorbar(
    data = sumdf,
    aes(x = x, ymin = mean - sd, ymax = mean + sd),
    width = 0.16, linewidth = 0.9, color = "black"
  ) +
  geom_line(
    data = df,
    aes(x = xj, y = y, group = Subject),
    color = "grey35", linewidth = 0.6, alpha = 0.7
  ) +
  geom_point(
    data = df,
    aes(x = xj, y = y),
    shape = 21, fill = "white", color = "black",
    size = 3, stroke = 0.8, alpha = 0.95
  ) +
  scale_fill_manual(values = pal, guide = "none") +
  scale_x_continuous(breaks = 1:3, labels = c("NN","acute HH","48-h HH")) +
  scale_y_continuous(labels = y_labels) +
  labs(
    x = "Condition",
    y = "Consistency area index (a.u.)"
  ) +
  theme_bw(base_size = 20) +
  theme(
    panel.grid.major = element_line(color = "grey88", linewidth = 0.7),
    panel.grid.minor = element_line(color = "grey93", linewidth = 0.5),
    axis.title = element_text(size = 26),
    axis.text  = element_text(size = 20),
    axis.line  = element_line(linewidth = 1.1, color = "black")
  )

# ----------------------------
# 5) Bracket NN vs 48-h HH (p=0.023) + Δ
# ----------------------------
add_bracket <- function(p_obj, x1, x2, yb, step, p_txt, delta_txt,
                        p_off = 0.90, d_off = 0.55){
  p_obj +
    annotate("segment", x = x1, xend = x2, y = yb, yend = yb, linewidth = 0.9) +
    annotate("segment", x = x1, xend = x1, y = yb, yend = yb - step*0.25, linewidth = 0.9) +
    annotate("segment", x = x2, xend = x2, y = yb, yend = yb - step*0.25, linewidth = 0.9) +
    annotate("text", x = (x1+x2)/2, y = yb + step*p_off, label = p_txt, size = 6) +
    annotate("text", x = (x1+x2)/2, y = yb - step*d_off, label = delta_txt, size = 5.2)
}

y_top <- max(c(df$y, sumdf$mean + sumdf$sd), na.rm = TRUE)
y_br0 <- y_top * 1.08
step  <- y_top * 0.12

m_NN  <- sumdf$mean[sumdf$Condition=="NN"]
m_48h <- sumdf$mean[sumdf$Condition=="48-h HH"]
delta <- m_48h - m_NN

p <- add_bracket(
  p_obj     = p,
  x1        = 1, x2 = 3,
  yb        = y_br0,
  step      = step,
  p_txt     = "p=0.023",
  delta_txt = paste0("\u0394(48-h HH\u2013NN)=", fmt_delta(delta, 3))
) +
  coord_cartesian(ylim = c(0, y_br0 + step*2.2))

p

# ggsave("Fig_theta_A_c.png", p, width = 8.6, height = 6.0, dpi = 600)

#########################################################################################################################################


# FIG 3B (gamma_mean) — estilo tipo fc_hipotesis:
# barras = media ± SD, puntos + líneas por sujeto (sin desalineación),
# bracket SOLO NN vs 48-h HH (p=0.026),
# p-valor ARRIBA del bracket y Δ ABAJO del bracket.

library(readxl)
library(dplyr)
library(stringr)
library(ggplot2)

file_xlsx <- "C:/Users/jguer/Dropbox/linea_investigacion/borradores/coherencia/estadistica.xlsx"

fmt_delta <- function(x, digits = 3){
  out <- formatC(x, format="f", digits=digits)
  str_replace(out, "\\.", ",")
}
y_labels <- function(x){
  str_replace(format(x, nsmall = 2, trim = TRUE), "\\.", ",")
}

# ----------------------------
# 1) Datos individuales (sheet: ind) -> gamma_mean
# ----------------------------
df <- read_excel(file_xlsx, sheet = "ind") %>%
  mutate(
    Condition = case_when(
      condition == 1 ~ "NN",
      condition == 2 ~ "acute HH",
      condition == 3 ~ "48-h HH",
      TRUE ~ as.character(condition)
    ),
    Condition = factor(Condition, levels = c("NN", "acute HH", "48-h HH"))
  ) %>%
  group_by(Condition) %>%
  mutate(Subject = row_number()) %>%  # asume mismo orden de sujetos por condición
  ungroup() %>%
  transmute(Condition, Subject, y = gamma_mean)  # <- CAMBIA si tu columna se llama distinto

# ----------------------------
# 2) Jitter fijo por sujeto (líneas conectan EXACTO los puntos)
# ----------------------------
set.seed(123)
df <- df %>% mutate(x = as.numeric(Condition))
jit <- df %>% distinct(Subject) %>% mutate(j = runif(n(), -0.06, 0.06))
df <- df %>% left_join(jit, by = "Subject") %>% mutate(xj = x + j)

# ----------------------------
# 3) Resumen (media ± SD)
# ----------------------------
sumdf <- df %>%
  group_by(Condition) %>%
  summarise(
    x    = unique(x),
    mean = mean(y, na.rm = TRUE),
    sd   = sd(y, na.rm = TRUE),
    .groups = "drop"
  )

# ----------------------------
# 4) Estética
# ----------------------------
pal <- c("NN"="#3B74A6","acute HH"="#9E9E9E","48-h HH"="#6F6F6F")

p <- ggplot() +
  geom_col(
    data = sumdf,
    aes(x = x, y = mean, fill = Condition),
    width = 0.72, color = "black", linewidth = 0.7, alpha = 0.95
  ) +
  geom_errorbar(
    data = sumdf,
    aes(x = x, ymin = mean - sd, ymax = mean + sd),
    width = 0.16, linewidth = 0.9, color = "black"
  ) +
  geom_line(
    data = df,
    aes(x = xj, y = y, group = Subject),
    color = "grey35", linewidth = 0.6, alpha = 0.7
  ) +
  geom_point(
    data = df,
    aes(x = xj, y = y),
    shape = 21, fill = "white", color = "black",
    size = 3, stroke = 0.8, alpha = 0.95
  ) +
  scale_fill_manual(values = pal, guide = "none") +
  scale_x_continuous(breaks = 1:3, labels = c("NN","acute HH","48-h HH")) +
  scale_y_continuous(labels = y_labels) +
  labs(
    x = "Condition",
    y = "Mean coherence (a.u.)"
  ) +
  theme_bw(base_size = 20) +
  theme(
    panel.grid.major = element_line(color = "grey88", linewidth = 0.7),
    panel.grid.minor = element_line(color = "grey93", linewidth = 0.5),
    axis.title = element_text(size = 26),
    axis.text  = element_text(size = 20),
    axis.line  = element_line(linewidth = 1.1, color = "black")
  )

# ----------------------------
# 5) Bracket NN vs 48-h HH (p=0.026) + Δ
# ----------------------------
add_bracket <- function(p_obj, x1, x2, yb, step, p_txt, delta_txt,
                        p_off = 0.90, d_off = 0.55){
  p_obj +
    annotate("segment", x = x1, xend = x2, y = yb, yend = yb, linewidth = 0.9) +
    annotate("segment", x = x1, xend = x1, y = yb, yend = yb - step*0.25, linewidth = 0.9) +
    annotate("segment", x = x2, xend = x2, y = yb, yend = yb - step*0.25, linewidth = 0.9) +
    annotate("text", x = (x1+x2)/2, y = yb + step*p_off, label = p_txt, size = 6) +
    annotate("text", x = (x1+x2)/2, y = yb - step*d_off, label = delta_txt, size = 5.2)
}

y_top <- max(c(df$y, sumdf$mean + sumdf$sd), na.rm = TRUE)
y_br0 <- y_top * 1.08
step  <- y_top * 0.12

m_NN  <- sumdf$mean[sumdf$Condition=="NN"]
m_48h <- sumdf$mean[sumdf$Condition=="48-h HH"]
delta <- m_48h - m_NN

p <- add_bracket(
  p_obj     = p,
  x1        = 1, x2 = 3,
  yb        = y_br0,
  step      = step,
  p_txt     = "p=0.026",
  delta_txt = paste0("\u0394(48-h HH\u2013NN)=", fmt_delta(delta, 3))
) +
  coord_cartesian(ylim = c(0, y_br0 + step*2.2))

p

# ggsave("Fig3B_gamma_mean.png", p, width = 8.6, height = 6.0, dpi = 600)

