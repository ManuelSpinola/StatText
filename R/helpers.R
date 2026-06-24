# ============================================================
# helpers.R — Funciones y objetos compartidos entre módulos
# StatText · StatSuite · Manuel Spínola · ICOMVIS · UNA
# Paleta: Tableau Color Blind (coherente con StatSuite)
# ============================================================

library(bslib)
library(bsicons)
library(shiny)
library(shinyjs)
library(ggplot2)
library(dplyr)
library(DT)
library(readr)
library(readxl)
library(quanteda)
library(quanteda.textstats)
library(quanteda.textplots)
library(topicmodels)
library(tidytext)
library(FactoMineR)
library(factoextra)
library(likert)
library(stringr)
library(tidyr)

# ── Paleta de colores (idéntica a StatSuite) ───────────────
colores <- list(
  fondo       = "#F4F7FB",
  primario    = "#1170AA",
  acento      = "#FC7D0B",
  secundario  = "#5FA2CE",
  texto       = "#57606C",
  exito       = "#5FA2CE",
  advertencia = "#F1CE63",
  peligro     = "#C85200",
  borde       = "#C8D9EC",

  tableau = c(
    "#1170AA", "#FC7D0B", "#A3ACB9", "#57606C",
    "#C85200", "#7BC8ED", "#5FA2CE", "#F1CE63",
    "#9F8B75", "#B85A0D"
  )
)

# ── Tema visual (idéntico a StatSuite) ─────────────────────
tema_app <- bslib::bs_theme(
  version      = 5,
  bg           = colores$fondo,
  fg           = colores$texto,
  primary      = colores$primario,
  secondary    = colores$secundario,
  success      = colores$exito,
  danger       = colores$peligro,
  warning      = colores$advertencia,
  base_font    = bslib::font_google("Nunito"),
  heading_font = bslib::font_google("Nunito", wght = 700),
  bootswatch   = NULL
) |>
  bslib::bs_add_rules("
  .navbar { background-color: #1170AA !important; }
  .navbar-brand { color: #ffffff !important; display: flex !important;
                  align-items: center !important;
                  padding-top: 0 !important; padding-bottom: 0 !important; }
  .navbar .nav-link { color: #ffffff !important; }
  .navbar .nav-link.active { border-bottom: 2px solid #FC7D0B; }
  .nav-tabs .nav-link.active,
  .nav-tabs .nav-item .nav-link.active,
  ul.nav.nav-tabs li.nav-item a.nav-link.active {
    background-color: #1170AA !important;
    color: #ffffff !important;
    border-top-color: #1170AA !important;
    border-left-color: #1170AA !important;
    border-right-color: #1170AA !important;
    border-bottom-color: transparent !important;
    font-weight: 600 !important;
  }
  .nav-tabs .nav-link:not(.active):hover {
    background-color: #EEF3FA !important;
    color: #1170AA !important;
  }
  .btn-primary { background-color: #FC7D0B; border-color: #FC7D0B; color: #ffffff; }
  .btn-primary:hover { background-color: #d4680a; border-color: #d4680a; }
  .card > .card-header { background-color: #C8D9EC; color: #1170AA; font-weight: 700;
                         border-bottom: none; }
  .card > .card-header:has(.nav-tabs) { background-color: transparent; color: inherit;
                                        border-bottom: revert; }
  .navbar-brand { display: flex !important; align-items: center !important;
                  padding-top: 0 !important; padding-bottom: 0 !important; }

  /* Semáforo de diagnóstico */
  .sem-ok   { background: #f0f9f5; border-left: 4px solid #5FA2CE; }
  .sem-warn { background: #fffbf0; border-left: 4px solid #F1CE63; }
  .sem-bad  { background: #fff0f2; border-left: 4px solid #C85200; }

  /* Código R */
  .codigo-bloque { background: #1e1e2e; color: #cdd6f4;
                   border-radius: 8px; padding: 1rem;
                   font-family: 'Fira Code', monospace;
                   font-size: 0.82rem; line-height: 1.7;
                   overflow-x: auto; white-space: pre; }

  /* Nube de palabras */
  .wordcloud-wrap { background: #ffffff; border-radius: 8px;
                    border: 1px solid #C8D9EC; }
")

# ── Escalas ggplot2 (Tableau Color Blind) ─────────────────
scale_fill_tableau_cb <- function(...) {
  scale_fill_manual(values = colores$tableau, ...)
}
scale_color_tableau_cb <- function(...) {
  scale_color_manual(values = colores$tableau, ...)
}

# ── Encabezado estándar de scripts R ──────────────────────
encabezado_script <- function(modulo) {
  paste0(
    "# ============================================\n",
    "# StatText · StatSuite\n",
    "# Módulo: ", modulo, "\n",
    "# Generado: ", format(Sys.Date(), "%Y-%m-%d"), "\n",
    "# Manuel Spínola · ICOMVIS · UNA · Costa Rica\n",
    "# ============================================\n\n"
  )
}

# ── Helper: value box card ─────────────────────────────────
vbox_card <- function(icono, label, valor, color) {
  div(
    class = "card h-100",
    style = paste0(
      "background:", colores$fondo,
      "; border-radius:8px; padding:1rem; text-align:center;"
    ),
    div(
      style = paste0(
        "font-size:13px; color:", colores$texto,
        "; margin-bottom:4px;"
      ),
      bs_icon(icono, class = "me-1"), label
    ),
    div(
      style = paste0(
        "font-size:24px; font-weight:500; color:", color, ";"
      ),
      valor
    )
  )
}

# ── Helper: pantalla "próximamente" ───────────────────────
proximamente_ui <- function(icono, titulo, subtitulo, paquete, datasets) {
  div(
    class = "py-5 px-3",
    style = "max-width: 640px; margin: 0 auto;",
    div(
      class = "text-center mb-4",
      bs_icon(icono, size = "3em",
              style = paste0("color:", colores$secundario)),
      h4(class = "mt-3 mb-2",
         style = paste0("color:", colores$primario, "; font-weight:700;"),
         titulo),
      p(class = "text-muted", subtitulo)
    ),
    div(
      class = "alert mb-3",
      style = paste0(
        "background:", colores$fondo, ";",
        "border-left: 4px solid ", colores$acento, ";"
      ),
      bs_icon("hourglass-split", class = "me-2",
              style = paste0("color:", colores$acento)),
      tags$strong("En desarrollo."),
      " Este módulo estará disponible en una próxima versión de StatText."
    ),
    layout_columns(
      col_widths = c(6, 6),
      card(
        card_header(bs_icon("box-seam", class = "me-1"), "Paquete R"),
        card_body(p(class = "small text-muted mb-0", paquete))
      ),
      card(
        card_header(bs_icon("database", class = "me-1"), "Datasets previstos"),
        card_body(p(class = "small text-muted mb-0", datasets))
      )
    )
  )
}

# ── Stopwords para español e inglés ───────────────────────
stopwords_es <- quanteda::stopwords("es")
stopwords_en <- quanteda::stopwords("en")

# ── Preprocesamiento estándar de corpus ───────────────────
preprocesar_corpus <- function(textos, idioma = "es",
                               remover_numeros = TRUE,
                               remover_puntuacion = TRUE,
                               min_nchar = 3) {
  corp  <- quanteda::corpus(textos)
  toks  <- quanteda::tokens(
    corp,
    remove_numbers     = remover_numeros,
    remove_punct       = remover_puntuacion,
    remove_symbols     = TRUE,
    remove_separators  = TRUE
  )
  sw <- if (idioma == "es") stopwords_es else stopwords_en
  toks <- quanteda::tokens_remove(toks, pattern = sw, padding = FALSE)
  toks <- quanteda::tokens_select(toks, min_nchar = min_nchar)
  toks
}

# ── Construir DFM desde tokens ────────────────────────────
construir_dfm <- function(toks, stem = FALSE) {
  dfm <- quanteda::dfm(toks)
  if (stem) dfm <- quanteda::dfm_wordstem(dfm)
  dfm
}

# ── Top features como data.frame ──────────────────────────
top_features_df <- function(dfm, n = 20) {
  tf <- quanteda.textstats::textstat_frequency(dfm, n = n)
  as.data.frame(tf[, c("feature", "frequency", "rank")])
}

# ── Detección de idioma simple ────────────────────────────
detectar_idioma <- function(texto) {
  # Heurística básica: presencia de artículos/preposiciones
  es_words <- c("que", "los", "las", "una", "con", "del", "para", "por",
                "este", "esta", "son", "ser", "hay", "también", "pero")
  en_words <- c("the", "and", "for", "that", "this", "with", "are",
                "have", "from", "they", "been", "will", "would")
  words <- tolower(unlist(strsplit(texto, "\\s+")))
  n_es  <- sum(words %in% es_words)
  n_en  <- sum(words %in% en_words)
  if (n_es >= n_en) "es" else "en"
}
