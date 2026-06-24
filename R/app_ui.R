#' Application UI
#'
#' @return A Shiny UI object.
#' @noRd
app_ui <- function() {

  golem::add_resource_path(
    "www",
    system.file("app/www", package = "R_shiny_stattext")
  )

  page_navbar(
    header = shinyjs::useShinyjs(),
    title  = div(
      style = "display: flex; align-items: center; gap: 10px; margin-top: 4px;",
      img(src = "www/hexsticker_StatText.png", height = "38px"),
      span("StatText", style = "font-weight: 600;")
    ),
    theme  = tema_app,
    lang   = "es",
    footer = div(
      class = "text-center text-muted small py-2",
      style = paste0("border-top: 1px solid ", colores$borde, ";"),
      "Manuel Spínola · ICOMVIS · Universidad Nacional · Costa Rica"
    ),

    # ── Módulo 1: Texto libre ─────────────────────────────
    nav_panel(
      title = "Texto libre",
      icon  = bs_icon("file-text"),
      mod_texto_libre_ui("texto_libre")
    ),

    # ── Módulo 2: Preguntas abiertas ──────────────────────
    nav_panel(
      title = "Preguntas abiertas",
      icon  = bs_icon("chat-text"),
      mod_enc_abiertas_ui("enc_abiertas")
    ),

    # ── Módulo 3: Encuestas cerradas ──────────────────────
    nav_panel(
      title = "Encuestas cerradas",
      icon  = bs_icon("bar-chart-steps"),
      mod_enc_cerradas_ui("enc_cerradas")
    ),

    # ── Módulo 4: Sentimiento (próximamente) ──────────────
    nav_panel(
      title = "Sentimiento",
      icon  = bs_icon("emoji-smile"),
      proximamente_ui(
        icono     = "emoji-smile",
        titulo    = "Análisis de sentimiento",
        subtitulo = paste0(
          "Clasificación automática de polaridad (positivo/negativo/neutro) ",
          "y emociones básicas (alegría, tristeza, miedo, enojo). ",
          "Soporta diccionarios en español e inglés, y modelos transformer ",
          "vía sentimentr y udpipe. Pensado para redes sociales, comentarios ",
          "y noticias en ciencias sociales."
        ),
        paquete  = "sentimentr · udpipe · syuzhet",
        datasets = "Tweets políticos · comentarios de encuestas"
      )
    ),

    # ── Módulo 5: STM (próximamente) ─────────────────────
    nav_panel(
      title = "STM",
      icon  = bs_icon("diagram-3"),
      proximamente_ui(
        icono     = "diagram-3",
        titulo    = "Structural Topic Model (STM)",
        subtitulo = paste0(
          "Extensión del LDA que permite incorporar covariables del documento ",
          "para modelar cómo los tópicos varían según características de autor, ",
          "fecha, región u otras variables contextuales. Especialmente útil para ",
          "corpus de ciencias sociales donde el contexto importa. ",
          "Paquete: stm (Roberts et al. 2019)."
        ),
        paquete  = "stm — Roberts et al. (2019)",
        datasets = "Corpus de noticias · tweets · actas parlamentarias"
      )
    ),

    nav_spacer(),

    nav_panel(
      title = "Acerca de",
      icon  = bs_icon("info-circle"),
      mod_acerca_de_ui("acerca_de")
    ),

    nav_item(
      tags$span(class = "text-white-50 small", "StatText v1.0")
    )
  )
}
