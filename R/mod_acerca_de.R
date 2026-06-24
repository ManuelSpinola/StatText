# ============================================================
# mod_acerca_de.R — Información sobre StatText
# StatText · StatSuite · Manuel Spínola · ICOMVIS · UNA
# ============================================================

mod_acerca_de_ui <- function(id) {
  ns <- NS(id)
  tagList(
    div(
      class = "py-4 px-3",
      style = "max-width: 780px; margin: 0 auto;",

      h4(
        bs_icon("info-circle", class = "me-2"),
        "Acerca de StatText",
        style = paste0("color:", colores$primario, "; font-weight:700;")
      ),
      p(
        class = "text-muted mb-4",
        "StatText es la app de minería de texto de StatSuite, desarrollada en",
        " el ICOMVIS, Universidad Nacional, Costa Rica. Está diseñada para",
        " investigadores en ciencias sociales que necesitan analizar texto libre,",
        " respuestas abiertas de encuestas y escalas Likert con herramientas",
        " estadísticas sólidas y reproducibles en R."
      ),

      layout_columns(
        col_widths = c(6, 6),

        card(
          card_header(bs_icon("collection", class = "me-1"),
                      "StatSuite — Ecosistema completo"),
          card_body(
            tags$ul(
              class = "small",
              tags$li(strong("StatDesign"),    " — Diseño de estudios y muestreo"),
              tags$li(strong("StatFlow"),      " — Primeros análisis y visualización"),
              tags$li(strong("StatGeo"),       " — Análisis espacial y mapas"),
              tags$li(strong("StatMonitor"),   " — Monitoreo poblacional"),
              tags$li(strong("StatModels"),    " — Modelos estadísticos"),
              tags$li(strong("StatOccu"),      " — Modelos de ocupación"),
              tags$li(strong("StatAbundance"), " — Modelos de abundancia"),
              tags$li(strong("StatText"),      " — Minería de texto ← aquí")
            )
          )
        ),

        card(
          card_header(bs_icon("box-seam", class = "me-1"),
                      "Módulos de StatText"),
          card_body(
            tags$ul(
              class = "small",
              tags$li(
                bs_icon("check-circle-fill", class = "me-1",
                        style = paste0("color:", colores$exito)),
                strong("Texto libre"),
                " — quanteda · topicmodels · LDA"
              ),
              tags$li(
                bs_icon("check-circle-fill", class = "me-1",
                        style = paste0("color:", colores$exito)),
                strong("Preguntas abiertas"),
                " — quanteda · comparación de grupos"
              ),
              tags$li(
                bs_icon("check-circle-fill", class = "me-1",
                        style = paste0("color:", colores$exito)),
                strong("Encuestas cerradas"),
                " — FactoMineR · ACM · Likert"
              ),
              tags$li(
                bs_icon("hourglass-split", class = "me-1",
                        style = paste0("color:", colores$acento)),
                strong("Análisis de sentimiento"),
                " — (próximamente)"
              ),
              tags$li(
                bs_icon("hourglass-split", class = "me-1",
                        style = paste0("color:", colores$acento)),
                strong("STM (Structural Topic Model)"),
                " — (próximamente)"
              )
            )
          )
        )
      ),

      card(
        class = "mt-3",
        card_header(bs_icon("box-seam", class = "me-1"),
                    "Motor de análisis — paquetes R"),
        card_body(
          layout_columns(
            col_widths = c(4, 4, 4),
            div(
              tags$b(class = "small",
                     style = paste0("color:", colores$primario),
                     "Texto libre"),
              tags$ul(
                class = "small text-muted",
                tags$li(code("quanteda")),
                tags$li(code("quanteda.textstats")),
                tags$li(code("quanteda.textplots")),
                tags$li(code("topicmodels")),
                tags$li(code("tidytext"))
              )
            ),
            div(
              tags$b(class = "small",
                     style = paste0("color:", colores$acento),
                     "Preguntas abiertas"),
              tags$ul(
                class = "small text-muted",
                tags$li(code("quanteda")),
                tags$li(code("quanteda.textstats")),
                tags$li(code("tidytext")),
                tags$li(code("stringr"))
              )
            ),
            div(
              tags$b(class = "small",
                     style = paste0("color:", colores$secundario),
                     "Encuestas cerradas"),
              tags$ul(
                class = "small text-muted",
                tags$li(code("FactoMineR")),
                tags$li(code("factoextra")),
                tags$li(code("likert")),
                tags$li(code("psych"))
              )
            )
          )
        )
      ),

      card(
        class = "mt-3",
        card_header(bs_icon("book", class = "me-1"), "Referencias clave"),
        card_body(
          tags$ul(
            class = "small text-muted mb-0",
            tags$li(
              "Benoit, K., et al. (2018). quanteda: An R package for the",
              " quantitative analysis of textual data.",
              em(" Journal of Open Source Software"), ", 3(30), 774."
            ),
            tags$li(class = "mt-1",
              "Blei, D.M., Ng, A.Y. & Jordan, M.I. (2003). Latent Dirichlet",
              " Allocation.", em(" Journal of Machine Learning Research"), ", 3, 993–1022."
            ),
            tags$li(class = "mt-1",
              "Lê, S., Josse, J. & Husson, F. (2008). FactoMineR: An R Package",
              " for Multivariate Analysis.",
              em(" Journal of Statistical Software"), ", 25(1), 1–18."
            ),
            tags$li(class = "mt-1",
              "Silge, J. & Robinson, D. (2017).",
              em(" Text Mining with R: A Tidy Approach."),
              " O'Reilly Media."
            )
          )
        )
      ),

      div(
        class = "alert alert-info small mt-3",
        bs_icon("envelope", class = "me-1"),
        "Contacto: ",
        tags$a(href = "mailto:manuel.spinola@una.ac.cr",
               "manuel.spinola@una.ac.cr")
      )
    )
  )
}

mod_acerca_de_server <- function(id) {
  moduleServer(id, function(input, output, session) {
    # sin lógica reactiva por ahora
  })
}
