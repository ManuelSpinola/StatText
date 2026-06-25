# ============================================================
# mod_enc_abiertas.R — Preguntas abiertas de encuestas
# StatText · StatSuite · Manuel Spínola · ICOMVIS · UNA
#
# Análisis: preprocesamiento de texto corto/coloquial,
# frecuencia de términos, categorización temática,
# comparación entre grupos
# Motor: quanteda + tidytext
# ============================================================

# ── Datos de ejemplo ──────────────────────────────────────
EJEMPLO_ABIERTAS <- data.frame(
  respuesta = c(
    "El gobierno no escucha a la gente del barrio",
    "Estoy contenta con la convivencia, hay buen ambiente",
    "Falta trabajo para los jóvenes, muchos se van",
    "Muchísima inseguridad, no podemos salir de noche",
    "Los servicios de salud son muy malos y lentos",
    "Buena atención en el centro de salud del municipio",
    "No hay lugares para los chicos, faltan plazas y parques",
    "El transporte es un desastre, siempre llega tarde",
    "Creo que el municipio trabaja bien en general",
    "Necesitamos más iluminación en las calles, es peligroso",
    "Las calles están en muy mal estado, llenas de huecos",
    "Me siento escuchada por los líderes comunitarios",
    "El acceso al agua potable sigue siendo un problema grave",
    "Los precios están muy altos, no alcanza el sueldo",
    "Hay mucha discriminación contra los jóvenes acá",
    "La escuela pública mejoró mucho en los últimos años",
    "Falta apoyo para las mujeres que trabajan y tienen hijos",
    "Los vecinos se ayudan, hay buena organización comunitaria",
    "El ambiente está muy contaminado, el río huele mal",
    "Necesitamos más médicos especialistas en el hospital"
  ),
  grupo = rep(c("Zona urbana", "Zona rural"), each = 10),
  stringsAsFactors = FALSE
)


# ── UI ────────────────────────────────────────────────────
mod_enc_abiertas_ui <- function(id) {
  ns <- NS(id)

  tagList(
    div(
      class = "py-3 px-2",
      h4(
        bs_icon("chat-text", class = "me-2"),
        "Preguntas abiertas de encuestas",
        style = paste0("color:", colores$primario, "; font-weight:700;")
      ),
      p(
        class = "text-muted mb-0",
        "Respuestas cortas a una o pocas preguntas. Preprocesamiento especial",
        " para texto coloquial, frecuencias, comparación entre grupos",
        " y resumen temático con ", tags$strong("quanteda"), "."
      )
    ),

    navset_card_tab(

      # ══════════════════════════════════════════════════
      # PESTAÑA 1: Datos
      # ══════════════════════════════════════════════════
      nav_panel(
        title = tagList(bs_icon("upload", class = "me-1"), "Datos"),
        card_body(
          layout_columns(
            col_widths = c(5, 7),

            div(

              # ── Card: Fuente de respuestas ─────────────
              card(
                card_header(
                  bs_icon("database", class = "me-1"), "Fuente de respuestas"
                ),
                card_body(
                  tags$p(class = "small fw-semibold text-muted mb-1",
                         bs_icon("bookmark", class = "me-1"), "Ejemplo"),
                  radioButtons(
                    ns("fuente_abiertas"),
                    label = NULL,
                    choices = c("Ejemplo: encuesta barrial" = "ejemplo"),
                    selected = "ejemplo"
                  ),

                  tags$hr(class = "my-2"),
                  tags$p(class = "small fw-semibold text-muted mb-1",
                         bs_icon("upload", class = "me-1"), "Subir archivo"),
                  radioButtons(
                    ns("fuente_archivo_ab"),
                    label = NULL,
                    choices = c("Subir archivo (.csv, .xlsx)" = "archivo"),
                    selected = character(0)
                  ),
                  conditionalPanel(
                    condition = paste0("output['", ns("fuente_ab_es_archivo"), "']"),
                    fileInput(
                      ns("archivo_abiertas"),
                      label       = NULL,
                      accept      = c(".csv", ".xlsx"),
                      buttonLabel = tagList(
                        bs_icon("folder2-open", class = "me-1"), "Examinar…"),
                      placeholder = "Sin archivo seleccionado"
                    )
                  ),

                  uiOutput(ns("sel_columnas_ui"))
                )
              ),

              # ── Card: Preprocesamiento ─────────────────
              card(
                class = "mt-3",
                card_header(
                  bs_icon("sliders", class = "me-1"), "Preprocesamiento"
                ),
                card_body(
                  p(class = "small text-muted mb-2",
                    "Ajustado para texto corto y coloquial."),
                  checkboxInput(ns("norm_puntuacion"),
                                "Normalizar puntuación", TRUE),
                  checkboxInput(ns("norm_numeros"),
                                "Remover números", TRUE),
                  checkboxInput(ns("unificar_terminos"),
                                "Unificar variantes (stemming leve)", FALSE),
                  div(
                    class = "p-2 mb-2",
                    style = paste0("background:", colores$fondo,
                                   "; border-radius:6px; font-size:12px;"),
                    bs_icon("info-circle", class = "me-1",
                            style = paste0("color:", colores$primario)),
                    tags$strong("Stemming leve:"),
                    " agrupa variantes de una misma palabra sin reducirla",
                    " tanto como el stemming completo. Útil para texto coloquial."
                  ),
                  numericInput(ns("min_nchar_ab"), "Longitud mínima:",
                               value = 3, min = 2, max = 6, step = 1),
                  tags$hr(),
                  actionButton(
                    ns("procesar_ab"),
                    label  = tagList(bs_icon("play-fill", class = "me-1"),
                                     "Procesar respuestas"),
                    class  = "btn btn-primary w-100"
                  )
                )
              )
            ),

            div(
              uiOutput(ns("estado_ab_ui")),
              br(),
              DTOutput(ns("preview_ab"))
            )
          )
        )
      ), # /PESTAÑA 1

      # ══════════════════════════════════════════════════
      # PESTAÑA 2: Frecuencias
      # ══════════════════════════════════════════════════
      nav_panel(
        title = tagList(bs_icon("bar-chart", class = "me-1"), "Frecuencias"),
        card_body(
          layout_columns(
            col_widths = c(4, 8),
            card(
              card_header(bs_icon("sliders", class = "me-1"), "Controles"),
              card_body(
                sliderInput(ns("ab_n_top"), "Términos a mostrar:",
                            min = 5, max = 40, value = 15, step = 5),
                div(
                  class = "p-2 mt-2",
                  style = paste0("background:", colores$fondo,
                                 "; border-radius:6px; font-size:12px;"),
                  bs_icon("info-circle", class = "me-1",
                          style = paste0("color:", colores$primario)),
                  "Las respuestas cortas de encuesta producen DFM dispersas.",
                  " TF-IDF ayuda a identificar términos más discriminantes."
                ),
                tags$hr(),
                downloadButton(ns("descarga_ab_freq"),
                               "Descargar tabla",
                               class = "btn-sm btn-outline-primary w-100")
              )
            ),
            div(
              card(
                class = "mb-3",
                card_header(bs_icon("bar-chart-fill", class = "me-1"),
                            "Términos más frecuentes — todas las respuestas"),
                card_body(plotOutput(ns("plot_ab_freq"), height = "360px"))
              )
            )
          ),
          div(class = "mt-3",
            card(
              card_header(bs_icon("table", class = "me-1"), "Tabla de frecuencias"),
              card_body(DTOutput(ns("tabla_ab_freq")))
            )
          )
        )
      ), # /PESTAÑA 2

      # ══════════════════════════════════════════════════
      # PESTAÑA 3: Nube de palabras
      # ══════════════════════════════════════════════════
      nav_panel(
        title = tagList(bs_icon("cloud", class = "me-1"), "Nube"),
        card_body(
          layout_columns(
            col_widths = c(3, 9),
            card(
              card_header(bs_icon("sliders", class = "me-1"), "Controles"),
              card_body(
                sliderInput(ns("ab_nube_max"), "Máx. palabras:",
                            min = 15, max = 100, value = 50, step = 5),
                sliderInput(ns("ab_nube_minfreq"), "Frecuencia mínima:",
                            min = 1, max = 10, value = 1, step = 1),
                actionButton(
                  ns("regen_nube_ab"),
                  tagList(bs_icon("arrow-clockwise", class = "me-1"), "Regenerar"),
                  class = "btn btn-outline-primary btn-sm w-100 mt-2"
                )
              )
            ),
            card(
              class = "wordcloud-wrap",
              card_body(plotOutput(ns("plot_ab_nube"), height = "420px"))
            )
          )
        )
      ), # /PESTAÑA 3

      # ══════════════════════════════════════════════════
      # PESTAÑA 4: Comparación entre grupos
      # ══════════════════════════════════════════════════
      nav_panel(
        title = tagList(bs_icon("fullscreen", class = "me-1"),
                        "Comparar grupos"),
        card_body(
          p(class = "small text-muted mb-3",
            "Compará la frecuencia de términos entre subgrupos de la muestra.",
            " Requiere una columna de agrupación en los datos."),

          uiOutput(ns("ui_grupo_disponible")),

          conditionalPanel(
            condition = paste0("output['", ns("tiene_grupo"), "']"),
            layout_columns(
              col_widths = c(4, 8),
              card(
                card_header(bs_icon("sliders", class = "me-1"), "Controles"),
                card_body(
                  uiOutput(ns("sel_col_grupo_ui")),
                  sliderInput(ns("comp_n_top"), "Términos por grupo:",
                              min = 5, max = 25, value = 10, step = 5),
                  selectInput(
                    ns("comp_metrica"),
                    "Métrica:",
                    choices = c(
                      "Frecuencia absoluta" = "freq",
                      "TF-IDF"             = "tfidf"
                    )
                  )
                )
              ),
              card(
                card_header(bs_icon("graph-up-arrow", class = "me-1"),
                            "Términos más distintivos por grupo"),
                card_body(
                  plotOutput(ns("plot_comp_grupos"), height = "420px")
                )
              )
            )
          )
        )
      ), # /PESTAÑA 4

      # ══════════════════════════════════════════════════
      # PESTAÑA 5: Código R
      # ══════════════════════════════════════════════════
      nav_panel(
        title = tagList(bs_icon("code-slash", class = "me-1"), "Código R"),
        card_body(
          p(class = "small text-muted",
            "Código reproducible con quanteda para análisis de respuestas abiertas."),
          div(class = "d-flex gap-2 mb-3",
            downloadButton(ns("descarga_codigo_ab"),
                           "Descargar .R",
                           class = "btn-sm btn-outline-primary")
          ),
          verbatimTextOutput(ns("codigo_r_ab")) |>
            tagAppendAttributes(class = "codigo-bloque")
        )
      ) # /PESTAÑA 5

    ) # /navset_card_tab
  ) # /tagList
} # /mod_enc_abiertas_ui


# ── Server ────────────────────────────────────────────────
mod_enc_abiertas_server <- function(id) {
  moduleServer(id, function(input, output, session) {
    ns <- session$ns

    # ── Datos crudos ─────────────────────────────────────
    datos_ab <- reactive({
      if (fuente_ab_activa() == "ejemplo") {
        return(EJEMPLO_ABIERTAS)
      }
      req(input$archivo_abiertas)
      ext <- tools::file_ext(input$archivo_abiertas$name)
      df  <- if (ext == "xlsx")
        readxl::read_excel(input$archivo_abiertas$datapath)
      else
        readr::read_csv(input$archivo_abiertas$datapath,
                        show_col_types = FALSE)
      as.data.frame(df)
    })

    # ── Selectores de columnas ────────────────────────────
    output$sel_columnas_ui <- renderUI({
      req(datos_ab())
      nms <- names(datos_ab())
      tagList(
        selectInput(ns("col_respuesta"),
                    "Columna de respuestas:",
                    choices  = nms,
                    selected = nms[1]),
        selectInput(ns("col_grupo"),
                    "Columna de grupo (opcional):",
                    choices  = c("(ninguna)" = "", nms),
                    selected = if ("grupo" %in% nms) "grupo" else "")
      )
    })

    # ── DFM procesado ────────────────────────────────────
    dfm_ab    <- reactiveVal(NULL)
    dfm_ab_tfidf <- reactiveVal(NULL)
    datos_con_grupo <- reactiveVal(NULL)

    observeEvent(input$procesar_ab, {
      req(datos_ab(), input$col_respuesta)
      withProgress(message = "Procesando respuestas…", value = 0.3, {
        tryCatch({
          df   <- datos_ab()
          req(input$col_respuesta %in% names(df))
          txts <- as.character(df[[input$col_respuesta]])
          txts <- txts[!is.na(txts) & nchar(trimws(txts)) > 0]

          idioma <- detectar_idioma(paste(txts, collapse = " "))
          corp   <- quanteda::corpus(txts)

          toks <- quanteda::tokens(
            corp,
            remove_numbers   = input$norm_numeros,
            remove_punct     = input$norm_puntuacion,
            remove_symbols   = TRUE,
            remove_separators = TRUE
          ) |>
            quanteda::tokens_tolower() |>
            quanteda::tokens_remove(
              pattern  = quanteda::stopwords(idioma),
              padding  = FALSE
            ) |>
            quanteda::tokens_select(min_nchar = input$min_nchar_ab)

          if (input$unificar_terminos) {
            toks <- quanteda::tokens_wordstem(toks, language = idioma)
          }

          dfm_base <- quanteda::dfm(toks)
          dfm_ab(dfm_base)
          dfm_ab_tfidf(quanteda::dfm_tfidf(dfm_base))

          # Guardar datos con columna de grupo si existe
          col_g <- input$col_grupo
          if (!is.null(col_g) && col_g != "" && col_g %in% names(df)) {
            grupos <- as.character(df[[col_g]])[
              !is.na(df[[input$col_respuesta]]) &
              nchar(trimws(df[[input$col_respuesta]])) > 0
            ]
            datos_con_grupo(data.frame(
              respuesta = txts,
              grupo     = grupos,
              stringsAsFactors = FALSE
            ))
          } else {
            datos_con_grupo(NULL)
          }

          incProgress(0.7)
        }, error = function(e) {
          showNotification(paste("Error:", conditionMessage(e)),
                           type = "error", duration = 6)
        })
      })
    })

    # Inicializar con ejemplo automáticamente
    observe({
      req(fuente_ab_activa() == "ejemplo")
      if (is.null(dfm_ab())) {
        shinyjs::click(ns("procesar_ab"))
      }
    })

    # ── Estado ───────────────────────────────────────────
    output$estado_ab_ui <- renderUI({
      if (is.null(dfm_ab())) {
        return(div(
          class = "alert alert-info small py-2 px-3",
          bs_icon("info-circle", class = "me-1"),
          "Seleccioná los datos y hacé clic en ", strong("Procesar respuestas"), "."
        ))
      }
      dfm <- dfm_ab()
      div(
        class = "alert alert-success small py-2 px-3",
        bs_icon("check-circle-fill", class = "me-1"),
        strong("Procesado. "),
        ndoc(dfm), " respuestas · ",
        nfeat(dfm), " términos únicos"
      )
    })

    # Fuente activa archivo
    output$fuente_ab_es_archivo <- reactive({
      !is.null(input$fuente_archivo_ab) &&
        length(input$fuente_archivo_ab) > 0 &&
        input$fuente_archivo_ab == "archivo"
    })
    outputOptions(output, "fuente_ab_es_archivo", suspendWhenHidden = FALSE)

    # Deselección mutua
    observeEvent(input$fuente_abiertas, {
      req(input$fuente_abiertas)
      updateRadioButtons(session, "fuente_archivo_ab", selected = character(0))
    })
    observeEvent(input$fuente_archivo_ab, {
      req(input$fuente_archivo_ab)
      updateRadioButtons(session, "fuente_abiertas", selected = character(0))
    })

    # Fuente activa para el server
    fuente_ab_activa <- reactive({
      if (!is.null(input$fuente_archivo_ab) &&
          length(input$fuente_archivo_ab) > 0 &&
          input$fuente_archivo_ab == "archivo") {
        "archivo"
      } else {
        "ejemplo"
      }
    })

    output$preview_ab <- renderDT({
      req(datos_ab())
      datatable(
        head(datos_ab(), 8),
        options  = list(dom = "t", pageLength = 8, scrollX = TRUE),
        rownames = FALSE,
        class    = "table-sm table-striped"
      )
    })

    # ── Frecuencias ───────────────────────────────────────
    freq_ab_df <- reactive({
      req(dfm_ab())
      top_features_df(dfm_ab(), n = input$ab_n_top)
    })

    output$plot_ab_freq <- renderPlot({
      req(freq_ab_df())
      df <- freq_ab_df() |> dplyr::arrange(dplyr::desc(frequency))
      df$feature <- factor(df$feature, levels = df$feature)

      ggplot2::ggplot(df, ggplot2::aes(x = frequency, y = feature)) +
        ggplot2::geom_col(fill = colores$acento, alpha = 0.85, width = 0.7) +
        ggplot2::geom_text(
          ggplot2::aes(label = frequency),
          hjust = -0.2, size = 3.2, color = colores$texto
        ) +
        ggplot2::xlim(0, max(df$frequency) * 1.18) +
        ggplot2::labs(x = "Frecuencia", y = NULL) +
        ggplot2::theme_minimal(base_size = 13) +
        ggplot2::theme(
          panel.grid.minor   = ggplot2::element_blank(),
          panel.grid.major.y = ggplot2::element_blank()
        )
    })

    output$tabla_ab_freq <- renderDT({
      req(freq_ab_df())
      datatable(
        freq_ab_df(),
        options  = list(pageLength = 15, dom = "tp"),
        rownames = FALSE,
        class    = "table-sm table-striped",
        colnames = c("Término", "Frecuencia", "Rango")
      )
    })

    output$descarga_ab_freq <- downloadHandler(
      filename = function() paste0("frecuencias_abiertas_", Sys.Date(), ".csv"),
      content  = function(file) readr::write_csv(freq_ab_df(), file)
    )

    # ── Nube ─────────────────────────────────────────────
    nube_ab_seed <- reactiveVal(42)
    observeEvent(input$regen_nube_ab, {
      nube_ab_seed(sample.int(10000, 1))
    })

    output$plot_ab_nube <- renderPlot({
      req(dfm_ab())
      set.seed(nube_ab_seed())
      quanteda.textplots::textplot_wordcloud(
        dfm_ab(),
        max_words   = input$ab_nube_max,
        min_count   = input$ab_nube_minfreq,
        color       = colorRampPalette(
          c(colores$advertencia, colores$acento, colores$peligro))(8),
        random_order = FALSE
      )
    })

    # ── Comparación entre grupos ──────────────────────────
    output$tiene_grupo <- reactive({
      !is.null(datos_con_grupo())
    })
    outputOptions(output, "tiene_grupo", suspendWhenHidden = FALSE)

    output$ui_grupo_disponible <- renderUI({
      if (is.null(datos_con_grupo())) {
        div(
          class = "alert alert-warning small py-2 px-3",
          bs_icon("exclamation-triangle", class = "me-1"),
          "Para comparar grupos, seleccioná una columna de grupo en la pestaña ",
          strong("Datos"), " y volvé a procesar."
        )
      }
    })

    output$sel_col_grupo_ui <- renderUI({
      req(datos_con_grupo())
      grupos <- unique(datos_con_grupo()$grupo)
      div(
        p(class = "small text-muted mb-0",
          bs_icon("people-fill", class = "me-1"),
          strong(length(grupos)), " grupos detectados: ",
          paste(grupos, collapse = ", "))
      )
    })

    output$plot_comp_grupos <- renderPlot({
      req(dfm_ab(), datos_con_grupo())
      df_g  <- datos_con_grupo()
      txts  <- df_g$respuesta
      grps  <- df_g$grupo
      idioma <- detectar_idioma(paste(txts, collapse = " "))

      corp <- quanteda::corpus(txts)
      quanteda::docvars(corp, "grupo") <- grps

      toks <- quanteda::tokens(
        corp,
        remove_numbers = input$norm_numeros,
        remove_punct   = input$norm_puntuacion,
        remove_symbols = TRUE
      ) |>
        quanteda::tokens_tolower() |>
        quanteda::tokens_remove(quanteda::stopwords(idioma)) |>
        quanteda::tokens_select(min_nchar = input$min_nchar_ab)

      dfm_g <- quanteda::dfm(toks) |>
        quanteda::dfm_group(groups = grupo)

      metrica <- input$comp_metrica
      if (metrica == "tfidf") {
        dfm_g <- quanteda::dfm_tfidf(dfm_g)
      }

      n <- input$comp_n_top
      top_g <- quanteda.textstats::textstat_frequency(
        dfm_g, n = n, groups = quanteda::docvars(dfm_g, "grupo")
      )

      ggplot2::ggplot(top_g,
        ggplot2::aes(x = frequency,
                     y = tidytext::reorder_within(feature, frequency, group),
                     fill = group)) +
        ggplot2::geom_col(show.legend = FALSE, alpha = 0.85) +
        tidytext::scale_y_reordered() +
        ggplot2::facet_wrap(~group, scales = "free_y") +
        scale_fill_tableau_cb() +
        ggplot2::labs(
          x = if (metrica == "tfidf") "TF-IDF" else "Frecuencia",
          y = NULL
        ) +
        ggplot2::theme_minimal(base_size = 12) +
        ggplot2::theme(panel.grid.minor = ggplot2::element_blank())
    })

    # ── Código R ──────────────────────────────────────────
    codigo_r_ab_gen <- reactive({
      encabezado_script("Preguntas abiertas de encuestas") |>
        paste0(
          "# -- Paquetes ------------------------------------------------\n",
          "library(quanteda)\n",
          "library(quanteda.textstats)\n",
          "library(quanteda.textplots)\n",
          "library(tidytext)\n",
          "library(tidyverse)\n",
          "library(readr)    # o readxl para XLSX\n\n",
          "# -- Datos ----------------------------------------------------\n",
          "df <- read_csv('tu_encuesta.csv')  # o read_excel()\n",
          "# Columna con respuestas: '", input$col_respuesta %||% "respuesta", "'\n",
          "# Columna de grupo:       '", input$col_grupo %||% "grupo", "'\n\n",
          "textos <- df$`", input$col_respuesta %||% "respuesta", "`\n",
          "textos <- textos[!is.na(textos) & nchar(trimws(textos)) > 0]\n\n",
          "# -- Preprocesamiento para texto corto/coloquial --------------\n",
          "corp <- corpus(textos)\n",
          "toks <- tokens(corp,\n",
          "  remove_numbers = ", input$norm_numeros, ",\n",
          "  remove_punct   = ", input$norm_puntuacion, ",\n",
          "  remove_symbols = TRUE\n",
          ") |>\n",
          "  tokens_tolower() |>\n",
          "  tokens_remove(stopwords('es')) |>\n",
          "  tokens_select(min_nchar = ", input$min_nchar_ab, ")\n\n",
          "dfm <- dfm(toks)\n\n",
          "# -- Frecuencias ----------------------------------------------\n",
          "textstat_frequency(dfm, n = ", input$ab_n_top, ")\n\n",
          "# -- Nube de palabras -----------------------------------------\n",
          "textplot_wordcloud(dfm, max_words = ", input$ab_nube_max, ",\n",
          "                   color = c('#F1CE63', '#FC7D0B', '#C85200'))\n\n",
          "# -- Comparación entre grupos ---------------------------------\n",
          "# Agrupá el corpus por la variable de grupo\n",
          "docvars(corp, 'grupo') <- df$`", input$col_grupo %||% "grupo", "`\n",
          "toks_g <- tokens(corp, remove_punct = TRUE) |>\n",
          "  tokens_tolower() |>\n",
          "  tokens_remove(stopwords('es'))\n",
          "dfm_g <- dfm(toks_g) |> dfm_group(groups = grupo)\n\n",
          "# TF-IDF por grupo\n",
          "dfm_tfidf <- dfm_tfidf(dfm_g)\n",
          "textstat_frequency(dfm_tfidf, n = ", input$comp_n_top, ",\n",
          "                   groups = docvars(dfm_g, 'grupo'))\n"
        )
    })

    output$codigo_r_ab <- renderText({ codigo_r_ab_gen() })

    output$descarga_codigo_ab <- downloadHandler(
      filename = function() paste0("enc_abiertas_", format(Sys.Date(), "%Y%m%d"), ".R"),
      content  = function(file) writeLines(codigo_r_ab_gen(), file)
    )

  }) # /moduleServer
} # /mod_enc_abiertas_server

# Helper para null-coalesce en R base
`%||%` <- function(x, y) if (is.null(x) || length(x) == 0 || x == "") y else x
