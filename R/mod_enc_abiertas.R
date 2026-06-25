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
                    div(
                      class = "p-2 mb-2",
                      style = paste0("background:", colores$fondo,
                                     "; border-radius:6px; font-size:12px;"),
                      bs_icon("info-circle", class = "me-1",
                              style = paste0("color:", colores$primario)),
                      tags$strong("Formato esperado del archivo:"),
                      tags$ul(
                        class = "mb-1 mt-1 ps-3",
                        tags$li("Una fila por respuesta"),
                        tags$li("Una columna con el texto de la respuesta"),
                        tags$li("Columnas adicionales opcionales para agrupar",
                                " (ej. sexo, región, edad)"),
                        tags$li("Primera fila: nombres de columnas")
                      ),
                      tags$em("Ejemplo: respuesta, sexo, region")
                    ),
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
              uiOutput(ns("metricas_ab_ui")),
              uiOutput(ns("vista_posterior_ab_ui")),
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
            col_widths = c(3, 9),
            div(
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
              card(
                class = "mt-3",
                card_header(bs_icon("lightbulb", class = "me-1"), "Interpretación"),
                card_body(
                  style = "white-space: normal; word-wrap: break-word;",
                  uiOutput(ns("exp_freq_ab_ui"))
                )
              )
            ),
            div(
              card(
                class = "mb-3",
                card_header(bs_icon("bar-chart-fill", class = "me-1"),
                            "Términos más frecuentes — todas las respuestas"),
                card_body(uiOutput(ns("plot_ab_freq_ui")))
              ),
              card(
                card_header(bs_icon("table", class = "me-1"), "Tabla de frecuencias"),
                card_body(DTOutput(ns("tabla_ab_freq"), height = "250px"))
              )
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
      # PESTAÑA 4: N-gramas
      # ══════════════════════════════════════════════════
      nav_panel(
        title = tagList(bs_icon("text-paragraph", class = "me-1"), "N-gramas"),
        card_body(
          layout_columns(
            col_widths = c(3, 9),
            div(
              card(
                card_header(bs_icon("sliders", class = "me-1"), "Controles"),
                card_body(
                  radioButtons(
                    ns("ab_ngrama_n"),
                    "Tipo:",
                    choices = c(
                      "Bigramas (2 palabras)"  = "2",
                      "Trigramas (3 palabras)" = "3"
                    ),
                    selected = "2"
                  ),
                  sliderInput(ns("ab_ngrama_min_freq"), "Frecuencia mínima:",
                              min = 1, max = 10, value = 1, step = 1),
                  sliderInput(ns("ab_ngrama_top_n"), "N-gramas a mostrar:",
                              min = 5, max = 30, value = 15, step = 5),
                  tags$hr(),
                  actionButton(ns("ab_calcular_ngramas"),
                               tagList(bs_icon("play-fill", class = "me-1"),
                                       "Calcular"),
                               class = "btn btn-primary w-100")
                )
              ),
              card(
                class = "mt-3",
                card_header(bs_icon("lightbulb", class = "me-1"), "Interpretación"),
                card_body(
                  style = "white-space: normal; word-wrap: break-word;",
                  uiOutput(ns("exp_ab_ngramas_ui"))
                )
              )
            ),
            div(
              card(
                class = "mb-3",
                card_header(bs_icon("table", class = "me-1"), "Tabla de n-gramas"),
                card_body(DTOutput(ns("tabla_ab_ngramas"), height = "250px"))
              ),
              card(
                card_header(bs_icon("bar-chart-fill", class = "me-1"),
                            "N-gramas más frecuentes"),
                card_body(uiOutput(ns("plot_ab_ngramas_ui")))
              )
            )
          )
        )
      ), # /PESTAÑA 4


      # ══════════════════════════════════════════════════
      # PESTAÑA 5: Comparación entre grupos
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
              col_widths = c(3, 9),
              div(
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
                  class = "mt-3",
                  card_header(bs_icon("lightbulb", class = "me-1"), "Interpretación"),
                  card_body(
                    style = "white-space: normal; word-wrap: break-word;",
                    uiOutput(ns("exp_comp_grupos_ui"))
                  )
                )
              ),
              card(
                card_header(bs_icon("graph-up-arrow", class = "me-1"),
                            "Términos más distintivos por grupo"),
                card_body(
                  uiOutput(ns("plot_comp_grupos_ui"))
                )
              )
            )
          )
        )
      ), # /PESTAÑA 5

      # ══════════════════════════════════════════════════
      # PESTAÑA 6: Concordancias
      # ══════════════════════════════════════════════════
      nav_panel(
        title = tagList(bs_icon("search", class = "me-1"), "Concordancias"),
        card_body(
          div(
            class = "alert mb-3",
            style = paste0("background:", colores$fondo,
                           "; border-left: 4px solid ", colores$primario, ";"),
            tags$b(
              bs_icon("info-circle", class = "me-1",
                      style = paste0("color:", colores$primario)),
              "¿Qué son las concordancias (KWIC)?"
            ),
            tags$p(class = "small text-muted mb-0 mt-1",
              tags$strong("Key Word In Context"), " muestra cada vez que aparece",
              " una palabra junto al texto que la rodea.",
              " Muy útil para analizar cómo usan los encuestados un término."
            )
          ),
          layout_columns(
            col_widths = c(3, 9),
            div(
              card(
                card_header(bs_icon("sliders", class = "me-1"), "Controles"),
                card_body(
                  textInput(ns("ab_kwic_patron"),
                            "Palabra a buscar:",
                            placeholder = "ej. servicio"),
                  sliderInput(ns("ab_kwic_ventana"),
                              "Ventana de contexto (tokens):",
                              min = 2, max = 10, value = 3, step = 1),
                  tags$hr(),
                  actionButton(ns("ab_buscar_kwic"),
                               tagList(bs_icon("search", class = "me-1"), "Buscar"),
                               class = "btn btn-primary w-100")
                )
              ),
              card(
                class = "mt-3",
                card_header(bs_icon("lightbulb", class = "me-1"), "Interpretación"),
                card_body(
                  style = "white-space: normal; word-wrap: break-word;",
                  uiOutput(ns("exp_ab_kwic_ui"))
                )
              )
            ),
            div(
              card(
                card_header(bs_icon("list-ul", class = "me-1"),
                            "Concordancias encontradas"),
                card_body(
                  uiOutput(ns("ab_kwic_info_ui")),
                  DTOutput(ns("tabla_ab_kwic"))
                )
              )
            )
          )
        )
      ), # /PESTAÑA 6

      # ══════════════════════════════════════════════════
      # PESTAÑA 7: Tópicos (LDA)
      # ══════════════════════════════════════════════════
      nav_panel(
        title = tagList(bs_icon("tag", class = "me-1"), "Tópicos (LDA)"),
        card_body(
          div(
            class = "alert mb-3",
            style = paste0("background:", colores$fondo,
                           "; border-left: 4px solid ", colores$primario, ";"),
            tags$b(
              bs_icon("info-circle", class = "me-1",
                      style = paste0("color:", colores$primario)),
              "¿Qué es LDA?"
            ),
            tags$p(class = "small text-muted mb-1 mt-1",
              tags$strong("Latent Dirichlet Allocation"), " identifica temas",
              " latentes en las respuestas. Cada respuesta es una mezcla de",
              " tópicos y cada tópico una distribución de palabras."
            ),
            tags$p(class = "small text-muted mb-0",
              tags$strong("Útil para:"), " identificar los temas principales",
              " que emergen de las respuestas sin codificación manual.",
              " Probá distintos valores de K para encontrar la mejor solución."
            )
          ),
          layout_columns(
            col_widths = c(3, 9),
            div(
              card(
                card_header(bs_icon("sliders", class = "me-1"), "Controles"),
                card_body(
                  numericInput(ns("ab_lda_k"), "Número de tópicos (K):",
                               value = 3, min = 2, max = 10, step = 1),
                  numericInput(ns("ab_lda_iter"), "Iteraciones Gibbs:",
                               value = 1000, min = 200, max = 3000, step = 200),
                  sliderInput(ns("ab_lda_top_terms"), "Términos por tópico:",
                              min = 3, max = 15, value = 8, step = 1),
                  div(
                    class = "p-2 mb-2",
                    style = paste0("background:", colores$fondo,
                                   "; border-radius:6px; font-size:12px;"),
                    bs_icon("info-circle", class = "me-1",
                            style = paste0("color:", colores$primario)),
                    "Para encuestas cortas se recomienda K entre 2 y 5.",
                    " Cada respuesta se divide en segmentos si es muy breve."
                  ),
                  tags$hr(),
                  actionButton(ns("ab_ajustar_lda"),
                               tagList(bs_icon("play-fill", class = "me-1"),
                                       "Ajustar LDA"),
                               class = "btn btn-primary w-100"),
                  div(class = "mt-2", uiOutput(ns("ab_estado_lda_ui"))),
                  uiOutput(ns("ab_nombres_topicos_ui"))
                )
              ),
              card(
                class = "mt-3",
                card_header(bs_icon("lightbulb", class = "me-1"), "Interpretación"),
                card_body(
                  style = "white-space: normal; word-wrap: break-word;",
                  uiOutput(ns("exp_ab_lda_ui"))
                )
              )
            ),
            div(
              card(
                class = "mb-3",
                card_header(bs_icon("bar-chart-fill", class = "me-1"),
                            "Términos por tópico (β)"),
                card_body(uiOutput(ns("plot_ab_lda_beta_ui")))
              ),
              card(
                card_header(bs_icon("table", class = "me-1"),
                            "Tópico dominante por respuesta (γ)"),
                card_body(
                  p(class = "small text-muted mb-2",
                    "Tópico con mayor probabilidad por respuesta."),
                  DTOutput(ns("tabla_ab_lda_gamma"))
                )
              )
            )
          )
        )
      ), # /PESTAÑA 7


      # ══════════════════════════════════════════════════
      # PESTAÑA 8: Código R
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
      ) # /PESTAÑA 8

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
    dfm_ab      <- reactiveVal(NULL)
    dfm_ab_tfidf <- reactiveVal(NULL)
    toks_ab      <- reactiveVal(NULL)
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

          toks_ab(toks)
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

    output$metricas_ab_ui <- renderUI({
      req(datos_ab(), dfm_ab())
      df  <- datos_ab()
      col <- input$col_respuesta

      # Obtener textos de la columna correcta
      textos <- if (!is.null(col) && col %in% names(df)) df[[col]] else df[[1]]
      textos <- as.character(textos)

      n_total   <- length(textos)
      n_vacias  <- sum(is.na(textos) | trimws(textos) == "")
      n_validas <- n_total - n_vacias
      long_med  <- round(mean(nchar(textos[textos != ""], type = "chars"),
                              na.rm = TRUE), 1)
      n_palabras_med <- round(mean(
        sapply(strsplit(trimws(textos), "\\s+"), length),
        na.rm = TRUE), 1)
      n_unicos  <- nfeat(dfm_ab())

      idx_max <- which.max(nchar(textos))
      idx_min <- which.min(nchar(textos[nchar(textos) > 0]))

      tagList(
        br(),
        div(
          style = paste0("background:", colores$fondo,
                         "; border-radius:8px; padding:12px;",
                         " border:1px solid #e0e0e0;"),
          tags$b(
            bs_icon("bar-chart-line", class = "me-1",
                    style = paste0("color:", colores$primario)),
            "Estadísticas descriptivas"
          ),
          layout_columns(
            col_widths = c(6, 6),
            class = "mt-2",
            vbox_card("chat-left-text", "Respuestas",
                      format(n_validas, big.mark = ","), colores$primario),
            vbox_card("x-circle",       "Vacías",
                      format(n_vacias,  big.mark = ","), colores$peligro)
          ),
          layout_columns(
            col_widths = c(6, 6),
            class = "mt-2",
            vbox_card("fonts",   "Palabras únicas",
                      format(n_unicos, big.mark = ","), colores$acento),
            vbox_card("rulers",  "Long. media",
                      paste0(long_med, " car."), colores$secundario)
          ),
          div(
            class = "mt-2 p-2 small",
            style = paste0("background:white; border-radius:6px;"),
            tags$b("Resp. más larga: "),
            tags$span(
              class = "text-muted",
              substr(textos[idx_max], 1, 80),
              if (nchar(textos[idx_max]) > 80) "…" else ""
            )
          )
        )
      )
    })

    # Fuente activa archivo
    output$vista_posterior_ab_ui <- renderUI({
      req(toks_ab(), datos_ab())
      col    <- input$col_respuesta
      df     <- datos_ab()
      txts   <- if (!is.null(col) && col %in% names(df)) df[[col]] else df[[1]]
      txts   <- as.character(txts)
      txts   <- txts[!is.na(txts) & nchar(trimws(txts)) > 0]
      n_pre  <- sum(sapply(strsplit(trimws(txts), "\\s+"), length))
      tok_vec    <- unlist(quanteda::as.list(toks_ab()))
      n_post     <- length(tok_vec)
      pct        <- round(max(0L, n_pre - n_post) / max(n_pre, 1) * 100)
      muestra    <- head(tok_vec, 100)
      chips <- lapply(muestra, function(w) {
        tags$span(w, class = "badge me-1 mb-1",
                  style = paste0("background:", colores$fondo,
                                 "; color:", colores$texto,
                                 "; border:1px solid #ddd;",
                                 " font-weight:400; font-size:12px;"))
      })
      if (length(tok_vec) > 100)
        chips <- c(chips, list(tags$span(
          paste0("… y ", length(tok_vec) - 100, " más"),
          class = "text-muted small")))
      tagList(
        br(),
        div(
          class = "p-3",
          style = paste0("background:", colores$fondo,
                         "; border-radius:8px; border:1px solid #e0e0e0;"),
          div(class = "d-flex justify-content-between align-items-center mb-2",
            tags$b(bs_icon("eye", class = "me-1",
                           style = paste0("color:", colores$primario)),
                   "Tokens resultantes"),
            tags$span(class = "badge",
                      style = paste0("background:", colores$acento,
                                     "; color:white; font-size:11px;"),
                      paste0(n_pre, " → ", n_post, " tokens (−", pct, "%)"))
          ),
          div(style = "line-height:2;", chips)
        )
      )
    })

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

    output$plot_ab_freq_ui <- renderUI({
      req(freq_ab_df())
      n      <- nrow(freq_ab_df())
      altura <- max(300L, n * 38L)
      plotOutput(ns("plot_ab_freq"), height = paste0(altura, "px"))
    })

    output$plot_ab_freq <- renderPlot({
      req(freq_ab_df())
      df <- freq_ab_df() |> dplyr::arrange(dplyr::desc(frequency))
      df$feature <- factor(df$feature, levels = df$feature)

      ggplot2::ggplot(df, ggplot2::aes(x = frequency, y = feature)) +
        ggplot2::geom_col(fill = colores$acento, alpha = 0.85, width = 0.6) +
        ggplot2::geom_text(
          ggplot2::aes(label = frequency),
          hjust = -0.2, size = 3.2, color = colores$texto
        ) +
        ggplot2::expand_limits(x = max(df$frequency) * 1.2) +
        ggplot2::scale_x_continuous(
          expand = ggplot2::expansion(mult = c(0, 0.18))
        ) +
        ggplot2::labs(x = "Frecuencia", y = NULL) +
        ggplot2::theme_minimal(base_size = 13) +
        ggplot2::theme(
          panel.grid.minor   = ggplot2::element_blank(),
          panel.grid.major.y = ggplot2::element_blank(),
          axis.title.x       = ggplot2::element_text(margin = ggplot2::margin(t = 8))
        )
    })

    output$exp_freq_ab_ui <- renderUI({
      req(freq_ab_df())
      df  <- freq_ab_df()
      top <- df[which.max(df$frequency), ]
      p(paste0("La palabra más frecuente fue '", top$feature,
               "' con ", top$frequency, " ocurrencias. ",
               "El vocabulario tiene ", nrow(df), " términos únicos."))
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
      suppressWarnings(
        quanteda.textplots::textplot_wordcloud(
          dfm_ab(),
          max_words    = input$ab_nube_max,
          min_count    = input$ab_nube_minfreq,
          color        = colorRampPalette(
            c(colores$advertencia, colores$acento, colores$peligro))(8),
          random_order = FALSE
        )
      )
    })

    # ── N-gramas ──────────────────────────────────────────
    ab_ngramas_df <- reactiveVal(NULL)

    observeEvent(input$ab_calcular_ngramas, {
      req(toks_ab())
      withProgress(message = "Calculando n-gramas…", value = 0.5, {
        tryCatch({
          n      <- as.integer(input$ab_ngrama_n)
          tok    <- quanteda::tokens_ngrams(toks_ab(), n = n)
          dfm_ng <- quanteda::dfm(tok)
          dfm_ng <- quanteda::dfm_trim(dfm_ng,
                                       min_termfreq = input$ab_ngrama_min_freq)
          freq_ng <- quanteda.textstats::textstat_frequency(
            dfm_ng, n = input$ab_ngrama_top_n)
          if (nrow(freq_ng) == 0) {
            showNotification(
              "No hay n-gramas con esa frecuencia mínima. Reducí el umbral.",
              type = "warning", duration = 4)
            return()
          }
          freq_ng$feature <- gsub("_", " ", freq_ng$feature)
          ab_ngramas_df(freq_ng)
          incProgress(0.5)
        }, error = function(e) {
          showNotification(paste("Error:", conditionMessage(e)),
                           type = "error", duration = 5)
        })
      })
    })

    output$plot_ab_ngramas_ui <- renderUI({
      req(ab_ngramas_df())
      n      <- nrow(ab_ngramas_df())
      altura <- max(300L, n * 38L)
      plotOutput(ns("plot_ab_ngramas"), height = paste0(altura, "px"))
    })

    output$plot_ab_ngramas <- renderPlot({
      req(ab_ngramas_df())
      df         <- ab_ngramas_df()
      df$feature <- factor(df$feature, levels = rev(df$feature))
      ggplot2::ggplot(df, ggplot2::aes(x = frequency, y = feature)) +
        ggplot2::geom_col(fill = colores$acento, alpha = 0.85, width = 0.6) +
        ggplot2::geom_text(
          ggplot2::aes(label = frequency),
          hjust = -0.2, size = 3.5, color = colores$texto
        ) +
        ggplot2::expand_limits(x = max(df$frequency) * 1.2) +
        ggplot2::scale_x_continuous(
          expand = ggplot2::expansion(mult = c(0, 0.18))
        ) +
        ggplot2::labs(x = "Frecuencia", y = NULL) +
        ggplot2::theme_minimal(base_size = 13) +
        ggplot2::theme(
          panel.grid.minor   = ggplot2::element_blank(),
          panel.grid.major.y = ggplot2::element_blank(),
          axis.title.x       = ggplot2::element_text(margin = ggplot2::margin(t = 8))
        )
    })

    output$tabla_ab_ngramas <- renderDT({
      req(ab_ngramas_df())
      df <- ab_ngramas_df()[, c("feature", "frequency", "rank")]
      datatable(
        df,
        options  = list(pageLength = 10, dom = "tp",
                        autoWidth = FALSE, scrollX = FALSE),
        rownames = FALSE,
        class    = "table-sm table-striped",
        colnames = c("N-grama", "Frecuencia", "Rango")
      )
    })

    output$exp_ab_ngramas_ui <- renderUI({
      req(ab_ngramas_df())
      df   <- ab_ngramas_df()
      top  <- df[1, ]
      tipo <- if (input$ab_ngrama_n == "2") "bigrama" else "trigrama"
      p(paste0("El ", tipo, " más frecuente fue '", top$feature,
               "' con ", top$frequency, " ocurrencias."))
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

    output$plot_comp_grupos_ui <- renderUI({
      req(datos_con_grupo())
      df_g   <- datos_con_grupo()
      grupos <- unique(df_g$grupo)
      n_top  <- input$comp_n_top
      n_rows <- ceiling(length(grupos) / 2)
      altura <- max(350L, n_rows * n_top * 28L)
      plotOutput(ns("plot_comp_grupos"), height = paste0(altura, "px"))
    })

    output$exp_comp_grupos_ui <- renderUI({
      req(datos_con_grupo())
      df_g   <- datos_con_grupo()
      grupos <- unique(df_g$grupo)
      metrica <- if (input$comp_metrica == "tfidf") "TF-IDF" else "frecuencia"
      p(paste0(
        "Se compararon ", length(grupos), " grupos: ",
        paste(grupos, collapse = " y "), ". ",
        "La comparación usa ", metrica, " como métrica. ",
        "Los términos más altos en cada panel son los más distintivos de ese grupo."
      ))
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

    # ── Concordancias (KWIC) ──────────────────────────────
    ab_kwic_rv <- reactiveVal(NULL)

    observeEvent(input$ab_buscar_kwic, {
      req(toks_ab(), nchar(trimws(input$ab_kwic_patron)) > 0)
      tryCatch({
        kwic_res <- quanteda::kwic(
          toks_ab(),
          pattern   = input$ab_kwic_patron,
          window    = input$ab_kwic_ventana,
          valuetype = "glob"
        )
        ab_kwic_rv(as.data.frame(kwic_res))
      }, error = function(e) {
        showNotification(paste("Error:", conditionMessage(e)),
                         type = "error", duration = 5)
      })
    })

    output$ab_kwic_info_ui <- renderUI({
      df <- ab_kwic_rv()
      if (is.null(df)) return(NULL)
      div(class = "alert alert-info small py-2 px-3 mb-2",
          bs_icon("info-circle", class = "me-1"),
          strong(nrow(df)), " ocurrencia(s) encontrada(s).")
    })

    output$tabla_ab_kwic <- renderDT({
      req(ab_kwic_rv())
      df_show <- data.frame(
        Antes   = ab_kwic_rv()$pre,
        Palabra = ab_kwic_rv()$keyword,
        Después = ab_kwic_rv()$post,
        stringsAsFactors = FALSE
      )
      datatable(
        df_show,
        options  = list(pageLength = 15, dom = "tp",
                        autoWidth = FALSE, scrollX = FALSE,
                        columnDefs = list(
                          list(className = "dt-right",  targets = 0),
                          list(className = "dt-center", targets = 1),
                          list(className = "dt-left",   targets = 2)
                        )),
        rownames = FALSE,
        class    = "table-sm table-striped",
        escape   = FALSE
      ) |>
        DT::formatStyle("Palabra", fontWeight = "bold",
                        color = colores$acento)
    })

    output$exp_ab_kwic_ui <- renderUI({
      req(ab_kwic_rv())
      n <- nrow(ab_kwic_rv())
      p(paste0("Se encontraron ", n, " ocurrencia(s) del término '",
               input$ab_kwic_patron, "' en las respuestas."))
    })

    # ── Tópicos LDA ───────────────────────────────────────
    ab_modelo_lda <- reactiveVal(NULL)

    observeEvent(input$ab_ajustar_lda, {
      req(dfm_ab())
      withProgress(message = "Ajustando LDA…", value = 0.3, {
        tryCatch({
          dfm <- dfm_ab()
          # Chunk dinámico si hay pocos documentos
          if (ndoc(dfm) < max(input$ab_lda_k, 2)) {
            chunk_sz <- max(5L, as.integer(ndoc(dfm_ab()) / (input$ab_lda_k * 2)))
            toks_seg <- quanteda::tokens_chunk(toks_ab(), size = chunk_sz)
            dfm <- quanteda::dfm(toks_seg)
          }
          dfm  <- quanteda::dfm_trim(dfm, min_termfreq = 1)
          sumas <- Matrix::rowSums(quanteda::as.dfm(dfm))
          dfm  <- dfm[sumas > 0, ]
          validate(
            need(ndoc(dfm) >= max(input$ab_lda_k, 2) && nfeat(dfm) >= 2,
                 paste0("Se necesitan al menos K = ", input$ab_lda_k,
                        " respuestas con ≥ 2 términos. Reducí K o añadí más respuestas."))
          )
          lda <- topicmodels::LDA(
            quanteda::convert(dfm, to = "topicmodels"),
            k       = input$ab_lda_k,
            method  = "Gibbs",
            control = list(seed = 42, iter = input$ab_lda_iter)
          )
          ab_modelo_lda(lda)
          incProgress(0.7)
          output$ab_estado_lda_ui <- renderUI({
            div(class = "alert alert-success small py-2 px-3",
                bs_icon("check-circle-fill", class = "me-1"),
                strong("Modelo ajustado. "),
                input$ab_lda_k, " tópicos identificados.")
          })
        }, error = function(e) {
          showNotification(paste("Error LDA:", conditionMessage(e)),
                           type = "error", duration = 6)
        })
      })
    })

    output$ab_nombres_topicos_ui <- renderUI({
      req(ab_modelo_lda())
      k <- input$ab_lda_k
      tagList(
        tags$hr(),
        tags$p(class = "small fw-semibold mb-1",
               bs_icon("tag", class = "me-1"), "Nombrar tópicos (opcional)"),
        tags$p(class = "small text-muted mb-2",
               "Mirá el gráfico β e ingresá un nombre descriptivo para cada tópico."),
        lapply(seq_len(k), function(i) {
          textInput(
            ns(paste0("ab_nombre_topico_", i)),
            label = paste0("Tópico ", i, ":"),
            placeholder = paste0("ej. Problemas ambientales"),
            value = isolate(input[[paste0("ab_nombre_topico_", i)]]) %||% ""
          )
        })
      )
    })

    output$plot_ab_lda_beta_ui <- renderUI({
      req(ab_modelo_lda())
      k      <- input$ab_lda_k
      n_rows <- ceiling(k / 3)
      altura <- max(400L, n_rows * 300L)
      plotOutput(ns("plot_ab_lda_beta"), height = paste0(altura, "px"))
    })

    output$plot_ab_lda_beta <- renderPlot({
      req(ab_modelo_lda())
      n <- input$ab_lda_top_terms
      k <- input$ab_lda_k
      nombres <- sapply(seq_len(k), function(i) {
        nm <- input[[paste0("ab_nombre_topico_", i)]]
        if (is.null(nm) || trimws(nm) == "") paste0("Tópico ", i) else trimws(nm)
      })
      beta_df <- tidytext::tidy(ab_modelo_lda(), matrix = "beta") |>
        dplyr::group_by(topic) |>
        dplyr::slice_max(beta, n = n, with_ties = FALSE) |>
        dplyr::ungroup() |>
        dplyr::mutate(
          topic = nombres[topic],
          term  = tidytext::reorder_within(term, beta, topic)
        )
      ggplot2::ggplot(beta_df,
        ggplot2::aes(x = beta, y = term, fill = topic)) +
        ggplot2::geom_col(show.legend = FALSE, alpha = 0.85, width = 0.7) +
        tidytext::scale_y_reordered() +
        ggplot2::facet_wrap(~topic, scales = "free_y",
                            ncol = min(input$ab_lda_k, 3)) +
        scale_fill_tableau_cb() +
        ggplot2::labs(x = "Probabilidad del término (β)", y = NULL) +
        ggplot2::theme_minimal(base_size = 12) +
        ggplot2::theme(
          panel.grid.minor = ggplot2::element_blank(),
          strip.text       = ggplot2::element_text(face = "bold", size = 12),
          axis.text.y      = ggplot2::element_text(size = 10),
          plot.margin      = ggplot2::margin(8, 12, 8, 8)
        )
    }, res = 96)

    output$tabla_ab_lda_gamma <- renderDT({
      req(ab_modelo_lda())
      gamma_df <- tidytext::tidy(ab_modelo_lda(), matrix = "gamma") |>
        dplyr::group_by(document) |>
        dplyr::slice_max(gamma, n = 1, with_ties = FALSE) |>
        dplyr::ungroup() |>
        dplyr::transmute(
          Respuesta          = document,
          `Tópico dominante` = {
            k <- input$ab_lda_k
            nombres <- sapply(seq_len(k), function(i) {
              nm <- input[[paste0("ab_nombre_topico_", i)]]
              if (is.null(nm) || trimws(nm) == "") paste0("Tópico ", i) else trimws(nm)
            })
            nombres[topic]
          },
          `γ máximo`         = round(gamma, 3)
        ) |>
        dplyr::arrange(Respuesta)
      datatable(
        gamma_df,
        options  = list(pageLength = 10, dom = "tp",
                        autoWidth = FALSE, scrollX = FALSE),
        rownames = FALSE,
        class    = "table-sm table-condensed"
      ) |> DT::formatRound(columns = "γ máximo", digits = 3)
    })

    output$exp_ab_lda_ui <- renderUI({
      req(ab_modelo_lda())
      gamma_df <- tidytext::tidy(ab_modelo_lda(), matrix = "gamma") |>
        dplyr::group_by(document) |>
        dplyr::slice_max(gamma, n = 1, with_ties = FALSE) |>
        dplyr::ungroup()
      topic_dom <- gamma_df |>
        dplyr::count(topic) |>
        dplyr::slice_max(n, n = 1, with_ties = FALSE)
      p(paste0("Se identificaron ", input$ab_lda_k, " tópicos. ",
               "El tópico dominante en la mayoría de respuestas fue ",
               {
                 nm <- input[[paste0("ab_nombre_topico_", topic_dom$topic)]]
                 if (is.null(nm) || trimws(nm) == "")
                   paste0("Tópico ", topic_dom$topic)
                 else trimws(nm)
               },
               " (", topic_dom$n, " respuestas)."))
    })

    # ── Código R ──────────────────────────────────────────
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
