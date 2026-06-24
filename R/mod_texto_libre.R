# ============================================================
# mod_texto_libre.R — Análisis de texto libre
# StatText · StatSuite · Manuel Spínola · ICOMVIS · UNA
#
# Análisis: frecuencia de términos, nube de palabras,
# coocurrencias, análisis de sentimiento, tópicos (LDA)
# Motor: quanteda + topicmodels + tidytext
# ============================================================

# ── Textos de ejemplo ──────────────────────────────────────
EJEMPLOS_TEXTO <- list(
  discurso = list(
    titulo = "Discurso político (simulado)",
    texto  = paste(
      "La participación ciudadana en los procesos democráticos ha experimentado",
      "transformaciones significativas en las últimas décadas. Los movimientos",
      "sociales emergentes demandan mayor transparencia y rendición de cuentas",
      "por parte del Estado. La desigualdad estructural continúa siendo uno de",
      "los principales obstáculos para el desarrollo equitativo de la sociedad.",
      "Las políticas públicas deben incorporar perspectivas interseccionales para",
      "abordar las múltiples dimensiones de la exclusión social. El acceso a",
      "derechos fundamentales como la salud, la educación y la vivienda sigue",
      "siendo diferencial según clase social, género y etnia. La sociedad civil",
      "organizada ha cobrado protagonismo en la denuncia de irregularidades y en",
      "la construcción de alternativas colectivas. El fortalecimiento institucional",
      "requiere mecanismos efectivos de control ciudadano y rendición de cuentas.",
      "La participación política de los jóvenes representa un desafío central",
      "para la democracia del siglo XXI. Las organizaciones comunitarias articulan",
      "demandas sociales desde los territorios, generando nuevas formas de",
      "representación política fuera de los canales tradicionales.",
      collapse = " "
    )
  ),
  entrevista = list(
    titulo = "Entrevista sobre territorio (simulado)",
    texto  = paste(
      "El territorio es más que el espacio físico donde vivimos. Es la memoria",
      "colectiva de nuestras comunidades, es la historia de nuestras familias.",
      "Cuando llegan las empresas extractivas, no solo se llevan los recursos,",
      "se llevan también la identidad. Hemos resistido durante generaciones.",
      "La tierra no es una mercancía, es un bien común que heredamos de nuestros",
      "ancestros y tenemos la obligación de cuidar para los que vienen después.",
      "Los conflictos territoriales han generado divisiones profundas en la comunidad.",
      "Algunos jóvenes se van porque no ven futuro aquí, pero otros vuelven porque",
      "entienden que la lucha vale la pena. El Estado ha sido ausente en los momentos",
      "más críticos. Las instituciones no llegan, o cuando llegan ya es demasiado tarde.",
      "Nosotros hemos construido nuestras propias formas de organización, nuestros",
      "propios sistemas de cuidado. La solidaridad comunitaria es lo que nos mantiene.",
      collapse = " "
    )
  )
)

# ── UI ────────────────────────────────────────────────────
mod_texto_libre_ui <- function(id) {
  ns <- NS(id)

  tagList(
    div(
      class = "py-3 px-2",
      h4(
        bs_icon("file-text", class = "me-2"),
        "Análisis de texto libre",
        style = paste0("color:", colores$primario, "; font-weight:700;")
      ),
      p(
        class = "text-muted mb-0",
        "Documentos, artículos, discursos, entrevistas. Frecuencia de términos,",
        " coocurrencias, nube de palabras, sentimiento y tópicos (LDA) con",
        tags$strong("quanteda"), " y ", tags$strong("topicmodels"), "."
      )
    ),

    navset_card_tab(

      # ══════════════════════════════════════════════════
      # PESTAÑA 1: Corpus
      # ══════════════════════════════════════════════════
      nav_panel(
        title = tagList(bs_icon("upload", class = "me-1"), "Corpus"),
        card_body(
          layout_columns(
            col_widths = c(5, 7),

            # Panel izquierdo: carga
            div(
              tags$b(bs_icon("database", class = "me-1"), "Fuente de texto"),
              p(class = "small text-muted mt-1 mb-3",
                "Pegá texto directamente, subí un archivo TXT o usá un ejemplo."),

              radioButtons(
                ns("fuente_texto"),
                label = NULL,
                choices = c(
                  "Pegar texto"                   = "pegar",
                  "Subir archivo"                 = "archivo",
                  "Ejemplo: discurso político"    = "discurso",
                  "Ejemplo: entrevista territorial" = "entrevista"
                ),
                selected = "pegar"
              ),

              conditionalPanel(
                condition = paste0("input['", ns("fuente_texto"), "'] == 'pegar'"),
                textAreaInput(
                  ns("texto_input"),
                  label = NULL,
                  placeholder = "Pegá aquí tu texto...",
                  rows = 8,
                  width = "100%"
                )
              ),

              conditionalPanel(
                condition = paste0("input['", ns("fuente_texto"), "'] == 'archivo'"),
                fileInput(
                  ns("archivo_texto"),
                  label = NULL,
                  accept = c(".txt", ".csv", ".docx", ".odt", ".pdf"),
                  buttonLabel = tagList(
                    bs_icon("folder2-open", class = "me-1"), "Examinar…"),
                  placeholder = "Sin archivo seleccionado"
                ),
                div(
                  class = "p-2 mb-2",
                  style = paste0("background:", colores$fondo,
                                 "; border-radius:6px; font-size:12px;"),
                  bs_icon("info-circle", class = "me-1",
                          style = paste0("color:", colores$primario)),
                  tags$strong("Formatos aceptados: "),
                  ".txt  ·  .csv  ·  .docx  ·  .odt  ·  .pdf"
                ),
                conditionalPanel(
                  condition = paste0(
                    "input['", ns("fuente_texto"), "'] == 'archivo' && output['",
                    ns("archivo_es_csv"), "']"
                  ),
                  selectInput(
                    ns("col_texto"),
                    "Columna de texto (CSV):",
                    choices = NULL
                  )
                )
              ),

              tags$hr(),
              tags$b(bs_icon("sliders", class = "me-1"), "Preprocesamiento"),
              p(class = "small text-muted mt-1 mb-2",
                "El idioma se detecta automáticamente."),
              checkboxInput(ns("remover_numeros"),  "Remover números",     TRUE),
              checkboxInput(ns("remover_punt"),     "Remover puntuación",  TRUE),
              checkboxInput(ns("aplicar_stemming"), "Aplicar stemming",    FALSE),
              numericInput(ns("min_nchar"), "Longitud mínima de término:",
                           value = 3, min = 1, max = 10, step = 1),

              tags$hr(),
              actionButton(
                ns("analizar"),
                label = tagList(
                  bs_icon("play-fill", class = "me-1"), "Analizar corpus"),
                class = "btn btn-primary w-100"
              )
            ),

            # Panel derecho: estado y métricas del corpus
            div(
              uiOutput(ns("estado_corpus_ui")),
              br(),
              uiOutput(ns("metricas_corpus_ui"))
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
                sliderInput(ns("n_top_terms"), "Términos a mostrar:",
                            min = 5, max = 50, value = 20, step = 5),
                selectInput(
                  ns("freq_orden"),
                  "Ordenar por:",
                  choices = c(
                    "Frecuencia (descendente)" = "desc",
                    "Frecuencia (ascendente)"  = "asc",
                    "Alfabético"               = "alfa"
                  )
                ),
                tags$hr(),
                downloadButton(ns("descarga_freq"),
                               "Descargar tabla",
                               class = "btn-sm btn-outline-primary w-100")
              )
            ),
            div(
              card(
                class = "mb-3",
                card_header(
                  bs_icon("bar-chart-fill", class = "me-1"),
                  "Términos más frecuentes"
                ),
                card_body(
                  plotOutput(ns("plot_freq"), height = "380px")
                )
              )
            )
          ),
          div(class = "mt-3",
            h5(style = paste0("color:", colores$primario, "; font-weight:700;"),
               "Tabla de frecuencias"),
            DTOutput(ns("tabla_freq"))
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
                sliderInput(ns("nube_max_words"), "Máximo de palabras:",
                            min = 20, max = 200, value = 80, step = 10),
                sliderInput(ns("nube_min_freq"), "Frecuencia mínima:",
                            min = 1, max = 20, value = 2, step = 1),
                selectInput(
                  ns("nube_color"),
                  "Paleta:",
                  choices = c(
                    "Azul (primario)"   = "blue",
                    "Naranja (acento)"  = "orange",
                    "Multicolor"        = "multi"
                  )
                ),
                actionButton(ns("regen_nube"),
                             tagList(bs_icon("arrow-clockwise", class = "me-1"),
                                     "Regenerar"),
                             class = "btn btn-outline-primary btn-sm w-100 mt-2")
              )
            ),
            card(
              class = "wordcloud-wrap",
              card_body(
                plotOutput(ns("plot_nube"), height = "480px")
              )
            )
          )
        )
      ), # /PESTAÑA 3

      # ══════════════════════════════════════════════════
      # PESTAÑA 4: Coocurrencias
      # ══════════════════════════════════════════════════
      nav_panel(
        title = tagList(bs_icon("diagram-3", class = "me-1"), "Coocurrencias"),
        card_body(
          p(class = "small text-muted mb-3",
            "Palabras que aparecen juntas frecuentemente en el mismo contexto",
            " (ventana deslizante). Útil para identificar conceptos compuestos",
            " y redes semánticas."),
          layout_columns(
            col_widths = c(4, 8),
            card(
              card_header(bs_icon("sliders", class = "me-1"), "Controles"),
              card_body(
                sliderInput(ns("cooc_ventana"), "Ventana de contexto (tokens):",
                            min = 2, max = 10, value = 5, step = 1),
                sliderInput(ns("cooc_min_count"), "Frecuencia mínima de par:",
                            min = 1, max = 20, value = 2, step = 1),
                sliderInput(ns("cooc_top_n"), "Pares a visualizar:",
                            min = 10, max = 50, value = 20, step = 5),
                actionButton(ns("calcular_cooc"),
                             tagList(bs_icon("play-fill", class = "me-1"),
                                     "Calcular"),
                             class = "btn btn-primary btn-sm w-100 mt-2")
              )
            ),
            div(
              card(
                card_header(bs_icon("bar-chart-steps", class = "me-1"),
                            "Pares de términos más coocurrentes"),
                card_body(
                  plotOutput(ns("plot_cooc"), height = "400px")
                )
              )
            )
          ),
          div(class = "mt-3",
            DTOutput(ns("tabla_cooc"))
          )
        )
      ), # /PESTAÑA 4

      # ══════════════════════════════════════════════════
      # PESTAÑA 5: Tópicos (LDA)
      # ══════════════════════════════════════════════════
      nav_panel(
        title = tagList(bs_icon("tag", class = "me-1"), "Tópicos (LDA)"),
        card_body(
          p(class = "small text-muted mb-3",
            "Latent Dirichlet Allocation (LDA) descubre temas latentes en el corpus.",
            " Cada tópico se representa por sus términos más probable."),
          layout_columns(
            col_widths = c(4, 8),
            card(
              card_header(bs_icon("sliders", class = "me-1"), "Controles"),
              card_body(
                numericInput(ns("lda_k"), "Número de tópicos (K):",
                             value = 4, min = 2, max = 15, step = 1),
                numericInput(ns("lda_iter"), "Iteraciones Gibbs:",
                             value = 1000, min = 200, max = 5000, step = 200),
                sliderInput(ns("lda_top_terms"), "Términos por tópico:",
                            min = 5, max = 20, value = 10, step = 1),
                div(
                  class = "p-2 mt-2",
                  style = paste0("background:", colores$fondo,
                                 "; border-radius:6px; font-size:12px;"),
                  bs_icon("info-circle", class = "me-1",
                          style = paste0("color:", colores$primario)),
                  "LDA requiere ≥ 2 documentos. Para texto continuo, se divide",
                  " automáticamente en segmentos de 50 tokens."
                ),
                tags$hr(),
                actionButton(
                  ns("ajustar_lda"),
                  label = tagList(bs_icon("play-fill", class = "me-1"),
                                  "Ajustar LDA"),
                  class = "btn btn-primary w-100"
                ),
                div(class = "mt-2", uiOutput(ns("estado_lda_ui")))
              )
            ),
            div(
              card(
                class = "mb-3",
                card_header(
                  bs_icon("bar-chart-fill", class = "me-1"),
                  "Términos por tópico (β)"
                ),
                card_body(
                  plotOutput(ns("plot_lda_beta"), height = "420px")
                )
              )
            )
          ),
          div(class = "mt-3",
            card(
              card_header(bs_icon("table", class = "me-1"),
                          "Distribución de tópicos por documento (γ)"),
              card_body(
                DTOutput(ns("tabla_lda_gamma"))
              )
            )
          )
        )
      ), # /PESTAÑA 5

      # ══════════════════════════════════════════════════
      # PESTAÑA 6: Código R
      # ══════════════════════════════════════════════════
      nav_panel(
        title = tagList(bs_icon("code-slash", class = "me-1"), "Código R"),
        card_body(
          p(class = "small text-muted",
            "Código R reproducible con quanteda y topicmodels."),
          div(class = "d-flex gap-2 mb-3",
            downloadButton(ns("descarga_codigo"),
                           "Descargar .R",
                           class = "btn-sm btn-outline-primary")
          ),
          verbatimTextOutput(ns("codigo_r")) |>
            tagAppendAttributes(class = "codigo-bloque")
        )
      ) # /PESTAÑA 6

    ) # /navset_card_tab
  ) # /tagList
} # /mod_texto_libre_ui


# ── Server ────────────────────────────────────────────────
mod_texto_libre_server <- function(id) {
  moduleServer(id, function(input, output, session) {
    ns <- session$ns

    # ── Texto fuente ─────────────────────────────────────
    texto_raw <- reactive({
      fuente <- input$fuente_texto
      if (fuente == "pegar") {
        req(nchar(trimws(input$texto_input)) > 0)
        input$texto_input
      } else if (fuente == "archivo") {
        req(input$archivo_texto)
        ext <- tolower(tools::file_ext(input$archivo_texto$name))
        if (ext == "csv") {
          df <- readr::read_csv(input$archivo_texto$datapath,
                                show_col_types = FALSE)
          req(input$col_texto %in% names(df))
          paste(df[[input$col_texto]], collapse = " ")
        } else if (ext == "docx") {
          doc <- officer::read_docx(input$archivo_texto$datapath)
          txt <- officer::docx_summary(doc)
          paste(txt$text[txt$content_type == "paragraph"], collapse = " ")
        } else if (ext == "odt") {
          # readtext maneja odt directamente
          rt <- readtext::readtext(input$archivo_texto$datapath)
          rt$text[1]
        } else if (ext == "pdf") {
          rt <- readtext::readtext(input$archivo_texto$datapath)
          rt$text[1]
        } else {
          readr::read_file(input$archivo_texto$datapath)
        }
      } else {
        EJEMPLOS_TEXTO[[fuente]]$texto
      }
    })

    # Actualizar selector de columnas para CSV
    output$archivo_es_csv <- reactive({
      req(input$archivo_texto)
      tools::file_ext(input$archivo_texto$name) == "csv"
    })
    outputOptions(output, "archivo_es_csv", suspendWhenHidden = FALSE)

    observeEvent(input$archivo_texto, {
      ext <- tools::file_ext(input$archivo_texto$name)
      if (ext == "csv") {
        df   <- readr::read_csv(input$archivo_texto$datapath,
                                show_col_types = FALSE, n_max = 1)
        cols <- names(df)
        updateSelectInput(session, "col_texto", choices = cols,
                          selected = cols[1])
      }
    })

    # ── Corpus procesado (reactiveVal para mantener) ──────
    corpus_proc <- reactiveVal(NULL)
    dfm_proc    <- reactiveVal(NULL)
    idioma_det  <- reactiveVal("es")

    observeEvent(input$analizar, {
      req(texto_raw())
      withProgress(message = "Procesando corpus…", value = 0.3, {
        tryCatch({
          txt    <- texto_raw()
          idioma <- detectar_idioma(txt)
          idioma_det(idioma)

          toks <- preprocesar_corpus(
            txt,
            idioma              = idioma,
            remover_numeros     = input$remover_numeros,
            remover_puntuacion  = input$remover_punt,
            min_nchar           = input$min_nchar
          )
          incProgress(0.4)

          dfm <- construir_dfm(toks, stem = input$aplicar_stemming)
          incProgress(0.3)

          corpus_proc(toks)
          dfm_proc(dfm)
        }, error = function(e) {
          showNotification(
            paste("Error al procesar:", conditionMessage(e)),
            type = "error", duration = 6
          )
        })
      })
    })

    # ── Estado del corpus ─────────────────────────────────
    output$estado_corpus_ui <- renderUI({
      if (is.null(dfm_proc())) {
        return(div(
          class = "alert alert-info small py-2 px-3",
          bs_icon("info-circle", class = "me-1"),
          "Ingresá texto y hacé clic en ", strong("Analizar corpus"), "."
        ))
      }
      dfm <- dfm_proc()
      div(
        class = "alert alert-success small py-2 px-3",
        bs_icon("check-circle-fill", class = "me-1"),
        strong("Corpus procesado."),
        " Idioma detectado: ",
        strong(if (idioma_det() == "es") "Español" else "Inglés"),
        " — ", strong(ndoc(dfm)), " documento(s) · ",
        strong(nfeat(dfm)), " términos únicos."
      )
    })

    output$metricas_corpus_ui <- renderUI({
      req(dfm_proc())
      dfm    <- dfm_proc()
      n_tok  <- sum(quanteda::ntoken(dfm))
      n_feat <- nfeat(dfm)
      ttr    <- round(n_feat / max(n_tok, 1), 3)

      tagList(
        layout_columns(
          col_widths = c(4, 4, 4),
          vbox_card("file-text", "Tokens",       format(n_tok,  big.mark = ","),
                    colores$primario),
          vbox_card("fonts",     "Tipos únicos", format(n_feat, big.mark = ","),
                    colores$acento),
          vbox_card("percent",   "TTR",          ttr,
                    colores$secundario)
        ),
        div(
          class = "mt-2 p-2 small",
          style = paste0("background:", colores$fondo,
                         "; border-radius:6px; line-height:2;"),
          tags$b("Tokens:"), " palabras del texto tras eliminar stopwords y puntuación.",
          tags$br(),
          tags$b("Tipos únicos:"), " formas distintas del vocabulario (sin repetición).",
          tags$br(),
          tags$b("TTR"), " (Type-Token Ratio):", " proporción tipos/tokens.",
          " Valores cercanos a 1 indican mayor diversidad léxica;",
          " valores bajos señalan vocabulario repetitivo."
        )
      )
    })

    # ── Frecuencias ───────────────────────────────────────
    freq_df <- reactive({
      req(dfm_proc())
      top_features_df(dfm_proc(), n = input$n_top_terms)
    })

    output$plot_freq <- renderPlot({
      req(freq_df())
      df <- freq_df()
      df <- switch(input$freq_orden,
        "desc" = dplyr::arrange(df, dplyr::desc(frequency)),
        "asc"  = dplyr::arrange(df, frequency),
        "alfa" = dplyr::arrange(df, feature)
      )
      df$feature <- factor(df$feature, levels = df$feature)

      ggplot2::ggplot(df, ggplot2::aes(x = frequency, y = feature)) +
        ggplot2::geom_col(fill = colores$primario, alpha = 0.85, width = 0.7) +
        ggplot2::geom_text(
          ggplot2::aes(label = frequency),
          hjust = -0.2, size = 3, color = colores$texto
        ) +
        ggplot2::xlim(0, max(df$frequency) * 1.15) +
        ggplot2::labs(x = "Frecuencia", y = NULL) +
        ggplot2::theme_minimal(base_size = 13) +
        ggplot2::theme(
          panel.grid.minor  = ggplot2::element_blank(),
          panel.grid.major.y = ggplot2::element_blank()
        )
    })

    output$tabla_freq <- renderDT({
      req(freq_df())
      datatable(
        freq_df(),
        options  = list(pageLength = 15, dom = "tp", scrollX = TRUE),
        rownames = FALSE,
        class    = "table-sm table-striped",
        colnames = c("Término", "Frecuencia", "Rango")
      )
    })

    output$descarga_freq <- downloadHandler(
      filename = function() paste0("frecuencias_", Sys.Date(), ".csv"),
      content  = function(file) {
        readr::write_csv(freq_df(), file)
      }
    )

    # ── Nube de palabras ──────────────────────────────────
    nube_seed <- reactiveVal(42)

    observeEvent(input$regen_nube, {
      nube_seed(sample.int(10000, 1))
    })

    output$plot_nube <- renderPlot({
      req(dfm_proc())
      set.seed(nube_seed())
      dfm <- dfm_proc()

      paleta <- switch(input$nube_color,
        "blue"   = colorRampPalette(c(colores$secundario, colores$primario))(8),
        "orange" = colorRampPalette(c("#F1CE63", colores$acento, colores$peligro))(8),
        "multi"  = colores$tableau
      )

      suppressWarnings(
        quanteda.textplots::textplot_wordcloud(
          dfm,
          min_count    = input$nube_min_freq,
          max_words    = input$nube_max_words,
          color        = paleta,
          random_order = FALSE
        )
      )
    })

    # ── Coocurrencias ─────────────────────────────────────
    cooc_df <- reactiveVal(NULL)

    observeEvent(input$calcular_cooc, {
      req(corpus_proc())
      withProgress(message = "Calculando coocurrencias…", value = 0.5, {
        tryCatch({
          fcm_mat <- quanteda::fcm(
            corpus_proc(),
            context  = "window",
            window   = input$cooc_ventana,
            ordered  = FALSE,
            tri      = TRUE
          )
          # Convertir a data.frame long
          fcm_mat2 <- quanteda::fcm_select(
            fcm_mat,
            pattern = quanteda::topfeatures(
              quanteda::dfm(corpus_proc()), 100
            ) |> names()
          )
          mat   <- as.matrix(fcm_mat2)
          nms   <- rownames(mat)
          pairs <- which(upper.tri(mat) & mat >= input$cooc_min_count,
                         arr.ind = TRUE)
          if (nrow(pairs) == 0) {
            showNotification(
              "No hay pares con esa frecuencia mínima. Reducí el umbral.",
              type = "warning", duration = 4
            )
            return()
          }
          df <- data.frame(
            term1 = nms[pairs[, 1]],
            term2 = nms[pairs[, 2]],
            count = mat[pairs]
          ) |>
            dplyr::arrange(dplyr::desc(count)) |>
            dplyr::slice_head(n = input$cooc_top_n)
          cooc_df(df)
          incProgress(0.5)
        }, error = function(e) {
          showNotification(paste("Error:", conditionMessage(e)),
                           type = "error", duration = 5)
        })
      })
    })

    output$plot_cooc <- renderPlot({
      req(cooc_df())
      df <- cooc_df()
      df$par <- paste0(df$term1, " — ", df$term2)
      df$par <- factor(df$par, levels = rev(df$par))

      ggplot2::ggplot(df, ggplot2::aes(x = count, y = par)) +
        ggplot2::geom_col(fill = colores$secundario, alpha = 0.85, width = 0.7) +
        ggplot2::geom_text(
          ggplot2::aes(label = count),
          hjust = -0.2, size = 3, color = colores$texto
        ) +
        ggplot2::xlim(0, max(df$count) * 1.15) +
        ggplot2::labs(x = "Frecuencia de coocurrencia", y = NULL) +
        ggplot2::theme_minimal(base_size = 12) +
        ggplot2::theme(
          panel.grid.minor   = ggplot2::element_blank(),
          panel.grid.major.y = ggplot2::element_blank()
        )
    })

    output$tabla_cooc <- renderDT({
      req(cooc_df())
      datatable(
        cooc_df(),
        options  = list(pageLength = 10, dom = "tp"),
        rownames = FALSE,
        class    = "table-sm table-striped",
        colnames = c("Término 1", "Término 2", "Frecuencia de coocurrencia")
      )
    })

    # ── LDA ───────────────────────────────────────────────
    modelo_lda  <- reactiveVal(NULL)
    estado_lda  <- reactiveVal(NULL)

    observeEvent(input$ajustar_lda, {
      req(dfm_proc())
      withProgress(message = "Ajustando LDA…", value = 0.2, {
        tryCatch({
          dfm <- dfm_proc()

          # Si hay un solo documento, dividir en segmentos
          if (ndoc(dfm) < 2) {
            toks_seg <- quanteda::tokens_chunk(corpus_proc(), size = 50)
            dfm      <- construir_dfm(toks_seg,
                                      stem = input$aplicar_stemming)
          }

          # Eliminar features con frecuencia 0 y documentos vacíos
          dfm <- quanteda::dfm_trim(dfm, min_termfreq = 1)
          keep <- which(rowSums(dfm) > 0)
          dfm  <- dfm[keep, ]

          validate(
            need(ndoc(dfm) >= input$lda_k,
                 paste0(
                   "Se necesitan al menos K = ", input$lda_k,
                   " documentos/segmentos. Reducí K o añadí más texto."
                 ))
          )

          # Convertir a formato topicmodels
          dtm <- quanteda::convert(dfm, to = "topicmodels")
          incProgress(0.3)

          set.seed(42)
          lda <- topicmodels::LDA(
            dtm,
            k       = input$lda_k,
            method  = "Gibbs",
            control = list(iter = input$lda_iter, seed = 42, verbose = 0)
          )
          incProgress(0.5)
          modelo_lda(lda)
          estado_lda("ok")
        }, error = function(e) {
          estado_lda("error")
          showNotification(
            paste("Error en LDA:", conditionMessage(e)),
            type = "error", duration = 6
          )
        })
      })
    })

    output$estado_lda_ui <- renderUI({
      est <- estado_lda()
      if (is.null(est)) return(NULL)
      if (est == "ok") {
        div(class = "alert alert-success small py-1 px-2 mt-1",
            bs_icon("check-circle-fill", class = "me-1"),
            "Modelo ajustado — K = ", strong(input$lda_k), " tópicos.")
      } else {
        div(class = "alert alert-danger small py-1 px-2 mt-1",
            bs_icon("exclamation-triangle", class = "me-1"),
            "Error. Reducí K o añadí más texto.")
      }
    })

    output$plot_lda_beta <- renderPlot({
      req(modelo_lda())
      lda <- modelo_lda()
      n   <- input$lda_top_terms

      beta_df <- tidytext::tidy(lda, matrix = "beta") |>
        dplyr::group_by(topic) |>
        dplyr::slice_max(beta, n = n, with_ties = FALSE) |>
        dplyr::ungroup() |>
        dplyr::mutate(
          topic = paste0("Tópico ", topic),
          term  = tidytext::reorder_within(term, beta, topic)
        )

      ggplot2::ggplot(beta_df,
        ggplot2::aes(x = beta, y = term, fill = topic)) +
        ggplot2::geom_col(show.legend = FALSE, alpha = 0.85) +
        tidytext::scale_y_reordered() +
        ggplot2::facet_wrap(~topic, scales = "free_y",
                            ncol = min(input$lda_k, 3)) +
        scale_fill_tableau_cb() +
        ggplot2::labs(x = "Probabilidad del término (β)", y = NULL) +
        ggplot2::theme_minimal(base_size = 11) +
        ggplot2::theme(panel.grid.minor = ggplot2::element_blank())
    })

    output$tabla_lda_gamma <- renderDT({
      req(modelo_lda())
      gamma_df <- tidytext::tidy(modelo_lda(), matrix = "gamma") |>
        dplyr::mutate(
          topic = paste0("T", topic),
          gamma = round(gamma, 3)
        ) |>
        tidyr::pivot_wider(names_from = topic, values_from = gamma,
                           names_prefix = "Tópico_")

      datatable(
        gamma_df,
        options  = list(pageLength = 10, scrollX = TRUE, dom = "tp"),
        rownames = FALSE,
        class    = "table-sm table-condensed",
        colnames = c("Documento", paste0("Tópico ", seq_len(input$lda_k)))
      ) |>
        DT::formatRound(
          columns = paste0("Tópico_T", seq_len(input$lda_k)),
          digits  = 3
        )
    })

    # ── Código R ──────────────────────────────────────────
    codigo_generado <- reactive({
      lam_str <- if (!is.null(input$col_texto)) input$col_texto else "texto"

      encabezado_script("Análisis de texto libre") |>
        paste0(
          "# -- Paquetes ------------------------------------------------\n",
          "library(quanteda)\n",
          "library(quanteda.textstats)\n",
          "library(quanteda.textplots)\n",
          "library(topicmodels)\n",
          "library(tidytext)\n",
          "library(tidyverse)\n\n",
          "# -- Datos ----------------------------------------------------\n",
          "# Reemplazá con tu propio texto o vector de textos\n",
          "textos <- c(\n",
          "  \"Tu primer documento aquí.\",\n",
          "  \"Tu segundo documento aquí.\"\n",
          ")\n\n",
          "# -- Preprocesamiento -----------------------------------------\n",
          "corp <- corpus(textos)\n",
          "toks <- tokens(corp,\n",
          "  remove_numbers   = ", input$remover_numeros, ",\n",
          "  remove_punct     = ", input$remover_punt, ",\n",
          "  remove_symbols   = TRUE\n",
          ") |>\n",
          "  tokens_remove(stopwords('", idioma_det(), "')) |>\n",
          "  tokens_select(min_nchar = ", input$min_nchar, ")\n\n",
          if (input$aplicar_stemming)
            "toks <- tokens_wordstem(toks)\n\n"
          else "",
          "dfm <- dfm(toks)\n",
          "dfm\n\n",
          "# -- Frecuencias ----------------------------------------------\n",
          "freq <- textstat_frequency(dfm, n = ", input$n_top_terms, ")\n",
          "freq\n\n",
          "# -- Nube de palabras -----------------------------------------\n",
          "textplot_wordcloud(dfm,\n",
          "  max_words = ", input$nube_max_words, ",\n",
          "  min_count = ", input$nube_min_freq, ",\n",
          "  color     = c('#5FA2CE', '#1170AA')\n",
          ")\n\n",
          "# -- Coocurrencias --------------------------------------------\n",
          "fcm_mat <- fcm(toks, context = 'window',\n",
          "               window = ", input$cooc_ventana, ", ordered = FALSE)\n",
          "# Visualizar top pares\n",
          "feat <- names(topfeatures(dfm, 30))\n",
          "fcm_sel <- fcm_select(fcm_mat, pattern = feat)\n",
          "textplot_network(fcm_sel, min_freq = ", input$cooc_min_count, ")\n\n",
          "# -- LDA (topicmodels) ----------------------------------------\n",
          "dtm <- convert(dfm, to = 'topicmodels')\n",
          "set.seed(42)\n",
          "lda <- LDA(dtm,\n",
          "  k       = ", input$lda_k, ",\n",
          "  method  = 'Gibbs',\n",
          "  control = list(iter = ", input$lda_iter, ", seed = 42)\n",
          ")\n\n",
          "# Términos por tópico\n",
          "terms(lda, ", input$lda_top_terms, ")\n\n",
          "# Beta (prob. término por tópico) con tidytext\n",
          "beta_df <- tidy(lda, matrix = 'beta') |>\n",
          "  group_by(topic) |>\n",
          "  slice_max(beta, n = ", input$lda_top_terms, ") |>\n",
          "  ungroup()\n\n",
          "ggplot(beta_df, aes(x = beta, y = reorder(term, beta))) +\n",
          "  geom_col(fill = '#1170AA', alpha = 0.85) +\n",
          "  facet_wrap(~topic, scales = 'free_y') +\n",
          "  labs(x = 'Beta', y = NULL) +\n",
          "  theme_minimal()\n"
        )
    })

    output$codigo_r <- renderText({ codigo_generado() })

    output$descarga_codigo <- downloadHandler(
      filename = function() paste0("texto_libre_", format(Sys.Date(), "%Y%m%d"), ".R"),
      content  = function(file) writeLines(codigo_generado(), file)
    )

  }) # /moduleServer
} # /mod_texto_libre_server
