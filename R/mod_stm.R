# mod_stm.R — Structural Topic Model (STM)
# StatText · StatSuite · ICOMVIS · Universidad Nacional · Costa Rica
# Autor: Manuel Spínola · manuel.spinola@una.ac.cr

# ── UI ────────────────────────────────────────────────────
mod_stm_ui <- function(id) {
  ns <- NS(id)

  tagList(
    navset_card_tab(

      # ══════════════════════════════════════════════════
      # PESTAÑA 1: Corpus y metadatos
      # ══════════════════════════════════════════════════
      nav_panel(
        title = tagList(bs_icon("upload", class = "me-1"), "Corpus"),
        card_body(
          div(
            class = "alert mb-3",
            style = paste0("background:", colores$fondo,
                           "; border-left: 4px solid ", colores$primario,
                           "; border-radius: 6px;"),
            tags$b(bs_icon("mortarboard", class = "me-1",
                           style = paste0("color:", colores$primario)),
                   "¿Qué es el STM y para qué sirve?"),
            tags$p(class = "small text-muted mb-1 mt-1",
              tags$strong("Structural Topic Model (STM)"), " es una técnica de",
              " minería de texto que descubre temas latentes en un corpus,",
              " igual que el LDA, pero con una ventaja clave:"
            ),
            tags$p(class = "small text-muted mb-1",
              tags$strong("Incorpora covariables del documento"),
              " — variables como sexo, región, año o grupo — para modelar",
              " cómo la presencia de cada tópico varía según esas características.",
              " Por ejemplo: ¿hablan más las mujeres del Tópico 2?",
              " ¿Los documentos más recientes usan más el Tópico 3?"
            ),
            tags$p(class = "small text-muted mb-0",
              tags$strong("Flujo de trabajo:"),
              tags$ol(
                class = "mb-0 mt-1 ps-3",
                tags$li("Cargá el corpus con metadatos (esta pestaña)"),
                tags$li("Ajustá el modelo en la pestaña ", tags$strong("Modelo")),
                tags$li("Explorá la prevalencia de tópicos en ", tags$strong("Prevalencia")),
                tags$li("Revisá qué documentos pertenecen a cada tópico en ",
                        tags$strong("Documentos"))
              )
            )
          ),
          layout_columns(
            col_widths = c(5, 7),
            div(
              card(
                card_header(bs_icon("database", class = "me-1"), "Fuente de texto"),
                card_body(
                  tags$p(class = "small fw-semibold text-muted mb-1",
                         bs_icon("bookmark", class = "me-1"), "Ejemplos"),
                  radioButtons(
                    ns("stm_fuente"),
                    label = NULL,
                    choices = c(
                      "Discursos parlamentarios (simulado)" = "parlamentarios",
                      "Noticias de prensa (simulado)"       = "noticias"
                    ),
                    selected = "parlamentarios"
                  ),
                  tags$hr(class = "my-2"),
                  tags$p(class = "small fw-semibold text-muted mb-1",
                         bs_icon("upload", class = "me-1"), "Subir archivo"),
                  radioButtons(
                    ns("stm_fuente_archivo"),
                    label = NULL,
                    choices = c("Subir archivo (.csv, .xlsx)" = "archivo"),
                    selected = character(0)
                  ),
                  conditionalPanel(
                    condition = paste0("output['", ns("stm_es_archivo"), "']"),
                    div(
                      class = "p-2 mb-2",
                      style = paste0("background:", colores$fondo,
                                     "; border-radius:6px; font-size:12px;"),
                      bs_icon("info-circle", class = "me-1",
                              style = paste0("color:", colores$primario)),
                      tags$strong("Formato esperado:"),
                      tags$ul(
                        class = "mb-1 mt-1 ps-3",
                        tags$li("Una fila por documento"),
                        tags$li("Una columna con el texto"),
                        tags$li("Columnas adicionales como metadatos/covariables",
                                " (sexo, región, fecha, grupo, etc.)"),
                        tags$li("Primera fila: nombres de columnas")
                      ),
                      tags$em("Ejemplo: texto, sexo, region, anio")
                    ),
                    fileInput(
                      ns("stm_archivo"),
                      label       = NULL,
                      accept      = c(".csv", ".xlsx"),
                      buttonLabel = tagList(
                        bs_icon("folder2-open", class = "me-1"), "Examinar…"),
                      placeholder = "Sin archivo seleccionado"
                    )
                  )
                )
              ),
              card(
                class = "mt-3",
                card_header(bs_icon("sliders", class = "me-1"), "Configuración"),
                card_body(
                  uiOutput(ns("stm_sel_col_texto_ui")),
                  uiOutput(ns("stm_sel_covariables_ui")),
                  tags$hr(),
                  actionButton(
                    ns("stm_cargar"),
                    tagList(bs_icon("play-fill", class = "me-1"), "Cargar corpus"),
                    class = "btn btn-primary w-100"
                  )
                )
              )
            ),
            div(
              uiOutput(ns("stm_estado_corpus_ui")),
              br(),
              DTOutput(ns("stm_preview"))
            )
          )
        )
      ), # /PESTAÑA 1

      # ══════════════════════════════════════════════════
      # PESTAÑA 2: Ajustar modelo
      # ══════════════════════════════════════════════════
      nav_panel(
        title = tagList(bs_icon("gear", class = "me-1"), "Modelo"),
        card_body(
          layout_columns(
            col_widths = c(3, 9),
            div(
              card(
                card_header(bs_icon("sliders", class = "me-1"), "Controles"),
                card_body(
                  numericInput(ns("stm_k"), "Número de tópicos (K):",
                               value = 5, min = 2, max = 20, step = 1),
                  uiOutput(ns("stm_sel_prevalencia_ui")),
                  div(
                    class = "p-2 mb-2",
                    style = paste0("background:", colores$fondo,
                                   "; border-radius:6px; font-size:12px;"),
                    bs_icon("info-circle", class = "me-1",
                            style = paste0("color:", colores$primario)),
                    tags$strong("Covariable de prevalencia:"),
                    " variable que puede influir en la proporción de tópicos",
                    " por documento (ej. sexo, región, año)."
                  ),
                  numericInput(ns("stm_max_em"), "Iteraciones máximas (EM):",
                               value = 75, min = 20, max = 200, step = 25),
                  tags$hr(),
                  actionButton(
                    ns("stm_ajustar"),
                    tagList(bs_icon("play-fill", class = "me-1"), "Ajustar STM"),
                    class = "btn btn-primary w-100"
                  ),
                  div(class = "mt-2", uiOutput(ns("stm_estado_modelo_ui")))
                )
              ),
              card(
                class = "mt-3",
                card_header(bs_icon("lightbulb", class = "me-1"), "Interpretación"),
                card_body(
                  style = "white-space: normal; word-wrap: break-word;",
                  uiOutput(ns("exp_stm_modelo_ui"))
                )
              )
            ),
            div(
              card(
                card_header(bs_icon("bar-chart-fill", class = "me-1"),
                            "Palabras más probables por tópico (β)"),
                card_body(uiOutput(ns("stm_plot_beta_ui")))
              )
            )
          )
        )
      ), # /PESTAÑA 2

      # ══════════════════════════════════════════════════
      # PESTAÑA 3: Prevalencia de tópicos
      # ══════════════════════════════════════════════════
      nav_panel(
        title = tagList(bs_icon("graph-up", class = "me-1"), "Prevalencia"),
        card_body(
          div(
            class = "alert mb-3",
            style = paste0("background:", colores$fondo,
                           "; border-left: 4px solid ", colores$secundario, ";"),
            tags$b(bs_icon("info-circle", class = "me-1",
                           style = paste0("color:", colores$secundario)),
                   "Prevalencia de tópicos"),
            tags$p(class = "small text-muted mb-0 mt-1",
              "Muestra la ", tags$strong("proporción esperada"),
              " de cada tópico en el corpus (θ̄).",
              " Si se especificó una covariable de prevalencia, el gráfico de",
              " efecto muestra cómo varía la probabilidad de cada tópico",
              " según esa variable."
            )
          ),
          layout_columns(
            col_widths = c(3, 9),
            div(
              card(
                card_header(bs_icon("sliders", class = "me-1"), "Controles"),
                card_body(
                  uiOutput(ns("stm_nombres_ui")),
                  tags$hr(),
                  uiOutput(ns("stm_sel_efecto_ui"))
                )
              ),
              card(
                class = "mt-3",
                card_header(bs_icon("lightbulb", class = "me-1"), "Interpretación"),
                card_body(
                  style = "white-space: normal; word-wrap: break-word;",
                  uiOutput(ns("exp_stm_prevalencia_ui"))
                )
              )
            ),
            div(
              card(
                class = "mb-3",
                card_header(bs_icon("bar-chart-fill", class = "me-1"),
                            "Proporción media de tópicos (θ̄)"),
                card_body(plotOutput(ns("stm_plot_theta"), height = "350px"))
              ),
              card(
                card_header(bs_icon("graph-up-arrow", class = "me-1"),
                            "Efecto de covariable sobre prevalencia"),
                card_body(uiOutput(ns("stm_plot_efecto_ui")))
              )
            )
          )
        )
      ), # /PESTAÑA 3

      # ══════════════════════════════════════════════════
      # PESTAÑA 4: Documentos
      # ══════════════════════════════════════════════════
      nav_panel(
        title = tagList(bs_icon("file-text", class = "me-1"), "Documentos"),
        card_body(
          layout_columns(
            col_widths = c(3, 9),
            div(
              card(
                card_header(bs_icon("sliders", class = "me-1"), "Controles"),
                card_body(
                  style = "overflow: visible; min-height: 200px;",
                  sliderInput(ns("stm_doc_top_n"), "Documentos a mostrar:",
                              min = 5, max = 50, value = 20, step = 5),
                  selectInput(
                    ns("stm_doc_orden"),
                    "Ordenar por:",
                    choices = c(
                      "Tópico dominante" = "topico",
                      "γ máximo"         = "gamma"
                    )
                  )
                )
              ),
              card(
                class = "mt-3",
                card_header(bs_icon("lightbulb", class = "me-1"), "Interpretación"),
                card_body(
                  style = "white-space: normal; word-wrap: break-word;",
                  uiOutput(ns("exp_stm_docs_ui"))
                )
              )
            ),
            card(
              card_header(bs_icon("table", class = "me-1"),
                          "Tópico dominante por documento (γ)"),
              card_body(DTOutput(ns("stm_tabla_docs")))
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
          encabezado_script("STM — Structural Topic Model"),
          div(class = "d-flex gap-2 mb-3",
            downloadButton(ns("descarga_codigo_stm"),
                           "Descargar .R",
                           class = "btn-sm btn-outline-primary")),
          verbatimTextOutput(ns("codigo_stm_r")) |>
            tagAppendAttributes(class = "codigo-bloque")
        )
      ) # /PESTAÑA 5

    ) # /navset_card_tab
  ) # /tagList
} # /mod_stm_ui


# ── Server ────────────────────────────────────────────────
mod_stm_server <- function(id) {
  moduleServer(id, function(input, output, session) {
    ns <- session$ns

    # ── Datos de ejemplo ──────────────────────────────────
    EJEMPLO_PARLAMENTARIOS <- data.frame(
      texto = c(
        "La propuesta de ley busca mejorar el acceso a la educación pública en zonas rurales",
        "Es necesario aumentar el presupuesto para salud y atención primaria en comunidades",
        "El desarrollo económico requiere inversión en infraestructura y transporte regional",
        "Debemos garantizar los derechos de las mujeres y la igualdad de género en el trabajo",
        "La seguridad ciudadana es prioridad para nuestra comunidad y requiere más recursos",
        "La educación es el pilar fundamental del desarrollo social y debe ser gratuita",
        "Los servicios de salud deben llegar a todas las comunidades sin distinción",
        "La construcción de carreteras mejora la economía de las regiones más alejadas",
        "Las políticas de equidad de género son fundamentales para una sociedad justa",
        "El combate a la delincuencia necesita coordinación entre instituciones del Estado",
        "Necesitamos más escuelas y maestros para las zonas de difícil acceso",
        "La cobertura universal de salud es un derecho que debemos garantizar",
        "El presupuesto nacional debe priorizar obras de infraestructura productiva",
        "La violencia contra la mujer es un problema que requiere atención urgente",
        "La prevención del delito es más efectiva que la represión policial",
        "Invertir en educación temprana tiene los mejores retornos sociales",
        "Los hospitales regionales necesitan equipamiento moderno y personal suficiente",
        "Las redes viales conectan mercados y reducen la pobreza en zonas rurales",
        "La paridad de género en política fortalece la democracia representativa",
        "La reforma policial debe enfocarse en la confianza comunitaria"
      ),
      sexo   = rep(c("Mujer", "Hombre"), 10),
      region = rep(c("Central", "Pacífico", "Caribe", "Norte"), 5),
      anio   = rep(c(2021, 2022, 2023), length.out = 20),
      stringsAsFactors = FALSE
    )

    EJEMPLO_NOTICIAS <- data.frame(
      texto = c(
        "El gobierno anunció nuevas medidas para controlar la inflación y el costo de vida",
        "Las exportaciones del sector agrícola crecieron un diez por ciento este trimestre",
        "La cumbre climática acordó reducir emisiones de carbono en los próximos años",
        "El banco central ajustó las tasas de interés para frenar la inflación",
        "Los agricultores exigen mejores precios para sus productos en el mercado internacional",
        "Los países firmaron acuerdos para reducir el uso de combustibles fósiles",
        "La crisis económica afecta a los sectores más vulnerables de la población",
        "La producción de café y banano registró cifras récord en el último año",
        "Las energías renovables avanzan como alternativa a los combustibles contaminantes",
        "Las empresas reportan pérdidas por el aumento en los costos de producción",
        "El turismo rural se presenta como alternativa para pequeños productores",
        "El acuerdo de París exige compromisos más ambiciosos de los países industrializados",
        "La desaceleración económica mundial impacta las exportaciones nacionales",
        "La biodiversidad del país es un activo para el ecoturismo y la investigación",
        "Los mercados financieros reaccionan ante la incertidumbre económica global",
        "Las cooperativas agropecuarias fortalecen la economía de las zonas rurales",
        "El cambio climático amenaza la seguridad alimentaria en países en desarrollo",
        "Las políticas fiscales buscan equilibrar el crecimiento y el bienestar social",
        "La reforestación de cuencas hidrográficas mejora la disponibilidad de agua",
        "Los indicadores económicos muestran una recuperación moderada tras la pandemia"
      ),
      medio    = rep(c("Nacional", "Regional", "Digital"), length.out = 20),
      seccion  = rep(c("Economía", "Ambiente", "Política"), length.out = 20),
      anio     = rep(c(2022, 2023, 2024), length.out = 20),
      stringsAsFactors = FALSE
    )

    # ── Reactivos de fuente ───────────────────────────────
    output$stm_es_archivo <- reactive({
      !is.null(input$stm_fuente_archivo) &&
        length(input$stm_fuente_archivo) > 0 &&
        input$stm_fuente_archivo == "archivo"
    })
    outputOptions(output, "stm_es_archivo", suspendWhenHidden = FALSE)

    observeEvent(input$stm_fuente, {
      req(input$stm_fuente)
      updateRadioButtons(session, "stm_fuente_archivo", selected = character(0))
    })
    observeEvent(input$stm_fuente_archivo, {
      req(input$stm_fuente_archivo)
      updateRadioButtons(session, "stm_fuente", selected = character(0))
    })

    fuente_stm_activa <- reactive({
      if (!is.null(input$stm_fuente_archivo) &&
          length(input$stm_fuente_archivo) > 0 &&
          input$stm_fuente_archivo == "archivo") "archivo"
      else input$stm_fuente %||% "parlamentarios"
    })

    # ── Datos crudos ──────────────────────────────────────
    datos_stm <- reactive({
      switch(fuente_stm_activa(),
        "parlamentarios" = EJEMPLO_PARLAMENTARIOS,
        "noticias"       = EJEMPLO_NOTICIAS,
        "archivo" = {
          req(input$stm_archivo)
          ext <- tolower(tools::file_ext(input$stm_archivo$name))
          if (ext == "xlsx")
            as.data.frame(readxl::read_excel(input$stm_archivo$datapath))
          else
            as.data.frame(readr::read_csv(input$stm_archivo$datapath,
                                          show_col_types = FALSE))
        }
      )
    })

    # ── Selectores de columnas ────────────────────────────
    output$stm_sel_col_texto_ui <- renderUI({
      req(datos_stm())
      nms <- names(datos_stm())
      selectInput(ns("stm_col_texto"), "Columna de texto:",
                  choices = nms, selected = nms[1])
    })

    output$stm_sel_covariables_ui <- renderUI({
      req(datos_stm())
      nms <- names(datos_stm())
      meta_cols <- setdiff(nms, input$stm_col_texto %||% nms[1])
      if (length(meta_cols) == 0) return(NULL)
      checkboxGroupInput(
        ns("stm_covariables"),
        "Covariables / metadatos a incluir:",
        choices  = meta_cols,
        selected = meta_cols
      )
    })

    # ── Estado corpus ─────────────────────────────────────
    corpus_stm <- reactiveVal(NULL)
    meta_stm   <- reactiveVal(NULL)

    observeEvent(input$stm_cargar, {
      req(datos_stm(), input$stm_col_texto)
      tryCatch({
        df      <- datos_stm()
        col_txt <- input$stm_col_texto
        textos  <- as.character(df[[col_txt]])
        textos  <- ifelse(is.na(textos) | trimws(textos) == "", " ", textos)

        corp <- quanteda::corpus(textos)
        corpus_stm(corp)

        # Metadatos
        cov_cols <- input$stm_covariables
        if (!is.null(cov_cols) && length(cov_cols) > 0) {
          meta <- df[, cov_cols, drop = FALSE]
          meta_stm(meta)
          quanteda::docvars(corp) <- meta
          corpus_stm(corp)
        } else {
          meta_stm(NULL)
        }

        output$stm_estado_corpus_ui <- renderUI({
          n <- quanteda::ndoc(corpus_stm())
          div(class = "alert alert-success small py-2 px-3",
              bs_icon("check-circle-fill", class = "me-1"),
              strong(n), " documentos cargados.",
              if (!is.null(meta_stm()))
                paste0(" Covariables: ",
                       paste(names(meta_stm()), collapse = ", "), ".")
          )
        })
      }, error = function(e) {
        showNotification(paste("Error:", conditionMessage(e)),
                         type = "error", duration = 5)
      })
    })

    output$stm_preview <- renderDT({
      req(datos_stm())
      datatable(
        head(datos_stm(), 8),
        options  = list(dom = "t", scrollX = TRUE, pageLength = 8),
        rownames = FALSE,
        class    = "table-sm table-striped"
      )
    })

    # ── Selector covariable prevalencia ───────────────────
    output$stm_sel_prevalencia_ui <- renderUI({
      req(meta_stm())
      nms <- names(meta_stm())
      selectInput(
        ns("stm_cov_prev"),
        "Covariable de prevalencia:",
        choices  = c("(ninguna)" = "", nms),
        selected = ""
      )
    })

    # ── Modelo STM ────────────────────────────────────────
    modelo_stm    <- reactiveVal(NULL)
    dfm_stm_rv    <- reactiveVal(NULL)
    nombres_stm   <- reactiveVal(NULL)

    observeEvent(input$stm_ajustar, {
      req(corpus_stm())
      withProgress(message = "Ajustando STM…", value = 0.2, {
        tryCatch({
          # Preprocesar
          toks <- quanteda::tokens(
            corpus_stm(),
            remove_punct  = TRUE,
            remove_numbers = TRUE,
            remove_symbols = TRUE
          ) |>
            quanteda::tokens_tolower() |>
            quanteda::tokens_remove(
              pattern = quanteda::stopwords(
                detectar_idioma(
                  paste(as.character(corpus_stm()), collapse = " ")
                )
              )
            ) |>
            quanteda::tokens_wordstem()

          dfm <- quanteda::dfm(toks) |>
            quanteda::dfm_trim(min_termfreq = 1)

          sumas <- Matrix::rowSums(quanteda::as.dfm(dfm))
          dfm   <- dfm[sumas > 0, ]

          validate(
            need(ndoc(dfm) >= max(input$stm_k, 2),
                 paste0("Se necesitan al menos ", input$stm_k,
                        " documentos. Reducí K."))
          )

          incProgress(0.3)

          # Fórmula de prevalencia y metadatos
          prev_formula <- NULL
          meta_sub     <- NULL

          tiene_cov <- isTRUE(
            !is.null(input$stm_cov_prev) &&
            length(input$stm_cov_prev) == 1 &&
            nchar(trimws(input$stm_cov_prev)) > 0 &&
            !is.null(meta_stm()) &&
            input$stm_cov_prev %in% names(meta_stm())
          )
          if (tiene_cov) {
            # Alinear metadatos con documentos no vacíos
            doc_idx  <- which(Matrix::rowSums(quanteda::as.dfm(dfm)) > 0)
            meta_sub <- meta_stm()[doc_idx, , drop = FALSE]
            prev_formula <- as.formula(paste0("~", input$stm_cov_prev))
          }

          # Convertir dfm a formato stm
          out <- quanteda::convert(dfm, to = "stm",
                                   docvars = meta_sub)
          # Alinear meta si hay covariable
          meta_alin <- if (!is.null(meta_sub)) out$meta else NULL

          dfm_stm_rv(list(documents = out$documents,
                          vocab     = out$vocab,
                          meta      = meta_alin))

          incProgress(0.3)

          # Ajustar modelo
          stm_fit <- stm::stm(
            documents  = out$documents,
            vocab      = out$vocab,
            K          = input$stm_k,
            prevalence = prev_formula,
            data       = meta_alin,
            max.em.its = input$stm_max_em,
            init.type  = "Spectral",
            verbose    = FALSE
          )

          modelo_stm(stm_fit)

          # Inicializar nombres
          nombres_stm(paste0("Tópico ", seq_len(input$stm_k)))

          incProgress(0.2)

          output$stm_estado_modelo_ui <- renderUI({
            div(class = "alert alert-success small py-2 px-3",
                bs_icon("check-circle-fill", class = "me-1"),
                strong("Modelo ajustado. "),
                input$stm_k, " tópicos identificados.")
          })
        }, error = function(e) {
          showNotification(paste("Error STM:", conditionMessage(e)),
                           type = "error", duration = 6)
        })
      })
    })

    # ── Gráfico beta ──────────────────────────────────────
    output$stm_plot_beta_ui <- renderUI({
      req(modelo_stm())
      k      <- input$stm_k
      n_rows <- ceiling(k / 3)
      altura <- max(400L, n_rows * 300L)
      plotOutput(ns("stm_plot_beta"), height = paste0(altura, "px"))
    })

    output$stm_plot_beta <- renderPlot({
      req(modelo_stm())
      nms <- nombres_reactivos()
      n   <- 10L

      beta_mat <- exp(modelo_stm()$beta$logbeta[[1]])
      vocab    <- dfm_stm_rv()$vocab

      df_list <- lapply(seq_len(input$stm_k), function(k) {
        ord <- order(beta_mat[k, ], decreasing = TRUE)[seq_len(n)]
        data.frame(
          topic = nms[k],
          term  = vocab[ord],
          beta  = beta_mat[k, ord],
          stringsAsFactors = FALSE
        )
      })
      beta_df <- do.call(rbind, df_list)
      beta_df$term  <- tidytext::reorder_within(
        beta_df$term, beta_df$beta, beta_df$topic)
      beta_df$topic <- factor(beta_df$topic, levels = nms)

      ggplot2::ggplot(beta_df,
        ggplot2::aes(x = beta, y = term, fill = topic)) +
        ggplot2::geom_col(show.legend = FALSE, alpha = 0.85, width = 0.7) +
        tidytext::scale_y_reordered() +
        ggplot2::facet_wrap(~topic, scales = "free_y",
                            ncol = min(input$stm_k, 3)) +
        scale_fill_tableau_cb() +
        ggplot2::labs(x = "Probabilidad del término (β)", y = NULL) +
        ggplot2::theme_minimal(base_size = 12) +
        ggplot2::theme(
          panel.grid.minor = ggplot2::element_blank(),
          strip.text       = ggplot2::element_text(face = "bold", size = 11),
          axis.text.y      = ggplot2::element_text(size = 10),
          plot.margin      = ggplot2::margin(8, 12, 8, 8)
        )
    }, res = 96)

    output$exp_stm_modelo_ui <- renderUI({
      req(modelo_stm())
      p(paste0(
        "Se ajustó un STM con K = ", input$stm_k, " tópicos sobre ",
        ndoc(corpus_stm()), " documentos. ",
        if (isTRUE(!is.null(input$stm_cov_prev) &&
            length(input$stm_cov_prev) == 1 &&
            nchar(trimws(input$stm_cov_prev)) > 0))
          paste0("La prevalencia se modeló en función de '",
                 input$stm_cov_prev, "'. ")
        else
          "Sin covariable de prevalencia. ",
        "Nombrá los tópicos en la pestaña Prevalencia mirando las palabras del gráfico."
      ))
    })

    # ── Prevalencia ───────────────────────────────────────
    output$stm_nombres_ui <- renderUI({
      req(modelo_stm())
      k <- input$stm_k
      tagList(
        tags$p(class = "small fw-semibold mb-1",
               bs_icon("tag", class = "me-1"), "Nombrar tópicos (opcional)"),
        lapply(seq_len(k), function(i) {
          textInput(
            ns(paste0("stm_nombre_", i)),
            label     = paste0("Tópico ", i, ":"),
            value     = isolate(input[[paste0("stm_nombre_", i)]]) %||% "",
            placeholder = "ej. Educación y desarrollo"
          )
        })
      )
    })

    nombres_reactivos <- reactive({
      req(modelo_stm())
      k <- modelo_stm()$settings$dim$K  # K real del modelo ajustado
      sapply(seq_len(k), function(i) {
        nm <- input[[paste0("stm_nombre_", i)]]
        if (is.null(nm) || trimws(nm) == "") paste0("Tópico ", i)
        else trimws(nm)
      })
    })

    # Helper: prevalencia real usando tidytext (respeta numeración interna del modelo)
    prevalencia_stm <- reactive({
      req(modelo_stm())
      tidytext::tidy(modelo_stm(), matrix = "gamma") |>
        dplyr::group_by(topic) |>
        dplyr::summarise(prop = mean(gamma), .groups = "drop") |>
        dplyr::arrange(dplyr::desc(prop))
    })

    output$stm_plot_theta <- renderPlot({
      req(modelo_stm())
      nms  <- nombres_reactivos()
      prev <- prevalencia_stm()
      df   <- data.frame(
        topico = factor(nms[prev$topic],
                        levels = nms[prev$topic]),
        prop   = prev$prop
      )

      ggplot2::ggplot(df, ggplot2::aes(x = prop, y = topico, fill = topico)) +
        ggplot2::geom_col(alpha = 0.85, width = 0.6, show.legend = FALSE) +
        ggplot2::geom_text(ggplot2::aes(label = scales::percent(prop, accuracy = 0.1)),
                           hjust = -0.2, size = 3.5, color = colores$texto) +
        ggplot2::expand_limits(x = max(prev$prop) * 1.25) +
        scale_fill_tableau_cb() +
        ggplot2::labs(x = "Proporción media (θ̄)", y = NULL) +
        ggplot2::theme_minimal(base_size = 13) +
        ggplot2::theme(
          panel.grid.minor   = ggplot2::element_blank(),
          panel.grid.major.y = ggplot2::element_blank()
        )
    })

    # ── Efecto de covariable ──────────────────────────────
    output$stm_sel_efecto_ui <- renderUI({
      req(modelo_stm(), dfm_stm_rv())
      if (is.null(dfm_stm_rv()$meta)) {
        return(div(class = "small text-muted",
                   "No hay covariables disponibles.",
                   " Cargá el corpus con metadatos y ajustá el modelo",
                   " con una covariable de prevalencia."))
      }
      nms <- names(dfm_stm_rv()$meta)
      tagList(
        tags$hr(),
        selectInput(ns("stm_efecto_cov"), "Covariable para efecto:",
                    choices = nms, selected = nms[1]),
        actionButton(ns("stm_calcular_efecto"),
                     tagList(bs_icon("graph-up-arrow", class = "me-1"),
                             "Calcular efecto"),
                     class = "btn btn-outline-primary btn-sm w-100 mt-2")
      )
    })

    efecto_stm <- reactiveVal(NULL)

    observeEvent(input$stm_calcular_efecto, {
      req(modelo_stm(), dfm_stm_rv(), input$stm_efecto_cov)
      tryCatch({
        prep <- stm::estimateEffect(
          formula  = as.formula(paste0("1:", input$stm_k,
                                       " ~ ", input$stm_efecto_cov)),
          stmobj   = modelo_stm(),
          metadata = dfm_stm_rv()$meta,
          uncertainty = "Global"
        )
        efecto_stm(prep)
      }, error = function(e) {
        showNotification(paste("Error efecto:", conditionMessage(e)),
                         type = "error", duration = 5)
      })
    })

    output$stm_plot_efecto_ui <- renderUI({
      req(efecto_stm())
      k      <- input$stm_k
      altura <- max(300L, k * 60L)
      plotOutput(ns("stm_plot_efecto"), height = paste0(altura, "px"))
    })

    output$stm_plot_efecto <- renderPlot({
      req(efecto_stm(), modelo_stm())
      nms  <- nombres_reactivos()
      prep <- efecto_stm()
      cov  <- input$stm_efecto_cov
      k    <- input$stm_k

      # Extraer coeficientes por tópico
      df_list <- lapply(seq_len(k), function(i) {
        sm <- summary(prep)$tables[[i]]
        if (is.null(sm)) return(NULL)
        # Buscar fila con la covariable
        idx <- grep(cov, rownames(sm), ignore.case = TRUE)
        if (length(idx) == 0) return(NULL)
        row <- sm[idx[1], ]
        data.frame(
          topico  = nms[i],
          estimado = row["Estimate"],
          se       = row["Std. Error"],
          stringsAsFactors = FALSE
        )
      })
      df <- do.call(rbind, Filter(Negate(is.null), df_list))
      if (is.null(df) || nrow(df) == 0) {
        return(ggplot2::ggplot() +
          ggplot2::annotate("text", x=0.5, y=0.5,
                            label="No hay coeficientes disponibles") +
          ggplot2::theme_void())
      }

      df$topico <- factor(df$topico, levels = df$topico[order(df$estimado)])
      df$ci_lo  <- df$estimado - 1.96 * df$se
      df$ci_hi  <- df$estimado + 1.96 * df$se

      ggplot2::ggplot(df, ggplot2::aes(x = estimado, y = topico)) +
        ggplot2::geom_vline(xintercept = 0, linetype = "dashed",
                            color = colores$gris_medio) +
        ggplot2::geom_errorbarh(
          ggplot2::aes(xmin = ci_lo, xmax = ci_hi),
          height = 0.3, color = colores$primario, alpha = 0.6
        ) +
        ggplot2::geom_point(size = 3, color = colores$acento) +
        ggplot2::labs(
          x = paste0("Efecto de '", cov, "' sobre prevalencia (IC 95%)"),
          y = NULL
        ) +
        ggplot2::theme_minimal(base_size = 13) +
        ggplot2::theme(panel.grid.minor = ggplot2::element_blank())
    })

    output$exp_stm_prevalencia_ui <- renderUI({
      req(modelo_stm())
      nms  <- nombres_reactivos()
      prev <- prevalencia_stm()
      top  <- nms[prev$topic[1]]
      p(paste0(
        "El tópico más prevalente es '", top,
        "' (θ̄ = ", round(prev$prop[1], 3), "). ",
        if (!is.null(efecto_stm()))
          paste0("El gráfico de efectos muestra cómo varía la prevalencia",
                 " de cada tópico según '", input$stm_efecto_cov, "'.")
        else
          "Calculá el efecto de una covariable para ver cómo varía la prevalencia."
      ))
    })

    # ── Documentos ────────────────────────────────────────
    output$stm_tabla_docs <- renderDT({
      req(modelo_stm(), datos_stm(), input$stm_col_texto)
      nms   <- nombres_reactivos()
      theta <- as.matrix(modelo_stm()$theta)
      n_doc <- nrow(theta)
      dom   <- apply(theta, 1, function(x) which.max(as.numeric(x)))
      gmax  <- apply(theta, 1, function(x) max(as.numeric(x)))

      df_orig <- datos_stm()
      textos  <- as.character(df_orig[[input$stm_col_texto]])
      # Alinear textos con documentos no vacíos del modelo
      if (length(textos) > n_doc) textos <- textos[seq_len(n_doc)]
      if (length(textos) < n_doc) textos <- c(textos, rep("", n_doc - length(textos)))
      n_show  <- min(input$stm_doc_top_n, n_doc)

      df_out <- data.frame(
        Documento  = seq_len(n_doc),
        topico_dom = nms[dom],
        gamma_max  = round(gmax, 3),
        Texto      = substr(textos, 1, 80),
        stringsAsFactors = FALSE
      )

      if (input$stm_doc_orden == "gamma") {
        df_out <- df_out[order(df_out$gamma_max, decreasing = TRUE), ]
      } else {
        df_out <- df_out[order(df_out$topico_dom), ]
      }

      datatable(
        head(df_out, n_show),
        options  = list(pageLength = 10, dom = "tp",
                        autoWidth = FALSE, scrollX = FALSE),
        rownames = FALSE,
        class    = "table-sm table-striped",
        colnames = c("Documento", "Tópico dominante", "γ máximo", "Texto")
      ) |> DT::formatRound(3, digits = 3)
    })

    output$exp_stm_docs_ui <- renderUI({
      req(modelo_stm())
      nms  <- nombres_reactivos()
      dom  <- apply(modelo_stm()$theta, 1, which.max)
      freq <- table(dom)
      top  <- nms[as.integer(names(freq)[which.max(freq)])]
      p(paste0(
        "De ", nrow(modelo_stm()$theta), " documentos, el tópico más común fue '",
        top, "' (", max(freq), " documentos)."
      ))
    })

    # ── Código R ──────────────────────────────────────────
    output$codigo_stm_r <- renderText({
      req(modelo_stm())
      cov_prev <- if (isTRUE(!is.null(input$stm_cov_prev) &&
                      length(input$stm_cov_prev) == 1 &&
                      nchar(trimws(input$stm_cov_prev)) > 0))
        paste0("prevalence = ~", input$stm_cov_prev, ",\n    ")
      else ""

      paste0(
        "# ── STM: Structural Topic Model ──────────────────────\n",
        "library(stm)\n",
        "library(quanteda)\n\n",
        "# 1. Preprocesar corpus\n",
        "toks <- tokens(corpus, remove_punct = TRUE,\n",
        "               remove_numbers = TRUE) |>\n",
        "  tokens_tolower() |>\n",
        "  tokens_remove(stopwords('es')) |>\n",
        "  tokens_wordstem()\n",
        "dfm <- dfm(toks) |> dfm_trim(min_termfreq = 1)\n\n",
        "# 2. Convertir a formato stm\n",
        "# stm() acepta el dfm de quanteda directamente\n\n",
        "# 3. Ajustar modelo\n",
        "stm_fit <- stm(\n",
        "  documents  = out$documents,\n",
        "  vocab      = out$vocab,\n",
        "  K          = ", input$stm_k, ",\n",
        "  ", cov_prev,
        "max.em.its = ", input$stm_max_em, ",\n",
        "  init.type  = 'Spectral'\n",
        ")\n\n",
        "# 4. Explorar resultados\n",
        "labelTopics(stm_fit)          # palabras por tópico\n",
        "plot(stm_fit, type = 'summary')  # prevalencia\n"
      )
    })

    output$descarga_codigo_stm <- downloadHandler(
      filename = function() paste0("stm_", format(Sys.Date(), "%Y%m%d"), ".R"),
      content  = function(file) writeLines(output$codigo_stm_r(), file)
    )

  }) # /moduleServer
} # /mod_stm_server
