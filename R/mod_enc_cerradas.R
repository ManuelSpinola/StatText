# ============================================================
# mod_enc_cerradas.R — Preguntas cerradas
# StatText · StatSuite · Manuel Spínola · ICOMVIS · UNA
#
# Análisis: escala Likert (perfil de respuesta, diverging bars),
# análisis de correspondencias (FactoMineR/ca),
# correlaciones entre ítems (psych / corrplot)
# ============================================================

# ── Datos de ejemplo ──────────────────────────────────────
EJEMPLO_CERRADAS <- data.frame(
  `Acceso a salud`        = c(4,2,5,3,4,1,5,3,4,2, 3,4,2,5,3,4,1,5,3,4),
  `Calidad educación`     = c(3,4,5,3,5,2,4,3,4,3, 4,3,5,3,5,2,4,3,4,3),
  `Seguridad ciudadana`   = c(2,1,4,2,3,1,3,2,3,1, 2,1,4,2,3,1,3,2,3,1),
  `Servicios municipales` = c(5,3,4,2,5,1,4,3,4,2, 3,4,2,5,3,4,1,5,3,2),
  `Transporte público`    = c(3,2,4,2,3,1,4,2,3,2, 3,2,4,2,3,1,4,2,3,2),
  `Espacios recreativos`  = c(2,3,5,2,4,1,3,3,3,2, 2,3,5,2,4,1,3,3,3,2),
  grupo = rep(c("Urbano", "Rural"), each = 10),
  check.names = FALSE,
  stringsAsFactors = FALSE
)


# ── UI ────────────────────────────────────────────────────
mod_enc_cerradas_ui <- function(id) {
  ns <- NS(id)

  tagList(
    div(
      class = "py-3 px-2",
      h4(
        bs_icon("bar-chart-steps", class = "me-2"),
        "Preguntas cerradas",
        style = paste0("color:", colores$primario, "; font-weight:700;")
      ),
      p(
        class = "text-muted mb-0",
        "Escalas Likert y múltiple opción. Perfil de respuesta, consistencia interna,",
        " análisis de correspondencias con ",
        tags$strong("FactoMineR"), " y correlaciones entre ítems."
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

              # ── Card: Fuente de datos ──────────────────
              card(
                card_header(
                  bs_icon("database", class = "me-1"), "Fuente de datos"
                ),
                card_body(
                  tags$p(class = "small fw-semibold text-muted mb-1",
                         bs_icon("bookmark", class = "me-1"), "Ejemplo"),
                  radioButtons(
                    ns("fuente_cerradas"),
                    label = NULL,
                    choices = c("Ejemplo: encuesta satisfacción" = "ejemplo"),
                    selected = "ejemplo"
                  ),

                  tags$hr(class = "my-2"),
                  tags$p(class = "small fw-semibold text-muted mb-1",
                         bs_icon("upload", class = "me-1"), "Subir archivo"),
                  radioButtons(
                    ns("fuente_archivo_cerr"),
                    label = NULL,
                    choices = c("Subir archivo (.csv, .xlsx)" = "archivo"),
                    selected = character(0)
                  ),
                  conditionalPanel(
                    condition = paste0("output['", ns("fuente_cerr_es_archivo"), "']"),
                    div(
                      class = "p-2 mb-2",
                      style = paste0("background:", colores$fondo,
                                     "; border-radius:6px; font-size:12px;"),
                      bs_icon("info-circle", class = "me-1",
                              style = paste0("color:", colores$primario)),
                      tags$strong("Formato esperado:"),
                      tags$ul(
                        class = "mb-1 mt-1 ps-3",
                        tags$li("Una fila por participante"),
                        tags$li("Una columna por ítem Likert (valores numéricos)"),
                        tags$li("Columnas opcionales para agrupar (sexo, región, etc.)"),
                        tags$li("Primera fila: nombres de columnas")
                      )
                    ),
                    fileInput(
                      ns("archivo_cerradas"),
                      label = NULL,
                      accept = c(".csv", ".xlsx"),
                      buttonLabel = tagList(
                        bs_icon("folder2-open", class = "me-1"), "Examinar…"),
                      placeholder = "Sin archivo seleccionado"
                    )
                  )
                )
              ),

              # ── Card: Configuración ────────────────────
              card(
                class = "mt-3",
                card_header(
                  bs_icon("sliders", class = "me-1"), "Configuración"
                ),
                card_body(
                  uiOutput(ns("sel_items_ui")),
                  uiOutput(ns("sel_grupo_cerradas_ui")),
                  tags$hr(),
                  numericInput(ns("escala_min"), "Valor mínimo de escala:", 1, 1, 5),
                  numericInput(ns("escala_max"), "Valor máximo de escala:", 5, 2, 10),
                  tags$hr(),
                  actionButton(
                    ns("procesar_cerradas"),
                    label = tagList(bs_icon("play-fill", class = "me-1"),
                                    "Cargar datos"),
                    class = "btn btn-primary w-100"
                  )
                )
              )
            ),
            div(
              uiOutput(ns("estado_cerradas_ui")),
              br(),
              DTOutput(ns("preview_cerradas"))
            )
          )
        )
      ), # /PESTAÑA 1

      # ══════════════════════════════════════════════════
      # PESTAÑA 2: Perfil de respuesta
      # ══════════════════════════════════════════════════
      nav_panel(
        title = tagList(bs_icon("bar-chart-fill", class = "me-1"),
                        "Perfil Likert"),
        card_body(
          div(
            class = "alert mb-3",
            style = paste0("background:", colores$fondo,
                           "; border-left: 4px solid ", colores$primario, ";"),
            tags$b(bs_icon("info-circle", class = "me-1",
                           style = paste0("color:", colores$primario)),
                   "Perfil de respuesta Likert"),
            tags$p(class = "small text-muted mb-0 mt-1",
              "Gráfico de barras divergentes: las respuestas negativas/bajas se",
              " extienden hacia la izquierda y las positivas/altas hacia la derecha.",
              " Permite comparar visualmente la distribución de todos los ítems.")
          ),
          layout_columns(
            col_widths = c(3, 9),
            div(
              card(
                card_header(bs_icon("sliders", class = "me-1"), "Controles"),
                card_body(
                  checkboxInput(ns("likert_porcentaje"),
                                "Mostrar porcentajes", TRUE),
                  checkboxInput(ns("likert_ordenar"),
                                "Ordenar por media", TRUE),
                  tags$hr(),
                  uiOutput(ns("metricas_likert_ui"))
                )
              ),
              card(
                class = "mt-3",
                card_header(bs_icon("lightbulb", class = "me-1"), "Interpretación"),
                card_body(
                  style = "white-space: normal; word-wrap: break-word;",
                  uiOutput(ns("exp_likert_ui"))
                )
              )
            ),
            div(
              card(
                class = "mb-3",
                card_header(bs_icon("bar-chart-fill", class = "me-1"),
                            "Perfil de respuesta — diverging bars"),
                card_body(
                  uiOutput(ns("plot_likert_ui"))
                )
              ),
              card(
                card_header(bs_icon("table", class = "me-1"),
                            "Estadísticas por ítem"),
                card_body(
                  DTOutput(ns("tabla_stats_items"), height = "250px")
                )
              )
            )
          )
        )
      ), # /PESTAÑA 2

      # ══════════════════════════════════════════════════
      # PESTAÑA 3: Correlaciones
      # ══════════════════════════════════════════════════
      nav_panel(
        title = tagList(bs_icon("grid-3x3", class = "me-1"), "Correlaciones"),
        card_body(
          div(
            class = "alert mb-3",
            style = paste0("background:", colores$fondo,
                           "; border-left: 4px solid ", colores$primario, ";"),
            tags$b(bs_icon("info-circle", class = "me-1",
                           style = paste0("color:", colores$primario)),
                   "Matriz de correlaciones"),
            tags$p(class = "small text-muted mb-0 mt-1",
              "Muestra la relación entre pares de ítems. Valores cercanos a 1",
              " indican correlación positiva fuerte; cercanos a -1, negativa.",
              " Útil para identificar dimensiones subyacentes.")
          ),
          layout_columns(
            col_widths = c(3, 9),
            div(
              card(
                card_header(bs_icon("sliders", class = "me-1"), "Controles"),
                card_body(
                  selectInput(
                    ns("corr_metodo"),
                    "Método:",
                    choices = c(
                      "Pearson"  = "pearson",
                      "Spearman" = "spearman",
                      "Kendall"  = "kendall"
                    )
                  ),
                  checkboxInput(ns("corr_mostrar_val"),
                                "Mostrar valores", TRUE),
                  tags$hr(),
                  uiOutput(ns("alpha_ui"))
                )
              ),
              card(
                class = "mt-3",
                card_header(bs_icon("lightbulb", class = "me-1"), "Interpretación"),
                card_body(
                  style = "white-space: normal; word-wrap: break-word;",
                  uiOutput(ns("exp_corr_ui"))
                )
              )
            ),
            card(
              card_header(bs_icon("grid-3x3", class = "me-1"),
                          "Matriz de correlaciones entre ítems"),
              card_body(
                uiOutput(ns("plot_corr_ui"))
              )
            )
          )
        )
      ), # /PESTAÑA 3

      # ══════════════════════════════════════════════════
      # PESTAÑA 4: Análisis de correspondencias
      # ══════════════════════════════════════════════════
      nav_panel(
        title = tagList(bs_icon("diagram-3", class = "me-1"),
                        "Correspondencias"),
        card_body(
          div(
            class = "alert mb-3",
            style = paste0("background:", colores$fondo,
                           "; border-left: 4px solid ", colores$primario, ";"),
            tags$b(bs_icon("info-circle", class = "me-1",
                           style = paste0("color:", colores$primario)),
                   "¿Qué es el ACM?"),
            tags$p(class = "small text-muted mb-1 mt-1",
              tags$strong("Análisis de Correspondencias Múltiples (ACM)"),
              " es una técnica de reducción de dimensionalidad para datos categóricos.",
              " Mapea ítems y categorías de respuesta en un espacio bidimensional."),
            tags$p(class = "small text-muted mb-0",
              tags$strong("Biplot:"), " muestra ítems (puntos) y grupos en el mismo plano.",
              " Puntos cercanos indican perfiles de respuesta similares.",
              tags$br(),
              tags$strong("Scree plot:"), " muestra cuánta varianza explica cada dimensión.",
              tags$br(),
              "Los valores numéricos se discretizan automáticamente en cuartiles.")
          ),
          layout_columns(
            col_widths = c(3, 9),
            div(
              card(
                card_header(bs_icon("sliders", class = "me-1"), "Controles"),
                card_body(
                  numericInput(ns("acm_ncomp"), "Dimensiones a retener:",
                               value = 2, min = 2, max = 5, step = 1),
                  checkboxInput(ns("acm_ellipses"),
                                "Mostrar elipses de confianza", TRUE),
                  tags$hr(),
                  actionButton(
                    ns("calcular_acm"),
                    tagList(bs_icon("play-fill", class = "me-1"), "Calcular ACM"),
                    class = "btn btn-primary w-100"
                  ),
                  div(class = "mt-2", uiOutput(ns("estado_acm_ui")))
                )
              ),
              card(
                class = "mt-3",
                card_header(bs_icon("lightbulb", class = "me-1"), "Interpretación"),
                card_body(
                  style = "white-space: normal; word-wrap: break-word;",
                  uiOutput(ns("exp_acm_ui"))
                )
              )
            ),
            div(
              card(
                class = "mb-3",
                card_header(bs_icon("diagram-3", class = "me-1"),
                            "Biplot ACM — ítems y grupos"),
                card_body(plotOutput(ns("plot_acm"), height = "440px"))
              ),
              layout_columns(
                col_widths = c(6, 6),
                card(
                  card_header(bs_icon("bar-chart", class = "me-1"),
                              "Varianza explicada"),
                  card_body(plotOutput(ns("plot_acm_scree"), height = "220px"))
                ),
                card(
                  card_header(bs_icon("table", class = "me-1"),
                              "Coordenadas (Dim 1–2)"),
                  card_body(
                    DTOutput(ns("tabla_acm_coords"), height = "220px")
                  )
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
            "Código reproducible con FactoMineR, psych y ggplot2."),
          div(class = "d-flex gap-2 mb-3",
            downloadButton(ns("descarga_codigo_cerradas"),
                           "Descargar .R",
                           class = "btn-sm btn-outline-primary")
          ),
          verbatimTextOutput(ns("codigo_r_cerradas")) |>
            tagAppendAttributes(class = "codigo-bloque")
        )
      ) # /PESTAÑA 5

    ) # /navset_card_tab
  ) # /tagList
} # /mod_enc_cerradas_ui


# ── Server ────────────────────────────────────────────────
mod_enc_cerradas_server <- function(id) {
  moduleServer(id, function(input, output, session) {
    ns <- session$ns

    # ── Datos crudos ─────────────────────────────────────
    output$fuente_cerr_es_archivo <- reactive({
      !is.null(input$fuente_archivo_cerr) &&
        length(input$fuente_archivo_cerr) > 0 &&
        input$fuente_archivo_cerr == "archivo"
    })
    outputOptions(output, "fuente_cerr_es_archivo", suspendWhenHidden = FALSE)

    observeEvent(input$fuente_cerradas, {
      req(input$fuente_cerradas)
      updateRadioButtons(session, "fuente_archivo_cerr", selected = character(0))
    })
    observeEvent(input$fuente_archivo_cerr, {
      req(input$fuente_archivo_cerr)
      updateRadioButtons(session, "fuente_cerradas", selected = character(0))
    })

    fuente_cerr_activa <- reactive({
      if (!is.null(input$fuente_archivo_cerr) &&
          length(input$fuente_archivo_cerr) > 0 &&
          input$fuente_archivo_cerr == "archivo") "archivo" else "ejemplo"
    })

    datos_cerr <- reactive({
      if (fuente_cerr_activa() == "ejemplo") return(EJEMPLO_CERRADAS)
      req(input$archivo_cerradas)
      ext <- tools::file_ext(input$archivo_cerradas$name)
      df  <- if (ext == "xlsx")
        readxl::read_excel(input$archivo_cerradas$datapath)
      else
        readr::read_csv(input$archivo_cerradas$datapath, show_col_types = FALSE)
      as.data.frame(df)
    })

    # ── Selector de ítems ─────────────────────────────────
    output$sel_items_ui <- renderUI({
      req(datos_cerr())
      nms_num <- names(datos_cerr())[sapply(datos_cerr(), is.numeric)]
      checkboxGroupInput(
        ns("items_sel"),
        label    = "Ítems (columnas numéricas):",
        choices  = nms_num,
        selected = nms_num
      )
    })

    output$sel_grupo_cerradas_ui <- renderUI({
      req(datos_cerr())
      nms_cat <- names(datos_cerr())[sapply(datos_cerr(), function(x)
        is.character(x) | is.factor(x))]
      selectInput(
        ns("col_grupo_cerradas"),
        "Variable de grupo (opcional):",
        choices  = c("(ninguna)" = "", nms_cat),
        selected = if ("grupo" %in% nms_cat) "grupo" else ""
      )
    })

    # ── Datos procesados ──────────────────────────────────
    datos_items <- reactiveVal(NULL)
    datos_grupo_cerr <- reactiveVal(NULL)

    observeEvent(input$procesar_cerradas, {
      req(datos_cerr(), input$items_sel)
      df    <- datos_cerr()
      items <- intersect(input$items_sel, names(df))
      req(length(items) >= 2)

      df_items <- df[, items, drop = FALSE]
      # Coercionar a numérico y filtrar rango
      df_items <- as.data.frame(
        lapply(df_items, function(x) {
          x <- suppressWarnings(as.numeric(as.character(x)))
          x[x < input$escala_min | x > input$escala_max] <- NA
          x
        })
      )
      datos_items(df_items)

      col_g <- input$col_grupo_cerradas
      if (!is.null(col_g) && col_g != "" && col_g %in% names(df)) {
        datos_grupo_cerr(as.character(df[[col_g]]))
      } else {
        datos_grupo_cerr(NULL)
      }

      showNotification(
        paste0(nrow(df_items), " respuestas · ", length(items), " ítems cargados."),
        type = "message", duration = 3
      )
    })

    # Inicializar automáticamente con ejemplo
    observe({
      req(fuente_cerr_activa() == "ejemplo")
      req(is.null(datos_items()))
      req(!is.null(input$items_sel))
      shinyjs::click(ns("procesar_cerradas"))
    })

    output$estado_cerradas_ui <- renderUI({
      if (is.null(datos_items())) {
        return(div(
          class = "alert alert-info small py-2 px-3",
          bs_icon("info-circle", class = "me-1"),
          "Seleccioná los ítems y hacé clic en ", strong("Cargar datos"), "."
        ))
      }
      df <- datos_items()
      n_resp <- nrow(df)
      n_comp <- sum(complete.cases(df))
      div(
        class = "alert alert-success small py-2 px-3",
        bs_icon("check-circle-fill", class = "me-1"),
        strong("Datos cargados. "),
        n_resp, " filas · ", ncol(df), " ítems · ",
        n_comp, " casos completos",
        if (!is.null(datos_grupo_cerr())) {
          paste0(" · ", length(unique(datos_grupo_cerr())), " grupos")
        }
      )
    })

    output$preview_cerradas <- renderDT({
      req(datos_cerr())
      datatable(
        head(datos_cerr(), 6),
        options  = list(dom = "tp", pageLength = 10,
                         autoWidth = FALSE, scrollX = FALSE),
        rownames = FALSE,
        class    = "table-sm table-striped"
      )
    })

    # ── Estadísticas por ítem ─────────────────────────────
    stats_items <- reactive({
      req(datos_items())
      df <- datos_items()
      data.frame(
        Item   = names(df),
        N      = sapply(df, function(x) sum(!is.na(x))),
        Media  = round(sapply(df, mean, na.rm = TRUE), 2),
        DE     = round(sapply(df, sd,   na.rm = TRUE), 2),
        Mediana = sapply(df, median, na.rm = TRUE),
        Min    = sapply(df, min,  na.rm = TRUE),
        Max    = sapply(df, max,  na.rm = TRUE),
        stringsAsFactors = FALSE
      )
    })

    output$tabla_stats_items <- renderDT({
      req(stats_items())
      datatable(
        stats_items(),
        options  = list(dom = "t", pageLength = 20),
        rownames = FALSE,
        class    = "table-sm table-striped"
      )
    })

    # ── Métricas Likert ───────────────────────────────────
    output$metricas_likert_ui <- renderUI({
      req(stats_items())
      st <- stats_items()
      top_item <- st$Item[which.max(st$Media)]
      bot_item <- st$Item[which.min(st$Media)]
      tagList(
        vbox_card("arrow-up-circle", "Ítem más positivo",
                  top_item, colores$exito),
        br(),
        vbox_card("arrow-down-circle", "Ítem más crítico",
                  bot_item, colores$peligro)
      )
    })

    # ── Plot Likert ───────────────────────────────────────
    output$plot_likert_ui <- renderUI({
      req(datos_items())
      n_items <- ncol(datos_items())
      altura  <- max(300L, n_items * 45L)
      plotOutput(ns("plot_likert"), height = paste0(altura, "px"))
    })

    output$exp_likert_ui <- renderUI({
      req(datos_items())
      df   <- datos_items()
      meds <- colMeans(df, na.rm = TRUE)
      top  <- names(which.max(meds))
      bot  <- names(which.min(meds))
      mid  <- (max(df, na.rm=TRUE) + min(df, na.rm=TRUE)) / 2
      p(paste0(
        "El ítem con mayor puntuación media fue '", top,
        "' (M = ", round(max(meds), 2), "). ",
        "El ítem con menor puntuación fue '", bot,
        "' (M = ", round(min(meds), 2), ")."
      ))
    })

    output$plot_likert <- renderPlot({
      req(datos_items())
      df   <- datos_items()
      emin <- input$escala_min
      emax <- input$escala_max
      rng  <- emax - emin + 1
      mid  <- (emin + emax) / 2

      # Colores para escala: rojo → gris → verde
      n_cat  <- rng
      colores_likert <- colorRampPalette(
        c(colores$peligro, "#F1CE63", "#CCCCCC", colores$secundario, colores$primario)
      )(n_cat)

      # Distribución por ítem y categoría
      df_long <- tidyr::pivot_longer(
        dplyr::mutate(df, id = dplyr::row_number()),
        cols = -id, names_to = "item", values_to = "resp"
      ) |>
        dplyr::filter(!is.na(resp)) |>
        dplyr::group_by(item, resp) |>
        dplyr::summarise(n = dplyr::n(), .groups = "drop") |>
        dplyr::group_by(item) |>
        dplyr::mutate(
          pct  = n / sum(n) * 100,
          resp = factor(resp, levels = emin:emax)
        ) |>
        dplyr::ungroup()

      # Ordenar ítems por media si se solicita
      if (input$likert_ordenar) {
        orden <- stats_items()$Item[order(stats_items()$Media)]
        df_long$item <- factor(df_long$item, levels = orden)
      }

      # Ajuste divergente: negativo / positivo respecto al punto medio
      df_div <- df_long |>
        dplyr::mutate(
          valor_num = as.numeric(as.character(resp)),
          lado      = dplyr::case_when(
            valor_num < mid  ~ "negativo",
            valor_num > mid  ~ "positivo",
            TRUE             ~ "neutro"
          ),
          pct_dir   = dplyr::case_when(
            lado == "negativo" ~ -pct,
            lado == "neutro"   ~ 0,
            TRUE               ~  pct
          )
        )

      etiq_fun <- if (input$likert_porcentaje) {
        ggplot2::aes(label = paste0(round(abs(pct_dir)), "%"))
      } else {
        ggplot2::aes(label = n)
      }

      ggplot2::ggplot(
        df_div,
        ggplot2::aes(
          x    = pct_dir,
          y    = item,
          fill = resp
        )
      ) +
        ggplot2::geom_col(
          position = ggplot2::position_stack(vjust = 0.5),
          width    = 0.7,
          alpha    = 0.9
        ) +
        ggplot2::geom_text(
          etiq_fun,
          position = ggplot2::position_stack(vjust = 0.5),
          size     = 3,
          color    = "white",
          fontface = "bold",
          na.rm    = TRUE
        ) +
        ggplot2::geom_vline(xintercept = 0, linewidth = 0.4,
                            color = colores$texto) +
        ggplot2::scale_fill_manual(
          values = setNames(colores_likert, as.character(emin:emax)),
          name   = "Respuesta"
        ) +
        ggplot2::labs(
          x = if (input$likert_porcentaje) "Porcentaje (%)" else "Conteo",
          y = NULL
        ) +
        ggplot2::theme_minimal(base_size = 13) +
        ggplot2::theme(
          panel.grid.minor  = ggplot2::element_blank(),
          panel.grid.major.y = ggplot2::element_blank(),
          legend.position   = "top"
        )
    })

    # ── Correlaciones ─────────────────────────────────────
    output$alpha_ui <- renderUI({
      req(datos_items())
      df <- datos_items()[, , drop = FALSE]
      df_comp <- df[complete.cases(df), ]
      if (ncol(df_comp) < 2 || nrow(df_comp) < 3) return(NULL)

      # Alfa de Cronbach manual
      k    <- ncol(df_comp)
      vars <- apply(df_comp, 2, var, na.rm = TRUE)
      var_total <- var(rowSums(df_comp, na.rm = TRUE), na.rm = TRUE)
      alpha_val <- (k / (k - 1)) * (1 - sum(vars) / var_total)

      semaforo <- if (alpha_val >= 0.8) "sem-ok"
                  else if (alpha_val >= 0.6) "sem-warn"
                  else "sem-bad"
      icono <- if (alpha_val >= 0.8) "check-circle"
               else if (alpha_val >= 0.6) "exclamation-circle"
               else "x-circle"

      div(
        class = paste("p-2", semaforo),
        style = "border-radius:6px;",
        bs_icon(icono, class = "me-1"),
        tags$b("Alfa de Cronbach: "),
        round(alpha_val, 3),
        p(class = "small mb-0 mt-1",
          if (alpha_val >= 0.8) "Consistencia interna adecuada (≥ 0.8)"
          else if (alpha_val >= 0.6) "Consistencia aceptable (0.6–0.8)"
          else "Consistencia baja (< 0.6)")
      )
    })

    output$plot_corr_ui <- renderUI({
      req(datos_items())
      n_items <- ncol(datos_items())
      altura  <- max(500L, n_items * 65L)
      plotOutput(ns("plot_corr_items"), height = paste0(altura, "px"))
    })

    output$exp_corr_ui <- renderUI({
      req(datos_items())
      df      <- datos_items()[complete.cases(datos_items()), ]
      cor_mat <- cor(df, method = input$corr_metodo, use = "pairwise.complete.obs")
      diag(cor_mat) <- NA
      max_idx <- which(cor_mat == max(cor_mat, na.rm = TRUE), arr.ind = TRUE)[1, ]
      min_idx <- which(cor_mat == min(cor_mat, na.rm = TRUE), arr.ind = TRUE)[1, ]
      p(paste0(
        "La correlación más alta fue entre '", rownames(cor_mat)[max_idx[1]],
        "' y '", colnames(cor_mat)[max_idx[2]],
        "' (r = ", round(max(cor_mat, na.rm = TRUE), 2), "). ",
        "La más baja entre '", rownames(cor_mat)[min_idx[1]],
        "' y '", colnames(cor_mat)[min_idx[2]],
        "' (r = ", round(min(cor_mat, na.rm = TRUE), 2), ")."
      ))
    })

    output$plot_corr_items <- renderPlot({
      req(datos_items())
      df      <- datos_items()[complete.cases(datos_items()), ]
      validate(need(ncol(df) >= 2, "Se necesitan al menos 2 ítems."))

      cor_mat <- cor(df, method = input$corr_metodo, use = "pairwise.complete.obs")
      nms     <- colnames(cor_mat)
      df_long <- data.frame(
        x    = rep(nms, each  = length(nms)),
        y    = rep(nms, times = length(nms)),
        corr = as.vector(cor_mat)
      )
      df_long$x <- factor(df_long$x, levels = nms)
      df_long$y <- factor(df_long$y, levels = rev(nms))

      p <- ggplot2::ggplot(df_long,
        ggplot2::aes(x = x, y = y, fill = corr)) +
        ggplot2::geom_tile(color = "white", linewidth = 0.5) +
        ggplot2::scale_fill_gradient2(
          low      = colores$peligro,
          mid      = "white",
          high     = colores$primario,
          midpoint = 0,
          limits   = c(-1, 1),
          name     = "r"
        ) +
        ggplot2::labs(x = NULL, y = NULL) +
        ggplot2::theme_minimal(base_size = 14) +
        ggplot2::theme(
          axis.text.x     = ggplot2::element_text(angle = 45, hjust = 1, size = 12),
          axis.text.y     = ggplot2::element_text(size = 12),
          panel.grid      = ggplot2::element_blank(),
          legend.position = "right"
        )

      if (input$corr_mostrar_val) {
        p <- p + ggplot2::geom_label(
          ggplot2::aes(label = round(corr, 2)),
          size       = 4,
          fill       = "white",
          color      = colores$texto,
          label.size = 0
        )
      }
      p
    })

    # ── ACM ───────────────────────────────────────────────
    modelo_acm <- reactiveVal(NULL)
    estado_acm <- reactiveVal(NULL)

    observeEvent(input$calcular_acm, {
      req(datos_items())
      withProgress(message = "Calculando ACM…", value = 0.4, {
        tryCatch({
          df <- datos_items()[complete.cases(datos_items()), ]
          validate(need(ncol(df) >= 2,
                        "Se necesitan al menos 2 ítems."))

          # Discretizar en cuartiles para tratamiento categórico
          df_cat <- as.data.frame(
            lapply(df, function(x) {
              factor(x, levels = sort(unique(x)),
                     labels = paste0("cat", sort(unique(x))))
            })
          )

          if (!is.null(datos_grupo_cerr())) {
            grp <- datos_grupo_cerr()[complete.cases(datos_items())]
            df_cat$grupo <- factor(grp)
          }

          acm <- FactoMineR::MCA(
            df_cat,
            ncp     = input$acm_ncomp,
            graph   = FALSE,
            quali.sup = if ("grupo" %in% names(df_cat))
                          which(names(df_cat) == "grupo") else NULL
          )
          modelo_acm(acm)
          estado_acm("ok")
          incProgress(0.6)
        }, error = function(e) {
          estado_acm("error")
          showNotification(paste("Error en ACM:", conditionMessage(e)),
                           type = "error", duration = 5)
        })
      })
    })

    output$estado_acm_ui <- renderUI({
      est <- estado_acm()
      if (is.null(est)) return(NULL)
      if (est == "ok")
        div(class = "alert alert-success small py-1 px-2",
            bs_icon("check-circle-fill", class = "me-1"), "ACM calculado.")
      else
        div(class = "alert alert-danger small py-1 px-2",
            bs_icon("exclamation-triangle", class = "me-1"), "Error en ACM.")
    })

    output$plot_acm <- renderPlot({
      req(modelo_acm())
      acm <- modelo_acm()

      # Coordenadas de las categorías activas
      coords_cat <- as.data.frame(acm$var$coord[, 1:2])
      names(coords_cat) <- c("Dim1", "Dim2")
      coords_cat$label <- rownames(coords_cat)
      coords_cat$tipo  <- "Categoría de ítem"

      p <- ggplot2::ggplot(coords_cat,
          ggplot2::aes(x = Dim1, y = Dim2)) +
        ggplot2::geom_hline(yintercept = 0, linewidth = 0.3,
                            color = colores$texto, linetype = "dashed") +
        ggplot2::geom_vline(xintercept = 0, linewidth = 0.3,
                            color = colores$texto, linetype = "dashed") +
        ggplot2::geom_point(color = colores$primario,
                            size = 2.5, alpha = 0.8) +
        ggplot2::geom_text(
          ggplot2::aes(label = label),
          size   = 3,
          color  = colores$primario,
          hjust  = -0.15,
          vjust  = 0.5,
          check_overlap = TRUE
        ) +
        ggplot2::labs(
          x = paste0("Dim 1 (",
                     round(acm$eig[1, 2], 1), "%)"),
          y = paste0("Dim 2 (",
                     round(acm$eig[2, 2], 1), "%)"),
          title = NULL
        ) +
        ggplot2::theme_minimal(base_size = 13) +
        ggplot2::theme(panel.grid.minor = ggplot2::element_blank())

      # Agregar grupos suplementarios si existen
      if (!is.null(acm$quali.sup)) {
        coords_grp <- as.data.frame(acm$quali.sup$coord[, 1:2])
        names(coords_grp) <- c("Dim1", "Dim2")
        coords_grp$label <- rownames(coords_grp)
        if (input$acm_ellipses && !is.null(datos_grupo_cerr())) {
          # Agregar puntos de grupos
          p <- p +
            ggplot2::geom_point(
              data  = coords_grp,
              color = colores$acento, size = 4, shape = 17
            ) +
            ggplot2::geom_text(
              data  = coords_grp,
              ggplot2::aes(label = label),
              color = colores$acento, size = 3.5,
              fontface = "bold", hjust = -0.15
            )
        }
      }
      p
    })

    output$plot_acm_scree <- renderPlot({
      req(modelo_acm())
      acm <- modelo_acm()
      eig <- as.data.frame(acm$eig)
      eig$dim <- factor(paste0("Dim ", seq_len(nrow(eig))))

      ggplot2::ggplot(eig,
        ggplot2::aes(x = dim, y = `percentage of variance`)) +
        ggplot2::geom_col(fill = colores$primario, alpha = 0.85, width = 0.6) +
        ggplot2::geom_text(
          ggplot2::aes(label = paste0(round(`percentage of variance`, 1), "%")),
          vjust = -0.4, size = 3.5, color = colores$texto
        ) +
        ggplot2::ylim(0, max(eig$`percentage of variance`) * 1.2) +
        ggplot2::labs(x = NULL, y = "Varianza explicada (%)") +
        ggplot2::theme_minimal(base_size = 12) +
        ggplot2::theme(panel.grid.minor = ggplot2::element_blank())
    })

    output$exp_acm_ui <- renderUI({
      req(modelo_acm())
      res <- modelo_acm()
      eig <- res$eig
      dim1_pct <- round(eig[1, 2], 1)
      dim2_pct <- round(eig[2, 2], 1)
      p(paste0(
        "Las dos primeras dimensiones explican el ",
        round(dim1_pct + dim2_pct, 1),
        "% de la varianza total (Dim 1: ", dim1_pct,
        "%, Dim 2: ", dim2_pct, "%). ",
        "Puntos cercanos en el biplot indican perfiles de respuesta similares."
      ))
    })

    output$tabla_acm_coords <- renderDT({
      req(modelo_acm())
      acm   <- modelo_acm()
      coords <- round(as.data.frame(acm$var$coord[, 1:2]), 3)
      names(coords) <- c("Dim 1", "Dim 2")
      coords$Categoría <- rownames(coords)
      coords <- coords[, c("Categoría", "Dim 1", "Dim 2")]
      datatable(
        coords,
        options  = list(pageLength = 10, dom = "tp",
                         autoWidth = FALSE, scrollX = FALSE),
        rownames = FALSE,
        class    = "table-sm table-striped"
      )
    })

    # ── Código R ──────────────────────────────────────────
    codigo_cerradas_gen <- reactive({
      items_str <- paste0(
        "c('", paste(input$items_sel %||% "item1", collapse = "', '"), "')"
      )
      encabezado_script("Preguntas cerradas — Likert y ACM") |>
        paste0(
          "# -- Paquetes ------------------------------------------------\n",
          "library(FactoMineR)\n",
          "library(factoextra)\n",
          "library(tidyverse)\n",
          "library(readr)   # o readxl para XLSX\n\n",
          "# -- Datos ----------------------------------------------------\n",
          "df <- read_csv('tu_encuesta.csv')\n",
          "# Seleccioná las columnas de ítems Likert:\n",
          "items <- ", items_str, "\n",
          "df_items <- df[, items]\n\n",
          "# -- Estadísticas descriptivas --------------------------------\n",
          "summary(df_items)\n",
          "apply(df_items, 2, mean, na.rm = TRUE)  # medias\n",
          "apply(df_items, 2, sd,   na.rm = TRUE)  # desv. estándar\n\n",
          "# -- Alfa de Cronbach (psych) ---------------------------------\n",
          "# library(psych)\n",
          "# alpha(df_items)\n\n",
          "# -- Correlaciones entre ítems --------------------------------\n",
          "cor_mat <- cor(df_items, method = '", input$corr_metodo, "',\n",
          "               use = 'pairwise.complete.obs')\n",
          "corrplot::corrplot(cor_mat, method = 'color', type = 'upper',\n",
          "                   addCoef.col = 'black', tl.col = 'black')\n\n",
          "# -- Diverging stacked bar (likert) ---------------------------\n",
          "# library(likert)\n",
          "# df_fac <- data.frame(lapply(df_items, function(x)\n",
          "#   factor(x, levels = ", input$escala_min, ":", input$escala_max, ")))\n",
          "# lk <- likert(df_fac)\n",
          "# plot(lk, type = 'bar')\n\n",
          "# -- ACM (FactoMineR) -----------------------------------------\n",
          "# Convertir a factores para ACM\n",
          "df_cat <- data.frame(lapply(df_items, function(x)\n",
          "  factor(x, levels = ", input$escala_min, ":", input$escala_max, ")))\n\n",
          "acm <- MCA(df_cat, ncp = ", input$acm_ncomp, ", graph = FALSE)\n\n",
          "# Biplot\n",
          "fviz_mca_biplot(acm,\n",
          "  repel         = TRUE,\n",
          "  col.var       = '#1170AA',\n",
          "  col.ind       = '#A3ACB9',\n",
          "  ggtheme       = theme_minimal()\n",
          ")\n\n",
          "# Varianza explicada por dimensión\n",
          "fviz_screeplot(acm, addlabels = TRUE, barfill = '#1170AA')\n"
        )
    })

    output$codigo_r_cerradas <- renderText({ codigo_cerradas_gen() })

    output$descarga_codigo_cerradas <- downloadHandler(
      filename = function() paste0("enc_cerradas_", format(Sys.Date(), "%Y%m%d"), ".R"),
      content  = function(file) writeLines(codigo_cerradas_gen(), file)
    )

  }) # /moduleServer
} # /mod_enc_cerradas_server
