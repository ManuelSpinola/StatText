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

            # Panel izquierdo: carga y preprocesamiento
            div(

              # ── Card: Fuente de texto ──────────────────
              card(
                card_header(
                  bs_icon("database", class = "me-1"), "Fuente de texto"
                ),
                card_body(

                  tags$p(class = "small fw-semibold text-muted mb-1",
                         bs_icon("bookmark", class = "me-1"), "Ejemplos"),
                  radioButtons(
                    ns("fuente_ejemplo"),
                    label = NULL,
                    choices = c(
                      "Discurso político"       = "discurso",
                      "Entrevista territorial"  = "entrevista"
                    ),
                    selected = "discurso"
                  ),

                  tags$hr(class = "my-2"),
                  tags$p(class = "small fw-semibold text-muted mb-1",
                         bs_icon("pencil", class = "me-1"), "Pegar texto"),
                  radioButtons(
                    ns("fuente_pegar"),
                    label = NULL,
                    choices = c("Pegar texto" = "pegar"),
                    selected = character(0)
                  ),
                  conditionalPanel(
                    condition = paste0("output['", ns("fuente_activa_pegar"), "']"),
                    textAreaInput(
                      ns("texto_input"),
                      label = NULL,
                      placeholder = "Pegá aquí tu texto...",
                      rows = 6,
                      width = "100%"
                    )
                  ),

                  tags$hr(class = "my-2"),
                  tags$p(class = "small fw-semibold text-muted mb-1",
                         bs_icon("upload", class = "me-1"), "Subir archivo"),
                  radioButtons(
                    ns("fuente_archivo"),
                    label = NULL,
                    choices = c("Subir archivo" = "archivo"),
                    selected = character(0)
                  ),
                  conditionalPanel(
                    condition = paste0("output['", ns("fuente_activa_archivo"), "']"),
                    fileInput(
                      ns("archivo_texto"),
                      label = NULL,
                      accept = c(".txt", ".csv", ".xlsx", ".docx",
                                 ".odt", ".pdf", ".rtf"),
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
                      tags$strong("Formatos: "),
                      ".txt · .csv · .xlsx · .docx · .odt · .pdf · .rtf"
                    ),
                    conditionalPanel(
                      condition = paste0("output['", ns("archivo_es_csv"), "']"),
                      selectInput(
                        ns("col_texto"),
                        "Columna de texto (CSV / Excel):",
                        choices = NULL
                      )
                    )
                  )
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
                    "El idioma se detecta automáticamente."),
                  checkboxInput(ns("remover_numeros"),
                                "Remover números", TRUE),
                  checkboxInput(ns("remover_punt"),
                                "Remover puntuación", TRUE),
                  checkboxInput(ns("aplicar_stemming"),
                                "Aplicar stemming", FALSE),
                  div(
                    class = "p-2 mb-2",
                    style = paste0("background:", colores$fondo,
                                   "; border-radius:6px; font-size:12px;"),
                    bs_icon("info-circle", class = "me-1",
                            style = paste0("color:", colores$primario)),
                    tags$strong("Stemming:"),
                    " reduce cada palabra a su raíz. 'participación',",
                    " 'participar' y 'participando' se convierten en 'particip'.",
                    " Agrupa variantes de un mismo término pero hace el texto",
                    " menos legible."
                  ),
                  numericInput(ns("min_nchar"),
                               "Longitud mínima de término:",
                               value = 3, min = 1, max = 10, step = 1),
                  tags$hr(),
                  actionButton(
                    ns("analizar"),
                    label = tagList(
                      bs_icon("play-fill", class = "me-1"), "Analizar corpus"),
                    class = "btn btn-primary w-100"
                  )
                )
              )
            ),

            # Panel derecho: estado, métricas y vista posterior
            div(
              uiOutput(ns("estado_corpus_ui")),
              br(),
              uiOutput(ns("metricas_corpus_ui")),
              uiOutput(ns("vista_posterior_ui"))
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
          div(
            class = "alert mb-3",
            style = paste0("background:", colores$fondo,
                           "; border-left: 4px solid ", colores$primario, ";"),
            tags$b(
              bs_icon("info-circle", class = "me-1",
                      style = paste0("color:", colores$primario)),
              "¿Qué muestra este análisis?"
            ),
            tags$p(class = "small text-muted mb-1 mt-1",
              "La ", tags$strong("frecuencia de términos"), " indica cuántas veces",
              " aparece cada palabra en el corpus tras el preprocesamiento.",
              " Las palabras más frecuentes suelen revelar los temas centrales del texto."
            ),
            tags$p(class = "small text-muted mb-0",
              tags$strong("Frecuencia absoluta:"), " número de veces que aparece el término.",
              tags$br(),
              tags$strong("Rango:"), " posición del término según su frecuencia (1 = más frecuente).",
              tags$br(),
              "El gráfico de ", tags$strong("lollipop"), " es una alternativa visual más limpia",
              " para comparar frecuencias cuando hay muchos términos."
            )
          ),
          layout_columns(
            col_widths = c(4, 8),
            card(
              card_header(bs_icon("sliders", class = "me-1"), "Controles"),
              card_body(
                sliderInput(ns("n_top_terms"), "Términos a mostrar:",
                            min = 5, max = 50, value = 10, step = 5),
                selectInput(
                  ns("freq_tipo"),
                  "Tipo de gráfico:",
                  choices = c(
                    "Barras"    = "barras",
                    "Lollipop"  = "lollipop"
                  )
                ),
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
                card_header(bs_icon("table", class = "me-1"),
                            "Tabla de frecuencias"),
                card_body(DTOutput(ns("tabla_freq"), height = "250px"))
              ),
              card(
                card_header(bs_icon("bar-chart-fill", class = "me-1"),
                            "Términos más frecuentes"),
                card_body(uiOutput(ns("plot_freq_ui")))
              )
            ),
            uiOutput(ns("exp_freq_ui"))
          )
        )
      ), # /PESTAÑA 2

      # ══════════════════════════════════════════════════
      # PESTAÑA 3: Nube de palabras
      # ══════════════════════════════════════════════════
      nav_panel(
        title = tagList(bs_icon("cloud", class = "me-1"), "Nube"),
        card_body(
          div(
            class = "alert mb-3",
            style = paste0("background:", colores$fondo,
                           "; border-left: 4px solid ", colores$secundario, ";"),
            tags$b(
              bs_icon("info-circle", class = "me-1",
                      style = paste0("color:", colores$secundario)),
              "¿Qué es una nube de palabras?"
            ),
            tags$p(class = "small text-muted mb-1 mt-1",
              "La ", tags$strong("nube de palabras"), " representa visualmente",
              " la frecuencia de los términos: cuanto más grande aparece una palabra,",
              " más frecuente es en el corpus."
            ),
            tags$p(class = "small text-muted mb-0",
              "Aunque tiene limitaciones académicas (no muestra contexto ni relaciones),",
              " es útil para una ", tags$strong("primera exploración visual"),
              " del vocabulario predominante.",
              " Usá el botón ", tags$strong("Regenerar"), " para cambiar la disposición."
            )
          ),
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
                plotOutput(ns("plot_nube"), height = "600px")
              )
            )
          )
        )
      ), # /PESTAÑA 3

      # ══════════════════════════════════════════════════
      # PESTAÑA 4: N-gramas
      nav_panel(
        title = tagList(bs_icon("text-paragraph", class = "me-1"), "N-gramas"),
        card_body(
          div(
            class = "alert mb-3",
            style = paste0("background:", colores$fondo,
                           "; border-left: 4px solid ", colores$primario, ";"),
            tags$b(
              bs_icon("info-circle", class = "me-1",
                      style = paste0("color:", colores$primario)),
              "¿Qué son los n-gramas?"
            ),
            tags$p(class = "small text-muted mb-1 mt-1",
              "Secuencias ", tags$strong("contiguas"), " de N palabras en el texto.",
              " Un ", tags$strong("bigrama"), " es un par ('derechos humanos');",
              " un ", tags$strong("trigrama"), " es un trío ('sociedad civil organizada')."
            ),
            tags$p(class = "small text-muted mb-0",
              "Útiles para detectar ", tags$strong("frases hechas"),
              " y ", tags$strong("términos compuestos"), " frecuentes en el discurso.",
              " A diferencia de las coocurrencias, los n-gramas son secuencias",
              " estrictamente contiguas."
            )
          ),
          layout_columns(
            col_widths = c(4, 8),
            card(
              card_header(bs_icon("sliders", class = "me-1"), "Controles"),
              card_body(
                radioButtons(
                  ns("ngrama_n"),
                  "Tipo:",
                  choices = c(
                    "Bigramas (2 palabras)"  = "2",
                    "Trigramas (3 palabras)" = "3"
                  ),
                  selected = "2"
                ),
                sliderInput(ns("ngrama_min_freq"), "Frecuencia mínima:",
                            min = 1, max = 20, value = 2, step = 1),
                sliderInput(ns("ngrama_top_n"), "N-gramas a mostrar:",
                            min = 5, max = 30, value = 15, step = 5),
                tags$hr(),
                actionButton(ns("calcular_ngramas"),
                             tagList(bs_icon("play-fill", class = "me-1"),
                                     "Calcular"),
                             class = "btn btn-primary w-100")
              )
            ),
            div(
              card(
                class = "mb-3",
                card_header(bs_icon("table", class = "me-1"), "Tabla de n-gramas"),
                card_body(DTOutput(ns("tabla_ngramas"), height = "250px"))
              ),
              card(
                card_header(bs_icon("bar-chart-fill", class = "me-1"),
                            "N-gramas más frecuentes"),
                card_body(uiOutput(ns("plot_ngramas_ui")))
              )
            ),
            uiOutput(ns("exp_ngramas_ui"))
          )
        )
      ), # /PESTAÑA 4

      # ══════════════════════════════════════════════════
      # PESTAÑA 5: Coocurrencias
      nav_panel(
        title = tagList(bs_icon("diagram-3", class = "me-1"), "Coocurrencias"),
        card_body(
          div(
            class = "alert mb-3",
            style = paste0("background:", colores$fondo,
                           "; border-left: 4px solid ", colores$secundario, ";"),
            tags$b(
              bs_icon("info-circle", class = "me-1",
                      style = paste0("color:", colores$secundario)),
              "¿Qué son las coocurrencias?"
            ),
            tags$p(class = "small text-muted mb-1 mt-1",
              "Dos términos ", tags$strong("coocurren"), " cuando aparecen cerca",
              " uno del otro dentro de una ventana de N tokens. Una ventana de 5",
              " significa que se cuentan los pares de palabras que aparecen a 5",
              " posiciones de distancia o menos, hacia adelante y hacia atrás."
            ),
            tags$p(class = "small text-muted mb-0",
              "Útil para identificar ", tags$strong("conceptos compuestos"),
              " (ej. 'salud pública'), ",
              tags$strong("asociaciones temáticas"), " frecuentes y ",
              tags$strong("redes semánticas"), " en el discurso.",
              " A mayor frecuencia de coocurrencia, más fuerte es la asociación."
            )
          ),
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
                tags$hr(),
                actionButton(ns("calcular_cooc"),
                             tagList(bs_icon("play-fill", class = "me-1"),
                                     "Calcular"),
                             class = "btn btn-primary btn-sm w-100 mt-2")
              )
            ),
            div(
              card(
                class = "mb-3",
                card_header(bs_icon("table", class = "me-1"),
                            "Tabla de coocurrencias"),
                card_body(DTOutput(ns("tabla_cooc"), height = "250px"))
              ),
              card(
                card_header(bs_icon("bar-chart-steps", class = "me-1"),
                            "Pares de términos más coocurrentes"),
                card_body(plotOutput(ns("plot_cooc"), height = "520px"))
              )
            ),
            uiOutput(ns("exp_cooc_ui"))
          )
        )
      ), # /PESTAÑA 5

      # ══════════════════════════════════════════════════
      # PESTAÑA 6: Concordancias (KWIC)
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
            tags$p(class = "small text-muted mb-1 mt-1",
              tags$strong("Key Word In Context (KWIC)"), " muestra el contexto",
              " de una palabra en el texto: las palabras que aparecen antes y después."
            ),
            tags$p(class = "small text-muted mb-0",
              "Técnica central del análisis cualitativo de discurso.",
              " Permite ver cómo se ", tags$strong("usa realmente"),
              " un término en su contexto, más allá de su frecuencia."
            )
          ),
          layout_columns(
            col_widths = c(4, 8),
            card(
              card_header(bs_icon("sliders", class = "me-1"), "Controles"),
              card_body(
                textInput(
                  ns("kwic_patron"),
                  "Palabra o frase a buscar:",
                  placeholder = "ej. participación"
                ),
                sliderInput(ns("kwic_ventana"), "Ventana de contexto (tokens):",
                            min = 2, max = 15, value = 5, step = 1),
                tags$hr(),
                actionButton(ns("buscar_kwic"),
                             tagList(bs_icon("search", class = "me-1"),
                                     "Buscar"),
                             class = "btn btn-primary w-100")
              )
            ),
            div(
              card(
                card_header(bs_icon("list-ul", class = "me-1"),
                            "Concordancias encontradas"),
                card_body(
                  uiOutput(ns("kwic_info_ui")),
                  DTOutput(ns("tabla_kwic"))
                )
              ),
              uiOutput(ns("exp_kwic_ui"))
            )
          )
        )
      ), # /PESTAÑA 6

      # ══════════════════════════════════════════════════
      # PESTAÑA 7: Tópicos (LDA)
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
              tags$strong("Latent Dirichlet Allocation (LDA)"), " es un modelo",
              " probabilístico que descubre ", tags$strong("temas latentes"),
              " en un conjunto de textos. Asume que cada documento es una",
              " mezcla de tópicos, y cada tópico es una distribución de palabras."
            ),
            tags$p(class = "small text-muted mb-0",
              tags$strong("β (beta):"), " probabilidad de que una palabra pertenezca",
              " a un tópico — las palabras con β más alto definen ese tópico.",
              tags$br(),
              tags$strong("γ (gamma):"), " probabilidad de que un documento/segmento",
              " pertenezca a cada tópico.",
              tags$br(),
              tags$strong("K:"), " número de tópicos a descubrir (lo define el investigador).",
              " Se recomienda explorar distintos valores de K y elegir el que",
              " produzca tópicos más interpretables."
            )
          ),
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
                  class = "p-2 mb-3",
                  style = paste0("background:", colores$fondo,
                                 "; border-radius:6px; font-size:12px;"),
                  bs_icon("info-circle", class = "me-1",
                          style = paste0("color:", colores$primario)),
                  "LDA requiere ≥ K documentos/segmentos. Para texto continuo,",
                  " se divide automáticamente en segmentos de tamaño dinámico."
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
                card_header(bs_icon("bar-chart-fill", class = "me-1"),
                            "Términos por tópico (β)"),
                card_body(uiOutput(ns("plot_lda_beta_ui")))
              ),
              card(
                card_header(bs_icon("table", class = "me-1"),
                            "Tópico dominante por segmento (γ)"),
                card_body(
                  p(class = "small text-muted mb-2",
                    "Tópico con mayor probabilidad (γ) por documento/segmento."),
                  DTOutput(ns("tabla_lda_gamma"))
                )
              ),
              uiOutput(ns("exp_lda_ui"))
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
            "Código R reproducible con quanteda y topicmodels."),
          div(class = "d-flex gap-2 mb-3",
            downloadButton(ns("descarga_codigo"),
                           "Descargar .R",
                           class = "btn-sm btn-outline-primary")
          ),
          verbatimTextOutput(ns("codigo_r")) |>
            tagAppendAttributes(class = "codigo-bloque")
        )
      ) # /PESTAÑA 8

    ) # /navset_card_tab
  ) # /tagList
} # /mod_texto_libre_ui


# ── Server ────────────────────────────────────────────────
mod_texto_libre_server <- function(id) {
  moduleServer(id, function(input, output, session) {
    ns <- session$ns

    # ── Texto fuente ─────────────────────────────────────
    texto_raw <- reactive({
      # Determinar fuente activa: el último grupo con selección
      fuente <- dplyr::coalesce(
        if (!is.null(input$fuente_archivo) && length(input$fuente_archivo) > 0 &&
            input$fuente_archivo == "archivo") "archivo" else NULL,
        if (!is.null(input$fuente_pegar) && length(input$fuente_pegar) > 0 &&
            input$fuente_pegar == "pegar") "pegar" else NULL,
        input$fuente_ejemplo
      )
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
        } else if (ext == "xlsx") {
          df <- readxl::read_excel(input$archivo_texto$datapath)
          req(input$col_texto %in% names(df))
          paste(df[[input$col_texto]], collapse = " ")
        } else if (ext == "rtf") {
          paste(striprtf::read_rtf(input$archivo_texto$datapath), collapse = " ")
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
    # Deseleccionar otros grupos cuando se selecciona uno
    observeEvent(input$fuente_ejemplo, {
      req(input$fuente_ejemplo)
      updateRadioButtons(session, "fuente_pegar",  selected = character(0))
      updateRadioButtons(session, "fuente_archivo", selected = character(0))
    })
    observeEvent(input$fuente_pegar, {
      req(input$fuente_pegar)
      updateRadioButtons(session, "fuente_ejemplo", selected = character(0))
      updateRadioButtons(session, "fuente_archivo",  selected = character(0))
    })
    observeEvent(input$fuente_archivo, {
      req(input$fuente_archivo)
      updateRadioButtons(session, "fuente_ejemplo", selected = character(0))
      updateRadioButtons(session, "fuente_pegar",   selected = character(0))
    })

    output$fuente_activa_pegar <- reactive({
      !is.null(input$fuente_pegar) && length(input$fuente_pegar) > 0 &&
        input$fuente_pegar == "pegar"
    })
    outputOptions(output, "fuente_activa_pegar", suspendWhenHidden = FALSE)

    output$fuente_activa_archivo <- reactive({
      !is.null(input$fuente_archivo) && length(input$fuente_archivo) > 0 &&
        input$fuente_archivo == "archivo"
    })
    outputOptions(output, "fuente_activa_archivo", suspendWhenHidden = FALSE)

    output$archivo_es_csv <- reactive({
      req(input$archivo_texto)
      tolower(tools::file_ext(input$archivo_texto$name)) %in% c("csv", "xlsx")
    })
    outputOptions(output, "archivo_es_csv", suspendWhenHidden = FALSE)

    observeEvent(input$archivo_texto, {
      ext <- tolower(tools::file_ext(input$archivo_texto$name))
      if (ext == "csv") {
        df   <- readr::read_csv(input$archivo_texto$datapath,
                                show_col_types = FALSE, n_max = 1)
        cols <- names(df)
        updateSelectInput(session, "col_texto", choices = cols, selected = cols[1])
      } else if (ext == "xlsx") {
        df   <- readxl::read_excel(input$archivo_texto$datapath, n_max = 1)
        cols <- names(df)
        updateSelectInput(session, "col_texto", choices = cols, selected = cols[1])
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
      req(dfm_proc(), corpus_proc())
      dfm    <- dfm_proc()
      n_tok  <- sum(quanteda::ntoken(dfm))
      n_feat <- nfeat(dfm)
      ttr    <- round(n_feat / max(n_tok, 1), 3)

      # Hapax legomena: términos que aparecen exactamente una vez
      freq_vec <- quanteda::colSums(dfm)
      n_hapax  <- sum(freq_vec == 1)
      pct_hapax <- round(n_hapax / max(n_feat, 1) * 100, 1)

      # Longitud media de documentos (en tokens)
      tok_por_doc  <- quanteda::ntoken(dfm)
      long_media   <- round(mean(tok_por_doc), 1)

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
        layout_columns(
          col_widths = c(6, 6),
          class = "mt-2",
          vbox_card("1-circle", "Hapax legomena",
                    paste0(format(n_hapax, big.mark = ","),
                           " (", pct_hapax, "%)"),
                    colores$primario),
          vbox_card("rulers", "Long. media doc.",
                    paste0(long_media, " tokens"),
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
          " valores bajos señalan vocabulario repetitivo.",
          tags$br(),
          tags$b("Hapax legomena:"), " términos que aparecen exactamente una vez.",
          " Un porcentaje alto indica vocabulario muy diverso o corpus pequeño.",
          tags$br(),
          tags$b("Long. media doc.:"), " promedio de tokens por documento tras preprocesamiento."
        )
      )
    })

    output$vista_posterior_ui <- renderUI({
      req(corpus_proc(), texto_raw())

      # Tokens resultantes
      toks      <- corpus_proc()
      tok_vec   <- unlist(quanteda::as.list(toks))
      n_post    <- length(tok_vec)

      # Palabras originales (aprox.) contando por espacios
      n_pre     <- length(unlist(strsplit(trimws(texto_raw()), "\\s+")))
      eliminados <- max(0L, n_pre - n_post)
      pct        <- round(eliminados / max(n_pre, 1) * 100)

      # Muestra los primeros 120 tokens como chips
      muestra   <- head(tok_vec, 120)
      chips     <- lapply(muestra, function(w) {
        tags$span(
          w,
          class = "badge me-1 mb-1",
          style = paste0("background:", colores$fondo,
                         "; color:", colores$texto,
                         "; border:1px solid #ddd; font-weight:400;",
                         " font-size:12px;")
        )
      })
      if (length(tok_vec) > 120) {
        chips <- c(chips, list(tags$span(
          paste0("… y ", length(tok_vec) - 120, " más"),
          class = "text-muted small"
        )))
      }

      tagList(
        br(),
        div(
          class = "p-3",
          style = paste0("background:", colores$fondo,
                         "; border-radius:8px; border:1px solid #e0e0e0;"),
          div(class = "d-flex justify-content-between align-items-center mb-2",
            tags$b(
              bs_icon("eye", class = "me-1",
                      style = paste0("color:", colores$primario)),
              "Tokens resultantes del preprocesamiento"
            ),
            tags$span(
              class = "badge",
              style = paste0("background:", colores$acento,
                             "; color:white; font-size:11px;"),
              paste0(n_pre, " → ", n_post, " tokens (−", pct, "%)")
            )
          ),
          div(style = "line-height:2;", chips)
        )
      )
    })

    # ── Frecuencias ───────────────────────────────────────
    freq_df <- reactive({
      req(dfm_proc())
      top_features_df(dfm_proc(), n = input$n_top_terms)
    })

    # renderUI dinámico: altura crece con número de términos (~32px por barra)
    output$plot_freq_ui <- renderUI({
      req(freq_df())
      n      <- nrow(freq_df())
      altura <- max(300L, n * 38L)
      plotOutput(ns("plot_freq"), height = paste0(altura, "px"))
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

      p <- ggplot2::ggplot(df, ggplot2::aes(x = frequency, y = feature))

      if (isTRUE(input$freq_tipo == "lollipop")) {
        p <- p +
          ggplot2::geom_col(fill = colores$primario, alpha = 0.9, width = 0.05) +
          ggplot2::geom_point(color = colores$acento, size = 4) +
          ggplot2::geom_text(
            ggplot2::aes(label = frequency),
            hjust = -0.4, size = 3.5, color = colores$texto
          )
      } else {
        p <- p +
          ggplot2::geom_col(fill = colores$primario, alpha = 0.85, width = 0.6) +
          ggplot2::geom_text(
            ggplot2::aes(label = frequency),
            hjust = -0.2, size = 3.5, color = colores$texto
          )
      }

      p +
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

    output$tabla_freq <- renderDT({
      req(freq_df())
      datatable(
        freq_df(),
        options  = list(pageLength = 15, dom = "tp",
                         autoWidth = FALSE, scrollX = FALSE),
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
        "multi"  = rep(colores$tableau, 10)
      )

      suppressWarnings(
        quanteda.textplots::textplot_wordcloud(
          dfm,
          min_count    = input$nube_min_freq,
          max_words    = input$nube_max_words,
          color        = paleta,
          random_order = FALSE,
          random_color = (input$nube_color == "multi")
        )
      )
    })

    # ── N-gramas ──────────────────────────────────────────
    ngramas_df <- reactiveVal(NULL)

    observeEvent(input$calcular_ngramas, {
      req(corpus_proc())
      withProgress(message = "Calculando n-gramas…", value = 0.5, {
        tryCatch({
          n   <- as.integer(input$ngrama_n)
          tok <- quanteda::tokens_ngrams(corpus_proc(), n = n)
          dfm_ng <- quanteda::dfm(tok)
          dfm_ng <- quanteda::dfm_trim(dfm_ng, min_termfreq = input$ngrama_min_freq)

          freq_ng <- quanteda.textstats::textstat_frequency(dfm_ng,
                                                   n = input$ngrama_top_n)
          if (nrow(freq_ng) == 0) {
            showNotification(
              "No hay n-gramas con esa frecuencia mínima. Reducí el umbral.",
              type = "warning", duration = 4
            )
            return()
          }
          # Reemplazar _ por espacio para legibilidad
          freq_ng$feature <- gsub("_", " ", freq_ng$feature)
          ngramas_df(freq_ng)
          incProgress(0.5)
        }, error = function(e) {
          showNotification(paste("Error:", conditionMessage(e)),
                           type = "error", duration = 5)
        })
      })
    })

    output$plot_ngramas_ui <- renderUI({
      req(ngramas_df())
      n <- nrow(ngramas_df())
      altura <- max(300L, n * 38L)
      plotOutput(ns("plot_ngramas"), height = paste0(altura, "px"))
    })

    output$plot_ngramas <- renderPlot({
      req(ngramas_df())
      df <- ngramas_df()
      df$feature <- factor(df$feature, levels = rev(df$feature))

      ggplot2::ggplot(df, ggplot2::aes(x = frequency, y = feature)) +
        ggplot2::geom_col(fill = colores$primario, alpha = 0.85, width = 0.6) +
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

    output$tabla_ngramas <- renderDT({
      req(ngramas_df())
      df <- ngramas_df()[, c("feature", "frequency", "rank")]
      datatable(
        df,
        options  = list(pageLength = 10, dom = "tp",
                        autoWidth = FALSE, scrollX = FALSE),
        rownames = FALSE,
        class    = "table-sm table-striped",
        colnames = c("N-grama", "Frecuencia", "Rango")
      )
    })

    # ── N-gramas ──────────────────────────────────────────
    ngramas_df <- reactiveVal(NULL)

    observeEvent(input$calcular_ngramas, {
      req(corpus_proc())
      withProgress(message = "Calculando n-gramas…", value = 0.5, {
        tryCatch({
          n      <- as.integer(input$ngrama_n)
          tok    <- quanteda::tokens_ngrams(corpus_proc(), n = n)
          dfm_ng <- quanteda::dfm(tok)
          dfm_ng <- quanteda::dfm_trim(dfm_ng,
                                       min_termfreq = input$ngrama_min_freq)
          freq_ng <- quanteda.textstats::textstat_frequency(dfm_ng,
                                                   n = input$ngrama_top_n)
          if (nrow(freq_ng) == 0) {
            showNotification(
              "No hay n-gramas con esa frecuencia mínima. Reducí el umbral.",
              type = "warning", duration = 4
            )
            return()
          }
          freq_ng$feature <- gsub("_", " ", freq_ng$feature)
          ngramas_df(freq_ng)
          incProgress(0.5)
        }, error = function(e) {
          showNotification(paste("Error:", conditionMessage(e)),
                           type = "error", duration = 5)
        })
      })
    })

    output$plot_ngramas_ui <- renderUI({
      req(ngramas_df())
      n      <- nrow(ngramas_df())
      altura <- max(300L, n * 38L)
      plotOutput(ns("plot_ngramas"), height = paste0(altura, "px"))
    })

    output$plot_ngramas <- renderPlot({
      req(ngramas_df())
      df         <- ngramas_df()
      df$feature <- factor(df$feature, levels = rev(df$feature))

      ggplot2::ggplot(df, ggplot2::aes(x = frequency, y = feature)) +
        ggplot2::geom_col(fill = colores$primario, alpha = 0.85, width = 0.6) +
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

    output$tabla_ngramas <- renderDT({
      req(ngramas_df())
      df <- ngramas_df()[, c("feature", "frequency", "rank")]
      datatable(
        df,
        options  = list(pageLength = 10, dom = "tp",
                        autoWidth = FALSE, scrollX = FALSE),
        rownames = FALSE,
        class    = "table-sm table-striped",
        colnames = c("N-grama", "Frecuencia", "Rango")
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

    # ── Concordancias (KWIC) ──────────────────────────────
    observeEvent(input$buscar_kwic, {
      req(corpus_proc(), nchar(trimws(input$kwic_patron)) > 0)
      withProgress(message = "Buscando concordancias…", value = 0.5, {
        tryCatch({
          # Reconstruir corpus de texto desde tokens
          corp <- quanteda::corpus(
            sapply(quanteda::as.list(corpus_proc()), paste, collapse = " ")
          )
          kwic_res <- quanteda::kwic(
            quanteda::tokens(corp),
            pattern = input$kwic_patron,
            window  = input$kwic_ventana,
            valuetype = "glob"
          )
          kwic_rv(as.data.frame(kwic_res))
          incProgress(0.5)
        }, error = function(e) {
          showNotification(paste("Error:", conditionMessage(e)),
                           type = "error", duration = 5)
        })
      })
    })

    kwic_rv <- reactiveVal(NULL)

    output$kwic_info_ui <- renderUI({
      df <- kwic_rv()
      if (is.null(df)) return(NULL)
      n <- nrow(df)
      div(
        class = "alert alert-info small py-2 px-3 mb-3",
        bs_icon("info-circle", class = "me-1"),
        strong(n), " ocurrencia(s) encontrada(s) para ",
        strong(paste0('"', input$kwic_patron, '"')), "."
      )
    })

    output$tabla_kwic <- renderDT({
      req(kwic_rv())
      df <- kwic_rv()

      # Seleccionar y renombrar columnas relevantes
      df_show <- data.frame(
        Antes   = df$pre,
        Palabra = df$keyword,
        Después = df$post,
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
        DT::formatStyle(
          "Palabra",
          fontWeight = "bold",
          color      = colores$acento
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

          # Paso 1: si hay menos documentos que K, segmentar el corpus
          if (ndoc(dfm) < max(input$lda_k, 2)) {
            n_tokens <- sum(quanteda::ntoken(corpus_proc()))
            # Tamaño de chunk: al menos 10 tokens; apunta a lda_k * 3 segmentos
            chunk_sz <- max(10L, as.integer(n_tokens / (input$lda_k * 3)))
            toks_seg <- quanteda::tokens_chunk(corpus_proc(), size = chunk_sz)
            dfm      <- construir_dfm(toks_seg, stem = input$aplicar_stemming)
          }

          # Paso 2: recortar features con freq 0 y eliminar filas vacías
          dfm  <- quanteda::dfm_trim(dfm, min_termfreq = 1)
          # rowSums sobre la matriz completa evita el error de array de 1 dimensión
          sumas_filas <- Matrix::rowSums(quanteda::as.dfm(dfm))
          dfm  <- dfm[sumas_filas > 0, ]

          # Paso 3: verificar que siga habiendo suficientes filas y columnas
          validate(
            need(ndoc(dfm) >= max(input$lda_k, 2) && nfeat(dfm) >= 2,
                 paste0(
                   "Se necesitan al menos K = ", input$lda_k,
                   " segmentos con ≥ 2 términos. Reducí K o añadí más texto."
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

    # renderUI dinámico: altura del gráfico β crece con K (filas de facetas)
    output$plot_lda_beta_ui <- renderUI({
      req(modelo_lda())
      k        <- input$lda_k
      n_rows   <- ceiling(k / 3)           # min(k, 3) columnas en facet_wrap
      altura   <- max(400L, n_rows * 300L) # ~300px por fila de facetas
      plotOutput(ns("plot_lda_beta"), height = paste0(altura, "px"))
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
        ggplot2::geom_col(show.legend = FALSE, alpha = 0.85, width = 0.7) +
        tidytext::scale_y_reordered() +
        ggplot2::facet_wrap(~topic, scales = "free_y",
                            ncol = min(input$lda_k, 3)) +
        scale_fill_tableau_cb() +
        ggplot2::labs(x = "Probabilidad del término (β)", y = NULL) +
        ggplot2::theme_minimal(base_size = 12) +
        ggplot2::theme(
          panel.grid.minor  = ggplot2::element_blank(),
          strip.text        = ggplot2::element_text(face = "bold", size = 12),
          axis.text.y       = ggplot2::element_text(size = 10),
          plot.margin       = ggplot2::margin(8, 12, 8, 8)
        )
    }, res = 96)

    output$tabla_lda_gamma <- renderDT({
      req(modelo_lda())
      gamma_df <- tidytext::tidy(modelo_lda(), matrix = "gamma") |>
        dplyr::group_by(document) |>
        dplyr::slice_max(gamma, n = 1, with_ties = FALSE) |>
        dplyr::ungroup() |>
        dplyr::transmute(
          Segmento         = document,
          `Tópico dominante` = paste0("Tópico ", topic),
          `γ máximo`       = round(gamma, 3)
        ) |>
        dplyr::arrange(Segmento)

      datatable(
        gamma_df,
        options  = list(pageLength = 10, dom = "tp",
                         autoWidth = FALSE, scrollX = FALSE),
        rownames = FALSE,
        class    = "table-sm table-condensed"
      ) |>
        DT::formatRound(columns = "γ máximo", digits = 3)
    })

    # ── Explicaciones automáticas ─────────────────────────

    output$exp_freq_ui <- renderUI({
      req(freq_df())
      df  <- freq_df()
      top <- df[which.max(df$frequency), ]
      div(
        class = "mt-3 p-3 small",
        style = paste0("background:", colores$fondo,
                       "; border-left:4px solid ", colores$acento,
                       "; border-radius:4px;"),
        bs_icon("lightbulb", class = "me-1",
                style = paste0("color:", colores$acento)),
        tags$b("Interpretación: "),
        "La palabra más frecuente fue ",
        tags$strong(top$feature),
        paste0(" con ", top$frequency, " ocurrencias. "),
        paste0("El vocabulario tiene ", nrow(df), " términos únicos.")
      )
    })

    output$exp_ngramas_ui <- renderUI({
      req(ngramas_df())
      df  <- ngramas_df()
      top <- df[1, ]
      tipo <- if (input$ngrama_n == "2") "bigrama" else "trigrama"
      div(
        class = "mt-3 p-3 small",
        style = paste0("background:", colores$fondo,
                       "; border-left:4px solid ", colores$acento,
                       "; border-radius:4px;"),
        bs_icon("lightbulb", class = "me-1",
                style = paste0("color:", colores$acento)),
        tags$b("Interpretación: "),
        paste0("El ", tipo, " más frecuente fue '", top$feature,
               "' con ", top$frequency, " ocurrencias.")
      )
    })

    output$exp_cooc_ui <- renderUI({
      req(cooc_df())
      df  <- cooc_df()
      top <- df[1, ]
      div(
        class = "mt-3 p-3 small",
        style = paste0("background:", colores$fondo,
                       "; border-left:4px solid ", colores$acento,
                       "; border-radius:4px;"),
        bs_icon("lightbulb", class = "me-1",
                style = paste0("color:", colores$acento)),
        tags$b("Interpretación: "),
        paste0("El par de términos más frecuente fue '",
               top$term1, " — ", top$term2,
               "' con ", top$count, " coocurrencias.")
      )
    })

    output$exp_kwic_ui <- renderUI({
      req(kwic_rv())
      n <- nrow(kwic_rv())
      div(
        class = "mt-3 p-3 small",
        style = paste0("background:", colores$fondo,
                       "; border-left:4px solid ", colores$acento,
                       "; border-radius:4px;"),
        bs_icon("lightbulb", class = "me-1",
                style = paste0("color:", colores$acento)),
        tags$b("Interpretación: "),
        paste0("Se encontraron ", n, " ocurrencia(s) del término '",
               input$kwic_patron, "' en el corpus.")
      )
    })

    output$exp_lda_ui <- renderUI({
      req(modelo_lda())
      gamma_df <- tidytext::tidy(modelo_lda(), matrix = "gamma") |>
        dplyr::group_by(document) |>
        dplyr::slice_max(gamma, n = 1, with_ties = FALSE) |>
        dplyr::ungroup()
      topic_dom <- gamma_df |>
        dplyr::count(topic) |>
        dplyr::slice_max(n, n = 1, with_ties = FALSE)
      div(
        class = "mt-3 p-3 small",
        style = paste0("background:", colores$fondo,
                       "; border-left:4px solid ", colores$acento,
                       "; border-radius:4px;"),
        bs_icon("lightbulb", class = "me-1",
                style = paste0("color:", colores$acento)),
        tags$b("Interpretación: "),
        paste0("Se identificaron ", input$lda_k, " tópicos. ",
               "El tópico dominante en la mayoría de segmentos fue ",
               "Tópico ", topic_dom$topic, " (",
               topic_dom$n, " segmentos).")
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
