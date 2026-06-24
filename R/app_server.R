#' Application Server
#'
#' @param input,output,session Internal parameters for Shiny.
#' @noRd
app_server <- function(input, output, session) {
  mod_texto_libre_server("texto_libre")
  mod_enc_abiertas_server("enc_abiertas")
  mod_enc_cerradas_server("enc_cerradas")
  mod_acerca_de_server("acerca_de")
}
