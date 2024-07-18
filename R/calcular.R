#' Obter abundancia relativa e categorias
#'
#' Esta função obtém a abundância relativa as categoriza.
#'
#' @param a data.frame contendo na coluna 1 o nome dos táxons e na coluna 2 a abundância absoluta.
#' @export
calcular <- function(a) {
  # Calculando valores relativos
  relative.abundance <- (a[[2]] / sum(a[[2]])) * 100

  # Categorizando
  category <- c()
  for (i in 1:length(relative.abundance)) {
    if (relative.abundance[i] < 1.5) {cat <- "Raro"}
    else if (relative.abundance[i] < 5.0) {cat <- "ocasional"}
    else if (relative.abundance[i] >= 5.0) {cat <- "Abundante"}
    category[i] <- cat
  }

  # Criando dataframe com resultado
  df_result <- data.frame(a[[1]],a[[2]],relative.abundance,category)
  colnames(df_result) <- c("Taxon","Abd_Absoluta","Abd_Relativa","Categoria")

  return(df_result)
}
