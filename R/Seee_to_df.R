#' Seee_to_df
#'
#' @R/
#'
#' @dataframe
#' @export
#'
#' @Seee_to_df("Exports/")
#' fonctionqui lit l ensemble des fichiers d'entree SEEE et qui le convertit en dataframe
#'

Seee_to_df <- function(chemin){
  Liste_fichiers <-  list.files(chemin,pattern="*.txt",full.names = TRUE)
 # print(Liste_fichiers)
  n <- length(Liste_fichiers)
  data_entree <- NULL
  for (i in 1:n){
    data_temp <- read.table(Liste_fichiers[i], header = TRUE, sep = "\t",
                            stringsAsFactors = FALSE, quote = "\"",
                            colClasses = c(CODE_OPERATION = "character",
                                           CODE_STATION   = "character",
                                           CODE_TAXON     = "character")) %>%
      filter(RESULTAT > 0)
    data_entree <- rbind(data_entree,data_temp)

  }
  return(data_entree)
}
