# ------------------------------------------------------------------------------
# make-checksums.R -- registra o MD5 de cada arquivo de dados
#
#   Rscript tools/make-checksums.R           grava data-raw/CHECKSUMS.txt
#   Rscript tools/make-checksums.R --check   confere os arquivos contra o registro
#
# Serve para detectar corrupcao ou troca silenciosa de um arquivo de origem --
# o tipo de coisa que faria os resultados mudarem sem que nada no codigo tenha
# mudado.
# ------------------------------------------------------------------------------

source(here::here("R", "setup.R"))

ARQ_CHECKSUMS <- here::here("data-raw", "CHECKSUMS.txt")

arquivos <- c(
  list.files(DIR_RAW, pattern = "[.]xlsx$", full.names = TRUE, recursive = TRUE),
  ARQ_PAINEL
)
arquivos <- sort(arquivos[file.exists(arquivos)])
relativos <- sub(paste0("^", here::here(), "/?"), "", arquivos, fixed = FALSE)

md5 <- tools::md5sum(arquivos)
names(md5) <- relativos

conferir <- "--check" %in% commandArgs(TRUE)

if (conferir) {
  if (!file.exists(ARQ_CHECKSUMS)) stop("nao existe ", ARQ_CHECKSUMS)

  registro <- read.table(ARQ_CHECKSUMS, sep = " ", comment.char = "#",
                         col.names = c("md5", "arquivo"), strip.white = TRUE)
  esperado <- stats::setNames(registro$md5, registro$arquivo)

  faltando  <- setdiff(names(esperado), names(md5))
  divergindo <- names(md5)[names(md5) %in% names(esperado) &
                             md5 != esperado[names(md5)]]

  if (length(faltando))  message("AUSENTES:  ", paste(faltando, collapse = ", "))
  if (length(divergindo)) message("DIVERGEM:  ", paste(divergindo, collapse = ", "))

  if (length(faltando) || length(divergindo)) {
    stop("os arquivos de dados nao conferem com data-raw/CHECKSUMS.txt")
  }
  message("OK: ", length(md5), " arquivos conferem com o registro.")
} else {
  writeLines(
    c("# MD5 dos arquivos de dados deste pacote de replicacao.",
      "# Confira com: Rscript tools/make-checksums.R --check",
      sprintf("%s %s", md5, names(md5))),
    ARQ_CHECKSUMS
  )
  message("CHECKSUMS.txt gravado: ", length(md5), " arquivos.")
}
