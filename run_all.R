# ------------------------------------------------------------------------------
# run_all.R -- executa o pacote inteiro, do bruto ao resultado
#
#   Rscript run_all.R
# ------------------------------------------------------------------------------

if (!requireNamespace("here", quietly = TRUE)) {
  install.packages("here", repos = "https://cloud.r-project.org")
}

etapas <- c(
  "01-build-data.R",
  # A classificacao de unidades em areas e insumo das analises, mas e derivada
  # do painel: so pode ser gerada depois que o 01 o constroi.
  "areas",
  "02-descritivas.R",
  "03-modelos.R",
  "04-heterogeneidade.R",
  "05-cursos.R",
  "06-replica-fflch.R"
)

inicio_total <- Sys.time()

for (etapa in etapas) {
  caminho <- if (etapa == "areas") here::here("tools", "make-areas.R")
             else here::here("analysis", etapa)

  message("\n", strrep("=", 78))
  message("== ", basename(caminho))
  message(strrep("=", 78))

  inicio <- Sys.time()
  source(caminho, local = new.env(), echo = FALSE)
  message(sprintf("-- concluída em %.1f min",
                  as.numeric(difftime(Sys.time(), inicio, units = "mins"))))
}

message("\n", strrep("=", 78))
message(sprintf("== Pacote concluído em %.1f min",
                as.numeric(difftime(Sys.time(), inicio_total, units = "mins"))))
message("== Resultados em outputs/tables e outputs/figures")
message(strrep("=", 78))
