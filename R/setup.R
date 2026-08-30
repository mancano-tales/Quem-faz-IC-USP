# ------------------------------------------------------------------------------
# setup.R -- pacotes, caminhos e opcoes comuns a todos os scripts
#
# Pacote de replicacao: "Quem faz Iniciacao Cientifica na USP?"
# Mancano & Alcantara
# ------------------------------------------------------------------------------

.pkgs <- c("here", "dplyr", "tidyr", "readxl", "writexl",
           "ggplot2", "scales", "arrow", "broom")

.faltando <- .pkgs[!vapply(.pkgs, requireNamespace, logical(1), quietly = TRUE)]
if (length(.faltando)) {
  message("Instalando pacotes ausentes: ", paste(.faltando, collapse = ", "))
  install.packages(.faltando, repos = "https://cloud.r-project.org")
}
invisible(lapply(.pkgs, library, character.only = TRUE))

# Caminhos ---------------------------------------------------------------------
DIR_RAW     <- here::here("data-raw")
DIR_DATA    <- here::here("data")
DIR_TABLES  <- here::here("outputs", "tables")
DIR_FIGURES <- here::here("outputs", "figures")

for (d in c(DIR_DATA, DIR_TABLES, DIR_FIGURES)) {
  if (!dir.exists(d)) dir.create(d, recursive = TRUE)
}

ARQ_JUPITER <- file.path(DIR_RAW, "sic-usp-243654-ingressantes.xlsx")
ARQ_ATENA   <- file.path(DIR_RAW, "sic-usp-243681-perfil-ic.xlsx")
ARQ_PAINEL  <- file.path(DIR_DATA, "ic_usp.parquet")
ARQ_AREAS   <- file.path(DIR_RAW, "unidades-areas.csv")

# Recorte analitico ------------------------------------------------------------
# A janela e a mesma do estudo da FFLCH, para que os resultados sejam
# diretamente comparaveis com os do artigo anterior.
COORTES       <- 2010:2022
UNIDADE_FFLCH <- "Faculdade de Filosofia, Letras e Ciências Humanas"

# Unidades com menos ingressantes do que isto ficam fora das comparacoes por
# unidade: proporcoes sobre poucos casos oscilam demais para serem lidas.
MIN_INGRESSANTES <- 500

# Opcoes -----------------------------------------------------------------------
options(stringsAsFactors = FALSE, scipen = 999)
ggplot2::theme_set(ggplot2::theme_bw(base_size = 12))

TEMA <- ggplot2::theme(
  panel.grid.minor = ggplot2::element_blank(),
  legend.position  = "bottom"
)

set.seed(1234)

# Carrega a classificacao de unidades em areas do conhecimento.
carregar_areas <- function() {
  if (!file.exists(ARQ_AREAS)) {
    stop("rode antes: Rscript tools/make-areas.R")
  }
  utils::read.csv(ARQ_AREAS, encoding = "UTF-8")[, c("unidade", "area")]
}

# Campus de uma unidade, para desambiguar cursos homonimos.
#
# O mesmo curso existe em unidades diferentes -- Odontologia em tres faculdades,
# Medicina Veterinaria em duas. O que as separa e o campus, e nao a sigla: usar
# abbreviate() sobre o nome da unidade produz strings ilegiveis.
campus_unidade <- function(unidade) {
  dplyr::case_when(
    grepl("Ribeirão Preto", unidade)                    ~ "Ribeirão Preto",
    grepl("de Bauru", unidade)                          ~ "Bauru",
    grepl("São Carlos", unidade)                        ~ "São Carlos",
    grepl("Lorena", unidade)                            ~ "Lorena",
    grepl("Luiz de Queiroz", unidade)                   ~ "Piracicaba",
    grepl("Zootecnia e Engenharia de Alimentos", unidade) ~ "Pirassununga",
    TRUE                                                ~ "São Paulo"
  )
}

# Ordem das areas nas tabelas e figuras: da maior para a menor taxa de IC,
# definida uma vez para que todos os graficos fiquem consistentes.
ordenar_areas <- function(dados) {
  ord <- dados |>
    dplyr::group_by(area) |>
    dplyr::summarise(m = mean(IC, na.rm = TRUE), .groups = "drop") |>
    dplyr::arrange(dplyr::desc(m))
  dplyr::mutate(dados, area = factor(area, levels = ord$area))
}
