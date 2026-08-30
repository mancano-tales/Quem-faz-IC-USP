# ------------------------------------------------------------------------------
# make-areas.R -- gera data-raw/unidades-areas.csv
#
#   Rscript tools/make-areas.R
#
# A USP nao publica, no arquivo do SIC-USP, a area do conhecimento de cada
# unidade: so o nome da unidade. Esta classificacao e nossa, e segue as grandes
# areas do CNPq, com duas decisoes que precisam ficar explicitas:
#
#   - Unidades que abrigam cursos de varias areas (EACH, FFCLRP, os cursos
#     interunidades e a Pro-reitoria de Graduacao, que registra licenciaturas
#     interunidades) vao para "Multidisciplinar". Force-las numa area seria
#     inventar homogeneidade que elas nao tem.
#   - A FFLCH abriga tanto Ciencias Humanas quanto Letras. Ela vai inteira para
#     Ciencias Humanas, que e como o artigo original a trata e como a propria
#     USP a classifica; a alternativa exigiria separar por curso, o que muda a
#     unidade de analise.
#
# O CSV e o registro auditavel dessas escolhas: quem discordar edita a coluna
# `area` e roda o pipeline de novo.
# ------------------------------------------------------------------------------

source(here::here("R", "setup.R"))

# A classificacao cobre a janela ampla, e nao a das descritivas: uma unidade que
# so recebeu ingressantes a partir de 2019 precisa ter area atribuida do mesmo
# jeito, senao o join do modelo deixa buracos.
unidades <- arrow::read_parquet(ARQ_PAINEL) |>
  dplyr::filter(ano %in% COORTES_MODELO) |>
  dplyr::count(unidade, name = "n_ingressantes") |>
  dplyr::arrange(dplyr::desc(n_ingressantes))

# Regras aplicadas na ordem: a primeira que casar define a area.
REGRAS <- list(
  # Multidisciplinar vem primeiro: sao unidades que abrigam varias areas, e
  # cujos nomes casariam com regras mais especificas abaixo.
  c("Multidisciplinar",         "Artes, Ciências e Humanidades"),
  c("Multidisciplinar",         "Filosofia, Ciências e Letras de Ribeirão Preto"),
  c("Multidisciplinar",         "Interunidades"),
  c("Multidisciplinar",         "Pró-reitoria de Graduação"),
  c("Multidisciplinar",         "Escola de Engenharia de São Carlos e Instituto"),
  c("Multidisciplinar",         "Física Médica"),

  c("Ciências Humanas",         "Filosofia, Letras e Ciências Humanas"),
  c("Ciências Humanas",         "Faculdade de Educação"),
  c("Ciências Humanas",         "Instituto de Psicologia"),

  c("Linguística, Letras e Artes", "Comunicações e Artes"),

  c("Ciências Sociais Aplicadas", "Economia, Administração"),
  c("Ciências Sociais Aplicadas", "Faculdade de Direito"),
  c("Ciências Sociais Aplicadas", "Arquitetura e Urbanismo"),
  c("Ciências Sociais Aplicadas", "Relações Internacionais"),

  c("Ciências Agrárias",        "Luiz de Queiroz"),
  c("Ciências Agrárias",        "Medicina Veterinária"),
  c("Ciências Agrárias",        "Zootecnia e Engenharia de Alimentos"),

  c("Engenharias",              "Politécnica"),
  c("Engenharias",              "Escola de Engenharia"),

  c("Ciências da Saúde",        "Faculdade de Medicina"),
  c("Ciências da Saúde",        "Odontologia"),
  c("Ciências da Saúde",        "Ciências Farmacêuticas"),
  c("Ciências da Saúde",        "Enfermagem"),
  c("Ciências da Saúde",        "Saúde Pública"),
  c("Ciências da Saúde",        "Educação Física"),

  c("Ciências Biológicas",      "Biociências"),
  c("Ciências Biológicas",      "Ciências Biomédicas"),

  c("Ciências Exatas e da Terra", "Matemática"),
  c("Ciências Exatas e da Terra", "Instituto de Física"),
  c("Ciências Exatas e da Terra", "Instituto de Química"),
  c("Ciências Exatas e da Terra", "Geociências"),
  c("Ciências Exatas e da Terra", "Astronomia"),
  c("Ciências Exatas e da Terra", "Oceanográfico")
)

classificar <- function(nome) {
  for (r in REGRAS) if (grepl(r[2], nome, fixed = TRUE)) return(r[1])
  NA_character_
}

unidades$area <- vapply(unidades$unidade, classificar, character(1))

if (anyNA(unidades$area)) {
  stop("unidades sem area atribuida:\n  ",
       paste(unidades$unidade[is.na(unidades$area)], collapse = "\n  "))
}

utils::write.csv(unidades[, c("unidade", "area", "n_ingressantes")],
                 file.path(DIR_RAW, "unidades-areas.csv"),
                 row.names = FALSE, fileEncoding = "UTF-8")

message("unidades-areas.csv gravado: ", nrow(unidades), " unidades em ",
        dplyr::n_distinct(unidades$area), " áreas.")
print(unidades |> dplyr::count(area, wt = n_ingressantes, name = "ingressantes") |>
        dplyr::arrange(dplyr::desc(ingressantes)))
