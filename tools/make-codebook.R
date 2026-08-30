# ------------------------------------------------------------------------------
# make-codebook.R -- gera o CODEBOOK.md a partir do painel
#
#   Rscript tools/make-codebook.R
#
# As descricoes vem do dicionario de dados do SIC-USP
# (data-raw/auxiliar/dicionario-variaveis-sic-usp.xlsx); as estatisticas de
# preenchimento e as categorias sao calculadas do proprio painel, de modo que o
# codebook nunca fica dessincronizado dos dados.
# ------------------------------------------------------------------------------

source(here::here("R", "setup.R"))

painel <- arrow::read_parquet(ARQ_PAINEL)

# Descricao de cada variavel. As do QASE trazem o numero do item no
# questionario, que e como elas aparecem no arquivo bruto.
DESCRICOES <- c(
  id                         = "Identificador anonimizado do estudante (inteiro sequencial atribuído pela USP)",
  curso                      = "Curso de ingresso",
  unidade                    = "Unidade da USP",
  ano                        = "Ano de ingresso",
  periodo                    = "Período de ingresso",
  modo_ingresso              = "Modo de ingresso (FUVEST, SiSU, transferência, convênio…)",
  class_carreira             = "Classificação na carreira no vestibular; usada como proxy de desempenho",
  modalidade                 = "Modalidade de vaga, incluindo reserva de vagas",
  situ_bacharel              = "Situação atual no bacharelado",
  ano_conclusao_barcharel    = "Ano de conclusão ou desligamento do bacharelado",
  situ_licenciatura          = "Situação atual na licenciatura",
  ano_conclusao_licenciatura = "Ano de conclusão ou desligamento da licenciatura",
  sexo                       = "Sexo (binário no sistema da USP: F ou M)",
  raca                       = "Raça ou cor autodeclarada",
  n_ingresso                 = "Número de reingressos",
  estado_civil               = "QASE item 13 — estado civil",
  ef1                        = "QASE item 14 — onde cursou o ensino fundamental",
  ef2                        = "QASE item 15 — onde cursou o ensino médio (apesar do prefixo `ef`)",
  em                         = "QASE item 16 — tipo de ensino médio concluído",
  rfm                        = "QASE item 17 — renda familiar mensal, em faixas de salário mínimo",
  pessoas_contrib            = "QASE item 18 — pessoas que contribuem para a renda familiar",
  pessoas_resid              = "QASE item 19 — pessoas que vivem dessa renda",
  atv_remu                   = "QASE item 20 — exerce atividade remunerada",
  educ_resp1                 = "QASE item 21 — instrução do pai ou responsável 1",
  educ_resp2                 = "QASE item 22 — instrução da mãe ou responsável 2",
  ocup_resp1                 = "QASE item 23 — ocupação do principal contribuinte da família",
  pretensao_mant             = "QASE item 24 — como pretende se manter durante a graduação",
  idade_ano_vest             = "Idade no ano da prova do vestibular",
  inclusp                    = "QASE item 26 — participação no processo INCLUSP",
  situ                       = "Situação do primeiro projeto de IC registrado",
  tipo_fomento               = "Fonte de fomento do primeiro projeto de IC",
  data_inicio                = "Data de início do primeiro projeto de IC",
  data_fim                   = "Data de término do primeiro projeto de IC",
  aluno                      = "Vínculo do estudante (USP ou externo)",
  unidade_docente            = "Unidade do orientador do primeiro projeto",
  setor_docente              = "Departamento do orientador do primeiro projeto",
  sexo_docente               = "Sexo do orientador",
  raca_docente               = "Raça ou cor do orientador",
  qtd_ic                     = "Número de projetos de IC com fomento informado; `NA` para quem não consta no Atena",
  IC                         = "Indicador binário de ter realizado ao menos uma IC"
)

# As colunas de fomento sao contagens de projetos por fonte financiadora.
FONTES <- setdiff(names(painel), names(DESCRICOES))
for (f in FONTES) {
  DESCRICOES[[f]] <- sprintf("Número de projetos de IC financiados por: %s", f)
}

# Resumo de uma variavel: tipo, preenchimento e o que ela contem -------------
resumir <- function(v, nome) {
  n_na  <- sum(is.na(v))
  preenchido <- 1 - n_na / length(v)
  tipo <- if (inherits(v, "POSIXt") || inherits(v, "Date")) "data"
          else if (is.numeric(v)) "numérica" else "texto"

  valores <- if (is.numeric(v) && dplyr::n_distinct(v, na.rm = TRUE) > 12) {
    r <- range(v, na.rm = TRUE)
    sprintf("de %s a %s", format(r[1]), format(r[2]))
  } else if (inherits(v, "POSIXt") || inherits(v, "Date")) {
    r <- range(v, na.rm = TRUE)
    sprintf("de %s a %s", format(r[1], "%Y-%m-%d"), format(r[2], "%Y-%m-%d"))
  } else {
    cats <- names(sort(table(v), decreasing = TRUE))
    n <- dplyr::n_distinct(v, na.rm = TRUE)
    amostra <- paste0("`", utils::head(cats, 3), "`", collapse = ", ")
    if (n > 3) sprintf("%d categorias; mais frequentes: %s", n, amostra)
    else sprintf("%d categorias: %s", n, amostra)
  }

  data.frame(
    Variável   = sprintf("`%s`", nome),
    Descrição  = DESCRICOES[[nome]],
    Tipo       = tipo,
    Preenchida = sprintf("%.1f%%", 100 * preenchido),
    Conteúdo   = valores,
    check.names = FALSE
  )
}

linhas <- do.call(rbind, lapply(names(painel), function(nm) resumir(painel[[nm]], nm)))

# Escrita ---------------------------------------------------------------------
md <- c(
  "# Codebook — `data/ic_usp.parquet`",
  "",
  "Uma linha por estudante ingressante na USP.",
  sprintf("**%s linhas × %d colunas.**",
          format(nrow(painel), big.mark = "."), ncol(painel)),
  "",
  "Gerado por `tools/make-codebook.R` a partir do próprio painel — as taxas de",
  "preenchimento e as categorias são calculadas, não transcritas. Para",
  "regenerar após mudanças no pipeline:",
  "",
  "```bash",
  "Rscript tools/make-codebook.R",
  "```",
  "",
  "As variáveis prefixadas por `QASE item NN` vêm do Questionário de Avaliação",
  "Socioeconômica aplicado pela FUVEST no vestibular, e chegam no arquivo bruto",
  "rotuladas como `Resposta Item NN`. Note a inconsistência herdada em `ef2`,",
  "mantida para preservar a comparação com os resultados publicados.",
  "",
  "## Variáveis",
  "",
  knitr::kable(linhas, format = "markdown", row.names = FALSE),
  "",
  "## Origem",
  "",
  "As colunas 1 a 29 vêm do pedido SIC-USP #243654 (sistema JupiterWeb, com o",
  "QASE). As colunas 30 em diante são derivadas do pedido #243681 (sistema",
  "Atena), agregadas do nível do projeto para o nível do estudante por",
  "`analysis/01-build-data.R`.",
  "",
  "_Este arquivo é gerado. Não edite à mão: o CI regenera e compara._"
)

writeLines(md, here::here("CODEBOOK.md"))
message("CODEBOOK.md gravado: ", ncol(painel), " variáveis documentadas.")
