# ------------------------------------------------------------------------------
# 01-build-data.R -- constroi o painel analitico a partir dos microdados brutos
#
# Entrada : data-raw/sic-usp-243654-ingressantes.xlsx  (sistema JupiterWeb/PRG)
#           data-raw/sic-usp-243681-perfil-ic.xlsx     (sistema Atena/PRP)
# Saida   : data/ic_usp.parquet
#
# O painel tem uma linha por estudante ingressante na USP (298.704 linhas) com
# as variaveis de graduacao, as respostas do questionario socioeconomico da
# FUVEST (QASE) e o resumo da participacao em Iniciacao Cientifica.
#
# Este script reconstroi um elo do pipeline que nao havia sido versionado: o
# `ic_usp.parquet` distribuido com o artigo existia como arquivo, mas o codigo
# que o gerava se perdeu. As tres regras de agregacao abaixo foram inferidas dos
# brutos e conferidas contra o parquet original (ver README, secao "Validacao").
# ------------------------------------------------------------------------------

source(here::here("R", "setup.R"))

# 1. Ingressantes (JupiterWeb) -------------------------------------------------
# O arquivo tem 29 colunas, na ordem abaixo. As respostas do QASE chegam
# rotuladas como "Resposta Item NN"; renomeamos para nomes falantes.
#
# Atencao a uma inconsistencia herdada, mantida para nao quebrar a comparacao
# com os resultados publicados: `ef1` e o Item 14 (onde cursou o ensino
# fundamental), mas `ef2` e o Item 15, que pergunta onde o estudante cursou o
# ensino MEDIO -- apesar do prefixo "ef". `em` e o Item 16 (tipo de ensino
# medio concluido).

message("Lendo ingressantes (SIC-USP #243654)...")
jupiter <- readxl::read_excel(ARQ_JUPITER, sheet = "Planilha1")

names(jupiter) <- c(
  "id", "curso", "unidade", "ano", "periodo", "modo_ingresso",
  "class_carreira", "modalidade", "situ_bacharel", "ano_conclusao_barcharel",
  "situ_licenciatura", "ano_conclusao_licenciatura", "sexo", "raca",
  "n_ingresso",
  "estado_civil",    # Item 13 - estado civil
  "ef1",             # Item 14 - onde cursou o ensino fundamental
  "ef2",             # Item 15 - onde cursou o ensino medio
  "em",              # Item 16 - tipo de ensino medio concluido
  "rfm",             # Item 17 - renda familiar mensal
  "pessoas_contrib", # Item 18 - pessoas que contribuem para a renda
  "pessoas_resid",   # Item 19 - pessoas que vivem da renda
  "atv_remu",        # Item 20 - exerce atividade remunerada
  "educ_resp1",      # Item 21 - instrucao do pai ou responsavel 1
  "educ_resp2",      # Item 22 - instrucao da mae ou responsavel 2
  "ocup_resp1",      # Item 23 - ocupacao do principal contribuinte
  "pretensao_mant",  # Item 24 - como pretende se manter na graduacao
  "idade_ano_vest",
  "inclusp"          # Item 26 - participacao no processo INCLUSP
)

# 2. Projetos de Iniciacao Cientifica (Atena) ----------------------------------
# Uma linha por projeto: 29.544 projetos de 22.301 estudantes.

message("Lendo projetos de IC (SIC-USP #243681)...")
atena <- readxl::read_excel(ARQ_ATENA, sheet = "BD")

# 2a. Atributos do projeto, colapsados para uma linha por estudante.
#     Regra: primeiro projeto registrado no Atena (a ordem do arquivo e
#     cronologica). Descreve o vinculo de IC mais antigo do estudante.
projeto_representativo <- atena |>
  dplyr::group_by(Identificador) |>
  dplyr::slice(1) |>
  dplyr::ungroup() |>
  dplyr::select(
    id              = Identificador,
    situ            = situacao,
    tipo_fomento    = TipoFomento,
    data_inicio     = dtainiprj,
    data_fim        = dtafimprj,
    aluno           = `Aluno(USP/EXTERNO)`,
    unidade_docente = unidadeDocente,
    setor_docente   = setorDocente,
    sexo_docente    = sexoDocente,
    raca_docente    = racaDocente
  )

# 2b. Contagem de projetos por tipo de fomento, em formato largo.
#     Vira uma coluna por fonte financiadora (PIBIC, FAPESP, PUB, etc.).
#     Os projetos sem fomento informado geram uma coluna `NA`, que descartamos
#     depois do pivot -- e nao antes: assim os 398 estudantes cujos projetos
#     estao todos sem essa informacao permanecem na base, com zero em todas as
#     fontes, em vez de sumirem do cruzamento.
fomento_largo <- atena |>
  dplyr::count(Identificador, TipoFomento) |>
  tidyr::pivot_wider(names_from = TipoFomento, values_from = n, values_fill = 0) |>
  dplyr::select(-dplyr::any_of("NA")) |>
  dplyr::rename(id = Identificador)

# Ordena as fontes de fomento alfabeticamente, para que a ordem das colunas do
# painel nao dependa da ordem em que os tipos aparecem no arquivo de origem.
fomento_largo <- fomento_largo[, c("id", sort(setdiff(names(fomento_largo), "id")))]

# 2c. Quantidade de ICs do estudante.
#     Conta apenas projetos com tipo de fomento informado. Nao usamos o campo
#     `N_de_ICs` do proprio Atena: ele diverge da contagem em 576 estudantes,
#     e e a contagem que reproduz os numeros publicados.
qtd_ics <- atena |>
  dplyr::group_by(Identificador) |>
  dplyr::summarise(qtd_ic = sum(!is.na(TipoFomento)), .groups = "drop") |>
  dplyr::rename(id = Identificador)

# 3. Painel -------------------------------------------------------------------
# left_join a partir dos ingressantes: quem nunca fez IC fica com NA nas
# colunas de IC, o que distingue "nao fez IC" de "nao consta na base do Atena".

message("Montando o painel...")
ic_usp <- jupiter |>
  dplyr::left_join(projeto_representativo, by = "id") |>
  dplyr::left_join(fomento_largo,          by = "id") |>
  dplyr::left_join(qtd_ics,                by = "id") |>
  dplyr::mutate(IC = dplyr::if_else(qtd_ic >= 1, 1, 0))

# 4. Verificacoes de integridade ----------------------------------------------

stopifnot(
  "o painel deve ter uma linha por ingressante" =
    nrow(ic_usp) == nrow(jupiter),
  "o identificador deve ser unico" =
    !anyDuplicated(ic_usp$id),
  "IC deve ser binaria onde qtd_ic esta preenchida" =
    all(ic_usp$IC %in% c(0, 1) | is.na(ic_usp$IC)),

  # O painel distribuido com o artigo tem estas dimensoes exatas. Se a leitura
  # dos brutos ou uma das regras de agregacao mudar, o script para aqui.
  "o painel deve ter 298.704 linhas e 54 colunas" =
    identical(dim(ic_usp), c(298704L, 54L)),
  "21.902 estudantes fizeram ao menos uma IC" =
    sum(ic_usp$IC == 1, na.rm = TRUE) == 21902,
  "22.300 estudantes constam na base do Atena" =
    sum(!is.na(ic_usp$qtd_ic)) == 22300
)

message(sprintf(
  "Painel: %s estudantes, %s colunas | %s com ao menos uma IC | coortes %s-%s",
  format(nrow(ic_usp), big.mark = "."), ncol(ic_usp),
  format(sum(ic_usp$IC == 1, na.rm = TRUE), big.mark = "."),
  min(ic_usp$ano, na.rm = TRUE), max(ic_usp$ano, na.rm = TRUE)
))

arrow::write_parquet(ic_usp, ARQ_PAINEL)
message("Gravado em ", ARQ_PAINEL)
