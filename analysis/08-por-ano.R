# ------------------------------------------------------------------------------
# 08-por-ano.R -- acesso a IC por unidade e por curso, coorte a coorte
#
# As medias do periodo escondem trajetorias: uma unidade pode ter media alta
# porque comecou alta e caiu, ou porque comecou baixa e subiu. E a leitura
# coorte a coorte que separa expansao de estagnacao.
#
# A serie para em 2018 de proposito. As coortes seguintes estao censuradas a
# direita -- quem ingressou em 2021 ainda nao teve tempo de comecar uma IC
# dentro do periodo observado -- e a queda que aparece nelas e artefato de
# observacao, nao reducao de acesso.
#
# Entrada: data/ic_usp.parquet, data-raw/unidades-areas.csv
# Saida  : outputs/tables/tab-15*, tab-16*, outputs/figures/fig-11*, fig-12*
# ------------------------------------------------------------------------------

source(here::here("R", "setup.R"))
source(here::here("R", "recode.R"))

usp <- arrow::read_parquet(ARQ_PAINEL) |>
  dplyr::filter(ano %in% COORTES) |>
  dplyr::mutate(IC = dplyr::coalesce(IC, 0)) |>
  dplyr::left_join(carregar_areas(), by = "unidade") |>
  ordenar_areas()

pct <- function(x) scales::percent(x, accuracy = 0.1, decimal.mark = ",")

# Por unidade e coorte ---------------------------------------------------------

por_unidade <- usp |>
  dplyr::group_by(area, unidade, ano) |>
  dplyr::summarise(n = dplyr::n(), fez = sum(IC), prop = mean(IC), .groups = "drop")

# Unidades pequenas dao proporcoes anuais instaveis: o corte usa o total do
# periodo, para nao excluir uma unidade so por causa de um ano fraco.
grandes <- por_unidade |>
  dplyr::group_by(unidade) |>
  dplyr::summarise(total = sum(n), media = sum(fez) / sum(n), .groups = "drop") |>
  dplyr::filter(total >= MIN_INGRESSANTES)

por_unidade <- dplyr::semi_join(por_unidade, grandes, by = "unidade")

tab_unidade_ano <- por_unidade |>
  dplyr::select(area, unidade, ano, prop) |>
  tidyr::pivot_wider(names_from = ano, values_from = prop) |>
  dplyr::left_join(grandes, by = "unidade") |>
  dplyr::arrange(dplyr::desc(media))

writexl::write_xlsx(tab_unidade_ano,
                    file.path(DIR_TABLES, "tab-15-unidade-por-ano.xlsx"))

# Por curso e coorte -----------------------------------------------------------

MIN_CURSO <- 200

por_curso <- usp |>
  dplyr::group_by(area, unidade, curso, ano) |>
  dplyr::summarise(n = dplyr::n(), fez = sum(IC), prop = mean(IC), .groups = "drop")

cursos_grandes <- por_curso |>
  dplyr::group_by(unidade, curso) |>
  dplyr::summarise(total = sum(n), media = sum(fez) / sum(n), .groups = "drop") |>
  dplyr::filter(total >= MIN_CURSO)

por_curso <- dplyr::semi_join(por_curso, cursos_grandes, by = c("unidade", "curso"))

tab_curso_ano <- por_curso |>
  dplyr::select(area, unidade, curso, ano, prop) |>
  tidyr::pivot_wider(names_from = ano, values_from = prop) |>
  dplyr::left_join(cursos_grandes, by = c("unidade", "curso")) |>
  dplyr::arrange(dplyr::desc(media))

writexl::write_xlsx(tab_curso_ano,
                    file.path(DIR_TABLES, "tab-16-curso-por-ano.xlsx"))

# Figuras ----------------------------------------------------------------------
# Um mapa de calor le melhor que uma tabela de 48 linhas por 9 colunas: mostra
# de uma vez o nivel de cada unidade e a direcao da sua trajetoria.

mapa <- function(dados, rotulo, arquivo, altura) {
  fig <- dados |>
    ggplot2::ggplot(ggplot2::aes(x = factor(ano),
                                 y = stats::reorder(!!rlang::sym(rotulo), media),
                                 fill = prop)) +
    ggplot2::geom_tile(colour = "white", linewidth = 0.4) +
    ggplot2::geom_text(ggplot2::aes(label = sprintf("%.0f", 100 * prop)),
                       size = 2.4, colour = "grey15") +
    ggplot2::scale_fill_gradient(low = "white", high = "#2b6a99",
                                 labels = scales::percent, name = "% que fez IC") +
    ggplot2::labs(x = "Coorte de ingresso", y = NULL) +
    ggplot2::theme_minimal(base_size = 10) +
    ggplot2::theme(panel.grid = ggplot2::element_blank(),
                   legend.position = "bottom",
                   axis.text.y = ggplot2::element_text(size = 7))
  ggplot2::ggsave(file.path(DIR_FIGURES, arquivo), fig,
                  width = 9, height = altura, dpi = 300)
  fig
}

invisible(mapa(dplyr::left_join(por_unidade, grandes, by = "unidade"),
               "unidade", "fig-11-unidade-por-ano.png", 9))

# Para cursos, os vinte extremos: o mapa completo teria 136 linhas.
extremos <- dplyr::bind_rows(
  utils::head(cursos_grandes[order(-cursos_grandes$media), ], 10),
  utils::tail(cursos_grandes[order(-cursos_grandes$media), ], 10))

invisible(mapa(
  por_curso |>
    dplyr::semi_join(extremos, by = c("unidade", "curso")) |>
    dplyr::left_join(cursos_grandes, by = c("unidade", "curso")) |>
    dplyr::mutate(curso = paste0(curso, " — ", campus_unidade(unidade))),
  "curso", "fig-12-curso-por-ano.png", 6))

# A serie e comparavel entre coortes? ------------------------------------------
#
# A censura das coortes recentes tem um espelho: quem ingressou em 2010 teve
# doze anos de observacao, quem ingressou em 2018 teve quatro. Se a IC fosse
# iniciada tardiamente na graduacao, as coortes antigas apareceriam com taxas
# infladas e qualquer queda na serie seria artefato.
#
# O teste: recalcular o indicador contando apenas as ICs iniciadas em ate cinco
# anos apos o ingresso -- uma janela de exposicao igual para todas as coortes de
# 2010 a 2018, ja que os dados de projeto vao ate 2023.

atena <- readxl::read_excel(ARQ_ATENA, sheet = "BD")
atena$anoproj <- suppressWarnings(as.numeric(atena$`Ano do Projeto`))

ic_em_cinco_anos <- atena |>
  dplyr::filter(!is.na(TipoFomento), !is.na(anoproj)) |>
  dplyr::select(id = Identificador, anoproj) |>
  dplyr::inner_join(dplyr::select(usp, id, ano), by = "id") |>
  dplyr::filter(anoproj >= ano, anoproj <= ano + 5) |>
  dplyr::distinct(id)

comparacao_exposicao <- usp |>
  dplyr::mutate(ic_5 = as.integer(id %in% ic_em_cinco_anos$id)) |>
  dplyr::group_by(ano) |>
  dplyr::summarise(n = dplyr::n(), bruto = mean(IC), em_cinco_anos = mean(ic_5),
                   .groups = "drop") |>
  dplyr::mutate(diferenca = bruto - em_cinco_anos)

writexl::write_xlsx(comparacao_exposicao,
                    file.path(DIR_TABLES, "tab-17-exposicao.xlsx"))

stopifnot(
  "a exposicao desigual entre coortes nao deve mover a serie em mais de 0,5 pp" =
    max(abs(comparacao_exposicao$diferenca)) < 0.005
)

# Relato ------------------------------------------------------------------------

message("\n--- Unidades: ", nrow(grandes), " com ", MIN_INGRESSANTES,
        "+ ingressantes | cursos: ", nrow(cursos_grandes), " com ", MIN_CURSO, "+ ---")

message("\n--- A serie e comparavel entre coortes? ---")
message(sprintf("  maior diferenca entre o indicador bruto e o de 5 anos: %.2f pp",
                100 * max(abs(comparacao_exposicao$diferenca))))
message("  (praticamente toda IC comeca nos primeiros cinco anos, entao a serie")
message("   nao e contaminada pela exposicao desigual das coortes)")

message("\n--- Acesso a IC por unidade e coorte (5 maiores e 5 menores) ---")
mostra <- dplyr::bind_rows(utils::head(tab_unidade_ano, 5),
                           utils::tail(tab_unidade_ano, 5)) |>
  dplyr::mutate(unidade = substr(unidade, 1, 34)) |>
  dplyr::select(unidade, dplyr::all_of(as.character(COORTES))) |>
  dplyr::mutate(dplyr::across(-unidade, pct))
print(as.data.frame(mostra), row.names = FALSE)

message("\n--- Quem mais cresceu entre 2010 e 2018 ---")
cresc <- tab_unidade_ano |>
  dplyr::mutate(delta = .data[["2018"]] - .data[["2010"]]) |>
  dplyr::arrange(dplyr::desc(delta)) |>
  dplyr::transmute(unidade = substr(unidade, 1, 40),
                   `2010` = pct(.data[["2010"]]), `2018` = pct(.data[["2018"]]),
                   variação = sprintf("%+.1f pp", 100 * delta))
print(as.data.frame(utils::head(cresc, 5)), row.names = FALSE)
print(as.data.frame(utils::tail(cresc, 3)), row.names = FALSE)
