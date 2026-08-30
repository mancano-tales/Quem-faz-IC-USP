# ------------------------------------------------------------------------------
# 05-cursos.R -- o acesso a IC no nivel do curso
#
# A area agrega demais e a unidade ainda esconde variacao interna: a Escola
# Politecnica oferta dezenas de engenharias, a FFLCH oferta Letras e Filosofia
# lado a lado. O curso e a unidade em que o estudante de fato vive.
#
# Entrada: data/ic_usp.parquet, data-raw/unidades-areas.csv
# Saida  : outputs/tables/tab-11*, tab-12*, outputs/figures/fig-7*, fig-8*, fig-9*
# ------------------------------------------------------------------------------

source(here::here("R", "setup.R"))
source(here::here("R", "recode.R"))

usp <- arrow::read_parquet(ARQ_PAINEL) |>
  dplyr::filter(ano %in% COORTES) |>
  aplicar_recodificacoes() |>
  dplyr::mutate(IC = dplyr::coalesce(IC, 0)) |>
  dplyr::left_join(carregar_areas(), by = "unidade") |>
  ordenar_areas()

pct <- function(x) scales::percent(x, accuracy = 0.1, decimal.mark = ",")

# Cursos pequenos dao proporcoes instaveis; 200 ingressantes em treze coortes
# sao cerca de quinze por ano, o minimo para uma proporcao dizer alguma coisa.
MIN_CURSO <- 200

cursos <- usp |>
  dplyr::group_by(area, unidade, curso) |>
  dplyr::summarise(n = dplyr::n(), fez_ic = mean(IC), .groups = "drop") |>
  dplyr::filter(n >= MIN_CURSO) |>
  dplyr::arrange(dplyr::desc(fez_ic))

writexl::write_xlsx(cursos, file.path(DIR_TABLES, "tab-11-cursos.xlsx"))

# Ranking completo das unidades ------------------------------------------------

unidades <- usp |>
  dplyr::group_by(area, unidade) |>
  dplyr::summarise(n = dplyr::n(), fez_ic = mean(IC), .groups = "drop") |>
  dplyr::filter(n >= MIN_INGRESSANTES) |>
  dplyr::arrange(dplyr::desc(fez_ic))

fig7 <- unidades |>
  ggplot2::ggplot(ggplot2::aes(x = stats::reorder(unidade, fez_ic), y = fez_ic,
                               fill = area)) +
  ggplot2::geom_col(width = 0.75) +
  ggplot2::geom_text(ggplot2::aes(label = pct(fez_ic)), hjust = -0.15, size = 2.7) +
  ggplot2::scale_y_continuous(labels = scales::percent,
                              expand = ggplot2::expansion(mult = c(0, 0.12))) +
  ggplot2::coord_flip() +
  ggplot2::labs(x = NULL, y = "Estudantes que realizaram IC", fill = NULL) +
  TEMA + ggplot2::theme(axis.text.y = ggplot2::element_text(size = 7.5)) +
  ggplot2::guides(fill = ggplot2::guide_legend(nrow = 3))

ggplot2::ggsave(file.path(DIR_FIGURES, "fig-7-ranking-unidades.png"),
                fig7, width = 10, height = 9, dpi = 300)

# Os extremos do ranking de cursos ---------------------------------------------

# O mesmo nome de curso existe em unidades diferentes (Odontologia em tres
# faculdades, por exemplo). O rotulo carrega a unidade para nao confundi-los.
cursos <- cursos |>
  dplyr::group_by(curso) |>
  dplyr::mutate(rotulo = if (dplyr::n() > 1)
                  paste0(curso, " (", campus_unidade(unidade), ")") else curso) |>
  dplyr::ungroup()

extremos <- rbind(
  utils::head(cursos, 15) |> dplyr::mutate(grupo = "15 maiores"),
  utils::tail(cursos, 15) |> dplyr::mutate(grupo = "15 menores")
)

writexl::write_xlsx(extremos, file.path(DIR_TABLES, "tab-12-cursos-extremos.xlsx"))

fig8 <- extremos |>
  ggplot2::ggplot(ggplot2::aes(x = stats::reorder(rotulo, fez_ic), y = fez_ic,
                               fill = area)) +
  ggplot2::geom_col(width = 0.75) +
  ggplot2::geom_text(ggplot2::aes(label = pct(fez_ic)), hjust = -0.15, size = 2.8) +
  ggplot2::facet_wrap(~ grupo, ncol = 1, scales = "free") +
  ggplot2::scale_y_continuous(labels = scales::percent,
                              expand = ggplot2::expansion(mult = c(0, 0.15))) +
  ggplot2::coord_flip() +
  ggplot2::labs(x = NULL, y = "Estudantes que realizaram IC", fill = NULL) +
  TEMA + ggplot2::theme(axis.text.y = ggplot2::element_text(size = 8)) +
  ggplot2::guides(fill = ggplot2::guide_legend(nrow = 3))

ggplot2::ggsave(file.path(DIR_FIGURES, "fig-8-cursos-extremos.png"),
                fig8, width = 10, height = 9, dpi = 300)

# Todos os cursos, por area ----------------------------------------------------
# Mostra a variacao interna que a media da area esconde.

fig9 <- cursos |>
  ggplot2::ggplot(ggplot2::aes(x = area, y = fez_ic)) +
  ggplot2::geom_boxplot(outlier.shape = NA, colour = "grey65", width = 0.5) +
  ggplot2::geom_jitter(ggplot2::aes(size = n, colour = area), width = 0.18,
                       alpha = 0.65, show.legend = FALSE) +
  ggplot2::scale_y_continuous(labels = scales::percent) +
  ggplot2::scale_size_continuous(range = c(0.8, 5), guide = "none") +
  ggplot2::coord_flip() +
  ggplot2::labs(x = NULL,
                y = sprintf("Estudantes que realizaram IC, por curso (n ≥ %d)",
                            MIN_CURSO)) +
  TEMA

ggplot2::ggsave(file.path(DIR_FIGURES, "fig-9-cursos-por-area.png"),
                fig9, width = 9, height = 5.5, dpi = 300)

# Licenciatura contra bacharelado ----------------------------------------------
# Varios cursos no fundo do ranking sao licenciaturas. Vale separar: a
# licenciatura forma professores, nao pesquisadores, e a IC pode simplesmente
# nao fazer parte do que se espera dessa trajetoria.
#
# O nome do curso nao serve para identificar o vinculo -- a maioria nao traz
# "Bacharelado" nem "Licenciatura" no nome. Usamos os campos de situacao do
# painel, lembrando que eles marcam a ausencia de vinculo com a frase
# "Nao cursa/cursou ...", e nao com um valor faltante.

tem_vinculo <- function(x, ausencia) !is.na(x) & x != ausencia

usp <- usp |>
  dplyr::mutate(
    bac = tem_vinculo(situ_bacharel,     "Não cursa/cursou bacharelado"),
    lic = tem_vinculo(situ_licenciatura, "Não cursa/cursou licenciatura"),
    tipo = dplyr::case_when(
      bac & lic  ~ "Bacharelado e licenciatura",
      bac & !lic ~ "Somente bacharelado",
      !bac & lic ~ "Somente licenciatura",
      TRUE       ~ "Sem vínculo registrado"
    )
  )

tab_lic <- usp |>
  dplyr::group_by(tipo) |>
  dplyr::summarise(n = dplyr::n(), fez_ic = mean(IC), .groups = "drop") |>
  dplyr::arrange(dplyr::desc(fez_ic))

writexl::write_xlsx(tab_lic, file.path(DIR_TABLES, "tab-13-licenciatura.xlsx"))

# Relato ------------------------------------------------------------------------

message("\n--- Cursos com n >= ", MIN_CURSO, ": ", nrow(cursos), " ---")
message(sprintf("  maior: %s (%s, n = %d)", cursos$curso[1],
                pct(cursos$fez_ic[1]), cursos$n[1]))
u <- utils::tail(cursos, 1)
message(sprintf("  menor: %s (%s, n = %d)", u$curso, pct(u$fez_ic), u$n))
message(sprintf("  razão entre os extremos: %.0f vezes",
                cursos$fez_ic[1] / u$fez_ic))

message("\n--- 10 maiores ---")
print(as.data.frame(utils::head(cursos, 10) |>
        dplyr::transmute(curso, area, n, `% IC` = pct(fez_ic))), row.names = FALSE)

message("\n--- 10 menores ---")
print(as.data.frame(utils::tail(cursos, 10) |>
        dplyr::transmute(curso, area, n, `% IC` = pct(fez_ic))), row.names = FALSE)

message("\n--- Licenciatura x bacharelado ---")
print(as.data.frame(tab_lic |> dplyr::transmute(tipo, n, `% IC` = pct(fez_ic))),
      row.names = FALSE)

message("\n--- Quantos dos 15 cursos com menor acesso são licenciaturas? ---")
message(sprintf("  %d de 15",
                sum(grepl("Licenciatura", utils::tail(cursos, 15)$curso))))
