# ------------------------------------------------------------------------------
# 07-genero-curso.R -- o efeito de sexo sobrevive ao controle por curso?
#
# Uma versao anterior deste trabalho afirmou que o efeito de sexo "nao e
# segregacao horizontal entre carreiras, porque os modelos sao estimados dentro
# de cada area". A afirmacao era forte demais: uma area abriga dezenas de
# cursos, e homens e mulheres nao se distribuem igualmente entre eles. Em
# Ciencias da Saude convivem Enfermagem e Medicina; em Agrarias, Veterinaria e
# Zootecnia -- com composicoes de sexo e taxas de IC bem diferentes.
#
# Este script faz o teste que faltava: acrescenta efeitos fixos de curso ao
# modelo de cada area e mede quanto do efeito de sexo permanece.
#
# Entrada: data/ic_usp.parquet, data-raw/unidades-areas.csv
# Saida  : outputs/tables/tab-14-genero-curso.xlsx, outputs/figures/fig-10-*
# ------------------------------------------------------------------------------

source(here::here("R", "setup.R"))
source(here::here("R", "recode.R"))
source(here::here("R", "modelos.R"))

usp <- arrow::read_parquet(ARQ_PAINEL) |>
  dplyr::filter(ano %in% COORTES_MODELO) |>
  aplicar_recodificacoes() |>
  dplyr::mutate(IC = dplyr::coalesce(IC, 0)) |>
  dplyr::left_join(carregar_areas(), by = "unidade")

FORMULA_CURSO <- stats::update(FORMULA_BASE, . ~ . + curso)

# Cursos com poucos casos, ou sem variacao na variavel dependente, nao
# contribuem para o efeito fixo e so introduzem parametros.
preparar <- function(dados) {
  dados |>
    dplyr::select(dplyr::all_of(VARIAVEIS), curso) |>
    stats::na.exclude() |>
    dplyr::group_by(curso) |>
    dplyr::filter(dplyr::n() >= 100, dplyr::n_distinct(IC) > 1) |>
    dplyr::ungroup()
}

efeito_sexo <- function(m, d) {
  a <- ame(m, d)
  a[a$nivel == "M (vs F)", c("ame", "ep", "inf", "sup", "p")]
}

areas <- sort(unique(usp$area))

resultados <- do.call(rbind, lapply(areas, function(a) {
  d <- preparar(dplyr::filter(usp, area == a))
  if (nrow(d) < 500 || dplyr::n_distinct(d$curso) < 2) return(NULL)

  sem  <- efeito_sexo(ajustar(d, FORMULA_BASE),  d)
  com  <- efeito_sexo(ajustar(d, FORMULA_CURSO), d)

  data.frame(
    area = a, n = nrow(d), cursos = dplyr::n_distinct(d$curso),
    ame_sem = sem$ame, inf_sem = sem$inf, sup_sem = sem$sup,
    ame_com = com$ame, inf_com = com$inf, sup_com = com$sup, p_com = com$p,
    # A razao so faz sentido quando ha efeito para sobreviver: onde o efeito
    # sem controle ja e proximo de zero, ela explode e nao informa nada.
    sobrevive = ifelse(abs(sem$ame) < 0.01, NA_real_, com$ame / sem$ame)
  )
}))

writexl::write_xlsx(resultados, file.path(DIR_TABLES, "tab-14-genero-curso.xlsx"))

# Figura: o efeito antes e depois do controle, com intervalos ------------------

longo <- rbind(
  resultados |> dplyr::transmute(area, especificacao = "Sem efeito fixo de curso",
                                 ame = ame_sem, inf = inf_sem, sup = sup_sem),
  resultados |> dplyr::transmute(area, especificacao = "Com efeito fixo de curso",
                                 ame = ame_com, inf = inf_com, sup = sup_com)
) |>
  dplyr::mutate(especificacao = factor(especificacao,
    levels = c("Sem efeito fixo de curso", "Com efeito fixo de curso")))

fig <- longo |>
  ggplot2::ggplot(ggplot2::aes(x = stats::reorder(area, ame), y = ame,
                               colour = especificacao)) +
  ggplot2::geom_hline(yintercept = 0, colour = "grey50", linetype = "dashed") +
  ggplot2::geom_pointrange(ggplot2::aes(ymin = inf, ymax = sup),
                           position = ggplot2::position_dodge(width = 0.5),
                           size = 0.45) +
  ggplot2::scale_y_continuous(labels = scales::percent) +
  ggplot2::scale_colour_manual(values = c("Sem efeito fixo de curso" = "#e97d5a",
                                          "Com efeito fixo de curso" = "#1bb57f")) +
  ggplot2::coord_flip() +
  ggplot2::labs(x = NULL, colour = NULL,
                y = "Efeito marginal médio de ser homem (IC 95%)") +
  TEMA

ggplot2::ggsave(file.path(DIR_FIGURES, "fig-10-genero-curso.png"),
                fig, width = 9, height = 5.5, dpi = 300)

# Relato ------------------------------------------------------------------------

message("\n--- O efeito de sexo antes e depois do controle por curso ---")
print(as.data.frame(resultados |> dplyr::transmute(
  area, n, cursos,
  `sem curso` = sprintf("%+.2f", 100 * ame_sem),
  `com curso` = sprintf("%+.2f [%+.2f, %+.2f]", 100 * ame_com, 100 * inf_com, 100 * sup_com),
  `sobrevive` = ifelse(is.na(sobrevive), "—", sprintf("%.0f%%", 100 * sobrevive)),
  `p` = ifelse(p_com < 0.001, "<0,001", sprintf("%.3f", p_com))
)), row.names = FALSE)

message("\n--- Em quantas areas o efeito segue significativo com curso? ---")
message(sprintf("  %d de %d (p < 0,05)", sum(resultados$p_com < 0.05), nrow(resultados)))
