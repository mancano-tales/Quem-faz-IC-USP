# ------------------------------------------------------------------------------
# 04-heterogeneidade.R -- o mesmo modelo, area por area
#
# A pergunta nao e apenas se as areas ofertam quantidades diferentes de IC --
# isso a descritiva ja mostrou --, mas se o mecanismo e o mesmo em todas. Se o
# trabalho for o maior obstaculo em Humanas e irrelevante em Saude, o achado do
# artigo original nao generaliza; ele descreve um tipo de area.
#
# Entrada: data/ic_usp.parquet, data-raw/unidades-areas.csv
# Saida  : outputs/tables/tab-8*, tab-9*, outputs/figures/fig-5*, fig-6*
# ------------------------------------------------------------------------------

source(here::here("R", "setup.R"))
source(here::here("R", "recode.R"))
source(here::here("R", "modelos.R"))

usp <- arrow::read_parquet(ARQ_PAINEL) |>
  dplyr::filter(ano %in% COORTES_MODELO) |>
  aplicar_recodificacoes() |>
  dplyr::mutate(IC = dplyr::coalesce(IC, 0)) |>
  dplyr::left_join(carregar_areas(), by = "unidade")

areas <- sort(unique(usp$area))

# Um modelo por area -----------------------------------------------------------
# Ciencias Biologicas tem 2.206 ingressantes; depois de descartar quem tem
# resposta faltante, sobra pouco. Guardamos o N de cada ajuste para que a
# leitura das areas menores seja feita com a devida cautela.

ajustes <- lapply(areas, function(a) {
  d <- amostra_modelo(dplyr::filter(usp, area == a))
  if (nrow(d) < 500 || dplyr::n_distinct(d$IC) < 2) return(NULL)
  m <- ajustar(d, FORMULA_BASE)
  list(area = a, modelo = m, dados = d)
})
names(ajustes) <- areas
ajustes <- Filter(Negate(is.null), ajustes)

# Efeitos marginais por area ---------------------------------------------------

ames <- do.call(rbind, lapply(ajustes, function(x) {
  cbind(area = x$area, n = stats::nobs(x$modelo), ame(x$modelo, x$dados))
}))
rownames(ames) <- NULL

writexl::write_xlsx(ames, file.path(DIR_TABLES, "tab-8-ame-por-area.xlsx"))

# As quatro dimensoes que o artigo original poe em disputa: de um lado as
# caracteristicas adscritas, de outro o vinculo com o trabalho.
DESTAQUES <- c(
  "trabalho|Sim, em tempo integral (vs Não)"                  = "Trabalha em tempo integral",
  "sustento|Por conta própria (vs Suporte da família)"         = "Sustento: por conta própria",
  "periodo|noturno (vs diurno)"                                = "Período noturno",
  "raca|PPI (vs Brancos)"                                      = "Raça: PPI",
  "sfmpct|+1 desvio-padrão"                                    = "Renda per capita",
  "educ_resp|ES completo (vs EF incompleto)"                   = "Responsável com superior",
  "sexo|M (vs F)"                                              = "Sexo: masculino"
)

destaques <- ames |>
  dplyr::mutate(chave = paste(variavel, nivel, sep = "|")) |>
  dplyr::filter(chave %in% names(DESTAQUES)) |>
  dplyr::mutate(efeito = unname(DESTAQUES[chave]))

tab_destaques <- destaques |>
  dplyr::mutate(pp = round(100 * ame, 2)) |>
  dplyr::select(area, efeito, pp) |>
  tidyr::pivot_wider(names_from = efeito, values_from = pp)

writexl::write_xlsx(tab_destaques, file.path(DIR_TABLES, "tab-9-destaques-area.xlsx"))

# Qual e o maior efeito de cada area? ------------------------------------------
# Duas exclusoes: o ano de ingresso, que capta a expansao da politica no periodo
# e nao uma caracteristica do estudante; e a categoria residual "Outros" do
# sustento, que reune de 0,8% a 2,0% dos casos -- catorze pessoas em Ciencias
# Biologicas -- e produz efeitos grandes que sao ruido, nao achado.

maiores <- ames |>
  dplyr::filter(variavel != "ano",
                !grepl("^Outros", nivel)) |>
  dplyr::group_by(area) |>
  dplyr::slice_max(abs(ame), n = 1) |>
  dplyr::ungroup() |>
  dplyr::mutate(pp = round(100 * ame, 2)) |>
  dplyr::select(area, n, variavel, nivel, pp)

writexl::write_xlsx(maiores, file.path(DIR_TABLES, "tab-10-maior-efeito-area.xlsx"))

# Figuras ----------------------------------------------------------------------

fig5 <- destaques |>
  ggplot2::ggplot(ggplot2::aes(x = stats::reorder(area, ame), y = ame)) +
  ggplot2::geom_hline(yintercept = 0, colour = "grey50", linetype = "dashed") +
  ggplot2::geom_point(size = 2.6, colour = "steelblue") +
  ggplot2::geom_segment(ggplot2::aes(xend = area, y = 0, yend = ame),
                        colour = "steelblue", linewidth = 0.7) +
  ggplot2::facet_wrap(~ efeito, ncol = 4, scales = "free_x") +
  ggplot2::scale_y_continuous(labels = scales::percent) +
  ggplot2::coord_flip() +
  ggplot2::labs(x = NULL,
                y = "Efeito marginal médio sobre a probabilidade de fazer IC") +
  TEMA + ggplot2::theme(legend.position = "none")

ggplot2::ggsave(file.path(DIR_FIGURES, "fig-5-heterogeneidade.png"),
                fig5, width = 12, height = 6.5, dpi = 300)

# Trabalho contra origem social: em quais areas o trabalho domina?
confronto <- tab_destaques |>
  dplyr::mutate(
    trabalho = abs(`Trabalha em tempo integral`),
    origem   = pmax(abs(`Raça: PPI`), abs(`Renda per capita`),
                    abs(`Responsável com superior`))
  )

fig6 <- confronto |>
  ggplot2::ggplot(ggplot2::aes(x = origem, y = trabalho, label = area)) +
  ggplot2::geom_abline(slope = 1, intercept = 0, linetype = "dashed",
                       colour = "grey50") +
  ggplot2::geom_point(size = 3, colour = "steelblue") +
  ggplot2::geom_text(hjust = -0.08, size = 3.2) +
  ggplot2::expand_limits(x = c(0, max(confronto$origem) * 1.9),
                         y = c(0, max(confronto$trabalho) * 1.1)) +
  ggplot2::labs(
    x = "Maior efeito entre as características de origem (pp, valor absoluto)",
    y = "Efeito de trabalhar em tempo integral (pp, valor absoluto)"
  ) +
  TEMA

ggplot2::ggsave(file.path(DIR_FIGURES, "fig-6-trabalho-vs-origem.png"),
                fig6, width = 8.5, height = 6, dpi = 300)

# Relato ------------------------------------------------------------------------

message("\n--- Efeitos marginais por area, em pontos percentuais ---")
print(as.data.frame(tab_destaques), row.names = FALSE)

message("\n--- O maior efeito de cada area (excluindo o ano de ingresso) ---")
print(as.data.frame(maiores), row.names = FALSE)

message("\n--- O trabalho supera a origem social em quantas areas? ---")
message(sprintf("  %d de %d", sum(confronto$trabalho > confronto$origem),
                nrow(confronto)))
