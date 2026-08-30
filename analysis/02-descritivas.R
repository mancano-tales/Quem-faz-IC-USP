# ------------------------------------------------------------------------------
# 02-descritivas.R -- quem entra e quem faz IC, por area e por unidade
#
# Entrada: data/ic_usp.parquet, data-raw/unidades-areas.csv
# Saida  : outputs/tables/tab-*, outputs/figures/fig-*
# ------------------------------------------------------------------------------

source(here::here("R", "setup.R"))
source(here::here("R", "recode.R"))

usp <- arrow::read_parquet(ARQ_PAINEL) |>
  dplyr::filter(ano %in% COORTES) |>
  aplicar_recodificacoes() |>
  dplyr::mutate(IC = dplyr::coalesce(IC, 0)) |>
  dplyr::left_join(carregar_areas(), by = "unidade")

stopifnot("toda unidade deve ter area" = !anyNA(usp$area))

usp <- ordenar_areas(usp)
pct <- function(x) scales::percent(x, accuracy = 0.1, decimal.mark = ",")

# Tabela 1 -- acesso a IC e composicao social, por area ------------------------
# As duas metades da tabela respondem a perguntas diferentes: a primeira, quanta
# IC cada area oferece; a segunda, quem sao os estudantes de cada uma. Sem a
# segunda, nao da para saber se a diferenca de acesso vem da area ou do perfil
# de quem entra nela.

tab_areas <- usp |>
  dplyr::group_by(area) |>
  dplyr::summarise(
    n            = dplyr::n(),
    fez_ic       = mean(IC),
    projetos     = sum(qtd_ic, na.rm = TRUE),
    ppi          = mean(raca == "PPI", na.rm = TRUE),
    noturno      = mean(periodo == "noturno", na.rm = TRUE),
    trabalha     = mean(trabalho != "Não", na.rm = TRUE),
    conta_propria = mean(sustento == "Por conta própria", na.rm = TRUE),
    renda_pc     = stats::median(sfmpct, na.rm = TRUE),
    idade        = stats::median(idade, na.rm = TRUE),
    .groups      = "drop"
  )

writexl::write_xlsx(tab_areas, file.path(DIR_TABLES, "tab-1-areas.xlsx"))

# Tabela 2 -- acesso a IC por unidade ------------------------------------------

tab_unidades <- usp |>
  dplyr::group_by(area, unidade) |>
  dplyr::summarise(n = dplyr::n(), fez_ic = mean(IC), .groups = "drop") |>
  dplyr::filter(n >= MIN_INGRESSANTES) |>
  dplyr::arrange(dplyr::desc(fez_ic))

writexl::write_xlsx(tab_unidades, file.path(DIR_TABLES, "tab-2-unidades.xlsx"))

# Tabela 3 -- evolucao do acesso por coorte e area -----------------------------

tab_coortes <- usp |>
  dplyr::group_by(area, ano) |>
  dplyr::summarise(n = dplyr::n(), fez_ic = mean(IC), .groups = "drop")

writexl::write_xlsx(tab_coortes, file.path(DIR_TABLES, "tab-3-coortes-area.xlsx"))

# Quanto da variacao esta entre areas e quanto esta dentro delas? --------------
# Um R2 de um modelo so com efeitos fixos de area da a fracao da variacao
# individual em fazer IC que a area sozinha explica.

m_area <- stats::glm(IC ~ area, data = usp, family = stats::binomial)
m_nulo <- stats::glm(IC ~ 1,    data = usp, family = stats::binomial)
pseudo_r2_area <- 1 - as.numeric(stats::logLik(m_area) / stats::logLik(m_nulo))

m_unid <- stats::glm(IC ~ unidade, data = usp, family = stats::binomial)
pseudo_r2_unid <- 1 - as.numeric(stats::logLik(m_unid) / stats::logLik(m_nulo))

decomposicao <- data.frame(
  fonte = c("Área do conhecimento (9 categorias)",
            "Unidade da USP (48 categorias)"),
  pseudo_r2 = c(pseudo_r2_area, pseudo_r2_unid)
)
writexl::write_xlsx(decomposicao, file.path(DIR_TABLES, "tab-4-decomposicao.xlsx"))

# Figuras ----------------------------------------------------------------------

fig1 <- tab_areas |>
  ggplot2::ggplot(ggplot2::aes(x = stats::reorder(area, fez_ic), y = fez_ic)) +
  ggplot2::geom_col(fill = "steelblue", width = 0.7) +
  ggplot2::geom_text(ggplot2::aes(label = pct(fez_ic)), hjust = -0.15, size = 3.5) +
  ggplot2::scale_y_continuous(labels = scales::percent,
                              expand = ggplot2::expansion(mult = c(0, 0.15))) +
  ggplot2::coord_flip() +
  ggplot2::labs(x = NULL, y = "Estudantes que realizaram IC") +
  TEMA

ggplot2::ggsave(file.path(DIR_FIGURES, "fig-1-ic-por-area.png"),
                fig1, width = 8, height = 5, dpi = 300)

fig2 <- tab_coortes |>
  ggplot2::ggplot(ggplot2::aes(x = ano, y = fez_ic, colour = area)) +
  ggplot2::geom_line(linewidth = 0.9) +
  ggplot2::geom_point(size = 1.4) +
  ggplot2::scale_y_continuous(labels = scales::percent) +
  ggplot2::scale_x_continuous(breaks = seq(2010, 2022, 3)) +
  ggplot2::labs(x = "Coorte de ingresso", y = "Estudantes que realizaram IC",
                colour = NULL) +
  TEMA + ggplot2::guides(colour = ggplot2::guide_legend(nrow = 3))

ggplot2::ggsave(file.path(DIR_FIGURES, "fig-2-coortes-area.png"),
                fig2, width = 9, height = 5.5, dpi = 300)

# Dispersao entre unidades: a area explica pouco se as unidades de uma mesma
# area estiverem espalhadas.
fig3 <- tab_unidades |>
  ggplot2::ggplot(ggplot2::aes(x = area, y = fez_ic)) +
  ggplot2::geom_boxplot(outlier.shape = NA, colour = "grey60") +
  ggplot2::geom_jitter(ggplot2::aes(size = n), width = 0.15, alpha = 0.6,
                       colour = "steelblue") +
  ggplot2::scale_y_continuous(labels = scales::percent) +
  ggplot2::scale_size_continuous(range = c(1, 6), guide = "none") +
  ggplot2::coord_flip() +
  ggplot2::labs(x = NULL, y = "Estudantes que realizaram IC, por unidade") +
  TEMA

ggplot2::ggsave(file.path(DIR_FIGURES, "fig-3-dispersao-unidades.png"),
                fig3, width = 8, height = 5, dpi = 300)

# Relato ------------------------------------------------------------------------

message("\n--- Acesso a IC por area (coortes ", min(COORTES), "-", max(COORTES), ") ---")
print(as.data.frame(tab_areas |> dplyr::transmute(
  area, n, `% IC` = pct(fez_ic), `% PPI` = pct(ppi), `% noturno` = pct(noturno),
  `% trabalha` = pct(trabalha), `renda pc (SM)` = round(renda_pc, 2))),
  row.names = FALSE)

message("\n--- Amplitude entre unidades (n >= ", MIN_INGRESSANTES, ") ---")
message(sprintf("  maior: %s (%s)", tab_unidades$unidade[1], pct(tab_unidades$fez_ic[1])))
u <- utils::tail(tab_unidades, 1)
message(sprintf("  menor: %s (%s)", u$unidade, pct(u$fez_ic)))

message("\n--- Quanto a estrutura institucional explica ---")
message(sprintf("  pseudo-R2 so com area   : %.3f", pseudo_r2_area))
message(sprintf("  pseudo-R2 so com unidade: %.3f", pseudo_r2_unid))

stopifnot(
  "a USP deve ter 152.729 ingressantes nas coortes 2010-2022" = nrow(usp) == 152729,
  "as 48 unidades devem estar classificadas em 9 areas" =
    dplyr::n_distinct(usp$area) == 9
)
