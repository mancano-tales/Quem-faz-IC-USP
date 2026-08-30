# ------------------------------------------------------------------------------
# 03-modelos.R -- a especificacao da FFLCH aplicada a USP inteira
#
# Tres modelos, na mesma especificacao, para separar tres perguntas:
#   (1) FFLCH        -- replica o artigo original, como marco de comparacao
#   (2) USP          -- a mesma especificacao na universidade inteira
#   (3) USP + area   -- acrescenta efeitos fixos de area do conhecimento
#
# A diferenca entre (1) e (2) diz se a FFLCH e um caso particular. A diferenca
# entre (2) e (3) diz quanto do efeito aparente de cada variavel era, na
# verdade, composicao: estudantes de certos perfis concentrados em areas que
# ofertam mais IC.
# ------------------------------------------------------------------------------

source(here::here("R", "setup.R"))
source(here::here("R", "recode.R"))
source(here::here("R", "modelos.R"))

usp <- arrow::read_parquet(ARQ_PAINEL) |>
  dplyr::filter(ano %in% COORTES) |>
  aplicar_recodificacoes() |>
  dplyr::mutate(IC = dplyr::coalesce(IC, 0)) |>
  dplyr::left_join(carregar_areas(), by = "unidade")

fflch <- dplyr::filter(usp, unidade == UNIDADE_FFLCH)

md_fflch <- amostra_modelo(fflch)
md_usp   <- amostra_modelo(usp, com_area = TRUE)

m_fflch <- ajustar(md_fflch, FORMULA_BASE)
m_usp   <- ajustar(md_usp,   FORMULA_BASE)
m_area  <- ajustar(md_usp,   FORMULA_AREA)

# Tabela de coeficientes -------------------------------------------------------

extrair <- function(m, nome) {
  co <- summary(m)$coefficients
  data.frame(
    termo   = rownames(co),
    modelo  = nome,
    coef    = co[, 1],
    ep      = co[, 2],
    p       = co[, 4],
    or      = exp(co[, 1]),
    row.names = NULL
  )
}

coefs <- rbind(extrair(m_fflch, "FFLCH"),
               extrair(m_usp,   "USP"),
               extrair(m_area,  "USP + área"))

writexl::write_xlsx(coefs, file.path(DIR_TABLES, "tab-5-coeficientes.xlsx"))

# Efeitos marginais medios -----------------------------------------------------
# Postos na mesma unidade (pontos percentuais), os efeitos de variaveis em
# escalas diferentes passam a ser comparaveis entre si.

ames <- rbind(
  cbind(modelo = "FFLCH",      ame(m_fflch, md_fflch)),
  cbind(modelo = "USP",        ame(m_usp,   md_usp)),
  cbind(modelo = "USP + área", ame(m_area,  md_usp))
)

writexl::write_xlsx(ames, file.path(DIR_TABLES, "tab-6-efeitos-marginais.xlsx"))

# Ajuste dos modelos -----------------------------------------------------------

ajuste <- data.frame(
  modelo = c("FFLCH", "USP", "USP + área"),
  n      = c(stats::nobs(m_fflch), stats::nobs(m_usp), stats::nobs(m_area)),
  aic    = c(stats::AIC(m_fflch), stats::AIC(m_usp), stats::AIC(m_area)),
  pseudo_r2 = vapply(list(m_fflch, m_usp, m_area), function(m) {
    nulo <- stats::glm(stats::update(stats::formula(m), . ~ 1),
                       data = m$model, family = stats::binomial)
    1 - as.numeric(stats::logLik(m) / stats::logLik(nulo))
  }, numeric(1))
)

writexl::write_xlsx(ajuste, file.path(DIR_TABLES, "tab-7-ajuste.xlsx"))

# Figura: os maiores efeitos marginais, USP contra FFLCH -----------------------

principais <- ames |>
  dplyr::filter(modelo != "USP") |>
  dplyr::mutate(rotulo = paste0(variavel, ": ", nivel)) |>
  dplyr::filter(!grepl("^area", variavel))

top <- principais |>
  dplyr::group_by(rotulo) |>
  dplyr::summarise(m = max(abs(ame)), .groups = "drop") |>
  dplyr::slice_max(m, n = 10) |>
  dplyr::pull(rotulo)

fig <- principais |>
  dplyr::filter(rotulo %in% top) |>
  ggplot2::ggplot(ggplot2::aes(x = stats::reorder(rotulo, ame), y = ame,
                               fill = modelo)) +
  ggplot2::geom_col(position = ggplot2::position_dodge(width = 0.75),
                    width = 0.7) +
  ggplot2::geom_hline(yintercept = 0, colour = "grey40") +
  ggplot2::scale_y_continuous(labels = scales::percent) +
  ggplot2::scale_fill_manual(values = c("FFLCH" = "#e97d5a",
                                        "USP + área" = "#1bb57f")) +
  ggplot2::coord_flip() +
  ggplot2::labs(x = NULL, y = "Efeito marginal médio sobre a probabilidade de fazer IC",
                fill = NULL) +
  TEMA

ggplot2::ggsave(file.path(DIR_FIGURES, "fig-4-ame-usp-vs-fflch.png"),
                fig, width = 9, height = 5.5, dpi = 300)

# Relato ------------------------------------------------------------------------

message("\n--- Ajuste ---")
print(ajuste |> dplyr::mutate(aic = round(aic, 1), pseudo_r2 = round(pseudo_r2, 4)),
      row.names = FALSE)

message("\n--- Efeitos marginais medios, em pontos percentuais ---")
comparar <- ames |>
  dplyr::filter(!grepl("^area", variavel)) |>
  dplyr::mutate(pp = round(100 * ame, 2)) |>
  dplyr::select(variavel, nivel, modelo, pp) |>
  tidyr::pivot_wider(names_from = modelo, values_from = pp)
print(as.data.frame(comparar), row.names = FALSE)

message("\n--- O maior efeito de cada amostra ---")
for (mm in unique(ames$modelo)) {
  a <- ames |> dplyr::filter(modelo == mm, !grepl("^area|^ano", variavel))
  i <- which.max(abs(a$ame))
  message(sprintf("  %-11s %s: %s -> %+.2f pp", mm, a$variavel[i], a$nivel[i],
                  100 * a$ame[i]))
}
