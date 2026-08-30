# ------------------------------------------------------------------------------
# 06-replica-fflch.R -- o aparato descritivo do artigo da FFLCH, agora na USP
#
# O artigo anterior descreveu quem faz IC na FFLCH atraves de nove tabelas e
# duas figuras. Reproduzi-las na universidade inteira serve a duas coisas: da
# ao leitor a mesma base descritiva com que o argumento anterior foi construido,
# e permite ver, item a item, onde a USP se afasta da FFLCH.
#
# Entrada: data/ic_usp.parquet, data-raw/sic-usp-243681-perfil-ic.xlsx
# Saida  : outputs/tables/rep-*, outputs/figures/rep-*
# ------------------------------------------------------------------------------

source(here::here("R", "setup.R"))
source(here::here("R", "recode.R"))
source(here::here("R", "modelos.R"))

usp <- arrow::read_parquet(ARQ_PAINEL) |>
  dplyr::filter(ano %in% COORTES_MODELO) |>
  aplicar_recodificacoes() |>
  dplyr::mutate(IC = dplyr::coalesce(IC, 0)) |>
  dplyr::left_join(carregar_areas(), by = "unidade")

fflch <- dplyr::filter(usp, unidade == UNIDADE_FFLCH)
pct <- function(x, a = 0.1) scales::percent(x, accuracy = a, decimal.mark = ",")

# Acesso a IC por coorte -------------------------------------------------------
# Na FFLCH a serie ia de 9,1% (2010) a 15,3% (2018). Aqui, com o mesmo calculo,
# vemos se a expansao da politica no periodo foi um fenomeno da unidade ou da
# universidade.

por_coorte <- dplyr::bind_rows(
  usp   |> dplyr::group_by(ano) |>
    dplyr::summarise(n = dplyr::n(), fez = sum(IC), .groups = "drop") |>
    dplyr::mutate(recorte = "USP"),
  fflch |> dplyr::group_by(ano) |>
    dplyr::summarise(n = dplyr::n(), fez = sum(IC), .groups = "drop") |>
    dplyr::mutate(recorte = "FFLCH")
) |>
  dplyr::mutate(prop = fez / n)

writexl::write_xlsx(por_coorte, file.path(DIR_TABLES, "rep-1-coortes.xlsx"))

# Taxa de resposta do questionario socioeconomico ------------------------------

questoes_qase <- c(
  estado_civil    = "Estado civil",
  ef1             = "Onde cursou o ensino fundamental",
  ef2             = "Onde cursou o ensino médio",
  em              = "Tipo de ensino médio concluído",
  rfm             = "Renda familiar mensal",
  pessoas_contrib = "Pessoas que contribuem para a renda",
  pessoas_resid   = "Pessoas que vivem da renda",
  atv_remu        = "Exerce atividade remunerada",
  educ_resp1      = "Instrução do responsável 1",
  educ_resp2      = "Instrução do responsável 2",
  ocup_resp1      = "Ocupação do principal contribuinte",
  pretensao_mant  = "Como pretende se manter",
  inclusp         = "Participação no INCLUSP"
)

taxa <- function(d) vapply(names(questoes_qase),
                           function(v) mean(!is.na(d[[v]])), numeric(1))

resposta_qase <- data.frame(
  questao = unname(questoes_qase),
  usp     = taxa(usp),
  fflch   = taxa(fflch),
  row.names = NULL
)

writexl::write_xlsx(resposta_qase, file.path(DIR_TABLES, "rep-2-qase.xlsx"))

# Tipo de fomento --------------------------------------------------------------
# Lido do arquivo bruto, e nao das colunas do painel, para conseguir informar
# tambem os projetos sem fomento registrado.

atena <- readxl::read_excel(ARQ_ATENA, sheet = "BD")

fomento <- atena |>
  dplyr::filter(Identificador %in% usp$id) |>
  dplyr::count(tipo = TipoFomento, sort = TRUE) |>
  dplyr::mutate(tipo = dplyr::coalesce(tipo, "Sem informação sobre fomento"),
                prop = n / sum(n))

writexl::write_xlsx(fomento, file.path(DIR_TABLES, "rep-3-fomento.xlsx"))

# Perfil de quem faz IC --------------------------------------------------------

faixa_etaria <- function(idade) {
  cut(idade, breaks = c(-Inf, 18, 19, 21, 24, Inf),
      labels = c("18 anos ou menos", "19 anos", "20 e 21 anos",
                 "22 a 24 anos", "25 anos ou mais"))
}

# O artigo anterior distingue quatro niveis de trabalho, separando o eventual do
# parcial -- um corte mais fino do que o usado no modelo.
trabalho4 <- function(a) {
  dplyr::case_when(
    a == "Não" ~ "Não",
    a == "Sim, eventualmente" ~ "Eventualmente",
    a %in% c("Sim, em meio período (até 20 horas semanais)",
             "Sim, em tempo semi-integral (de 21 a 32 horas semanais)",
             "Sim, regularmente, em tempo parcial") ~ "Em tempo parcial",
    a == "Sim, regularmente, em tempo integral" ~ "Em tempo integral"
  )
}
NIVEIS_TRAB <- c("Não", "Eventualmente", "Em tempo parcial", "Em tempo integral")

# Um resumo por grupo, aplicado a USP e a FFLCH para permitir a comparacao.
perfil <- function(variavel, rotulo) {
  f <- function(d, nome) {
    d |>
      dplyr::filter(!is.na(.data[[variavel]])) |>
      dplyr::group_by(grupo = .data[[variavel]]) |>
      dplyr::summarise(n = dplyr::n(), fez = mean(IC), .groups = "drop") |>
      dplyr::mutate(share = n / sum(n), recorte = nome, dimensao = rotulo)
  }
  dplyr::bind_rows(f(usp, "USP"), f(fflch, "FFLCH"))
}

usp   <- dplyr::mutate(usp,   fx = faixa_etaria(idade), tr = factor(trabalho4(atv_remu), NIVEIS_TRAB))
fflch <- dplyr::mutate(fflch, fx = faixa_etaria(idade), tr = factor(trabalho4(atv_remu), NIVEIS_TRAB))

perfis <- dplyr::bind_rows(
  perfil("fx", "Faixa etária no vestibular"),
  perfil("tr", "Atividade remunerada"),
  perfil("pretensao_mant", "Pretensão de sustento"),
  perfil("raca", "Raça/cor"),
  perfil("periodo", "Período do curso")
)

writexl::write_xlsx(perfis, file.path(DIR_TABLES, "rep-4-perfis.xlsx"))

# Cruzamento entre trabalho e idade --------------------------------------------

cruz <- usp |>
  dplyr::filter(!is.na(tr), !is.na(fx)) |>
  dplyr::count(fx, tr) |>
  dplyr::group_by(tr) |>
  dplyr::mutate(prop = n / sum(n)) |>
  dplyr::ungroup()

writexl::write_xlsx(cruz, file.path(DIR_TABLES, "rep-5-trabalho-idade.xlsx"))

# Raca por coorte --------------------------------------------------------------

raca_coorte <- usp |>
  dplyr::filter(!is.na(raca)) |>
  dplyr::group_by(ano, raca) |>
  dplyr::summarise(n = dplyr::n(), fez = mean(IC), .groups = "drop")

writexl::write_xlsx(raca_coorte, file.path(DIR_TABLES, "rep-6-raca-coorte.xlsx"))

# Figuras: densidade das probabilidades preditas -------------------------------
# As duas figuras do artigo anterior, agora sobre o modelo da USP.

md <- amostra_modelo(usp, com_area = TRUE)
m  <- ajustar(md, FORMULA_AREA)
md$p <- stats::predict(m, md, type = "response")

tema_dens <- ggplot2::theme_bw(base_size = 13) +
  ggplot2::theme(legend.position = "bottom")

cores_trab <- c("Sim, em tempo integral" = "#e97d5a",
                "Sim, em tempo parcial"  = "#a3a500",
                "Não"                    = "#1bb57f")

f1 <- md |>
  dplyr::mutate(trabalho = factor(trabalho, levels = names(cores_trab))) |>
  ggplot2::ggplot(ggplot2::aes(x = p, colour = trabalho, fill = trabalho)) +
  ggplot2::geom_density(linewidth = 1.1, alpha = 0.5) +
  ggplot2::scale_fill_manual(values = cores_trab) +
  ggplot2::scale_colour_manual(values = cores_trab) +
  ggplot2::scale_x_continuous(limits = c(0, 0.6), expand = c(0.001, 0.001)) +
  ggplot2::scale_y_continuous(expand = c(0.001, 0.001)) +
  ggplot2::labs(x = "Probabilidades preditas", y = "Densidade",
                colour = "Trabalho", fill = "Trabalho") +
  tema_dens

ggplot2::ggsave(file.path(DIR_FIGURES, "rep-1-densidade-trabalho.png"),
                f1, width = 9, height = 5, dpi = 300)

cores_sust <- c("Por conta própria"               = "#e97d5a",
                "Com trabalho e apoio da família" = "#00aff6",
                "Suporte da família"              = "#a3a500",
                "Com bolsa e apoio da família"    = "#1bb57f")

f2 <- md |>
  dplyr::filter(sustento != "Outros") |>
  dplyr::mutate(sustento = factor(as.character(sustento), names(cores_sust))) |>
  ggplot2::ggplot(ggplot2::aes(x = p, colour = sustento, fill = sustento)) +
  ggplot2::geom_density(linewidth = 1.1, alpha = 0.5) +
  ggplot2::scale_fill_manual(values = cores_sust) +
  ggplot2::scale_colour_manual(values = cores_sust) +
  ggplot2::scale_x_continuous(limits = c(0, 0.6), expand = c(0.001, 0.001)) +
  ggplot2::scale_y_continuous(expand = c(0.001, 0.001)) +
  ggplot2::labs(x = "Probabilidades preditas", y = "Densidade",
                colour = "Sustento", fill = "Sustento") +
  tema_dens

ggplot2::ggsave(file.path(DIR_FIGURES, "rep-2-densidade-sustento.png"),
                f2, width = 9, height = 5, dpi = 300)

# Relato ------------------------------------------------------------------------

message("\n--- Acesso a IC por coorte: USP x FFLCH ---")
print(as.data.frame(por_coorte |>
        dplyr::select(ano, recorte, prop) |>
        tidyr::pivot_wider(names_from = recorte, values_from = prop) |>
        dplyr::mutate(dplyr::across(-ano, pct))), row.names = FALSE)

message("\n--- Taxa de resposta do QASE ---")
print(as.data.frame(resposta_qase |>
        dplyr::transmute(questao, USP = pct(usp, 0.01), FFLCH = pct(fflch, 0.01))),
      row.names = FALSE, right = FALSE)

message("\n--- Tipo de fomento na USP (", sum(fomento$n), " projetos) ---")
print(as.data.frame(fomento |> dplyr::transmute(tipo, n, `%` = pct(prop, 0.01))),
      row.names = FALSE)

message("\n--- Perfil: proporcao que fez IC em cada grupo ---")
print(as.data.frame(perfis |>
        dplyr::filter(dimensao %in% c("Faixa etária no vestibular",
                                      "Atividade remunerada")) |>
        dplyr::select(dimensao, grupo, recorte, fez) |>
        tidyr::pivot_wider(names_from = recorte, values_from = fez) |>
        dplyr::mutate(dplyr::across(c(USP, FFLCH), pct))), row.names = FALSE)
