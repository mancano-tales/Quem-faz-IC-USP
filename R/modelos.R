# ------------------------------------------------------------------------------
# modelos.R -- especificacao do modelo e efeitos marginais medios
#
# Mantemos a especificacao do artigo da FFLCH intacta. E o que permite dizer se
# a USP em agregado se comporta como a FFLCH: se mudassemos o modelo junto com
# a amostra, nao saberiamos qual das duas mudancas explica a diferenca.
# ------------------------------------------------------------------------------

# Especificacao do artigo original (Mancano & Alcantara, FFLCH).
FORMULA_BASE <- IC ~ ano + idade + sexo + raca + educ_resp + sfmpct +
  trabalho + sustento + periodo + class_carreira

# A versao para a USP inteira acrescenta efeitos fixos de area: sem eles, o
# coeficiente de qualquer variavel confunde o efeito individual com o fato de
# certas areas ofertarem muito mais IC do que outras.
FORMULA_AREA <- update(FORMULA_BASE, . ~ . + area)

VARIAVEIS <- c("IC", "ano", "idade", "sexo", "raca", "educ_resp", "sfmpct",
               "trabalho", "sustento", "periodo", "class_carreira")

# Prepara a amostra de estimacao: seleciona as variaveis do modelo e descarta
# quem tem resposta faltante em qualquer uma delas.
amostra_modelo <- function(dados, com_area = FALSE) {
  v <- if (com_area) c(VARIAVEIS, "area") else VARIAVEIS
  dados |>
    dplyr::select(dplyr::all_of(v)) |>
    stats::na.exclude()
}

ajustar <- function(dados, formula = FORMULA_BASE) {
  stats::glm(formula, data = dados, family = stats::binomial)
}

# Efeito marginal medio ---------------------------------------------------------
#
# Coeficientes logisticos de variaveis em escalas diferentes -- reais por pessoa,
# anos de idade, categorias de trabalho -- nao sao comparaveis entre si. O AME
# poe todos na mesma unidade: pontos percentuais na probabilidade de fazer IC.
# E o que permite responder "o trabalho continua sendo o maior efeito?".
#
# Para variaveis categoricas, contrasta cada nivel contra a categoria de
# referencia mantendo o resto como esta. Para continuas, usa uma diferenca
# finita de um desvio-padrao.
ame <- function(modelo, dados) {
  termos <- attr(stats::terms(modelo), "term.labels")
  base_p <- stats::predict(modelo, dados, type = "response")

  purrr_bind <- function(x) do.call(rbind, x)

  purrr_bind(lapply(termos, function(v) {
    if (!v %in% names(dados)) return(NULL)
    x <- dados[[v]]

    if (is.numeric(x)) {
      d <- dados
      passo <- stats::sd(x, na.rm = TRUE)
      d[[v]] <- x + passo
      efeito <- mean(stats::predict(modelo, d, type = "response") - base_p)
      # O rotulo nao carrega o valor do desvio-padrao: ele muda de amostra para
      # amostra, e carrega-lo impediria alinhar os modelos lado a lado.
      data.frame(variavel = v, nivel = "+1 desvio-padrão",
                 passo = passo, ame = efeito)
    } else {
      f <- factor(x)
      ref <- levels(f)[1]
      niveis <- levels(f)[-1]
      d0 <- dados; d0[[v]] <- factor(ref, levels = levels(f))
      p0 <- stats::predict(modelo, d0, type = "response")

      purrr_bind(lapply(niveis, function(nv) {
        d1 <- dados; d1[[v]] <- factor(nv, levels = levels(f))
        p1 <- stats::predict(modelo, d1, type = "response")
        data.frame(variavel = v, nivel = sprintf("%s (vs %s)", nv, ref),
                   passo = NA_real_, ame = mean(p1 - p0))
      }))
    }
  }))
}

# Rotulos legiveis para as tabelas e figuras.
ROTULOS <- c(
  ano                                       = "Ano de ingresso",
  idade                                     = "Idade no vestibular",
  sexoM                                     = "Sexo: masculino",
  racaPPI                                   = "Raça: PPI",
  `educ_respEM completo`                    = "Responsável: médio completo",
  `educ_respES completo`                    = "Responsável: superior completo",
  sfmpct                                    = "Renda per capita (SM)",
  `trabalhoSim, em tempo integral`          = "Trabalha em tempo integral",
  `trabalhoSim, em tempo parcial`           = "Trabalha em tempo parcial",
  `sustentoPor conta própria`               = "Sustento: por conta própria",
  `sustentoCom bolsa e apoio da família`    = "Sustento: bolsa e apoio familiar",
  `sustentoCom trabalho e apoio da família` = "Sustento: trabalho e apoio familiar",
  sustentoOutros                            = "Sustento: outros",
  periodonoturno                            = "Período noturno",
  class_carreira                            = "Classificação na carreira"
)
