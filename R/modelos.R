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
#
# O erro-padrao vem do metodo delta. O AME e a media, sobre os individuos, da
# diferenca entre duas probabilidades preditas:
#
#   AME(b) = (1/n) * sum_i [ g(x1_i'b) - g(x0_i'b) ],   g = logistica
#
# cujo gradiente em relacao a b e
#
#   grad = (1/n) * sum_i [ p1_i(1-p1_i) x1_i - p0_i(1-p0_i) x0_i ],
#
# e dai Var(AME) = grad' V grad, com V a matriz de covariancia do modelo. E
# exato em primeira ordem, custa uma passada pelos dados, e evita o bootstrap,
# que com 111 mil observacoes e vinte contrastes seria proibitivo.
ame <- function(modelo, dados, nivel_confianca = 0.95) {
  termos <- attr(stats::terms(modelo), "term.labels")
  V      <- stats::vcov(modelo)
  z      <- stats::qnorm(1 - (1 - nivel_confianca) / 2)

  # Matriz de desenho de um cenario contrafactual, na parametrizacao do modelo.
  desenho <- function(d) {
    stats::model.matrix(stats::delete.response(stats::terms(modelo)), data = d,
                        xlev = modelo$xlevels)
  }

  contraste <- function(d0, d1, variavel, rotulo, passo = NA_real_) {
    X0 <- desenho(d0); X1 <- desenho(d1)
    b  <- stats::coef(modelo)
    p0 <- stats::plogis(as.vector(X0 %*% b))
    p1 <- stats::plogis(as.vector(X1 %*% b))

    efeito <- mean(p1 - p0)
    grad   <- colMeans(p1 * (1 - p1) * X1 - p0 * (1 - p0) * X0)
    ep     <- sqrt(as.numeric(t(grad) %*% V %*% grad))

    data.frame(variavel = variavel, nivel = rotulo, passo = passo,
               ame = efeito, ep = ep,
               inf = efeito - z * ep, sup = efeito + z * ep,
               p = 2 * stats::pnorm(-abs(efeito / ep)))
  }

  do.call(rbind, lapply(termos, function(v) {
    if (!v %in% names(dados)) return(NULL)
    x <- dados[[v]]

    if (is.numeric(x)) {
      passo <- stats::sd(x, na.rm = TRUE)
      d1 <- dados; d1[[v]] <- x + passo
      # O rotulo nao carrega o valor do desvio-padrao: ele muda de amostra para
      # amostra, e carrega-lo impediria alinhar os modelos lado a lado.
      contraste(dados, d1, v, "+1 desvio-padrão", passo)
    } else {
      f      <- factor(x)
      ref    <- levels(f)[1]
      d0 <- dados; d0[[v]] <- factor(ref, levels = levels(f))

      do.call(rbind, lapply(levels(f)[-1], function(nv) {
        d1 <- dados; d1[[v]] <- factor(nv, levels = levels(f))
        contraste(d0, d1, v, sprintf("%s (vs %s)", nv, ref))
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
