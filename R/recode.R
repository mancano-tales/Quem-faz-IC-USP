# ------------------------------------------------------------------------------
# recode.R -- recodificacao das variaveis do questionario socioeconomico (QASE)
#
# As faixas de resposta do QASE/FUVEST mudam de redacao a cada ano (os valores em
# reais sao atualizados pelo salario minimo vigente). As funcoes abaixo mapeiam
# todas as redacoes usadas entre 2010 e 2022 para categorias comparaveis.
#
# Nos scripts originais essa recodificacao estava copiada em quatro arquivos;
# aqui ela existe uma vez so, e e a mesma para o artigo e para os suplementos.
# ------------------------------------------------------------------------------

# Renda familiar mensal em salarios minimos (ponto medio da faixa).
# Item 17 do QASE.
recode_renda_sm <- function(rfm) {
  dplyr::case_when(
    rfm %in% c("Até 1 SM - até R$ 1.045,00",
               "Até 1 SM - até R$ 1.100,00",
               "Até 1 SM - até R$ 1.212,00",
               "Inferior a 1 SM.") ~ 0.5,

    rfm %in% c("Acima de 1 até 2 SM - de R$ 1.045,01 até R$ 2.090,00",
               "Acima de 1 até 2 SM - de R$ 1.100,01 até R$ 2.200,00",
               "Acima de 1 até 2 SM - de R$ 1.212,01 até R$ 2.424,00",
               "De 1 a 1,9 SM.") ~ 1.5,

    rfm %in% c("Acima de 2 até 3 SM - de R$ 2.090,01 até R$ 3.135,00",
               "Acima de 2 até 3 SM - de R$ 2.200,01 até R$ 3.300,00",
               "Acima de 2 até 3 SM - de R$ 2.424,01 até R$ 3.636,00",
               "De 2 a 2,9 SM.") ~ 2.5,

    rfm %in% c("Acima de 3 até 5 SM - de R$ 3.135,01 até R$ 5.225,00",
               "Acima de 3 até 5 SM - de R$ 3.300,01 até R$ 5.500,00",
               "Acima de 3 até 5 SM - de R$ 3.636,01 até R$ 6.060,00",
               "De 3 a 4,9 SM.") ~ 3.5,

    rfm %in% c("Acima de 5 até 7 SM - de R$ 5.225,01 até R$ 7.315,00",
               "Acima de 5 até 7 SM - de R$ 5.500,01 até R$ 7.700,00",
               "Acima de 5 até 7 SM - de R$ 6.060,01 até R$ 8.484,00",
               "De 5 a 6,9 SM.") ~ 6,

    rfm %in% c("Acima de 7 até 10 SM - de R$ 7.315,01 até R$ 10.450,00",
               "Acima de 7 até 10 SM - de R$ 7.700,01 até R$ 11.000,00",
               "Acima de 7 até 10 SM - de R$ 8.484,01 até R$ 12.120,00",
               "De 7 a 9,9 SM.") ~ 8.5,

    rfm %in% c("Acima de 10 até 15 SM - de R$ 10.450,01 até R$ 15.675,00",
               "Acima de 10 até 15 SM - de R$ 11.000,01 até R$ 16.500,00",
               "Acima de 10 até 15 SM - de R$ 12.120,01 até R$ 18.180,00",
               "Acima de 15 até 20 SM - de R$ 15.675,01 até R$ 20.900,00",
               "Acima de 15 até 20 SM - de R$ 16.500,01 até R$ 22.000,00",
               "Acima de 15 até 20 SM - de R$ 18.180,01 até R$ 24.240,00",
               "De 10 a 14,9 SM.", "De 10 a 13,9 SM.",
               "De 14 a 19,9 SM.", "De 15 a 19,9 SM.") ~ 15,

    rfm %in% c("Acima de 20 SM até 30 SM - de R$ 20.900,01 até R$ 31.350,00",
               "Acima de 20 SM até 30 SM - de R$ 22.000,01 até R$ 33.000,00",
               "Acima de 20 SM até 30 SM - de R$ 24.240,01 até R$ 36.360,00",
               "Acima de 30 SM até 50 SM - de R$ 31.350,01 até R$ 52.250,00",
               "Acima de 30 SM até 50 SM - de R$ 33.000,01 até R$ 55.000,00",
               "Acima de 30 SM até 50 SM - de R$ 36.360,01 até R$ 60.600,00",
               "Acima de 50 SM - superior a R$ 52.250,00",
               "Acima de 50 SM - superior a R$ 55.000,00",
               "Acima de 50 SM - superior a R$ 60.600,00",
               "Igual ou superior a 20 SM.") ~ 25
  )
}

# Numero de pessoas que vivem da renda familiar. Item 19 do QASE.
recode_pessoas_resid <- function(pessoas_resid) {
  dplyr::case_when(
    pessoas_resid == "Uma"    ~ 1,
    pessoas_resid == "Duas"   ~ 2,
    pessoas_resid == "Três"   ~ 3,
    pessoas_resid == "Quatro" ~ 4,
    pessoas_resid == "Cinco"  ~ 5,
    pessoas_resid %in% c("Seis", "Seis ou mais", "Sete", "Oito ou mais") ~ 6
  )
}

# Periodo do curso, colapsado em diurno x noturno.
recode_periodo <- function(periodo) {
  dplyr::case_when(
    periodo %in% c("diurno", "integral", "matutino", "vespertino") ~ "diurno",
    periodo == "noturno" ~ "noturno"
  )
}

# Escolaridade do responsavel 1, em tres niveis. Item 21 do QASE.
recode_educ_resp <- function(educ_resp1) {
  dplyr::case_when(
    educ_resp1 %in% c("Não estudou",
                      "Ensino fundamental incompleto",
                      "Ensino fundamental completo",
                      "Ensino médio incompleto",
                      "Iniciou o Ensino Fundamental, mas abandonou entre a 1ª e a 4ª",
                      "Iniciou o Ensino Fundamental, mas abandonou entre a 5ª e a 8ª")
      ~ "EF incompleto",

    educ_resp1 %in% c("Ensino médio completo",
                      "Ensino superior incompleto")
      ~ "EM completo",

    educ_resp1 %in% c("Ensino superior completo",
                      "Mestrado ou doutorado",
                      "Pós-Graduação completa",
                      "Pós-Graduação incompleta")
      ~ "ES completo"
  )
}

# Raca/cor colapsada em PPI (pretos, pardos e indigenas) x Brancos, seguindo a
# categoria usada nas politicas de acao afirmativa. "Amarela" e agrupada com
# "Branca", como no script original dos autores.
recode_raca <- function(raca) {
  dplyr::case_when(
    raca %in% c("Parda", "Preta / negra", "Indígena") ~ "PPI",
    raca %in% c("Amarela", "Branca") ~ "Brancos"
  )
}

# Exercicio de atividade remunerada, em tres niveis. Item 20 do QASE.
recode_trabalho <- function(atv_remu) {
  dplyr::case_when(
    atv_remu %in% c("Sim, em meio período (até 20 horas semanais)",
                    "Sim, em tempo semi-integral (de 21 a 32 horas semanais)",
                    "Sim, eventualmente",
                    "Sim, regularmente, em tempo parcial") ~ "Sim, em tempo parcial",

    atv_remu == "Sim, regularmente, em tempo integral" ~ "Sim, em tempo integral",

    atv_remu == "Não" ~ "Não"
  )
}

# Forma pretendida de sustento durante a graduacao. Item 24 do QASE.
recode_sustento <- function(pretensao_mant) {
  dplyr::case_when(
    pretensao_mant %in% c("Com bolsa de estudos",
                          "Com bolsa, trabalhando e contando, ainda, com o apoio da família")
      ~ "Com bolsa e apoio da família",

    pretensao_mant == "Trabalhando para participar do rateio das despesas da família"
      ~ "Com trabalho e apoio da família",

    pretensao_mant == "Por conta própria, com recursos oriundos de trabalho remunerado"
      ~ "Por conta própria",

    pretensao_mant %in% c("Somente com recursos dos pais",
                          "Trabalhando, mas contando, para o essencial, com os recursos da família")
      ~ "Suporte da família",

    pretensao_mant == "Outros" ~ "Outros"
  )
}

# Niveis do fator `sustento` nos modelos logisticos. O primeiro elemento
# ("Suporte da familia") e a categoria de referencia usada no artigo.
NIVEIS_SUSTENTO <- c("Suporte da família", "Por conta própria",
                     "Com bolsa e apoio da família",
                     "Com trabalho e apoio da família", "Outros")

# Aplica de uma vez todas as recodificacoes do artigo sobre o painel.
# Recebe o data frame com as colunas cruas de `ic_usp.parquet` e devolve o mesmo
# data frame acrescido das variaveis derivadas.
aplicar_recodificacoes <- function(dados) {
  dados |>
    dplyr::mutate(
      smf         = recode_renda_sm(rfm),
      qtd_pessoas = recode_pessoas_resid(pessoas_resid),
      # renda familiar per capita, em salarios minimos
      sfmpct      = smf / qtd_pessoas,
      periodo     = recode_periodo(periodo),
      educ_resp   = recode_educ_resp(educ_resp1),
      raca        = recode_raca(raca),
      trabalho    = recode_trabalho(atv_remu),
      sustento    = factor(recode_sustento(pretensao_mant), levels = NIVEIS_SUSTENTO),
      idade       = idade_ano_vest
    )
}
