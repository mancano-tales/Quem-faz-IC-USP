# Quem faz Iniciação Científica na USP?

**Pacote de replicação** do artigo *Quem faz Iniciação Científica na USP?
Heterogeneidade entre áreas do conhecimento no acesso à Iniciação Científica
(2010–2022)*, de **Tales Mançano** ([0000-0001-5923-9743](https://orcid.org/0000-0001-5923-9743))
e **Victor Alcantara** ([0000-0001-8846-9652](https://orcid.org/0000-0001-8846-9652)).

📕 **[Working paper em PDF](docs/working-paper.pdf)** · 📄 **[O artigo com todo o código](docs/paper.html)**

Este pacote é **irmão** de [`Quem-faz-IC`](https://github.com/mancano-tales/Quem-faz-IC),
que analisa a mesma pergunta na FFLCH. Dele herda os microdados, a construção
do painel e a recodificação das variáveis, sem alteração. O que muda é o
recorte: onde aquele filtrava uma unidade, este mantém as 48.

---

## A pergunta

O estudo anterior concluiu que, na FFLCH, as características de origem social
explicam pouco o acesso à Iniciação Científica, e a necessidade de trabalhar
explica muito. Mas a FFLCH é atípica: concentra cursos noturnos, recebe uma
proporção de estudantes trabalhadores bem acima da média da USP, e sua relação
com a pesquisa de graduação difere da das ciências experimentais.

**Aquele estudo analisou uma unidade. Este pergunta se o achado descreve a
universidade.**

## O que encontramos

**1. A heterogeneidade institucional supera tudo o que estávamos medindo.**

A proporção de estudantes que realizam IC vai de **2,7%** (Interunidades de
Licenciatura IFSC/IQSC/ICMC) a **36,1%** (Escola de Enfermagem). Entre áreas,
de 6,9% em Linguística, Letras e Artes a 23,8% em Ciências da Saúde.

| O que o modelo conhece | Pseudo-R² |
|---|---|
| Apenas a área do conhecimento (9 categorias) | 0,034 |
| **Apenas a unidade de ingresso (48 categorias)** | **0,063** |
| Apenas as características do estudante (o modelo do estudo anterior) | 0,056 |

Saber apenas em qual unidade o estudante entrou explica mais da variação
individual em fazer IC do que sua renda, raça, escolaridade dos pais, idade,
turno, vínculo com trabalho e forma de sustento — todos juntos.

**2. O mecanismo do trabalho se confirma, e se fortalece.**

| Efeito marginal médio | FFLCH | USP |
|---|---|---|
| Trabalha em tempo integral | −2,35 pp | **−3,73 pp** |
| Sustento por conta própria | −2,89 pp | −3,28 pp |
| Período noturno | −2,72 pp | −3,06 pp |
| Renda familiar per capita (+1 dp) | −1,20 pp | −0,77 pp |
| Raça: PPI | −1,18 pp | −1,73 pp |
| Responsável com superior completo | +0,59 pp | −0,40 pp |

O trabalho supera a maior das características de origem em **seis das nove
áreas**. E em **oito das nove**, o fator dominante é alguma medida de
disponibilidade de tempo — trabalho ou turno noturno.

**3. Apareceu o que não estávamos procurando: gênero.**

Na FFLCH, ser homem tem efeito de −0,05 pp, indistinguível de zero. Na USP, com
efeitos fixos de área, é **−3,13 pp**. Dentro de Ciências Agrárias (−8,1 pp),
da Saúde (−6,8 pp) e Engenharias (−3,8 pp), homens fazem menos IC que mulheres,
e o efeito supera o de qualquer característica de origem. Como os modelos são
estimados dentro de cada área, não é segregação horizontal entre carreiras.

O artigo registra o padrão e não o explica — nosso desenho não distingue entre
diferenças de desempenho, de disponibilidade para atividades não remuneradas,
de relação com orientadores, ou de custo de oportunidade no mercado de trabalho.

---

## Como replicar

**Requisitos**: R 4.4 ou superior; [Quarto](https://quarto.org) para os
documentos. Os pacotes são instalados na primeira execução.

```bash
git clone https://github.com/mancano-tales/Quem-faz-IC-USP.git
cd Quem-faz-IC-USP
Rscript run_all.R
quarto render
```

Para reproduzir com as versões exatas, `renv::restore()`.

O pipeline **falha em vez de avisar**: `01-build-data.R` interrompe se o painel
não sair com 298.704 linhas e 54 colunas, e `02-descritivas.R` interrompe se a
USP não tiver 152.729 ingressantes nas coortes analisadas ou se as 48 unidades
não estiverem classificadas nas 9 áreas.

## Estrutura

```
├── run_all.R                  executa o pipeline inteiro
├── paper.qmd                  o artigo (→ docs/paper.html e working-paper.pdf)
│
├── data-raw/                  microdados do SIC-USP + a classificação em áreas
├── data/                      ic_usp.parquet — painel analítico, gerado
│
├── R/
│   ├── setup.R                pacotes, caminhos e recortes
│   ├── recode.R               recodificação do QASE (herdada do pacote irmão)
│   └── modelos.R              especificação do modelo e efeitos marginais
│
├── analysis/
│   ├── 01-build-data.R        brutos → painel (herdado)
│   ├── 02-descritivas.R       acesso à IC por área e por unidade
│   ├── 03-modelos.R           a especificação da FFLCH aplicada à USP
│   └── 04-heterogeneidade.R   o mesmo modelo, área por área
│
├── tools/                     geradores das áreas, do codebook e dos checksums
└── outputs/{tables,figures}   resultados gerados
```

## A classificação em áreas

O arquivo do SIC-USP informa a unidade de ingresso, não a área do conhecimento.
A classificação das 48 unidades em nove áreas **é nossa**, segue as grandes
áreas do CNPq, e vive em [`data-raw/unidades-areas.csv`](data-raw/unidades-areas.csv),
gerado por `tools/make-areas.R`. Quem discordar edita a coluna `area` e roda o
pipeline de novo.

Duas decisões ficam explícitas no código:

- Unidades que abrigam cursos de várias áreas — EACH, FFCLRP, cursos
  interunidades e a Pró-reitoria de Graduação — vão para **Multidisciplinar**.
  Forçá-las numa área seria inventar homogeneidade que elas não têm.
- A FFLCH, que abriga Ciências Humanas e Letras, vai inteira para Ciências
  Humanas, como a USP a classifica. Separá-la por curso mudaria a unidade de
  análise.

## Escolhas metodológicas

**A especificação do estudo anterior é mantida intacta.** Se mudássemos o
modelo junto com a amostra, não saberíamos qual das duas mudanças explica a
diferença. A única adição é o efeito fixo de área no modelo agregado, sem o
qual o coeficiente de qualquer variável confundiria o efeito individual com o
fato de certas áreas ofertarem muito mais IC.

**Comparamos efeitos marginais médios, não coeficientes.** Coeficientes
logísticos de variáveis em escalas diferentes — salários mínimos, anos de
idade, categorias de trabalho — não são comparáveis entre si. O AME põe todos
em pontos percentuais na probabilidade de fazer IC, que é o que permite
responder "o trabalho continua sendo o maior efeito?".

**A categoria residual `sustento: Outros` é excluída dos rankings.** Ela reúne
de 0,8% a 2,0% dos casos — catorze pessoas em Ciências Biológicas — e produz
efeitos grandes que são ruído, não achado.

## Limitações

Herdadas do estudo anterior, e mais pesadas aqui:

- **Não há indicador de desempenho acadêmico.** A classificação na carreira no
  vestibular é um proxy pobre, e piora ao comparar áreas cujos processos
  seletivos têm concorrências muito diferentes.
- **O sub-registro de bolsas fora do CNPq no sistema Atena não é uniforme entre
  áreas.** Se as unidades das ciências experimentais registram melhor suas
  bolsas FAPESP do que as de humanidades, parte da diferença que atribuímos à
  área é diferença de registro. Não temos como quantificar esse viés com os
  dados disponíveis, e ele é a primeira coisa que um estudo seguinte deveria
  enfrentar.

## Dados e licença

Microdados de dois pedidos ao SIC-USP sob a Lei de Acesso à Informação
(Cod. #243654 e #243681), entregues já anonimizados pela universidade — o
identificador é um inteiro sequencial, sem nome, número USP, CPF ou data de
nascimento. O [CODEBOOK.md](CODEBOOK.md) documenta as 54 variáveis do painel; o
`data-raw/CHECKSUMS.txt` registra o MD5 de cada arquivo.

O **código** é distribuído sob a **GNU General Public License v2**. Os **dados**
são informação pública e permanecem públicos, sob os termos da
**[CC BY 4.0](https://creativecommons.org/licenses/by/4.0/deed.pt-br)**; ao
reutilizá-los, cite o SIC-USP e os códigos dos pedidos.
