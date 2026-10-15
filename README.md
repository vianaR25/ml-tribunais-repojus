# Resultado dos recursos, tempo de tramitação e ementas em quatro tribunais de justiça

**Projeto de Machine Learning 2026.2 · IBMEC Rio de Janeiro · Grupo 6**
Rafael Viana, Lucas Goulart, Marcelo Saggio e Douglas Hancok

Análise descritiva, em **R**, de todos os acórdãos julgados em 2025 pelos tribunais de justiça de
São Paulo (TJSP), Espírito Santo (TJES), Pará (TJPA) e Bahia (TJBA), extraídos do corpus público
[RepoJus](https://huggingface.co/datasets/andrebadini/repojus) (commit `5791ee7`).

O trabalho (AP1) aplica estatística descritiva, gráficos, testes de normalidade (Shapiro-Wilk,
Kolmogorov-Smirnov com correção de Lilliefors e qui-quadrado de aderência), correlações de Pearson e Spearman e associação entre
variáveis categóricas (qui-quadrado e V de Cramér). A conclusão orienta a próxima etapa (AP2): um
modelo de classificação que estima, no ajuizamento, a chance de um processo de primeiro grau ter o
recurso julgado **cinco anos ou mais** depois, só com o que se conhece nessa data (tribunal,
comarca, assunto, matéria e tipo de recurso). O modelo será treinado com os julgados de 2025 e
testado com os de 2026.

## Como rodar

Requisitos: R 4.3 ou mais recente e conexão com a internet na primeira execução.

```bash
Rscript instalar_pacotes.R                     # uma vez
Rscript -e 'testthat::test_dir("tests/testthat")'   # testes da limpeza e do rótulo
Rscript analise.R                              # análise completa
```

Na primeira execução, `analise.R` lê do Hugging Face os cerca de 208 mil acórdãos de 2025 dos
quatro tribunais (só as colunas necessárias, alguns minutos) e guarda um cache em `dados/`. As
execuções seguintes usam o cache. Para refazer a leitura: `Rscript analise.R --forcar`.

No RStudio, abra a pasta do projeto como diretório de trabalho e rode `source("analise.R")`
seguido de `executar()`.

## Estrutura

| Arquivo | O que faz |
|---|---|
| `analise.R` | Script principal: roda tudo e grava `resultados/` |
| `R/config.R` | Recorte: commit do RepoJus, ano, tribunais, parâmetros |
| `R/dados.R` | Leitura do RepoJus com DuckDB direto do Hugging Face |
| `R/limpeza.R` | Limpeza da ementa por tribunal e rótulo do resultado por expressões regulares |
| `R/variaveis.R` | Tempo de tramitação, origem e comarca (numeração do CNJ), carga do relator, classe, duplicatas |
| `R/estatistica.R` | Descritiva, normalidade, correlação, qui-quadrado e V de Cramér |
| `R/graficos.R` | Figuras em ggplot2 |
| `tests/testthat/` | Testes da limpeza, do rótulo e das variáveis |
| `resultados/` | Tabelas (`tabelas/*.csv`), figuras (`figs/*.png`) e `resumo.json` com os números do artigo |
| `paper/` | Artigo em LaTeX (modelo IEEE) |

## Principais decisões de método

- **Base completa, não amostra:** todos os acórdãos de 2025 dos quatro tribunais no RepoJus.
  Ficam de fora as Turmas Recursais do TJES (juizados especiais) e cópias exatas de uma mesma decisão.
- **Só decisões colegiadas:** o RepoJus traz acórdãos; decisões monocráticas não entram na análise.
- **Resultado:** extraído do texto da ementa. Recursos providos (no todo ou em parte) e ordens
  concedidas em *habeas corpus* ou mandado de segurança contam como êxito.
- **Tempo de tramitação:** ano do julgamento menos o ano de ajuizamento registrado no número único
  do processo (Resolução CNJ 65/2008), contado uma vez por processo. Processos com origem `0000`
  foram autuados no próprio tribunal (ações originárias e agravos de instrumento); neles o tempo
  mede só a fase no tribunal.
- **Normalidade:** variáveis aproximadamente contínuas (tamanho da ementa) com Shapiro-Wilk até 2.000
  observações e Kolmogorov-Smirnov-Lilliefors acima; o tempo de tramitação, discreto (anos inteiros),
  com qui-quadrado de aderência a uma normal discretizada e w de Cohen; a carga, que é atributo do
  relator, testada por relator.
- **Correlação:** Pearson quando a normalidade das duas variáveis não é rejeitada, Spearman nos
  demais casos. Por relator, as correlações também são calculadas só entre relatores de pauta
  cível, para separar o efeito da composição da pauta.

## Dados

O RepoJus é mantido por André Badini no Hugging Face. Este repositório não redistribui os dados:
eles são lidos diretamente da fonte, no commit fixado, para garantir reprodutibilidade.
