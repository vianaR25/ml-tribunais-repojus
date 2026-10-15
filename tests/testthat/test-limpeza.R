# Testes da limpeza da ementa, do rótulo de resultado e das variáveis derivadas.
# Rodar na pasta do projeto:  Rscript -e 'testthat::test_dir("tests/testthat")'

raiz <- normalizePath(file.path(testthat::test_path(), "..", ".."))
for (f in c("config", "limpeza", "variaveis", "estatistica"))
  source(file.path(raiz, "R", paste0(f, ".R")), encoding = "UTF-8")

test_that("TJSP: fica só o texto após a última 'Ementa:'", {
  corpo <- paste("DIREITO CIVIL.", paste(rep("palavra", 150), collapse = " "), "Recurso não provido.")
  trecho <- paste(strsplit(corpo, " ")[[1]][1:58], collapse = " ")
  d <- limpar_ementa("TJSP", paste(trecho, "Ementa:", corpo))
  expect_equal(d$n_palavras_ementa, length(strsplit(corpo, " ")[[1]]))
  expect_false(d$sem_ementa)
})

test_that("TJBA: remove cabeçalho, extrai o órgão e tira a cauda da contagem", {
  e <- paste("PODER JUDICIÁRIO TRIBUNAL DE JUSTIÇA DO ESTADO DA BAHIA Processo: Apelação",
             "Órgão Julgador: Terceira Câmara Cível APELANTE: FULANO Advogado(s): BELTRANO",
             "APELADO: SICRANO Advogado(s): X DIREITO CIVIL. APELAÇÃO. Dano moral.",
             "ACÓRDÃO Vistos, relatados e discutidos. ACORDAM os Desembargadores em NEGAR",
             "PROVIMENTO ao recurso. Salvador. Relator")
  d <- limpar_ementa("TJBA", e)
  expect_equal(d$orgao_cabecalho, "TERCEIRA CÂMARA CÍVEL")
  expect_equal(d$n_palavras_ementa, 5L)
  expect_equal(classificar_resultado(d$ementa_limpa)$resultado, "não provido")
})

test_that("TJES: 'Voto servindo como ementa' não tem ementa", {
  d <- limpar_ementa("TJES", "Voto servindo como ementa.")
  expect_true(d$sem_ementa)
  expect_equal(classificar_resultado(d$ementa_limpa, d$sem_ementa)$resultado, "sem ementa")
})

casos <- list(
  c("Contrato bancário. Juros. Sentença mantida. Recurso não provido.", "não provido"),
  c("Decisão reformada. RECURSO PROVIDO.", "provido"),
  c("Danos morais. Recurso parcialmente provido.", "parcialmente provido"),
  c("Omissão inexistente. Embargos de declaração rejeitados.", "embargos rejeitados"),
  c("Erro material. Embargos acolhidos.", "embargos acolhidos"),
  c("Preliminar rejeitada. Recurso desprovido.", "não provido"),
  c("Tráfico. Prisão preventiva. Ordem denegada.", "ordem denegada"),
  c("Constrangimento ilegal. Ordem concedida.", "ordem concedida"),
  c("Intempestividade. Recurso não conhecido.", "não conhecido"),
  c(paste("I. CASO EM EXAME 1. Apelação. IV. DISPOSITIVO E TESE 8. Recurso desprovido.",
          "Tese de julgamento: x. Dispositivos relevantes citados: CPC."), "não provido"),
  c("Ementa. ACORDAM os Desembargadores em DAR PARCIAL PROVIMENTO ao recurso, nos termos do voto. Relator",
    "parcialmente provido"),
  c("Texto sem dispositivo algum.", "não identificado"))

test_that("rótulo de resultado por expressões regulares", {
  textos <- vapply(casos, `[`, "", 1)
  esperado <- vapply(casos, `[`, "", 2)
  expect_equal(classificar_resultado(textos)$resultado, esperado)
})

test_that("êxito: provido, parcial e ordem concedida contam como êxito", {
  r <- processar(rep("TJSP", 4), c("Recurso provido.", "Recurso parcialmente provido.",
                                   "Ordem concedida.", "Recurso não provido."))
  expect_equal(r$exito, c(1, 1, 1, 0))
  expect_equal(r$tipo_julgamento, c("Recurso", "Recurso", "Ação originária (HC/MS)", "Recurso"))
})

test_that("numeração única do CNJ", {
  s <- c("1001234-56.2019.8.26.0100", "2123456-78.2025.8.26.0000", "00012345620198260100",
         "1001234-56.2019.8.05.0100", "texto qualquer")
  r <- numeracao_cnj(s, rep("26", 5), 2025)
  expect_equal(r$ano_ajuizamento[1:3], c(2019L, 2025L, 2019L))
  expect_equal(r$origem[1:3], c("Primeiro grau", "Autuado no tribunal", "Primeiro grau"))
  expect_false(r$numero_valido[4])  # código de outro tribunal
  expect_false(r$numero_valido[5])
})

test_that("classe harmonizada entre tribunais", {
  expect_equal(classe_grupo(c("Apelação Cível", "Apelação", "AGRAVO DE INSTRUMENTO",
                              "Embargos de Declaração Cível", "Conflito de competência"),
                            c("1ª Câmara de Direito Privado", "PRIMEIRA CAMARA CRIMINAL", "", "", "")),
               c("Apelação cível", "Apelação criminal", "Agravo de instrumento",
                 "Embargos de declaração", "Outras"))
})

test_that("cópias exatas são removidas", {
  b <- data.frame(normalized_hash = c("a", "b", "c"), tribunal = "TJBA",
                  processo = c("1", "1", "2"), data_julgamento = as.Date("2025-05-01"),
                  ementa = c("Texto  igual.", "Texto igual.", "Texto igual."))
  expect_equal(nrow(remover_copias(b)), 2)
})

test_that("qui-quadrado de aderência para dados discretos", {
  set.seed(1)
  quase_normal <- round(rnorm(5000, 50, 10))       # normal arredondada: não deve rejeitar
  assimetrica <- rpois(5000, 1)                     # muitos zeros, assimétrica: deve rejeitar
  expect_gt(aderencia_discreta(quase_normal, 0.05)$p, 0.01)
  r <- aderencia_discreta(assimetrica, 0.05)
  expect_lt(r$p, 0.001)
  expect_gt(r$w, 0.1)
})
