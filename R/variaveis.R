# Variáveis derivadas ------------------------------------------------------------
# Tempo de tramitação, origem e comarca (numeração única do CNJ), carga do
# relator, classe harmonizada entre tribunais, matéria, exclusões e duplicatas.

library(dplyr)

VARS <- c("tempo_tramitacao", "n_palavras_ementa", "carga_relator")
ROTULOS <- c(tempo_tramitacao = "Tempo de tramitação (anos)",
             n_palavras_ementa = "Tamanho da ementa (palavras)",
             carga_relator = "Carga do relator (decisões no ano)")
CURTO <- c(tempo_tramitacao = "Tramitação", n_palavras_ementa = "Ementa",
           carga_relator = "Carga", exito = "Êxito")

# Número único (Res. CNJ 65/2008): NNNNNNN-DD.AAAA.J.TR.OOOO
# AAAA = ano de ajuizamento; J = segmento (8 = Justiça Estadual);
# TR = tribunal; OOOO = unidade de origem (0000 = originário do tribunal).
numeracao_cnj <- function(processo, tr, ano_max) {
  dig <- gsub("\\D", "", processo)
  ok <- !is.na(dig) & nchar(dig) == 20
  ano <- suppressWarnings(as.integer(ifelse(ok, substr(dig, 10, 13), NA)))
  justica <- ifelse(ok, substr(dig, 14, 14), NA)
  tr_num <- ifelse(ok, substr(dig, 15, 16), NA)
  cod <- ifelse(ok, substr(dig, 17, 20), NA)
  valido <- ok & justica == "8" & tr_num == tr & !is.na(ano) & ano >= 1980 & ano <= ano_max
  valido[is.na(valido)] <- FALSE
  data.frame(
    ano_ajuizamento = ifelse(valido, ano, NA_integer_),
    origem_cod = cod,
    numero_valido = valido,
    dig20 = ok,
    origem = ifelse(valido, ifelse(cod == "0000", "Originário do tribunal", "Primeiro grau"), NA),
    stringsAsFactors = FALSE
  )
}

# Classe processual harmonizada entre tribunais. No TJBA a classe diz só
# "Apelação": o órgão julgador indica se é cível ou criminal.
classe_grupo <- function(classe, orgao) {
  c <- toupper(sem_acentos(ifelse(is.na(classe), "", classe)))
  o <- toupper(sem_acentos(ifelse(is.na(orgao), "", orgao)))
  criminal <- grepl("CRIMINAL|PENAL", c) | grepl("CRIMINAL|PENAL", o)
  dplyr::case_when(
    grepl("EMBARGOS DE DECLARA", c) ~ "Embargos de declaração",
    grepl("AGRAVO DE INSTRUMENTO", c) ~ "Agravo de instrumento",
    grepl("AGRAVO INTERNO|AGRAVO REGIMENTAL", c) ~ "Agravo interno",
    grepl("AGRAVO DE EXECU|AGRAVO EM EXECU", c) ~ "Agravo de execução penal",
    grepl("HABEAS CORPUS", c) ~ "Habeas corpus",
    grepl("MANDADO DE SEGURAN", c) ~ "Mandado de segurança",
    grepl("RECURSO EM SENTIDO ESTRITO", c) ~ "Recurso em sentido estrito",
    grepl("REMESSA NECESS|REEXAME NECESS", c) ~ "Remessa necessária",
    startsWith(c, "APELA") & criminal ~ "Apelação criminal",
    startsWith(c, "APELA") ~ "Apelação cível",
    TRUE ~ "Outras"
  )
}

CLASSES_CRIMINAIS <- c("Apelação criminal", "Habeas corpus", "Agravo de execução penal",
                       "Recurso em sentido estrito")

# Cópias exatas: mesmo tribunal, processo, data e texto da ementa
remover_copias <- function(base) {
  # ementa_md5 vem do DuckDB (dados.R); sem ela, usa-se o próprio texto normalizado
  if (is.null(base$ementa_md5)) base$ementa_md5 <- normalizar(base$ementa)
  base <- base[order(base$normalized_hash), ]
  base[!duplicated(base[, c("tribunal", "processo", "data_julgamento", "ementa_md5")]), ]
}

# Limpeza e rótulo em blocos, para não manter várias cópias das ementas na memória
processar_em_blocos <- function(tribunal, ementa, tamanho = 20000L) {
  blocos <- split(seq_along(ementa), ceiling(seq_along(ementa) / tamanho))
  do.call(rbind, lapply(blocos, function(i) processar(tribunal[i], ementa[i])))
}

preparar <- function(base, cfg = CONFIG) {
  a <- remover_copias(base)
  rm(base)
  a <- cbind(a, processar_em_blocos(a$tribunal, a$ementa))
  a$ementa <- NULL  # o texto não é mais necessário
  gc(verbose = FALSE)

  # Relator: no TJSP o campo relator vem vazio e o nome está em orgao_julgador.
  a$relator_ef <- trimws(dplyr::coalesce(a$relator, a$orgao_julgador))
  # Órgão: no TJBA o campo diz onde o processo está; o órgão real vem do cabeçalho.
  org <- aparar(dplyr::coalesce(a$orgao_cabecalho, a$orgao_colegiado, ""), " ;")
  a$orgao <- ifelse(org == "", NA, org)
  a$mes <- format(as.Date(a$data_julgamento), "%Y-%m")

  # Turmas Recursais (juizados especiais) só existem no TJES e quase não têm ementa.
  a$excluida <- grepl("Turma Recursal", a$orgao_colegiado, ignore.case = TRUE) |
    grepl("RECURSO INOMINADO", toupper(dplyr::coalesce(a$classe, "")))

  # Carga do relator: decisões julgadas no ano, em toda a base lida.
  a <- a |>
    dplyr::group_by(tribunal, relator_ef) |>
    dplyr::mutate(carga_relator = ifelse(is.na(relator_ef), NA_integer_, dplyr::n())) |>
    dplyr::ungroup() |>
    as.data.frame()

  tr <- TRIBUNAIS$tr[match(a$tribunal, TRIBUNAIS$tribunal)]
  a <- cbind(a, numeracao_cnj(a$processo, tr, cfg$ano))
  a$tempo_tramitacao <- as.integer(format(as.Date(a$data_julgamento), "%Y")) - a$ano_ajuizamento

  partes <- stringi::stri_split_fixed(dplyr::coalesce(a$classe, ""), " / ", n = 2, simplify = TRUE)
  base_cl <- toupper(trimws(partes[, 1]))
  a$classe_base <- ifelse(base_cl == "", NA, base_cl)
  if (ncol(partes) > 1) {  # TJSP: "Classe / Assunto"
    sub <- trimws(partes[, 2])
    a$assunto <- dplyr::coalesce(a$assunto, ifelse(sub == "", NA, sub))
  }
  # TJPA traz o código CNJ antes do nome ("5566 - Roubo Majorado"); TJBA não tem assunto.
  ass <- toupper(trimws(sub("^\\d+\\s*-\\s*", "", a$assunto)))
  a$assunto <- ifelse(is.na(ass) | ass == "", NA, ass)

  a$comarca <- ifelse(!is.na(a$origem) & a$origem == "Primeiro grau",
                      paste0(a$tribunal, "-", a$origem_cod), NA)
  a$tramitacao_longa <- ifelse(!is.na(a$tempo_tramitacao) & !is.na(a$origem) & a$origem == "Primeiro grau",
                               as.numeric(a$tempo_tramitacao >= cfg$anos_longa), NA)
  a$classe_grupo <- classe_grupo(a$classe_base, a$orgao)
  a$materia <- ifelse(a$classe_grupo %in% CLASSES_CRIMINAIS |
                        grepl("CRIMINAL|PENAL", toupper(dplyr::coalesce(a$orgao, ""))),
                      "Criminal", "Cível")
  a$tribunal <- factor(a$tribunal, levels = ORDEM_TRIBUNAIS)
  a
}

analitica <- function(a) a[!a$excluida, ]

# Uma linha por processo: o tempo de tramitação é do processo, não da decisão.
# Prefere-se o julgamento de mérito e, entre eles, o mais antigo de 2025.
por_processo <- function(a) {
  a |>
    dplyr::filter(!is.na(tempo_tramitacao)) |>
    dplyr::arrange(tribunal, processo, is.na(resultado_merito), data_julgamento, normalized_hash) |>
    dplyr::distinct(tribunal, processo, .keep_all = TRUE) |>
    as.data.frame()
}

grupos <- function(a) {
  c(list(Total = a), split(a, a$tribunal, drop = TRUE))
}

por_relator <- function(a, min_merito) {
  a |>
    dplyr::filter(!is.na(relator_ef)) |>
    dplyr::group_by(tribunal, relator_ef) |>
    dplyr::summarise(
      n_decisoes = dplyr::n(),
      n_merito = sum(!is.na(exito)),
      taxa_exito = mean(exito, na.rm = TRUE) * 100,
      ementa_mediana = stats::median(n_palavras_ementa, na.rm = TRUE),
      pct_cnj = mean(modelo_cnj) * 100,
      tempo_mediana = stats::median(tempo_tramitacao, na.rm = TRUE),
      pct_criminal = mean(materia == "Criminal") * 100,
      .groups = "drop") |>
    dplyr::filter(n_merito >= min_merito) |>
    as.data.frame()
}

por_orgao <- function(a, min_merito) {
  a |>
    dplyr::filter(!is.na(orgao)) |>
    dplyr::group_by(tribunal, orgao) |>
    dplyr::summarise(n_decisoes = dplyr::n(), n_merito = sum(!is.na(exito)),
                     taxa_exito = mean(exito, na.rm = TRUE) * 100,
                     ementa_mediana = stats::median(n_palavras_ementa, na.rm = TRUE),
                     pct_cnj = mean(modelo_cnj) * 100, .groups = "drop") |>
    dplyr::filter(n_merito >= min_merito) |>
    as.data.frame()
}

# Comarca de origem (processos de primeiro grau): tempo por processo, êxito por julgamento
por_comarca <- function(a, p, min_n) {
  t <- p |>
    dplyr::filter(!is.na(comarca)) |>
    dplyr::group_by(tribunal, comarca) |>
    dplyr::summarise(n = dplyr::n(), tempo_mediana = stats::median(tempo_tramitacao),
                     tempo_media = mean(tempo_tramitacao), pct_longa = mean(tramitacao_longa) * 100,
                     .groups = "drop")
  e <- a |>
    dplyr::filter(!is.na(comarca)) |>
    dplyr::group_by(tribunal, comarca) |>
    dplyr::summarise(taxa_exito = mean(exito, na.rm = TRUE) * 100, .groups = "drop")
  dplyr::left_join(t, e, by = c("tribunal", "comarca")) |>
    dplyr::filter(n >= min_n) |>
    as.data.frame()
}

por_assunto <- function(a, p, min_n) {
  t <- p |>
    dplyr::filter(!is.na(assunto), !is.na(tramitacao_longa)) |>
    dplyr::group_by(tribunal, assunto) |>
    dplyr::summarise(n = dplyr::n(), tempo_mediana = stats::median(tempo_tramitacao),
                     pct_longa = mean(tramitacao_longa) * 100, .groups = "drop")
  e <- a |>
    dplyr::filter(!is.na(assunto)) |>
    dplyr::group_by(tribunal, assunto) |>
    dplyr::summarise(taxa_exito = mean(exito, na.rm = TRUE) * 100, .groups = "drop")
  dplyr::left_join(t, e, by = c("tribunal", "assunto")) |>
    dplyr::filter(n >= min_n) |>
    as.data.frame()
}
