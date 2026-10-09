# AP1 — Estatística descritiva de acórdãos de quatro tribunais de justiça (RepoJus, 2025)
#
# Uso (na pasta do projeto):
#   Rscript analise.R            usa o cache em dados/ se existir
#   Rscript analise.R --forcar   refaz a leitura do RepoJus (alguns minutos)
#
# Saídas:
#   resultados/tabelas/*.csv     tabelas
#   resultados/figs/*.png        figuras (copiadas para paper/figs/)
#   resultados/resumo.json       números citados no artigo

suppressPackageStartupMessages({
  for (f in c("config", "dados", "limpeza", "variaveis", "estatistica", "graficos"))
    source(file.path("R", paste0(f, ".R")), encoding = "UTF-8")
})

executar <- function(cfg = CONFIG, forcar = FALSE) {
  figs <- file.path(cfg$dir_resultados, "figs")
  tabs <- file.path(cfg$dir_resultados, "tabelas")
  for (d in c(figs, tabs)) dir.create(d, showWarnings = FALSE, recursive = TRUE)
  gravar <- function(df, nome) readr::write_csv(df, file.path(tabs, paste0(nome, ".csv")), na = "")
  resumo <- list(recorte = list(tribunais = ORDEM_TRIBUNAIS, ano = cfg$ano, commit = cfg$sha,
                                semente = cfg$semente, min_relator = cfg$min_relator,
                                min_categoria = cfg$min_categoria, anos_longa = cfg$anos_longa))

  message("1/6 Lendo o RepoJus (todos os acórdãos do ano)...")
  con <- conectar(); on.exit(DBI::dbDisconnect(con, shutdown = TRUE))
  base <- carregar(con, cfg, forcar)

  message("2/6 Limpeza, rótulo de resultado e variáveis...")
  todas <- preparar(base, cfg)
  a <- analitica(todas)          # uma linha por decisão
  p <- por_processo(a)           # uma linha por processo (tempo de tramitação)
  pg <- p[!is.na(p$tramitacao_longa), ]
  conta <- function(x) as.list(table(factor(x, levels = ORDEM_TRIBUNAIS)))
  tp <- function(...) as.list(tapply(...))  # vetor por tribunal, com nomes no JSON
  resumo$recorte <- c(resumo$recorte, list(
    n_lidas = nrow(base), n_lidas_por_tribunal = conta(base$tribunal),
    n_copias_removidas = nrow(base) - nrow(todas),
    n_excluidas = sum(todas$excluida), n_excluidas_por_tribunal = conta(todas$tribunal[todas$excluida]),
    n_analitica = nrow(a), n_por_tribunal = conta(a$tribunal),
    n_merito = sum(!is.na(a$exito)), n_merito_por_tribunal = conta(a$tribunal[!is.na(a$exito)]),
    n_processos = nrow(p), n_processos_por_tribunal = conta(p$tribunal),
    n_processos_primeiro_grau = nrow(pg), n_processos_primeiro_grau_por_tribunal = conta(pg$tribunal)))

  message("3/6 Qualidade da base e rótulo...")
  prev <- todas |> dplyr::group_by(tribunal) |> dplyr::summarise(
    n = dplyr::n(), relator_campo = mean(!is.na(relator)) * 100,
    orgao_colegiado = mean(!is.na(orgao_colegiado)) * 100, classe = mean(!is.na(classe)) * 100,
    assunto = mean(!is.na(assunto)) * 100, data_publicacao = mean(!is.na(data_publicacao)) * 100,
    processo_20_digitos = mean(dig20) * 100, numero_cnj_valido = mean(numero_valido) * 100,
    inteiro_teor_diferente = mean(n_chars_texto != n_chars_ementa_bruta, na.rm = TRUE) * 100,
    sem_ementa = mean(sem_ementa) * 100, modelo_cnj = mean(modelo_cnj) * 100,
    excluidas = mean(excluida) * 100)
  gravar(prev, "preenchimento")
  resumo$preenchimento <- prev
  cob <- as.data.frame.matrix(prop.table(table(factor(a$resultado, levels = CATEGORIAS), a$tribunal), 2) * 100)
  cob$Total <- as.numeric(prop.table(table(factor(a$resultado, levels = CATEGORIAS))) * 100)
  gravar(cbind(resultado = rownames(cob), round(cob, 2)), "cobertura_resultado")
  resumo$cobertura_resultado <- cbind(resultado = rownames(cob), cob)
  resumo$resultado_janela <- as.data.frame.matrix(prop.table(table(a$tribunal, dplyr::coalesce(a$resultado_janela, "—")), 1) * 100)
  resumo$numeracao_cnj <- list(
    pct_validos = lapply(tp(a$numero_valido, a$tribunal, mean), `*`, 100),
    pct_originario = lapply(tp(a$origem == "Originário do tribunal", a$tribunal, mean, na.rm = TRUE), `*`, 100),
    origem_por_classe = as.data.frame.matrix(prop.table(table(a$classe_grupo, a$origem), 1) * 100))
  resumo$classes <- as.data.frame.matrix(prop.table(table(a$classe_grupo, a$tribunal), 2) * 100)
  resumo$materia <- as.data.frame.matrix(prop.table(table(a$tribunal, a$materia), 1) * 100)
  resumo$tipo_julgamento <- as.data.frame.matrix(prop.table(table(a$tribunal, dplyr::coalesce(a$tipo_julgamento, "—")), 1) * 100)
  resumo$relatores_distintos <- tp(a$relator_ef, a$tribunal, function(x) length(unique(stats::na.omit(x))))
  # Duplicatas e monocráticas
  resumo$duplicatas <- list(
    processos_com_mais_de_uma_decisao = tp(a$processo, a$tribunal, function(x) sum(table(x) > 1)),
    decisoes_mesmo_processo_e_data = tp(duplicated(a[, c("tribunal", "processo", "data_julgamento")]) |
      duplicated(a[, c("tribunal", "processo", "data_julgamento")], fromLast = TRUE), a$tribunal, sum))

  message("4/6 Estatística...")
  desc <- descritiva(a, p)
  gravar(desc, "descritiva")
  resumo$descritiva <- desc
  resumo$tempo_por_origem <- p |> dplyr::filter(!is.na(origem)) |> dplyr::group_by(tribunal, origem) |>
    dplyr::summarise(n = dplyr::n(), mediana = stats::median(tempo_tramitacao),
                     q1 = stats::quantile(tempo_tramitacao, .25), q3 = stats::quantile(tempo_tramitacao, .75),
                     p90 = stats::quantile(tempo_tramitacao, .9), pct_zero = mean(tempo_tramitacao == 0) * 100,
                     .groups = "drop")
  resumo$tempo_p90 <- c(Total = stats::quantile(p$tempo_tramitacao, .9, names = FALSE),
                        tp(p$tempo_tramitacao, p$tribunal, stats::quantile, .9))
  resumo$tempo_pct_zero <- c(Total = mean(p$tempo_tramitacao == 0) * 100,
                             lapply(tp(p$tempo_tramitacao == 0, p$tribunal, mean), `*`, 100))
  resumo$ementa_por_modelo <- a |> dplyr::filter(!is.na(n_palavras_ementa)) |>
    dplyr::group_by(tribunal, modelo = ifelse(modelo_cnj, "CNJ", "Tradicional")) |>
    dplyr::summarise(n = dplyr::n(), mediana = stats::median(n_palavras_ementa), .groups = "drop")
  resumo$adocao_cnj <- lapply(tp(a$modelo_cnj, a$tribunal, mean), `*`, 100)

  m <- a[!is.na(a$resultado_merito), ]
  resumo$resultado_merito <- list(
    pct = as.data.frame.matrix(prop.table(table(m$tribunal, m$resultado_merito), 1) * 100),
    n = as.data.frame.matrix(table(m$tribunal, m$resultado_merito)),
    taxa_exito_total = mean(m$exito) * 100,
    taxa_exito = lapply(tp(m$exito, m$tribunal, mean), `*`, 100))
  for (por in c("classe_grupo", "materia", "modelo_cnj", "origem"))
    resumo[[paste0("taxa_por_", por)]] <- taxa_exito(a, por)
  a$faixa_tempo_txt <- as.character(cut(a$tempo_tramitacao, c(-0.5, 0.5, 2.5, 5.5, 10.5, 100),
                                        labels = c("0", "1–2", "3–5", "6–10", "11+")))
  resumo$taxa_por_faixa_tempo <- taxa_exito(a, "faixa_tempo_txt")

  normal <- normalidade(a, p, cfg$alfa)
  gravar(normal, "normalidade")
  resumo$normalidade <- normal

  corr <- por_grupo(a, function(d) correlacoes(d, PARES_DECISAO, CURTO))
  gravar(corr, "correlacao_decisao")
  resumo$correlacao_decisao <- corr

  rel <- por_relator(a, cfg$min_relator)
  # Na tabela publicada, os relatores aparecem com um código, não com o nome.
  rel_pub <- rel |> dplyr::group_by(tribunal) |>
    dplyr::mutate(relator_ef = sprintf("%s-R%03d", tribunal, dplyr::row_number())) |>
    dplyr::ungroup() |> dplyr::rename(relator = relator_ef)
  gravar(rel_pub, "relatores")
  resumo$relatores <- rel |> dplyr::group_by(tribunal) |> dplyr::summarise(
    n = dplyr::n(), taxa_mediana = stats::median(taxa_exito), taxa_min = min(taxa_exito),
    taxa_max = max(taxa_exito), taxa_q1 = stats::quantile(taxa_exito, .25),
    taxa_q3 = stats::quantile(taxa_exito, .75), carga_mediana = stats::median(n_decisoes))
  normal_rel <- normalidade_agregada(rel, c("n_decisoes", "taxa_exito", "ementa_mediana"), cfg$alfa)
  gravar(normal_rel, "normalidade_relator")
  resumo$normalidade_relator <- normal_rel
  corr_rel <- por_grupo(rel, function(d) correlacoes(d, PARES_RELATOR, ROT_RELATOR))
  gravar(corr_rel, "correlacao_relator")
  resumo$correlacao_relator <- corr_rel

  org <- por_orgao(a, cfg$min_categoria)
  gravar(org, "orgaos")
  resumo$orgaos <- lapply(split(org, org$tribunal, drop = TRUE), function(g) list(
    n_orgaos = nrow(g), mediana = stats::median(g$taxa_exito),
    maior = g[which.max(g$taxa_exito), c("orgao", "taxa_exito", "n_merito")],
    menor = g[which.min(g$taxa_exito), c("orgao", "taxa_exito", "n_merito")]))

  assoc <- associacao_resultado(a, cfg$min_categoria)
  gravar(assoc, "associacao_resultado")
  resumo$associacao_resultado <- assoc

  # Tramitação longa nos processos de primeiro grau
  resumo$tramitacao_longa <- list(
    anos = cfg$anos_longa, n = nrow(pg), pct_total = mean(pg$tramitacao_longa) * 100,
    pct_por_tribunal = lapply(tp(pg$tramitacao_longa, pg$tribunal, mean), `*`, 100),
    pct_por_materia = as.data.frame.matrix(tapply(pg$tramitacao_longa, list(pg$tribunal, pg$materia), mean) * 100),
    pct_por_classe = pg |> dplyr::group_by(classe_grupo) |>
      dplyr::summarise(n = dplyr::n(), pct = mean(tramitacao_longa) * 100))
  cm <- por_comarca(a, p, cfg$min_categoria)
  gravar(cm, "comarcas")
  resumo$comarcas <- lapply(split(cm, cm$tribunal, drop = TRUE), function(g) list(
    n_comarcas = nrow(g), tempo_mediana_min = min(g$tempo_mediana), tempo_mediana_max = max(g$tempo_mediana),
    tempo_mediana_q1 = stats::quantile(g$tempo_mediana, .25, names = FALSE),
    tempo_mediana_q3 = stats::quantile(g$tempo_mediana, .75, names = FALSE),
    pct_longa_min = min(g$pct_longa), pct_longa_max = max(g$pct_longa)))
  resumo$comarcas_total <- list(n = nrow(cm), n_total = sum(!is.na(unique(pg$comarca))))
  corr_cm <- por_grupo(cm, function(d) correlacoes(d, PARES_COMARCA, ROT_COMARCA))
  gravar(corr_cm, "correlacao_comarca")
  resumo$correlacao_comarca <- corr_cm
  normal_cm <- normalidade_agregada(cm, c("tempo_mediana", "pct_longa", "n"), cfg$alfa)
  gravar(normal_cm, "normalidade_comarca")
  resumo$normalidade_comarca <- normal_cm
  ass <- por_assunto(a, p, cfg$min_categoria)
  gravar(ass, "assuntos")
  resumo$assuntos <- lapply(split(ass, ass$tribunal, drop = TRUE), function(g)
    utils::head(g[order(-g$n), c("assunto", "n", "tempo_mediana", "pct_longa")], 12))
  assoc_t <- associacao_tempo(p, cfg$min_categoria)
  gravar(assoc_t, "associacao_tempo")
  resumo$associacao_tempo <- assoc_t

  message("5/6 Figuras...")
  fig_resultado_tribunal(a, figs)
  fig_exito_classe(a, figs)
  fig_tempo_tribunal(p, figs)
  fig_ementa_tribunal(a, figs)
  fig_relator_carga_exito(rel, figs)
  fig_qq(a, p, figs, cfg$semente)
  fig_spearman(a, figs)
  fig_cnj_tribunal(a, figs)
  fig_exito_orgao(org, figs)
  resumo$volume_mensal <- fig_volume_mensal(a, figs, cfg$ano)
  fig_tempo_comarca(cm, figs)
  if (nrow(ass)) fig_tempo_assunto(ass, figs)

  message("6/6 Salvando resumo e copiando figuras para o artigo...")
  jsonlite::write_json(resumo, file.path(cfg$dir_resultados, "resumo.json"), auto_unbox = TRUE,
                       pretty = TRUE, digits = 6, na = "null", dataframe = "rows")
  destino <- file.path(cfg$dir_artigo, "figs")
  dir.create(destino, showWarnings = FALSE, recursive = TRUE)
  file.copy(list.files(figs, "\\.png$", full.names = TRUE), destino, overwrite = TRUE)
  writeLines(utils::capture.output(utils::sessionInfo()), file.path(cfg$dir_resultados, "sessao_R.txt"))
  message(sprintf("Pronto. %s acórdãos, %s processos. Resultados em %s/",
                  format(nrow(a), big.mark = ".", decimal.mark = ","), format(nrow(p), big.mark = ".", decimal.mark = ","), cfg$dir_resultados))
  invisible(list(resumo = resumo, a = a, p = p))
}

if (sys.nframe() == 0L) {
  executar(forcar = "--forcar" %in% commandArgs(trailingOnly = TRUE))
}
