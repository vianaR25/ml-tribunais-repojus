# Limpeza da ementa e resultado do julgamento -------------------------------
# Particularidades de cada tribunal no RepoJus (2025):
# - TJSP: em ~83% das decisões o campo traz um trecho truncado, a linha
#   "Ementa:" e o texto completo. Fica-se com o texto após a última "Ementa:".
# - TJBA: a ementa começa com o cabeçalho do acórdão (partes, advogados,
#   "Órgão Julgador: ..."). O cabeçalho é removido e o órgão julgador real é
#   extraído dele.
# - TJPA e TJBA: a ementa termina com o dispositivo do acórdão
#   ("ACORDAM ... em NEGAR PROVIMENTO ...") e as assinaturas. Essa "cauda" é a
#   melhor fonte do resultado e fica fora da contagem de palavras.
# - TJES: ~26% das ementas são o texto "Voto servindo como ementa." (Turmas
#   Recursais, excluídas da análise).

library(stringi)

normalizar <- function(x) {
  x[is.na(x)] <- ""
  stri_trim_both(stri_replace_all_regex(x, "\\s+", " "))
}

aparar <- function(x, chars) {
  # remove os caracteres de 'chars' das duas pontas (equivale a str.strip(chars))
  classe <- paste0("[", gsub("([\\]\\[\\\\^-])", "\\\\\\1", chars), "]+")
  stri_replace_all_regex(x, paste0("^", classe, "|", classe, "$"), "")
}

sem_acentos <- function(x) stri_trans_general(x, "Latin-ASCII")

contar_palavras <- function(x) {
  x <- stri_trim_both(x)
  ifelse(nchar(x) == 0, 0L, stri_count_regex(x, "\\s+") + 1L)
}

RX_TJBA_INICIO <- "(A C O R D Ã O|AC[OÓ]RD[AÃ]O|EMENTA|Ementa:)"
RX_TJBA_ORGAO <- paste0(
  "Órgão Julgador:\\s*(.+?)\\s+(?:APELANTE|APELADO|AGRAVANTE|AGRAVADO|RECORRENTE|",
  "IMPETRANTE|EMBARGANTE|REQUERENTE|AUTOR|SUSCITANTE|PACIENTE|RECLAMANTE|",
  "INTERESSADO|[A-ZÇÃÕÉÊÍÓÚÂ]{3,}:)")
RX_CAUDA <- "(AC[OÓ]RD[AÃ]O\\s*:?\\s*Vistos|\\bVistos\\b,?\\s*(relatados|etc)|\\bA\\s?C\\s?O\\s?R\\s?D\\s?A\\s?M\\b)"
RX_PLACEHOLDER <- "voto\\s+servindo\\s+como\\s+ementa"
RX_CNJ <- "caso\\s+em\\s+exame"

# Limpa um vetor de ementas de um mesmo tribunal ou de vários (vetor 'tribunal')
limpar_ementa <- function(tribunal, ementa) {
  t <- normalizar(ementa)
  n <- length(t)
  orgao <- rep(NA_character_, n)

  # TJSP: texto após a última "Ementa:"
  sp <- tribunal == "TJSP"
  pos <- stri_locate_last_fixed(t[sp], "Ementa:")[, "start"]
  ok <- !is.na(pos)
  t[sp][ok] <- stri_trim_both(stri_sub(t[sp][ok], pos[ok] + 7L))

  # TJBA: cabeçalho com partes e órgão julgador
  ba <- which(tribunal == "TJBA" & stri_startswith_fixed(t, "PODER JUDICI"))
  if (length(ba)) {
    tb <- t[ba]
    m <- stri_match_first_regex(tb, RX_TJBA_ORGAO)[, 2]
    orgao[ba] <- stri_trans_toupper(aparar(m, " ;,."))
    j <- stri_locate_last_fixed(tb, "Advogado(s):")[, "start"]
    j2 <- stri_locate_first_fixed(tb, "Órgão Julgador:")[, "start"]
    j[is.na(j)] <- j2[is.na(j)]
    tem <- !is.na(j)
    resto <- stri_sub(tb[tem], j[tem])
    m2 <- stri_locate_first_regex(resto, RX_TJBA_INICIO)
    m3 <- stri_locate_first_regex(resto, "\\bDIREITO\\b")
    usa2 <- !is.na(m2[, "start"]) & (is.na(m3[, "start"]) | m2[, "start"] < m3[, "start"])
    usa3 <- !usa2 & !is.na(m3[, "start"])
    novo <- resto
    novo[usa2] <- aparar(stri_sub(resto[usa2], m2[usa2, "end"] + 1L), " :")
    novo[usa3] <- stri_sub(resto[usa3], m3[usa3, "start"])
    tb[tem] <- novo
    t[ba] <- tb
  }

  # Cauda do acórdão (Vistos..., ACORDAM...): fora da contagem de palavras
  mc <- stri_locate_first_regex(t, RX_CAUDA, case_insensitive = TRUE)[, "start"]
  corpo <- ifelse(is.na(mc), t, stri_trim_both(stri_sub(t, 1L, mc - 1L)))
  placeholder <- stri_detect_regex(corpo, RX_PLACEHOLDER, case_insensitive = TRUE) & nchar(corpo) < 60

  data.frame(
    ementa_limpa = t,
    orgao_cabecalho = orgao,
    sem_ementa = placeholder | nchar(t) == 0,
    modelo_cnj = stri_detect_regex(t, RX_CNJ, case_insensitive = TRUE),
    n_palavras_ementa = ifelse(placeholder, 0L, contar_palavras(corpo)),
    stringsAsFactors = FALSE
  )
}

# Resultado do julgamento ------------------------------------------------------
# A ordem importa: o primeiro padrão que casa, na janela de maior prioridade,
# define o rótulo.
PADROES <- list(
  list(nome = "parcialmente provido", rx = paste0(
    "parcial(mente)?\\s+provid[oa]s?|provid[oa]s?\\s+em\\s+parte|provimento\\s+parcial|parcial\\s+provimento|",
    "d(ar|á|ão|eram|ou)(-lhes?)?\\s+(parcial\\s+provimento|provimento\\s+parcial|provimento,?\\s+em\\s+parte)|",
    "prov(er|ido|eram)\\s+parcialmente|parcialmente\\s+acolhid[oa]s?|acolhid[oa]s?\\s+(em\\s+parte|parcialmente)|",
    "acolher\\s+(em\\s+parte|parcialmente)|parcial\\s+acolhimento|acolhimento\\s+parcial|",
    "conhecid[oa]s?\\s+e\\s+parcialmente\\s+provid[oa]s?")),
  list(nome = "não admitido", rx = paste0(
    "n[aã]o\\s+admitid[oa]s?|inadmitid[oa]s?|inadmiss(ão|ível)\\s+do\\s+recurso|",
    "neg(a|ar|ado|ou)(-se)?\\s+seguimento|seguimento\\s+negado")),
  list(nome = "não provido", rx = paste0(
    "n[aã]o\\s+provid[oa]s?|nega(r|do|ram|-se|m|ndo)?(-lhes?)?\\s+(o\\s+)?provimento|desprovid[oa]s?|improvid[oa]s?|",
    "nego\\s+provimento|provimento\\s+negado|n[aã]o\\s+se\\s+d[aá]\\s+provimento|desprovimento|improvimento|",
    "n[aã]o\\s+prov(er|eu)|negou\\s+provimento|n[aã]o\\s+merece\\s+provimento|recurso\\s+(denegado|negado)\\b|",
    "n[aã]o\\s+provimento")),
  list(nome = "não conhecido", rx = paste0(
    "n[aã]o\\s+conhec(id[oa]s?|er|e|eu|imento)|n[aã]o\\s+se\\s+conhece|n[aã]o[- ]conhecimento|n[aã]o\\s+conhe[cç]")),
  list(nome = "prejudicado", rx = "prejudicad[oa]s?"),
  list(nome = "ordem", rx = paste0(
    "ordem\\s+(parcialmente\\s+)?(concedida|denegada)|",
    "(conced|deneg)(e|er|ar|ida|em|eram|ada|a|am|aram|ido|ou|ando)(-se)?\\s+(parcialmente\\s+)?(a\\s+)?(ordem|seguran[cç]a)|",
    "seguran[cç]a\\s+(concedida|denegada)|denega[cç][aã]o\\s+da\\s+(ordem|seguran[cç]a)|",
    "concess[aã]o\\s+(parcial\\s+)?da\\s+(ordem|seguran[cç]a)|(ordem|seguran[cç]a)\\s+(parcialmente\\s+)?(concedida|denegada)|",
    "hc\\s+(concedido|denegado)|habeas\\s+corpus\\s+(concedido|denegado)")),
  list(nome = "embargos", rx = paste0(
    "embargos\\s+(de\\s+declara[cç][aã]o\\s+)?(acolhidos|rejeitados|conhecidos\\s+e\\s+(acolhidos|rejeitados|desacolhidos))|",
    "(acolh|rejeit)(er|ar|ido|idos|ida|idas|eram|ados|ada|adas|am|ando)(-se)?\\s+(os\\s+|aos\\s+)?(embargos|aclarat)|",
    "n[aã]o\\s+acolh(er|idos|ido)\\s+os\\s+embargos|\\bacolhid[oa]s?\\b|\\brejeitad[oa]s?\\b|rejei[cç][aã]o\\s+dos\\s+embargos|",
    "acolhimento\\s+dos\\s+embargos|\\brejei[cç][aã]o\\s*\\.?\\s*$")),
  list(nome = "provido", rx = paste0(
    "\\bprovid[oa]s?\\b|d(ar|á|ão|eram|ou)(-lhes?)?\\s+(integral\\s+)?provimento|",
    "provimento\\s+(ao|do|aos|dos)\\s+(recurso|apelo|agravo|apela[cç])|\\bprov(er|eram|eu)\\b|provimento\\s+integral|",
    "conhecid[oa]s?\\s+e\\s+provid[oa]s?|recurso\\s+provido|senten[cç]a\\s+(reformada|cassada|anulada)|",
    "decis[aã]o\\s+(reformada|cassada|anulada)|\\bprovimento\\s*\\.?\\s*\\*?$")),
  list(nome = "procedente/improcedente", rx = "\\b(im)?procedente(s)?\\b|\\b(im)?proced[eê]ncia\\b"),
  list(nome = "sentença mantida", rx = paste0(
    "senten[cç]a\\s+(mantida|confirmada)|decis[aã]o\\s+(mantida|confirmada)|mantida\\s+a\\s+(senten[cç]a|decis[aã]o)|",
    "manuten[cç][aã]o\\s+da\\s+(senten[cç]a|decis[aã]o)|confirmar\\s+a\\s+senten[cç]a"))
)

RX_EMBARGOS_CTX <- "embarg|aclarat|declarat"
RX_ACORDAM <- "\\ba\\s?c\\s?o\\s?r\\s?d\\s?a\\s?m\\b"
RX_DISP <- "(iv\\.?\\s*[-–—]?\\s*dispositivo(\\s+e\\s+tese)?|\\bdispositivo\\s+e\\s+tese|\\bdispositivo\\b)"
RX_DISP_FIM <- paste0("(tese\\s+de\\s+julgamento|dispositivos\\s+relevantes|jurisprud[eê]ncia\\s+relevante|",
                      "legisla[cç][aã]o\\s+relevante|ac[oó]rd[aã]o\\s*:?\\s*vistos|\\bvistos\\b|\\ba\\s?c\\s?o\\s?r\\s?d\\s?a\\s?m\\b)")
RX_CASO <- "\\bi\\s*[.\\-–—)]?\\s*caso\\s+em\\s+exame"

# Trechos onde procurar o resultado, em ordem de prioridade (NA = trecho inexistente)
janelas <- function(tl) {
  n <- length(tl)
  ac <- stri_locate_last_regex(tl, RX_ACORDAM)[, "end"]
  acordam <- ifelse(is.na(ac), NA_character_, stri_sub(tl, ac + 1L, ac + 700L))

  d <- stri_locate_first_regex(tl, RX_DISP)[, "end"]
  resto <- ifelse(is.na(d), NA_character_, stri_sub(tl, d + 1L))
  f <- stri_locate_first_regex(resto, RX_DISP_FIM)[, "start"]
  dispositivo <- ifelse(is.na(d), NA_character_,
                        ifelse(is.na(f), stri_sub(resto, 1L, 600L), stri_sub(resto, 1L, f - 1L)))

  cs <- stri_locate_first_regex(tl, RX_CASO)[, "start"]
  cabecalho <- ifelse(is.na(cs), NA_character_, stri_sub(tl, pmax(1L, cs - 350L), cs - 1L))

  list(acordam = acordam, dispositivo = dispositivo, cabecalho = cabecalho,
       fim300 = stri_sub(tl, -300L), todo = tl)
}

classificar_resultado <- function(ementa_limpa, sem_ementa = rep(FALSE, length(ementa_limpa))) {
  tl <- stri_trans_tolower(ementa_limpa)
  res <- rep(NA_character_, length(tl))
  onde <- rep(NA_character_, length(tl))
  res[sem_ementa] <- "sem ementa"
  jan <- janelas(tl)
  for (w in names(jan)) {
    txt <- jan[[w]]
    for (p in PADROES) {
      idx <- which(is.na(res) & !is.na(txt))
      if (!length(idx)) break
      m <- stri_extract_first_regex(txt[idx], p$rx)
      hit <- !is.na(m)
      rotulo <- rep(p$nome, length(idx))
      if (p$nome == "embargos") {
        hit <- hit & stri_detect_regex(txt[idx], RX_EMBARGOS_CTX)
        rotulo <- ifelse(stri_detect_regex(m, "rejeit|n[aã]o\\s+acolh|desacolh"),
                         "embargos rejeitados", "embargos acolhidos")
      } else if (p$nome == "ordem") {
        rotulo <- ifelse(stri_detect_fixed(m, "deneg"), "ordem denegada", "ordem concedida")
      }
      hit[is.na(hit)] <- FALSE
      res[idx[hit]] <- rotulo[hit]
      onde[idx[hit]] <- w
    }
  }
  res[is.na(res)] <- "não identificado"
  data.frame(resultado = res, resultado_janela = onde, stringsAsFactors = FALSE)
}

CATEGORIAS <- c("provido", "parcialmente provido", "não provido", "não conhecido", "prejudicado",
                "não admitido", "ordem concedida", "ordem denegada", "embargos acolhidos",
                "embargos rejeitados", "procedente/improcedente", "sentença mantida",
                "sem ementa", "não identificado")

# Natureza do julgamento a partir do rótulo fino
TIPO_JULGAMENTO <- c(
  "provido" = "Recurso", "parcialmente provido" = "Recurso", "não provido" = "Recurso",
  "não conhecido" = "Recurso", "prejudicado" = "Recurso", "não admitido" = "Recurso",
  "sentença mantida" = "Recurso",
  "ordem concedida" = "Ação originária (HC/MS)", "ordem denegada" = "Ação originária (HC/MS)",
  "embargos acolhidos" = "Embargos de declaração", "embargos rejeitados" = "Embargos de declaração",
  "procedente/improcedente" = "Outra ação originária")

# Resultado de mérito (recursos e ações de habeas corpus / mandado de segurança)
RESULTADO_MERITO <- c(
  "provido" = "Êxito total", "ordem concedida" = "Êxito total",
  "parcialmente provido" = "Êxito parcial",
  "não provido" = "Sem êxito", "ordem denegada" = "Sem êxito", "sentença mantida" = "Sem êxito")

# Aplica limpeza e rótulo; devolve as colunas novas
processar <- function(tribunal, ementa) {
  l <- limpar_ementa(tribunal, ementa)
  r <- classificar_resultado(l$ementa_limpa, l$sem_ementa)
  out <- cbind(l[, setdiff(names(l), "ementa_limpa")], r)
  out$tipo_julgamento <- unname(TIPO_JULGAMENTO[out$resultado])
  out$resultado_merito <- unname(RESULTADO_MERITO[out$resultado])
  out$exito <- ifelse(is.na(out$resultado_merito), NA_real_,
                      ifelse(out$resultado_merito == "Sem êxito", 0, 1))
  out$n_palavras_ementa <- ifelse(out$n_palavras_ementa == 0, NA, out$n_palavras_ementa)
  out
}
