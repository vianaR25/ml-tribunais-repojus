# Estatística descritiva, normalidade, correlação e associação ---------------

moda <- function(x) {
  tab <- table(x)
  as.numeric(names(tab)[which.max(tab)])  # em empate, o menor valor
}

# Assimetria e curtose com correção para amostra (mesmas fórmulas do Excel)
assimetria <- function(x) {
  n <- length(x); m <- mean(x); s <- stats::sd(x)
  if (n < 3 || s == 0) return(NA_real_)
  n / ((n - 1) * (n - 2)) * sum(((x - m) / s)^3)
}
curtose <- function(x) {
  n <- length(x); m <- mean(x); s <- stats::sd(x)
  if (n < 4 || s == 0) return(NA_real_)
  n * (n + 1) / ((n - 1) * (n - 2) * (n - 3)) * sum(((x - m) / s)^4) -
    3 * (n - 1)^2 / ((n - 2) * (n - 3))
}

descrever <- function(x) {
  x <- x[!is.na(x)]
  q <- stats::quantile(x, c(.25, .75), names = FALSE)
  c(n = length(x), media = mean(x), mediana = stats::median(x), moda = moda(x),
    desvio = stats::sd(x), variancia = stats::var(x), min = min(x), q1 = q[1], q3 = q[2],
    max = max(x), amplitude = max(x) - min(x), iqr = q[2] - q[1],
    cv = stats::sd(x) / mean(x) * 100, assimetria = assimetria(x), curtose = curtose(x))
}

# Tempo de tramitação por processo; ementa e carga por decisão
descritiva <- function(a, p) {
  linhas <- list()
  for (g in c("Total", ORDEM_TRIBUNAIS)) {
    dp <- if (g == "Total") p else p[p$tribunal == g, ]
    da <- if (g == "Total") a else a[a$tribunal == g, ]
    for (v in VARS) {
      x <- if (v == "tempo_tramitacao") dp[[v]] else da[[v]]
      linhas[[length(linhas) + 1]] <- data.frame(grupo = g, variavel = v, t(descrever(x)))
    }
  }
  do.call(rbind, linhas)
}

# Shapiro-Wilk até 2.000 observações; Kolmogorov-Smirnov com correção de
# Lilliefors acima disso (regra adotada na disciplina).
testar <- function(x, alfa) {
  x <- x[!is.na(x)]
  if (length(x) < 3 || length(unique(x)) < 2)
    return(data.frame(teste = "—", n = length(x), estatistica = NA, p = NA, normal = "não se aplica"))
  if (length(x) > 2000) {
    r <- nortest::lillie.test(x); nome <- "KS-Lilliefors"
  } else {
    r <- stats::shapiro.test(x); nome <- "Shapiro-Wilk"
  }
  data.frame(teste = nome, n = length(x), estatistica = unname(r$statistic), p = r$p.value,
             normal = ifelse(r$p.value < alfa, "não", "sim"))
}

normalidade <- function(a, p, alfa) {
  linhas <- list()
  for (g in c("Total", ORDEM_TRIBUNAIS)) {
    dp <- if (g == "Total") p else p[p$tribunal == g, ]
    da <- if (g == "Total") a else a[a$tribunal == g, ]
    for (v in VARS) {
      x <- if (v == "tempo_tramitacao") dp[[v]] else da[[v]]
      for (escala in c("original", "log(1+x)")) {
        y <- if (escala == "original") x else log1p(x)
        linhas[[length(linhas) + 1]] <- cbind(data.frame(grupo = g, variavel = v, escala = escala), testar(y, alfa))
      }
    }
  }
  do.call(rbind, linhas)
}

normalidade_agregada <- function(d, vars, alfa) {
  linhas <- list()
  for (g in c("Total", ORDEM_TRIBUNAIS)) {
    dd <- if (g == "Total") d else d[d$tribunal == g, ]
    for (v in vars) linhas[[length(linhas) + 1]] <- cbind(data.frame(grupo = g, variavel = v), testar(dd[[v]], alfa))
  }
  do.call(rbind, linhas)
}

# Pearson e Spearman para cada par de variáveis
correlacoes <- function(d, pares, rotulos) {
  linhas <- list()
  for (par in pares) {
    x <- d[[par[1]]]; y <- d[[par[2]]]
    ok <- !is.na(x) & !is.na(y)
    x <- x[ok]; y <- y[ok]
    if (length(x) < 10 || length(unique(x)) < 2 || length(unique(y)) < 2) next
    pe <- stats::cor.test(x, y, method = "pearson")
    sp <- suppressWarnings(stats::cor.test(x, y, method = "spearman", exact = FALSE))
    linhas[[length(linhas) + 1]] <- data.frame(
      par = paste(rotulos[par[1]], "×", rotulos[par[2]]), n = length(x),
      pearson_r = unname(pe$estimate), p_r = pe$p.value,
      spearman_rho = unname(sp$estimate), p_rho = sp$p.value)
  }
  do.call(rbind, linhas)
}

por_grupo <- function(d, f) {
  out <- lapply(c("Total", ORDEM_TRIBUNAIS), function(g) {
    dd <- if (g == "Total") d else d[d$tribunal == g, ]
    r <- f(dd)
    if (is.null(r) || !nrow(r)) return(NULL)
    cbind(data.frame(grupo = g), r)
  })
  do.call(rbind, out)
}

PARES_DECISAO <- list(c("tempo_tramitacao", "n_palavras_ementa"), c("tempo_tramitacao", "carga_relator"),
                      c("carga_relator", "n_palavras_ementa"), c("exito", "tempo_tramitacao"),
                      c("exito", "n_palavras_ementa"), c("exito", "carga_relator"))
PARES_RELATOR <- list(c("n_decisoes", "taxa_exito"), c("n_decisoes", "ementa_mediana"),
                      c("pct_cnj", "ementa_mediana"), c("pct_cnj", "taxa_exito"),
                      c("tempo_mediana", "taxa_exito"), c("ementa_mediana", "taxa_exito"),
                      c("pct_criminal", "taxa_exito"))
ROT_RELATOR <- c(n_decisoes = "Carga", taxa_exito = "Taxa de êxito", ementa_mediana = "Ementa (mediana)",
                 pct_cnj = "% modelo CNJ", tempo_mediana = "Tramitação (mediana)", pct_criminal = "% criminal")
PARES_COMARCA <- list(c("n", "tempo_mediana"), c("n", "pct_longa"), c("pct_longa", "taxa_exito"))
ROT_COMARCA <- c(n = "Processos da comarca", tempo_mediana = "Tramitação (mediana)",
                 pct_longa = "% longa", taxa_exito = "Taxa de êxito")

# Qui-quadrado e V de Cramér
cramer_v <- function(tab) {
  ct <- suppressWarnings(stats::chisq.test(tab))
  n <- sum(tab); k <- min(dim(tab)) - 1
  data.frame(chi2 = unname(ct$statistic), gl = unname(ct$parameter), p = ct$p.value,
             n = n, cramer_v = sqrt(unname(ct$statistic) / (n * k)))
}

associar <- function(d, col, alvo, min_cat = 0) {
  d <- d[!is.na(d[[col]]) & !is.na(d[[alvo]]), ]
  if (min_cat > 0) {
    cont <- table(d[[col]])
    d <- d[d[[col]] %in% names(cont)[cont >= min_cat], ]
  }
  tab <- table(d[[col]], d[[alvo]])
  tab <- tab[rowSums(tab) > 0, colSums(tab) > 0, drop = FALSE]
  if (nrow(tab) < 2 || ncol(tab) < 2) return(NULL)
  cbind(data.frame(variavel = col, categorias = nrow(tab)), cramer_v(tab))
}

# Resultado de mérito × categóricas (por julgamento de mérito)
associacao_resultado <- function(a, min_cat) {
  m <- a[!is.na(a$resultado_merito), ]
  m$modelo_cnj <- ifelse(m$modelo_cnj, "CNJ", "Tradicional")
  tot <- do.call(rbind, list(
    associar(m, "tribunal", "resultado_merito"), associar(m, "classe_grupo", "resultado_merito"),
    associar(m, "materia", "resultado_merito"), associar(m, "modelo_cnj", "resultado_merito"),
    associar(m, "origem", "resultado_merito"), associar(m, "assunto", "resultado_merito", min_cat),
    associar(m, "comarca", "resultado_merito", min_cat)))
  trib <- lapply(ORDEM_TRIBUNAIS, function(t) {
    g <- m[m$tribunal == t, ]
    r <- do.call(rbind, list(
      associar(g, "classe_grupo", "resultado_merito"), associar(g, "modelo_cnj", "resultado_merito"),
      associar(g, "orgao", "resultado_merito", min_cat), associar(g, "assunto", "resultado_merito", min_cat),
      associar(g, "comarca", "resultado_merito", min_cat)))
    if (is.null(r)) NULL else cbind(data.frame(grupo = t), r)
  })
  rbind(cbind(data.frame(grupo = "Total"), tot), do.call(rbind, trib))
}

# Tramitação longa × categóricas (por processo de primeiro grau)
associacao_tempo <- function(p, min_cat) {
  m <- p[!is.na(p$tramitacao_longa), ]
  tot <- do.call(rbind, list(
    associar(m, "tribunal", "tramitacao_longa"), associar(m, "classe_grupo", "tramitacao_longa", min_cat),
    associar(m, "materia", "tramitacao_longa"), associar(m, "assunto", "tramitacao_longa", min_cat),
    associar(m, "comarca", "tramitacao_longa", min_cat)))
  trib <- lapply(ORDEM_TRIBUNAIS, function(t) {
    g <- m[m$tribunal == t, ]
    r <- do.call(rbind, list(
      associar(g, "classe_grupo", "tramitacao_longa", min_cat), associar(g, "assunto", "tramitacao_longa", min_cat),
      associar(g, "comarca", "tramitacao_longa", min_cat), associar(g, "orgao", "tramitacao_longa", min_cat)))
    if (is.null(r)) NULL else cbind(data.frame(grupo = t), r)
  })
  rbind(cbind(data.frame(grupo = "Total"), tot), do.call(rbind, trib))
}

# Taxa de êxito por categoria e tribunal
taxa_exito <- function(a, por) {
  a |>
    dplyr::filter(!is.na(exito), !is.na(.data[[por]])) |>
    dplyr::group_by(categoria = .data[[por]], tribunal) |>
    dplyr::summarise(n = dplyr::n(), taxa = mean(exito) * 100, .groups = "drop") |>
    as.data.frame()
}
