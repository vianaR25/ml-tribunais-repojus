# Figuras (ggplot2) ------------------------------------------------------------
# Tamanho de coluna IEEE: 3,5 polegadas de largura. Os nomes dos arquivos são os
# usados em paper/main.tex.

library(ggplot2)

COL <- 3.5
CORES_TRIB <- c(TJSP = "#1f77b4", TJES = "#ff7f0e", TJPA = "#2ca02c", TJBA = "#d62728")
CORES_MERITO <- c("Êxito total" = "#2a9d8f", "Êxito parcial" = "#e9c46a", "Sem êxito" = "#e76f51")
virgula <- scales::label_number(decimal.mark = ",", big.mark = ".")
pct <- function(x) paste0(format(round(x), big.mark = ".", decimal.mark = ","), "%")

tema <- theme_bw(base_size = 8) +
  theme(panel.grid.minor = element_blank(), legend.key.size = unit(8, "pt"),
        strip.background = element_rect(fill = "grey95"))

salvar <- function(g, pasta, nome, largura = COL, altura = 2.4) {
  dir.create(pasta, showWarnings = FALSE, recursive = TRUE)
  ggsave(file.path(pasta, paste0(nome, ".png")), g, width = largura, height = altura, dpi = 300)
}

fig_resultado_tribunal <- function(a, pasta) {
  d <- a |>
    dplyr::filter(!is.na(resultado_merito)) |>
    dplyr::count(tribunal, resultado_merito) |>
    dplyr::group_by(tribunal) |>
    dplyr::mutate(pct = n / sum(n) * 100, total = sum(n)) |>
    dplyr::ungroup() |>
    dplyr::mutate(resultado_merito = factor(resultado_merito, levels = names(CORES_MERITO)),
                  rotulo = sprintf("%s\n(n = %s)", tribunal, format(total, big.mark = ".", decimal.mark = ",")),
                  rotulo = factor(rotulo, levels = rev(unique(rotulo[order(tribunal)]))))
  g <- ggplot(d, aes(x = pct, y = rotulo, fill = resultado_merito)) +
    geom_col(width = 0.8, position = position_stack(reverse = TRUE)) +
    geom_text(aes(label = ifelse(pct > 7, pct(pct), "")), size = 2.4,
              position = position_stack(vjust = 0.5, reverse = TRUE)) +
    scale_fill_manual(values = CORES_MERITO, name = NULL) +
    scale_x_continuous(limits = c(0, 100.01), expand = expansion(mult = c(0, 0.03))) +
    labs(x = "Julgamentos de mérito (%)", y = NULL) +
    tema + theme(legend.position = "bottom")
  salvar(g, pasta, "fig1_resultado_tribunal", altura = 2.3)
}

fig_exito_classe <- function(a, pasta, min_n = 30) {
  d <- a |>
    dplyr::filter(!is.na(exito), classe_grupo != "Outras") |>
    dplyr::group_by(classe_grupo, tribunal) |>
    dplyr::summarise(n = dplyr::n(), taxa = mean(exito) * 100, .groups = "drop") |>
    dplyr::filter(n >= min_n)
  ordem <- a |> dplyr::filter(!is.na(exito), classe_grupo %in% d$classe_grupo) |>
    dplyr::count(classe_grupo, sort = TRUE) |> dplyr::pull(classe_grupo) |> utils::head(7)
  d <- d |> dplyr::filter(classe_grupo %in% ordem) |>
    dplyr::mutate(classe_grupo = factor(classe_grupo, levels = rev(ordem)))
  g <- ggplot(d, aes(x = taxa, y = classe_grupo, fill = tribunal)) +
    geom_col(position = position_dodge2(reverse = TRUE, width = 0.85), width = 0.85) +
    scale_fill_manual(values = CORES_TRIB, name = NULL) +
    labs(x = "Taxa de êxito (%)", y = NULL) +
    tema + theme(legend.position = "bottom")
  salvar(g, pasta, "fig2_exito_classe", altura = 2.9)
}

fig_tempo_tribunal <- function(p, pasta, lim = 15) {
  d <- p |>
    dplyr::mutate(t = pmin(tempo_tramitacao, lim)) |>
    dplyr::count(tribunal, t) |>
    dplyr::group_by(tribunal) |> dplyr::mutate(pct = n / sum(n) * 100) |> dplyr::ungroup()
  g <- ggplot(d, aes(t, pct, colour = tribunal)) +
    geom_line() + geom_point(size = 0.8) +
    scale_colour_manual(values = CORES_TRIB, name = NULL) +
    scale_x_continuous(breaks = c(0, 5, 10, lim), labels = c(0, 5, 10, paste0(lim, "+"))) +
    labs(x = ROTULOS[["tempo_tramitacao"]], y = "Processos do tribunal (%)") + tema
  salvar(g, pasta, "fig3_tempo_tribunal")
}

fig_ementa_tribunal <- function(a, pasta) {
  d <- a |> dplyr::filter(!is.na(n_palavras_ementa)) |>
    dplyr::mutate(Modelo = factor(ifelse(modelo_cnj, "CNJ", "Tradicional"), levels = c("Tradicional", "CNJ")))
  g <- ggplot(d, aes(tribunal, n_palavras_ementa, fill = Modelo)) +
    geom_boxplot(outlier.size = 0.2, linewidth = 0.3) +
    scale_y_log10(labels = virgula) +
    scale_fill_manual(values = c(Tradicional = "grey75", CNJ = "#ff7f0e"), name = NULL) +
    labs(x = NULL, y = "Ementa (palavras, escala log)") +
    tema + theme(legend.position = "bottom")
  salvar(g, pasta, "fig4_ementa_tribunal", altura = 2.5)
}

fig_relator_carga_exito <- function(r, pasta) {
  rot <- r |> dplyr::group_by(tribunal) |>
    dplyr::summarise(rho = suppressWarnings(stats::cor(n_decisoes, taxa_exito, method = "spearman")),
                     n = dplyr::n(), .groups = "drop") |>
    dplyr::mutate(titulo = sprintf("%s (n = %d, ρ = %s)", tribunal, n, format(round(rho, 2), decimal.mark = ",")))
  d <- dplyr::left_join(r, rot, by = "tribunal") |>
    dplyr::mutate(titulo = factor(titulo, levels = rot$titulo[order(rot$tribunal)]))
  g <- ggplot(d, aes(n_decisoes, taxa_exito, colour = tribunal)) +
    geom_point(size = 0.8, alpha = 0.7, show.legend = FALSE) +
    facet_wrap(~titulo, nrow = 1, scales = "free_x") +
    scale_colour_manual(values = CORES_TRIB) +
    labs(x = "Decisões do relator no ano", y = "Taxa de êxito (%)") + tema
  salvar(g, pasta, "fig5_relator_carga_exito", largura = COL * 2, altura = 2.1)
}

fig_qq <- function(a, p, pasta, semente) {
  set.seed(semente)
  amostra <- function(x, k = 3000) { x <- x[!is.na(x)]; if (length(x) > k) sample(x, k) else x }
  fontes <- list(tempo_tramitacao = p$tempo_tramitacao, n_palavras_ementa = a$n_palavras_ementa,
                 carga_relator = a$carga_relator)
  d <- do.call(rbind, lapply(names(fontes), function(v) {
    x <- amostra(fontes[[v]])
    rbind(data.frame(variavel = CURTO[[v]], escala = "original", valor = x),
          data.frame(variavel = CURTO[[v]], escala = "log(1+x)", valor = log1p(x)))
  }))
  d$variavel <- factor(d$variavel, levels = CURTO[VARS])
  d$escala <- factor(d$escala, levels = c("original", "log(1+x)"))
  g <- ggplot(d, aes(sample = valor)) +
    stat_qq(size = 0.3, colour = "#1f4e9c") + stat_qq_line(colour = "red", linewidth = 0.4) +
    facet_wrap(escala ~ variavel, scales = "free", nrow = 2,
               labeller = labeller(.multi_line = FALSE)) +
    labs(x = "Quantis teóricos", y = "Quantis observados") + tema
  salvar(g, pasta, "fig6_qq", largura = COL * 2, altura = 3.2)
}

fig_spearman <- function(a, pasta) {
  cols <- c(VARS, "exito")
  m <- stats::cor(a[, cols], method = "spearman", use = "pairwise.complete.obs")
  rot <- c(CURTO[VARS], exito = "Êxito")
  d <- as.data.frame(as.table(m))
  d$Var1 <- factor(rot[as.character(d$Var1)], levels = rot)
  d$Var2 <- factor(rot[as.character(d$Var2)], levels = rev(rot))
  g <- ggplot(d, aes(Var1, Var2, fill = Freq)) +
    geom_tile() + geom_text(aes(label = format(round(Freq, 2), decimal.mark = ",")), size = 2.4) +
    scale_fill_distiller(palette = "RdBu", limits = c(-1, 1), name = "ρ") +
    labs(x = NULL, y = NULL) + tema + coord_equal()
  salvar(g, pasta, "fig7_spearman", largura = COL, altura = 2.8)
}

fig_cnj_tribunal <- function(a, pasta) {
  d <- a |> dplyr::filter(!is.na(exito)) |>
    dplyr::group_by(tribunal, Modelo = ifelse(modelo_cnj, "CNJ", "Tradicional")) |>
    dplyr::summarise(taxa = mean(exito) * 100, .groups = "drop")
  g <- ggplot(d, aes(tribunal, taxa, fill = factor(Modelo, levels = c("Tradicional", "CNJ")))) +
    geom_col(position = "dodge", width = 0.75) +
    scale_fill_manual(values = c(Tradicional = "grey75", CNJ = "#ff7f0e"), name = NULL) +
    labs(x = NULL, y = "Taxa de êxito (%)") + tema + theme(legend.position = "bottom")
  salvar(g, pasta, "fig8_cnj_tribunal")
}

fig_exito_orgao <- function(o, pasta) {
  set.seed(1)
  g <- ggplot(o, aes(tribunal, taxa_exito)) +
    geom_boxplot(outlier.shape = NA, fill = "grey90", linewidth = 0.3) +
    geom_jitter(aes(colour = tribunal), width = 0.2, height = 0, size = 0.9, show.legend = FALSE) +
    scale_colour_manual(values = CORES_TRIB) +
    labs(x = NULL, y = "Taxa de êxito do órgão (%)") + tema
  salvar(g, pasta, "fig9_exito_orgao")
}

fig_volume_mensal <- function(a, pasta, ano) {
  d <- a |> dplyr::count(mes, tribunal)
  g <- ggplot(d, aes(substr(mes, 6, 7), n, colour = tribunal, group = tribunal)) +
    geom_line() + geom_point(size = 0.8) +
    scale_colour_manual(values = CORES_TRIB, name = NULL) +
    scale_y_continuous(labels = virgula) +
    labs(x = sprintf("Mês de julgamento (%d)", ano), y = "Acórdãos no RepoJus") + tema
  salvar(g, pasta, "fig10_volume_mensal")
  as.data.frame(tidyr::pivot_wider(d, names_from = tribunal, values_from = n))
}

fig_tempo_comarca <- function(cm, pasta) {
  set.seed(0)
  g <- ggplot(cm, aes(tribunal, tempo_mediana)) +
    geom_boxplot(outlier.shape = NA, fill = "grey90", linewidth = 0.3) +
    geom_jitter(aes(colour = tribunal, size = n), width = 0.25, height = 0, alpha = 0.5,
                show.legend = FALSE) +
    scale_size_area(max_size = 4) +
    scale_colour_manual(values = CORES_TRIB) +
    labs(x = NULL, y = "Tempo mediano da comarca (anos)") + tema
  salvar(g, pasta, "fig11_tempo_comarca", altura = 2.5)
}

fig_tempo_assunto <- function(s, pasta, top = 10) {
  curto <- function(x) {
    x <- stringr::str_to_sentence(x)
    x <- sub(" e condutas afins", "", x)
    x <- sub("Iptu/ imposto predial e territorial urbano", "IPTU", x)
    ifelse(nchar(x) > 30, paste0(substr(x, 1, 28), "…"), x)
  }
  ordem <- s |> dplyr::group_by(assunto) |> dplyr::summarise(n = sum(n)) |>
    dplyr::arrange(dplyr::desc(n)) |> utils::head(top) |> dplyr::pull(assunto)
  d <- s |> dplyr::filter(assunto %in% ordem) |>
    dplyr::mutate(assunto = factor(curto(assunto), levels = rev(curto(ordem))))
  g <- ggplot(d, aes(pct_longa, assunto, fill = tribunal)) +
    geom_col(position = position_dodge2(reverse = TRUE, preserve = "single", width = 0.85), width = 0.85) +
    scale_fill_manual(values = CORES_TRIB, name = NULL) +
    scale_x_continuous(limits = c(0, 100), breaks = seq(0, 100, 25)) +
    labs(x = "Processos com 5 anos ou mais (%)", y = NULL) +
    tema + theme(legend.position = "bottom")
  salvar(g, pasta, "fig12_tempo_assunto", altura = 3.1)
}
