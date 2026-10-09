# Leitura do RepoJus ----------------------------------------------------------
# Lê, com DuckDB, todos os acórdãos do ano dos tribunais configurados direto do
# Hugging Face (protocolo hf://), sem baixar a base inteira (~39 GB). Só as
# colunas leves e a ementa são lidas; do inteiro teor guarda-se apenas o
# tamanho. O resultado de cada tribunal fica em cache em dados/.

library(DBI)
library(duckdb)

sql_data <- function(col) sprintf("TRY_CAST(left(CAST(%s AS VARCHAR), 10) AS DATE)", col)
sql_nome <- function(col) sprintf("NULLIF(trim(json_extract_string(to_json(%s), '$.nome')), '')", col)

conectar <- function() {
  con <- dbConnect(duckdb())
  dbExecute(con, "INSTALL httpfs; LOAD httpfs; INSTALL json; LOAD json;")
  con
}

# Lista os arquivos Parquet de um tribunal no commit fixado (API do Hugging Face)
listar_arquivos <- function(trib, cfg = CONFIG) {
  sk <- TRIBUNAIS$source_key[TRIBUNAIS$tribunal == trib]
  pasta <- sprintf("data/v2/p_tribunal=%s/p_source_key=%s/p_tipo_decisao=%s/p_year=%d",
                   trib, sk, cfg$tipo_decisao, cfg$ano)
  if (nzchar(cfg$local_dir)) {
    arqs <- list.files(file.path(cfg$local_dir, pasta), pattern = "\\.parquet$",
                       recursive = TRUE, full.names = TRUE)
    return(arqs)
  }
  api <- sprintf("https://huggingface.co/api/datasets/%s/tree/%s/%s?recursive=true",
                 cfg$repo, cfg$sha, utils::URLencode(pasta))
  itens <- jsonlite::fromJSON(api)
  caminhos <- itens$path[itens$type == "file" & grepl("\\.parquet$", itens$path)]
  sprintf("hf://datasets/%s@%s/%s", cfg$repo, cfg$sha, sort(caminhos))
}

# Lê um tribunal (com cache em dados/<TRIB>_<ano>.parquet)
carregar_tribunal <- function(con, trib, cfg = CONFIG, forcar = FALSE) {
  cache <- file.path(cfg$dir_dados, sprintf("%s_%d.parquet", trib, cfg$ano))
  if (!file.exists(cache) || forcar) {
    dir.create(cfg$dir_dados, showWarnings = FALSE, recursive = TRUE)
    urls <- listar_arquivos(trib, cfg)
    if (!length(urls)) stop("Nenhum arquivo encontrado para ", trib)
    message(sprintf("   %s: lendo %d arquivos do RepoJus...", trib, length(urls)))
    lista <- paste0("[", paste0("'", urls, "'", collapse = ", "), "]")
    dbExecute(con, sprintf("
      COPY (
        SELECT * FROM (
          SELECT normalized_hash, tribunal, tipo_decisao, processo,
                 %s AS data_julgamento,
                 %s AS data_publicacao,
                 NULLIF(trim(relator), '') AS relator,
                 %s AS orgao_julgador,
                 %s AS orgao_colegiado,
                 %s AS classe,
                 len(assuntos) AS n_assuntos,
                 NULLIF(trim(json_extract_string(to_json(assuntos), '$[0].nome')), '') AS assunto,
                 ementa,
                 length(ementa) AS n_chars_ementa_bruta,
                 length(texto_integral) AS n_chars_texto
          FROM read_parquet(%s, union_by_name = true)
        )
        WHERE year(data_julgamento) = %d AND tribunal = '%s'
        QUALIFY row_number() OVER (PARTITION BY normalized_hash ORDER BY normalized_hash) = 1
      ) TO '%s' (FORMAT parquet)",
      sql_data("datajulgamento"), sql_data("datapublicacao"),
      sql_nome("orgaojulgador"), sql_nome("orgaojulgadorcolegiado"), sql_nome("classe"),
      lista, cfg$ano, trib, cache))
  }
  # O hash da ementa normalizada identifica cópias exatas sem guardar duas vezes o texto.
  dbGetQuery(con, sprintf("
    SELECT *, md5(trim(regexp_replace(coalesce(ementa, ''), '\\s+', ' ', 'g'))) AS ementa_md5
    FROM read_parquet('%s')", cache))
}

# Todos os tribunais configurados, num único data.frame
carregar <- function(con, cfg = CONFIG, forcar = FALSE) {
  partes <- lapply(ORDEM_TRIBUNAIS, function(t) carregar_tribunal(con, t, cfg, forcar))
  do.call(rbind, partes)
}
