# Parâmetros do recorte ------------------------------------------------------
# Mudar qualquer valor aqui muda a base lida e, portanto, todos os números do
# artigo. As pastas podem ser trocadas por variáveis de ambiente (usado nos
# testes).

CONFIG <- list(
  repo           = "andrebadini/repojus",
  sha            = "5791ee745a5f63a9280bcf7e94e0242eaae88911",  # commit fixado
  ano            = 2025L,      # a partição p_year segue a data de julgamento
  tipo_decisao   = "acordao",
  semente        = 42L,
  alfa           = 0.05,
  min_relator    = 30L,        # mínimo de julgamentos de mérito por relator
  min_categoria  = 30L,        # mínimo de casos por órgão, comarca ou assunto
  anos_longa     = 5L,         # tramitação longa: 5 anos ou mais
  local_dir      = Sys.getenv("REPOJUS_LOCAL_DIR", ""),
  dir_dados      = Sys.getenv("AP1_DIR_DADOS", "dados"),
  dir_resultados = Sys.getenv("AP1_DIR_RESULTADOS", "resultados"),
  dir_artigo     = Sys.getenv("AP1_DIR_ARTIGO", "paper")
)

# Tribunais analisados: fonte no RepoJus e código TR da numeração única
# (Resolução CNJ 65/2008).
TRIBUNAIS <- data.frame(
  tribunal   = c("TJSP", "TJES", "TJPA", "TJBA"),
  source_key = c("tjsp_acordaos", "tjes_jurisprudencia_acordaos",
                 "tjpa_jurisprudencia_acordaos", "tjba_jurisprudencia_acordaos"),
  tr         = c("26", "08", "14", "05"),
  nome       = c("Tribunal de Justiça de São Paulo",
                 "Tribunal de Justiça do Espírito Santo",
                 "Tribunal de Justiça do Pará",
                 "Tribunal de Justiça da Bahia"),
  stringsAsFactors = FALSE
)
ORDEM_TRIBUNAIS <- TRIBUNAIS$tribunal
