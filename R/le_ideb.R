#' Processa arquivo XLSX de resultados do IDEB com colunas hierárquicas
#'
#' @param caminho_arquivo Caminho para o arquivo .xlsx
#' @return Tibble com dados formatados
#' @export
le_ideb <- function(caminho_arquivo) {

  # Verificar dependências
  if (!requireNamespace("readxl", quietly = TRUE)) {
    stop("Por favor instale o pacote readxl: install.packages('readxl')")
  }

  # 1. Ler TODOS os cabeçalhos hierárquicos
  cabecalho <- readxl::read_excel(
    caminho_arquivo,
    sheet = 1,
    range = readxl::cell_rows(8:10),  # Ler todas as colunas nas linhas 8-10
    col_names = FALSE
  )


  # Combinar cabeçalhos hierárquicos
  colunas <- apply(cabecalho, 2, function(col) {
    paste(na.omit(col), collapse = "_")  # Combina hierarquia com "_"
  })

  # Ler dados (ignorando cabeçalhos hierárquicos)
  dados <- readxl::read_excel(
    caminho_arquivo,
    sheet = 1,
    skip = 10,  # Pular cabeçalhos
    col_names = colunas  # Usar nomes combinados
  )

  normalizar_nomes <- function(nomes) {
    nomes |>
      # Remover acentos e caracteres especiais
      iconv(to = "ASCII//TRANSLIT") |>
      # Converter para minúsculas
      tolower() |>
      # Substituir " de " por "_" primeiro
      gsub(pattern = " de ", replacement = "_") |>
      # Substituir espaços restantes por "_"
      gsub(pattern = "\\s+", replacement = "_") |>
      # Remover underscores duplicados
      gsub(pattern = "_+", replacement = "_") |>
      # Remover underscores no início/fim
      gsub(pattern = "^_|_$", replacement = "")|>
      # Remover referencia cruzada sobrescrita
      gsub(pattern = "_\\d$", replacement = "")|>
      # Remover referencia cruzada sobrescrita
      gsub(pattern = "_si$", replacement = "")|>
      # Remover _ apos vl_
      gsub(pattern = "_vl_", replacement = "_vl-")|>
      # Remover separacao _ nomes_indicadores a_[pm] ;
      gsub(pattern = "a_([pm])", replacement = "a-\\1")|>
      # Remover _ao_ o_\\d
      gsub(pattern = "o_a", replacement = "o-a")|>
      # Remover _ao_ o_\\d
      gsub(pattern = "o_(\\do)", replacement = "o-\\1")|>
      # Remover _ (.)_
      gsub(pattern = "_\\(.\\)_", replacement = "_")|>
      # Remover _ (.)_
      gsub(pattern = "r_r", replacement = "r-r")|>
    # adiciona nome do indicador IDEB
    gsub(pattern = "^vl_", replacement = "ideb_vl-")|>
    # adiciona nome do indicador IDEB ao inves de repetir ano nas metas,projecao
    gsub(pattern = "^\\d{4}", replacement = "meta-para-o-ideb")

  }

  # Aplicar a normalização
  colnames(dados) <- normalizar_nomes(colnames(dados))




  # Processar dados
  dados_long <- dados |>
    tidyr::pivot_longer(
      cols = -(1:4),
      names_to = c("detalhe","indicador","ano"),
      names_sep = "_",
      values_to = "valor"
    ) |>
    dplyr::mutate(
      codigo_municipio = stringr::str_pad(co_municipio, 7, "left", "0"),  # Garantir código de 7 dígitos
      nome_municipio = no_municipio,
      ano = as.integer(ano),
      valor = as.numeric(valor),
      indicador = dplyr::case_when(
        indicador == "vl-aprovacao" ~ "Taxa de Aprovação",
        grepl("indicador-rendimento",indicador) ~ "Indicador de Rendimento",
        indicador == "vl-nota-media" ~ "Nota SAEB",
        indicador == "vl-observado" ~ "IDEB",
        indicador == "vl-projecao" ~ "Meta para o IDEB",
        TRUE ~ indicador
      ),
      detalhe = dplyr::case_when(
        detalhe == "1o-ao-5o-ano" ~ "1ª à 5ª Série",
        detalhe == "1o" ~ "1ª Série",
        detalhe == "2o" ~ "2ª Série",
        detalhe == "3o" ~ "3ª Série",
        detalhe == "4o" ~ "4ª Série",
        detalhe == "5o" ~ "5ª Série",
        detalhe == "matematica" ~ "Matemática",
        detalhe == "lingua-portuguesa" ~ "Língua Portuguesa",
        TRUE ~ detalhe
      )
    ) |>
    dplyr::select(codigo_municipio, nome_municipio, ano, indicador, detalhe, valor)

return(dados_long)
}
