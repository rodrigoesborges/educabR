#' Processa arquivo CSV e dicion XLSX de resultados do censoescolar com colunas hierárquicas - NÃO TERMINADO!
#'
#' @param ano character , 1997-2024
#' @return Tibble com dados formatados
#' @export
le_censoescolar <- \(ano=2023) {

  censoescolarmeta <- educabR::metainep|>dplyr::filter(grepl("Microdados da Educação Básica",tabela,fixed = F))

  caminho_fonte <- (censoescolarmeta|>
    dplyr::filter(grepl(ano,tab_url)))$tab_url

  f <- tempfile()


  retry <- \(expr, isError=function(x) "try-error" %in% class(x), maxErrors=5, sleep=0) {
    attempts = 0
    retval = try(eval(expr))
    while (isError(retval)) {
      attempts = attempts + 1
      if (attempts >= maxErrors) {
        msg = sprintf("retry: too many retries [[%s]]", capture.output(str(retval)))
        stop(msg)
      } else {
        msg = sprintf("retry: error in attempt %i/%i [[%s]]", attempts,  maxErrors,
                      capture.output(str(retval)))
        warning(msg)

      }
      if (sleep > 0) Sys.sleep(sleep)
      retval = try(eval(expr))
    }
    return(retval)
  }

  retry(download.file(caminho_fonte,f,method="curl"),maxErrors = 5,sleep = 1)

  availf <- unzip(f,list=T)

  unzip(f,files = availf[grepl("microdados[^/]*csv",availf$Name),]$Name,junkpaths = T,
        exdir = dirname(f))

  caminho_arquivo <-
    paste0(dirname(f),"/",
           basename(availf[grepl("csv",availf$Name),]$Name[1]))

  ##dicionario
  unzip(f,files = availf[4,]$Name,junkpaths = T,
        exdir = dirname(f))

  caminho_dic <-
    paste0(dirname(f),"/",
           basename(availf[4,]$Name[1]))


  # Verificar dependências
  if (!requireNamespace("readxl", quietly = TRUE)) {
    stop("Por favor instale o pacote readxl: install.packages('readxl')")
  }

  # 1. Ler TODOS os cabeçalhos hierárquicos
  cabecalho <- readxl::read_excel(
    caminho_dic,
    sheet = 1,
    range = readxl::cell_rows(7:8),  # Ler todas as colunas nas linhas 8-10
    col_names = FALSE
  )


  # Combinar cabeçalhos hierárquicos
  colunas <- apply(cabecalho, 2, function(col) {
    paste(na.omit(col), collapse = "_")  # Combina hierarquia com "_"
  })

  # Ler dados (ignorando cabeçalhos hierárquicos)
  dicdados <- readxl::read_excel(
    caminho_dic,
    sheet = 1,
    skip = 9,  # Pular cabeçalhos
    col_names = colunas  # Usar nomes combinados
  )


  microdados <- data.table::fread(caminho_arquivo)




  # Processar dados
  dados_long <- dados |>
    tidyr::pivot_longer(
      cols = -(1:4),
      names_to = c("detalhe","indicador","ano"),
      names_sep = "_",
      values_to = "valor"
    ) |>
    dplyr::mutate(
      codigo_municipio = as.numeric(co_municipio),
      nome_municipio = no_municipio,
      ano = as.integer(ano),
      valor = as.numeric(valor),
      indicador = dplyr::case_when(
        indicador == "vl-aprovacao" ~ "Taxa de Aprovação",
        grepl("indicador-rend",indicador) ~ "Indicador de Rendimento",
        indicador == "vl-nota-media" ~ "Nota SAEB",
        grepl("nota",indicador) ~ "Nota SAEB",
        indicador == "vl-observado" ~ "censoescolar",
        indicador == "vl-projecao" ~ "censoescolar",
        TRUE ~ indicador
      ),
      detalhe = dplyr::case_when(
        detalhe == "1o-ao-5o-ano" ~ "1ª à 5ª Série",
        detalhe == "1o" ~ "1ª Série",
        detalhe == "2o" ~ "2ª Série",
        detalhe == "3o" ~ "3ª Série",
        detalhe == "4o" ~ "4ª Série",
        detalhe == "5o" ~ "5ª Série",
        detalhe == "6o-a-9o-ano" ~ "6ª à 9ª Série",
        detalhe == "6o" ~ "6ª Série",
        detalhe == "7o" ~ "7ª Série",
        detalhe == "8o" ~ "8ª Série",
        detalhe == "9o" ~ "9ª Série",
        detalhe == "matematica" ~ "Matemática",
        detalhe == "lingua-portuguesa" ~ "Língua Portuguesa",
        detalhe == "1a" ~ "1ª Série do Ensino Médio",
        detalhe == "2a" ~ "2ª Série do Ensino Médio",
        detalhe == "3a" ~ "3ª Série do Ensino Médio",
        detalhe == "4a" ~ "4ª Série do Ensino Médio",
        detalhe == "total" ~ "Ensino Médio (Total)",
        grepl("media",detalhe) ~ "Nota Média Padronidaza",
        grepl("meta",detalhe) ~ "Meta para o censoescolar",
        grepl("censoescolar",detalhe) ~ "censoescolar",
        grepl("rend",detalhe) ~ "Taxa de aprovação Média (Indicador de Rendimento)",

        TRUE ~ detalhe
      )
    ) |>
    dplyr::select("codigo_municipio", "nome_municipio",
                  "rede","ano", "indicador", "detalhe", "valor")|>
    dplyr::filter(!is.na(codigo_municipio))

  if(replica){
    dados_long <- dados_long|>
      dplyr::bind_rows(dados_long|>dplyr::mutate(ano=ano+1))|>
      dplyr::arrange(codigo_municipio,ano)
  }
return(dados_long)
}
