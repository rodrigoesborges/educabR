#' Processa arquivo CSV e dicion XLSX de resultados do idadeserie com colunas hierárquicas - NÃO TERMINADO!
#'
#' @param ano character , 1997-2024
#' @return Tibble com dados formatados
#' @export
le_idadeserie <- \(ano=2023) {

  idadeseriemeta <- educabR::metainep|>dplyr::filter(grepl("Taxas de Distorção Idade-série",assunto,fixed=FALSE),grepl("Munic",tabela))

  caminho_fonte <- (idadeseriemeta|>
    dplyr::filter(grepl(ano,tab_url)))$tab_url
  print(caminho_fonte)

  f <- tempfile(fileext = ".zip")


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

  retry(download.file(caminho_fonte,f,method="curl",extra = '-k'),maxErrors = 5,sleep = 1)

  availf <- unzip(f,list=T)

  unzip(f,files = availf[grepl("TDI.*[^/]*xls",availf$Name),]$Name,junkpaths = T,
        exdir = dirname(f))

  caminho_arquivo <-
    paste0(dirname(f),"/",
           basename(availf[grepl("xls",availf$Name),]$Name[1]))


  # Verificar dependências
  if (!requireNamespace("readxl", quietly = TRUE)) {
    stop("Por favor instale o pacote readxl: install.packages('readxl')")
  }

  # 1. Ler TODOS os cabeçalhos hierárquicos
  cabecalho <- suppressMessages(readxl::read_excel(
    caminho_arquivo,
    sheet = 1,
    range = readxl::cell_rows(6:8),  # Ler todas as colunas nas linhas 8-10
    col_names = FALSE
  ))

  cabecalho <- cabecalho|>
    t()|>as.data.frame()|>
    tidyr::fill(dplyr::everything(),.direction='down')|>
    t()|>tibble::as_tibble()

  # Combinar cabeçalhos hierárquicos
  colunas <- apply(cabecalho, 2, function(col) {
    paste(na.omit(col), collapse = "_")  # Combina hierarquia com "_"
  })


  # Ler codigos (ignorando cabeçalhos hierárquicos)
  codigos <- suppressMessages(readxl::read_excel(
    caminho_arquivo,
    sheet = 1,
    range = readxl::cell_rows(9),
    col_names = FALSE  # Usar nomes combinados
  ))

  codigos <- unlist(codigos,use.names = FALSE)

  dicdados <- data.frame(
    'codigo'=tolower(codigos),
    'nome'=gsub(" de 8 e 9 anos","",colunas)
  )

  microdados <- suppressMessages(readxl::read_excel(
    caminho_arquivo,
    sheet = 1,skip=8,col_names = TRUE))

  microdados <- microdados|>
    janitor::clean_names()


  valorsoma <- ifelse(ano==2015,1,0)

  # Processar dados
  dados_long <- microdados |>
    tidyr::pivot_longer(
      cols = -(1:(7+valorsoma)),
      names_to = 'codigo',
      values_to = "valor"
    ) |>
    dplyr::rename(
      codigo_municipio = 4+valorsoma,
      nome_municipio = 5+valorsoma,
      ano = 1,
      rede = 7+valorsoma,
      localizacao = 6+valorsoma)|>
    dplyr::mutate(
      valor=as.numeric(gsub(",",".",valor)),
      ano=as.integer(ano),
      codigo_municipio=as.integer(codigo_municipio),
      indicador = 'Taxa de Distorção Idade-Série'
    )|>
  dplyr::left_join(dicdados)|>
    ##HARMONIZA SAÍDA COM LE_IDEB
    #  tidyr::separate_wider_delim(nome,"_",names=c("etapa_de_ensino","detalhe"),too_few='align_start')|>
    dplyr::mutate(detalhe=paste(localizacao,nome,sep="_"))|>
    dplyr::select("codigo_municipio", "nome_municipio",
                  "rede","ano", "indicador", "detalhe", "valor")|>
    dplyr::filter(!is.na(codigo_municipio),!is.na(ano))

return(dados_long)
}
