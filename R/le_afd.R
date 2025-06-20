#' Processa arquivo XLSX de resultados da AFD com colunas hierárquicas
#'
#' @param ano number , year
#' @param regiao character , one of escolas,municipios, ufs
#' @param localizacao character, one of "total","urbana","rural"
#' @param dependencia character, one of "total","publica","privada","estadual","municipal"
#' @param nivel character, one of infantil,ensino_fundamental,ensino_medio,eja_fundamental,eja_medio
#' @param subnivel character, one of total,anos_iniciais,anos_finais
#' @return Tibble com dados formatados
#' @export
le_afd <- \(ano=2024,regiao="municipios",localizacoes='total',dependencias='total',niveis="ensino_medio",subniveis='total') {

  afdmeta <- educabR::metainep|>dplyr::filter(grepl("Adequação",assunto,fixed = F))

  caminho_fonte <- (afdmeta|>
                      dplyr::filter(grepl(regiao,tolower(tab_url)),,grepl(ano,tab_url)))$tab_url

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
        #warning(msg)

      }
      if (sleep > 0) Sys.sleep(sleep)
      retval = try(eval(expr))
    }
    return(retval)
  }

  retry(download.file(caminho_fonte,f,method="curl",quiet=TRUE),maxErrors = 5,sleep = 1)

  availf <- unzip(f,list=T)

  unzip(f,files = availf[grepl("xlsx",availf$Name),]$Name,junkpaths = T,
        exdir = dirname(f))

  caminho_arquivo <-
    paste0(dirname(f),"/",
           basename(availf[grepl("xlsx",availf$Name),]$Name[1]))
  # Verificar dependências
  if (!requireNamespace("readxl", quietly = TRUE)) {
    stop("Por favor instale o pacote readxl: install.packages('readxl')")
  }

  # 1. Ler TODOS os cabeçalhos hierárquicos
  cabecalho <- readxl::read_excel(
    caminho_arquivo,
    sheet = 1,
    range = readxl::cell_rows(7:10),  # Ler todas as colunas nas linhas 8-10
    col_names = FALSE
  )

  #Elimina primeira linha pela parte de 'percentual...'
  cabecalho[2,1:7] <- cabecalho[1,1:7]
  cabecalho <- cabecalho[-1,]
  #adiciona total para educacao infantil
  cabecalho[2,8] <- 'Total'

  #adiciona total para ensino médio
  cabecalho[2,28] <- 'Total'

  preenchecols <- \(df) {
    df <- df |>
    t() |>
    as.data.frame(stringsAsFactors = FALSE) |>
    tidyr::fill(everything(), .direction = "down") |>
    t() |>
    as.data.frame(stringsAsFactors = FALSE)
    return(df)
  }

  # Combinar cabeçalhos hierárquicos
  colunas <- apply(preenchecols(cabecalho), 2, function(col) {
    paste(na.omit(col), collapse = "9")  # Combina hierarquia com "."
  })

  # Ler dados (ignorando cabeçalhos hierárquicos)
  dados <- suppressWarnings(readxl::read_excel(
    caminho_arquivo,
    sheet = 1,
    na=c('','--'),
    skip = 11,  # Pular cabeçalhos
    col_names = colunas  # Usar nomes combinados
  ))|>janitor::clean_names()

  dados <- dados[!is.na(dados$ano),]

  ##Fixes for later separation and eja
  names(dados) <- gsub("(eja)-([^-]+)-","\\1_\\2-total-",gsub(9,"-",names(dados)))


  # Processar dados
  dados <- dados |>
    tidyr::pivot_longer(
      cols = -(1:7),
      names_to = c("nivel","subnivel","indicador_afd"),
      names_sep="-",
      values_to = "valor"
    ) |>
    dplyr::transmute(
      ano = as.integer(ano),
      regiao,
      uf=sigla,
      codigo_municipio = as.numeric(codigo_do_municipio),
      localizacao = stringi::stri_trans_general(tolower(localizacao),"latin-ascii"),
      dependencia_administrativa = stringi::stri_trans_general(tolower(dependencia_administrativa),"latin-ascii"),
      nivel,subnivel,indicador_afd,
      valor = as.numeric(valor))
  if(regiao=="municipios"){
    dados <- dados|>
      dplyr::filter(!is.na(codigo_municipio))
  }

  if(localizacoes %in% c("total","urbana","rural")){
    dados <- dados|>
      dplyr::filter(localizacao==localizacoes)
  }

  if(dependencias %in% c("total","publica","privada","estadual","municipal")) {
    dados <- dados|>
      dplyr::filter(dependencia_administrativa==dependencias)
  }

  if(niveis %in% c("infantil","ensino_fundamental","ensino_medio","eja_fundamental","eja_medio")) {
    dados <- dados|>
      dplyr::filter(nivel == niveis)
  }

  if(subniveis %in% c("total","anos_iniciais","anos_finais")) {
    dados <- dados|>
      dplyr::filter(subnivel == subniveis)
  }

  return(dados)
}
