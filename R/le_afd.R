#' Processa arquivo XLSX de resultados da AFD com colunas hierárquicas
#'
#' @param ano number , year
#' @param regiao character , one of escolas,municipios, ufs
#' @param localizacao character, one of "total","urbana","rural"
#' @param dependencia character, one of "total","publica","privada","estadual","municipal"
#' @param nivel character, one of infantil,ensino_fundamental,ensino_medio,eja_fundamental,eja_medio
#' @param subnivel character, one of total,anos_iniciais,anos_finais
#' @param cache_dir character, optional directory to cache downloads
#'   (avoids re-downloading from INEP CDN which rate-limits sequential calls)
#' @return Tibble com dados formatados
#' @export
le_afd <- \(ano=2024,regiao="municipios",localizacoes='total',dependencias='total',niveis="ensino_medio",subniveis='total',cache_dir=NULL) {

  afdmeta <- educabR::metainep|>dplyr::filter(grepl("Adequação",assunto,fixed = F))

  caminho_fonte <- (afdmeta|>
                      dplyr::filter(grepl(regiao,tolower(tab_url)),grepl(ano,tab_url)))$tab_url

  retry <- \(expr, maxErrors=5, sleep=1) {
    attempts = 0
    retval = try(eval(expr))
    while ("try-error" %in% class(retval)) {
      attempts = attempts + 1
      if (attempts >= maxErrors) stop("retry: too many retries")
      if (sleep > 0) Sys.sleep(sleep)
      retval = try(eval(expr))
    }
    return(retval)
  }

  if (!is.null(cache_dir)) {
    f <- file.path(cache_dir, sprintf("AFD_%s_%s.zip", ano, regiao))
    if (!dir.exists(cache_dir)) dir.create(cache_dir, recursive=TRUE, showWarnings=FALSE)
    if (!file.exists(f) || file.size(f) < 10000) {
      retry({
        h <- curl::new_handle(
          ssl_verifypeer = 0, ssl_verifyhost = 0,
          http_version = 1, followlocation = 1,
          useragent = "Mozilla/5.0 (X11; Linux x86_64; rv:130.0) Gecko/20100101 Firefox/130.0"
        )
        curl::curl_download(caminho_fonte, f, handle = h)
      }, maxErrors = 5, sleep = 2)
    }
  } else {
    f <- tempfile(fileext = ".zip")
    retry({
      h <- curl::new_handle(
        ssl_verifypeer = 0, ssl_verifyhost = 0,
        http_version = 1, followlocation = 1,
        useragent = "Mozilla/5.0 (X11; Linux x86_64; rv:130.0) Gecko/20100101 Firefox/130.0"
      )
      curl::curl_download(caminho_fonte, f, handle = h)
    }, maxErrors = 5, sleep = 1)
  }

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
