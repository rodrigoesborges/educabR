## code to prepare `metainep` dataset goes here

##inep metadados
options(chromote.headless = "new")

metabaseurl <- "https://www.gov.br/inep/pt-br/areas-de-atuacao/pesquisas-estatisticas-e-indicadores/"

page <- xml2::read_html(metabaseurl)

grupos <- data.frame(
  grupo = paste0("Área de ",
                 page|>
                   rvest::html_element(
                     xpath = "//h2[@class = 'outstanding-title']")|>
                   rvest::html_text()),
  url = metabaseurl
)

getcards <- \(id){
  assuntosraw <- xml2::read_html(grupos$url[id])|>
    rvest::html_elements(xpath="//a[@class = 'govbr-card-content']")

  data.frame(
    grupo_id = id,
    assunto = assuntosraw|>
      rvest::html_text()|>
      gsub(pattern="\\n[ ]+",replacement=""),
    url=assuntosraw|>
      rvest::html_attr("href")
  )
}


assuntos <- getcards(1)

#Separating as high level theme with cards
## 4 - indicadores-educacionais/resultados não existe -> subgrupo
## 6 - subgrupo sem 'resultados
# 9 - subgrupo sem 'resultados
grupos <- rbind(grupos,
                assuntos[c(4,6,9),-1]|>
                  dplyr::rename_with(.fn=\(x){gsub("assunto","grupo",x)}))

grupos$id <- 1:nrow(grupos)


## 5 - restrito // not available for general public, removing as others

#assuntos <- assuntos[-c(4,5,6,9),]

assuntos <- data.table::rbindlist(lapply(1:nrow(grupos),getcards))

assuntos <- assuntos[-c(4,5,6,9),]

###ASSUNTOS 24 TO 29 REMOVED
# Either like estatisticas-educacionais-internacionais/historico" empty ..."Aguarde!"
# ... or historico without tables
# ... or only documentation (pdfs)

assuntos$id <- 1:nrow(assuntos)


assuntos <- assuntos[1:23]


scrapeass <- \(assunto) {

  suburl <- "/resultados"
  burl <- paste0(assuntos[assunto,]$url,suburl)
  print(burl)
  checasubres <- httr::GET(burl)
  if(checasubres$status_code==404){
    burl <- gsub(suburl,"",burl)
  }
  pinfo <- rvest::read_html_live(burl)

  ##Check if there is more than one tab element for results :
  abasres <- pinfo|>
    rvest::html_elements(css="#content-core > div.govbr-tabs.swiper-container-initialized.swiper-container-horizontal.swiper-container-free-mode > div.tabs > div > a")|>
    rvest::html_attr("data-id")


  if (length(abasres) <= 1) {
  tabelaurl <- pinfo|>
    rvest::html_elements(xpath='//a[contains(@href,"zip")]')|>
    rvest::html_attr("href")

  tabelalabels <- (pinfo|>
                     rvest::html_elements(xpath='//a[contains(@href,"zip")]')|>
                     rvest::html_text2())

  tabelaurlpdf <- pinfo|>
    rvest::html_elements(xpath='//a[contains(@href,"pdf")]')|>
    rvest::html_attr("href")

  tabelalabelspdf <- (pinfo|>
                     rvest::html_elements(xpath='//a[contains(@href,"pdf")]')|>
                     rvest::html_text2())

    pinfo$session$close()

    data.frame(
    assunto_id=assunto,
    periodo=gsub(".*(\\d{4})[^\\d].*","\\1",c(tabelaurl,tabelaurlpdf)),
    taburl=c(tabelaurl,tabelaurlpdf),
    tabela=c(tabelalabels,tabelalabelspdf)
  )
  } else {
  numabas <- abasres
  #[!is.na(as.numeric(abasres))]


  zipsassunto <- \(naba) {

    #localizador <- paste0('div.tab.swiper-slide-next ',paste0(rep('+ div.tab',naba-diminui),collapse= " "),' > a')
    localizador <- paste0("div.tab > a[data-id='",naba,"']")
    print(localizador)
  pinfo$click(localizador)
  Sys.sleep(1)
  }


  #Clica em todas as abas para carregar os links

    try(lapply(numabas,zipsassunto))

  if(2015 %in% numabas){
    pinfo$click("div.tab > a[data-id='2015']")
    Sys.sleep(4)
  }

    tabelaurlzip <- pinfo|>
      rvest::html_elements(xpath='//a[contains(@href,"zip")]')|>
      rvest::html_attr("href")

    tabelalabelszip <- (pinfo|>
                       rvest::html_elements(xpath='//a[contains(@href,"zip")]')|>
                       rvest::html_text())

    tabelaurlxls <- pinfo|>
      rvest::html_elements(xpath='//a[contains(@href,"xls")]')|>
      rvest::html_attr("href")

    tabelalabelxls <- (pinfo|>
                          rvest::html_elements(xpath='//a[contains(@href,"xls")]')|>
                          rvest::html_text())

    tabelaurlpdf <- pinfo|>
      rvest::html_elements(xpath='//a[contains(@href,".pdf")]')|>
      rvest::html_attr("href")

    tabelaurlpdf <- tabelaurlpdf[!grepl("politica_e_plano_de_dados",tabelaurlpdf)]

    tabelalabelpdf <- (pinfo|>
                         rvest::html_elements(xpath='//a[contains(@href,"pdf")]')|>
                         rvest::html_text())

    tabelalabelpdf <- tabelalabelpdf[!grepl("Plano de Dados Abertos",tabelalabelpdf)]

    pinfo$session$close()

  urlstab <- c(tabelaurlzip,tabelaurlxls,tabelaurlpdf)
  labeltabs <- c(tabelalabelszip,tabelalabelxls,tabelalabelpdf)



    tabelascrps <- data.frame(
      assunto_id=assunto,
      periodo=gsub(".*(\\d{4})[^\\d].*","\\1",urlstab),
      taburl=urlstab,
      tabela=labeltabs
    )

    tabelascrps
}



}

tabelas <-
  data.frame(
    assunto_id=numeric(),
    taburl=character(),
    tabela=character()
  )

#for (i in assuntos$id) {
for (i in 1:nrow(assuntos)) {
  tabelascrps <- scrapeass(i)
  tabelas <- rbind(tabelas,tabelascrps)
}


tabelas <- dplyr::distinct(tabelas,taburl,.keep_all = TRUE)




tabelas$id <- 1:nrow(tabelas)

metainep <- tabelas|>
  dplyr::left_join(assuntos|>dplyr::rename(assunto_url=url),
                   by=c("assunto_id"="id"))|>
  dplyr::left_join(grupos|>dplyr::rename(grupo_url=url),
                   by=c("grupo_id"="id"))|>
  dplyr::select(grupo_id,grupo,grupo_url,assunto_id,assunto,assunto_url,id,tabela,periodo,tab_url=taburl)



usethis::use_data(metainep, overwrite = TRUE)

