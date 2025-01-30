## code to prepare `metaideb` dataset goes here
idebtab <- le_ideb()
idebfinais <- le_ideb(nivel="finais")

idebmeta <- \(tabela=idebtab){
  c(
    codigo_municipio = "",
    nome_municipio = "",
    apply(tabela|>dplyr::select(-1:-2,-valor),2,unique),
    valor=""
  )
}

metaideb <- list(
  iniciais = idebmeta(),
  finais = idebmeta(idebfinais)
)
usethis::use_data(metaideb, overwrite = TRUE)
