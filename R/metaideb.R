#'   metaideb - basic metadata table - name , url, group/subject
#'   with auto-generated id - obtaining by applying le_ideb function
#'
#'   @format A list with two sublists iniciais and finais with similar structure:
#' \describe{
#'   \item{codigo_municipio}{codigo IBGE municipio}
#'   \item{nome_municipio}{Nome do Município}
#'   \item{rede}{Rede - Estadual, municipal, pública}
#'   \item{ano}{Ano}
#'   \item{indicador}{taxa de aprovação, Nota SAEB ,IDEB}
#'   \item{detalhe}{geral/por série; ponderada/por disciplina; valor x meta}
#'   \item{valor}{valor do indicador}
#'   }
#' @source \url{https://www.gov.br/inep/pt-br/areas-de-atuacao/pesquisas-estatisticas-e-indicadores/}
"metaideb"
