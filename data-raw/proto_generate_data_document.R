coldesc <- rep("", ncol(metainep))
names(coldesc) <- names(metainep)
cat(paste0('c(',paste0(names(metainep),collapse='="",\n'),'=""))'))
#copiar, colar e alterar descrição
vars <- c(grupo_id="id do grupo/página superior principal",
          grupo="Nome do Grupo de assuntos/página superior principal",
          grupo_url="url do Grupo/Página superior",
          assunto_id="id do assunto",
          assunto="Assunto - subgrupo de indicadores",
          assunto_url="url do assunto",
          id="id da tabela",
          tabela="Nome da tabela",
          periodo="Periodo de referencia da tabela",
          tab_url="URL da tabela")

glue::glue("#'   \\item{[colname]}{[coldesc]}",
           colname = names(vars),
           coldesc = vars,
           .open = "[",
           .close = "]")
