library(RISmed)
library(stringi)
library(tidyverse)
library(officer)

StyleBase <- fp_text(color = "#000000", font.size = 12, font.family = "Calibri", bold = FALSE)
StyleBichat <- fp_text(color = "#000000", font.size = 12, font.family = "Calibri", bold = TRUE)

# Tableau des auteurs de Bichat
Auteurs <- tibble(
  noms = c("Dupont", "Roy"),
  prenoms = c("Axelle", "Carine")
) %>% 
  mutate(requete = paste0(noms, " ", prenoms, "[Author]"))

# On fait la recherche
Query <- EUtilsSummary(paste(Auteurs$requete, collapse = " OR "), 
                       type = "esearch",
                       db = "pubmed",
                       datetype = "pdat",
                       mindate = 2021,
                       maxdate = 2021) # un warning, mais ça a l'air de marcher donc je ne sais pas trop ^^
# Si jamais tes warnings se tranforment en erreurs, tape en console options(warn = 0)
Articles <- EUtilsGet(Query)

# Recréer la norme Vancouver (s'il y a des erreurs, je te laisse changer, notamment s'il y a des autres types qu'articles, etc.)
ArticlesChar <- paste0(
  seq_len(Query@count), ". ",
  map_chr(Author(Articles),
          function(Tab) {
            Tab <- filter(Tab, !is.na(LastName))
            Noms <- stri_trans_general(paste0(Tab$LastName, " ", Tab$ForeName), "Latin-ASCII")
            Index <- which(str_to_upper(Noms) %in% str_to_upper(stri_trans_general(paste0(Auteurs$noms, " ", Auteurs$prenoms), "Latin-ASCII")))
            NomsDef <- stri_trans_general(paste0(Tab$LastName, " ", str_sub(Tab$ForeName, 1, 1)), "Latin-ASCII")
            NomsDef[Index] <- paste0("<ancre>", NomsDef[Index], "<ancre>")
            NomsDef <- paste(NomsDef, collapse = ", ")
            return(NomsDef)
          }) %>% as.character(),
  ". ", ArticleTitle(Articles), " ",
  ISOAbbreviation(Articles), ". ",
  YearArticleDate(Articles), " ; ", Volume(Articles), "(",
  Issue(Articles), ") : ", MedlinePgn(Articles), ". [PMID : ", PMID(Articles), "]"
)

# Mettre en forme pour officer
ArticleOfficer <- map(ArticlesChar,
    function(Texte) {
      TexteSplit <- str_split(Texte, "<ancre>")[[1]]
      TexteSplit <- TexteSplit[TexteSplit != ""]
      IndexSplit <- which(str_to_upper(stri_trans_general(TexteSplit, "Latin-ASCII")) %in% str_to_upper(stri_trans_general(paste0(Auteurs$noms, " ", str_sub(Auteurs$prenoms, 1, 1)), "Latin-ASCII")))
      ListeFp <- map(seq_along(TexteSplit),
                     function(x) {
                       if (x != IndexSplit) {
                         Resultat <- ftext(TexteSplit[x], prop = StyleBase)
                       } else {
                         Resultat <- ftext(TexteSplit[x], prop = StyleBichat)
                       }
                     })
    })

# Editer le document petit à petit, ce qui est un peu laborieux mais a l'air de marcher
# Apparemment, le fait de piper l'objet Document dans des fonctions officer le modifie, donc pas besoin d'assigner pour 
# modifier ni de donner l'environnement.
# Par contre, à chaque fois, il faudra tout relancer, sinon ça va réécrire sur le Word déjà fait je ne sais pas trop pourquoi
Document <- read_docx()
walk(seq_along(ArticleOfficer),
     function(Numero) {
       Document %>% body_add_fpar(fpar(values = ArticleOfficer[[Numero]]))
     })

# Finalement on sauvegarde et on espère que ça va marcher lol
print(Document, target = paste0(dirname(rstudioapi::getSourceEditorContext()$path), "/biblio_marmotte.docx"))
