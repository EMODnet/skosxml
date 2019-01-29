#' get skos xml data
#'
#' This function allows to get the information form different concepts stored in skoss xml. It takes a single concept, multiple concepts or a shema. The output is a list of 2 data frames; one with the lables and definitions. The other with the relations with other concepts.
#' @param vocids mandatory parameter, the XML link(s)  you want to read the read
#' @param vocabs optional parameter, include which vocabs you want to include. Takes multiple vocabs if separated by a "|" (eg. "P01|P06|Q01")
#' @import dplyr xml2 stringr
#' @export
#' @examples
#' getskossxmldata("http://vocab.nerc.ac.uk/collection/P06/current/UMCU/")
#' getskossxmldata(c("http://vocab.nerc.ac.uk/collection/P06/current/UMCU/","http://vocab.nerc.ac.uk/collection/P06/current/ULAA/")
#' getskossxmldata("http://vocab.nerc.ac.uk/collection/S10/current/", vocabs ="S10|S11")
#' getskossxmldata("http://dd.eionet.europa.eu/vocabulary/biodiversity/eunishabitats/rdf")


getskossxmldata <- function (vocids = NA, vocabs = NA) {
  
  vocids <- unique(vocids)
  
  if(!is.na(vocabs)) {
    vocids <- unique(subset (vocids,(grepl (vocabs, vocids)) & 
                               (grepl ("http://vocab.nerc.ac.uk/collection", vocids) &
                                  !grepl(glob2rx("*current?") , vocids) & 
                                  !grepl(glob2rx("*current") , vocids))))
  }  
  
  for (i in vocids) {
    skossxml  <- suppressWarnings(readskossxml(i))
    terminfo <- suppressWarnings(skossxmlinfo(skossxml))
    termrelation <- suppressWarnings(skossxmlrelations(skossxml))
    
    if (exists("terminfos")) {terminfos <- bind_rows(terminfos, terminfo)} else {  terminfos <- terminfo}
    if (exists("termrelations")) {termrelations <- bind_rows(termrelations, termrelation)} else {  termrelations <- termrelation}
  }
  
  skossxmldata <-list()
  skossxmldata$termrelations <- termrelations
  skossxmldata$terminfos <- terminfos
  
  return(skossxmldata)
  
}




#' get skos xml dataterms
#'
#' This function allows to get the information form different concepts stored in skoss xml. It takes a single concept, multiple concepts or a shema. The output a dataframe with the lables and definitions. 
#' @param vocids mandatory parameter, the XML link(s)  you want to read the read
#' @param vocabs optional parameter, include which vocabs you want to include. Takes multiple vocabs if separated by a "|" (eg. "P01|P06|Q01")
#' @import dplyr xml2 stringr
#' @export
#' @examples
#' getskossxmldatainfo("http://vocab.nerc.ac.uk/collection/P06/current/UMCU/")
#' getskossxmldatainfo(c("http://vocab.nerc.ac.uk/collection/P06/current/UMCU/","http://vocab.nerc.ac.uk/collection/P06/current/ULAA/")
#' getskossxmldatainfo("http://vocab.nerc.ac.uk/collection/S10/current/", vocabs ="S10|S11")
#' getskossxmldatainfo("http://dd.eionet.europa.eu/vocabulary/biodiversity/eunishabitats/rdf")



getskossxmldatainfo <- function (vocids = NA, vocabs = NA) {
  
  vocids <- unique(vocids)
  
  if(!is.na(vocabs)) {
    vocids <- unique(subset (vocids,(grepl (vocabs, vocids) & 
                                       !grepl(glob2rx("*current?") , vocids) & 
                                       !grepl(glob2rx("*current") , vocids))))
  }  
 
  terminfo<- suppressWarnings(lapply(vocids, function(x) {
  skossxmlinfo(readskossxml(x))
  } )) 
  terminfos <- bind_rows(terminfo)                  
                    
 
  return(terminfos)
}






#' read skoss xml file
#'
#' This function reads a skoss xml file
#' @param vocid mandatory parameter, the XML link  you want to read
#' @import  xml2
#' @export


readskossxml <- function(X) {
 tryCatch({
  x <- suppressWarnings(xml2::read_xml(X))
  skossxml <- suppressWarnings(xml2::xml_find_all(x, ".//skos:Concept")) }
  , error = function(x){
  print(paste0(X," does not resolve to a concept"))
  })
if (exists("skossxml")){
return(skossxml)
} 
}

#' parse skoss xml file
#'
#' This function pases a skoss xml file and provides a dataframe with lables and definitions.
#' @param skossxml mandatory parameter, the output from readskossxml
#' @import  xml2
#' @export


skossxmlinfo <- function (skossxml = NA) {
  if(!is.null(skossxml)) {if(!is.na(skossxml)){
    
  l <- lapply(skossxml, function(term) {
    element <- xml2::as_list(term)
    return(list(
      identifier = as.character(if (length(unlist(element$identifier))>0) {unlist(element$identifier)} else {"NA"}),
      preflabel  = as.character(if (length(unlist(element$prefLabel ))>0) {unlist(element$prefLabel )} else {"NA"}),
      altlabel  = as.character(if (length(unlist(element$altLabel ))>0) {unlist(element$altLabel )} else {"NA"}),
      definition = as.character(if (length(unlist(element$definition))>0) {unlist(element$definition)} else {"NA"}),
      deprecated = as.character(if (length(unlist(element$deprecated))>0) {unlist(element$deprecated)} else {"NA"}),
      uri = xml2::xml_attr(term, "about")))})

  terminfo <- bind_rows(l)
  return (terminfo)
}}}


#' parse skoss xml file2
#'
#' This function pases a skoss xml file and provides the relations
#' @param skossxml mandatory parameter, the output from readskossxml
#' @import  xml2
#' @export


skossxmlrelations <- function (skossxml) {
  if(!is.null(skossxml)) {if(!is.na(skossxml)){
  
    
    termr2 <- suppressWarnings(lapply(skossxml, function(y) {
    termr <-  lapply(skossconcepts, function(j) {
        
        options(stringsAsFactors = FALSE)
        templ <- xml2::xml_find_all(skossxml,paste0(".//",j))
        tempid <- xml2::xml_attr(skossxml, "about")
        
        if(length(templ)>0){
          temp <- dplyr::bind_rows(lapply(xml2::xml_attrs(templ), function(x) data.frame(as.list(x), stringsAsFactors=FALSE)))
          temp <- cbind(term = tempid ,temp,relation =richtfrom(j,":",0))
          }
        })
      termr2 <-  bind_rows(termr)  
    }))
    
    termrelations <- bind_rows(termr2)

    return(termrelations)
  }}}






#' left
#'
#' helpfunction simular to left in excel
#' @param x mandatory parameter, a vector
#' @param y mandatory parameter, the text from which you want the left of
#' @import  xml2
#' @export

leftfrom <- function(x, y, n = 0) {
  substr(x, 1, stringr::str_locate(x, y)[1] - n)
}



substrRight <- function(x, n){
  substr(x, nchar(x)-n+1, nchar(x))
}


#' right
#'
#' helpfunction simular to right in excel
#' @param x mandatory parameter, a vector
#' @param y mandatory parameter, the text from which you want the rightmost characters of
#' @import  xml2
#' @export


richtfrom <- function(x, y, n = 0) {
  substrRight(x, nchar(x)-stringr::str_locate(x, y)[1] - n)
}

