#' get all related terms of a concept 
#'
#' This function retuns to you all related terms of one or more concept. The output is a vector with the related ids 
#' @param vocid mandatory parameter, the XML link(s) of which you want the related concepts
#' @param relation optional parameter, the type of the retation you want. It takes as values: "narrower", "broader", "related", "sameas" 
#' @param vocab optional parameter, the vocab of the returned id's. For example: "P01", "P06" 
#' @import dplyr 
#' @export
#' @examples
#' getrelatedterms(vocid = "http://vocab.nerc.ac.uk/collection/S25/current/BE007117/", relation = "narrower", vocab = "P01" )



getrelatedterms <- function (vocid=NA, relation= NA, vocab = NA ) {
tryCatch({
if (!is.na(relation) & !is.na(vocab)){
relatedterms <- (getskossxmldata(vocid)$termrelation %>% filter (relation == relation,grepl(vocab, resource)))$resource
} else {
if (is.na(relation) & !is.na(vocab)){
relatedterms <- (getskossxmldata(vocid)$termrelation %>% filter (grepl(vocab, resource)))$resource
  
} else {
  if (!is.na(relation) & is.na(vocab)){
relatedterms <- (getskossxmldata(vocid)$termrelation %>% filter (relation == relation))$resource
} else {
relatedterms <- (getskossxmldata(vocid)$termrelation)$resource
}}}
}, error = function(x) {print(past0(vocid ," does not resolve"))}) 
if(exists("relatedterms")){return(relatedterms)  }
}




#' get both the parameterinfo and the unitinfo of a given concept 
#'
#' @param vocid mandatory parameter, the XML link(s) of which you want the related concepts.
#' @param vocabs optional parameter, include which vocabs you want to include. Takes multiple vocabs if separated by a "|" (eg. "P01|P06|Q01")
#' @import dplyr 
#' @export
#' @examples
#' getunitsandparams(vocid = "http://vocab.nerc.ac.uk/collection/P01/current/OWETBM01/" )
#' getunitsandparams(vocid = "http://vocab.nerc.ac.uk/collection/P01/current/OWETBM01/" , vocabs ="P01|Q01" )


getunitsandparams <- function(vocids=NA, vocabs = NA) {
  
  
  if(!is.na(vocabs)) {
    vocids <- unique(subset (vocids,(grepl (vocabs, vocids)) & 
                               (grepl ("http://vocab.nerc.ac.uk/collection", vocids) &
                                  !grepl(glob2rx("*current?") , vocids) & 
                                  !grepl(glob2rx("*current") , vocids))))
  }  
  
  
      #df<- getskossxmldata(vocids) 
      if(length(vocids) >  0) {
        df<- getskossxmldata(vocids) 
      
  
      if(length(df) > 0 & 
         if("termrelations" %in% names(df)) {
           length(df$termrelations) > 0
         } &
         if("terminfos" %in% names(df)) {
           length(df$terminfos) > 0
         }) {
      
      para <- df$terminfo %>% select (preflabel , altlabel, definition, deprecated, uri)
      paraandunit <- df$termrelation %>% filter (relation == "related" & grepl("P06", resource)) %>% rename(standardUnitID =resource)
      unit <- getskossxmldatainfo(unique(paraandunit$standardUnitID)) %>% select(standardUnitID = uri, unitpref = preflabel, standardunit = altlabel )
    
      if (nrow(paraandunit)>0){
      parawithhunit <-  para %>% left_join(paraandunit, by =c( "uri" = "term" )) %>% left_join(unit, by = c("standardUnitID")) %>% distinct()
      } else {parawithhunit <-  para}
      return (parawithhunit) 
  
  }
      }
  }

