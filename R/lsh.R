require(BMS)

get.ngrams <- function(word, n_gram){
  sst <- strsplit(word, "")[[1]]
  n_characters = length(sst)
  terms = sapply(X=1:(n_characters-n_gram+1),function(x) paste0(sst[x:(x+n_gram-1)],collapse=""))
  return(terms)
}

generate.featurespace <- function(corpus, n_gram){
  corpus = paste0(corpus, collapse=" ")
  terms = get.ngrams(corpus, n_gram)
  terms = names(as.list(sort(table(terms))))
  return(terms)
}

numerichash <- function(vec_dim, n_bits, preprocessfun){
  n_cols = vec_dim
  n_rows = n_bits
  operatorMatrix <- t(replicate(n_rows, rnorm(n_cols)))
  function(x){
    x <- preprocessfun(x)
    hashed_vec = operatorMatrix %*% x > 0
    bin2hex(hashed_vec)
  }
}

texthash <- function(featurespace, n_gram, n_bits, preprocessfun){
  n_cols = length(featurespace)
  n_rows = n_bits
  operatorMatrix <- t(replicate(n_rows, rnorm(n_cols)))
  function(textstring){
    textstring <- preprocessfun(textstring)
    v <- featurespace %in% get.ngrams(textstring, n_gram)
    hashed_text = operatorMatrix %*% v > 0
    bin2hex(hashed_text)
  }
}





