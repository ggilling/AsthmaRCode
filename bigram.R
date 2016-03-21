data("crude")

  BigramTokenizer <- function(x) NGramTokenizer(x, Weka_control(min = 2, max = 2))
  asthmaBigram.tdm <- TermDocumentMatrix(asthma.new.Corpus, control = list(tokenize = BigramTokenizer))
