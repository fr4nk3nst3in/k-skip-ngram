#########################################################################################
# Function to generate k-skip-ngram                                                     #
# Alexander Spät                                                                        #
# Licence GPL V. 3                                                                      #
# http://www.gnu.org/licenses/gpl.html                                                  #
#########################################################################################

library(RWeka)

# x - should be text, sentense
# n - n-gramm
# skip - number of skips

kSkipNgram <- function(x, n=1, skip=0)
  {
  ngram <- NULL
  n <- n-1
  skip <- skip +1
  # WEKA Tokenizer 
  Tokenizer <- function(x) NGramTokenizer(x, Weka_control(min = 1, max = 1))
  x <- Tokenizer(x)
  for(i in 1:length(x))
    {
      if (i <= length(x)-n)
      {
      switch(as.character(n),
             "0" = {ngram<-c(ngram, paste(x[i]))},
             "1" = {for(j in skip:1)
                      {
                        if (i+j <= length(x)) 
                          {ngram<-c(ngram, paste(x[i],x[i+j]))}
                      }
                    },
             "2" = {for(j in skip:1)
                      {for (k in skip:1)
                        {
                          if (i+j <= length(x) && i+j+k <= length(x)) 
                            {ngram<-c(ngram, paste(x[i],x[i+j],x[i+j+k]))}
                        }
                      }
                    },
             "3" = {for(j in skip:1)
                      {for (k in skip:1)
                        {for (l in skip:1)
                          {
                          if (i+j <= length(x) && i+j+k <= length(x) && i+j+k+l <= length(x)) 
                              {ngram<-c(ngram, paste(x[i],x[i+j],x[i+j+k],x[i+j+k+l]))}
                          }
                        }
                      }
                    },
             "4" = {for(j in skip:1)
                      {for (k in skip:1)
                          {for (l in skip:1)
                            {for (m in skip:1)
                                {
                                if (i+j <= length(x) && i+j+k <= length(x) && i+j+k+l <= length(x) && i+j+k+l+m <= length(x)) 
                                      {ngram<-c(ngram, paste(x[i],x[i+j],x[i+j+k],x[i+j+k+l],x[i+j+k+l+m]))}
                                }
                            }
                          }
                        }
                      }
            )
      }
    }
  return(ngram) 
}
