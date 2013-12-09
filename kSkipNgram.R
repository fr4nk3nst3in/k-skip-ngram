#########################################################################################
# Function to generate k-skip-ngram                                                     #
# Alexander Spät                                                                        #
# Licence GPL V. 3                                                                      #
# http://www.gnu.org/licenses/gpl.html                                                  #
#########################################################################################


# x - should be text, sentense
# n - n-gramm
# skip - number of skips

# x as text
tokenizer <- function(x)
{
  # if non character object
  if (!is.character(x)) 
    stop("'x' not a character")
  
  # split string with given regular expresion
  x <- unlist(strsplit(x, "[[:space:]|[:blank:][:punct:][:space:]|[:blank:]]"))
  
  # return tokens
  return(unlist(lapply(x , function(x) if(nchar(x)>0) return(x))))
}

kSkipNgram <- function(x, n=1, skip=0)
  {
  ngram <- NULL
  n <- n-1
  skip <- skip +1
  x <- tokenizer(x)
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
