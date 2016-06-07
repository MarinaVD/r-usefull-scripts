binning <- function(training, vars, target, newSet)
{
  
  setClass("BinningResult",
           slots=c(
             training="data.frame",
             vars="character",
             not_binned = "character",
             excluded_vars = "character",
             newSet = "data.frame"
           )
  )
  
  n <- nrow(training)*0.05
  not_binned <- ''
  excluded_vars <- ''  
  for (i in vars)
  {
    if ("numeric" %in% class(training[[i]]) | "integer" %in% class(training[[i]]))
    {
      
      if (sort(table(training[[i]]),decreasing=TRUE)[[1]]/length(training[[i]]) >= 0.95)
      {
        excluded_vars <- c(excluded_vars, i)
      }
      else
      {
        formula <- paste(target,' ~ ', i, sep='')
        fit_t<-rpart(formula=formula,
                     data=training[!is.na(training[[i]]), ],
                     method="anova",
                     minbucket=n,
                     xval=5,
                     cp=0.001)
        cut_points <- fit_t$splits[,'index']
        cut_points1 <- c()
        if (length(cut_points) > 0)
        {
          cut_points1 <- cut_points[1]
        }
        fit_t<- prune(fit_t, cp= fit_t$cptable[which.min(fit_t$cptable[,"xerror"]),"CP"])
        cut_points <- fit_t$splits[,'index']
        cut_points <- sort(cut_points)
        print(i)
        print(cut_points)  
                
        if (length(cut_points) == 0 & is.null(cut_points1))
        {
          not_binned <- c(not_binned,  i)          
        }
        else 
        {
          vars <- vars[vars != i]
          vars <- c(vars, paste('bin_', i, sep=''))
          if (length(cut_points) > 0)
          {
            cut_points[length(cut_points) + 1] <- -Inf
            cut_points[length(cut_points) + 1] <- Inf
            training[[paste('bin_', i, sep='')]] <- cut(training[[i]], cut_points)
            newSet[[paste('bin_', i, sep='')]] <- cut(newSet[[i]], cut_points)
          } else
          {
            training[[paste('bin_', i, sep='')]] <- cut(training[[i]], c(-Inf, cut_points1, Inf))
            newSet[[paste('bin_', i, sep='')]] <- cut(newSet[[i]], c(-Inf, cut_points1, Inf))
            #binned_equal_parts <- c(binned_equal_parts,  i)          
          }
          
          if (length(training[[paste('bin_', i, sep='')]][is.na(training[[paste('bin_', i, sep='')]])]) > 0)
          {
            levels(training[[paste('bin_', i, sep='')]]) = c(levels(training[[paste('bin_', i, sep='')]]), "none")
            training[[paste('bin_', i, sep='')]][is.na(training[[paste('bin_', i, sep='')]])] <- 'none'
          }
          if (length(newSet[[paste('bin_', i, sep='')]][is.na(newSet[[paste('bin_', i, sep='')]])]) > 0)
          {
            levels(newSet[[paste('bin_', i, sep='')]]) = c(levels(newSet[[paste('bin_', i, sep='')]]), "none")
            newSet[[paste('bin_', i, sep='')]][is.na(newSet[[paste('bin_', i, sep='')]])] <- 'none'
          } 
        }
      }     
    }   
  }

  excluded_vars <- excluded_vars[!excluded_vars=='']  
  not_binned<- not_binned[!not_binned=='']  
  return(new("BinningResult",
             training=training,
             vars=vars,
             not_binned=not_binned,
             excluded_vars=excluded_vars,
             newSet=newSet
  )
  )
}




