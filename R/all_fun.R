#' pattern_tuning
#'
#' Allow to tune a pattern very precisely and output a vector containing its variations n times.
#' @param pattrn is the character that will be tuned 
#' @param spe_nb is the number of new character that will be replaced
#' @param spe_l is the source vector from which the new characters will replace old ones
#' @param exclude_type is character that won't be replaced
#' @param hmn is how many output the function will return
#' @param rg is a vector with two parameters (index of the first letter that will be replaced, index of the last letter that will be replaced) default is set to all the letters from the source pattern
#' @examples
#'
#' print(pattern_tuning(pattrn="oui", spe_nb=2, spe_l=c("e", "r", "T", "O"), exclude_type="o", hmn=3))
#' 
#' #[1] "orT" "oTr" "oOi"
#'
#' @export

pattern_tuning <- function(pattrn, spe_nb, spe_l, exclude_type, hmn=1, rg=c(1, nchar(pattrn))){
  
  lngth <- nchar(pattrn)
  
  rtnl <- c()
  
  if (spe_nb <= lngth & rg[1] > -1 & rg[2] < (lngth+1)){
    
    pattrn_l <- unlist(strsplit(pattrn, ""))
    
    pattrn <- pattrn_l
    
    b_l <- c()
    
    for (I in 1:hmn){ 
       
        cnt = 0

        while (cnt <= spe_nb){
          
          if (rg[2] == lngth & rg[1] == 1){
            
            idx <- round(runif(1, 1, lngth), 0)
            
          }else{
            
            idx <- round(runif(1, rg[1], rg[2]), 0)
            
          }
          
          if (sum(grepl(pattrn[idx], exclude_type)) == 0){
            
            pattrn[idx] <- spe_l[round(runif(1, 1, length(spe_l)), 0)]
            
            cnt = cnt + 1

          }
          
        }
        
        pattrn <- paste(pattrn, collapse="")
        
        rtnl <- append(rtnl, pattrn)
        
        pattrn <- pattrn_l 
        
    }
      
    return(rtnl)
    
  }else{
    
    print("word too short for your arguments, see the documentation")
    
  }
  
}

#' unique_pos
#'
#' Allow to find the first index of the unique values from a vector. 
#' @param vec is the input vector
#' @examples
#'
#' print(unique_pos(vec=c(3, 4, 3, 5, 6)))
#'
#' #[1] 1 2 4 5
#'
#' @export

unique_pos <- function(vec){

        u_vec <- unique(vec)

        return(match(u_vec, vec))

}

#' data_meshup
#'
#' Allow to automatically arrange 1 dimensional data according to vector and parameters
#' @param data is the data provided (vector) each column is separated by a unic separator and each dataset from the same column is separated by another unic separator (ex: c("_", c("d", "-", "e", "-", "f"), "_", c("a", "a1", "-", "b", "-", "c", "c1"), "_")
#' @param cols are the colnames of the data generated in a csv
#' @param file_ is the file to which the data will be outputed, defaults to NA which means that the functio will return the dataframe generated and won't write it to a csv file
#' @param sep_ is the separator of the csv outputed
#' @param organisation is the way variables include themselves, for instance ,resuming precedent example, if organisation=c(1, 0) so the data output will be:
#' d, a
#' d, a1
#' e, c
#' f, c
#' f, c1
#' @param unic_sep1 is the unic separator between variables (default is "_")
#' @param unic_sep2 is the unic separator between datasets (default is "-")
#' @examples
#' 
#' print(data_meshup(data=c("_", c("-", "d", "-", "e", "-", "f"), "_", 
#'      c("-", "a", "a1", "-", "B", "r", "uy", "-", "c", "c1"), "_"), organisation=c(1, 0)))
#'
#' #  X1 X2
#' #1  d  a
#' #2  d a1
#' #3  e  B
#' #4  e  r
#' #5  e uy
#' #6  f  c
#' #7  f c1
#'
#' @export

data_meshup <- function(data, cols=NA, file_=NA, sep_=";", 
                        organisation=c(2, 1, 0), unic_sep1="_", 
                        unic_sep2="-"){
 
  l_l <- c()
  
  l_lngth <- c()
  
  old_max_row <- -1
  
  sep_dd <- str_detect(data, unic_sep1)
  
  jsq <- sum(sep_dd[!is.na(sep_dd)]) - 1 #numb of var
  
  sep_dd <- str_detect(data, unic_sep2)
  
  hmn <- (sum(sep_dd[!is.na(sep_dd)]) / jsq) #numb of datasets
 
  val_nb <- length(data) - (hmn * jsq + (jsq + 1))  
  
  dataset_l <- which(str_detect(unic_sep2, data))
  
  datf <- data.frame(matrix(nrow = val_nb, ncol = jsq))
  
  for (I in 1:hmn){
    
    idx_s = 0
    
    seq_l <- seq(I, length(dataset_l), hmn)
   
    for (i in 1:jsq){
      
      idx <- dataset_l[seq_l[i]]
      
      t = 1
      
      sep_dd <- grepl(data[idx + 1], c(unic_sep2, unic_sep1))
     
      while(sum(sep_dd[!is.na(sep_dd)]) == 0 & (idx + t <= length(data))){
       
        l_l <- append(l_l, data[idx + t])
        
        t = t + 1
        
        sep_dd <- grepl(data[idx + t], c(unic_sep2, unic_sep1))
        
      }
      
      l_lngth <- append(l_lngth, (t - 1))
      
      datf[1:(t-1), i] <- l_l
      
      l_l <- c()
      
    }
    
    if (old_max_row == -1){
      
      datf2 <- data.frame(matrix(nrow=0, ncol=jsq))
      
    }
   
    old_max_row <- max(l_lngth, na.rm=TRUE)
    
    l_lngth <- c()
  
    for (i in 1:jsq){
      
      v_rel <- datf[, i]
      
      var_ = 1
      
      x = 1
     
      while (x <= organisation[i]){
       
        v_relb <- datf[, i + x]
        
        val_ <- v_rel[var_] 
        
        for (t in 1:length(v_relb[!is.na(v_relb)])){
          
          datf[t, i] <- val_ 
          
          if (t + 1 <= length(v_rel)){
            
            if (is.na(v_rel[t + 1]) == FALSE){
              
              var_ = var_ + 1 
              
              while (is.na(v_rel[var_]) == TRUE){
                
                var_ = var_ + 1 
                
              }
              
              val_ <- v_rel[var_]  
              
            }
            
          }
          
        }
        
        x = x + 1
        
      }
      
    }
   
    datf2 <- rbind(datf2, datf[1:old_max_row, 1:jsq])
    
    datf[1:nrow(datf), 1:ncol(datf)] <- NA
    
  }
  
  if (all(is.na(cols)) == FALSE){
    
    colnames(datf2) <- cols
    
  }
  
  if (is.na(file_)){
    
    return(datf2)
    
  }else{
    
    write.table(datf2, file_, sep=sep_, row.names=FALSE)
    
  }
  
}

#' until_stnl
#'
#' Maxes a vector to a chosen length. ex: if i want my vector c(1, 2) to be 5 of length this function will return me: c(1, 2, 1, 2, 1) 
#' @param vec1 is the input vector
#' @param goal is the length to reach
#' @examples
#'
#' print(until_stnl(vec1=c(1, 3, 2), goal=56))
#'
#' # [1] 1 3 2 1 3 2 1 3 2 1 3 2 1 3 2 1 3 2 1 3 2 1 3 2 1 3 2 1 3 2 1 3 2 1 3 2 1 3
#' #[39] 2 1 3 2 1 3 2 1 3 2 1 3 2 1 3 2 1 3
#'
#' @export

until_stnl <- function(vec1, goal){

  max_ = 0

  ld <- length(vec1)

  for (i in (length(vec1)+1):goal){

        if (max_ < ld){

                max_ = max_ + 1 

        }else{

                max_ = 1

        }

        vec1 <- append(vec1, vec1[max_])

  }

  return(vec1)

}

#' multitud
#'
#' From a list containing vectors allow to generate a vector following this rule: list(c("a", "b"), c("1", "2"), c("A", "Z", "E")) --> c("a1A", "b1A", "a2A", "b2A", a1Z, ...)
#' @param l is the list
#' @param sep_ is the separator between elements (default is set to "" as you see in the example)
#' @examples
#'
#' print(multitud(l=list(c("a", "b"), c("1", "2"), c("A", "Z", "E"), c("Q", "F")), sep_="/"))
#' 
#' #[1] "a/1/A/Q" "b/1/A/Q" "a/2/A/Q" "b/2/A/Q" "a/1/Z/Q" "b/1/Z/Q" "a/2/Z/Q"
#' #[8] "b/2/Z/Q" "a/1/E/Q" "b/1/E/Q" "a/2/E/Q" "b/2/E/Q" "a/1/A/F" "b/1/A/F"
#' #[15] "a/2/A/F" "b/2/A/F" "a/1/Z/F" "b/1/Z/F" "a/2/Z/F" "b/2/Z/F" "a/1/E/F"
#' #[22] "b/1/E/F" "a/2/E/F" "b/2/E/F"
#'
#' @export

multitud <- function(l, sep_=""){
  
  rtnl <- unlist(l[1])

  for (I in 2:length(l)){
    
    rtnl2 <- c()
  
    cur_ <- unlist(l[I])
    
    for (i in 1:length(cur_)){
      
      for (t in 1:length(rtnl)){
        
        rtnl2 <- append(rtnl2, paste(rtnl[t], cur_[i], sep=sep_))

      }

    }
    
    rtnl <- rtnl2
    
  }

  return(rtnl)
  
}

#' save_untl
#'
#' Get the elements in each vector from a list that are located before certain values
#'
#' @param inpt_l is the input list containing all the vectors
#' @param val_to_stop_v is a vector containing the values that marks the end of the vectors returned in the returned list, see the examples
#' 
#' @examples
#'
#' print(save_untl(inpt_l=list(c(1:4), c(1, 1, 3, 4), c(1, 2, 4, 3)), val_to_stop_v=c(3, 4)))
#'
#' #[[1]]
#' #[1] 1 2
#' #
#' #[[2]]
#' #[1] 1 1
#' #
#' #[[3]]
#' #[1] 1 2
#'
#' print(save_untl(inpt_l=list(c(1:4), c(1, 1, 3, 4), c(1, 2, 4, 3)), val_to_stop_v=c(3)))
#' 
#' #[[1]]
#' #[1] 1 2
#' #
#' #[[2]]
#' #[1] 1 1
#' #
#' #[[3]]
#' #[1] 1 2 4
#'
#' @export

save_untl <- function(inpt_l=list(), val_to_stop_v=c()){

        rtn_l <- list()

        for (vec in inpt_l){

                t = 1

                cur_v <- c()

                while (!(vec[t] %in% val_to_stop_v) & t <= length(vec)){

                        cur_v <- c(cur_v, vec[t])

                        t = t + 1

                }

                rtn_l <- append(x=rtn_l, values=list(cur_v))

        }

        return(rtn_l)

}

#' pattern_gettr 
#'
#' Search for pattern(s) contained in a vector in another vector and return a list containing matched one (first index) and their position (second index) according to these rules: First case: Search for patterns strictly, it means that the searched pattern(s) will be matched only if the patterns containded in the vector that is beeing explored by the function are present like this c("pattern_searched", "other", ..., "pattern_searched") and not as c("other_thing pattern_searched other_thing", "other", ..., "pattern_searched other_thing") 
#' Second case: It is the opposite to the first case, it means that if the pattern is partially present like in the first position and the last, it will be considered like a matched pattern. REGEX can also be used as pattern 
#'
#' @param word_ is the vector containing the patterns
#' @param vct is the vector being searched for patterns
#' @param occ a vector containing the occurence of the pattern in word_ to be matched in the vector being searched, if the occurence is 2 for the nth pattern in word_ and only one occurence is found in vct so no pattern will be matched, put "forever" to no longer depend on the occurence for the associated pattern
#' @param strict a vector containing the "strict" condition for each nth vector in word_ ("strict" is the string to activate this option)
#' @param btwn is a vector containing the condition ("yes" to activate this option) meaning that if "yes", all elements between two matched patern in vct will be returned , so the patterns you enter in word_ have to be in the order you think it will appear in vct 
#' @param all_in_word is a value (default set to "yes", "no" to activate this option) that, if activated, won't authorized a previous matched pattern to be matched again
#' @param notatall is a string that you are sure is not present in vct
#' @examples
#'
#' print(pattern_gettr(word_=c("oui", "non", "erer"), vct=c("oui", "oui", "non", "oui", 
#'  "non", "opp", "opp", "erer", "non", "ok"), occ=c(1, 2, 1), 
#'  btwn=c("no", "yes", "no"), strict=c("no", "no", "ee")))
#'
#' #[[1]]
#' #[1] 1 5 8
#' #
#' #[[2]]
#' #[1] "oui"  "non"  "opp"  "opp"  "erer"
#'
#' @export

pattern_gettr <- function(word_, vct, occ=c(1), strict, btwn, all_in_word="yes", notatall="###"){

  all_occ <- c()

  for (i in 1:length(word_)){ all_occ <- append(all_occ, 0) }

  if (length(btwn) < (length(occ) - 1)){

          to_app <- btwn[length(btwn)]

          for (i in length(btwn):(length(occ) - 2)){

                btwn <- append(btwn, to_app)

          }

  }

  if (length(strict) < length(occ)){

          to_app <- strict[length(strict)]

          for (i in length(strict):(length(occ) - 1)){

                strict <- append(strict, to_app)

          }

  }

  frst_occ <- c()

  occ_idx = 1

  get_ins = 0

  vct2 <- c()

  can_ins <- 0

  for (i in 1:length(vct)){

    to_compare = 0

    if (all_in_word == "yes"){

            if (strict[occ_idx] == "yes"){

                t = 1

                while (to_compare < 1 & t <= length(word_)){

                        if (nchar(word_[t]) == nchar(vct[i])){

                                v_bool <- str_detect(vct[i], word_[t])

                                to_compare = sum(v_bool)

                                if (to_compare > 0){indx <- t}

                        }

                        t = t + 1

                }

            }else{

                    v_bool <- str_detect(vct[i], word_)

                    to_compare =  sum(v_bool)

                    if (to_compare > 0){indx <- match(TRUE, v_bool)}

            }

    }else{

       if (strict[occ_idx] == "yes"){

         t = 1

         while (t <= length(word_) & to_compare < 1){

           if (nchar(word_[t]) == nchar(vct[i])){

                v_bool <- str_detect(vct[i], word_[t])

                to_compare = sum(v_bool)

                if (to_compare > 0){indx <- t}

            }

            t = t + 1

         }

       }else{

        v_bool <- str_detect(vct[i], word_)

        to_compare =  sum(v_bool)

        if (to_compare > 0){indx <- match(TRUE, v_bool)}

       }

    }

    if (to_compare > 0) {

      all_occ <- as.numeric(all_occ)

      all_occ[indx] = all_occ[indx] + 1

      all_occ <- as.character(all_occ)

      if (all_in_word == "no"){

              if (length(word_) >= 2){

                word_ <- word_[-indx]

              }else{

                word_[1] <- notatall

              }

      }

      if (all_occ[indx] == occ[indx] | occ[indx] == "forever"){

        can_ins <- 1
        
        frst_occ <- append(frst_occ, i)

        if (occ_idx <= length(btwn)){

          if (btwn[occ_idx] == "yes"){get_ins <- 1}else{get_ins <- 0}

        }else{

          get_ins <- 0

        }

        if ((occ_idx + 1) <= length(occ)){ occ_idx = occ_idx + 1 }

      }
      
    }

    if (get_ins == 1 | can_ins == 1){

        can_ins <- 0

        vct2 <- append(vct2, vct[i])

    }
    
  }
  
  return(list(frst_occ, vct2))
  
}

#' see_idx
#'
#' Returns a boolean vector to see if a set of elements contained in v1 is also contained in another vector (v2)
#' 
#' @param v1 is the first vector
#' @param v2 is the second vector
#' @examples
#'
#' print(see_idx(v1=c("oui", "non", "peut", "oo"), v2=c("oui", "peut", "oui")))
#'
#' #[1]  TRUE FALSE  TRUE  FALSE
#'
#' @export

see_idx <- function(v1, v2){
 
  rtnl <- c()
 
  for (i in 1:length(v1)){

    if (length(grep(pattern=v1[i], x=v2)) > 0){

            r_idx <- TRUE

    }else{

            r_idx <- FALSE

    }

    rtnl <- append(x=rtnl, values=r_idx)
    
  }
 
  return(rtnl)
 
}

#' ptrn_twkr
#'
#' Allow to modify the pattern length of element in a vector according to arguments. What is here defined as a pattern is something like this xx-xx-xx or xx/xx/xxx... So it is defined by the separator
#' @param inpt_l is the input vector
#' @param depth is the number (numeric) of separator it will keep as a result. To keep the number of separator of the element that has the minimum amount of separator do depth="min" and depth="max" (character) for the opposite. This value defaults to "max".
#' @param sep is the separator of the pattern, defaults to "-"
#' @param default_val is the default val that will be placed between the separator, defaults to "00" 
#' @param add_sep defaults to TRUE. If set to FALSE, it will remove the separator for the patterns that are included in the interval between the depth amount of separator and the actual number of separator of the element.
#' @param end_ is if the default_val will be added at the end or at the beginning of each element that lacks length compared to depth
#'
#' @examples
#' 
#' v <- c("2012-06-22", "2012-06-23", "2022-09-12", "2022")
#'
#' ptrn_twkr(inpt_l=v, depth="max", sep="-", default_val="00", add_sep=TRUE)
#'
#' #[1] "2012-06-22" "2012-06-23" "2022-09-12" "2022-00-00"
#'
#' ptrn_twkr(inpt_l=v, depth=1, sep="-", default_val="00", add_sep=TRUE)
#'
#' #[1] "2012-06" "2012-06" "2022-09" "2022-00"
#' 
#' ptrn_twkr(inpt_l=v, depth="max", sep="-", default_val="00", add_sep=TRUE, end_=FALSE)
#'
#' #[1] "2012-06-22" "2012-06-23" "2022-09-12" "00-00-2022"
#'
#' @export

ptrn_twkr <- function(inpt_l, depth="max", sep="-", 
                      default_val="0", add_sep=TRUE, end_=TRUE){
  
  ln <- length(inpt_l)
  
  if (depth == "min"){
    
    pre_val <- str_count(inpt_l[1], sep)
    
    for (i in 2:ln){
      
      if (str_count(inpt_l[i], sep) < pre_val){
        
        pre_val <- str_count(inpt_l[i], sep)
        
      }
      
    }
    
    depth <- pre_val
    
  }

  if (depth == "max"){
    
    pre_val <- str_count(inpt_l[1], sep)
    
    for (i in 2:ln){
      
      if (str_count(inpt_l[i], sep) > pre_val){
        
        pre_val <- str_count(inpt_l[i], sep)
        
      }
      
    }
    
    depth <- pre_val
    
  }

  if (end_){

          for (I in 1:ln){
           
            hmn <- str_count(inpt_l[I], "-")
            
            if (hmn < depth){
             
              inpt_l[I] <- paste0(inpt_l[I], sep, default_val)

              diff <- depth - hmn - 1

              if (diff > 0){
              
                        if (add_sep == TRUE){
                          
                          for (i in 1:diff){
                          
                            inpt_l[I] <- paste0(inpt_l[I], sep, default_val)
                          
                          }
                        
                        }else{
                          
                          for (i in 1:diff){
                            
                            inpt_l[I] <- paste0(inpt_l[I], default_val)
                            
                          }
                          
                        }

             }
            
            }else if(depth < hmn){

                if (add_sep == TRUE){

                        inpt_l[I] <- paste(unlist(strsplit(inpt_l[I], split=sep))[1:(depth+1)], collapse=sep)

                }else{

                        inpt_l[I] <- paste(unlist(strsplit(inpt_l[I], split=sep))[1:(depth+1)], collapse="")
               
                }

            }

          }
  
  }else{

        for (I in 1:ln){
           
            hmn <- str_count(inpt_l[I], "-")
            
            if (hmn < depth){
             
              inpt_l[I] <- paste0(default_val, sep, inpt_l[I])

              diff <- depth - hmn - 1

              if (diff > 0){
              
                        if (add_sep == TRUE){
                          
                          for (i in 1:diff){
                          
                            inpt_l[I] <- paste0(default_val, sep, inpt_l[I])
                          
                          }
                        
                        }else{
                          
                          for (i in 1:diff){
                            
                            inpt_l[I] <- paste0(default_val, inpt_l[I])
                            
                          }
                          
                        }

             }
            
            }else if(depth < hmn){

                if (add_sep == TRUE){

                        inpt_l[I] <- paste(unlist(strsplit(inpt_l[I], split=sep))[1:(depth+1)], collapse=sep)

                }else{

                        inpt_l[I] <- paste(unlist(strsplit(inpt_l[I], split=sep))[1:(depth+1)], collapse="")
               
                }

            }

          }

  }

  return(inpt_l)
  
}

#' fillr
#' 
#' Allow to fill a vector by the last element n times
#' @param inpt_v is the input vector
#' @param ptrn_fill is the pattern used to detect where the function has to fill the vector by the last element n times. It defaults to "...\\d" where "\\d" is the regex for an int value. So this paramater has to have "\\d" which designates n.
#' @examples
#'
#' print(fillr(c("a", "b", "...3", "c")))
#'
#' #[1] "a" "b" "b" "b" "b" "c"
#'
#' @export

fillr <- function(inpt_v, ptrn_fill="\\.\\.\\.\\d"){
  
  ptrn <- grep(ptrn_fill, inpt_v)

  while (length(ptrn) > 0){
   
    ptrn <- grep(ptrn_fill, inpt_v)

    idx <- ptrn[1] 
    
    untl <- as.numeric(c(unlist(strsplit(inpt_v[idx], split="\\.")))[4]) - 1

    if (untl > -1){

    pre_val <- inpt_v[(idx - 1)]

    inpt_v[idx] <- pre_val

    if (untl > 0){
   
      for (i in 1:untl){
        
        inpt_v <- append(inpt_v, pre_val, idx)
        
      }
      
    }

    }else{

      inpt_v <- inpt_v[1]

    }

  ptrn <- grep(ptrn_fill, inpt_v)
    
  }
  
  return(inpt_v)
  
}

#' ptrn_switchr
#' 
#' Allow to switch, copy pattern for each element in a vector. Here a pattern is the values that are separated by a same separator. Example: "xx-xxx-xx" or "xx/xx/xxxx". The xx like values can be swicthed or copied from whatever index to whatever index. Here, the index is like this 1-2-3 etcetera, it is relative of the separator. 
#' @param inpt_l is the input vector
#' @param f_idx_l is a vector containing the indexes of the pattern you want to be altered.
#' @param t_idx_l is a vector containing the indexes to which the indexes in f_idx_l are related.
#' @param sep is the separator, defaults to "-"
#' @param default_val is the default value , if not set to NA, of the pattern at the indexes in f_idx_l. If it is not set to NA, you do not need to fill t_idx_l because this is the vector containing the indexes of the patterns that will be set as new values relatively to the indexes in f_idx_l. Defaults to NA.
#' @examples
#' 
#' print(ptrn_switchr(inpt_l=c("2022-01-11", "2022-01-14", "2022-01-21", 
#' "2022-01-01"), f_idx_l=c(1, 2, 3), t_idx_l=c(3, 2, 1)))
#'
#' #[1] "11-01-2022" "14-01-2022" "21-01-2022" "01-01-2022"
#'
#' print(ptrn_switchr(inpt_l=c("2022-01-11", "2022-01-14", "2022-01-21", 
#' "2022-01-01"), f_idx_l=c(1), default_val="ee"))
#' 
#' #[1] "ee-01-11" "ee-01-14" "ee-01-21" "ee-01-01"
#'
#' @export

ptrn_switchr <- function(inpt_l, f_idx_l=c(), t_idx_l=c(), sep="-", default_val=NA){

        if (is.na(default_val) == TRUE){

                for (I in 1:length(inpt_l)){

                        pre_val <- unlist(strsplit(inpt_l[I], split=sep))

                        pre_val2 <- pre_val

                        for (i in 1:length(f_idx_l)){

                               pre_val2[f_idx_l[i]] <- pre_val[t_idx_l[i]]

                        }

                        inpt_l[I] <- paste(pre_val2, collapse=sep)

                }

        }else{

                for (I in 1:length(inpt_l)){

                        pre_val <- unlist(strsplit(inpt_l[I], split=sep))

                        for (i in 1:length(f_idx_l)){

                               pre_val[f_idx_l[i]] <- default_val

                        }

                        inpt_l[I] <- paste(pre_val, collapse=sep)

                }

        }

        return(inpt_l)

}

#' occu
#'
#' Allow to see the occurence of each variable in a vector. Returns a datafame with, as the first column, the all the unique variable of the vector and , in he second column, their occurence respectively.
#' 
#' @param inpt_v the input dataframe
#' @examples
#'
#' print(occu(inpt_v=c("oui", "peut", "peut", "non", "oui")))
#'
#' #   var occurence
#' #1  oui         2
#' #2 peut         2
#' #3  non         1
#' 
#' @export

occu <- function(inpt_v){

    presence <- which(inpt_v == "")

    if (length(presence) > 0){ inpt_v <- inpt_v[-presence] }

    occu_v <- c()
    
    modal_v <- c()

    for (el in inpt_v){
      
      if (length(grep(el, modal_v)) == 1){
        
        idx <- which(modal_v == el)
        
        occu_v[idx] = occu_v[idx] + 1
        
      }else{
        
        occu_v <- append(x=occu_v, values=1, after=length(occu_v))
        
        modal_v <- append(x=modal_v, values=el, after=length(occu_v))
       
      }
    
    }

    return(data.frame("var"=modal_v, "occurence"=occu_v))
 
}

#' inter_min
#'
#' Takes as input a list of vectors composed of ints or floats ascendly ordered (intervals) that can have a different step to one of another element ex: list(c(0, 2, 4), c(0, 4), c(1, 2, 2.3)). This function will return the list of vectors with the same steps preserving the begin and end value of each interval. The way the algorythmn searches the common step of all the sub-lists is also given by the user as a parameter, see `how_to` paramaters.
#' @param inpt_l is the input list containing all the intervals
#' @param  min_ is a value you are sure is superior to the maximum step value in all the intervals
#' @param sensi is the decimal accuracy of how the difference between each value n to n+1 in an interval is calculated
#' @param sensi2 is the decimal accuracy of how the value with the common step is calculated in all the intervals
#' @param how_to_op is a vector containing the operations to perform to the pre-common step value, defaults to only "divide". The operations can be "divide", "substract", "multiply" or "add". All type of operations can be in this parameter.
#' @param how_to_val is a vector containing the value relatives to the operations in `hot_to_op`, defaults to 3
#' output from ex:
#' @examples
#'
#' print(inter_min(inpt_l=list(c(0, 2, 4), c(0, 4), c(1, 2, 2.3))))
#'
#' # [[1]]
#' # [1] 0.0 0.1 0.2 0.3 0.4 0.5 0.6 0.7 0.8 0.9 1.0 1.1 1.2 1.3 1.4 1.5 1.6 1.7 1.8
#' #[20] 1.9 2.0 2.1 2.2 2.3 2.4 2.5 2.6 2.7 2.8 2.9 3.0 3.1 3.2 3.3 3.4 3.5 3.6 3.7
#' #[39] 3.8 3.9 4.0
#' #
#' #[[2]]
#' # [1] 0.0 0.1 0.2 0.3 0.4 0.5 0.6 0.7 0.8 0.9 1.0 1.1 1.2 1.3 1.4 1.5 1.6 1.7 1.8
#' #[20] 1.9 2.0 2.1 2.2 2.3 2.4 2.5 2.6 2.7 2.8 2.9 3.0 3.1 3.2 3.3 3.4 3.5 3.6 3.7
#' #[39] 3.8 3.9 4.0
#' #
#' #[[3]]
#' # [1] 1.0 1.1 1.2 1.3 1.4 1.5 1.6 1.7 1.8 1.9 2.0 2.1 2.2 2.3
#' 
#' @export

inter_min <- function(inpt_l, min_=1000, sensi=3, sensi2=3, how_to_op=c("divide"),
                      how_to_val=c(3)){

        fillr <- function(inpt_v, ptrn_fill="...\\d"){
  
          ptrn <- grep(ptrn_fill, inpt_v)

          while (length(ptrn) > 0){
           
            ptrn <- grep(ptrn_fill, inpt_v)

            idx <- ptrn[1] 
            
            untl <- as.numeric(c(unlist(strsplit(inpt_v[idx], split="\\.")))[4]) - 1
           
            pre_val <- inpt_v[(idx - 1)]

            inpt_v[idx] <- pre_val

            if (untl > 0){
            
              for (i in 1:untl){
                
                inpt_v <- append(inpt_v, pre_val, idx)
                
              }
              
            }

          ptrn <- grep(ptrn_fill, inpt_v)
            
          }
          
          return(inpt_v)
          
        }

        diff_v2 <- c()

        diff_v <- c()

        for (lst in 1:length(inpt_l)){

            pre_v <- unlist(inpt_l[lst])

            for (idx in 1:(length(pre_v)-1)){

                diff_v <- c(diff_v, round((pre_v[idx+1] - pre_v[idx]), sensi))

            }

            diff_v2 <- c(diff_v2, diff_v)

            if (min(diff_v) < min_){ 

                min_ <- min(diff_v)

                diff_v <- c()

            }

        }

        verify <- function(diff_v2, min_){

            for (delta in diff_v2){

                pre_val <- delta / min_ %% 1

                all_eq <- 1

                if (length(grep("\\.", as.character(pre_val))) > 0){ 

                        pre_val_str <- unlist(strsplit(as.character(pre_val), split="\\."))[2]

                        pre_val_str <- unlist(strsplit(pre_val_str, split=""))[1:sensi]

                        if (length(grep(NA, pre_val_str)) > 0){

                                untl <- length(grep(NA, pre_val_str))

                                pre_val_str <- c(pre_val_str[which(is.na(pre_val_str) == FALSE)], "0")

                                pre_val_str <- fillr(inpt_v=c(pre_val_str, paste0("...", untl))) 

                        }

                        if (pre_val_str[length(pre_val_str)] != "9"){

                            all_eq <- 0

                        }else{

                                all_eq <- 1

                                for (i in 1:(length(pre_val_str)-1)){

                                    if (pre_val_str[i+1] != pre_val_str[i] | pre_val_str[i] != "9"){

                                            all_eq <- 0

                                    }

                                }

                        }

                }

                if (round(pre_val * (10 ** sensi), 0) != 0 & all_eq != 1){

                    ht <- how_to_op[1]

                    nb <- how_to_val[1]

                    if (length(how_to_op) > 1){

                        how_to_op <- how_to_op[2:length(how_to_op)]

                    }

                    if (length(how_to_val) > 1){

                        how_to_val <- how_to_val[2:length(how_to_op)]

                    }
                
                    if (ht == "divide"){

                        min_ <- round((min_ / nb), sensi)

                    }else if (ht == "add"){

                        min_ <- min_ + nb

                    }else if (ht == "multiply"){

                        min_ <- min * nb

                    }else{

                        min_ <- min_ - nb

                    }

                }

            }

            cnt <- 0

            for (lst in inpt_l){

                pre_v <- c()

                add_val <- lst[1]

                inpt_l[1] <- c()

                while (add_val <= lst[length(lst)]){

                    pre_v <- c(pre_v, add_val)

                    add_val <- round(add_val + min_, sensi2)

                }

                inpt_l <- append(x=inpt_l, values=list(pre_v))

                cnt <- cnt + 1

            }

            return(inpt_l)

        }

        rtn_l <- verify(diff_v2=diff_v2, min_=min_)

        return(rtn_l)

}

#' inter_max
#'
#' Takes as input a list of vectors composed of ints or floats ascendly ordered (intervals) that can have a different step to one of another element ex: list(c(0, 2, 4), c(0, 4), c(1, 2, 2.3)). The function will return the list of lists altered according to the maximum step found in the input list.
#' @param inpt_l is the input list
#' @param max_ is a value you are sure is the minimum step value of all the sub-lists
#' @param get_lst is the parameter that, if set to True, will keep the last values of vectors in the return value if the last step exceeds the end value of the vector.
#' @examples
#'
#' print(inter_max(inpt_l=list(c(0, 2, 4), c(0, 4), c(1, 2, 2.3)), get_lst=TRUE))
#'  
#' #[[1]]
#' #[1] 0 4
#' #
#' #[[2]]
#' #[1] 0 4
#' #
#' #[[3]]
#' #[1] 1.0 2.3
#' 
#' print(inter_max(inpt_l=list(c(0, 2, 4), c(0, 4), c(1, 2, 2.3)), get_lst=FALSE))
#'
#' # [[1]]
#' #[1] 0 4
#' #
#' #[[2]]
#' #[1] 0 4
#' #
#' #[[3]]
#' #[1] 1
#'
#' @export

inter_max <- function(inpt_l, max_=-1000, get_lst=TRUE){

    for (lst in 1:length(inpt_l)){

            diff_v <- c()

            cur_v <- unlist(inpt_l[lst])

            for (el in 1:(length(cur_v) - 1)){

                diff_v <- c(diff_v, (cur_v[el + 1] - cur_v[el]))

            }

            if (max(diff_v) > max_){

                max_ <- max(diff_v)

            }

    }

    cnt <- 0

    for (lst in inpt_l){

        cur_lst <- unlist(lst)

        add_val <- cur_lst[1]

        pre_v <- c()

        inpt_l[1] <- c()

        while (add_val <= cur_lst[length(cur_lst)]){

            pre_v <- c(pre_v, add_val)

            add_val <- add_val + max_

        }

        if (get_lst & cur_lst[length(cur_lst)] != pre_v[length(pre_v)]){

            pre_v <- c(pre_v, cur_lst[length(cur_lst)])

        }

        inpt_l <- append(x=inpt_l, values <- list(pre_v), after=length(inpt_l))

        cnt <- cnt + 1

    }

    return(inpt_l)

}

#' incr_fillr
#' 
#' Take a vector uniquely composed by double and sorted ascendingly, a step, another vector of elements whose length is equal to the length of the first vector, and a default value. If an element of the vector is not equal to its predecessor minus a user defined step, so these can be the output according to the parameters (see example):
#' @param inpt_v is the asending double only composed vector
#' @param wrk_v is the other vector (size equal to inpt_v), defaults to NA
#' @param default_val is the default value put when the difference between two following elements of inpt_v is greater than step, defaults to NA
#' @param step is the allowed difference between two elements of inpt_v
#' @examples
#'
#' print(incr_fillr(inpt_v=c(1, 2, 4, 5, 9, 10), 
#'                 wrk_v=NA, 
#'                 default_val="increasing"))
#'
#' #[1]  1  2  3  4  5  6  7  8  9 10
#'
#' print(incr_fillr(inpt_v=c(1, 1, 2, 4, 5, 9), 
#'                 wrk_v=c("ok", "ok", "ok", "ok", "ok"), 
#'                 default_val=NA))
#'
#' #[1] "ok" "ok" "ok" NA   "ok" "ok" NA   NA   NA  
#'
#' print(incr_fillr(inpt_v=c(1, 2, 4, 5, 9, 10), 
#'                 wrk_v=NA, 
#'                 default_val="NAN"))
#'
#' #[1] "1"   "2"   "NAN" "4"   "5"   "NAN" "NAN" "NAN" "9"   "10" 
#'
#' @export

incr_fillr <- function(inpt_v, wrk_v=NA, default_val=NA, step=1){

    if (all(is.na(wrk_v))){

        rtn_v <- inpt_v

    }else{

        rtn_v <- wrk_v

    }

    if (is.na(default_val)){

        i = 2

        while (i <= length(inpt_v)){

            if (is.na(inpt_v[(i-1)]) == FALSE){

                if ((inpt_v[(i-1)] + step) < inpt_v[i]){

                    rtn_v <- append(x=rtn_v, values=default_val, after=(i-1))

                    inpt_v <- append(x=inpt_v, values=default_val, after=(i-1))

                    bf_val = inpt_v[(i-1)] + 1

                }

            }else if ((bf_val + step) < inpt_v[i]){

                    rtn_v <- append(x=rtn_v, values=default_val, after=(i-1))

                    inpt_v <- append(x=inpt_v, values=default_val, after=(i-1))

                    bf_val = bf_val + 1

            }

            i = i + 1

        }

    }else if (default_val != "increasing"){

        i = 2

        while (i <= length(inpt_v)){

            if (inpt_v[(i-1)] != default_val){

                if ((as.numeric(inpt_v[(i-1)]) + step) < as.numeric(inpt_v[i])){

                    rtn_v <- append(x=rtn_v, values=default_val, after=(i-1))

                    inpt_v <- append(x=inpt_v, values=default_val, after=(i-1))

                    bf_val = as.numeric(inpt_v[(i-1)]) + 1

                }

            }else if ((bf_val + step) < as.numeric(inpt_v[i])){

                    inpt_v <- append(x=inpt_v, values=default_val, after=(i-1))

                    rtn_v <- append(x=rtn_v, values=default_val, after=(i-1))

                    bf_val = bf_val + 1

            }

            i = i + 1

        }

    }else{

        i = 2

        while (i <= length(rtn_v)){

            if ((inpt_v[(i-1)] + step) < inpt_v[i]){

                rtn_v <- append(x=rtn_v, values=(inpt_v[(i-1)]+1), after=(i-1))

                inpt_v <- append(x=inpt_v, values=(inpt_v[(i-1)]+1), after=(i-1))

            }

            i = i + 1

        }

    }

    return(rtn_v)

}

#' nest_v
#' 
#' Nest two vectors according to the following parameters.
#' @param f_v is the vector that will welcome the nested vector t_v
#' @param t_v is the imbriquator vector
#' @param step defines after how many elements of f_v the next element of t_v can be put in the output
#' @param after defines after how many elements of f_v, the begining of t_v can be put 
#' @examples
#' 
#' print(nest_v(f_v=c(1, 2, 3, 4, 5, 6), t_v=c("oui", "oui2", "oui3", "oui4", "oui5", "oui6"), 
#'      step=2, after=2))
#'
#' #[1] "1"    "2"    "oui"  "3"    "4"    "oui2" "5"    "6"    "oui3" "oui4"
#' 
#' @export

nest_v <- function(f_v, t_v, step=1, after=1){

    cnt = after

    for (i in 1:length(t_v)){

        f_v <- append(x=f_v, values=t_v[i], after=cnt)

        cnt = cnt + step + 1

    }

    return(f_v)

}

#' fixer_nest_v
#'
#' Retur the elements of a vector "wrk_v" (1) that corresponds to the pattern of elements in another vector "cur_v" (2) according to another vector "pttrn_v" (3) that contains the patterof elements.
#' @param cur_v is the input vector
#' @param pttrn_v is the vector containing all the patterns that may be contained in cur_v
#' @param wrk_v is a vector containing all the indexes of cur_v taken in count in the function
#' @examples
#'
#'print(fixer_nest_v(cur_v=c("oui", "non", "peut-etre", "oui", "non", "peut-etre"), 
#'              pttrn_v=c("oui", "non", "peut-etre"), 
#'                   wrk_v=c(1, 2, 3, 4, 5, 6)))
#'
#'#[1] 1 2 3 4 5 6
#'
#'print(fixer_nest_v(cur_v=c("oui", "non", "peut-etre", "oui", "non", "peut-etre"), 
#'                  pttrn_v=c("oui", "non"), 
#'                   wrk_v=c(1, 2, 3, 4, 5, 6)))
#'
#'#[1]  1  2 NA  4  5 NA
#'
#' @export

fixer_nest_v <- function(cur_v, pttrn_v, wrk_v){

    cnt = 1

    cnt2 = 0

    for (i in 1:length(cur_v)){

        if (pttrn_v[cnt] != cur_v[i]){

            if (cnt2 == 0){

                idx <- (cnt2*length(pttrn_v)-1) + match(TRUE, str_detect(pttrn_v, paste0("\\b(", cur_v[i], ")\\b"))) 

            }else{

                idx <- cnt2*length(pttrn_v) + match(TRUE, str_detect(pttrn_v, paste0("\\b(", cur_v[i], ")\\b"))) 

            }

            rtain_val <- wrk_v[idx]

            wrk_v[idx] <- wrk_v[i] 

            wrk_v[i] <- rtain_val

            rtain_val <- cur_v[idx]

            cur_v[idx] <- cur_v[i]

            cur_v[i] <- rtain_val

            if (cnt == length(pttrn_v)){ cnt = 1; cnt2 = cnt2 + 1 }else if (cnt > 1) { cnt = cnt + 1 }

        }else{

            if (cnt == length(pttrn_v)){ cnt = 1; cnt2 = cnt2 + 1 }else { cnt = cnt + 1 }

        }

    }

    return(wrk_v)

}

#' lst_flatnr
#'
#' Flatten a list to a vector
#' 
#' @param inpt_l is the input list
#'
#' @examples
#'
#'print(lst_flatnr(inpt_l=list(c(1, 2), c(5, 3), c(7, 2, 7))))
#'
#'#[1] 1 2 5 3 7 2 7
#'
#' @export

lst_flatnr <- function(inpt_l){

    rtn_v <- c()

    for (el in inpt_l){

        rtn_v <- c(rtn_v, el)

    }

    return(rtn_v)

}

#' extrt_only_v
#' 
#' Returns the elements from a vector "inpt_v" that are in another vector "pttrn_v"
#'
#' @param inpt_v is the input vector 
#' @param pttrn_v is the vector contining all the elements that can be in inpt_v 
#' @examples
#'
#'print(extrt_only_v(inpt_v=c("oui", "non", "peut", "oo", "ll", "oui", "non", "oui", "oui"), 
#'      pttrn_v=c("oui")))
#'
#'#[1] "oui" "oui" "oui" "oui"
#'
#' @export

extrt_only_v <- function(inpt_v, pttrn_v){

    rtn_v <- c()

    for (el in inpt_v){

        if (el %in% pttrn_v){ rtn_v <- c(rtn_v, el) }

    }

    return(rtn_v)

}

#' new_ordered
#'
#' Returns the indexes of elements contained in "w_v" according to "f_v"
#' 
#' @param f_v is the input vector
#' @param w_v is the vector containing the elements that can be in f_v
#' @param nvr_here is a value you are sure is not present in f_v
#' @examples
#'
#' print(new_ordered(f_v=c("non", "non", "non", "oui"), w_v=c("oui", "non", "non")))
#'
#' #[1] 4 1 2
#' 
#' @export

new_ordered <- function(f_v, w_v, nvr_here=NA){

    rtn_v <- c()

    for (el in w_v){

        idx <- match(el, f_v)

        rtn_v <- c(rtn_v, idx)

        f_v[idx] <- nvr_here

    }

    return(rtn_v)

}

#' appndr
#'
#' Append to a vector "inpt_v" a special value "val" n times "mmn". The appending begins at "strt" index.
#' @param inpt_v is the input vector
#' @param val is the special value
#' @param hmn is the number of special value element added
#' @param strt is the index from which appending begins, defaults to max which means the end of "inpt_v"
#' @examples
#'
#' print(appndr(inpt_v=c(1:3), val="oui", hmn=5))
#'
#' #[1] "1"   "2"   "3"   "oui" "oui" "oui" "oui" "oui"
#'
#' print(appndr(inpt_v=c(1:3), val="oui", hmn=5, strt=1))
#'
#' #[1] "1"   "oui" "oui" "oui" "oui" "oui" "2"   "3" 
#' 
#' @export

appndr <- function(inpt_v, val=NA, hmn, strt="max"){

    if (strt == "max"){

        strt <- length(inpt_v)

    }

    if (hmn > 0){

        for (i in 1:hmn){ inpt_v <- append(x=inpt_v, values=val, after=strt) }

    }

    return(inpt_v)

}

#' equalizer_v
#'
#' Takes a vector of character as an input and returns a vector with the elements at the same size. The size can be chosen via depth parameter.
#'
#' @param inpt_v is the input vector containing all the characters
#' @param depth is the depth parameter, defaults to "max" which means that it is equal to the character number of the element(s) in inpt_v that has the most 
#' @param default_val is the default value that will be added to the output characters if those has an inferior length (characters) than the value of depth 
#' @examples 
#'
#'  print(equalizer_v(inpt_v=c("aa", "zzz", "q"), depth=2))
#'
#'  #[1] "aa" "zz" "q?"
#'
#'  print(equalizer_v(inpt_v=c("aa", "zzz", "q"), depth=12))
#'
#'  #[1] "aa??????????" "zzz?????????" "q???????????"
#'
#' @export

equalizer_v <- function(inpt_v, depth="max", default_val="?"){

        if (depth == "min"){ 

           depth <- nchar(inpt_v[1]) 

            if (length(inpt_v) > 1){

                    for (ptrn in inpt_v[2:length(inpt_v)]){

                        if (nchar(ptrn) < depth){ depth <- nchar(ptrn) }

                    }

            }

        }

        if (depth == "max"){ 

           depth <- nchar(inpt_v[1]) 

            if (length(inpt_v) > 1){

                    for (ptrn in inpt_v[2:length(inpt_v)]){

                        if (nchar(ptrn) > depth){ depth <- nchar(ptrn) }

                    }

            }

        }

        rtn_v <- c()

        for (ptrn in inpt_v){

                if (nchar(ptrn) < depth){ 

                        for (i in 1:(depth-nchar(ptrn))){ ptrn <- paste0(ptrn, default_val) }
                       
                        rtn_v <- c(rtn_v, ptrn)

                }else{

                        rtn_v <- c(rtn_v, paste(unlist(strsplit(x=ptrn, split=""))[1:depth], collapse=""))

                }

        }


        return(rtn_v)

}

#' rearangr_v
#'
#' Reanranges a vector "w_v" according to another vector "inpt_v". inpt_v contains a sequence of number. inpt_v and w_v have the same size and their indexes are related. The output will be a vector containing all the elements of w_v rearanges in descending or asending order according to inpt_v
#'
#' @param inpt_v is the vector that contains the sequance of number
#' @param w_v is the vector containing the elements related to inpt_v
#' @param how is the way the elements of w_v will be outputed according to if inpt_v will be sorted ascendigly or descendingly
#' @examples 
#'
#' print(rearangr_v(inpt_v=c(23, 21, 56), w_v=c("oui", "peut", "non"), how="decreasing"))
#'
#' #[1] "non"  "oui"  "peut"
#'
#' @export

rearangr_v <- function(inpt_v, w_v, how="increasing"){

    rtn_v <- c()

    pre_v <- inpt_v

    if (how == "increasing"){

        inpt_v <- sort(inpt_v)

    }else {

        inpt_v <- sort(inpt_v, decreasing=TRUE)

    }

    for (el in inpt_v){

        idx <- match(el, pre_v)

        rtn_v <- c(rtn_v, w_v[idx])

        pre_v[idx] <- NA

    }

    return(rtn_v)

}

#' clusterizer_v
#' 
#' Allow to output clusters of elements. Takes as input a vector "inpt_v" containing a sequence of number. Can also take another vector "w_v" that has the same size of inpt_v because its elements are related to it. The way the clusters are made is related to an accuracy value which is "c_val". It means that if the difference between the values associated to 2 elements is superior to c_val, these two elements are in distinct clusters. The second element of the outputed list is the begin and end value of each cluster.
#' 
#' @param inpt_v is the vector containing the sequence of number
#' @param w_v is the vector containing the elements related to inpt_v, defaults to NA
#' @param c_val is the accuracy of the clusterization
#' 
#' @examples
#'  print(clusterizer_v(inpt_v=sample.int(20, 26, replace=TRUE), w_v=NA, c_val=0.9))
#' 
#' # [[1]]
#' #[[1]][[1]]
#' #[1] 1
#' #
#' #[[1]][[2]]
#' #[1] 2
#' #
#' #[[1]][[3]]
#' #[1] 3
#' #
#' #[[1]][[4]]
#' #[1] 4
#' #
#' #[[1]][[5]]
#' #[1] 5 5
#' #
#' #[[1]][[6]]
#' #[1] 6 6 6 6
#' #
#' #[[1]][[7]]
#' #[1] 7 7 7
#' #
#' #[[1]][[8]]
#' #[1] 8 8 8
#' #
#' #[[1]][[9]]
#' #[1] 9
#' #
#' #[[1]][[10]]
#' #[1] 10
#' #
#' #[[1]][[11]]
#' #[1] 12
#' #
#' #[[1]][[12]]
#' #[1] 13 13 13
#' #
#' #[[1]][[13]]
#' #[1] 18 18 18
#' #
#' #[[1]][[14]]
#' #[1] 20
#' #
#' #
#' #[[2]]
#' # [1] "1"  "1"  "-"  "2"  "2"  "-"  "3"  "3"  "-"  "4"  "4"  "-"  "5"  "5"  "-" 
#' #[16] "6"  "6"  "-"  "7"  "7"  "-"  "8"  "8"  "-"  "9"  "9"  "-"  "10" "10" "-" 
#' #[31] "12" "12" "-"  "13" "13" "-"  "18" "18" "-"  "20" "20"
#' 
#' print(clusterizer_v(inpt_v=sample.int(40, 26, replace=TRUE), w_v=letters, c_val=0.29))
#'
#' #[[1]]
#' #[[1]][[1]]
#' #[1] "a"
#' #
#' #[[1]][[2]]
#' #[1] "b"
#' #
#' #[[1]][[3]]
#' #[1] "c" "d"
#' #
#' #[[1]][[4]]
#' #[1] "e" "f"
#' #
#' #[[1]][[5]]
#' #[1] "g" "h" "i" "j"
#' #
#' #[[1]][[6]]
#' #[1] "k"
#' #
#' #[[1]][[7]]
#' #[1] "l"
#' #
#' #[[1]][[8]]
#' #[1] "m" "n"
#' #
#' #[[1]][[9]]
#' #[1] "o"
#' #
#' #[[1]][[10]]
#' #[1] "p"
#' #
#' #[[1]][[11]]
#' #[1] "q" "r"
#' #
#' #[[1]][[12]]
#' #[1] "s" "t" "u"
#' #
#' #[[1]][[13]]
#' #[1] "v"
#' #
#' #[[1]][[14]]
#' #[1] "w"
#' #
#' #[[1]][[15]]
#' #[1] "x"
#' #
#' #[[1]][[16]]
#' #[1] "y"
#' #
#' #[[1]][[17]]
#' #[1] "z"
#' #
#' #
#' #[[2]]
#' # [1] "13" "13" "-"  "14" "14" "-"  "15" "15" "-"  "16" "16" "-"  "17" "17" "-" 
#' #[16] "19" "19" "-"  "21" "21" "-"  "22" "22" "-"  "23" "23" "-"  "25" "25" "-" 
#' #[31] "27" "27" "-"  "29" "29" "-"  "30" "30" "-"  "31" "31" "-"  "34" "34" "-" 
#' #[46] "35" "35" "-"  "37" "37"
#'
#' @export

clusterizer_v <- function(inpt_v, w_v=NA, c_val){

    rearangr_v <- function(inpt_v, w_v, how="increasing"){

            rtn_v <- c()

            pre_v <- inpt_v

            if (how == "increasing"){

                inpt_v <- sort(inpt_v)

            }else {

                inpt_v <- sort(inpt_v, decreasing=TRUE)

            }

            for (el in inpt_v){

                idx <- match(el, pre_v)

                rtn_v <- c(rtn_v, w_v[idx])

                pre_v[idx] <- NA

            }

            return(rtn_v)

    }

    inpt_v <- sort(inpt_v)

    idx_v <- c()

    rtn_l <- list() 

    if (all(is.na(w_v)) == FALSE){

            w_v <- rearangr_v(inpt_v=inpt_v, w_v=w_v)

            pre_v <- c(w_v[1])

            pre_idx <- inpt_v[1]

            if (length(inpt_v) > 1){

                    for (i in 2:length(inpt_v)){

                        if ((inpt_v[i] - inpt_v[i - 1]) > c_val){

                                rtn_l <- append(rtn_l, list(pre_v))

                                idx_v <- c(idx_v, "-", pre_idx, inpt_v[i-1])

                                pre_idx <- inpt_v[i]

                                pre_v <- c()

                        }

                        pre_v <- c(pre_v, w_v[i])

                    }

                    rtn_l <- append(rtn_l, list(pre_v))

                    idx_v <- c(idx_v, "-", pre_idx, inpt_v[length(inpt_v)])

            }else{

                rtn_l <- append(rtn_l, pre_v[1])

            }

    }else{

            pre_v <- c(inpt_v[1])

            pre_idx <- inpt_v[1]

            if (length(inpt_v) > 1){

                    for (i in 2:length(inpt_v)){

                        if ((inpt_v[i] - inpt_v[i - 1]) > c_val){

                                rtn_l <- append(rtn_l, list(pre_v))

                                idx_v <- c(idx_v, "-", pre_idx, inpt_v[i-1])

                                pre_idx <- inpt_v[i]

                                pre_v <- c()

                        }
                                
                        pre_v <- c(pre_v, inpt_v[i])

                    }

                    rtn_l <- append(rtn_l, list(pre_v))

                    idx_v <- c(idx_v, "-", pre_idx, inpt_v[length(inpt_v)])

            }else{

                rtn_l <- append(rtn_l, pre_v[1])

            }

    }

    return(list(rtn_l, idx_v[2:length(idx_v)]))

}

#' closer_ptrn_adv
#' 
#' Allow to find how patterns are far or near between each other relatively to a vector containing characters at each index ("base_v"). The function gets the sum of the indexes of each pattern letter relatively to the characters in base_v. So each pattern can be compared.
#' 
#' @param inpt_v is the input vector containing all the patterns to be analyzed
#' @param res is a parameter controling the result. If set to "raw_stat", each word in inpt_v will come with its score (indexes of its letters relatively to base_v). If set to something else, so "c_word" parameter must be filled.
#' @param c_word is a pattern from which the nearest to the farest pattern in inpt_v will be compared 
#' @param base_v is the vector from which all pattern get its result (letters indexes for each pattern relatively to base_v), defaults to c("default_val", letters). "default_val" is another parameter and letters is all the western alphabetic letters in a vector
#' @param default_val is the value that will be added to all patterns that do not equal the length of the longest pattern in inpt_v. Those get this value added to make all patterns equal in length so they can be compared, defaults to "?"
#' 
#' @examples
#'
#' print(closer_ptrn_adv(inpt_v=c("aurevoir", "bonnour", "nonnour", "fin", "mois", "bonjour"), 
#'      res="word", c_word="bonjour"))
#' 
#'#[[1]]
#'#[1]  1  5 15 17 38 65
#'#
#'#[[2]]
#'#[1] "bonjour"  "bonnour"  "aurevoir" "nonnour"  "mois"     "fin"     
#' 
#' print(closer_ptrn_adv(inpt_v=c("aurevoir", "bonnour", "nonnour", "fin", "mois")))
#' 
#'#[[1]]
#'#[1] 117 107 119  37  64
#'#
#'#[[2]]
#'#[1] "aurevoir" "bonnour"  "nonnour"  "fin"      "mois"    
#'
#' @export

closer_ptrn_adv <- function(inpt_v, res="raw_stat", default_val="?", base_v=c(default_val, letters), c_word=NA){

        chr_removr <- function(inpt_v, ptrn_v){

                rm_fun <- function(x){

                    rm_ids <- c()

                    cur_chr <- unlist(strsplit(x, split=""))

                    for (ptrn in ptrn_v){

                            rm_ids <- c(rm_ids, which(cur_chr == ptrn))

                    }

                    if (length(rm_ids) == 0){

                            return(x)

                    }else {

                            cur_chr <- cur_chr[-rm_ids]

                            return(paste(cur_chr, collapse=""))

                    }

                }
              
                rtn_v <- mapply(function(x) return(rm_fun(x)), inpt_v) 

                return(as.vector(rtn_v))

        }

        rearangr_v <- function(inpt_v, w_v, how="increasing"){

            rtn_v <- c()

            pre_v <- inpt_v

            if (how == "increasing"){

                inpt_v <- sort(inpt_v)

            }else {

                inpt_v <- sort(inpt_v, decreasing=TRUE)

            }

            for (el in inpt_v){

                idx <- match(el, pre_v)

                rtn_v <- c(rtn_v, w_v[idx])

                pre_v[idx] <- NA

            }

            return(rtn_v)

        }

        equalizer_v <- function(inpt_v, depth="max", default_val="?"){

                if (depth == "min"){ 

                   depth <- nchar(inpt_v[1]) 

                    if (length(inpt_v) > 1){

                            for (ptrn in inpt_v[2:length(inpt_v)]){

                                if (nchar(ptrn) < depth){ depth <- nchar(ptrn) }

                            }

                    }

                }

                if (depth == "max"){ 

                   depth <- nchar(inpt_v[1]) 

                    if (length(inpt_v) > 1){

                            for (ptrn in inpt_v[2:length(inpt_v)]){

                                if (nchar(ptrn) > depth){ depth <- nchar(ptrn) }

                            }

                    }

                }

                rtn_v <- c()

                for (ptrn in inpt_v){

                        if (nchar(ptrn) < depth){ 

                                for (i in 1:(depth-nchar(ptrn))){ ptrn <- paste0(ptrn, default_val) }
                               
                                rtn_v <- c(rtn_v, ptrn)

                        }else{

                                rtn_v <- c(rtn_v, paste(unlist(strsplit(x=ptrn, split=""))[1:depth], collapse=""))

                        }

                }


                return(rtn_v)

    }

    inpt_v <- equalizer_v(inpt_v=inpt_v, default_val=default_val)

    ref_v <- base_v

    res_v <- c()

    for (ptrn in inpt_v){

        cur_delta = 0

        ptrn <- unlist(strsplit(ptrn, split=""))

        for (ltr in ptrn){

            cur_delta = cur_delta + match(ltr, base_v)

        }

        res_v <- c(res_v, cur_delta)

    }

    if (res == "raw_stat"){

         return(list(res_v, chr_removr(inpt_v=inpt_v, ptrn_v=c(default_val))))

    }else if (is.na(c_word) == FALSE){

        cur_delta = 0

        for (ltr in unlist(strsplit(c_word, split=""))){

            cur_delta = cur_delta + match(ltr, base_v)

        }

        cur_delta <- abs(res_v - cur_delta)

        inpt_v <- rearangr_v(inpt_v=cur_delta, w_v=inpt_v, how="increasing")

        return(list(sort(cur_delta, decreasing=FALSE), chr_removr(inpt_v=inpt_v, ptrn_v=c(default_val))))

    }

}

#' unique_ltr_from_v 
#'
#' Returns the unique characters contained in all the elements from an input vector "inpt_v"
#'
#' @param inpt_v is the input vector containing all the elements
#' @param keep_v is the vector containing all the characters that the elements in inpt_v may contain
#'
#' @examples
#'
#' print(unique_ltr_from_v(inpt_v=c("bonjour", "lpoerc", "nonnour", "bonnour", "nonjour", "aurevoir")))
#'
#' #[1] "b" "o" "n" "j" "u" "r" "l" "p" "e" "c" "a" "v" "i" 
#'
#' @export

unique_ltr_from_v <- function(inpt_v, keep_v=c("?", "!", ":", "&", ",", ".", letters)){

    cnt = 1

    add_v <- c(1)

    rtn_v <- c()

    while (length(keep_v) > 0 & cnt <= length(inpt_v)){

            add_v <- as.vector(mapply(function(x) return(match(x, keep_v)), unlist(strsplit(inpt_v[cnt], split=""))))

            if (all(is.na(add_v)) == FALSE){

                add_v <- add_v[(is.na(add_v)==FALSE)]

                rtn_v <- c(rtn_v, keep_v[unique(add_v)])

                keep_v <- keep_v[-add_v]

            }

            cnt = cnt + 1

    }

    return(rtn_v)

}

#' closer_ptrn
#'
#' Take a vector of patterns as input and output each chosen word with their closest patterns from chosen patterns. 
#' 
#' @param inpt_v is the input vector containing all the patterns
#' @param excl_v is the vector containing all the patterns from inpt_v to exclude for comparing them to others patterns. If this parameter is filled, so "rtn_v" must be empty.
#' @param rtn_v is the vector containing all the patterns from inpt_v to keep for comparing them to others patterns. If this parameter is filled, so "rtn_v" must be empty.
#' @param sub_excl_v is the vector containing all the patterns from inpt_v to exclude for using them to compare to another pattern. If this parameter is filled, so "sub_rtn_v" must be empty.
#' @param sub_rtn_v is the vector containing all the patterns from inpt_v to retain for using them to compare to another pattern. If this parameter is filled, so "sub_excl_v" must be empty.
#' @param base_v must contain all the characters that the patterns are succeptible to contain, defaults to c("?", letters). "?" is necessary because it is internaly the default value added to each element that does not have a suffiient length compared to the longest pattern in inpt_v. If set to NA, the function will find by itself the elements to be filled with but it may takes an extra time 
#' @examples
#' 
#' print(closer_ptrn(inpt_v=c("bonjour", "lpoerc", "nonnour", "bonnour", "nonjour", "aurevoir")))
#'
#'#[[1]]
#'#[1] "bonjour"
#'#
#'#[[2]]
#'#[1] "lpoerc"   "nonnour"  "bonnour"  "nonjour"  "aurevoir"
#'#
#'#[[3]]
#'#[1] 1 1 2 7 8
#'#
#'#[[4]]
#'#[1] "lpoerc"
#'#
#'#[[5]]
#'#[1] "bonjour"  "nonnour"  "bonnour"  "nonjour"  "aurevoir"
#'#
#'#[[6]]
#'#[1] 7 7 7 7 7
#'#
#'#[[7]]
#'#[1] "nonnour"
#'#
#'#[[8]]
#'#[1] "bonjour"  "lpoerc"   "bonnour"  "nonjour"  "aurevoir"
#'#
#'#[[9]]
#'#[1] 1 1 2 7 8
#'#
#'#[[10]]
#'#[1] "bonnour"
#'#
#'#[[11]]
#'#[1] "bonjour"  "lpoerc"   "nonnour"  "nonjour"  "aurevoir"
#'#
#'#[[12]]
#'#[1] 1 1 2 7 8
#'#
#'#[[13]]
#'#[1] "nonjour"
#'#
#'#[[14]]
#'#[1] "bonjour"  "lpoerc"   "nonnour"  "bonnour"  "aurevoir"
#'#
#'#[[15]]
#'#[1] 1 1 2 7 8
#'#
#'#[[16]]
#'#[1] "aurevoir"
#'#
#'#[[17]]
#'#[1] "bonjour" "lpoerc"  "nonnour" "bonnour" "nonjour"
#'#
#'#[[18]]
#'#[1] 7 8 8 8 8
#' 
#' print(closer_ptrn(inpt_v=c("bonjour", "lpoerc", "nonnour", "bonnour", "nonjour", "aurevoir"), 
#' excl_v=c("nonnour", "nonjour"),
#'                  sub_excl_v=c("nonnour")))
#'
#'#[1] 3 5
#'#[[1]]
#'#[1] "bonjour"
#'#
#'#[[2]]
#'#[1] "lpoerc"   "bonnour"  "nonjour"  "aurevoir"
#'#
#'#[[3]]
#'#[1] 1 1 7 8
#'#
#'#[[4]]
#'#[1] "lpoerc"
#'#
#'#[[5]]
#'#[1] "bonjour"  "bonnour"  "nonjour"  "aurevoir"
#'#
#'#[[6]]
#'#[1] 7 7 7 7
#'#
#'#[[7]]
#'#[1] "bonnour"
#'#
#'#[[8]]
#'#[1] "bonjour"  "lpoerc"   "bonnour"  "nonjour"  "aurevoir"
#'#
#'#[[9]]
#'#[1] 0 1 2 7 8
#'#
#'#[[10]]
#'#[1] "aurevoir"
#'#
#'#[[11]]
#'#[1] "bonjour"  "lpoerc"   "nonjour"  "aurevoir"
#'#
#'#[[12]]
#'#[1] 0 7 8 8
#'
#' @export

closer_ptrn <- function(inpt_v, base_v=c("?", letters), excl_v=c(), rtn_v=c(), 
                        sub_excl_v=c(), sub_rtn_v=c()){

        unique_ltr_from_v <- function(inpt_v, keep_v=c("?", "!", ":", "&", ",", ".", letters)){

            cnt = 1

            add_v <- c(1)

            rtn_v <- c()

            while (length(keep_v) > 0 & cnt <= length(inpt_v)){

                    add_v <- as.vector(mapply(function(x) return(match(x, keep_v)), unlist(strsplit(inpt_v[cnt], split=""))))

                    if (all(is.na(add_v)) == FALSE){

                        add_v <- add_v[(is.na(add_v)==FALSE)]

                        rtn_v <- c(rtn_v, keep_v[unique(add_v)])

                        keep_v <- keep_v[-add_v]

                    }

                    cnt = cnt + 1

            }

            return(rtn_v)

        }

        default_val <- "?"

        if (all(is.na(base_v))){

            base_v <- unique_ltr_from_v(inpt_v=inpt_v)

        }

        if (("?" %in% base_v) == FALSE) { base_v <- c(base_v, "?") }

        rearangr_v <- function(inpt_v, w_v, how="increasing"){

            rtn_v <- c()

            pre_v <- inpt_v

            if (how == "increasing"){

                inpt_v <- sort(inpt_v)

            }else {

                inpt_v <- sort(inpt_v, decreasing=TRUE)

            }

            for (el in inpt_v){

                idx <- match(el, pre_v)

                rtn_v <- c(rtn_v, w_v[idx])

                pre_v[idx] <- NA

            }

            return(rtn_v)

        }

        chr_removr <- function(inpt_v, ptrn_v){

                rm_fun <- function(x){

                    rm_ids <- c()

                    cur_chr <- unlist(strsplit(x, split=""))

                    for (ptrn in ptrn_v){

                            rm_ids <- c(rm_ids, which(cur_chr == ptrn))

                    }

                    if (length(rm_ids) == 0){

                            return(x)

                    }else {

                            cur_chr <- cur_chr[-rm_ids]

                            return(paste(cur_chr, collapse=""))

                    }

                }
              
                rtn_v <- mapply(function(x) return(rm_fun(x)), inpt_v) 

                return(as.vector(rtn_v))

        }

        equalizer_v <- function(inpt_v, depth="max", default_val="?"){

                if (depth == "min"){ 

                   depth <- nchar(inpt_v[1]) 

                    if (length(inpt_v) > 1){

                            for (ptrn in inpt_v[2:length(inpt_v)]){

                                if (nchar(ptrn) < depth){ depth <- nchar(ptrn) }

                            }

                    }

                }

                if (depth == "max"){ 

                   depth <- nchar(inpt_v[1]) 

                    if (length(inpt_v) > 1){

                            for (ptrn in inpt_v[2:length(inpt_v)]){

                                if (nchar(ptrn) > depth){ depth <- nchar(ptrn) }

                            }

                    }

                }

                rtn_v <- c()

                for (ptrn in inpt_v){

                        if (nchar(ptrn) < depth){ 

                                for (i in 1:(depth-nchar(ptrn))){ ptrn <- paste0(ptrn, default_val) }
                               
                                rtn_v <- c(rtn_v, ptrn)

                        }else{

                                rtn_v <- c(rtn_v, paste(unlist(strsplit(x=ptrn, split=""))[1:depth], collapse=""))

                        }

                }


                return(rtn_v)

    }

    inpt_v <- equalizer_v(inpt_v=inpt_v, default_val=default_val)

    ref_v <- base_v

    res_l <- list()

    for (ptrn in inpt_v){

        cur_delta = c()

        ptrn <- unlist(strsplit(ptrn, split=""))

        for (ltr in ptrn){

            cur_delta = c(cur_delta, match(ltr, base_v))

        }

        res_l <- append(res_l, list(cur_delta))

    }

    rtn_l <- list()

    rmids <- c()

    sub_rmids <- c()

    if (length(excl_v) > 0){

        rmids <- as.vector(mapply(function(x) return(match(x, chr_removr(inpt_v=inpt_v, ptrn_v=c(default_val)))), excl_v))     

    }

    if (length(rtn_v) > 0){

        rmids <- c(1:length(inpt_v))[-as.vector(mapply(function(x) return(match(x, chr_removr(inpt_v=inpt_v, ptrn_v=c(default_val)))), rtn_v))]

    }

    if (length(sub_excl_v) > 0){

        sub_rmids <-  as.vector(mapply(function(x) return(match(x, chr_removr(inpt_v=inpt_v, ptrn_v=c(default_val)))), sub_excl_v))     

    }

    if (length(sub_rtn_v) > 0){

        sub_rmids <- c(1:length(inpt_v))[-as.vector(mapply(function(x) return(match(x, c(inpt_v=inpt_v, ptrn_v=c(default_val)))), sub_rtn_v))]

    }

    if (length(rmids) > 0){

        inpt_v2 <- inpt_v[-rmids] 

        res_l2 <- res_l[-rmids]

    }else{

        inpt_v2 <- inpt_v

        res_l2 <- res_l

    }

    for (f_ptrn in 1:length(res_l2)){

            pre_l <- list(chr_removr(inpt_v=inpt_v2[f_ptrn], ptrn_v=default_val))

            pre_v <- c()

            f_ptrn_v <- unlist(res_l2[f_ptrn])

            for (cur_ptrn in res_l[-c(sub_rmids, (match(inpt_v[f_ptrn], inpt_v)))]){

                    diff_val = 0

                    for (pos in 1:length(cur_ptrn)){

                        if (cur_ptrn[pos] != f_ptrn_v[pos]){

                            diff_val = diff_val + 1 

                        }

                    }

                    pre_v <- c(pre_v, diff_val)

            }

            pre_ptrn <- chr_removr(inpt_v=inpt_v[-c(sub_rmids, 
                                        (match(inpt_v[f_ptrn], inpt_v)))], ptrn_v=c(default_val))

            pre_l <- append(x=pre_l, values=list(pre_ptrn))

            pre_l <- append(x=pre_l, values=list(sort(pre_v)))

            rtn_l <- append(x=rtn_l, values=pre_l)

    }
  
    return(rtn_l)

}

#' v_to_datf
#'
#' Allow to convert a vector to a dataframe according to a separator.
#' 
#' @param inpt_v is the input vector
#' @param sep_ is the separator of the elements in inpt_v, defaults to ""
#' 
#' @examples
#'
#' print(cut_v(inpt_v=c("oui", "non", "oui", "non")))
#' 
#' #    X.o. X.u. X.i.
#' #oui "o"  "u"  "i" 
#' #non "n"  "o"  "n" 
#' #oui "o"  "u"  "i" 
#' #non "n"  "o"  "n" 
#' 
#' print(cut_v(inpt_v=c("ou-i", "n-on", "ou-i", "n-on"), sep_="-"))
#' 
#' #     X.ou. X.i.
#' #ou-i "ou"  "i" 
#' #n-on "n"   "on"
#' #ou-i "ou"  "i" 
#' #n-on "n"   "on"
#' 
#' @export

cut_v <- function(inpt_v, sep_=""){

        rtn_datf <- data.frame(matrix(data=NA, nrow=0, ncol=length(unlist(strsplit(inpt_v[1], split=sep_)))))

        for (el in inpt_v){ rtn_datf <- rbind(rtn_datf, unlist(strsplit(el, split=sep_))) }

        return(rtn_datf)

}

#' id_keepr_datf
#'
#' Allow to get the original indexes after multiple equality comparaison according to the original number of row
#'
#' @param inpt_datf is the input dataframe
#' @param col_v is the vector containing the column numbers or names to be compared to their respective elements in "el_v"
#' @param el_v is a vector containing the elements that may be contained in their respective column described in "col_v" 
#' @param rstr_l is a list containing the vector composed of the indexes of the elements chosen for each comparison. If the length of the list is inferior to the lenght of comparisons, so the last vector of rstr_l will be the same as the last one to fill make rstr_l equal in term of length to col_v and el_v
#' @examples
#' 
#' datf1 <- data.frame(c("oui", "oui", "oui", "non", "oui"), 
#'      c("opui", "op", "op", "zez", "zez"), c(5:1), c(1:5))
#' 
#' print(id_keepr(inpt_datf=datf1, col_v=c(1, 2), el_v=c("oui", "op")))
#'
#' #[1] 2 3
#' 
#' print(id_keepr(inpt_datf=datf1, col_v=c(1, 2), el_v=c("oui", "op"), 
#'      rstr_l=list(c(1:5), c(3, 2, 2, 2, 3))))
#'
#' #[1] 2 3
#'
#' print(id_keepr(inpt_datf=datf1, col_v=c(1, 2), el_v=c("oui", "op"), 
#'      rstr_l=list(c(1:5), c(3))))
#'
#' #[1] 3
#'
#' print(id_keepr(inpt_datf=datf1, col_v=c(1, 2), el_v=c("oui", "op"), rstr_l=list(c(1:5))))
#' 
#' #[1] 2 3
#' 
#' @export

id_keepr <- function(inpt_datf, col_v=c(), el_v=c(), rstr_l=NA){

    rtn_v <- c(1:nrow(inpt_datf))

    if (typeof(col_v) == "character"){

        cl_nms <- colnames(inpt_datf)

        for (i in 1:length(col_v)){

                col_v[i] <- match(col_v[i], cl_nms)

        }

        col_v <- as.numeric(col_v)

    }

    if (all(is.na(rstr_l))){

        for (i in 1:length(col_v)){

            rtn_v <- rtn_v[inpt_datf[rtn_v, col_v[i]] == el_v[i]]  

        }

        return(rtn_v)

    }else if (length(rstr_l) < length(col_v)){

            lst_v <- unlist(rstr_l[length(rstr_l)])

            for (i in (length(rstr_l)+1):length(col_v)){

                rstr_l <- append(x=rstr_l, values=list(lst_v))

            }

    }

    pre_vec <- c()

    fun <- function() { return(c(pre_vec, FALSE)) }

    for (i in 1:length(col_v)){

        pre_vec2 <- mapply(function(x) return(fun()), c(1:length(rtn_v)))

        interst <- intersect(unlist(rstr_l[i]), rtn_v)

        pre_vec2[interst] <- inpt_datf[interst, col_v[i]] == el_v[i]

        rtn_v <- rtn_v[pre_vec2]  

    }

    return(rtn_v)

}

#' better_unique
#'
#' Returns the element that are not unique from the input vector
#'
#' @param inpt_v  is the input vector containing the elements
#' @param occu is a parameter that specifies the occurence of the elements that must be returned, defaults to ">-1-" it means that the function will return all the elements that are present more than one time in inpt_v. The synthax is the following "comparaison_type-actual_value-". The comparaison type may be "==" or ">" or "<". Occu can also be a vector containing all the occurence that must have the elements to be returned.
#' @examples
#'
#' print(better_unique(inpt_v=c("oui", "oui", "non", "non", "peut", "peut1", "non")))
#'
#' #[1] "oui" "non"
#'
#' print(better_unique(inpt_v=c("oui", "oui", "non", "non", "peut", "peut1", "non"), occu="==-2-"))
#'
#' #[1] "oui"
#'
#' print(better_unique(inpt_v=c("oui", "oui", "non", "non", "peut", "peut1", "non"), occu=">-2-"))
#'
#' #[1] "non"
#' 
#' print(better_unique(inpt_v=c("oui", "oui", "non", "non", "peut", "peut1", "non"), occu=c(1, 3)))
#' 
#' #[1] "non"   "peut"  "peut1"
#'
#' print(better_unique(inpt_v = c("a", "b", "c", "c"), occu = "==-1-"))
#' 
#' [1] "a" "b"
#'
#' print(better_unique(inpt_v = c("a", "b", "c", "c"), occu = "<-2-"))
#'
#' [1] "a" "b"
#'
#' @export

better_unique <- function(inpt_v, occu=">-1-"){

   rtn_v <- c()

   if (typeof(occu) == "character"){

           pre_vec <- str_locate(occu, "-(.*?)-")

           occu_v <- unlist(strsplit(occu, split=""))

           max_val <- as.numeric(occu_v[(pre_vec[1]+1):(pre_vec[length(pre_vec)]-1)])

           comp_ <- paste(occu_v[1:(pre_vec[1] - 1)], collapse="")

           if (comp_ == "=="){

                for (el in unique(inpt_v)){ if (sum(inpt_v == el) == max_val) { rtn_v <- c(rtn_v, el) } }

           }

           if (comp_ == ">"){

                for (el in unique(inpt_v)){ if (sum(inpt_v == el) > max_val) { rtn_v <- c(rtn_v, el) } }

           }

           if (comp_ == "<"){

                for (el in unique(inpt_v)){ if (sum(inpt_v == el) < max_val) { rtn_v <- c(rtn_v, el) } }

           }

   }else{

          for (el in unique(inpt_v)){ if (sum(inpt_v == el) %in% occu) { rtn_v <- c(rtn_v, el) } }

   }

   return(rtn_v)

}

#' r_print
#'
#' Allow to print vector elements in one row.
#'
#' @param inpt_v is the input vector
#' @param sep_ is the separator between each elements
#' @param begn is the character put at the beginning of the print
#' @param end is the character put at the end of the print
#' @examples
#'
#' print(r_print(inpt_v=c(1:33)))
#'
#' #[1] "This is  1 and 2 and 3 and 4 and 5 and 6 and 7 and 8 and 9 and 10 and 11 and 12 and 13 
#' #and 14 and 15 and 16 and 17 and 18 and 19 and 20 and 21 and 22 and 23 and 24 and 25 and 26 
#' #and 27 and 28 and 29 and 30 and 31 and 32 and 33 and , voila!"
#'
#' @export

r_print <- function(inpt_v, sep_="and", begn="This is", end=", voila!"){

        rtn_val <- ""

       for (el in inpt_v){ rtn_val <- paste(rtn_val, el, sep_, sep=" ") } 

       return(paste(begn, rtn_val, end, sep=" "))

}

#' str_remove_untl
#'
#' Allow to remove pattern within elements from a vector precisely according to their occurence.
#'
#' @param inpt_v is the input vector
#' @param ptrn_rm_v is a vector containing the patterns to remove
#' @param untl is a list containing the occurence(s) of each pattern to remove in the elements.
#' @param nvr_following_ptrn is a sequel of characters that you are sure is not present in any of the elements in inpt_v
#'
#' @examples
#'
#' vec <- c("45/56-/98mm", "45/56-/98mm", "45/56-/98-mm//")
#' 
#' print(str_remove_untl(inpt_v=vec, ptrn_rm_v=c("-", "/"), untl=list(c("max"), c(1))))
#' 
#' #[1] "4556/98mm"   "4556/98mm"   "4556/98mm//"
#' 
#' print(str_remove_untl(inpt_v=vec, ptrn_rm_v=c("-", "/"), untl=list(c("max"), c(1:2))))
#' 
#' #[1] "455698mm"   "455698mm"   "455698mm//"
#'
#' print(str_remove_untl(inpt_v=vec[1], ptrn_rm_v=c("-", "/"), untl=c("max")))
#'
#' #[1] "455698mm" "455698mm" "455698mm"
#' 
#' @export

str_remove_untl <- function(inpt_v, ptrn_rm_v=c(), untl=list(c(1)), nvr_following_ptrn="NA"){

   rtn_v <- c()

   if (length(untl) < length(ptrn_rm_v)){

           for (i in 1:(length(ptrn_rm_v) - (length(untl)))){

                   untl <- append(x=untl, values=list(unlist(untl[length(untl)])))

           }

   }

   for (el in inpt_v){

        pre_el <- el

        cur_el <- el

        for (ptrn in 1:length(ptrn_rm_v)) {

                cur_el <- str_remove(string=cur_el, pattern=ptrn_rm_v[ptrn])

                if (unlist(untl[ptrn])[1] == "max"){

                        while (cur_el != pre_el){

                                pre_el <- cur_el

                                cur_el <- str_remove(string=cur_el, pattern=ptrn_rm_v[ptrn])

                        }

                }else {

                        cur_untl <- unlist(untl[ptrn])

                        cnt = 1

                        pre_cnt <- 1

                        rm_ids <- c()

                        cur_el_vstr <- cur_el

                        cur_untl <- cur_untl - 1

                        cur_untl <- cur_untl[cur_untl > 0]

                        cnt2 = -1

                        while (cur_el_vstr != pre_el & cnt <= length(cur_untl)){

                                for (i in pre_cnt:cur_untl[cnt]){

                                        rm_id <- str_locate(string=cur_el_vstr, pattern=ptrn_rm_v[ptrn])

                                        pre_el <- cur_el_vstr
                                        
                                        cur_el_vstr <- str_remove(string=cur_el_vstr, pattern=ptrn_rm_v[ptrn])

                                        cnt2 = cnt2 + 1

                                }

                                rm_id <- rm_id + cnt2

                                if (all(is.na(rm_id)) == FALSE){  rm_ids <- c(rm_ids, rm_id[1]:rm_id[2]) }

                                pre_cnt = cur_untl[cnt] + 1

                                cnt = cnt + 1

                        }

                        if (length(rm_ids) > 0){

                                cur_el <- unlist(strsplit(x=cur_el, split=""))[-rm_ids]

                                cur_el <- paste(cur_el, collapse="")

                        }

                }

        }

        rtn_v <- c(rtn_v, cur_el)

   }

   return(rtn_v)

}

#' regroupr
#'
#' Allow to sort data like "c(X1/Y1/Z1, X2/Y1/Z2, ...)" to what you want. For example it can be to "c(X1/Y1/21, X1/Y1/Z2, ...)"
#'
#' @param inpt_v is the input vector containing all the data you want to sort in a specific way. All the sub-elements should be separated by a unique separator such as "-" or "/"
#' @param sep_ is the unique separator separating the sub-elements in each elements of inpt_v
#' @param order is a vector describing the way the elements should be sorted. For example if you want this dataset  "c(X1/Y1/Z1, X2/Y1/Z2, ...)" to be sorted by the last element you should have order=c(3:1), for example, and it should returns something like this c(X1/Y1/Z1, X2/Y1/Z1, X1/Y2/Z1, ...) assuming you have only two values for X. 
#' @param l_order is a list containing the vectors of values you want to order first for each sub-elements
#' @examples 
#'
#' vec <- multitud(l=list(c("a", "b"), c("1", "2"), c("A", "Z", "E"), c("Q", "F")), sep_="/")
#'
#' print(vec)
#' 
#' # [1] "a/1/A/Q" "b/1/A/Q" "a/2/A/Q" "b/2/A/Q" "a/1/Z/Q" "b/1/Z/Q" "a/2/Z/Q"
#' # [8] "b/2/Z/Q" "a/1/E/Q" "b/1/E/Q" "a/2/E/Q" "b/2/E/Q" "a/1/A/F" "b/1/A/F"
#' #[15] "a/2/A/F" "b/2/A/F" "a/1/Z/F" "b/1/Z/F" "a/2/Z/F" "b/2/Z/F" "a/1/E/F"
#' #[22] "b/1/E/F" "a/2/E/F" "b/2/E/F"
#'
#' print(regroupr(inpt_v=vec, sep_="/"))
#'
#' # [1] "a/1/1/1"   "a/1/2/2"   "a/1/3/3"   "a/1/4/4"   "a/1/5/5"   "a/1/6/6"  
#' # [7] "a/2/7/7"   "a/2/8/8"   "a/2/9/9"   "a/2/10/10" "a/2/11/11" "a/2/12/12"
#' #[13] "b/1/13/13" "b/1/14/14" "b/1/15/15" "b/1/16/16" "b/1/17/17" "b/1/18/18"
#' #[19] "b/2/19/19" "b/2/20/20" "b/2/21/21" "b/2/22/22" "b/2/23/23" "b/2/24/24"
#'
#'  vec <- vec[-2]
#'
#'  print(regroupr(inpt_v=vec, sep_="/"))
#'
#' # [1] "a/1/1/1"   "a/1/2/2"   "a/1/3/3"   "a/1/4/4"   "a/1/5/5"   "a/1/6/6"  
#' # [7] "a/2/7/7"   "a/2/8/8"   "a/2/9/9"   "a/2/10/10" "a/2/11/11" "a/2/12/12"
#' #[13] "b/1/13/13" "b/1/14/14" "b/1/15/15" "b/1/16/16" "b/1/17/17" "b/2/18/18"
#' #[19] "b/2/19/19" "b/2/20/20" "b/2/21/21" "b/2/22/22" "b/2/23/23"
#'
#' print(regroupr(inpt_v=vec, sep_="/", order=c(4:1)))
#'
#' #[1] "1/1/A/Q"   "2/2/A/Q"   "3/3/A/Q"   "4/4/A/Q"   "5/5/Z/Q"   "6/6/Z/Q"  
#' # [7] "7/7/Z/Q"   "8/8/Z/Q"   "9/9/E/Q"   "10/10/E/Q" "11/11/E/Q" "12/12/E/Q"
#' #[13] "13/13/A/F" "14/14/A/F" "15/15/A/F" "16/16/A/F" "17/17/Z/F" "18/18/Z/F"
#' #[19] "19/19/Z/F" "20/20/Z/F" "21/21/E/F" "22/22/E/F" "23/23/E/F" "24/24/E/F"
#'
#' @export

regroupr <- function(inpt_v, sep_="-", order=c(1:length(unlist(strsplit(x=inpt_v[1], split=sep_)))), l_order=NA){

        id_keepr <- function(inpt_datf, col_v=c(), el_v=c(), rstr_l=NA){

            rtn_v <- c(1:nrow(inpt_datf))

            if (typeof(col_v) == "character"){

                cl_nms <- colnames(inpt_datf)

                for (i in 1:length(col_v)){

                        col_v[i] <- match(col_v[i], cl_nms)

                }

                col_v <- as.numeric(col_v)

            }

            if (all(is.na(rstr_l))){

                for (i in 1:length(col_v)){

                    rtn_v <- rtn_v[inpt_datf[rtn_v, col_v[i]] == el_v[i]]  

                }

                return(rtn_v)

            }else if (length(rstr_l) < length(col_v)){

                    lst_v <- unlist(rstr_l[length(rstr_l)])

                    for (i in (length(rstr_l)+1):length(col_v)){

                        rstr_l <- append(x=rstr_l, values=list(lst_v))

                    }

            }

            pre_vec <- c()

            fun <- function() { return(c(pre_vec, FALSE)) }

            for (i in 1:length(col_v)){

                pre_vec2 <- mapply(function(x) return(fun()), c(1:length(rtn_v)))

                interst <- intersect(unlist(rstr_l[i]), rtn_v)

                pre_vec2[interst] <- (inpt_datf[interst, col_v[i]] == el_v[i])

                rtn_v <- rtn_v[pre_vec2]  

            }

            return(rtn_v)

        }

        colins_datf <- function(inpt_datf, target_col=list(), target_pos=list()){

            cl_nms <- colnames(inpt_datf)

            for (id_vec in 1:length(target_pos)){

                    vec <- unlist(target_pos[id_vec])

                    if (typeof(vec) == "character"){

                            pre_v <- c()

                            for (el in vec){

                                pre_v <- c(pre_v, match(el, cl_nms))

                            }

                            target_pos <- append(x=target_pos, values=list(pre_v), after=id_vec)

                            target_pos <- target_pos[-id_vec]

                    }

            }

            for (cl in 1:length(target_col)){

                cur_col <- unlist(target_col[cl])

                cur_pos_v <- unlist(target_pos[cl])

                for (pos in 1:length(cur_pos_v)){

                    idx <- cur_pos_v[pos]

                    if (idx == 0){

                        inpt_datf <- cbind(cur_col, inpt_datf[(idx+1):ncol(inpt_datf)])

                    }else if (idx < ncol(inpt_datf)){

                        inpt_datf <- cbind(inpt_datf[1:idx], cur_col, inpt_datf[(idx+1):ncol(inpt_datf)])

                    }else{

                        inpt_datf <- cbind(inpt_datf[1:idx], cur_col)

                    }

                    if (pos < length(cur_pos_v)){

                        cur_pos_v[(pos+1):length(cur_pos_v)] = cur_pos_v[(pos+1):length(cur_pos_v)] + 1 
                 
                    }

                    if (cl < length(target_pos)){

                            for (i in (cl+1):length(target_pos)){

                                target_pos <- append(x=target_pos, values=(unlist(target_pos[i])+1), after=i)

                                target_pos <- target_pos[-i]
                            
                            } 

                    }

                }

            }

          return(inpt_datf)

    }
    
    cut_v <- function(inpt_v, sep_=""){

        rtn_datf <- data.frame(matrix(data=NA, nrow=0, ncol=length(unlist(strsplit(inpt_v[1], split=sep_)))))

        for (el in inpt_v){ rtn_datf <- rbind(rtn_datf, unlist(strsplit(el, split=sep_))) }

        return(rtn_datf)

    }

    paste_datf <- function(inpt_datf, sep=""){

            if (ncol(as.data.frame(inpt_datf)) == 1){ 

                return(inpt_datf) 

            }else {

                rtn_datf <- inpt_datf[,1]

                for (i in 2:ncol(inpt_datf)){

                    rtn_datf <- paste(rtn_datf, inpt_datf[,i], sep=sep)

                }

                return(rtn_datf)

            }

  }

  w_datf <- cut_v(inpt_v, sep_=sep_) 

  if (all(is.na(l_order))){

          l_order <- list()

          for (i in order){

                  l_order <- append(x=l_order, values=list(unique(w_datf[,i])))

          }

  }

  cur_el <- w_datf[, order[1]]  

  v_ids <- c(1:nrow(w_datf))

  rec_ids = 0

  for (el in unlist(l_order[1])){

    cur_ids <- which(cur_el == el)

    v_ids[(rec_ids + 1):(rec_ids + length(cur_ids))] <- el 

    rec_ids = rec_ids + length(cur_ids)

  }

  w_datf <- cbind(w_datf[, order[1]], w_datf)

  order <- order + 1

  cnt = 2

  for (I in order[2:length(order)]){

        cur_el <- w_datf[, I]

        cur_v_ids <- c(1:nrow(w_datf))

        pre_bind_v <- c(1:nrow(w_datf))

        rec_ids <- c()

        rec_ids2 <- c()

        for (el in unique(v_ids)){ 

                cur_ids_stay <- which(w_datf[, 1] == el) 

                rec_ids2 <- c(rec_ids, cur_ids_stay)

                for (el2 in unlist(l_order[cnt])){

                    cur_ids <- id_keepr(inpt_datf=w_datf, col_v=c(I), el_v=c(el2), rstr_l=list(list(cur_ids_stay))) 

                    if (length(cur_ids) > 0){

                        pre_bind_v[cur_ids] <- el2

                        cur_v_ids[(length(rec_ids)+1):(length(rec_ids)+length(cur_ids))] <- el2

                        rec_ids <- c(rec_ids, cur_ids)

                    }

                }

        }

        if (order[cnt] > order[(cnt-1)]){

                w_datf[, 1] <- paste_datf(inpt_datf=data.frame(w_datf[, 1], pre_bind_v), sep="")

                v_ids <- as.vector(mapply(function(x, y) return(paste(x, sep_, y, sep="")), v_ids, cur_v_ids))

        }else{

                w_datf[, 1] <- paste_datf(inpt_datf=data.frame(pre_bind_v, w_datf[, 1]), sep="")

                v_ids <- as.vector(mapply(function(x, y) return(paste(y, sep_, x, sep="")), v_ids, cur_v_ids))

        }

        cnt = cnt + 1

  }

  return(v_ids)

}

#' vector_replacor
#'
#' Allow to replace certain values in a vector.
#'
#' @param inpt_v is the input vector
#' @param sus_val is a vector containing all the values that will be replaced
#' @param rpl_val is a vector containing the value of the elements to be replaced (sus_val), so sus_val and rpl_val should be the same size
#' @param grep_ is if the elements in sus_val should be equal to the elements to replace in inpt_v or if they just should found in the elements
#' @examples
#' 
#' print(vector_replacor(inpt_v=c(1:15), sus_val=c(3, 6, 8, 12), 
#'      rpl_val=c("oui", "non", "e", "a")))
#'
#' # [1] "1"   "2"   "oui" "4"   "5"   "non" "7"   "e"   "9"   "10"  "11"  "a"  
#' #[13] "13"  "14"  "15" 
#' 
#' print(vector_replacor(inpt_v=c("non", "zez", "pp a ftf", "fdatfd", "assistance", 
#' "ert", "repas", "repos"), 
#' sus_val=c("pp", "as", "re"), rpl_val=c("oui", "non", "zz"), grep_=TRUE))
#' 
#' #[1] "non"  "zez"  "oui"  "fdatfd" "non"  "ert"  "non"  "zz"  
#'
#' @export

vector_replacor <- function(inpt_v=c(), sus_val=c(), rpl_val=c(), grep_=FALSE){

        if (grep_){

            for (el in 1:length(inpt_v)){

                    cnt = 1

                    stop <- 0

                    while (cnt < (length(sus_val) + 1) & stop == 0){

                            if (str_detect(string=inpt_v[el], pattern=sus_val[cnt])){

                                inpt_v[el] <- rpl_val[cnt]

                                stop <- 1

                            }

                            cnt = cnt + 1

                    }

            }

        }else{

            for (el in 1:length(inpt_v)){

                pre_match <- match(x=inpt_v[el], table=sus_val)

                if (!(is.na(pre_match))){

                        inpt_v[el] <- rpl_val[pre_match]

                }

            }

        }

    return(inpt_v)

}

#' cutr_v
#'
#' Allow to reduce all the elements in a vector to a defined size of nchar
#'
#' @param inpt_v is the input vector
#' @param untl is the maximum size of nchar authorized by an element, defaults to "min", it means the shortest element in the list
#'
#' @examples
#'
#' test_v <- c("oui", "nonon", "ez", "aa", "a", "dsfsdsds")
#' 
#' print(cutr_v(inpt_v=test_v, untl="min"))
#' 
#' #[1] "o" "n" "e" "a" "a" "d"
#'
#' print(cutr_v(inpt_v=test_v, untl=3))
#'
#' #[1] "oui" "non" "ez"  "aa"  "a"   "dsf"
#'
#' @export

cutr_v <- function(inpt_v, untl="min"){

    if (untl == "min"){

        untl_ <- nchar(inpt_v[1])

        for (el in inpt_v[2:length(inpt_v)]){

                if (nchar(el) < untl){ untl <- nchar(el) }

        }

    }

    for (i in 1:length(inpt_v)){

        if (nchar(inpt_v[i]) > untl){

                inpt_v[i] <- paste(unlist(strsplit(x=inpt_v[i], split=""))[1:untl], collapse="")

        }

    }

    return(inpt_v)

}


#' nb_follow
#'
#' Allow to get the number of certains patterns that may be after an index of a vector continuously, see examples
#'
#' @param inpt_v is the input vector
#' @param inpt_idx is the index
#' @param inpt_follow_v is a vector containing all the potential patterns that may follow the element in the vector at the index inpt_idx
#' @examples
#'
#' print(nb_follow(inpt_v = c(1:13), inpt_idx = 6, inpt_follow_v = c(5:9)))
#' 
#' [1] 3
#'
#' print(nb_follow(inpt_v = c("ou", "nn", "pp", "zz", "zz", "ee", "pp"), inpt_idx = 2, 
#'                 inpt_follow_v = c("pp", "zz")))
#'
#' [1] 3
#' 
#' @export

nb_follow <- function(inpt_v, inpt_idx, inpt_follow_v = c()){
  rtn <- 0
  pattern_v <- c()
  inpt_idx = inpt_idx + 1
  while (inpt_v[inpt_idx] %in% inpt_follow_v){
    inpt_idx = inpt_idx + 1
    rtn = rtn  + 1
  }
  return(rtn)
}

#' nb2_follow
#'
#' Allows to get the number and pattern of potential continuous pattern after an index of a vector, see examples
#' 
#' @param inpt_v is the input vector
#' @param inpt_idx is the index
#' @param inpt_follow_v is a vector containing the patterns that are potentially just after inpt_nb
#' @examples
#'
#' print(nb2_follow(inpt_v = c(1:12), inpt_idx = 4, inpt_follow_v = c(5)))
#' 
#' [1] 1 5
#' # we have 1 times the pattern 5 just after the 4nth index of inpt_v
#' 
#' print(nb2_follow(inpt_v = c(1, "non", "oui", "oui", "oui", "nop", 5), inpt_idx = 2, inpt_follow_v = c("5", "oui")))
#'
#' [1] "3"   "oui"
#'
#' # we have 3 times continuously the pattern 'oui' and 0 times the pattern 5 just after the 2nd index of inpt_v
#'
#' print(nb2_follow(inpt_v = c(1, "non", "5", "5", "5", "nop", 5), inpt_idx = 2, inpt_follow_v = c("5", "oui")))
#'
#' [1] "3" "5"
#'
#' @export

nb2_follow <- function(inpt_v, inpt_idx, inpt_follow_v = c()){
  rtn <- 0
  pattern_ = NA
  inpt_idx = inpt_idx + 1
  for (ptrn in inpt_follow_v){
     cnt = 0
     while (ptrn == inpt_v[inpt_idx]){
       inpt_idx = inpt_idx + 1
       cnt = cnt + 1
     }
     if (cnt > 0){
       rtn = rtn + cnt
       pattern_ <- ptrn
     }
  }
  return(c(rtn, pattern_))
}

#' better_split
#'
#' Allows to split a string by multiple split, returns a vector and not a list.
#' @param inpt is the input character
#' @param split_v is the vector containing the splits
#' @examples
#'
#' print(better_split(inpt = "o-u_i", split_v = c("-")))
#' 
#' [1] "o"   "u_i"
#'
#' print(better_split(inpt = "o-u_i", split_v = c("-", "_")))
#'
#' [1] "o" "u" "i"
#'
#' @export

better_split <- function(inpt, split_v = c()){
  for (split in split_v){
    pre_inpt <- inpt
    inpt <- c()
    for (el in pre_inpt){
      inpt <- c(inpt, unlist(strsplit(x = el, split = split)))
    }
  }
  return(inpt)
}

#' pre_to_post_idx
#'
#' Allow to convert indexes from a pre-vector to post-indexes based on a current vector, see examples
#'
#' @param inpt_idx is the vector containing the pre-indexes 
#' @param inpt_v is the new vector
#' @examples
#'
#' print(pre_to_post_idx(inpt_v = c("oui", "no", "eee"), inpt_idx = c(1:8)))
#' 
#' [1] 1 1 1 2 2 3 3 3
#'
#' As if the first vector was c("o", "u", "i", "n", "o", "e", "e", "e")
#'
#' @export

pre_to_post_idx <- function(inpt_v = c(), inpt_idx = c(1:length(inppt_v))){
  rtn_v <- c()
  cur_step = nchar(inpt_v[1])
  cnt = 1
  for (idx in 1:length(inpt_idx)){
    if (inpt_idx[idx] > cur_step){
      cnt = cnt + 1
      cur_step = cur_step + nchar(inpt_v[cnt])
    }
    rtn_v <- c(rtn_v, cnt)
  }
  return(rtn_v)
}

#' unique_total
#'
#' Returns a vector with the total amount of occurences for each element in the input vector. The occurences of each element follow the same order as the unique function does, see examples
#'
#' @param inpt_v is the input vector containing all the elements
#' @examples
#'
#' print(unique_total(inpt_v = c(1:12, 1)))
#' 
#'  [1] 2 1 1 1 1 1 1 1 1 1 1 1
#' 
#' print(unique_total(inpt_v = c(1:12, 1, 11, 11)))
#'
#'  [1] 2 1 1 1 1 1 1 1 1 1 3 1
#'
#' vec <- c(1:12, 1, 11, 11)
#' names(vec) <- c(1:15)
#' print(unique_total(inpt_v = vec))
#'
#'  1  2  3  4  5  6  7  8  9 10 11 12 
#'  2  1  1  1  1  1  1  1  1  1  3  1 
#'
#' @export

unique_total <- function(inpt_v = c()){
  rtn_v <- c()
  for (el in unique(inpt_v)){
    rtn_v <- c(rtn_v, length(grep(pattern = paste0("^", el, "$"), x = inpt_v)))
    names(rtn_v)[length(rtn_v)] <- names(inpt_v)[match(x = el, table = inpt_v)]
  }
  return(rtn_v)
}

#' match_by
#'
#' Allow to match elements by ids, see examples.  
#'
#' @param to_match_v is the vector containing all the elements to match
#' @param inpt_v is the input vector containong all the elements that could contains the elements to match. Each elements is linked to an element from inpt_ids at any given index, see examples. So inpt_v and inpt_ids must be the same size
#' @param inpt_ids is the vector containing all the ids for the elements in inpt_v. An element is linked to the id x is both are at the same index. So inpt_v and inpt_ids must be the same size 
#' @examples
#'
#' print(match_by(to_match_v = c("a"), inpt_v = c("a", "z", "a", "p", "p", "e", "e", "a"), 
#'                inpt_ids = c(1, 1, 1, 2, 2, 3, 3, 3)))
#' 
#' [1] 1 8
#'
#' print(match_by(to_match_v = c("a"), inpt_v = c("a", "z", "a", "a", "p", "e", "e", "a"), 
#'                inpt_ids = c(1, 1, 1, 2, 2, 3, 3, 3)))
#'
#' [1] 1 4 8
#'
#' print(match_by(to_match_v = c("a", "e"), inpt_v = c("a", "z", "a", "a", "p", "e", "e", "a"), 
#'                inpt_ids = c(1, 1, 1, 2, 2, 3, 3, 3)))
#' 
#' [1] 1 4 8 6
#'
#' @export

match_by <- function(to_match_v = c(), inpt_v = c(), inpt_ids = c()){
  rtn_v <- c()
  for (el in to_match_v){
    for (id in unique(inpt_ids)){
      if (!(is.na(match(x = el, table = inpt_v[grep(pattern = id, x = inpt_ids)])))){
        rtn_v <- c(rtn_v, (match(x = id, table = inpt_ids) +
                  match(x = el, table = inpt_v[grep(pattern = id, x = inpt_ids)]) - 1))
      }
    }
  }
  return(rtn_v)
}

#' see_diff
#'
#' Output the opposite of intersect(a, b). Already seen at: https://stackoverflow.com/questions/19797954/function-to-find-symmetric-difference-opposite-of-intersection-in-r  
#'
#' @param vec1 is the first vector
#' @param vec2 is the second vector
#' @examples
#'
#' print(see_diff(c(1:7), c(4:12)))
#'
#' [1] 1 2 3 8 9 10 11 12
#'
#' @export

see_diff <- function(vec1 = c(), vec2 = c()){
  return(setdiff(union(vec1, vec2), intersect(vec1, vec2)))
}

#' see_mode
#'
#' Allow to get the mode of a vector, see examples.
#'
#' @param inpt_v is the input vector
#' @examples
#'
#' print(see_mode(inpt_v = c(1, 1, 2, 2, 2, 3, 1, 2)))
#'
#' [1] 2
#'
#' print(see_mode(inpt_v = c(1, 1, 2, 2, 2, 3, 1)))
#'
#' [1] 1
#' 
#' @export

see_mode <- function(inpt_v = c()){
  unique_total <- function(inpt_v = c()){
    rtn_v <- c()
    for (el in unique(inpt_v)){
      rtn_v <- c(rtn_v, length(grep(pattern = paste0("^", el, "$"), x = inpt_v)))
    }
    return(rtn_v)
  }
  return(unique(inpt_v)[which.max(unique_total(inpt_v))])
}

#' same_order
#'
#' Allow to get if two vectors have their commun elements in the same order, see examples
#'
#' @param is the vector that gives the elements order
#' @param is the vector we want to test if its commun element with inpt_v_from are in the same order
#' @examples
#'
#' print(test_order(inpt_v_from = c(1:8), inpt_v_test = c(1, 4)))
#' 
#' [1] TRUE
#'
#' print(test_order(inpt_v_from = c(1:8), inpt_v_test = c(1, 4, 2)))
#' 
#' [1] FALSE
#'
#' @export

test_order <- function(inpt_v_from, inpt_v_test){
  lst_idx <- match(x = inpt_v_test[1], table = inpt_v_from)
  if (length(inpt_v_test) > 1){
    for (i in inpt_v_test){
      tst_idx <- match(x = i, table = inpt_v_from)
      if (tst_idx < lst_idx){
        return(FALSE)
      }
      lst_idx <- tst_idx
    }
  }
  return(TRUE)
}

#' old_to_new_idx
#'
#' Allow to convert index of elements in a vector `inpt_v` to index of an vector type 1:sum(nchar(inpt_v)), see examples
#'
#' @param inpt_v is the input vector 
#' @examples
#'
#' print(old_to_new_idx(inpt_v = c("oui", "no", "eeee")))
#'
#' [1] 1 1 1 2 2 3 3 3 3
#'
#' @export

old_to_new_idx <- function(inpt_v = c()){
  rtn_v <- c()
  cur_step = nchar(inpt_v[1])
  cnt = 1
  for (idx in 1:sum(nchar(inpt_v))){
    if (idx > cur_step){
      cnt = cnt + 1
      cur_step = cur_step + nchar(inpt_v[cnt])
    }
    rtn_v <- c(rtn_v, cnt)
  }
  return(rtn_v)
}

#' successive_diff 
#'
#' Allow to see the difference beteen the suxxessive elements of an numeric vector
#'
#' @param inpt_v is the input numeric vector
#' @examples
#'
#' print(successive_diff(c(1:10)))
#'
#' [1] 1 1 1 1 1
#'
#' print(successive_diff(c(1:11, 13, 19)))
#'
#' [1] 1 1 1 1 1 2 6
#' 
#' @export

successive_diff <- function(inpt_v){
  lngth <- length(inpt_v)
  if (lngth > 1){
    if (lngth %% 2 == 0){
      return(inpt_v[seq(from = 2, to = lngth, by = 2)] - inpt_v[seq(from = 1, to = (lngth - 1), by = 2)])
    }else{
      return(c(inpt_v[seq(from = 2, to = (lngth - 1), by = 2)] - inpt_v[seq(from = 1, to = (lngth - 2), by = 2)], 
               inpt_v[lngth] - inpt_v[(lngth - 1)]))
    }
  }
  return(NULL)
}

#' elements_equalifier
#'
#' Takes an input vector with elements that have different occurence, and output a vector with all these elements with the same number of occurence, see examples
#'
#' @param inpt_v is the input vector
#' @param untl is how many times each elements will be in the output vector
#' @examples
#'
#' print(elements_equalifier(letters, untl = 2))
#' 
#'  [1] "a" "b" "c" "d" "e" "f" "g" "h" "i" "j" "k" "l" "m" "n" "o" "p" "q" "r" "s"
#' [20] "t" "u" "v" "w" "x" "y" "z" "a" "b" "c" "d" "e" "f" "g" "h" "i" "j" "k" "l"
#' [39] "m" "n" "o" "p" "q" "r" "s" "t" "u" "v" "w" "x" "y" "z"
#'
#' print(elements_equalifier(c(letters, letters[-1]), untl = 2))
#' 
#'  [1] "a" "b" "c" "d" "e" "f" "g" "h" "i" "j" "k" "l" "m" "n" "o" "p" "q" "r" "s"
#' [20] "t" "u" "v" "w" "x" "y" "z" "b" "c" "d" "e" "f" "g" "h" "i" "j" "k" "l" "m"
#' [39] "n" "o" "p" "q" "r" "s" "t" "u" "v" "w" "x" "y" "z" "a"
#' 
#' @export

elements_equalifier <- function(inpt_v, untl = 3){
  better_unique <- function(inpt_v, occu=">-1-"){
   rtn_v <- c()
   if (typeof(occu) == "character"){
           pre_vec <- str_locate(occu, "-(.*?)-")
           occu_v <- unlist(strsplit(occu, split=""))
           max_val <- as.numeric(occu_v[(pre_vec[1]+1):(pre_vec[length(pre_vec)]-1)])
           comp_ <- paste(occu_v[1:(pre_vec[1] - 1)], collapse="")
           if (comp_ == "=="){
                for (el in unique(inpt_v)){ if (sum(inpt_v == el) == max_val) { rtn_v <- c(rtn_v, el) } }
           }
           if (comp_ == ">"){
                for (el in unique(inpt_v)){ if (sum(inpt_v == el) > max_val) { rtn_v <- c(rtn_v, el) } }
           }
           if (comp_ == "<"){
                for (el in unique(inpt_v)){ if (sum(inpt_v == el) < max_val) { rtn_v <- c(rtn_v, el) } }
           }
   }else{
          for (el in unique(inpt_v)){ if (sum(inpt_v == el) %in% occu) { rtn_v <- c(rtn_v, el) } }
   }
   return(rtn_v)
  }
  occu_val <- paste0("<-", untl, "-")
  for (el in better_unique(inpt_v = inpt_v, occu = occu_val)){
    while (length(grep(x = inpt_v, pattern = el)) < untl){
      inpt_v <- c(inpt_v, el)
    }
  }
  return(inpt_v)
}

#' to_unique
#'
#' Allow to transform a vector containing elements that have more than 1 occurence to a vector with only uniques elements.
#'
#' @param inpt_v is the input vectors
#' @param distinct_type takes two values: suffix or prefix
#' @param distinct_val takes two values: number (unique sequence of number to differencfiate each value) or letter (unique sequence of letters to differenciate each value)
#' @examples
#'
#' print(to_unique(inpt_v = c("a", "a", "e", "a", "i", "i"), 
#'                 distinct_type = "suffix", 
#'                 distinct_val = "number", 
#'                 sep = "-"))
#' 
#' [1] "a-1" "a-2" "e"   "a-3" "i-1" "i-2"
#'
#' print(to_unique(inpt_v = c("a", "a", "e", "a", "i", "i"), 
#'                 distinct_type = "suffix", 
#'                 distinct_val = "letter", 
#'                 sep = "-"))
#' 
#' [1] "a-a" "a-b" "e"   "a-c" "i-a" "i-b"
#'
#' print(to_unique(inpt_v = c("a", "a", "e", "a", "i", "i"), 
#'                 distinct_type = "prefix", 
#'                 distinct_val = "number", 
#'                 sep = "/"))
#'
#' [1] "1/a" "2/a" "e"   "3/a" "1/i" "2/i"
#'
#' print(to_unique(inpt_v = c("a", "a", "e", "a", "i", "i"), 
#'                 distinct_type = "prefix", 
#'                 distinct_val = "letter", 
#'                 sep = "_"))
#' 
#' [1] "a_a" "b_a" "e"   "c_a" "a_i" "b_i" 
#'
#' @export

to_unique <- function(inpt_v, distinct_type = "suffix", distinct_val = "number", sep = "-"){
  nb_to_letter <- function(x){
    rtn_v <- c()
    cnt = 0
    while (26 ** cnt <= x){
      cnt = cnt + 1
      reste <- x %% (26 ** cnt)
      if (reste != 0){
        if (reste >= 26){ reste2 <- reste / (26 ** (cnt - 1)) }else{ reste2 <- reste }
        rtn_v <- c(rtn_v, letters[reste2])
      }else{
        reste <- 26 ** cnt
        rtn_v <- c(rtn_v, letters[26])
      }
      x = x - reste
    }
    return(paste(rtn_v[length(rtn_v):1], collapse = ""))
  }
  better_unique <- function(inpt_v, occu=">-1-"){
    rtn_v <- c()
    if (typeof(occu) == "character"){
            pre_vec <- str_locate(occu, "-(.*?)-")
            occu_v <- unlist(strsplit(occu, split=""))
            max_val <- as.numeric(occu_v[(pre_vec[1]+1):(pre_vec[length(pre_vec)]-1)])
            comp_ <- paste(occu_v[1:(pre_vec[1] - 1)], collapse="")
            if (comp_ == "=="){
                 for (el in unique(inpt_v)){ if (sum(inpt_v == el) == max_val) { rtn_v <- c(rtn_v, el) } }
            }
            if (comp_ == ">"){
                 for (el in unique(inpt_v)){ if (sum(inpt_v == el) > max_val) { rtn_v <- c(rtn_v, el) } }
            }
    }else{
           for (el in unique(inpt_v)){ if (sum(inpt_v == el) %in% occu) { rtn_v <- c(rtn_v, el) } }
    }
    return(rtn_v)
  }
  non_unique_v <- better_unique(inpt_v = inpt_v)
  if (distinct_type == "suffix"){
    if (distinct_val == "number"){
      for (el in non_unique_v){
        cnt = 1
        for (idx in grep(x = inpt_v, pattern = el)){
          inpt_v[idx] <- paste0(inpt_v[idx], sep, cnt)  
          cnt = cnt + 1
        }
      }
    }else if (distinct_val == "letter"){
      for (el in non_unique_v){
        cnt = 1
        for (idx in grep(x = inpt_v, pattern = el)){
          inpt_v[idx] <- paste0(inpt_v[idx], sep, nb_to_letter(cnt))
          cnt = cnt + 1
        }
      }
    }else{
      return("Invalid distinct_val specification")
    }
  }else if (distinct_type == "prefix"){
    if (distinct_val == "number"){
      for (el in non_unique_v){
        cnt = 1
        for (idx in grep(x = inpt_v, pattern = el)){
          inpt_v[idx] <- paste0(cnt, sep, inpt_v[idx])  
          cnt = cnt + 1
        }
      }
    }else if (distinct_val == "letter"){
      for (el in non_unique_v){
        cnt = 1
        for (idx in grep(x = inpt_v, pattern = el)){
          inpt_v[idx] <- paste0(nb_to_letter(cnt), sep, inpt_v[idx])
          cnt = cnt + 1
        }
      }
    }else{
      return("Invalid distinct_val specification")
    }
  }else{
    return("Invalid distinct_type specification")
  }
  return(inpt_v)
}

#' old_to_new_idx_nested
#'
#' Allow to convert the indices of vector ('from_v_ids') which are related to the elements of 'from_v_val' vector, to fir the newly established maximum character of elements in 'from_v_val', see examples.
#'
#' @param from_v_val is the input vector of elements
#' @param from_v_ids is the input vector of indices
#' @param val is the value - 1 from which the number of character of an element is too high, so the indices in 'from_v_ids' will be modified
#'
#' @examples
#'
#' print(old_to_new_idx_nested(from_v_val = c("oui", "no", "oui"), from_v_ids = c(1, 2, 3, 5), val = 1))
#'
#' [1] 1 4 6 10
#' 
#' # the new 'from_v_ids' is theorically c('o', 'u', 'i', 'n', 'o', 'o', 'u', 'i')
#' # here the indices five does not technically correspond to any element in the original 'from_v_val', 
#' # but corresponds to the 'o' of 'no' if the maximum number of character of from_v_val is one
#' # However, the old five index now corresponds to the 10nth elements of the new from_v_val which is 
#' # outside from the new 'from_v_val' by 2 indices, 5 for the old 'from_v_val'
#'
#' print(old_to_new_idx_nested(from_v_val = c("oui", "no", "oui"), from_v_ids = c(1, 2, 3, 5), val = 2))
#'
#' [1] 1 3 4 7
#'
#' @export

old_to_new_idx_nested <- function(from_v_val = c(), from_v_ids = c(), val = 1){
  print(val)
  for (I in 1:length(from_v_val)){
    if (nchar(from_v_val[I]) > val){
      print(paste("passe", nchar(from_v_val[I])), val)
      for (i in 1:(nchar(from_v_val[I]) - val)){
        from_v_ids[(I + 1):length(from_v_ids)] = from_v_ids[(I + 1):length(from_v_ids)] + 1
      }
    }
  }
  return(from_v_ids)
}


