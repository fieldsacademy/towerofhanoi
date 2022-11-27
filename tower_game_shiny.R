# Helper code for R shiny app to play Tower of Hanoi
# Author: Emma Kroell
# Last updated: November 26, 2022


#' Function that returns move options based on current towers set-up
#'
#' @param towers n by 3 matrix with entries for each block
#' (higher number is smaller block, 0 is no block)
#'
#' @return tibble with move number and label (for radio buttons) and
#'          corresponding columns to move from and to
tower_move_choices <- function(towers){
  n <- dim(towers)[1]   # max height of towers
  letter_labels <- LETTERS[1:n] |>  rev()  # letter labels for blocks
  # get the value of the top block in each column (0 if none)
  top_block <- rep(NA,3)
  for (i in 1:3){
    col_i <- towers[,i]   # pull relevant column
    blocks <- col_i[col_i>0]   # get top nonzero block
    top_block[i] <- ifelse(length(blocks) > 0, head(blocks),0)  # zero if no blocks
  }

  # get ordering of columns by smallest block size (largest number)
  indices <- order(top_block)

  # Valid moves: from a larger number to a smaller one
  if (! max(towers[,indices[2]])==0){
    move_from <- c(indices[3],indices[3],indices[2])
    move_block <- top_block[move_from]
    #move_block <- letter_labels[top_block[move_from]]
    move_to   <- c(indices[1],indices[2],indices[1])
  } else {  # if two columns have nothing, only two possible moves
    move_from <- c(indices[3],indices[3])
    #move_block <- letter_labels[top_block[move_from]]
    move_block <- top_block[move_from]
    move_to   <- c(indices[1],indices[2])
  }

  move <- as_tibble(cbind(move_from,move_to,move_block)) |>
    mutate(label = paste("Move block", letter_labels[move_block],
                         "from column",move_from, "to column", move_to),
           move_number = seq(1:length(move_from))) |>
    select(move_number, label, move_from, move_to)

  return(move)
}

#' Helper function that moves selected block in tower game
#'
#' @param towers n by 3 matrix with entries for each block
#'    (higher number is smaller block, 0 is no block)
#' @param index_from index to move top block from (int between 1 and 3)
#' @param index_to index to move top block to (int between 1 and 3)
#'
#' @return updated towers n by 3 matrix
tower_move <- function(towers,index_from,index_to){
  # find index of maximum block in column we are moving from:
  move_index <- which(towers[,index_from] == max(towers[,index_from]))
  # record the value of this block:
  move_value <- towers[move_index,index_from]
  # remove the block by setting that matrix entry to 0
  towers[move_index,index_from] <- 0
  # move block by setting lowest zero entry in the receiving column to that
  # block value:
  towers[tail(which(towers[,index_to]==0),1),index_to] <- move_value

  return(towers)
}



#' Plot towers in tower game using ggplot
#'
#' @param towers n by 3 matrix with entries for each block
#' (higher number is smaller block, 0 is no block)
#'
#' @return ggplot object
plot_towers <- function(towers){
  n <- dim(towers)[1]   # max height of towers
  letter_labels <- LETTERS[1:n] |>  rev()  # letter labels for blocks

  # set up empty vectors for plot elements:
  xmin <- rep(NA,n*3)
  xmax <- rep(NA,n*3)
  ymin <- rep(NA,n*3)
  ymax <- rep(NA,n*3)
  colour_vec <- rep(NA,n*3)
  letters_vec <- rep(NA,n*3)

  # set colour palette
  colours <- RColorBrewer::brewer.pal(7, "Set2")[1:n]

  # for each column, record dimensions of rectangle (xmin, xmax, ymin, ymax)
  # to plot each block (NA if no block) as well as colour from palette
  # and letter from list (NA if no block)
  for (i in 1:3){
    col_i <- towers[,i]
    xmin[((i-1)*n+1):(i*n)] <- ifelse(col_i>0, (i-1) + 0.5 + (col_i-1)*0.075, NA)
    xmax[((i-1)*n+1):(i*n)] <- ifelse(col_i>0, i + 0.5 - (col_i-1)*0.075, NA)
    ymin_temp <- seq(0.5*length(col_i)-0.5,0,-0.5)
    ymin_temp[which(col_i == 0)] <- NA
    ymax_temp <- seq(0.5*length(col_i),0.5,-0.5)
    ymax_temp[which(col_i == 0)] <- NA
    ymin[((i-1)*n+1):(i*n)] <- ymin_temp
    ymax[((i-1)*n+1):(i*n)] <- ymax_temp
    colours_temp <- colours[col_i]
    colour_vec[((i-1)*n+1):(i*n)] <- c(rep(NA,sum(as.numeric(col_i == 0))),colours_temp)
    letters_temp <- letter_labels[col_i]
    letters_vec[((i-1)*n+1):(i*n)] <- c(rep(NA,sum(as.numeric(col_i == 0))),letters_temp)
  }

  # make plot
  ggplot() +
    xlim(0.4,3.6) + ylim(0,5) +
    geom_rect(aes(xmin = xmin, xmax = xmax, ymin=ymin, ymax=ymax),
              fill = colour_vec, colour="black", na.rm=TRUE) +
    annotate("text", x = (xmin+xmax)/2, y = (ymin+ymax)/2,
             label = letters_vec, size = 10, na.rm = TRUE) +
    theme_void() +
    theme(axis.text.x = element_text(face="bold", color="#941515",size=18))
}
