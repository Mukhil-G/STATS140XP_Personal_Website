### Baseball Functions
library(dplyr)


ball <- c('BallCalled',
          'HitByPitch',
          'BallIntentional',
          'BallinDirt')

strike <- c('StrikeCalled',
            'StrikeSwinging',
            'InPlay',
            'FoulBall',
            'FoulBallNotFieldable',
            'FoulBallFieldable')

swung <- c('StrikeSwinging',
           'InPlay',
           'FoulBall',
           'FoulBallNotFieldable',
           'FoulBallFieldable')

whiff <- function(pitches) {
  miss <- 0
  swings <- 0
  if (nrow(pitches) == 0) {
    return (NaN)
  }
  for (i in 1:nrow(pitches)) {
    if (pitches[i,"PitchCall"] %in% swung) {
      swings <- swings + 1
    }
    if (pitches[i,"PitchCall"] == "StrikeSwinging") {
      miss <- miss + 1
    } 
  }
  return (round((miss/swings)*100, digits = 2))
} 

ozone <- function(pitches) {
  return(filter(pitches, PLH > 43 | PLH < 19 | PLS < -9.5 | PLS > 9.5))
}

chase <- function(pitches) {
  if (nrow(pitches) == 0) {
    return (NaN)
  }
  all <- ozone(pitches)
  total <- nrow(all)
  swings <- 0
  if (nrow(all) == 0) {
    return (NaN)
  }
  
  for (i in 1:nrow(all)) {
    if (all[i, "PitchCall"] %in% swung) {
      swings <- swings + 1
    }
  }
  return (round((swings/total)*100, digits = 2))
}

izone <- function(pitches) {
  return(filter(pitches, PLH <= 42 & PLH >= 20 & PLS >= -8.5 & PLS <= 8.5))  
}

zone_swing <- function(pitches) {
  if (nrow(pitches) == 0) {
    return (NaN)
  }
  all <- izone(pitches)
  total <- nrow(all)
  swings <- 0
  if (nrow(all) == 0) {
    return (NaN)
  }
  
  for (i in 1:nrow(all)) {
    if (all[i, "PitchCall"] %in% swung) {
      swings <- swings + 1
    }
  }
  return (round((swings/total)*100, digits = 2))
}

zone_whiff <- function(pitches) {
  if (nrow(pitches) == 0) {
    return (NaN)
  }
  all <- izone(pitches)
  swings <- 0
  misses <- 0
  
  if (nrow(all) == 0) {
    return (NaN)
  }
  for (i in 1:nrow(all)) {
    if (all[i, "PitchCall"] %in% swung) {
      swings <- swings + 1
    }
    if (all[i, "PitchCall"] == "StrikeSwinging") {
      misses <- misses + 1
    }
  }
  return (round((misses/swings)*100, digits = 2))
}

k_or_b <- function(pitches) {
  ks <- 0
  bs <- 0
  for (p in 1:nrow(pitches)){
    if (pitches[p, 'PitchCall'] %in% strike) {
      ks <- ks + 1
    }
    if (pitches[p, 'PitchCall'] %in% ball) {
      bs <- bs + 1
    }
  }
  return(c(ks, bs))
}

ba <- function(pitches) {
  at_bats <- c('Out','Single','Double','Triple','HomeRun','FieldersChoice','Error')
  hits <- c('Single','Double','Triple','HomeRun')
  ab <- 0
  h <- 0
  
  for (p in 1:nrow(pitches)) {
    if (pitches[p, 'PlayResult'] %in% hits){
      h <- h + 1
    }
    if (pitches[p, 'PlayResult'] %in% at_bats){
      ab <- ab + 1
    }
    if (pitches[p, 'PlayResult'] %in% c('StolenBase','CaughtStealing') & pitches[p, 'KorBB'] == 'Strikeout'){
      ab <- ab + 1
    }
  }
  avg <- h/ab
  return (sub("^0", "", format(round(avg, 3), nsmall = 3)))
}

slg <- function(pitches) {
  at_bats <- c('Out','Single','Double','Triple','HomeRun','FieldersChoice','Error')
  
  abs <- 0
  s <- 0
  d <- 0
  t <- 0
  hr <- 0
  
  for (p in 1:nrow(pitches)){
    
    if (pitches[p, 'PlayResult'] %in% at_bats){
      abs <- abs + 1
    }
    if (pitches[p, 'PlayResult'] %in% c('StolenBase','CaughtStealing') & pitches[p, 'KorBB'] == 'Strikeout'){
      abs <- abs + 1
    }
    
    if (pitches[p, 'PlayResult'] == 'Single') {
      s <- s + 1
    } else if (pitches[p, 'PlayResult'] == 'Double') {
      d <- d + 1
    } else if (pitches[p, 'PlayResult'] == 'Triple') {
      t <- t + 1
    } else if (pitches[p, 'PlayResult'] == 'HomeRun') {
      hr <- hr + 1
    }
  }
  
  slug <- (s + 2*d + 3*t + 4*hr)/abs
  return (sub("^0", "", format(round(slug, 3), nsmall = 3)))
}

gb_p <- function(pitches) {
  play <- pitches %>%
    filter(PitchCall == "InPlay")
  gbs <- play %>%
    filter(TaggedHitType == "GroundBall")
  return (round((nrow(gbs)/nrow(play))*100, digits = 2))
}

ld_p <- function(pitches) {
  play <- pitches %>%
    filter(PitchCall == "InPlay")
  lds <- play %>%
    filter(TaggedHitType == "LineDrive")
  return (round((nrow(lds)/nrow(play))*100, digits = 2))
}

fb_p <- function(pitches) {
  play <- pitches %>%
    filter(PitchCall == "InPlay")
  fbs <- play %>%
    filter(TaggedHitType %in% c("FlyBall","Flyball","Popup"))
  return (round((nrow(fbs)/nrow(play))*100, digits = 2))
}
