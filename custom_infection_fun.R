# Encoding: UTF-8 (Reopen with Encoding)
# Mit source() können Sie die Datei einbinden
##########################################################################
#
## param.icm
#
# In param.icm müssen die folgenden zeitlich abhängigen Parameter setzen:
#
# act.rate.new:         Neue Begegnungsrate 
# act.rate.new.start:   Start Kontaktbeschränkungen
# act.rate.new.end:     Ende Kontaktbeschränkungen
# inf.prob.new:         Neue Ansteckungswahrscheinlichkeit
# inf.prob.new.start:   Start Hygienemaßnahmen
# inf.prob.new.end:     Ende Hygienemaßnahmen
#
##########################################################################
#
## control.icm
#
# In control.icm müssen Sie Funktion infecton.icm_custom 
# an den Parameter infection.FUN
#
##########################################################################

infection.icm_custom <- function(dat, at) 
{
  suppressMessages(require(EpiModel))
  x <- dat$param$act.rate
  if (!is.null(dat$param$act.rate.new) &&
      at >= dat$param$act.rate.new.start &&
      at < dat$param$act.rate.new.end) 
  {
    x <- dat$param$act.rate.new
  }
  acts <- round(x * dat$epi$num[at - 1] / 2)
  p1 <- ssample(which(dat$attr$active == 1), acts, replace = TRUE)
  p2 <- ssample(which(dat$attr$active == 1), acts, replace = TRUE)
  # 15
  del <- NULL
  if (length(p1) > 0 & length(p2) > 0) 
  {
    del <- data.frame(p1, p2)
    while (any(del$p1 == del$p2)) 
    {
      del$p2 <- ifelse(del$p1 == del$p2,
                       ssample(which(dat$attr$active == 1), 1),
                       del$p2)
    }
    del$p1.stat <- dat$attr$status[del$p1]
    del$p2.stat <- dat$attr$status[del$p2]
    serodis <- (del$p1.stat == "s" & del$p2.stat == "i") |
      (del$p1.stat == "i" & del$p2.stat == "s")
    del <- del[serodis == TRUE,]
    if (nrow(del) > 0) {
      del$tprob <- dat$param$inf.prob
      if (!is.null(dat$param$inf.prob.new) && 
          at >= dat$param$inf.prob.new.start && 
          at < dat$param$inf.prob.new.end) 
      {
        del$tprob <- dat$param$inf.prob.new
      }
      if (!is.null(dat$param$inter.eff) &&
          at >= dat$param$inter.start) 
      {
        del$tprob <- del$tprob * (1 - dat$param$inter.eff)
      }
      del$trans <- rbinom(nrow(del), 1, del$tprob)
      del <- del[del$trans == TRUE, ]
      if (nrow(del) > 0) 
      {
        newIds <- unique(ifelse(del$p1.stat == "s", del$p1,
                                del$p2))
        nInf <- length(newIds)
        dat$attr$status[newIds] <- "i"
        dat$attr$infTime[newIds] <- at
      }
      else 
      {
        nInf <- 0
      }
    } 
    else 
    {
      nInf <- nInfg2 <- 0
    }
  }
  else 
  {
    nInf <- 0
  }
  if (at == 2) {
    dat$epi$si.flow <- c(0, nInf)
  }
  else 
  {
    dat$epi$si.flow[at] <- nInf
  }
  return(dat)
}