#' Hypotekarna kalkulacka
#'
#' Vyrata vysku mesacnej splatky podla urokovej miery, dlzky splacania a pozicanej sumy
#' @param r Vyska urokovej miery v percentach
#' @param n Dlzka splacania v rokoch
#' @param H Pozicana suma
#' @return Vyska mesacnej splatky
#' @examples 
#' splatka1 <- hypotekarna_kalkulacka(5, 30, 100000);
#' splatka2 <- hypotekarna_kalkulacka(4.5, 20 ,75000);
#' @export
hypotekarna_kalkulacka <- function(urok, dlzka, hodnota) {
  r <- urok / 100
  n <- dlzka
  
  an <- ((1 + r)^n - 1) / r * (1+r)^-n
  splatka <- hodnota / an / 12
  
  return(splatka)
}

#' Dochodkova kalkulacka
#'
#' Vypocita, kolko si musi clovek rocne odlozit, podla toho, kolko chce poberat rocny dochodok
#' @param m O kolko rokov chce ist do dochodku
#' @param n Kolko rokov chce poberat dochodok
#' @param r1 Urokova miera po zaciatok vyberania dochodku v percentach
#' @param r2 Urokova miera po zacati vyberiania dochodku v percentach
#' @param X Kolko eur chce poberat rocne dochodok
#' @return Kolko si treba rocne odlozit
#' @examples 
#' dochodok <- dochodkova_kalkulacka(40,20,5,5,12000);
#' @export
dochodkova_kalkulacka <- function(m,n,r1,r2,X) {
  
  r1 <-r1/100
  r2<- r2/100
  
  lava_s <- ((1 + r1)^m - 1) / r1
  prava_s <- X * ((1 + r2)^n - 1) / r2 * (1 + r2)^ -n
  
  vysledok<-solve(lava_s,prava_s)
  
  return(vysledok)
}