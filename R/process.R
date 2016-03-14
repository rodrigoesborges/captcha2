#' Limpar imagem
#' 
#' @rdname limpar
#' 
#' @param img imagem em formato de data.frame com as coluans x, y, r,g, e b
#' @param n raio da vizinhança utilizado. 1 utiliza apenas as células adjacentes.
#' @param k número de pontos escuros necessários no entorno, para que este seja mantido.
#' @param x x máximo
#' @param y y máximo
#' @param lim tom limite de cor [0,1]. quanto mais proximo de 0, mais escuro. Ou seja 
#' quanto menor o valor dessa variável, mais fácil de desconsiderar aquele ponto.
#' Como o default é 0, significa que apenas são considerados os pontos que são totalmente pretos.
#'
#' @export
limpar <- function(img, n = 1, k = 6, x = 170, y = 30, lim = 0){
  
  fk <- function(x) {sum(x == 0)}
  mk <- matrix(1, 1 + 2 * n, 1 + 2 * n)
  
  arrumado <- preencher(img, x, y, lim)
  
  for(j in k) {
    m_inicial <- arrumado %>% converter_em_matriz()
    m <- m_inicial %>%
      raster::raster() %>%
      raster::focal(mk, fk, pad = TRUE, padValue = 1) %>%
      raster::as.matrix()
    m <- ifelse(m >= j & m_inicial == 0, 0, 1)
    arrumado <- converter_em_df(m)
  }
  
  arrumado %>% dplyr::filter(r == 0)
}

#' Cortar
#' 
#' Observando grande partes dos captchas vimos que a imagem está compreendida 
#' no intervalo: x >= 10, x <= 180 e y >= 12, y <= 42
#' 
#' @param img imagem em formato de data.frame com as coluans x, y, r,g, e b
#' @param x_min 10
#' @param x_max 180
#' @param y_min 12
#' @param y_max 42
#'
#' @export
cortar <- function(img, x_min = 10, x_max = 180, y_min = 12, y_max = 42){
  img %>%
    dplyr::filter(y >= y_min, y <= y_max, x >= x_min, x <= x_max) %>%
    dplyr::mutate(y = y - y_min, x = x - x_min)
}
