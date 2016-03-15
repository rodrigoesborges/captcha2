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
#' @note se você estiver usando a função \code{limpar_por_posicao}, é importante
#' que o data.frame contenha uma coluna \code{posicao}
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

#' Picotar
#' 
#' Essa função serve p/ picotar a imagem nas 6 letras.
#' Ela recebe 5 pontos de corte p/ essa separação. 
#'
#' @param img imagem em formato de data.frame com as coluans x, y, r,g, e b
#' @param cortes vetor numérico de tamanho 5, com os pontos de corte
#' 
#' @export
picotar <- function(img, cortes = c(25, 55, 85, 120, 147)){
  img %>%
    dplyr::mutate(
      posicao = 6,
      posicao = ifelse(x <= cortes[5], 5, posicao),
      posicao = ifelse(x <= cortes[4], 4, posicao),
      posicao = ifelse(x <= cortes[3], 3, posicao),
      posicao = ifelse(x <= cortes[2], 2, posicao),
      posicao = ifelse(x <= cortes[1], 1, posicao)
    ) %>%
    dplyr::group_by(posicao) %>%
    dplyr::mutate(
      x = x - min(x),
      y = y - min(y)
    ) %>%
    dplyr::ungroup()
}

#' Limpar por posição
#' 
#' @rdname limpar
#'
#' @export
limpar_por_posicao <- function(img, n = 1, k = 6){
  
  pos <- img %>%
    dplyr::group_by(posicao) %>%
    dplyr::summarise(
      x = max(x),
      y = max(y)
    )
  
  d <- plyr::adply(pos, .margin = 1, function(pos, img, n , k){
    img %>%
      dplyr::filter(posicao == pos$posicao) %>%
      limpar(n = n, k = k, x = pos$x, y = pos$y)
  }, img = img, n = n, k = k)
  
  return(d)
}

#' Alinhar
#'
#' @param img imagem em formato de data.frame com as coluans x, y, r,g, e b
#'
#' @export
alinhar <- function(img){
  img %>%
    dplyr::mutate(
      x = x - min(x),
      y = y - min(y)
      )
}

#' Alinhar por posicao
#' 
#' @param img imagem com uma coluna indicando a posicao do ponto.
#' 
#' @export
alinhar_por_posicao <- function(img){
  plyr::ldply(1:6, function(p, img){
    img %>%
      dplyr::filter(posicao == p) %>%
      alinhar()
  }, img = img)
}

#' Processar Imagem
#'
#' @param img imagem em formato de data.frame com as colunas x, y, r,g, e b
#' @param cortes passado p/ a função \code{picotar}
#' 
#'
#' @export
processar <- function(img, cortes = c(25, 55, 85, 120, 147)){
  trat <- img %>%
    cortar() %>% 
    limpar() %>% 
    picotar(cortes) %>% 
    limpar_por_posicao() %>%
    alinhar_por_posicao() %>%
    redimensionar_por_posicao() %>%
    imagem_em_bd()
}

#' Arrumar BD 
#'
#' @param dir diretorio com todos os captchas já processados em data.frames
#'
#' @export
arrumar <- function(dir, cortes = c(25, 55, 85, 120, 147)){
  arqs <- list.files(dir)
  
  nomes <- dplyr::data_frame(
    arqs = arqs,
    letras = pegar_nome(arqs) %>%
      stringr::str_split("")
  )
  
  result <- plyr::adply(nomes, .margin = 1, function(n, dir){
    a <- readRDS(paste0(dir, n$arqs))
    processar(a, cortes = cortes) %>%
      acrescentar_letra(n$letras %>% unlist)
  }, dir = dir, .progress = "text") %>%
    dplyr::mutate_each(
      funs(na_1), 
      starts_with("x"))
}


