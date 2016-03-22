#' Download captcha
#' 
#' @param dest destino do arquivo que vc deseja salvar o captcha
#' @export
#' 
download <- function(dest = NULL) {
  base <- 'http://www.receita.fazenda.gov.br/pessoajuridica/cnpj/cnpjreva'
  u <- paste0(base, '/cnpjreva_solicitacao2.asp')
  captcha <- xml2::read_html(u) %>%
    rvest::html_nodes("#imgCaptcha") %>%
    rvest::html_attr("src")
  captcha <- gsub('^\\.', '', captcha)
  if(is.null(dest)) dest <- tempfile()
  # download.file(url = paste0(base, captcha), destfile = dest, mode = "wb")
  httr::GET(paste0(base, captcha), httr::write_disk(dest, overwrite = T))
  return(dest)
}

#' Ler Captcha
#'
#' copiada do captchasaj/ só mudei que o arquivo é JPEG
#' agora é PNG denovo :P
#' 
#' @export
#' 
ler <- function(a) {
  img <- png::readPNG(a)
  img_dim <- dim(img)
  img_df <- data.frame(
    x = rep(1:img_dim[2], each = img_dim[1]),
    y = rep(img_dim[1]:1, img_dim[2]),
    r = as.vector(img[,,1]),
    g = as.vector(img[,,2]),
    b = as.vector(img[,,3])
  )
  d <- dplyr::mutate(img_df, cor = rgb(r, g, b), id = 1:n())
  d <- dplyr::filter(d, cor != '#FFFFFF')
  d
}

#' Desenhar Capctha
#'
#' copiada do captchasaj
#' 
#' @export
#' 
desenhar <- function(d){
  d <- dplyr::mutate(d, cor = rgb(r, r, r), id = 1:n())
  p <- ggplot2::ggplot(d, ggplot2::aes(x = x, y = y))
  p <- p + 
    ggplot2::coord_equal() + 
    ggplot2::theme_bw()
  if(!is.null(d$posicao)) {
    p <- p + ggplot2::geom_point(shape = 15)
    p <- p + ggplot2::facet_wrap(~posicao, scales = 'free_x', ncol = 6)
  } else {
    p <- p + ggplot2::geom_point(colour = d$cor, shape = 15, size = 3)
  }
  p +
    ggplot2::scale_x_continuous(breaks = 0:1000 * 3) +
    ggplot2::scale_y_continuous(breaks = 0:100 * 5)
}

#' Converter
#'
#' Converter de data.frame p/ matriz e de matriz p/ data.frame.
#'
#' @rdname converter
#' @param img imagem em formato de data.frame com as coluans x, y, r,g, e b
#' 
#' @export
converter_em_matriz <- function(img){
  img %>% 
    dplyr::select(x, y, r) %>% 
    tidyr::spread(x, r, fill = 1) %>% 
    dplyr::select(-y) %>% 
    as.matrix()
}

#' Converter em data.frame
#' 
#' @rdname converter
#' @export
converter_em_df <- function(m) {
  as.data.frame(m) %>% 
    dplyr::mutate(y = as.numeric(1:nrow(.))) %>%
    tidyr::gather(x, r, -y) %>%
    dplyr::mutate(x = tidyr::extract_numeric(x)) 
}

#' Preencher
#' 
#' @rdname limpar
#' 
#' @export
preencher <- function(img, x, y, lim, x_min = 0, y_min = 0){
  img_completa = expand.grid(x = x_min:x, y = y_min:y)
  dplyr::left_join(img_completa, img, c('x', 'y')) %>% 
    dplyr::mutate(
      r = ifelse(is.na(r), 1, r), 
      r = ifelse(r > lim, 1, 0)
      )
}

#' Redimensionar
#'
#' http://stackoverflow.com/questions/10865489/scaling-an-r-image
#'
#' @rdname redimensionar
#'
#' @param img imagem
#' @param w.out largura final
#' @param h.out altura final
#'
#' @export
redimensionar <- function(img, w.out, h.out) {
  im <- converter_em_matriz(img)
  w.in = nrow(im)
  h.in = ncol(im)
  # Create empty matrix
  im.out = matrix(rep(0, w.out * h.out), nrow = w.out, ncol = h.out )
  # Compute ratios -- final number of indices is n.out, spaced over range of 1:n.in
  w_ratio = w.in / w.out
  h_ratio = h.in / h.out
  # Do resizing -- select appropriate indices
  im.out <- im[ floor(w_ratio * 1:w.out), floor(h_ratio * 1:h.out)]
  rownames(im.out) <- 1:nrow(im.out)
  colnames(im.out) <- 1:ncol(im.out)
  d_out <- converter_em_df(im.out) %>% dplyr::filter(r < 1)
  return(d_out)
}

#' Redimensionar por grupo
#' 
#' @rdname redimensionar
#' @export
redimensionar_por_posicao <- function(img, w.out = 20, h.out = 20){
  plyr::ldply(1:6, function(p, img){
    img %>%
      dplyr::filter(posicao == p) %>%
      redimensionar(w.out, h.out) %>%
      dplyr::mutate(posicao = p)
  }, img = img)
}

#' Imagem em bd
#' 
#' Transformar uma imagem em BD
#' Ele faz isso p/ cada posição sendo assim do jeito correto
#' 
#' 
imagem_em_bd <- function(img){
  a <- img %>%
    dplyr::mutate(
      col = sprintf("x%02dy%02d",x, y) 
        ) %>%
    dplyr::select(-x, -y) %>% 
    tidyr::spread(col, r, fill = 1)
}

#' Pegar nome pelo arquivo
#'
#' @param arq nome do arquivo
#'
pegar_nome <- function(arq){
  stringr::str_sub(arq, 1, 6)
}

#' Acrescentar letra
#'
#' @param img imagem
#' @param letras letras
#'
acrescentar_letra <- function(img, letras){
  let <- dplyr::data_frame(
    posicao = 1:6,
    letras = letras
  )
  dplyr::left_join(img, let, by = "posicao")
}

#' Tranformar NA em 1
#'
#' @param x vetor
#'
na_1 <- function(x){
  ifelse(is.na(x), 1, x)
}

#' Pipe operator
#'
#' See \code{\link[magrittr]{\%>\%}} for more details.
#'
#' @name %>%
#' @rdname pipe
#' @keywords internal
#' @export
#' @importFrom magrittr %>%
#' @usage lhs \%>\% rhs
NULL
