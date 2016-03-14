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
  download.file(url = paste0(base, captcha), destfile = dest)
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
  if(!is.null(d$grupo)) {
    p <- p + ggplot2::geom_point(shape = 15)
    p <- p + ggplot2::facet_wrap(~grupo, scales = 'free_x', ncol = 6)
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
preencher <- function(img, x, y, lim){
  img_completa = expand.grid(x = 0:x, y = 0:y)
  dplyr::left_join(img_completa, img, c('x', 'y')) %>% 
    dplyr::mutate(
      r = ifelse(is.na(r), 1, r), 
      r = ifelse(r > lim, 1, 0)
      )
}

