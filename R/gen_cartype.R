#' 处理相当问题
#'
#' @param file 文件名
#' @param sheet_name   页签
#' @param new_carType   新车型
#'
#' @return 返回值
#' @export
#'
#' @examples
#' nsgen_cartype()
nsgen_cartype <- function(file="data-raw/捷豹路虎品牌车型知识点模板V2.2_发现运动_需泛化相似问.xlsx",
                          sheet_name='发运-固定内容',
                          new_carType='极光揽胜'){

  faq <-read_excel(file,
                   sheet = sheet_name)
  faq$标准问 <-stringr::str_replace(faq$标准问,faq$车型,new_carType)
  faq$车型 <- new_carType
  res <- generator(faq)

  return(res)

}





