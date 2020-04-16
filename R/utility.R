#' 处理问题泛化业务
#'
#' @param faq 问题
#'
#' @return 返回值
#' @export
#'
#' @examples
#' generator()
generator <- function(faq){

  #faq <- read_excel(file,skip = skip);
  # View(faq)
  #提取标准问题------
  ques <- faq$标准问
  ques_count <- length(ques);
  #提取模板------
  busi_Obj <- faq$`业务对象(近义词）`;
  oper_Node <- faq$`操作节点（近义词）`;
  #提取泛化内容
  busi_Obj_gen<- faq$`业务对象(同义词）`;
  oper_Node_gen <- faq$`操作节点（同义词）`;
  #数据处理--------
  busi_Obj_gen_multi <- str_split(busi_Obj_gen,'~');
  oper_Node_gen_multi <-str_split(oper_Node_gen,'~');
  #处理情况；
  #完善一下逻辑
  res <-lapply(1:ques_count,function(i){
    q1 <- ques[i];
    #标准的业务对象
    busi_obj1 <- busi_Obj[i];
    #标准的操作节点
    oper_node1 <- oper_Node[i];
    #业务对象同义词
    busi_obj_set1 <-busi_Obj_gen_multi[[i]];
    #操作节点同义词
    oper_node_set1 <-oper_Node_gen_multi[[i]]

    alpha <- length(busi_obj_set1) #业务对象数

    belta <- length(oper_node_set1) #操作结点数；

    res_count <-alpha*belta;
    unit_res <-list_init(res_count);
    pcs <-1L #计数


    if(alpha==1 & is.na(busi_obj_set1)&belta >=1&!is.na(oper_node_set1)){
      #缺少业务对象，泛化操作节点
      for (oper_unit in oper_node_set1) {


        q001_belta <- str_replace(q1,oper_node1,oper_unit);
        #q001_belta;
        unit_res[[pcs]] <-q001_belta;
        pcs<- pcs+1L;



      }


    }else if (alpha >=1 &!is.na(busi_obj_set1)& belta ==1 & is.na(oper_node_set1)){
      #泛化业务对象,缺少操作节点
      for (busi_unit in busi_obj_set1) {


        q001_alpha <- str_replace(q1,busi_obj1,busi_unit);
        #q001_alpha;

        unit_res[[pcs]] <-q001_alpha;
        pcs<- pcs+1L;

      }

    }else if ((alpha >1 & belta >1)|(alpha ==1 & !is.na(busi_obj_set1))|(belta==1 & !is.na(oper_node_set1))){
      #处理说明
      for (busi_unit in busi_obj_set1) {
        for (oper_unit in oper_node_set1) {

          q001_alpha <- str_replace(q1,busi_obj1,busi_unit);
          #q001_alpha;
          q001_belta <- str_replace(q001_alpha,oper_node1,oper_unit);
          #q001_belta;
          unit_res[[pcs]] <-q001_belta;
          pcs<- pcs+1L;



        }

      }


    }else{
      unit_res[[pcs]] <-"";

    }



    #进行标准化处理,取消list
    unit_res <- unique(unlist(unit_res))
    unit_res <- paste(unit_res,collapse = "||")
    return(unit_res)
  })

  res <- unlist(res);
  print(res)
  faq2 <- faq[,1:14];
  faq2$`相似问题` <- res
  return(faq2)

}
