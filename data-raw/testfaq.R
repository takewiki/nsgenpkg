bbc <-nsgen(file='data-raw/JBLH_upload_V1.8 -a.xlsx');

openxlsx::write.xlsx(bbc,'data-raw/faq_112.xlsx')


bbc <-nsgen(file='data-raw/JBLH_upload_V1.8 -b.xlsx');

openxlsx::write.xlsx(bbc,'data-raw/faxian.xlsx')


bbc <-nsgen(file='data-raw/JBLH_upload_V1.8 -c.xlsx');

openxlsx::write.xlsx(bbc,'data-raw/faxian2.xlsx')

