library(tidyverse)
library(stringr)
library(lubridate)  # 加载lubridate库以使用日期函数

# 载入.csv文件 ----------------------------------------------------------------
Shipping_Order_file_path   <- "D:/发货单.csv"
Customer_Payment_file_path <- "D:/客户付款.csv"

# 加载发货单数据
Shipping_Order_data <- read_csv(Shipping_Order_file_path) %>%   
  mutate(账单日期 = ymd(账单日期))

# 加载收款数据
Customer_Payment_data <- read_csv(Customer_Payment_file_path) %>%  
  mutate(日期 = ymd(日期))


# Void          # 作废
# Closed        # 已结清
# Overdue       # 逾期
# PartiallyPaid # 部分支付

# 使用安全列名（处理可能不存在的列）
代收代付_cols <- c("入籍费用", "罐检费", "GPS服务费", "营运证年审费", "承运人险", 
               "安责险", "市内通行证", "车辆延期费", "机动车修理费", "机动车年检费",
               "危险标识牌", "其他", "税金", "代缴罚款")

车险费_cols <- c("代收车船税", "交强险", "商业险")


# 按账单编号对金额(FCY)进行求和 -----------------------------------------------

total_payments <- Customer_Payment_data %>%
  group_by(账单编号) %>%
  summarise(累计收款金额 = sum(`金额(FCY)`, na.rm = TRUE)) %>%
  ungroup()  # 可以选择是否保留分组信息

# 对Shipping_Order_data进行数据整理，匹配：total_payments的累计收款数据。------

Annual_Bill <- Shipping_Order_data %>%
  # 预处理步骤
  mutate(
    账单日期 = as.Date(账单日期),  # 确保转换为日期格式
    客户名称 = as.character(客户名称)
  ) %>%
  # 过滤排除客户
  filter(!str_detect(客户名称, "123|456")) %>%
  # 列处理
  select(-any_of(c("分类", "描述", "总计", "所属类别", "开始日期", "结束日期"))) %>%
  # 数据重塑
  pivot_wider(
    names_from = 款项名称,
    values_from = 款项金额,
    values_fill = 0,
    values_fn = sum
  ) %>%
  # 类型转换（确保数值列）
  mutate(across(where(is.numeric), ~ coalesce(., 0))) %>%
  # 特征工程
  mutate(
    年 = year(账单日期),
    月 = month(账单日期),
    押金 = coalesce(合同押金, 0) + coalesce(报废押金, 0),
    代收付类 = rowSums(across(any_of(代收代付_cols)), na.rm = TRUE),  # 修正1：补全参数
    车险费 = rowSums(across(any_of(车险费_cols)), na.rm = TRUE),     # 修正2：补全参数
    小计 = coalesce(车管服务费, 0) + 押金 + 车险费 + 代收付类
  ) %>%
  # 列选择与排序
  select(
    账单编号, Invoice状态, 账单日期, 年, 月, 客户名称,
    车管服务费, 押金, 车险费, 代收付类, 小计,
    `应收款 余额`, 调整
  ) %>%
  arrange(客户名称, 账单日期)%>%   # 关联支付数据
  left_join(
    total_payments %>% select(账单编号, 累计收款金额),
    by = "账单编号"
  ) %>%
  mutate(
    累计收款金额 = coalesce(累计收款金额, 0),
    应收款金额 = 小计 + coalesce(调整, 0) - 累计收款金额
  )


# 将结果保存为 CSV 文件
write.csv(Annual_Bill, file = "D:/oilcar/Invoice_dashboard/data/Annual_Bill.csv", row.names = FALSE)



# 车险费用明细
# 车辆保险  "Vehicle Insurance"
#  交强险   "Compulsory Liability Insurance of Vehicle Traffic Accident"
#  商业险   "Vehicle Commercial Insurance"
# 统计车险单数据 -----------------------------------------------------
# Vehicle_Insurance <- Shipping_Order_data %>%
#   filter(
#     between(账单日期, start_date, end_date),
#     !str_detect(客户名称, "123|456"),
#     款项名称 %in% c("代收车船税", "交强险",  "商业险")
#   ) %>%
#   select(账单编号, Invoice状态, 账单日期, 客户名称, 款项名称, 款项金额) %>%
#   # 数据转换和计算
#   pivot_wider(
#     names_from  = 款项名称,
#     values_from = 款项金额,
#     values_fill = list(0)
#   ) %>%
#   mutate(
#     小计 = rowSums(select(., 交强险, 代收车船税, 商业险), na.rm = TRUE)
#   ) %>%
#   # 添加合计行
#   add_row(账单编号 = "合计", 小计 = sum(.$小计))


