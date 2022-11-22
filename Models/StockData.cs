using System;
using System.Collections.Generic;
using System.ComponentModel;
using System.Linq;
using System.Threading.Tasks;

namespace WebApplication3.Models
{
    public class StockData
    {
        [DisplayName("年度")]
        public string year { get; set; }

        [DisplayName("營收")]
        public double Revenue { get; set; } // 營收

        [DisplayName("每股盈餘")]
        public double EPS { get; set; } //每股盈餘

        [DisplayName("現金股利")]
        public double Dividend { get; set; } //現金股利

        [DisplayName("淨自由現金流")]
        public double NetCashFlow { get; set; } //淨自由現金流

        [DisplayName("營業現金流")]
        public double OperatingCF { get; set; } //營業現金流

        [DisplayName("存貨")]
        public double Inventory { get; set; } //存貨

        [DisplayName("投資現金流")]
        public double InvestingCF { get; set; } //投資現金流

        [DisplayName("資本支出")]
        public double Capex { get; set; } //資本支出

        [DisplayName("融資現金流")]
        public double FinancingCF { get; set; } //融資現金流

        [DisplayName("淨收入")]
        public double NetIncome { get; set; } //淨收入

        [DisplayName("每股股東權益盈餘")]
        public double ShareholderEQU { get; set; } //股東權益

        [DisplayName("利息")]
        public double Interest { get; set; }  //利息

        [DisplayName("流動負債")]
        public double Liabilities { get; set; } //流動負債

        [DisplayName("當年度有沒有資料")]
        public Boolean exist { get; set; } //當年度有沒有資料
    }
}
