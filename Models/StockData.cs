﻿using System;
using System.Collections.Generic;
using System.Linq;
using System.Threading.Tasks;

namespace WebApplication3.Models
{
    public class StockData
    {
        public string year { get; set; }

        public double EPS { get; set; } //每股盈餘

        public double Dividend { get; set; } //現金股利

        public double NetCashFlow { get; set; } //淨現金流

        public double NetIncome { get; set; } //淨收入

        public double ShareholderEQU { get; set; } //股東權益

        public double Interest { get; set; }  //利息

        public Boolean exist { get; set; } //當年度有沒有資料
    }
}