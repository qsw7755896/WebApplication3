using System;
using System.Collections.Generic;
using System.Linq;
using System.Threading.Tasks;

namespace WebApplication3.Models
{
    public class GoodStocks
    {
        public string Id { get; set; }
        public string Name { get; set; }
        public List<StockData> stock { get; set;}
    }
}
