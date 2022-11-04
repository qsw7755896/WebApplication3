using AngleSharp.Html.Parser;
using Microsoft.AspNetCore.Mvc;
using Microsoft.Extensions.Logging;
using System;
using System.Collections.Generic;
using System.Diagnostics;
using System.Linq;
using System.Net.Http;
using System.Threading.Tasks;
using WebApplication3.Models;

namespace WebApplication2.Controllers
{
    public class HomeController : Controller
    {
        private readonly ILogger<HomeController> _logger;
        GoodStocks GoodStock = new GoodStocks();


        public HomeController(ILogger<HomeController> logger)
        {
            _logger = logger;
        }

        public IActionResult Index()
        {
            return View();
        }

        public IActionResult Privacy()
        {
            return View();
        }

        [ResponseCache(Duration = 0, Location = ResponseCacheLocation.None, NoStore = true)]
        public IActionResult Error()
        {
            return View(new ErrorViewModel { RequestId = Activity.Current?.Id ?? HttpContext.TraceIdentifier });
        }

        /**
         * Subject: 各項指標詳情
         * Route: GET /Home/detail
         * Description: 顯示該股票指定指標的數值
         */
        public IActionResult Detail()
        {
            return View();
        }

        /**
         * Subject: 找尋欲查詢的股票財報
         * Route: GET /Home/queryStock/2330
         * Description: 同步爬蟲公開觀測平台指定資料
         */
        public async Task<ActionResult> queryStock(string id)
        {
            string stockId = id;
            int year = System.DateTime.Today.Year - 1911 - 1;  // 民國年
            //double month = System.DateTime.Today.Month;
            //int season = Convert.ToInt16(Math.Ceiling(month / 3) - 2);  // 先取當季的前兩季
            int season = 4;
            GoodStock.stock = new List<StockData>();

            for (int i = 0; i < 10; i++)
            {
                if (year == 101)
                    break;

                StockData queryStock = new StockData();

                queryStock.year = year.ToString();
                GoodStock.Id = id;

                // 抓取EPS、淨收入：綜合損益表[基本每股盈餘、本期淨利（淨損）]
                await getDomWithPost("https://mops.twse.com.tw/mops/web/ajax_t164sb04", stockId, year, season, "1", queryStock);

                // 抓取股東權益：資產負債表[歸屬於母公司業主之權益合計]
                await getDomWithPost("https://mops.twse.com.tw/mops/web/ajax_t164sb03", stockId, year, season, "2", queryStock);

                // 抓取現金流：現金流量表[營業活動之淨現金流入（流出） + 投資活動之淨現金流入（流出）+ 籌資活動之淨現金流入（流出）]、[利息費用]
                await getDomWithPost("https://mops.twse.com.tw/mops/web/ajax_t164sb05", stockId, year, season, "3", queryStock);

                // 抓取現金股利
                await getDomWithPost("https://mops.twse.com.tw/mops/web/ajax_t05st09_2", stockId, year, season, "4", queryStock);

                // 新增至model中
                GoodStock.stock.Add(queryStock);

                year--;
                //season--;
            }
            object data = new { resultdt = GoodStock.stock };
            //return View();
            return Json(data);
        }

        /**
         * Subject: 分析dom表
         * Route: NaN
         * Description: 使用Angelsharp解析
         */
        private async Task getDomWithPost(string urlIN, string stockIN, int yearIN, int seasonIN, string dataFlag, StockData queryStock)
        {
            string targetUrl = urlIN;
            string result;
            string var_year = yearIN.ToString();
            String var_season = seasonIN.ToString();
            queryStock.exist = true; // default true

            using (var client = new HttpClient())
            {
                FormUrlEncodedContent data;
                if (dataFlag == "4")
                {
                    data = new FormUrlEncodedContent(new Dictionary<string, string>
                    {
                        ["encodeURIComponent"] = "1",
                        ["step"] = "1",
                        ["firstin"] = "1",
                        ["off"] = "1",
                        ["keyword4"] = "",
                        ["code1"] = "",
                        ["TYPEK2"] = "",
                        ["checkbtn"] = "",
                        ["queryName"] = "co_id",
                        ["inpuType"] = "co_id",
                        ["TYPEK"] = "all",
                        ["isnew"] = "false",
                        ["co_id"] = stockIN,
                        ["date1"] = var_year,
                        ["date2"] = var_year,
                        ["qryType"] = "2"
                    });
                }
                else
                {
                    data = new FormUrlEncodedContent(new Dictionary<string, string>
                    {
                        ["encodeURIComponent"] = "1",
                        ["step"] = "1",
                        ["firstin"] = "1",
                        ["off"] = "1",
                        ["keyword4"] = "",
                        ["code1"] = "",
                        ["TYPEK2"] = "",
                        ["checkbtn"] = "",
                        ["queryName"] = "co_id",
                        ["inpuType"] = "co_id",
                        ["TYPEK"] = "all",
                        ["isnew"] = "false",
                        ["co_id"] = stockIN,
                        ["year"] = var_year,
                        ["season"] = var_season
                    });
                }


                var response = await client.PostAsync(targetUrl, data);
                //如果失敗會拋出錯誤
                response.EnsureSuccessStatusCode();
                //取得結果
                var html = await response.Content.ReadAsStringAsync();

                var parser = new HtmlParser();
                var document = parser.ParseDocument(html);

                switch (dataFlag)
                {
                    case "1":
                        var list = document.QuerySelectorAll(".hasBorder tr").Skip(10 - 1);
                        if (list.Count() == 0)
                            queryStock.exist = false;
                        foreach (var item in list)
                        {
                            if (item.TextContent.IndexOf("本期淨利（淨損）") == -1)
                            {
                                continue;
                            }
                            else
                            {
                                result = item.QuerySelectorAll("td").Skip(2 - 1).FirstOrDefault().TextContent;
                                queryStock.NetIncome = Convert.ToDouble(result);
                                break;
                            }
                        }

                        list = document.QuerySelectorAll(".hasBorder tr").Skip(40 - 1);
                        foreach (var item in list)
                        {
                            if (item.TextContent.IndexOf("　基本每股盈餘") == -1)
                            {
                                continue;
                            }
                            else
                            {
                                result = item.QuerySelectorAll("td").Skip(2 - 1).FirstOrDefault().TextContent;
                                queryStock.EPS = Convert.ToDouble(result);
                                break;
                            }
                        }

                        break;

                    case "2":
                        list = document.QuerySelectorAll(".hasBorder tr").Skip(64 - 1);
                        if (list.Count() == 0)
                            queryStock.exist = false;
                        foreach (var item in list)
                        {
                            if (item.TextContent.IndexOf("歸屬於母公司業主之權益合計") == -1)
                            {
                                continue;
                            }
                            else
                            {
                                result = item.QuerySelectorAll("td").Skip(2 - 1).FirstOrDefault().TextContent;
                                queryStock.ShareholderEQU = Convert.ToDouble(result);
                                break;
                            }
                        }
                        break;

                    case "3":
                        string num1 = "", num2 = "", num3 = "", num_Interest = "";
                        list = document.QuerySelectorAll(".hasBorder tr").Skip(10 - 1);
                        if (list.Count() == 0)
                            queryStock.exist = false;
                        foreach (var item in list)
                        {
                            if (num1 != "" && num2 != "" && num3 != "" && num_Interest != "")
                            {
                                queryStock.NetCashFlow = Convert.ToDouble(num1) + Convert.ToDouble(num2) + Convert.ToDouble(num3);
                                queryStock.Interest = Convert.ToDouble(num_Interest);
                                break;
                            }
                            if (item.TextContent.IndexOf("利息費用") != -1)
                                num_Interest = item.QuerySelectorAll("td").Skip(2 - 1).FirstOrDefault().TextContent;
                            if (item.TextContent.IndexOf("營業活動之淨現金流入（流出）") != -1)
                                num1 = item.QuerySelectorAll("td").Skip(2 - 1).FirstOrDefault().TextContent;
                            if (item.TextContent.IndexOf("投資活動之淨現金流入（流出）") != -1)
                                num2 = item.QuerySelectorAll("td").Skip(2 - 1).FirstOrDefault().TextContent;
                            if (item.TextContent.IndexOf("籌資活動之淨現金流入（流出）") != -1)
                                num3 = item.QuerySelectorAll("td").Skip(2 - 1).FirstOrDefault().TextContent;
                        }
                        break;

                    case "4":
                        list = document.QuerySelectorAll(".hasBorder tr").Skip(4 - 1);
                        if (list.Count() == 0)
                            queryStock.exist = false;
                        foreach (var item in list)
                        {
                            result = item.QuerySelectorAll("td").Skip(11 - 1).FirstOrDefault().TextContent;
                            if (result.Length > 4)
                                result = result.Substring(0, result.Length - 4);
                            else
                                result = result.Substring(0, result.Length - 1);
                            queryStock.Dividend = queryStock.Dividend + Convert.ToDouble(result);
                        }
                        break;

                    default:
                        break;
                }
            }

        }
    }
}
