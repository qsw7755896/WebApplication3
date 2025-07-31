# 使用 GitHub Copilot + MCP 控制 Mainframe COBOL 源碼管理方案

## 一、註冊 MCP 服務
1. **GetCobolSource**  
   - 功能：取得指定 COBOL 程式的原始碼字串。  
   - 輸入：程式名稱(可選)  
   - 輸出：COBOL 程式碼字串。

2. **GetPdsMember**  
   - 功能：取得 PDS（Partitioned Data Set）中某個成員的內容。  
   - 輸入：資料集名稱與成員名稱。  
   - 輸出：該成員內容字串。

3. **TraversalAllDataSet**  
   - 功能：遞迴掃描並返回整個主機資料集的樹狀結構。  
   - 輸入：起始節點（可選）  
   - 輸出：資料集樹狀結構JSON。

4. **UploadCobolSource**  
   - 功能：將本地 COBOL 程式碼回傳並更新至 mainframe 資料集中。  
   - 輸入：資料集路徑、檔案名稱、檔案內容。  
   - 輸出：執行結果訊息。

## 二、關鍵策略與注意事項
- MCP 服務輸出一定要符合 MCP 協定規範，內容封裝於 `content` 陣列。
- 負責建立與維護本地版本庫的流程可由 Copilot 透過自然語言指令操控 MCP 工具完成。
- 授權控管與資料安全不可忽視，尤其是有修改或上傳功能時。
- 以 Git 或其他版本控制系統結合 MCP 與 Copilot，自動同步更新與管理。
- 分批呼叫大型資料集樹服務，注意效能與穩定。

## 三、示範 MCP Server 範例（Node.js）
const express = require(‘express’); const { McpServer } = require(’@modelcontextprotocol/sdk/server/mcp.js’); const { StreamableHTTPServerTransport } = require(’@modelcontextprotocol/sdk/server/streamableHttp.js’); const { z } = require(‘zod’);
const app = express(); const port = 3000; app.use(express.json());
const mcpServer = new McpServer({ name: “Mainframe COBOL MCP Server”, version: “1.0.0”, instructions: “提供 COBOL 源碼管理相關服務” });
// 1. GetCobolSource mcpServer.registerTool( “getCobolSource”, { description: “取得指定 COBOL 程式原始碼”, inputSchema: z.object({ programName: z.string() }) }, async (input) => { const sourceCode =  IDENTIFICATION DIVISION.\nPROGRAM-ID. ${input.programName}.\nPROCEDURE DIVISION.\n    DISPLAY 'This is a sample COBOL program.'\n    STOP RUN.\n ; return { content: { type: ‘text’, text: sourceCode } }; } );
// 2. GetPdsMember mcpServer.registerTool( “getPdsMember”, { description: “取得 PDS 某成員內容”, inputSchema: z.object({ dataset: z.string(), member: z.string() }) }, async (input) => { // 範例回傳固定字串 const memberContent =  * Member ${input.member} content from dataset ${input.dataset} ; return { content: { type: ‘text’, text: memberContent } }; } );
// 3. TraversalAllDataSet mcpServer.registerTool( “traversalAllDataSet”, { description: “取得主機資料集樹狀結構 JSON”, inputSchema: z.object({ root: z.string().optional() }) }, async (input) => { // 模擬資料集樹 JSON const tree = { name: input.root ?? “root”, children: [ { name: “DATASET1”, children: { name: “MEMBER1” }, { name: “MEMBER2” } }, { name: “DATASET2”, children: [] } ] }; return { content: { type: ‘json’, json: tree } }; } );
// 4. UploadCobolSource mcpServer.registerTool( “uploadCobolSource”, { description: “將本地 COBOL 程式碼上傳至主機”, inputSchema: z.object({ dataset: z.string(), member: z.string(), content: z.string() }) }, async (input) => { // 這裡可連接主機API或模擬寫入 console.log( Uploading to ${input.dataset}(${input.member}):\n${input.content} ); return { content: { type: ‘text’, text: “Upload successful” } }; } );
// MCP HTTP 路由 app.post(’/api/mcp’, async (req, res) => { const transport = new StreamableHTTPServerTransport(); res.on(‘close’, () => { transport.close(); mcpServer.close(); }); await mcpServer.connect(transport); await transport.handleRequest(req, res, req.body); });
app.listen(port, () => { console.log( MCP Server running at http://localhost:${port}/api/mcp ); });


---

透過上述設計，你的 MCP Server 功能全面涵蓋了主機 COBOL 原始碼拉取、資料集與成員巡覽以及程式碼上傳，搭配 GitHub Copilot 的 Agent 模式與自然語言指令，即可組建自動化的 COBOL 源碼管理與版本控制系統。

如需進一步幫助（如 Copilot 端 prompt 範例、部署 Azure 步驟等），也可以隨時告訴我。
