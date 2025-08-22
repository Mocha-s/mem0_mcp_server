### MCP 服务架构模版（可复用）

本模版用于快速搭建符合 MCP 2025-06-18 规范、可扩展、可维护、可观测的服务化架构。遵循 KISS、YAGNI、DRY、SOLID 原则，以注册表驱动和策略模式实现低侵入扩展。

---

### 适用范围
- 构建基于 MCP 的工具/记忆/检索/自动化服务
- 需要统一的传输层、网关、注册中心与微服务骨架
- 支持本地/容器化/编排部署，便于未来复用与扩展

---

### 目录结构模板
```text
<project_root>/
├── run_server_http.py                 # 开发/本地入口（Streamable HTTP）
├── run_server_production.py          # 生产入口（优化日志/配置）
├── requirements.txt                  # 依赖
├── Dockerfile                        # 生产镜像
├── docker-compose.yml                # 本地编排
├── config/
│   └── server_config.json            # 运行期配置（可选）
├── src/
│   ├── transport/
│   │   └── streamable_http.py        # 传输层：HTTP+SSE，会话与CORS
│   ├── server/
│   │   └── mcp_server.py             # MCP 服务端：JSON-RPC 方法注册
│   ├── gateway/
│   │   └── tool_manager.py           # API 网关：路由、熔断、健康检查
│   ├── registry/
│   │   ├── tools.json                # 注册表：服务/策略/Schema/依赖
│   │   └── registry_manager.py       # 注册中心：动态加载与校验
│   ├── services/
│   │   ├── base/
│   │   │   └── service.py            # BaseService/Strategy 模版与输出过滤
│   │   └── <your_service>/
│   │       └── service.py            # 新增服务实现（继承 BaseService）
│   ├── client/
│   │   └── api_client.py             # 业务后端 API 客户端（可替换 Mem0）
│   ├── protocol/
│   │   └── messages.py               # MCP 消息/结果类型
│   └── strategies/                   # 共享策略库（可选）
└── docs/
    └── architecture/
        └── mcp_service_architecture_template.md  # 本模版
```

---

### 关键组件职责
- **Transport**: HTTP POST(JSON-RPC) + GET(SSE)；会话管理、CORS、健康检查。
- **Server**: MCP 方法注册与协议协商；将工具调用转发到网关。
- **Gateway**: 根据注册表进行发现、参数校验、依赖校验、熔断/重试、调用服务实例，统一产出结果。
- **Registry**: `tools.json` 注册服务元数据、输入/输出 Schema、策略、依赖；支持动态加载 `module:Class`。
- **Services**: 以 `BaseService` + `BaseStrategy` 实现业务；策略模式支撑差异化执行路径。
- **Client**: 封装对后端实际 API 的访问（可换为任意业务后端）。
- **Protocol**: MCP 工具结果与错误类型，保证返回结构对齐。

---

### 生命周期（请求数据流）
1) 客户端 → POST `/mcp` 发送 JSON-RPC `initialize`/`tools/list`/`tools/call`
2) Transport 校验协议/来源 → 解析 JSON-RPC → 定位 handler
3) Server 将 `tools/call` 转至 Gateway
4) Gateway 依据注册表：校验依赖与参数 → 熔断检查 → 动态加载服务实例 → 执行策略
5) Service 执行并（可选）调用下游 API → 产出 `ServiceResponse`
6) BaseService 统一过滤为 `output_schema` 对齐的结果 → Gateway 包装为 MCP `ToolResult`
7) Server 返回 JSON-RPC 成功/错误响应；若流式则通过 SSE 推送

---

### JSON-RPC 方法约定
- `initialize(params)` → 协商 `protocolVersion`、返回 `serverInfo` 与 `capabilities`
- `tools/list(params)` → 返回 `[{name, title, description, inputSchema, outputSchema?}]`
- `tools/call({name, arguments})` → 返回 MCP 工具执行结构：
```json
{
  "content": [{"type": "text", "text": "..."}],
  "structuredContent": {"status": "success", ...},
  "isError": false
}
```

---

### 注册表模板（`src/registry/tools.json`）
```json
{
  "version": "2.0",
  "metadata": {"protocol_version": "2025-06-18"},
  "services": {
    "<service_name>": {
      "name": "<service_name>",
      "title": "<Human Readable Title>",
      "description": "<What this tool does>",
      "version": "1.0.0",
      "category": "memory|aggregated|specialized",
      "endpoint": "src.services.<service_name>.service:<ClassName>",
      "strategies": [
        {"name": "default", "description": "Default behavior", "default": true}
      ],
      "schema": { "type": "object", "properties": {"param": {"type": "string"}}, "required": ["param"] },
      "output_schema": { "type": "object", "properties": {"status": {"type": "string"}} },
      "dependencies": ["optional_other_service"],
      "health_check": "/health",
      "metrics": "/metrics"
    }
  },
  "global_config": {
    "timeout_seconds": 30,
    "max_retries": 3,
    "circuit_breaker": {"failure_threshold": 5, "reset_timeout_seconds": 60}
  }
}
```

---

### BaseService 模版
```python
from typing import Dict, Any
from src.services.base.service import BaseService, BaseStrategy, ServiceResponse

class DefaultStrategy(BaseStrategy):
    def __init__(self):
        super().__init__("default", "Default behavior")

    async def execute(self, arguments: Dict[str, Any], context: Dict[str, Any]) -> ServiceResponse:
        # 读取参数
        param = arguments.get("param")
        # 业务逻辑（调用下游 API 可通过自定义 client）
        result = {"result": f"processed:{param}"}
        # 返回统一结构
        return ServiceResponse(status="success", message="ok", data=result)

class YourService(BaseService):
    def _initialize_strategies(self) -> None:
        self.register_strategy(DefaultStrategy())
```

---

### 策略扩展示例
```python
class FastStrategy(BaseStrategy):
    def __init__(self):
        super().__init__("fast", "Lower latency trade-offs")

    async def execute(self, arguments, context):
        # 快速路径
        return ServiceResponse(status="success", message="fast", data={"mode": "fast"})
```

在 `tools.json` 内新增：
```json
{"name": "fast", "description": "Lower latency", "default": false}
```

---

### Gateway 约束与建议
- 在网关统一执行：
  - 服务存在性校验、依赖校验
  - 入参与输出 Schema 校验（建议接入 `jsonschema` 或 `pydantic`）
  - 熔断与重试策略（依赖 `global_config`）
- 统一将服务返回映射为 MCP `ToolResult`，`structuredContent` 对齐 `output_schema`

---

### Transport 规范
- 单一端点：`/mcp`
- 方法：`POST`(请求) / `GET`(SSE) / `DELETE`(终止会话)
- 会话：`Mcp-Session-Id`，心跳与过期清理；`Last-Event-ID`（可选重放）
- CORS：生产建议来源白名单，通过环境变量配置

---

### 配置与环境变量建议
- 服务端：
  - `MCP_HOST`、`MCP_PORT`、`MCP_ENDPOINT_PATH`、`MCP_SESSION_TIMEOUT`
  - `MCP_ALLOWED_ORIGINS`（生产建议明确白名单）
  - `MCP_LOG_LEVEL`、`MCP_DEV_MODE`
- 下游 API：
  - `API_URL`、`API_KEY`、`ORG_ID`、`PROJECT_ID`、`TIMEOUT`
- 严格 Schema 模式：
  - `MCP_STRICT_SCHEMA=true`（默认）

---

### 安全基线（最小集）
- 仅绑定本地或受控网段；生产禁止默认 `*` CORS
- 校验 `Origin`，阻断 DNS 重绑定
- 认证统一在网关/客户端封装，避免服务散落处理
- 日志脱敏（token/id/私有数据）

---

### 可靠性与弹性
- 熔断（closed → open → half-open）与重试后退
- 会话过期清理与心跳
- 超时分级：初始化/列举工具（短）、工具执行（长）

---

### 可观测性
- 健康检查：`GET /health`
- 状态页：`GET /status`
- 指标（可选）：服务层 `get_metrics()` 聚合

---

### 测试清单
- 单元：服务/策略输入输出契约、异常路径
- 集成：网关+注册表加载、动态实例化、熔断行为
- 合规：MCP 规范初始化、工具列举/调用、错误语义
- 性能：并发工具调用、SSE 连接稳定性、超时与背压

---

### 部署清单
- 本地：`python run_server_http.py`
- 生产：`python run_server_production.py --log-level INFO`
- Docker：按 `Dockerfile` 构建非 root 镜像；`docker-compose.yml` 暴露端口与网络

---

### 版本与兼容
- 协商 `protocolVersion`，维护受支持版本列表（集中常量）
- 对严格客户端（如 Dify）开启严格输出 Schema 过滤

---

### 常见扩展模式
- 聚合服务：编排多个基础服务（在 `dependencies` 声明，并通过 `call_dependency_service` 调用）
- 专用化服务：针对特定检索/规则/路由的专业化策略

---

### 新服务落地流程（Checklist）
1. 在 `src/registry/tools.json` 注册服务、策略、输入/输出 Schema、依赖
2. 在 `src/services/<service>/service.py` 基于 BaseService/Strategy 实现
3. 若需下游访问，实现/复用 `src/client/api_client.py`
4. 启动服务并 `tools/list` 验证注册；`tools/call` 验证 Schema 与输出
5. 编写最小单元与集成用例；跑合规与基本性能用例
6. 配置 CORS 与日志级别，部署到目标环境

---

### 设计原则落地（KISS/YAGNI/DRY/SOLID）
- KISS：分层清晰、职责单一；集中常量与配置；尽量少的可选路径
- YAGNI：未落地能力先用占位或特性开关；避免过早复杂化
- DRY：注册表驱动 + 统一校验与输出过滤；客户端/日志/错误处理集中复用
- SOLID：
  - SRP：Transport/Server/Gateway/Registry/Service 各司其职
  - OCP：新增服务仅注册+实现，无需改动现有核心
  - LSP：服务/策略实现统一接口，可互换
  - ISP：策略接口小而专注
  - DIP：上层依赖抽象（注册表/网关/客户端），下层实现可替换

---

### 模版变量（建议）
- `<service_name>`：工具名（唯一）
- `<ClassName>`：服务类名（PascalCase）
- `<param>`：输入枚举与类型约束放在 `schema`
- `output_schema`：仅暴露客户端需要的字段；开启严格模式后会自动过滤

---

如需示例，请复制本模版到新项目并替换尖括号字段；按“新服务落地流程”逐项完成即可。






