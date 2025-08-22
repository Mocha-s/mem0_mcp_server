# Mem0 MCP Server - 面向服务的混合式架构

🧠 **智能内存MCP服务器** - 基于MCP 2025-06-18规范和面向服务架构设计

## 🏗️ 架构概览

采用"聚合 + 专业化"的混合式设计理念，将工具能力服务化：

- **MCP服务器层**: 实现MCP 2025-06-18规范，支持Streamable HTTP传输
- **API网关层**: ToolManager作为统一入口，处理路由和负载均衡  
- **服务注册中心**: 动态服务发现和配置管理
- **微服务层**: 独立的内存操作服务，支持多种执行策略
- **Mem0客户端**: 与本地Mem0 API服务器通信 (localhost:8000)

## �� 项目结构

```
mem0_mcp_server/
├── 🚀 run_server_http.py         # Streamable HTTP服务器启动入口
├── 🚀 run_server_production.py   # 生产环境服务器启动入口
├── 📋 README.md                  # 项目说明
├── 📦 requirements.txt           # 依赖包
├── 🐳 Dockerfile                 # Docker容器配置
├── 🐳 docker-compose.yml         # Docker Compose配置
├── 🏗️ src/
│   ├── server/                   # MCP服务器实现
│   │   └── mcp_server.py         # 主服务器协调器
│   ├── transport/                # 传输层
│   │   └── streamable_http.py    # Streamable HTTP实现
│   ├── client/                   # Mem0 API客户端
│   │   └── mem0_api_client.py    # 异步HTTP客户端
│   ├── gateway/                  # API网关层
│   │   └── tool_manager.py       # 工具管理器和路由
│   ├── registry/                 # 服务注册中心
│   │   ├── tools.json            # 服务注册表
│   │   └── registry_manager.py   # 注册表管理器
│   ├── services/                 # 微服务层
│   │   ├── base/                 # 基础服务类
│   │   │   └── service.py        # 服务基类和策略模式
│   │   ├── mem0_add_memory/           # 添加内存服务
│   │   │   └── service.py        # 支持contextual/graph/multimodal策略
│   │   ├── mem0_search_memories/      # 搜索内存服务
│   │   │   └── service.py        # semantic/graph/advanced/hybrid策略
│   │   ├── mem0_update_memory/        # 更新内存服务
│   │   │   └── service.py        # single/batch策略
│   │   ├── delete_memory/        # 删除内存服务
│   │   │   └── service.py        # single/batch/filtered策略
│   │   ├── selective_memory/     # 选择性内存(聚合服务)
│   │   └── criteria_retrieval/   # 条件检索(专业化服务)
│   ├── protocol/                 # MCP协议层
│   │   └── messages.py           # 消息类型定义
│   └── strategies/               # 共享策略库
├── 🧪 tests/                    # 测试套件
│   ├── README.md                # 测试说明
│   ├── test_requirements.txt    # 测试依赖
│   └── run_compliance_tests.py  # 合规性测试
└── 📚 docs/                     # 文档
    └── architecture/             # 架构文档
        ├── architecture_design_proposal_v2.md  # 原设计提案
        └── service_oriented_architecture.md    # 服务化架构说明
```

## ⚡ 快速开始

### 1. 环境准备

```bash
# 克隆项目
git clone <repository-url>
cd mem0_mcp_server

# 安装依赖
pip install -r requirements.txt

# 配置环境变量
cp .env.example .env
# 编辑 .env 文件配置你的Mem0 API信息
```

### 2. 启动本地Mem0 API服务器

确保Mem0 API服务器运行在 `http://localhost:8000`

```bash
# 如果使用Docker
docker run -p 8000:8000 mem0/mem0-api

# 或者本地启动
mem0 serve --port 8000
```

### 3. 启动MCP服务器

**Streamable HTTP模式 (推荐)**:
```bash
python run_server_http.py
```

**生产环境模式**:
```bash
python run_server_production.py
```

**Docker模式**:
```bash
docker-compose up -d
```

### 4. 配置MCP客户端

**⚠️ 重要提醒：当前版本仅支持 Streamable HTTP 模式**

本服务器当前仅支持 Streamable HTTP 传输协议。请确保先启动HTTP服务器，然后配置客户端连接到正确的端点。

**Claude Desktop配置 (Streamable HTTP)**:
```json
{
  "mcpServers": {
    "mem0": {
      "transport": "http", 
      "endpoint": "http://127.0.0.1:8080/mcp",
      "env": {
        "MEM0_API_KEY": "your_api_key_if_needed"
      }
    }
  }
}
```

**其他MCP客户端配置示例**:
```json
{
  "servers": {
    "mem0": {
      "type": "http",
      "url": "http://127.0.0.1:8080/mcp",
      "headers": {
        "Authorization": "Token your_api_key_if_needed"
      }
    }
  }
}
```

**连接测试**：
```bash
# 测试服务器健康状态
curl http://127.0.0.1:8080/

# 测试MCP初始化
curl -X POST http://127.0.0.1:8080/mcp \
  -H "Content-Type: application/json" \
  -d '{"jsonrpc":"2.0","id":"1","method":"initialize","params":{"protocolVersion":"2025-06-18","capabilities":{},"clientInfo":{"name":"test","version":"1.0"}}}'
```

## 🔧 可用工具

### 内存操作服务

#### mem0_add_memory - 添加新内存
**端点**: `/v1/memories/` (支持version参数v2处理逻辑)

**策略**:
- `contextual`: 上下文感知策略 - 基于对话上下文智能提取关键信息
- `graph`: 图形关系策略 - 建立内存间的语义关系网络
- `multimodal`: 多模态策略 - 支持图像、音频、文本等多种数据类型

**示例**:
```json
{
  "tool": "mem0_add_memory",
  "arguments": {
    "messages": [
      {"role": "user", "content": "我计划下个月去东京旅行"},
      {"role": "assistant", "content": "好的，我会记住这个信息"}
    ],
    "user_id": "alice",
    "strategy": "graph",
    "enable_graph": true,
    "metadata": {
      "category": "travel",
      "priority": "high"
    }
  }
}
```

#### search_memories - 搜索内存
**端点**: `/v2/memories/search/` (v2 API专用)

**策略**:
- `semantic`: 语义搜索策略 - 基于向量相似度的语义匹配
- `graph`: 图形搜索策略 - 基于关系网络的图搜索
- `advanced_retrieval`: 高级搜索策略 - 支持重排序和混合检索
- `hybrid`: 混合搜索策略 - 结合多种搜索方法的智能检索

**示例**:
```json
{
  "tool": "search_memories", 
  "arguments": {
    "query": "旅行计划",
    "user_id": "alice",
    "filters": {
      "categories": ["travel", "planning"],
      "date_range": {
        "start": "2024-01-01",
        "end": "2024-12-31"
      }
    },
    "strategy": "hybrid",
    "top_k": 10,
    "threshold": 0.7
  }
}
```

#### update_memory - 更新内存
**端点**: `/v1/memories/{id}/`

**策略**:
- `single`: 单个更新策略 - 更新指定ID的内存
- `batch`: 批量更新策略 - 批量更新多个内存

**示例**:
```json
{
  "tool": "update_memory",
  "arguments": {
    "memory_id": "mem_123",
    "text": "更新后的内存内容",
    "metadata": {
      "updated_at": "2024-01-15T10:30:00Z",
      "version": "2.0"
    }
  }
}
```

#### delete_memory - 删除内存
**端点**: `/v1/memories/{id}/`

**策略**:
- `single`: 单个删除策略 - 删除指定ID的内存
- `batch`: 批量删除策略 - 批量删除多个内存
- `filtered`: 条件删除策略 - 根据条件删除匹配的内存

**示例**:
```json
{
  "tool": "delete_memory",
  "arguments": {
    "filters": {
      "user_id": "alice",
      "categories": ["temp", "draft"]
    },
    "strategy": "filtered"
  }
}
```

### 聚合服务

#### selective_memory - 选择性内存操作
基于条件的选择性内存操作，支持复杂的查询和批量处理。

**示例**:
```json
{
  "tool": "selective_memory",
  "arguments": {
    "criteria": {
      "user_id": "alice",
      "date_range": {
        "start": "2024-01-01",
        "end": "2024-01-31"
      },
      "categories": ["work", "important"]
    },
    "operation": "search"
  }
}
```

#### criteria_retrieval - 高级条件检索
专门用于复杂条件检索的专业化服务，支持高级查询语法。

**示例**:
```json
{
  "tool": "criteria_retrieval",
  "arguments": {
    "criteria": {
      "AND": [
        {"user_id": "alice"},
        {"OR": [
          {"categories": {"in": ["work", "urgent"]}},
          {"priority": {"gte": 8}}
        ]}
      ]
    },
    "user_id": "alice"
  }
}
```

## 🌐 MCP传输支持

### Streamable HTTP (MCP 2025-06-18) ✅ 完全支持
- ✅ HTTP POST for client requests
- ✅ HTTP GET for SSE streams  
- ✅ Session management with Mcp-Session-Id
- ✅ Resumable streams with Last-Event-ID
- ✅ Multiple concurrent connections
- ✅ Origin validation for security
- ✅ Protocol version negotiation
- ✅ JSON response mode for simple requests

### 传统stdio ❌ 暂不支持
目前 `run_server.py` 仅为服务架构演示，不提供实际的stdio MCP通信。计划在未来版本中实现完整的stdio传输支持。

**如需使用MCP功能，请使用 Streamable HTTP 模式：**
1. 启动: `python run_server_http.py` 
2. 连接: `http://127.0.0.1:8080/mcp`

## 🔌 API端点适配

### Mem0 API版本支持
- **Add Memory**: `/v1/memories/` (支持version参数v2处理逻辑)
- **Search Memory**: `/v2/memories/search/` (v2 API专用)
- **Update Memory**: `/v1/memories/{id}/` 
- **Delete Memory**: `/v1/memories/{id}/`
- **Get Memory**: `/v1/memories/{id}/`

### 本地服务器通信
- **默认地址**: `http://localhost:8000`
- **认证**: Token-based authentication
- **协议**: HTTP/1.1 with JSON payloads
- **超时**: 30秒默认超时

## 🛠️ 开发指南

### 添加新服务

1. **在tools.json注册服务**:
```json
{
  "new_service": {
    "name": "new_service",
    "title": "New Service",
    "description": "Description of the new service",
    "version": "1.0.0",
    "category": "custom",
    "endpoint": "src.services.new_service.service:NewService",
    "strategies": [
      {
        "name": "default",
        "description": "Default strategy",
        "default": true
      }
    ],
    "schema": {
      "type": "object",
      "properties": {
        "param1": {"type": "string"},
        "param2": {"type": "number"}
      },
      "required": ["param1"]
    }
  }
}
```

2. **实现服务类**:
```python
from src.services.base.service import BaseService, BaseStrategy

class DefaultStrategy(BaseStrategy):
    def __init__(self):
        super().__init__("default", "Default strategy")
    
    async def execute(self, arguments, context):
        # 实现具体逻辑
        return ServiceResponse(
            status="success",
            message="Operation completed",
            data={"result": "success"}
        )

class NewService(BaseService):
    def _initialize_strategies(self):
        self.register_strategy(DefaultStrategy())
```

3. **服务自动可用** - 无需修改其他代码

### 服务间调用
```python
# 在服务内调用其他服务
result = await self.call_dependency_service(
    "search_memories", 
    {"query": "context", "user_id": user_id}
)
```

### 环境配置
```bash
# 必需的环境变量
export MEM0_API_KEY="your_mem0_api_key"
export MEM0_API_URL="http://localhost:8000"

# 可选的环境变量
export MEM0_ORG_ID="your_org_id"
export MEM0_PROJECT_ID="your_project_id"
export MCP_PORT="8080"
export MCP_HOST="127.0.0.1"
export MCP_SESSION_TIMEOUT="3600"
export MCP_CORS_ORIGINS="*"
export MCP_DEV_MODE="true"
```

## 🐳 Docker部署

### 使用Docker Compose (推荐)
```bash
# 启动服务
docker-compose up -d

# 查看日志
docker-compose logs -f

# 停止服务
docker-compose down
```

### 手动Docker部署
```bash
# 构建镜像
docker build -t mem0-mcp-server .

# 运行容器
docker run -d \
  --name mem0-mcp-server \
  -p 8080:8080 \
  -e MEM0_API_KEY="your_api_key" \
  -e MEM0_API_URL="http://localhost:8000" \
  mem0-mcp-server
```

## 🧪 测试

### 运行测试套件
```bash
# 安装测试依赖
pip install -r tests/test_requirements.txt

# 运行合规性测试
python tests/run_compliance_tests.py

# 运行特定测试
python -m pytest tests/test_mcp_compliance_analysis.py
```

### 测试覆盖范围
- **合规性测试**: MCP 2025-06-18规范合规性验证
- **集成测试**: 与实际Mem0 API的集成测试
- **单元测试**: 各服务组件的单元测试
- **性能测试**: 负载和并发性能测试

## 🔐 安全特性

- **Origin验证**: 防止DNS重绑定攻击
- **本地绑定**: 仅绑定到127.0.0.1避免网络暴露
- **会话管理**: 安全的会话ID和超时机制
- **数据隔离**: 服务间不能直接访问彼此数据
- **认证代理**: 统一的API密钥管理
- **CORS控制**: 可配置的跨域资源共享策略

## 📊 监控和观测

- **健康检查**: 每个服务提供/health端点
- **指标监控**: 服务调用次数、成功率、响应时间
- **Circuit Breaker**: 自动故障隔离和恢复
- **结构化日志**: 便于调试和问题追踪
- **会话统计**: 活跃连接和会话监控

### 监控端点
```bash
# 健康检查
curl http://127.0.0.1:8080/health

# 服务状态
curl http://127.0.0.1:8080/status

# 指标监控
curl http://127.0.0.1:8080/metrics
```

## 🚨 故障排除

### 常见问题

#### 1. 连接Mem0 API失败
**症状**: `ConnectionError: Failed to connect to Mem0 API`
**解决方案**:
```bash
# 检查Mem0 API服务器状态
curl http://localhost:8000/health

# 验证API密钥
curl -H "Authorization: Token your_api_key" http://localhost:8000/v1/memories/
```

#### 2. MCP客户端连接失败
**症状**: `Failed to initialize MCP connection`
**解决方案**:
```bash
# 检查MCP服务器状态
curl http://127.0.0.1:8080/

# 验证端口配置
netstat -tlnp | grep 8080

# 检查防火墙设置
sudo ufw status
```

#### 3. 服务注册失败
**症状**: `Service not found in registry`
**解决方案**:
```bash
# 检查服务注册表
cat src/registry/tools.json

# 重启服务器
python run_server_http.py
```

#### 4. 内存不足错误
**症状**: `MemoryError: Out of memory`
**解决方案**:
```bash
# 增加系统内存限制
ulimit -m 1048576

# 使用Docker时增加内存限制
docker run --memory=2g mem0-mcp-server
```

### 日志分析
```bash
# 查看详细日志
tail -f logs/mcp_server.log

# 搜索错误日志
grep "ERROR" logs/mcp_server.log

# 分析性能日志
grep "response_time" logs/mcp_server.log | awk '{print $NF}' | sort -n
```

## 🎯 设计优势

1. **🔧 高度可扩展**: 新服务只需注册到Registry即可使用
2. **⚡ 松散耦合**: 服务间通过ToolManager中介调用
3. **🛡️ 错误隔离**: 单个服务失败不影响整体系统
4. **📈 性能优化**: 支持负载均衡和Circuit Breaker模式
5. **🔄 版本管理**: 服务独立版本演进
6. **🧪 易于测试**: 每个服务可独立测试
7. **🌐 传输灵活**: 支持Streamable HTTP和stdio传输
8. **🔒 安全保护**: Origin验证、会话隔离、本地绑定

## 🚀 未来扩展

- **多模态内存支持** (图像、音频、视频)
- **高级图形查询** (关系推理、路径查找)
- **个性化推荐** (基于用户历史的智能建议)
- **实时协作** (多用户共享内存空间)
- **知识图谱集成** (连接外部知识库)
- **WebSocket传输** (实时双向通信)
- **分布式部署** (支持集群和负载均衡)
- **插件系统** (第三方服务集成)

## 📞 支持

- **GitHub Issues**: [项目Issues页面]
- **Discord社区**: [Mem0官方Discord]
- **开发者文档**: `docs/` 目录
- **API文档**: 参考本文档的API部分
- **示例代码**: 参考本文档的示例部分

## 📄 许可证

本项目采用 MIT 许可证 - 详见 [LICENSE](LICENSE) 文件

---

基于 **MCP 2025-06-18** 规范 | 采用 **面向服务架构** | 支持 **Mem0智能内存平台** | 实现 **Streamable HTTP传输**