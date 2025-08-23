# Mem0 MCP Server v2.0

🧠 **智能内存管理MCP服务器** - 基于TypeScript和官方MCP SDK的企业级实现

[![MCP](https://img.shields.io/badge/MCP-2025--06--18-blue)](https://modelcontextprotocol.io)
[![TypeScript](https://img.shields.io/badge/TypeScript-5.5.4-blue)](https://www.typescriptlang.org/)
[![SDK](https://img.shields.io/badge/@modelcontextprotocol/sdk-1.17.4-green)](https://www.npmjs.com/package/@modelcontextprotocol/sdk)
[![Node](https://img.shields.io/badge/Node.js-%3E%3D18-green)](https://nodejs.org/)

## 📋 目录

- [功能特性](#-功能特性)
- [系统架构](#-系统架构)
- [快速开始](#-快速开始)
- [工具说明](#-工具说明)
- [API参考](#-api参考)
- [部署指南](#-部署指南)
- [开发指南](#-开发指南)
- [故障排除](#-故障排除)

## 🎯 功能特性

### 核心能力
- ✅ **完整MCP实现** - 严格遵循MCP 2025-06-18规范
- ✅ **TypeScript原生** - 100%类型安全，智能代码提示
- ✅ **6大内存工具** - 覆盖内存管理全生命周期
- ✅ **多策略支持** - 语义、图形、混合等多种执行策略
- ✅ **企业级设计** - 生产就绪，高可用架构

### 技术特点
- 🚀 **高性能** - 异步非阻塞，支持并发请求
- 🔒 **安全可靠** - JWT会话管理，CORS保护
- 🌐 **灵活部署** - 支持stdio/HTTP双模式
- 📊 **可观测性** - 健康检查，实时日志
- 🔧 **易于扩展** - 模块化设计，插件友好

## 🏗️ 系统架构

```
┌─────────────────────────────────────────────┐
│           MCP Client (Claude等)             │
└─────────────┬───────────────────────────────┘
              │ HTTP/stdio
┌─────────────▼───────────────────────────────┐
│         MCP Server (TypeScript)             │
│  ┌──────────────────────────────────────┐  │
│  │     Express HTTP Transport Layer     │  │
│  └──────────────┬───────────────────────┘  │
│  ┌──────────────▼───────────────────────┐  │
│  │        McpServer Core (SDK)          │  │
│  └──────────────┬───────────────────────┘  │
│  ┌──────────────▼───────────────────────┐  │
│  │         Memory Tools Layer           │  │
│  └──────────────┬───────────────────────┘  │
└─────────────────┼───────────────────────────┘
                  │ REST API
┌─────────────────▼───────────────────────────┐
│          Mem0 API Server                    │
│         (localhost:8000)                    │
└─────────────────────────────────────────────┘
```

## 🚀 快速开始

### 前置要求

- Node.js >= 18.0.0
- npm >= 9.0.0
- Mem0 API服务器 (本地或远程)

### 安装步骤

1. **克隆项目**
```bash
git clone <repository-url>
cd mem0_mcp_server
```

2. **安装依赖**
```bash
npm install
```

3. **环境配置**
```bash
# 创建环境配置文件
cp .env.example .env

# 编辑配置
nano .env
```

环境变量说明：
```env
# Mem0 API配置 (必需)
MEM0_API_URL=http://localhost:8000  # Mem0 API服务器地址
MEM0_API_KEY=your_api_key          # API密钥(如需要)

# MCP服务器配置 (可选)
MCP_HOST=127.0.0.1                 # 监听地址
MCP_PORT=8081                      # 监听端口
MCP_SESSION_TIMEOUT=3600           # 会话超时(秒)
MCP_CORS_ORIGINS=*                 # CORS允许的源
MCP_DEV_MODE=false                 # 开发模式

# Mem0配置 (可选)
MEM0_ORG_ID=your_org_id           # 组织ID
MEM0_PROJECT_ID=your_project_id   # 项目ID
MEM0_TIMEOUT=30000                 # 请求超时(毫秒)
MEM0_MAX_RETRIES=3                 # 最大重试次数
```

4. **构建项目**
```bash
npm run build
```

5. **启动服务器**

**HTTP模式 (推荐)**
```bash
# 本地访问
npm run start:http

# 局域网访问
npm run start:http:external
```

**stdio模式**
```bash
npm start
```

### 验证安装

```bash
# 健康检查
curl http://localhost:8081/health

# 预期输出
{
  "status": "healthy",
  "server": "mem0-mcp-server",
  "version": "2.0.0",
  "timestamp": "2025-08-23T05:00:00.000Z",
  "activeSessions": 0,
  "mem0Api": {
    "url": "http://localhost:8000",
    "connected": true
  }
}
```

## 🛠️ 工具说明

### 1. mem0_add_memory
**功能**: 从对话中提取并添加新内存

**参数**:
- `messages` (required): 对话消息数组
- `user_id` (required): 用户标识符
- `enable_graph` (optional): 启用图形关系
- `metadata` (optional): 附加元数据
- `infer` (optional): 自动推理事实

**示例**:
```json
{
  "messages": [
    {"role": "user", "content": "我下个月要去东京旅行"},
    {"role": "assistant", "content": "好的，我记住了"}
  ],
  "user_id": "alice",
  "enable_graph": true,
  "metadata": {"category": "travel"}
}
```

### 2. mem0_search_memories
**功能**: 智能搜索内存库

**参数**:
- `query` (required): 搜索查询
- `user_id` (required): 用户ID
- `strategy` (optional): 搜索策略 [semantic|graph|advanced_retrieval|hybrid]
- `top_k` (optional): 返回数量
- `threshold` (optional): 相似度阈值

**策略说明**:
- `semantic`: 基于语义向量的相似度搜索
- `graph`: 利用知识图谱关系搜索
- `advanced_retrieval`: 高级检索与重排序
- `hybrid`: 混合多种策略的智能搜索

### 3. mem0_update_memory
**功能**: 更新现有内存

**模式**:
- 单个更新: 指定memory_id
- 批量更新: 使用batch_updates数组

### 4. mem0_delete_memory
**功能**: 删除内存记录

**模式**:
- 按ID删除: 指定memory_id
- 按用户删除: 指定user_id
- 条件删除: 使用filters
- 批量删除: 使用batch_deletes

### 5. mem0_selective_memory
**功能**: 基于条件的聚合操作

**操作类型**:
- `add`: 添加符合条件的内存
- `search`: 搜索符合条件的内存
- `update`: 更新符合条件的内存
- `delete`: 删除符合条件的内存

### 6. mem0_criteria_retrieval
**功能**: 高级条件检索

**特性**:
- 支持复杂的AND/OR条件组合
- 多维度评分机制
- 智能结果排序

## 📡 API参考

### HTTP端点

| 端点 | 方法 | 描述 |
|------|------|------|
| `/mcp` | POST | MCP请求处理 |
| `/mcp` | GET | SSE事件流 |
| `/mcp` | DELETE | 会话终止 |
| `/health` | GET | 健康检查 |

### MCP协议方法

| 方法 | 描述 |
|------|------|
| `initialize` | 初始化会话 |
| `tools/list` | 列出可用工具 |
| `tools/call` | 调用工具 |

## 🚢 部署指南

### Docker部署

1. **构建镜像**
```bash
docker build -t mem0-mcp-server .
```

2. **运行容器**
```bash
docker run -d \
  --name mem0-mcp \
  -p 8081:8081 \
  -e MEM0_API_URL=http://host.docker.internal:8000 \
  -e MEM0_API_KEY=your_key \
  mem0-mcp-server
```

### PM2部署

```bash
# 安装PM2
npm install -g pm2

# 启动服务
pm2 start npm --name "mem0-mcp" -- run start:http:external

# 保存配置
pm2 save
pm2 startup
```

### systemd服务

```ini
[Unit]
Description=Mem0 MCP Server
After=network.target

[Service]
Type=simple
User=www-data
WorkingDirectory=/opt/mem0_mcp_server
ExecStart=/usr/bin/node dist/index.js --http --host 0.0.0.0 --port 8081
Restart=on-failure
Environment=MEM0_API_URL=http://localhost:8000

[Install]
WantedBy=multi-user.target
```

## 💻 开发指南

### 项目结构

```
mem0_mcp_server/
├── src/
│   ├── index.ts           # 主入口
│   ├── config/
│   │   └── index.ts       # 配置管理
│   ├── client/
│   │   └── mem0-api.ts    # Mem0 API客户端
│   └── tools/
│       └── index.ts       # 工具实现
├── dist/                  # 编译输出
├── package.json          # 项目配置
├── tsconfig.json         # TypeScript配置
└── README.md            # 本文档
```

### 开发命令

```bash
# 开发模式(自动重载)
npm run dev

# 构建项目
npm run build

# 运行测试
npm test

# 代码检查
npm run lint

# 格式化代码
npm run format
```

### 添加新工具

1. 在 `src/tools/index.ts` 中添加工具方法
2. 在 `src/index.ts` 中注册工具
3. 更新本README的工具说明

## 🔧 MCP客户端配置

### Claude Desktop

编辑 `claude_desktop_config.json`:

```json
{
  "mcpServers": {
    "mem0": {
      "transport": "http",
      "endpoint": "http://localhost:8081/mcp",
      "env": {
        "MEM0_API_KEY": "your_api_key"
      }
    }
  }
}
```

### 其他MCP客户端

```json
{
  "servers": [
    {
      "name": "mem0",
      "type": "http",
      "url": "http://localhost:8081/mcp",
      "headers": {
        "Authorization": "Token your_api_key"
      }
    }
  ]
}
```

## ❓ 故障排除

### 常见问题

**Q: 无法连接到Mem0 API**
```bash
# 检查Mem0服务状态
curl http://localhost:8000/health

# 验证API密钥
curl -H "Authorization: Token YOUR_KEY" http://localhost:8000/v1/memories/
```

**Q: 会话ID无效**
- 确保客户端正确处理mcp-session-id header
- 检查会话超时设置

**Q: CORS错误**
- 设置 `MCP_CORS_ORIGINS=*` 允许所有源
- 或指定具体的允许源

**Q: 端口已占用**
```bash
# 查找占用端口的进程
lsof -i :8081

# 或使用其他端口
npm run start:http -- --port 8082
```

### 日志调试

```bash
# 启用调试日志
MCP_DEV_MODE=true npm run start:http

# 查看实时日志
tail -f logs/mcp-server.log
```

## 📈 性能优化

- **连接池**: Mem0客户端使用持久连接
- **重试机制**: 指数退避算法
- **超时控制**: 可配置的请求超时
- **并发限制**: 防止资源耗尽

## 🔒 安全建议

1. **生产环境**:
   - 使用HTTPS
   - 启用认证
   - 限制CORS源
   - 配置防火墙

2. **API密钥**:
   - 使用环境变量
   - 定期轮换
   - 限制权限范围

3. **网络**:
   - 使用私有网络
   - 配置VPN访问
   - 监控异常流量

## 🤝 贡献指南

欢迎贡献代码、报告问题或提出建议！

1. Fork项目
2. 创建特性分支
3. 提交变更
4. 推送到分支
5. 创建Pull Request

## 📄 许可证

MIT License - 详见 [LICENSE](LICENSE) 文件

## 🔗 相关链接

- [MCP规范](https://modelcontextprotocol.io)
- [Mem0文档](https://docs.mem0.ai)
- [TypeScript手册](https://www.typescriptlang.org/docs/)
- [问题反馈](https://github.com/your-repo/issues)

---

💡 **提示**: 遇到问题？查看[故障排除](#-故障排除)或提交[Issue](https://github.com/your-repo/issues)

⭐ 如果这个项目对你有帮助，请给我们一个Star！