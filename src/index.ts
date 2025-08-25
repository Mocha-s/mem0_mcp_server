/**
 * Mem0 MCP Server with TypeScript SDK Integration
 * Implements all 6 memory management services
 */

import { McpServer } from '@modelcontextprotocol/sdk/server/mcp.js';
import { StdioServerTransport } from '@modelcontextprotocol/sdk/server/stdio.js';
import { StreamableHTTPServerTransport } from '@modelcontextprotocol/sdk/server/streamableHttp.js';
import { isInitializeRequest } from '@modelcontextprotocol/sdk/types.js';
import { z } from 'zod';
import { config } from './config/index.js';
import { Mem0Tools } from './tools/index.js';
import { AsyncLocalStorage } from 'async_hooks';

export class Mem0McpServer {
  private mcpServer: McpServer;
  private mem0Tools: Mem0Tools;
  private static sessionContexts: Map<string, { userId?: string }> = new Map();
  
  // AsyncLocalStorage for user context
  private static userContextStorage = new AsyncLocalStorage<{ userId?: string }>();

  // Get current user context from AsyncLocalStorage
  static getCurrentUserContext(): { userId?: string } {
    return Mem0McpServer.userContextStorage.getStore() || {};
  }

  // Set user context for AsyncLocalStorage
  static async runWithUserContext<T>(
    context: { userId?: string }, 
    callback: () => Promise<T>
  ): Promise<T> {
    return Mem0McpServer.userContextStorage.run(context, callback);
  }

  constructor() {
    // Create official SDK standard McpServer
    this.mcpServer = new McpServer(
      {
        name: 'mem0-mcp-server',
        version: '2.0.0'
      },
      {
        capabilities: {
          tools: {}
        }
      }
    );

    // Initialize Mem0 tools with session context access
    this.mem0Tools = new Mem0Tools(Mem0McpServer.sessionContexts);
    
    // Make the static method available globally for tools
    (global as any).Mem0McpServer = Mem0McpServer;
    
    this.registerTools();
  }

  // Static method to get session context
  static getSessionContext(sessionId?: string): { userId?: string } | undefined {
    return sessionId ? Mem0McpServer.sessionContexts.get(sessionId) : undefined;
  }

  // Static method to set session context  
  static setSessionContext(sessionId: string, context: { userId?: string }): void {
    Mem0McpServer.sessionContexts.set(sessionId, context);
  }

  // Static method to clear session context
  static clearSessionContext(sessionId: string): void {
    Mem0McpServer.sessionContexts.delete(sessionId);
  }

  private registerTools(): void {
    // Register mem0_add_memory tool
    this.mcpServer.registerTool(
      'mem0_add_memory',
      {
        title: '添加记忆',
        description: '从对话消息中添加新记忆，支持上下文、图形和多模态策略。至少需要提供 user_id、agent_id 或 run_id 中的一个。如果使用 /mcp/{user_id} 路径格式，会自动使用路径中的用户ID。',
        inputSchema: {
          messages: z.array(z.object({
            role: z.enum(['user', 'assistant']),
            content: z.string()
          })).describe('用于提取记忆的对话消息数组'),
          user_id: z.string().optional().describe('用户唯一标识符（如果未提供且路径中无user_id，则agent_id和run_id至少需要一个）'),
          agent_id: z.string().optional().describe('代理唯一标识符（如果未提供user_id和run_id则必需）'),
          run_id: z.string().optional().describe('运行唯一标识符（如果未提供user_id和agent_id则必需）'),
          enable_graph: z.boolean().optional().describe('是否启用图关系记忆'),
          metadata: z.record(z.any()).optional().describe('附加的元数据信息'),
          infer: z.boolean().optional().describe('是否启用自动事实推理')
        }
      },
      async ({ messages, user_id, agent_id, run_id, enable_graph, metadata, infer }) => {
        // Get user context from AsyncLocalStorage if user_id not explicitly provided
        if (!user_id && !agent_id && !run_id) {
          const currentContext = Mem0McpServer.getCurrentUserContext();
          if (currentContext.userId) {
            console.log(`🎯 Auto-injecting user_id: ${currentContext.userId} from AsyncLocalStorage context`);
            user_id = currentContext.userId;
          }
        }
        
        const result = await this.mem0Tools.addMemory({
          messages,
          user_id,
          agent_id,
          run_id,
          enable_graph,
          metadata,
          infer
        });
        
        return {
          content: [{
            type: 'text' as const,
            text: JSON.stringify(result, null, 2)
          }]
        };
      }
    );

    // Register mem0_search_memories tool
    this.mcpServer.registerTool(
      'mem0_search_memories',
      {
        title: '搜索记忆',
        description: '使用语义、图形、高级检索或混合策略搜索记忆。至少需要提供 user_id、agent_id 或 run_id 中的一个。支持自然语言查询，可以根据不同的搜索策略找到相关的历史记忆信息。',
        inputSchema: {
          query: z.string().describe('自然语言搜索查询'),
          user_id: z.string().optional().describe('要搜索的用户标识符（如果未提供 agent_id 和 run_id 则必需）'),
          agent_id: z.string().optional().describe('要搜索的代理标识符（如果未提供 user_id 和 run_id 则必需）'),
          run_id: z.string().optional().describe('要搜索的运行标识符（如果未提供 user_id 和 agent_id 则必需）'),
          filters: z.record(z.any()).optional().describe('高级过滤条件'),
          strategy: z.enum(['semantic', 'graph', 'advanced_retrieval', 'hybrid']).optional().describe('使用的搜索策略'),
          top_k: z.number().optional().describe('返回结果的最大数量'),
          threshold: z.number().optional().describe('最小相似度阈值')
        }
      },
      async ({ query, user_id, agent_id, run_id, filters, strategy, top_k, threshold }) => {
        // Get user context from AsyncLocalStorage if user_id not explicitly provided
        if (!user_id && !agent_id && !run_id) {
          const currentContext = Mem0McpServer.getCurrentUserContext();
          if (currentContext.userId) {
            console.log(`🎯 Auto-injecting user_id: ${currentContext.userId} from AsyncLocalStorage context`);
            user_id = currentContext.userId;
          }
        }
        
        const result = await this.mem0Tools.searchMemories({
          query,
          user_id,
          agent_id,
          run_id,
          filters,
          strategy,
          top_k,
          threshold
        });
        
        return {
          content: [{
            type: 'text' as const,
            text: JSON.stringify(result, null, 2)
          }]
        };
      }
    );

    // Register mem0_update_memory tool
    this.mcpServer.registerTool(
      'mem0_update_memory',
      {
        title: '更新记忆',
        description: '使用单个或批量策略更新现有记忆内容和元数据。可以修改记忆的文本内容、添加或更新元数据信息，支持批量操作提高效率。适用于记忆内容的维护和优化。',
        inputSchema: {
          memory_id: z.string().optional().describe('要更新的记忆ID'),
          text: z.string().optional().describe('新的记忆内容文本'),
          metadata: z.record(z.any()).optional().describe('更新的元数据'),
          batch_updates: z.array(z.object({
            memory_id: z.string(),
            text: z.string().optional(),
            metadata: z.record(z.any()).optional()
          })).optional().describe('批量更新操作')
        }
      },
      async ({ memory_id, text, metadata, batch_updates }) => {
        const result = await this.mem0Tools.updateMemory({
          memory_id,
          text,
          metadata,
          batch_updates
        });
        
        return {
          content: [{
            type: 'text' as const,
            text: JSON.stringify(result, null, 2)
          }]
        };
      }
    );

    // Register mem0_delete_memory tool
    this.mcpServer.registerTool(
      'mem0_delete_memory',
      {
        title: '删除记忆',
        description: '使用单个、批量或过滤策略删除记忆。可以删除特定ID的记忆、用户的所有记忆，或根据筛选条件删除符合要求的记忆。支持批量删除操作，谨慎使用避免误删重要记忆。',
        inputSchema: {
          memory_id: z.string().optional().describe('要删除的特定记忆ID'),
          user_id: z.string().optional().describe('删除用户的所有记忆'),
          filters: z.record(z.any()).optional().describe('删除的过滤条件'),
          batch_deletes: z.array(z.object({
            memory_id: z.string()
          })).optional().describe('批量删除操作')
        }
      },
      async ({ memory_id, user_id, filters, batch_deletes }) => {
        const result = await this.mem0Tools.deleteMemory({
          memory_id,
          user_id,
          filters,
          batch_deletes
        });
        
        return {
          content: [{
            type: 'text' as const,
            text: JSON.stringify(result, null, 2)
          }]
        };
      }
    );

    // Register mem0_selective_memory tool
    this.mcpServer.registerTool(
      'mem0_selective_memory',
      {
        title: '选择性记忆操作',
        description: '基于特定条件执行聚合的选择性记忆操作。根据配置的选择标准自动执行添加、搜索、更新或删除操作。这是一个高级功能，可以实现复杂的记忆管理逻辑和自动化处理。',
        inputSchema: {
          criteria: z.record(z.any()).describe('选择条件配置'),
          operation: z.enum(['add', 'search', 'update', 'delete']).describe('要执行的操作'),
          user_id: z.string().optional().describe('用户标识符')
        }
      },
      async ({ criteria, operation, user_id }) => {
        const result = await this.mem0Tools.selectiveMemory({
          criteria,
          operation,
          user_id
        });
        
        return {
          content: [{
            type: 'text' as const,
            text: JSON.stringify(result, null, 2)
          }]
        };
      }
    );

    // Register mem0_criteria_retrieval tool
    this.mcpServer.registerTool(
      'mem0_criteria_retrieval',
      {
        title: '条件检索记忆',
        description: '基于高级条件的记忆检索，支持多维度复杂条件查询。可以设置复杂的检索标准，实现精确的记忆过滤和定位。适用于需要精确匹配特定条件的记忆查找场景。',
        inputSchema: {
          criteria: z.record(z.any()).describe('多维度复杂条件的检索标准'),
          user_id: z.string().describe('搜索范围的用户标识符')
        }
      },
      async ({ criteria, user_id }) => {
        const result = await this.mem0Tools.criteriaRetrieval({
          criteria,
          user_id
        });
        
        return {
          content: [{
            type: 'text' as const,
            text: JSON.stringify(result, null, 2)
          }]
        };
      }
    );
  }

  async startStdio(): Promise<void> {
    const transport = new StdioServerTransport();
    await this.mcpServer.connect(transport);
    console.log('Mem0 MCP Server running on stdio');
  }

  async startHttp(host: string = '127.0.0.1', port: number = 8081): Promise<void> {
    const express = (await import('express')).default;
    const { randomUUID } = await import('node:crypto');
    const cors = (await import('cors')).default;
    
    const app = express();
    app.use(express.json());

    // CORS configuration following official SDK example
    app.use(cors({
      origin: '*',
      exposedHeaders: ['Mcp-Session-Id'],
      allowedHeaders: ['Content-Type', 'Accept', 'mcp-session-id'],
    }));

    // Request logging middleware
    app.use((req, res, next) => {
      console.log(`[${new Date().toISOString()}] ${req.method} ${req.path}`);
      if (config.server.devMode) {
        console.log('Headers:', JSON.stringify(req.headers, null, 2));
        if (req.body && Object.keys(req.body).length > 0) {
          console.log('Body:', JSON.stringify(req.body, null, 2));
        }
      }
      next();
    });

    // Map to store transports by session ID
    const transports: { [sessionId: string]: StreamableHTTPServerTransport } = {};

    // Helper function to get server instance for each request
    const getServer = () => {
      return this.mcpServer;
    };

    // Middleware to extract and validate user_id from path
    const extractUserContext = (req: any) => {
      const pathMatch = req.path.match(/^\/mcp(?:\/([^\/]+))?(?:\/.*)?$/);
      const userId = pathMatch?.[1] || req.headers['x-user-id'] as string || config.server.defaultUserId || undefined;
      
      // Validate user_id format (alphanumeric, underscore, hyphen, max 64 chars)
      if (userId && !/^[a-zA-Z0-9_-]{1,64}$/.test(userId)) {
        throw new Error(`Invalid user_id format: ${userId}. Must be alphanumeric with underscore/hyphen, max 64 characters.`);
      }
      
      return { 
        userId, 
        basePath: pathMatch?.[1] ? `/mcp/${pathMatch[1]}` : '/mcp',
        hasUserPath: !!pathMatch?.[1],
        source: pathMatch?.[1] ? 'path' : req.headers['x-user-id'] ? 'header' : config.server.defaultUserId ? 'config' : 'none'
      };
    };

    // MCP POST endpoint following official SDK pattern
    const mcpPostHandler = async (req: any, res: any) => {
      const sessionId = req.headers['mcp-session-id'] as string | undefined;
      
      try {
        console.log('📡 Received MCP request');
        
        // Extract user context from path or headers
        const { userId, basePath, hasUserPath, source } = extractUserContext(req);
        console.log(`👤 User context: ${userId || 'anonymous'} (source: ${source})`);
        
        // Debug: log request details
        if (config.server.devMode) {
          console.log('🔍 Request body:', JSON.stringify(req.body, null, 2));
          console.log('🔍 Request headers:', JSON.stringify(req.headers, null, 2));
          console.log('🔍 Base path:', basePath);
        }
        
        let transport: StreamableHTTPServerTransport;
        
        if (sessionId && transports[sessionId]) {
          // Reuse existing transport
          console.log(`♻️  Reusing existing session: ${sessionId}`);
          transport = transports[sessionId];
        } else if (!sessionId && isInitializeRequest(req.body)) {
          // New initialization request - follow official SDK pattern
          console.log('🆕 Creating new session for initialize request');
          
          transport = new StreamableHTTPServerTransport({
            sessionIdGenerator: () => randomUUID(),
            onsessioninitialized: (sessionId) => {
              console.log(`✅ Session initialized: ${sessionId} for user: ${userId || 'anonymous'}`);
              transports[sessionId] = transport;
              // Set session context for tools
              if (userId) {
                Mem0McpServer.setSessionContext(sessionId, { userId });
              }
            },
            enableDnsRebindingProtection: false
          });

          // Clean up transport when closed
          transport.onclose = () => {
            if (transport.sessionId) {
              console.log(`🧹 Cleaning up session: ${transport.sessionId}`);
              delete transports[transport.sessionId];
              Mem0McpServer.clearSessionContext(transport.sessionId);
            }
          };

          // Connect the transport to the MCP server BEFORE handling the request
          const server = getServer();
          await server.connect(transport);
          console.log('🔗 Transport connected to MCP server');
          
          // Handle the request and return early
          if (userId) {
            await Mem0McpServer.runWithUserContext({ userId }, async () => {
              await transport.handleRequest(req, res, req.body);
            });
          } else {
            await transport.handleRequest(req, res, req.body);
          }
          return;
        } else {
          // Invalid request - following official SDK error handling
          console.log(`❌ Invalid request - Session ID: ${sessionId}, Method: ${req.body?.method}`);
          res.status(400).json({
            jsonrpc: '2.0',
            error: {
              code: -32000,
              message: 'Bad Request: No valid session ID provided or not an initialize request',
            },
            id: req.body?.id || null,
          });
          return;
        }

        // Handle the request with existing transport
        console.log('🎯 Handling request via existing transport');
        if (userId) {
          await Mem0McpServer.runWithUserContext({ userId }, async () => {
            await transport.handleRequest(req, res, req.body);
          });
        } else {
          await transport.handleRequest(req, res, req.body);
        }
      } catch (error) {
        console.error('💥 Error handling MCP POST request:', error);
        if (!res.headersSent) {
          const errorMessage = error instanceof Error ? error.message : 'Internal server error';
          res.status(error instanceof Error && error.message.includes('Invalid user_id') ? 400 : 500).json({
            jsonrpc: '2.0',
            error: {
              code: error instanceof Error && error.message.includes('Invalid user_id') ? -32602 : -32603,
              message: errorMessage,
            },
            id: null,
          });
        }
      }
    };

    // Session handler for GET and DELETE requests - simplified following official SDK
    const handleSessionRequest = async (req: any, res: any) => {
      try {
        const sessionId = req.headers['mcp-session-id'] as string | undefined;
        console.log(`🔄 Session ${req.method} request for session: ${sessionId}`);
        
        if (!sessionId || !transports[sessionId]) {
          console.log('❌ Invalid or missing session ID');
          res.status(400).send('Invalid or missing session ID');
          return;
        }
        
        const transport = transports[sessionId];
        console.log('📨 Delegating to transport handler');
        await transport.handleRequest(req, res);
      } catch (error) {
        console.error(`💥 Error handling ${req.method} session request:`, error);
        if (!res.headersSent) {
          res.status(500).send('Internal server error');
        }
      }
    };

    // Register routes for both /mcp and /mcp/{user_id} formats following official SDK pattern
    app.post('/mcp', mcpPostHandler);
    app.post('/mcp/:user_id', mcpPostHandler);
    
    app.get('/mcp', handleSessionRequest);
    app.get('/mcp/:user_id', handleSessionRequest);
    
    app.delete('/mcp', handleSessionRequest);
    app.delete('/mcp/:user_id', handleSessionRequest);

    // Health check endpoint
    app.get('/health', (req, res) => {
      res.json({ 
        status: 'healthy', 
        server: 'mem0-mcp-server',
        version: '2.0.0',
        timestamp: new Date().toISOString(),
        activeSessions: Object.keys(transports).length,
        mem0Api: {
          url: config.mem0.apiUrl,
          connected: true
        }
      });
    });

    // 404 handler
    app.use((req, res) => {
      console.log(`❓ 404 - Path not found: ${req.method} ${req.path}`);
      res.status(404).json({
        error: 'Not Found',
        message: `Path ${req.path} not found. Available endpoints: POST/GET/DELETE /mcp, GET /health`
      });
    });

    app.listen(port, host, () => {
      console.log(`🚀 Mem0 MCP Server running on HTTP ${host}:${port}`);
      console.log(`📍 Available endpoints:`);
      console.log(`   POST/GET/DELETE http://${host}:${port}/mcp`);
      console.log(`   GET http://${host}:${port}/health`);
      console.log(`🔧 CORS enabled for all origins`);
      console.log(`🧠 Mem0 API: ${config.mem0.apiUrl}`);
      console.log(`⚡ Ready to manage memories...`);
    });
  }
}

// CLI entry point
if (import.meta.url === `file://${process.argv[1]}`) {
  const server = new Mem0McpServer();
  
  if (process.argv.includes('--http')) {
    const hostIndex = process.argv.indexOf('--host');
    const portIndex = process.argv.indexOf('--port');
    
    const host = hostIndex !== -1 ? process.argv[hostIndex + 1] : '127.0.0.1';
    const port = portIndex !== -1 ? parseInt(process.argv[portIndex + 1]) : 8081;
    
    await server.startHttp(host, port);
  } else {
    await server.startStdio();
  }
}