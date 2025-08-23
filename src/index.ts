/**
 * Mem0 MCP Server with TypeScript SDK Integration
 * Implements all 6 memory management services
 */

import { McpServer } from '@modelcontextprotocol/sdk/server/mcp.js';
import { StdioServerTransport } from '@modelcontextprotocol/sdk/server/stdio.js';
import { StreamableHTTPServerTransport } from '@modelcontextprotocol/sdk/server/streamableHttp.js';
import { z } from 'zod';
import { config } from './config/index.js';
import { Mem0Tools } from './tools/index.js';

export class Mem0McpServer {
  private mcpServer: McpServer;
  private mem0Tools: Mem0Tools;

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

    // Initialize Mem0 tools
    this.mem0Tools = new Mem0Tools();
    
    this.registerTools();
  }

  private registerTools(): void {
    // Register mem0_add_memory tool
    this.mcpServer.registerTool(
      'mem0_add_memory',
      {
        title: '添加记忆',
        description: '从对话消息中添加新记忆，支持上下文、图形和多模态策略。至少需要提供 user_id、agent_id 或 run_id 中的一个。可以从用户与助手的对话中自动提取并存储重要信息，建立持久的记忆存储。',
        inputSchema: {
          messages: z.array(z.object({
            role: z.enum(['user', 'assistant']),
            content: z.string()
          })).describe('Conversation messages to extract memory from'),
          user_id: z.string().optional().describe('Unique user identifier (required if agent_id and run_id not provided)'),
          agent_id: z.string().optional().describe('Unique agent identifier (required if user_id and run_id not provided)'),
          run_id: z.string().optional().describe('Unique run identifier (required if user_id and agent_id not provided)'),
          enable_graph: z.boolean().optional().describe('Enable graph relationship memory'),
          metadata: z.record(z.any()).optional().describe('Additional metadata'),
          infer: z.boolean().optional().describe('Enable automatic fact inference')
        }
      },
      async ({ messages, user_id, agent_id, run_id, enable_graph, metadata, infer }) => {
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
          query: z.string().describe('Natural language search query'),
          user_id: z.string().optional().describe('User identifier to search within (required if agent_id and run_id not provided)'),
          agent_id: z.string().optional().describe('Agent identifier to search within (required if user_id and run_id not provided)'),
          run_id: z.string().optional().describe('Run identifier to search within (required if user_id and agent_id not provided)'),
          filters: z.record(z.any()).optional().describe('Advanced filter conditions'),
          strategy: z.enum(['semantic', 'graph', 'advanced_retrieval', 'hybrid']).optional().describe('Search strategy to use'),
          top_k: z.number().optional().describe('Maximum number of results'),
          threshold: z.number().optional().describe('Minimum similarity threshold')
        }
      },
      async ({ query, user_id, agent_id, run_id, filters, strategy, top_k, threshold }) => {
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
          memory_id: z.string().optional().describe('Memory ID to update'),
          text: z.string().optional().describe('New memory content text'),
          metadata: z.record(z.any()).optional().describe('Updated metadata'),
          batch_updates: z.array(z.object({
            memory_id: z.string(),
            text: z.string().optional(),
            metadata: z.record(z.any()).optional()
          })).optional().describe('Batch update operations')
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
          memory_id: z.string().optional().describe('Specific memory ID to delete'),
          user_id: z.string().optional().describe('Delete all memories for user'),
          filters: z.record(z.any()).optional().describe('Filter conditions for deletion'),
          batch_deletes: z.array(z.object({
            memory_id: z.string()
          })).optional().describe('Batch delete operations')
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
          criteria: z.record(z.any()).describe('Selection criteria configuration'),
          operation: z.enum(['add', 'search', 'update', 'delete']).describe('Operation to perform'),
          user_id: z.string().optional().describe('User identifier')
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
          criteria: z.record(z.any()).describe('Complex retrieval criteria with multi-dimensional conditions'),
          user_id: z.string().describe('User identifier for search scope')
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

  async startHttp(host: string = '127.0.0.1', port: number = 8080): Promise<void> {
    const express = (await import('express')).default;
    const { randomUUID } = await import('node:crypto');
    
    const app = express();
    app.use(express.json());

    // Add CORS middleware
    app.use((req, res, next) => {
      res.header('Access-Control-Allow-Origin', '*');
      res.header('Access-Control-Allow-Methods', 'GET, POST, DELETE, OPTIONS');
      res.header('Access-Control-Allow-Headers', 'Content-Type, Accept, mcp-session-id');
      res.header('Access-Control-Expose-Headers', 'mcp-session-id');
      
      if (req.method === 'OPTIONS') {
        res.sendStatus(200);
        return;
      }
      next();
    });

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

    // Handle POST requests for client-to-server communication
    app.post('/mcp', async (req, res) => {
      try {
        console.log('📡 Received MCP request');
        
        // Debug: log request details
        if (config.server.devMode) {
          console.log('🔍 Request body:', JSON.stringify(req.body, null, 2));
          console.log('🔍 Request headers:', JSON.stringify(req.headers, null, 2));
        }
        
        // Check for existing session ID
        const sessionId = req.headers['mcp-session-id'] as string | undefined;
        let transport: StreamableHTTPServerTransport;

        console.log(`🔍 Session ID: ${sessionId || 'none'}`);
        console.log(`🔍 Request method: ${req.body?.method || 'none'}`);

        if (sessionId && transports[sessionId]) {
          console.log(`♻️  Reusing existing session: ${sessionId}`);
          transport = transports[sessionId];
        } else if (sessionId && !transports[sessionId]) {
          console.log(`🔄 Session ${sessionId} not found, creating new transport`);
          // Session ID exists but transport was lost (server restart), recreate transport
          transport = new StreamableHTTPServerTransport({
            sessionIdGenerator: () => sessionId, // Reuse existing session ID
            onsessioninitialized: (newSessionId) => {
              console.log(`♻️  Session recreated: ${newSessionId}`);
              transports[newSessionId] = transport;
            },
            enableDnsRebindingProtection: false
          });

          // Clean up transport when closed
          transport.onclose = () => {
            if (transport.sessionId) {
              console.log(`🧹 Cleaning up session: ${transport.sessionId}`);
              delete transports[transport.sessionId];
            }
          };

          await this.mcpServer.connect(transport);
          console.log('🔗 Transport reconnected to MCP server');
        } else if (!sessionId && this.isInitializeRequest(req.body)) {
          console.log('🆕 Creating new session for initialize request');
          
          transport = new StreamableHTTPServerTransport({
            sessionIdGenerator: () => randomUUID(),
            onsessioninitialized: (sessionId) => {
              console.log(`✅ Session initialized: ${sessionId}`);
              transports[sessionId] = transport;
            },
            enableDnsRebindingProtection: false  // Allow all connections including LAN
          });

          // Clean up transport when closed
          transport.onclose = () => {
            if (transport.sessionId) {
              console.log(`🧹 Cleaning up session: ${transport.sessionId}`);
              delete transports[transport.sessionId];
            }
          };

          // Connect to the MCP server
          await this.mcpServer.connect(transport);
          console.log('🔗 Transport connected to MCP server');
        } else {
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

        console.log('🎯 Handling request via transport');
        await transport.handleRequest(req, res, req.body);
      } catch (error) {
        console.error('💥 Error handling MCP POST request:', error);
        if (!res.headersSent) {
          res.status(500).json({
            jsonrpc: '2.0',
            error: {
              code: -32603,
              message: 'Internal server error',
            },
            id: null,
          });
        }
      }
    });

    // Reusable handler for GET and DELETE requests
    const handleSessionRequest = async (req: any, res: any) => {
      try {
        const sessionId = req.headers['mcp-session-id'] as string | undefined;
        console.log(`🔄 Session request - Method: ${req.method}, Session: ${sessionId}`);
        
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

    // Handle GET requests for server-to-client notifications via SSE
    app.get('/mcp', handleSessionRequest);

    // Handle DELETE requests for session termination
    app.delete('/mcp', handleSessionRequest);

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

  private isInitializeRequest(body: any): boolean {
    return body?.method === 'initialize';
  }
}

// CLI entry point
if (import.meta.url === `file://${process.argv[1]}`) {
  const server = new Mem0McpServer();
  
  if (process.argv.includes('--http')) {
    const hostIndex = process.argv.indexOf('--host');
    const portIndex = process.argv.indexOf('--port');
    
    const host = hostIndex !== -1 ? process.argv[hostIndex + 1] : '127.0.0.1';
    const port = portIndex !== -1 ? parseInt(process.argv[portIndex + 1]) : 8080;
    
    await server.startHttp(host, port);
  } else {
    await server.startStdio();
  }
}