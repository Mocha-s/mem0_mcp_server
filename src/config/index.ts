/**
 * Configuration for Mem0 MCP Server
 */

import * as dotenv from 'dotenv';

// Load environment variables
dotenv.config();

export const config = {
  // Mem0 API configuration
  mem0: {
    apiKey: process.env.MEM0_API_KEY || '',
    apiUrl: process.env.MEM0_API_URL || 'http://localhost:8000',
    orgId: process.env.MEM0_ORG_ID,
    projectId: process.env.MEM0_PROJECT_ID,
    timeout: parseInt(process.env.MEM0_TIMEOUT || '30000', 10),
    maxRetries: parseInt(process.env.MEM0_MAX_RETRIES || '3', 10)
  },
  
  // MCP Server configuration
  server: {
    name: 'mem0-mcp-server',
    version: '2.0.0',
    host: process.env.MCP_HOST || '127.0.0.1',
    port: parseInt(process.env.MCP_PORT || '8080', 10),
    sessionTimeout: parseInt(process.env.MCP_SESSION_TIMEOUT || '3600', 10),
    corsOrigins: process.env.MCP_CORS_ORIGINS || '*',
    devMode: process.env.MCP_DEV_MODE === 'true'
  },
  
  // Logging configuration
  logging: {
    level: process.env.LOG_LEVEL || 'info',
    format: process.env.LOG_FORMAT || 'json'
  }
};