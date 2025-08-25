# CLAUDE.md

This file provides guidance to Claude Code (claude.ai/code) when working with code in this repository.

## Project Overview

This is a **Mem0 MCP Server** - a TypeScript implementation of a Model Context Protocol (MCP) server that provides intelligent memory management services. The server acts as a bridge between MCP clients (like Claude Desktop) and the Mem0 API, offering 6 core memory management tools with support for both stdio and HTTP transport modes.

## Architecture

### High-Level Structure
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

### Key Components
- **Main Entry Point** (`src/index.ts`): Handles MCP server setup, tool registration, and transport layer management
- **Configuration** (`src/config/index.ts`): Centralized config management using environment variables
- **Mem0 API Client** (`src/client/mem0-api.ts`): HTTP client for Mem0 API with retry logic and proper error handling
- **Memory Tools** (`src/tools/index.ts`): Implementation of 6 memory management tools with validation and batch operations

## Development Commands

### Essential Commands
- `npm run build` - Compile TypeScript to JavaScript
- `npm run dev` - Development mode with auto-reload using tsx
- `npm start` - Run in stdio mode
- `npm run start:http` - Run HTTP server on localhost
- `npm run start:http:external` - Run HTTP server accessible from network (0.0.0.0:8081)

### Code Quality
- `npm run lint` - Run ESLint (configured in package.json)
- `npm run format` - Format code with Prettier
- `npm test` - Run Jest tests

### Testing and Debugging
- Test the server: `node test-mcp.js` (basic MCP protocol test)
- Direct API test: `node direct_api_test.js` (Mem0 API connectivity test)
- Health check: `curl http://localhost:8081/health`
- Development mode enables verbose logging: Set `MCP_DEV_MODE=true`
- Check port availability: `lsof -i :8081` (kill existing processes if needed)

## Environment Configuration

Essential environment variables (see `.env.example`):
- `MEM0_API_URL` - Mem0 API server endpoint (default: http://localhost:8000)
- `MEM0_API_KEY` - Optional API authentication
- `MCP_PORT` - Server port (default: 8081, configurable via env var)
- `MCP_HOST` - Binding address (127.0.0.1 for local, 0.0.0.0 for network)
- `MCP_DEV_MODE` - Enable debug logging

## Memory Tools Implementation

The server provides 6 memory management tools, each requiring at least one identifier (user_id, agent_id, or run_id):

1. **mem0_add_memory** - Extract and store memories from conversations
2. **mem0_search_memories** - Search with semantic, graph, advanced_retrieval, or hybrid strategies  
3. **mem0_update_memory** - Update memories (single or batch operations)
4. **mem0_delete_memory** - Delete memories (single, batch, or filtered)
5. **mem0_selective_memory** - Conditional aggregate operations
6. **mem0_criteria_retrieval** - Advanced multi-dimensional retrieval

### API Version Handling
- Uses Mem0 v1 API for add/update/delete operations
- Uses Mem0 v2 API for search operations with proper filter structure
- Handles identifier validation consistently across all tools

## Transport Modes

### HTTP Mode (Recommended)
- Supports session management with proper cleanup
- CORS enabled for cross-origin requests  
- Health check endpoint at `/health`
- Handles POST/GET/DELETE requests at `/mcp`

### stdio Mode
- Direct stdin/stdout communication
- Suitable for command-line MCP clients

## User Context Auto-Injection

### Path-Based User Context
- **Route Pattern**: `/mcp/{user_id}` automatically extracts user_id from URL path
- **Priority System**: Explicit `user_id` parameters override path-based extraction
- **AsyncLocalStorage**: Thread-safe user context management across async operations
- **Session Management**: User context preserved per session with automatic cleanup

### Context Flow
```
Request: POST /mcp/alice → AsyncLocalStorage stores {userId: "alice"}
Tool Call: mem0_search_memories({query: "travel"}) → Auto-injects user_id: "alice"
```

## Key Technical Patterns

### Error Handling
- All API calls wrapped in try-catch with structured error responses
- Exponential backoff retry logic in Mem0ApiClient (`src/client/mem0-api.ts`)
- Proper HTTP status codes and JSON-RPC error formats

### Type Safety
- Full TypeScript implementation with strict mode
- Zod schemas for runtime input validation
- Comprehensive interface definitions for Mem0 API types

### Code Quality Principles
- DRY: Centralized user context injection (`applyUserContextOverride` method)
- SOLID: Single responsibility classes, dependency injection
- KISS: Simple, focused tool implementations  
- Clean separation between transport, API client, and business logic

## Deployment

### Docker
- Multi-stage build with Node.js 18 Alpine
- Non-root user for security
- Health checks configured
- Default HTTP mode on port 8081

### Production Considerations
- Set `MCP_CORS_ORIGINS` to specific domains (not *)
- Configure proper `MEM0_API_KEY` if authentication required
- Use PM2 or systemd for process management
- Monitor via health check endpoint

## Testing

- Health endpoint provides server status and Mem0 API connectivity
- `test-mcp.js` script for basic MCP protocol validation
- `direct_api_test.js` script for direct Mem0 API testing
- Jest configured for unit tests (run with `npm test`)

## File Structure

Key files for development:
- `src/index.ts` - Main server implementation with HTTP/stdio transport modes
- `src/tools/index.ts` - All 6 memory management tools implementation  
- `src/client/mem0-api.ts` - Mem0 API client with retry logic and error handling
- `src/config/index.ts` - Environment configuration management
- `tsconfig.json` - TypeScript compiler settings with ES2022 target
- `.env.example` - Environment variables template