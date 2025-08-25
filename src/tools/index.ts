/**
 * Mem0 MCP Tools Implementation
 * Provides all memory management tools for MCP server
 */

import { 
  Mem0ApiClient,
  AddMemoryRequest,
  SearchMemoriesRequest,
  UpdateMemoryRequest,
  DeleteMemoryRequest,
  Memory
} from '../client/mem0-api.js';

export interface ToolResult {
  status: string;
  message: string;
  data?: any;
  error?: string;
}

export class Mem0Tools {
  private client: Mem0ApiClient;
  private sessionContexts: Map<string, { userId?: string }>;
  
  // Current session ID for context lookup
  private currentSessionId?: string;

  constructor(sessionContexts?: Map<string, { userId?: string }>) {
    this.client = new Mem0ApiClient();
    this.sessionContexts = sessionContexts || new Map();
  }

  /**
   * Set the current session ID for context lookup
   */
  setCurrentSession(sessionId: string): void {
    this.currentSessionId = sessionId;
  }

  /**
   * Get user context from current session
   */
  private getCurrentUserContext(): { userId?: string } {
    if (!this.currentSessionId) {
      return {};
    }
    return this.sessionContexts.get(this.currentSessionId) || {};
  }

  /**
   * Validate that at least one identifier is provided, with auto-injection from session
   */
  private validateIdentifiers(params: { user_id?: string; agent_id?: string; run_id?: string }): { params: typeof params; error?: string } {
    // Try to inject user_id from session context if none provided
    if (!params.user_id && !params.agent_id && !params.run_id) {
      const sessionContext = this.getCurrentUserContext();
      if (sessionContext.userId) {
        console.log(`🎯 Auto-injecting user_id: ${sessionContext.userId} from session context`);
        params = { ...params, user_id: sessionContext.userId };
      }
    }

    if (!params.user_id && !params.agent_id && !params.run_id) {
      return {
        params,
        error: "At least one of 'user_id', 'agent_id', or 'run_id' must be provided."
      };
    }
    
    return { params };
  }

  /**
   * Add a new memory from conversation messages
   */
  async addMemory(params: {
    messages: Array<{ role: 'user' | 'assistant'; content: string }>;
    user_id?: string;
    agent_id?: string;
    run_id?: string;
    enable_graph?: boolean;
    metadata?: Record<string, any>;
    infer?: boolean;
  }): Promise<ToolResult> {
    try {
      // Validate and potentially inject user context
      const validation = this.validateIdentifiers(params);
      if (validation.error) {
        return {
          status: 'error',
          message: 'Failed to add memory',
          error: validation.error
        };
      }

      const validatedParams = validation.params;
      const request: AddMemoryRequest = {
        messages: params.messages,
        enable_graph: params.enable_graph,
        metadata: params.metadata,
        infer: params.infer !== false // Default true
      };

      // Only add identifiers if they are provided (avoid undefined values)
      if (validatedParams.user_id) {
        request.user_id = validatedParams.user_id;
      }
      if (validatedParams.agent_id) {
        request.agent_id = validatedParams.agent_id;
      }
      if (validatedParams.run_id) {
        request.run_id = validatedParams.run_id;
      }

      const response = await this.client.addMemory(request);
      
      return {
        status: 'success',
        message: response.message,
        data: {
          memory_id: response.memory_id,
          extracted_facts: response.extracted_facts
        }
      };
    } catch (error) {
      return {
        status: 'error',
        message: 'Failed to add memory',
        error: error instanceof Error ? error.message : String(error)
      };
    }
  }

  /**
   * Search memories based on query and filters
   */
  async searchMemories(params: {
    query: string;
    user_id?: string;
    agent_id?: string;
    run_id?: string;
    filters?: Record<string, any>;
    strategy?: 'semantic' | 'graph' | 'advanced_retrieval' | 'hybrid';
    top_k?: number;
    threshold?: number;
  }): Promise<ToolResult> {
    try {
      // Validate that at least one identifier is provided (SOLID principle - input validation)
      const validation = this.validateIdentifiers(params);
      if (validation.error) {
        return {
          status: 'error',
          message: 'Failed to search memories',
          error: validation.error
        };
      }

      const validatedParams = validation.params;

      const request: SearchMemoriesRequest = {
        query: params.query,
        filters: params.filters,
        top_k: params.top_k || 10,
        threshold: params.threshold || 0.7
      };

      // Only add identifiers if they are provided (avoid undefined values)
      if (validatedParams.user_id) {
        request.user_id = validatedParams.user_id;
      }
      if (validatedParams.agent_id) {
        request.agent_id = validatedParams.agent_id;
      }
      if (validatedParams.run_id) {
        request.run_id = validatedParams.run_id;
      }

      const response = await this.client.searchMemories(request);
      
      return {
        status: 'success',
        message: `Found ${response.total_count} memories`,
        data: {
          memories: response.memories,
          total_count: response.total_count
        }
      };
    } catch (error) {
      return {
        status: 'error',
        message: 'Failed to search memories',
        error: error instanceof Error ? error.message : String(error)
      };
    }
  }

  /**
   * Update an existing memory
   */
  async updateMemory(params: {
    memory_id?: string;
    text?: string;
    metadata?: Record<string, any>;
    batch_updates?: Array<{
      memory_id: string;
      text?: string;
      metadata?: Record<string, any>;
    }>;
  }): Promise<ToolResult> {
    try {
      let updatedCount = 0;

      if (params.batch_updates && params.batch_updates.length > 0) {
        // Batch update mode
        for (const update of params.batch_updates) {
          try {
            await this.client.updateMemory({
              memory_id: update.memory_id,
              text: update.text,
              metadata: update.metadata
            });
            updatedCount++;
          } catch (error) {
            console.error(`Failed to update memory ${update.memory_id}:`, error);
          }
        }
      } else if (params.memory_id) {
        // Single update mode
        await this.client.updateMemory({
          memory_id: params.memory_id,
          text: params.text,
          metadata: params.metadata
        });
        updatedCount = 1;
      } else {
        throw new Error('Either memory_id or batch_updates must be provided');
      }

      return {
        status: 'success',
        message: `Updated ${updatedCount} memory(ies)`,
        data: {
          updated_count: updatedCount
        }
      };
    } catch (error) {
      return {
        status: 'error',
        message: 'Failed to update memory',
        error: error instanceof Error ? error.message : String(error)
      };
    }
  }

  /**
   * Delete memories
   */
  async deleteMemory(params: {
    memory_id?: string;
    user_id?: string;
    filters?: Record<string, any>;
    batch_deletes?: Array<{ memory_id: string }>;
  }): Promise<ToolResult> {
    try {
      let deletedCount = 0;

      if (params.batch_deletes && params.batch_deletes.length > 0) {
        // Batch delete mode
        for (const item of params.batch_deletes) {
          try {
            await this.client.deleteMemory({ memory_id: item.memory_id });
            deletedCount++;
          } catch (error) {
            console.error(`Failed to delete memory ${item.memory_id}:`, error);
          }
        }
      } else {
        // Single or filtered delete
        const response = await this.client.deleteMemory({
          memory_id: params.memory_id,
          user_id: params.user_id,
          filters: params.filters
        });
        deletedCount = response.deleted_count;
      }

      return {
        status: 'success',
        message: `Deleted ${deletedCount} memory(ies)`,
        data: {
          deleted_count: deletedCount
        }
      };
    } catch (error) {
      return {
        status: 'error',
        message: 'Failed to delete memory',
        error: error instanceof Error ? error.message : String(error)
      };
    }
  }

  /**
   * Selective memory operation based on criteria
   */
  async selectiveMemory(params: {
    criteria: Record<string, any>;
    operation: 'add' | 'search' | 'update' | 'delete';
    user_id?: string;
  }): Promise<ToolResult> {
    try {
      switch (params.operation) {
        case 'search':
          return await this.searchMemories({
            query: params.criteria.query || '*',
            user_id: params.user_id || params.criteria.user_id || '',
            filters: params.criteria.filters
          });
        
        case 'delete':
          return await this.deleteMemory({
            user_id: params.user_id || params.criteria.user_id,
            filters: params.criteria.filters
          });
        
        default:
          return {
            status: 'error',
            message: `Operation ${params.operation} not fully implemented in selective memory`,
            error: 'Not implemented'
          };
      }
    } catch (error) {
      return {
        status: 'error',
        message: 'Failed to perform selective memory operation',
        error: error instanceof Error ? error.message : String(error)
      };
    }
  }

  /**
   * Advanced criteria-based retrieval
   */
  async criteriaRetrieval(params: {
    criteria: Record<string, any>;
    user_id: string;
  }): Promise<ToolResult> {
    try {
      // Build complex query from criteria
      const query = this.buildQueryFromCriteria(params.criteria);
      
      const response = await this.client.searchMemories({
        query: query,
        user_id: params.user_id,
        filters: params.criteria.filters
      });

      // Calculate match scores
      const memoriesWithScores = response.memories.map(memory => ({
        ...memory,
        criteria_match_score: this.calculateMatchScore(memory, params.criteria)
      }));

      // Sort by match score
      memoriesWithScores.sort((a, b) => b.criteria_match_score - a.criteria_match_score);

      return {
        status: 'success',
        message: `Retrieved ${memoriesWithScores.length} memories matching criteria`,
        data: {
          memories: memoriesWithScores,
          total_count: memoriesWithScores.length
        }
      };
    } catch (error) {
      return {
        status: 'error',
        message: 'Failed to perform criteria retrieval',
        error: error instanceof Error ? error.message : String(error)
      };
    }
  }

  private buildQueryFromCriteria(criteria: Record<string, any>): string {
    // Build query from AND/OR conditions
    if (criteria.AND) {
      return criteria.AND.map((c: any) => this.buildQueryFromCriteria(c)).join(' AND ');
    }
    if (criteria.OR) {
      return criteria.OR.map((c: any) => this.buildQueryFromCriteria(c)).join(' OR ');
    }
    
    // Extract text-based criteria
    const textParts: string[] = [];
    for (const [key, value] of Object.entries(criteria)) {
      if (typeof value === 'string') {
        textParts.push(value);
      } else if (Array.isArray(value)) {
        textParts.push(...value.filter(v => typeof v === 'string'));
      }
    }
    
    return textParts.join(' ') || '*';
  }

  private calculateMatchScore(memory: Memory, criteria: Record<string, any>): number {
    let score = memory.score || 0.5;
    
    // Boost score based on metadata matches
    if (memory.metadata && criteria.categories) {
      const categories = Array.isArray(criteria.categories) ? criteria.categories : [criteria.categories];
      if (categories.some(cat => memory.metadata?.category === cat)) {
        score += 0.2;
      }
    }
    
    if (memory.metadata && criteria.priority) {
      if (memory.metadata.priority >= criteria.priority) {
        score += 0.1;
      }
    }
    
    return Math.min(score, 1.0);
  }
}