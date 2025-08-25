/**
 * Mem0 API Client
 * Handles all communication with Mem0 API server
 */

import fetch from 'node-fetch';
import { config } from '../config/index.js';

// Type definitions for Mem0 API
export interface Memory {
  id: string;
  text: string;
  user_id?: string;
  metadata?: Record<string, any>;
  created_at?: string;
  updated_at?: string;
  score?: number;
}

export interface AddMemoryRequest {
  messages: Array<{
    role: 'user' | 'assistant';
    content: string;
  }>;
  user_id?: string;
  agent_id?: string;
  run_id?: string;
  enable_graph?: boolean;
  metadata?: Record<string, any>;
  infer?: boolean;
  version?: 'v1' | 'v2';
}

export interface AddMemoryResponse {
  status: string;
  message: string;
  memory_id?: string;
  extracted_facts?: string[];
}

export interface SearchMemoriesRequest {
  query: string;
  user_id?: string;
  agent_id?: string;
  run_id?: string;
  filters?: Record<string, any>;
  strategy?: 'semantic' | 'graph' | 'advanced_retrieval' | 'hybrid';
  top_k?: number;
  threshold?: number;
  rerank?: boolean;
  keyword_search?: boolean;
  filter_memories?: boolean;
}

export interface SearchMemoriesResponse {
  memories: Memory[];
  total_count: number;
}

export interface UpdateMemoryRequest {
  memory_id: string;
  text?: string;
  metadata?: Record<string, any>;
}

export interface DeleteMemoryRequest {
  memory_id?: string;
  user_id?: string;
  agent_id?: string;
  app_id?: string;
  run_id?: string;
  metadata?: Record<string, any>;
  org_id?: string;
  project_id?: string;
  filters?: Record<string, any>; // For backward compatibility
}

export class Mem0ApiClient {
  private baseUrl: string;
  private apiKey: string;
  private timeout: number;
  private maxRetries: number;

  constructor() {
    this.baseUrl = config.mem0.apiUrl;
    this.apiKey = config.mem0.apiKey;
    this.timeout = config.mem0.timeout;
    this.maxRetries = config.mem0.maxRetries;
  }

  private async request<T>(
    endpoint: string,
    options: RequestInit = {}
  ): Promise<T> {
    const url = `${this.baseUrl}${endpoint}`;
    
    const headers: Record<string, string> = {
      'Content-Type': 'application/json',
      ...((options.headers as Record<string, string>) || {})
    };

    if (this.apiKey) {
      headers['Authorization'] = `Token ${this.apiKey}`;
    }

    let lastError: Error | null = null;
    
    for (let retry = 0; retry <= this.maxRetries; retry++) {
      try {
        const controller = new AbortController();
        const timeoutId = setTimeout(() => controller.abort(), this.timeout);

        const response = await fetch(url, {
          method: options.method,
          headers,
          body: options.body as any,
          signal: controller.signal
        });

        clearTimeout(timeoutId);

        if (!response.ok) {
          const errorText = await response.text();
          throw new Error(`Mem0 API error (${response.status}): ${errorText}`);
        }

        return response.json() as Promise<T>;
      } catch (error) {
        lastError = error as Error;
        if (retry < this.maxRetries) {
          // Exponential backoff
          await new Promise(resolve => setTimeout(resolve, Math.pow(2, retry) * 1000));
        }
      }
    }

    throw lastError || new Error('Failed to connect to Mem0 API');
  }

  async addMemory(request: AddMemoryRequest): Promise<AddMemoryResponse> {
    // Use v1 API for adding memories (POST method) - ensure at least one identifier
    const payload: any = {
      messages: request.messages,
      metadata: request.metadata || {},
      infer: request.infer !== false, // Default true
      ...(request.enable_graph && { enable_graph: request.enable_graph })
    };

    // Add version parameter for Contextual Add functionality
    if (request.version) {
      payload.version = request.version;
    }

    // Ensure at least one identifier is provided for v1 API as well
    const hasIdentifier = request.user_id || request.agent_id || request.run_id;
    if (!hasIdentifier) {
      throw new Error("At least one of 'user_id', 'agent_id', or 'run_id' must be provided.");
    }

    // Only add identifiers if they are provided (avoid undefined values)
    if (request.user_id) {
      payload.user_id = request.user_id;
    }
    if (request.agent_id) {
      payload.agent_id = request.agent_id;
    }
    if (request.run_id) {
      payload.run_id = request.run_id;
    }

    // Add org_id and project_id if configured
    if (config.mem0.orgId) {
      payload.org_id = config.mem0.orgId;
    }
    if (config.mem0.projectId) {
      payload.project_id = config.mem0.projectId;
    }

    const response = await this.request<any>('/v1/memories/', {
      method: 'POST',
      body: JSON.stringify(payload)
    });

    // Handle different response formats from Mem0 v1 API
    let memoryId: string = '';
    let extractedFacts: string[] = [];
    
    if (Array.isArray(response)) {
      // Array response format - extract from first element
      if (response.length > 0) {
        const firstItem = response[0];
        memoryId = firstItem.id || firstItem.memory_id || '';
        extractedFacts = firstItem.extracted_facts || [];
      } else {
        // Empty array response - memory was likely created but API doesn't return details
        // Generate a placeholder memory_id for client consistency
        memoryId = `mem0_${Date.now()}_${Math.random().toString(36).substring(2, 8)}`;
      }
    } else if (response && typeof response === 'object') {
      // Object response format
      memoryId = response.id || response.memory_id || '';
      extractedFacts = response.extracted_facts || [];
    }

    return {
      status: 'success',
      message: 'Memory added successfully',
      memory_id: memoryId,
      extracted_facts: extractedFacts
    };
  }

  async searchMemories(request: SearchMemoriesRequest): Promise<SearchMemoriesResponse> {
    // Use v2 API for search with POST method - identifiers must be in filters
    const filters: any = { ...request.filters };
    
    // Build filters object with required identifiers (at least one must be provided)
    const hasIdentifier = request.user_id || request.agent_id || request.run_id;
    if (!hasIdentifier) {
      throw new Error("At least one of 'user_id', 'agent_id', or 'run_id' must be provided.");
    }

    // Add identifiers to filters object as required by v2 API
    if (request.user_id) {
      filters.user_id = request.user_id;
    }
    if (request.agent_id) {
      filters.agent_id = request.agent_id; 
    }
    if (request.run_id) {
      filters.run_id = request.run_id;
    }

    const payload: any = {
      query: request.query,
      filters: filters,
      top_k: request.top_k || 10,
      threshold: request.threshold || 0.3,
      rerank: request.rerank !== undefined ? request.rerank : false,
      keyword_search: request.keyword_search !== undefined ? request.keyword_search : false,
      filter_memories: request.filter_memories !== undefined ? request.filter_memories : false
    };

    // Add strategy if provided
    if (request.strategy) {
      payload.strategy = request.strategy;
    }

    // Add org_id and project_id if configured
    if (config.mem0.orgId) {
      payload.org_id = config.mem0.orgId;
    }
    if (config.mem0.projectId) {
      payload.project_id = config.mem0.projectId;
    }

    const response = await this.request<any>('/v2/memories/search/', {
      method: 'POST',
      body: JSON.stringify(payload)
    });

    // Handle different response formats from Mem0 v2 API
    let memories: any[] = [];
    
    if (Array.isArray(response)) {
      // Direct array response
      memories = response;
    } else if (response && response.results && Array.isArray(response.results.results)) {
      // Nested object with results.results array (new v2 API format)
      memories = response.results.results;
    } else if (response && Array.isArray(response.results)) {
      // Object with results array (common v2 API format)
      memories = response.results;
    } else if (response && Array.isArray(response.memories)) {
      // Object with memories array (alternative format)
      memories = response.memories;
    } else if (response && Array.isArray(response.data)) {
      // Object with data array (another possible format)
      memories = response.data;
    } else {
      // Fallback to empty array for unexpected formats
      console.warn('Unexpected Mem0 search response format:', response);
      memories = [];
    }

    return {
      memories: memories,
      total_count: memories.length
    };
  }

  async updateMemory(request: UpdateMemoryRequest): Promise<{ status: string; message: string }> {
    await this.request(`/v1/memories/${request.memory_id}/`, {
      method: 'PUT',
      body: JSON.stringify({
        text: request.text,
        metadata: request.metadata
      })
    });

    return {
      status: 'success',
      message: 'Memory updated successfully'
    };
  }

  async deleteMemory(request: DeleteMemoryRequest): Promise<{ status: string; message: string; deleted_count: number }> {
    if (request.memory_id) {
      // Delete single memory by ID
      await this.request(`/v1/memories/${request.memory_id}/`, {
        method: 'DELETE'
      });
      
      return {
        status: 'success',
        message: 'Memory deleted successfully',
        deleted_count: 1
      };
    } else {
      // Batch delete using official API query parameters
      const params = new URLSearchParams();
      
      // Add all supported identifiers as query parameters per official API spec
      if (request.user_id) {
        params.append('user_id', request.user_id);
      }
      if (request.agent_id) {
        params.append('agent_id', request.agent_id);
      }
      if (request.app_id) {
        params.append('app_id', request.app_id);
      }
      if (request.run_id) {
        params.append('run_id', request.run_id);
      }
      
      // Add metadata as JSON string if provided (per official API spec)
      if (request.metadata) {
        params.append('metadata', JSON.stringify(request.metadata));
      }
      
      // Support legacy filters parameter for backward compatibility
      if (request.filters && !request.metadata) {
        params.append('metadata', JSON.stringify(request.filters));
      }
      
      // Add org_id and project_id (either from request or config)
      const orgId = request.org_id || config.mem0.orgId;
      const projectId = request.project_id || config.mem0.projectId;
      
      if (orgId) {
        params.append('org_id', orgId);
      }
      if (projectId) {
        params.append('project_id', projectId);
      }
      
      const queryString = params.toString();
      const response = await this.request<{ message: string }>(`/v1/memories/${queryString ? '?' + queryString : ''}`, {
        method: 'DELETE'
      });

      // Parse response to extract deleted count from message like "X memories deleted successfully!"
      const message = response.message || '';
      const countMatch = message.match(/(\d+)\s*memories?\s*deleted/i);
      const deletedCount = countMatch ? parseInt(countMatch[1]) : 1; // Default to 1 if pattern not found

      return {
        status: 'success',
        message: response.message || 'Memories deleted successfully',
        deleted_count: deletedCount
      };
    }
  }

  async getMemory(memoryId: string): Promise<Memory> {
    return await this.request<Memory>(`/v1/memories/${memoryId}/`, {
      method: 'GET'
    });
  }

  async listMemories(userId?: string, limit?: number): Promise<Memory[]> {
    // Use v2 API for listing memories (GET method with params)
    const params = new URLSearchParams({
      limit: (limit || 100).toString()
    });
    
    if (userId) {
      params.append('user_id', userId);
    }
    
    // Add org_id and project_id if configured
    if (config.mem0.orgId) {
      params.append('org_id', config.mem0.orgId);
    }
    if (config.mem0.projectId) {
      params.append('project_id', config.mem0.projectId);
    }

    const response = await this.request<any>(`/v2/memories/?${params}`, {
      method: 'GET'
    });

    return response.results || response.memories || [];
  }
}