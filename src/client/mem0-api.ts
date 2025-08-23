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
  top_k?: number;
  threshold?: number;
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
  filters?: Record<string, any>;
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

    return {
      status: 'success',
      message: 'Memory added successfully',
      memory_id: response.id || response.memory_id,
      extracted_facts: response.extracted_facts || []
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
      rerank: false,
      keyword_search: false,
      filter_memories: false
    };

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

    return {
      memories: response || [],
      total_count: (response || []).length
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
      // Delete single memory
      await this.request(`/v1/memories/${request.memory_id}/`, {
        method: 'DELETE'
      });
      
      return {
        status: 'success',
        message: 'Memory deleted successfully',
        deleted_count: 1
      };
    } else if (request.user_id || request.filters) {
      // Batch delete
      const searchResult = await this.searchMemories({
        query: '*',
        user_id: request.user_id || '',
        filters: request.filters
      });

      let deletedCount = 0;
      for (const memory of searchResult.memories) {
        try {
          await this.request(`/v1/memories/${memory.id}/`, {
            method: 'DELETE'
          });
          deletedCount++;
        } catch (error) {
          console.error(`Failed to delete memory ${memory.id}:`, error);
        }
      }

      return {
        status: 'success',
        message: `Deleted ${deletedCount} memories`,
        deleted_count: deletedCount
      };
    }

    return {
      status: 'error',
      message: 'No deletion criteria provided',
      deleted_count: 0
    };
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