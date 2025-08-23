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
  user_id: string;
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
  user_id: string;
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
    const response = await this.request<any>('/v1/memories/', {
      method: 'POST',
      body: JSON.stringify(request)
    });

    return {
      status: 'success',
      message: 'Memory added successfully',
      memory_id: response.id || response.memory_id,
      extracted_facts: response.extracted_facts || []
    };
  }

  async searchMemories(request: SearchMemoriesRequest): Promise<SearchMemoriesResponse> {
    // Use v2 API for search
    const params = new URLSearchParams({
      query: request.query,
      user_id: request.user_id,
      ...(request.top_k && { top_k: request.top_k.toString() }),
      ...(request.threshold && { threshold: request.threshold.toString() })
    });

    if (request.filters) {
      params.append('filters', JSON.stringify(request.filters));
    }

    const response = await this.request<any>(`/v2/memories/search/?${params}`, {
      method: 'GET'
    });

    return {
      memories: response.results || response.memories || [],
      total_count: response.total_count || (response.results || []).length
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
    const params = new URLSearchParams();
    if (userId) params.append('user_id', userId);
    if (limit) params.append('limit', limit.toString());

    const response = await this.request<any>(`/v1/memories/?${params}`, {
      method: 'GET'
    });

    return response.results || response.memories || [];
  }
}