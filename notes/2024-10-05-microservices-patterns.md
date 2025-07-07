---
title: Essential Microservices Patterns
date: 2024-10-05
tags: microservices, architecture, patterns, distributed-systems, design
---

# Essential Microservices Patterns

Microservices architecture brings benefits but also complexity. These patterns help manage that complexity effectively.

## Circuit Breaker Pattern

Prevent cascading failures by detecting when a service is unavailable:

```python
class CircuitBreaker:
    def __init__(self, failure_threshold=5, timeout=60):
        self.failure_threshold = failure_threshold
        self.timeout = timeout
        self.failure_count = 0
        self.last_failure_time = None
        self.state = 'CLOSED'  # CLOSED, OPEN, HALF_OPEN
    
    def call(self, func, *args, **kwargs):
        if self.state == 'OPEN':
            if time.time() - self.last_failure_time > self.timeout:
                self.state = 'HALF_OPEN'
            else:
                raise Exception("Circuit breaker is OPEN")
        
        try:
            result = func(*args, **kwargs)
            self.reset()
            return result
        except Exception as e:
            self.record_failure()
            raise e
```

## Saga Pattern

Manage distributed transactions across services:
- **Choreography**: Services coordinate directly
- **Orchestration**: Central coordinator manages the saga

## API Gateway Pattern

Single entry point for all client requests:
- Route requests to appropriate services
- Handle authentication and authorization
- Rate limiting and request/response transformation