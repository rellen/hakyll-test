---
title: Redis Caching Strategies
date: 2024-09-25
tags: redis, caching, performance, database, optimization
---

# Redis Caching Strategies

Redis is an in-memory data structure store that excels as a cache. Different caching strategies suit different use cases.

## Cache-Aside (Lazy Loading)

Application manages the cache directly:

```python
def get_user(user_id):
    # Try cache first
    user = redis.get(f"user:{user_id}")
    if user:
        return json.loads(user)
    
    # Cache miss - fetch from database
    user = database.get_user(user_id)
    if user:
        redis.setex(f"user:{user_id}", 3600, json.dumps(user))
    
    return user
```

## Write-Through

Cache is updated whenever the database is updated:

```python
def update_user(user_id, data):
    # Update database
    database.update_user(user_id, data)
    
    # Update cache
    user = database.get_user(user_id)
    redis.setex(f"user:{user_id}", 3600, json.dumps(user))
```

## Write-Behind (Write-Back)

Cache is updated immediately, database is updated asynchronously:
- Better performance for write-heavy workloads
- Risk of data loss if cache fails

## TTL Strategies

- **Fixed TTL**: Same expiration for all keys
- **Random TTL**: Avoid thundering herd problem
- **Sliding window**: Extend TTL on access

## Key Patterns

- `user:123` - Simple key-value
- `session:abc123` - Session storage  
- `rate_limit:user:123:api` - Rate limiting
- `leaderboard:game:456` - Sorted sets for rankings