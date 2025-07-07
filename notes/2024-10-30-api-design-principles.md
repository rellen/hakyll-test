---
title: REST API Design Principles
date: 2024-10-30
tags: api, rest, design, backend, web-development
---

# REST API Design Principles

Well-designed APIs are crucial for maintainable and scalable applications. Here are key principles for REST API design.

## Resource-Based URLs

```
GET    /users          # Get all users
GET    /users/123      # Get specific user
POST   /users          # Create new user
PUT    /users/123      # Update user
DELETE /users/123      # Delete user
```

## HTTP Status Codes

- `200 OK` - Successful GET, PUT, or PATCH
- `201 Created` - Successful POST
- `204 No Content` - Successful DELETE
- `400 Bad Request` - Invalid request
- `401 Unauthorized` - Authentication required
- `404 Not Found` - Resource doesn't exist
- `500 Internal Server Error` - Server error

## Versioning

Include version in URL or headers:
- `/api/v1/users`
- `Accept: application/vnd.api+json;version=1`

## Pagination

```json
{
  "data": [...],
  "meta": {
    "page": 1,
    "per_page": 20,
    "total": 100
  }
}
```