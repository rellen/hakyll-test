---
title: Docker Compose for Development Environments
date: 2024-12-08
tags: docker, devops, development, containers, productivity
---

# Docker Compose for Development Environments

Docker Compose makes it easy to define and run multi-container Docker applications, perfect for development environments.

## Basic Setup

```yaml
version: '3.8'
services:
  web:
    build: .
    ports:
      - "3000:3000"
    volumes:
      - .:/app
    depends_on:
      - db
  
  db:
    image: postgres:14
    environment:
      POSTGRES_DB: myapp
      POSTGRES_PASSWORD: password
```

## Hot Reloading

Volume mounts enable hot reloading during development, making the feedback loop much faster than rebuilding containers constantly.