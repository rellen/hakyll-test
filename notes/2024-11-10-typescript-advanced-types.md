---
title: Advanced TypeScript Types
date: 2024-11-10
tags: typescript, javascript, types, advanced, generics
---

# Advanced TypeScript Types

TypeScript's type system is incredibly powerful. Here are some advanced patterns that can make your code more type-safe and expressive.

## Conditional Types

```typescript
type ApiResponse<T> = T extends string 
  ? { message: T }
  : { data: T };
```

## Mapped Types

```typescript
type Partial<T> = {
  [P in keyof T]?: T[P];
};

type Required<T> = {
  [P in keyof T]-?: T[P];
};
```

## Template Literal Types

```typescript
type EventName<T extends string> = `on${Capitalize<T>}`;
type ClickEvent = EventName<"click">; // "onClick"
```

## Utility Types

- `Pick<T, K>` - Select specific properties
- `Omit<T, K>` - Exclude specific properties
- `Record<K, T>` - Create object type with specific keys