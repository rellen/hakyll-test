---
title: Next.js Performance Optimization
date: 2024-10-25
tags: nextjs, react, performance, optimization, frontend
---

# Next.js Performance Optimization

Next.js provides many built-in optimizations, but there are additional techniques to maximize performance.

## Image Optimization

```jsx
import Image from 'next/image'

function Hero() {
  return (
    <Image
      src="/hero.jpg"
      alt="Hero image"
      width={800}
      height={600}
      priority
      placeholder="blur"
    />
  )
}
```

## Code Splitting

```jsx
import dynamic from 'next/dynamic'

const DynamicComponent = dynamic(() => import('../components/Heavy'), {
  loading: () => <p>Loading...</p>,
  ssr: false
})
```

## Bundle Analysis

```bash
npm install --save-dev @next/bundle-analyzer
```

## Static Site Generation

Use `getStaticProps` and `getStaticPaths` for pages that can be pre-rendered at build time for maximum performance.