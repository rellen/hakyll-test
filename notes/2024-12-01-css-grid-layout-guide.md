---
title: CSS Grid Layout Complete Guide
date: 2024-12-01
tags: css, grid, layout, frontend, responsive-design
---

# CSS Grid Layout Complete Guide

CSS Grid is a powerful layout system that makes it easy to create complex, responsive layouts with clean code.

## Basic Grid Setup

```css
.container {
  display: grid;
  grid-template-columns: repeat(3, 1fr);
  grid-gap: 20px;
}
```

## Grid Areas

```css
.layout {
  grid-template-areas:
    "header header header"
    "sidebar main main"
    "footer footer footer";
}

.header { grid-area: header; }
.sidebar { grid-area: sidebar; }
.main { grid-area: main; }
.footer { grid-area: footer; }
```

Grid areas make complex layouts readable and maintainable.