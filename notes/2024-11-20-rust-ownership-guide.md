---
title: Understanding Rust Ownership
date: 2024-11-20
tags: rust, ownership, memory-safety, systems-programming
---

# Understanding Rust Ownership

Rust's ownership system is what makes it unique among systems programming languages, providing memory safety without garbage collection.

## Ownership Rules

1. Each value in Rust has a variable that's called its owner
2. There can only be one owner at a time
3. When the owner goes out of scope, the value will be dropped

## Borrowing

```rust
fn calculate_length(s: &String) -> usize {
    s.len()
} // s goes out of scope, but because it doesn't have ownership, nothing happens
```

## Mutable References

```rust
fn change(some_string: &mut String) {
    some_string.push_str(", world");
}
```

The borrow checker ensures memory safety at compile time, eliminating entire classes of bugs.