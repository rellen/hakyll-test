---
title: Go Concurrency Patterns
date: 2024-09-15
tags: go, concurrency, goroutines, channels, patterns
---

# Go Concurrency Patterns

Go's concurrency model with goroutines and channels makes it easy to write concurrent programs. Here are essential patterns.

## Worker Pool Pattern

```go
func workerPool(jobs <-chan int, results chan<- int, workers int) {
    var wg sync.WaitGroup
    
    for i := 0; i < workers; i++ {
        wg.Add(1)
        go func() {
            defer wg.Done()
            for job := range jobs {
                results <- processJob(job)
            }
        }()
    }
    
    wg.Wait()
    close(results)
}
```

## Fan-Out, Fan-In Pattern

```go
func fanOut(in <-chan int) (<-chan int, <-chan int) {
    out1 := make(chan int)
    out2 := make(chan int)
    
    go func() {
        defer close(out1)
        defer close(out2)
        for val := range in {
            out1 <- val
            out2 <- val
        }
    }()
    
    return out1, out2
}

func fanIn(inputs ...<-chan int) <-chan int {
    out := make(chan int)
    var wg sync.WaitGroup
    
    for _, input := range inputs {
        wg.Add(1)
        go func(ch <-chan int) {
            defer wg.Done()
            for val := range ch {
                out <- val
            }
        }(input)
    }
    
    go func() {
        wg.Wait()
        close(out)
    }()
    
    return out
}
```

## Pipeline Pattern

```go
func pipeline() {
    // Stage 1: Generate numbers
    numbers := make(chan int)
    go func() {
        defer close(numbers)
        for i := 1; i <= 100; i++ {
            numbers <- i
        }
    }()
    
    // Stage 2: Square numbers
    squares := make(chan int)
    go func() {
        defer close(squares)
        for n := range numbers {
            squares <- n * n
        }
    }()
    
    // Stage 3: Print results
    for s := range squares {
        fmt.Println(s)
    }
}
```