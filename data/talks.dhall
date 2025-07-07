-- List of talks using the Talk schema
let Talk = ./Talk.dhall

in  [ Talk::{
        title = "Building Resilient Systems with Elixir"
      , description = ''
          Exploring how Elixir's **actor model** and fault-tolerance mechanisms 
          enable building highly resilient distributed systems. 
          
          We'll cover:
          - OTP principles and supervision trees
          - Real-world patterns for handling failures gracefully
          - Building distributed systems that self-heal
          ''
      , organisation = "Elixir Australia"
      , year = 2024
      , month = 8
      , video = Some "https://www.youtube.com/watch?v=example1"
      , slides = Some "https://slides.example.com/elixir-resilient"
      }
    , Talk::{
        title = "Functional Programming Patterns in Practice"
      , description = ''
          A deep dive into practical **functional programming patterns** that can 
          improve code quality and maintainability. Topics include:

          - Immutability and pure functions
          - Higher-order functions and composition
          - Error handling with Maybe/Either types
          - Property-based testing
          ''
      , organisation = "BFPG"
      , year = 2024
      , month = 5
      , slides = Some "https://slides.example.com/fp-patterns"
      }
    , Talk::{
        title = "Type-Safe Web Development with Haskell"
      , description = ''
          Learn how Haskell's **type system** can eliminate entire classes of bugs 
          in web applications. 
          
          In this talk, we'll:
          - Build a small web service using Servant
          - Explore how types guide development
          - See compile-time guarantees in action
          - Discover why "if it compiles, it works"
          ''
      , organisation = "BFPG"
      , year = 2023
      , month = 11
      , video = Some "https://www.youtube.com/watch?v=example2"
      }
    , Talk::{
        title = "Introduction to Phoenix LiveView"
      , description = ''
          Phoenix LiveView revolutionizes web development by bringing **real-time, 
          interactive experiences** without complex JavaScript frameworks. 
          
          This talk covers:
          - The basics of LiveView architecture
          - Building dynamic UIs with server-side rendering
          - Real-time updates without JavaScript
          - When to use LiveView vs traditional SPAs
          ''
      , organisation = "Elixir Australia"
      , year = 2023
      , month = 3
      , video = Some "https://www.youtube.com/watch?v=example3"
      , slides = Some "https://slides.example.com/phoenix-liveview"
      }
    ]