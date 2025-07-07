---
title: GenServer Basics in Elixir
date: 2024-12-05
tags: elixir, otp, genserver, concurrency, erlang
---

# GenServer Basics in Elixir

GenServer is the foundation of OTP (Open Telecom Platform) in Elixir, providing a standard way to build stateful server processes.

## Basic GenServer Structure

```elixir
defmodule MyServer do
  use GenServer

  def start_link(initial_state) do
    GenServer.start_link(__MODULE__, initial_state, name: __MODULE__)
  end

  def init(initial_state) do
    {:ok, initial_state}
  end

  def handle_call(:get_state, _from, state) do
    {:reply, state, state}
  end
end
```

## Key Benefits

- **Fault tolerance**: Supervisors can restart failed processes
- **Hot code upgrades**: Update code without stopping the system
- **Message passing**: Clean separation of concerns