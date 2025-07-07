---
title: Essential Linux Command Line Tips
date: 2024-10-20
tags: linux, command-line, bash, productivity, tools
---

# Essential Linux Command Line Tips

Master these command line techniques to become more productive on Linux systems.

## File Operations

```bash
# Find files by pattern
find . -name "*.js" -type f

# Copy with progress bar
rsync -ah --progress source/ dest/

# Archive with compression
tar -czf archive.tar.gz directory/

# Extract specific files
tar -xzf archive.tar.gz --wildcards "*.txt"
```

## Process Management

```bash
# Find process by name
pgrep -f "node server.js"

# Kill processes by pattern
pkill -f "old-process"

# Background process with nohup
nohup long-running-command &

# Process tree
pstree -p
```

## Text Processing

```bash
# Sort and count unique lines
sort file.txt | uniq -c

# Search with context
grep -A 3 -B 3 "pattern" file.txt

# Replace text in multiple files
sed -i 's/old/new/g' *.txt
```