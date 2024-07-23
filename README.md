# task_dispatch
My cool new project!

```
range from=0 to=10 -> a
range from=-5 to=5 -> b
pow 2
sum
print
```

There are two options:
- we keep block outputs near the block definition
  - requires unique blocks (pointers)
- we keep the block outputs in the global cache
  - allows for block duplication (allocatable tree)
  - allows for sequential processing


Order:
 - check if block allows caching. if yes:
   - trace the operation
   - check if operation result is cached
   - if cached, return the cached result
 - otherwise, execute the operation
 - if block allows caching, cache the result
 - return the result
