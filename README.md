# task_dispatch

My cool new Fortran projects! Allows you to build functional operations based on string.

## Run by docker

```sh
docker build -t task_dispatch https://github.com/gronki/task-dispatch.git
docker run -it task_dispatch
```

Should see ``>`` prompt.

## Examples

Currently in the demo only ``add``, ``mul``, ``square``, ``pow`` and ``array`` are available. There are two data types: real numbers and ``"strings"``.

```
> 2  
 evaluated 2 as 2.0000
 RESULT ::::::::::::::: 2 = 2.0000
> add(1,2)
 evaluated 1 as 1.0000
 evaluated 2 as 2.0000
 TRACE add(1, 2)
 evaluated add(1, 2) as 3.0000
 RESULT ::::::::::::::: add(1, 2) = 3.0000
> c=1
 evaluated 1 as 1.0000
 RESULT ::::::::::::::: 1 = 1.0000
> add(c,2)%mul(0.3)%pow(array(0.5,1.0,1.5))
 evaluated 1 as 1.0000
 evaluated 2 as 2.0000
 TRACE add(1, 2)
 evaluated add(1, 2) as 3.0000
 evaluated 0.3 as .30000
 TRACE mul(add(1, 2), 0.3)
 evaluated mul(add(1, 2), 0.3) as .90000
 evaluated 0.5 as .50000
 evaluated 1.0 as 1.0000
 evaluated 1.5 as 1.5000
 TRACE [0.5, 1.0, 1.5]
 evaluated [0.5, 1.0, 1.5] as [.50000, 1.0000, 1.5000]
 TRACE mul(add(1, 2), 0.3)%pow([0.5, 1.0, 1.5])
  sequence item  1  mul(add(1, 2), 0.3)%pow(0.5) ---> .94868
  sequence item  2  mul(add(1, 2), 0.3)%pow(1.0) ---> .90000
  sequence item  3  mul(add(1, 2), 0.3)%pow(1.5) ---> .85381
 evaluated 
 [mul(add(1, 2), 0.3)%pow(0.5), mul(add(1, 2), 0.3)%pow(1.0), mul(add(1, 2), 0.3
 )%pow(1.5)] as [.94868, .90000, .85381]
 RESULT ::::::::::::::: 
 [mul(add(1, 2), 0.3)%pow(0.5), mul(add(1, 2), 0.3)%pow(1.0), mul(add(1, 2), 0.3
 )%pow(1.5)] = [.94868, .90000, .85381]
```