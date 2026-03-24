-- Factorial in Lua

function factorial(n)
    if n <= 1 then
        return 1
    else
        return n * factorial(n - 1)
    end
end

function factorial_iter(n)
    local acc = 1
    for i = 2, n do
        acc = acc * i
    end
    return acc
end

print(factorial(10))
print(factorial_iter(10))
