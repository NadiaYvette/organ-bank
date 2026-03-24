-- Table operations in Lua

function map(t, f)
    local result = {}
    for i = 1, #t do
        result[i] = f(t[i])
    end
    return result
end

function filter(t, f)
    local result = {}
    for i = 1, #t do
        if f(t[i]) then
            result[#result + 1] = t[i]
        end
    end
    return result
end

function reduce(t, f, init)
    local acc = init
    for i = 1, #t do
        acc = f(acc, t[i])
    end
    return acc
end

local nums = {1, 2, 3, 4, 5}
local doubled = map(nums, function(x) return x * 2 end)
local evens = filter(nums, function(x) return x % 2 == 0 end)
local sum = reduce(nums, function(a, b) return a + b end, 0)
