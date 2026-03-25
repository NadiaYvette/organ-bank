function factorial(n)
  if n <= 1 then return 1
  else return n * factorial(n - 1)
  end
end

function main()
  return factorial(10)
end
