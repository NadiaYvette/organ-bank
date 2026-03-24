-- Object-oriented programming with metatables

local Animal = {}
Animal.__index = Animal

function Animal.new(name, sound)
    local self = setmetatable({}, Animal)
    self.name = name
    self.sound = sound
    return self
end

function Animal:speak()
    print(self.name .. " says " .. self.sound)
end

function Animal:getName()
    return self.name
end

local dog = Animal.new("Rex", "Woof")
local cat = Animal.new("Whiskers", "Meow")
dog:speak()
cat:speak()
