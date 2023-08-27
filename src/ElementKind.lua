--[[
	Contains markers for annotating the type of an element.

	Use `ElementKind` as a key, and values from it as the value.

		local element = {
			[ElementKind] = ElementKind.Host,
		}
]]

local Symbol = require "./Symbol2"
local strict = require "./strict"
local Portal = require "./Portal"

local ElementKind = newproxy(true)

local ElementKindInternal = {
	Portal = Symbol.named "Portal",
	Host = Symbol.named "Host",
	Function = Symbol.named "Function",
	Stateful = Symbol.named "Stateful",
	Fragment = Symbol.named "Fragment",
}

function ElementKindInternal.of(value)
	if type(value) ~= "table" then
		return nil
	end

	return value[ElementKind]
end

local componentTypesToKinds = {
	["string"] = ElementKindInternal.Host,
	["function"] = ElementKindInternal.Function,
	["table"] = ElementKindInternal.Stateful,
}

function ElementKindInternal.fromComponent(component)
	if component == Portal then
		return ElementKind.Portal
	else
		return componentTypesToKinds[type(component)]
	end
end

getmetatable(ElementKind).__index = ElementKindInternal

strict(ElementKindInternal, "ElementKind")

return ElementKind
