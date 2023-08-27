local __DARKLUA_BUNDLE_MODULES = {}

do
	local defaultConfig = {
		["internalTypeChecks"] = false,
		["typeChecks"] = false,
		["elementTracing"] = false,
		["propValidation"] = false,
	}
	local defaultConfigKeys = {}

	for key in pairs(defaultConfig) do
		table.insert(defaultConfigKeys, key)
	end

	local Config = {}

	function Config.new()
		local self = {}

		self._currentConfig = setmetatable({}, {
			__index = function(_, key)
				local message = ([[Invalid global configuration key %q. Valid configuration keys are: %s]]):format(
					tostring(key),
					table.concat(defaultConfigKeys, ", ")
				)

				error(message, 3)
			end,
		})
		self.set = function(...)
			return Config.set(self, ...)
		end
		self.get = function(...)
			return Config.get(self, ...)
		end
		self.scoped = function(...)
			return Config.scoped(self, ...)
		end

		self.set(defaultConfig)

		return self
	end
	function Config:set(configValues)
		for key, value in pairs(configValues) do
			if defaultConfig[key] == nil then
				local message = ([[Invalid global configuration key %q (type %s). Valid configuration keys are: %s]]):format(
					tostring(key),
					typeof(key),
					table.concat(defaultConfigKeys, ", ")
				)

				error(message, 3)
			end
			if typeof(value) ~= "boolean" then
				local message = ([[Invalid value %q (type %s) for global configuration key %q. Valid values are: true, false]]):format(
					tostring(value),
					typeof(value),
					tostring(key)
				)

				error(message, 3)
			end

			self._currentConfig[key] = value
		end
	end
	function Config:get()
		return self._currentConfig
	end
	function Config:scoped(configValues, callback)
		local previousValues = table.clone(self._currentConfig)

		self.set(configValues)

		local success, result = pcall(callback)

		self.set(previousValues)
		assert(success, result)
	end

	__DARKLUA_BUNDLE_MODULES.a = Config
end
do
	local Config = __DARKLUA_BUNDLE_MODULES.a

	__DARKLUA_BUNDLE_MODULES.b = Config.new()
end
do
	local Symbol = {}

	function Symbol.named(name)
		assert(type(name) == "string", "Symbols must be created using a string name!")

		local self = newproxy(true)
		local wrappedName = ("Symbol(%s)"):format(name)

		getmetatable(self).__tostring = function()
			return wrappedName
		end

		return self
	end

	__DARKLUA_BUNDLE_MODULES.c = Symbol
end
do
	__DARKLUA_BUNDLE_MODULES.d = __DARKLUA_BUNDLE_MODULES.c
end
do
	local function strict(t, name)
		local newName = name or tostring(t)

		return setmetatable(t, {
			__index = function(_self, key)
				local message = ("%q (%s) is not a valid member of %s"):format(tostring(key), typeof(key), newName)

				error(message, 2)
			end,
			__newindex = function(_self, key, _value)
				local message = ("%q (%s) is not a valid member of %s"):format(tostring(key), typeof(key), newName)

				error(message, 2)
			end,
		})
	end

	__DARKLUA_BUNDLE_MODULES.e = strict
end
do
	local Symbol = __DARKLUA_BUNDLE_MODULES.d
	local strict = __DARKLUA_BUNDLE_MODULES.e
	local Type = newproxy(true)
	local TypeInternal = {}

	local function addType(name)
		TypeInternal[name] = Symbol.named("Roact" .. name)
	end

	addType "Binding"
	addType "Element"
	addType "HostChangeEvent"
	addType "HostEvent"
	addType "StatefulComponentClass"
	addType "StatefulComponentInstance"
	addType "VirtualNode"
	addType "VirtualTree"

	function TypeInternal.of(value)
		if typeof(value) ~= "table" then
			return nil
		end

		return value[Type]
	end

	getmetatable(Type).__index = TypeInternal
	getmetatable(Type).__tostring = function()
		return "RoactType"
	end

	strict(TypeInternal, "Type")

	__DARKLUA_BUNDLE_MODULES.f = Type
end
do
	local Symbol = __DARKLUA_BUNDLE_MODULES.c
	local Portal = Symbol.named "Portal"

	__DARKLUA_BUNDLE_MODULES.g = Portal
end
do
	local Symbol = __DARKLUA_BUNDLE_MODULES.d
	local strict = __DARKLUA_BUNDLE_MODULES.e
	local Portal = __DARKLUA_BUNDLE_MODULES.g
	local ElementKind = newproxy(true)
	local ElementKindInternal = {
		Portal = Symbol.named "Portal",
		Host = Symbol.named "Host",
		Function = Symbol.named "Function",
		Stateful = Symbol.named "Stateful",
		Fragment = Symbol.named "Fragment",
	}

	function ElementKindInternal.of(value)
		if typeof(value) ~= "table" then
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
			return componentTypesToKinds[typeof(component)]
		end
	end

	getmetatable(ElementKind).__index = ElementKindInternal

	strict(ElementKindInternal, "ElementKind")

	__DARKLUA_BUNDLE_MODULES.h = ElementKind
end
do
	local Type = __DARKLUA_BUNDLE_MODULES.f
	local Symbol = __DARKLUA_BUNDLE_MODULES.c

	local function noop()
		return nil
	end

	local ElementUtils = {}

	ElementUtils.UseParentKey = Symbol.named "UseParentKey"

	function ElementUtils.iterateElements(elementOrElements)
		local richType = Type.of(elementOrElements)

		if richType == Type.Element then
			local called = false

			return function(_, _)
				if called then
					return nil
				else
					called = true

					return ElementUtils.UseParentKey, elementOrElements
				end
			end
		end

		local regularType = typeof(elementOrElements)

		if elementOrElements == nil or regularType == "boolean" then
			return noop
		end
		if regularType == "table" then
			return pairs(elementOrElements)
		end

		error "Invalid elements"
	end
	function ElementUtils.getElementByKey(elements, hostKey)
		if elements == nil or typeof(elements) == "boolean" then
			return nil
		end
		if Type.of(elements) == Type.Element then
			if hostKey == ElementUtils.UseParentKey then
				return elements
			end

			return nil
		end
		if typeof(elements) == "table" then
			return elements[hostKey]
		end

		error "Invalid elements"
	end

	__DARKLUA_BUNDLE_MODULES.i = ElementUtils
end
do
	local Symbol = __DARKLUA_BUNDLE_MODULES.c
	local Children = Symbol.named "Children"

	__DARKLUA_BUNDLE_MODULES.j = Children
end
do
	local function internalAssert(condition, message)
		if not condition then
			error(message .. " (This is probably a bug in Roact!)", 3)
		end
	end

	__DARKLUA_BUNDLE_MODULES.k = internalAssert
end
do
	local Type = __DARKLUA_BUNDLE_MODULES.f
	local ElementKind = __DARKLUA_BUNDLE_MODULES.h
	local ElementUtils = __DARKLUA_BUNDLE_MODULES.i
	local Children = __DARKLUA_BUNDLE_MODULES.j
	local Symbol = __DARKLUA_BUNDLE_MODULES.c
	local internalAssert = __DARKLUA_BUNDLE_MODULES.k
	local config = __DARKLUA_BUNDLE_MODULES.b.get()
	local InternalData = Symbol.named "InternalData"

	local function createReconciler(renderer)
		local reconciler
		local mountVirtualNode
		local updateVirtualNode
		local unmountVirtualNode

		local function replaceVirtualNode(virtualNode, newElement)
			local hostParent = virtualNode.hostParent
			local hostKey = virtualNode.hostKey
			local depth = virtualNode.depth
			local parent = virtualNode.parent
			local context = virtualNode.originalContext or virtualNode.context
			local parentLegacyContext = virtualNode.parentLegacyContext

			if not virtualNode.wasUnmounted then
				unmountVirtualNode(virtualNode)
			end

			local newNode = mountVirtualNode(newElement, hostParent, hostKey, context, parentLegacyContext)

			if newNode ~= nil then
				newNode.depth = depth
				newNode.parent = parent
			end

			return newNode
		end
		local function updateChildren(virtualNode, hostParent, newChildElements)
			if config.internalTypeChecks then
				internalAssert(Type.of(virtualNode) == Type.VirtualNode, "Expected arg #1 to be of type VirtualNode")
			end

			virtualNode.updateChildrenCount = virtualNode.updateChildrenCount + 1

			local currentUpdateChildrenCount = virtualNode.updateChildrenCount
			local removeKeys = {}

			for childKey, childNode in pairs(virtualNode.children) do
				local newElement = ElementUtils.getElementByKey(newChildElements, childKey)
				local newNode = updateVirtualNode(childNode, newElement)

				if virtualNode.updateChildrenCount ~= currentUpdateChildrenCount then
					if newNode and newNode ~= virtualNode.children[childKey] then
						unmountVirtualNode(newNode)
					end

					return
				end
				if newNode ~= nil then
					virtualNode.children[childKey] = newNode
				else
					removeKeys[childKey] = true
				end
			end
			for childKey in pairs(removeKeys) do
				virtualNode.children[childKey] = nil
			end
			for childKey, newElement in ElementUtils.iterateElements(newChildElements) do
				local concreteKey = childKey

				if childKey == ElementUtils.UseParentKey then
					concreteKey = virtualNode.hostKey
				end
				if virtualNode.children[childKey] == nil then
					local childNode = mountVirtualNode(
						newElement,
						hostParent,
						concreteKey,
						virtualNode.context,
						virtualNode.legacyContext
					)

					if virtualNode.updateChildrenCount ~= currentUpdateChildrenCount then
						if childNode then
							unmountVirtualNode(childNode)
						end

						return
					end
					if childNode ~= nil then
						childNode.depth = virtualNode.depth + 1
						childNode.parent = virtualNode
						virtualNode.children[childKey] = childNode
					end
				end
			end
		end
		local function updateVirtualNodeWithChildren(virtualNode, hostParent, newChildElements)
			updateChildren(virtualNode, hostParent, newChildElements)
		end
		local function updateVirtualNodeWithRenderResult(virtualNode, hostParent, renderResult)
			if Type.of(renderResult) == Type.Element or renderResult == nil or typeof(renderResult) == "boolean" then
				updateChildren(virtualNode, hostParent, renderResult)
			else
				error(
					("%s\n%s"):format(
						"Component returned invalid children:",
						virtualNode.currentElement.source or "<enable element tracebacks>"
					),
					0
				)
			end
		end

		function unmountVirtualNode(virtualNode)
			if config.internalTypeChecks then
				internalAssert(Type.of(virtualNode) == Type.VirtualNode, "Expected arg #1 to be of type VirtualNode")
			end

			virtualNode.wasUnmounted = true

			local kind = ElementKind.of(virtualNode.currentElement)

			if kind == ElementKind.Host then
				renderer.unmountHostNode(reconciler, virtualNode)
			elseif kind == ElementKind.Function or kind == ElementKind.Portal or kind == ElementKind.Fragment then
				for _, childNode in pairs(virtualNode.children) do
					unmountVirtualNode(childNode)
				end
			elseif kind == ElementKind.Stateful then
				virtualNode.instance:__unmount()
			else
				error(("Unknown ElementKind %q"):format(tostring(kind)), 2)
			end
		end

		local function updateFunctionVirtualNode(virtualNode, newElement)
			local children = newElement.component(newElement.props)

			updateVirtualNodeWithRenderResult(virtualNode, virtualNode.hostParent, children)

			return virtualNode
		end
		local function updatePortalVirtualNode(virtualNode, newElement)
			local oldElement = virtualNode.currentElement
			local oldTargetHostParent = oldElement.props.target
			local targetHostParent = newElement.props.target

			assert(renderer.isHostObject(targetHostParent), "Expected target to be host object")

			if targetHostParent ~= oldTargetHostParent then
				return replaceVirtualNode(virtualNode, newElement)
			end

			local children = newElement.props[Children]

			updateVirtualNodeWithChildren(virtualNode, targetHostParent, children)

			return virtualNode
		end
		local function updateFragmentVirtualNode(virtualNode, newElement)
			updateVirtualNodeWithChildren(virtualNode, virtualNode.hostParent, newElement.elements)

			return virtualNode
		end

		function updateVirtualNode(virtualNode, newElement, newState)
			if config.internalTypeChecks then
				internalAssert(Type.of(virtualNode) == Type.VirtualNode, "Expected arg #1 to be of type VirtualNode")
			end
			if config.typeChecks then
				assert(
					Type.of(newElement) == Type.Element or typeof(newElement) == "boolean" or newElement == nil,
					"Expected arg #2 to be of type Element, boolean, or nil"
				)
			end
			if virtualNode.currentElement == newElement and newState == nil then
				return virtualNode
			end
			if typeof(newElement) == "boolean" or newElement == nil then
				unmountVirtualNode(virtualNode)

				return nil
			end
			if virtualNode.currentElement.component ~= newElement.component then
				return replaceVirtualNode(virtualNode, newElement)
			end

			local kind = ElementKind.of(newElement)
			local shouldContinueUpdate = true

			if kind == ElementKind.Host then
				virtualNode = renderer.updateHostNode(reconciler, virtualNode, newElement)
			elseif kind == ElementKind.Function then
				virtualNode = updateFunctionVirtualNode(virtualNode, newElement)
			elseif kind == ElementKind.Stateful then
				shouldContinueUpdate = virtualNode.instance:__update(newElement, newState)
			elseif kind == ElementKind.Portal then
				virtualNode = updatePortalVirtualNode(virtualNode, newElement)
			elseif kind == ElementKind.Fragment then
				virtualNode = updateFragmentVirtualNode(virtualNode, newElement)
			else
				error(("Unknown ElementKind %q"):format(tostring(kind)), 2)
			end
			if not shouldContinueUpdate then
				return virtualNode
			end

			virtualNode.currentElement = newElement

			return virtualNode
		end

		local function createVirtualNode(element, hostParent, hostKey, context, legacyContext)
			if config.internalTypeChecks then
				internalAssert(
					renderer.isHostObject(hostParent) or hostParent == nil,
					"Expected arg #2 to be a host object"
				)
				internalAssert(
					typeof(context) == "table" or context == nil,
					"Expected arg #4 to be of type table or nil"
				)
				internalAssert(
					typeof(legacyContext) == "table" or legacyContext == nil,
					"Expected arg #5 to be of type table or nil"
				)
			end
			if config.typeChecks then
				assert(hostKey ~= nil, "Expected arg #3 to be non-nil")
				assert(
					Type.of(element) == Type.Element or typeof(element) == "boolean",
					"Expected arg #1 to be of type Element or boolean"
				)
			end

			return {
				[Type] = Type.VirtualNode,
				currentElement = element,
				depth = 1,
				parent = nil,
				children = {},
				hostParent = hostParent,
				hostKey = hostKey,
				updateChildrenCount = 0,
				wasUnmounted = false,
				legacyContext = legacyContext,
				parentLegacyContext = legacyContext,
				context = context or {},
				originalContext = nil,
			}
		end
		local function mountFunctionVirtualNode(virtualNode)
			local element = virtualNode.currentElement
			local children = element.component(element.props)

			updateVirtualNodeWithRenderResult(virtualNode, virtualNode.hostParent, children)
		end
		local function mountPortalVirtualNode(virtualNode)
			local element = virtualNode.currentElement
			local targetHostParent = element.props.target
			local children = element.props[Children]

			assert(renderer.isHostObject(targetHostParent), "Expected target to be host object")
			updateVirtualNodeWithChildren(virtualNode, targetHostParent, children)
		end
		local function mountFragmentVirtualNode(virtualNode)
			local element = virtualNode.currentElement
			local children = element.elements

			updateVirtualNodeWithChildren(virtualNode, virtualNode.hostParent, children)
		end

		function mountVirtualNode(element, hostParent, hostKey, context, legacyContext)
			if config.internalTypeChecks then
				internalAssert(
					renderer.isHostObject(hostParent) or hostParent == nil,
					"Expected arg #2 to be a host object"
				)
				internalAssert(
					typeof(legacyContext) == "table" or legacyContext == nil,
					"Expected arg #5 to be of type table or nil"
				)
			end
			if config.typeChecks then
				assert(hostKey ~= nil, "Expected arg #3 to be non-nil")
				assert(
					Type.of(element) == Type.Element or typeof(element) == "boolean",
					"Expected arg #1 to be of type Element or boolean"
				)
			end
			if typeof(element) == "boolean" then
				return nil
			end

			local kind = ElementKind.of(element)
			local virtualNode = createVirtualNode(element, hostParent, hostKey, context, legacyContext)

			if kind == ElementKind.Host then
				renderer.mountHostNode(reconciler, virtualNode)
			elseif kind == ElementKind.Function then
				mountFunctionVirtualNode(virtualNode)
			elseif kind == ElementKind.Stateful then
				element.component:__mount(reconciler, virtualNode)
			elseif kind == ElementKind.Portal then
				mountPortalVirtualNode(virtualNode)
			elseif kind == ElementKind.Fragment then
				mountFragmentVirtualNode(virtualNode)
			else
				error(("Unknown ElementKind %q"):format(tostring(kind)), 2)
			end

			return virtualNode
		end

		local function mountVirtualTree(element, hostParent, hostKey)
			if config.typeChecks then
				assert(Type.of(element) == Type.Element, "Expected arg #1 to be of type Element")
				assert(renderer.isHostObject(hostParent) or hostParent == nil, "Expected arg #2 to be a host object")
			end
			if hostKey == nil then
				hostKey = "RoactTree"
			end

			local tree = {
				[Type] = Type.VirtualTree,
				[InternalData] = {
					rootNode = nil,
					mounted = true,
				},
			}

			tree[InternalData].rootNode = mountVirtualNode(element, hostParent, hostKey)

			return tree
		end
		local function unmountVirtualTree(tree)
			local internalData = tree[InternalData]

			if config.typeChecks then
				assert(Type.of(tree) == Type.VirtualTree, "Expected arg #1 to be a Roact handle")
				assert(internalData.mounted, [[Cannot unmounted a Roact tree that has already been unmounted]])
			end

			internalData.mounted = false

			if internalData.rootNode ~= nil then
				unmountVirtualNode(internalData.rootNode)
			end
		end
		local function updateVirtualTree(tree, newElement)
			local internalData = tree[InternalData]

			if config.typeChecks then
				assert(Type.of(tree) == Type.VirtualTree, "Expected arg #1 to be a Roact handle")
				assert(Type.of(newElement) == Type.Element, "Expected arg #2 to be a Roact Element")
			end

			internalData.rootNode = updateVirtualNode(internalData.rootNode, newElement)

			return tree
		end

		reconciler = {
			mountVirtualTree = mountVirtualTree,
			unmountVirtualTree = unmountVirtualTree,
			updateVirtualTree = updateVirtualTree,
			createVirtualNode = createVirtualNode,
			mountVirtualNode = mountVirtualNode,
			unmountVirtualNode = unmountVirtualNode,
			updateVirtualNode = updateVirtualNode,
			updateVirtualNodeWithChildren = updateVirtualNodeWithChildren,
			updateVirtualNodeWithRenderResult = updateVirtualNodeWithRenderResult,
		}

		return reconciler
	end

	__DARKLUA_BUNDLE_MODULES.l = createReconciler
end
do
	local outputEnabled = true
	local collectors = {}
	local onceUsedLocations = {}

	local function indent(source, indentLevel)
		local indentString = ("\t"):rep(indentLevel)

		return indentString .. source:gsub("\n", "\n" .. indentString)
	end
	local function indentLines(lines, indentLevel)
		local outputBuffer = {}

		for _, line in ipairs(lines) do
			table.insert(outputBuffer, indent(line, indentLevel))
		end

		return table.concat(outputBuffer, "\n")
	end

	local logInfoMetatable = {}

	function logInfoMetatable:__tostring()
		local outputBuffer = {
			"LogInfo {",
		}
		local errorCount = #self.errors
		local warningCount = #self.warnings
		local infosCount = #self.infos

		if errorCount + warningCount + infosCount == 0 then
			table.insert(outputBuffer, "\t(no messages)")
		end
		if errorCount > 0 then
			table.insert(outputBuffer, ("\tErrors (%d) {"):format(errorCount))
			table.insert(outputBuffer, indentLines(self.errors, 2))
			table.insert(outputBuffer, "\t}")
		end
		if warningCount > 0 then
			table.insert(outputBuffer, ("\tWarnings (%d) {"):format(warningCount))
			table.insert(outputBuffer, indentLines(self.warnings, 2))
			table.insert(outputBuffer, "\t}")
		end
		if infosCount > 0 then
			table.insert(outputBuffer, ("\tInfos (%d) {"):format(infosCount))
			table.insert(outputBuffer, indentLines(self.infos, 2))
			table.insert(outputBuffer, "\t}")
		end

		table.insert(outputBuffer, "}")

		return table.concat(outputBuffer, "\n")
	end

	local function createLogInfo()
		local logInfo = {
			errors = {},
			warnings = {},
			infos = {},
		}

		setmetatable(logInfo, logInfoMetatable)

		return logInfo
	end

	local Logging = {}

	function Logging.capture(callback)
		local collector = createLogInfo()
		local wasOutputEnabled = outputEnabled

		outputEnabled = false
		collectors[collector] = true

		local success, result = pcall(callback)

		collectors[collector] = nil
		outputEnabled = wasOutputEnabled

		assert(success, result)

		return collector
	end
	function Logging.warn(messageTemplate, ...)
		local message = messageTemplate:format(...)

		for collector in pairs(collectors) do
			table.insert(collector.warnings, message)
		end

		local trace = debug.traceback("", 2):sub(2)
		local fullMessage = ("%s\n%s"):format(message, indent(trace, 1))

		if outputEnabled then
			warn(fullMessage)
		end
	end
	function Logging.warnOnce(messageTemplate, ...)
		local trace = debug.traceback()

		if onceUsedLocations[trace] then
			return
		end

		onceUsedLocations[trace] = true

		Logging.warn(messageTemplate, ...)
	end

	__DARKLUA_BUNDLE_MODULES.m = Logging
end
do
	local Logging = __DARKLUA_BUNDLE_MODULES.m
	local reifyMessage = [[Roact.reify has been renamed to Roact.mount and will be removed in a future release.
Check the call to Roact.reify at:
]]
	local teardownMessage = [[Roact.teardown has been renamed to Roact.unmount and will be removed in a future release.
Check the call to Roact.teardown at:
]]
	local reconcileMessage = [[Roact.reconcile has been renamed to Roact.update and will be removed in a future release.
Check the call to Roact.reconcile at:
]]

	local function createReconcilerCompat(reconciler)
		local compat = {}

		function compat.reify(...)
			Logging.warnOnce(reifyMessage)

			return reconciler.mountVirtualTree(...)
		end
		function compat.teardown(...)
			Logging.warnOnce(teardownMessage)

			return reconciler.unmountVirtualTree(...)
		end
		function compat.reconcile(...)
			Logging.warnOnce(reconcileMessage)

			return reconciler.updateVirtualTree(...)
		end

		return compat
	end

	__DARKLUA_BUNDLE_MODULES.n = createReconcilerCompat
end
do
	local function createSignal()
		local connections = {}
		local suspendedConnections = {}
		local firing = false

		local function subscribe(_self, callback)
			assert(typeof(callback) == "function", "Can only subscribe to signals with a function.")

			local connection = {
				callback = callback,
				disconnected = false,
			}

			if firing and not connections[callback] then
				suspendedConnections[callback] = connection
			end

			connections[callback] = connection

			local function disconnect()
				assert(not connection.disconnected, "Listeners can only be disconnected once.")

				connection.disconnected = true
				connections[callback] = nil
				suspendedConnections[callback] = nil
			end

			return disconnect
		end
		local function fire(_self, ...)
			firing = true

			for callback, connection in pairs(connections) do
				if not connection.disconnected and not suspendedConnections[callback] then
					callback(...)
				end
			end

			firing = false

			for callback, _ in pairs(suspendedConnections) do
				suspendedConnections[callback] = nil
			end
		end

		return {
			subscribe = subscribe,
			fire = fire,
		}
	end

	__DARKLUA_BUNDLE_MODULES.o = createSignal
end
do
	local createSignal = __DARKLUA_BUNDLE_MODULES.o
	local Symbol = __DARKLUA_BUNDLE_MODULES.c
	local Type = __DARKLUA_BUNDLE_MODULES.f
	local config = __DARKLUA_BUNDLE_MODULES.b.get()
	local BindingImpl = Symbol.named "BindingImpl"
	local BindingInternalApi = {}
	local bindingPrototype = {}

	function bindingPrototype:getValue()
		return BindingInternalApi.getValue(self)
	end
	function bindingPrototype:map(predicate)
		return BindingInternalApi.map(self, predicate)
	end

	local BindingPublicMeta = {
		__index = bindingPrototype,
		__tostring = function(self)
			return string.format("RoactBinding(%s)", tostring(self:getValue()))
		end,
	}

	function BindingInternalApi.update(binding, newValue)
		return binding[BindingImpl].update(newValue)
	end
	function BindingInternalApi.subscribe(binding, callback)
		return binding[BindingImpl].subscribe(callback)
	end
	function BindingInternalApi.getValue(binding)
		return binding[BindingImpl].getValue()
	end
	function BindingInternalApi.create(initialValue)
		local impl = {
			value = initialValue,
			changeSignal = createSignal(),
		}

		function impl.subscribe(callback)
			return impl.changeSignal:subscribe(callback)
		end
		function impl.update(newValue)
			impl.value = newValue

			impl.changeSignal:fire(newValue)
		end
		function impl.getValue()
			return impl.value
		end

		return setmetatable({
			[Type] = Type.Binding,
			[BindingImpl] = impl,
		}, BindingPublicMeta), impl.update
	end
	function BindingInternalApi.map(upstreamBinding, predicate)
		if config.typeChecks then
			assert(Type.of(upstreamBinding) == Type.Binding, "Expected arg #1 to be a binding")
			assert(typeof(predicate) == "function", "Expected arg #1 to be a function")
		end

		local impl = {}

		function impl.subscribe(callback)
			return BindingInternalApi.subscribe(upstreamBinding, function(newValue)
				callback(predicate(newValue))
			end)
		end
		function impl.update(_newValue)
			error([[Bindings created by Binding:map(fn) cannot be updated directly]], 2)
		end
		function impl.getValue()
			return predicate(upstreamBinding:getValue())
		end

		return setmetatable({
			[Type] = Type.Binding,
			[BindingImpl] = impl,
		}, BindingPublicMeta)
	end
	function BindingInternalApi.join(upstreamBindings)
		if config.typeChecks then
			assert(typeof(upstreamBindings) == "table", "Expected arg #1 to be of type table")

			for key, value in pairs(upstreamBindings) do
				if Type.of(value) ~= Type.Binding then
					local message = ([[Expected arg #1 to contain only bindings, but key %q had a non-binding value]]):format(
						tostring(key)
					)

					error(message, 2)
				end
			end
		end

		local impl = {}

		local function getValue()
			local value = {}

			for key, upstream in pairs(upstreamBindings) do
				value[key] = upstream:getValue()
			end

			return value
		end

		function impl.subscribe(callback)
			local disconnects = {}

			for key, upstream in pairs(upstreamBindings) do
				disconnects[key] = BindingInternalApi.subscribe(upstream, function(_newValue)
					callback(getValue())
				end)
			end

			return function()
				if disconnects == nil then
					return
				end

				for _, disconnect in pairs(disconnects) do
					disconnect()
				end

				disconnects = nil
			end
		end
		function impl.update(_newValue)
			error([[Bindings created by joinBindings(...) cannot be updated directly]], 2)
		end
		function impl.getValue()
			return getValue()
		end

		return setmetatable({
			[Type] = Type.Binding,
			[BindingImpl] = impl,
		}, BindingPublicMeta)
	end

	__DARKLUA_BUNDLE_MODULES.p = BindingInternalApi
end
do
	local Logging = __DARKLUA_BUNDLE_MODULES.m
	local CHANGE_PREFIX = "Change."
	local EventStatus = {
		Disabled = "Disabled",
		Suspended = "Suspended",
		Enabled = "Enabled",
	}
	local SingleEventManager = {}

	SingleEventManager.__index = SingleEventManager

	function SingleEventManager.new(instance)
		local self = setmetatable({
			_suspendedEventQueue = {},
			_connections = {},
			_listeners = {},
			_status = EventStatus.Disabled,
			_isResuming = false,
			_instance = instance,
		}, SingleEventManager)

		return self
	end
	function SingleEventManager:connectEvent(key, listener)
		self:_connect(key, self._instance[key], listener)
	end
	function SingleEventManager:connectPropertyChange(key, listener)
		local success, event = pcall(function()
			return self._instance:GetPropertyChangedSignal(key)
		end)

		if not success then
			error(("Cannot get changed signal on property %q: %s"):format(tostring(key), event), 0)
		end

		self:_connect(CHANGE_PREFIX .. key, event, listener)
	end
	function SingleEventManager:_connect(eventKey, event, listener)
		if listener == nil then
			if self._connections[eventKey] ~= nil then
				self._connections[eventKey]:Disconnect()

				self._connections[eventKey] = nil
			end

			self._listeners[eventKey] = nil
		else
			if self._connections[eventKey] == nil then
				self._connections[eventKey] = event:Connect(function(...)
					if self._status == EventStatus.Enabled then
						self._listeners[eventKey](self._instance, ...)
					elseif self._status == EventStatus.Suspended then
						local argumentCount = select("#", ...)

						table.insert(self._suspendedEventQueue, { eventKey, argumentCount, ... })
					end
				end)
			end

			self._listeners[eventKey] = listener
		end
	end
	function SingleEventManager:suspend()
		self._status = EventStatus.Suspended
	end
	function SingleEventManager:resume()
		if self._isResuming then
			return
		end

		self._isResuming = true

		local index = 1

		while index <= #self._suspendedEventQueue do
			local eventInvocation = self._suspendedEventQueue[index]
			local listener = self._listeners[eventInvocation[1]]
			local argumentCount = eventInvocation[2]

			if listener ~= nil then
				local listenerCo = coroutine.create(listener)
				local success, result =
					coroutine.resume(listenerCo, self._instance, unpack(eventInvocation, 3, 2 + argumentCount))

				if not success then
					Logging.warn("%s", result)
				end
			end

			index = index + 1
		end

		self._isResuming = false
		self._status = EventStatus.Enabled
		self._suspendedEventQueue = {}
	end

	__DARKLUA_BUNDLE_MODULES.q = SingleEventManager
end
do
	local Symbol = __DARKLUA_BUNDLE_MODULES.c
	local Nil = Symbol.named "Nil"
	local _cachedPropertyValues = {}

	local function getDefaultInstanceProperty(className, propertyName)
		local classCache = _cachedPropertyValues[className]

		if classCache then
			local propValue = classCache[propertyName]

			if propValue == Nil then
				return true, nil
			end
			if propValue ~= nil then
				return true, propValue
			end
		else
			classCache = {}
			_cachedPropertyValues[className] = classCache
		end

		local created = Instance.new(className)
		local ok, defaultValue = pcall(function()
			return created[propertyName]
		end)

		created:Destroy()

		if ok then
			if defaultValue == nil then
				classCache[propertyName] = Nil
			else
				classCache[propertyName] = defaultValue
			end
		end

		return ok, defaultValue
	end

	__DARKLUA_BUNDLE_MODULES.r = getDefaultInstanceProperty
end
do
	local Symbol = __DARKLUA_BUNDLE_MODULES.c
	local Ref = Symbol.named "Ref"

	__DARKLUA_BUNDLE_MODULES.s = Ref
end
do
	local Binding = __DARKLUA_BUNDLE_MODULES.p
	local Children = __DARKLUA_BUNDLE_MODULES.j
	local ElementKind = __DARKLUA_BUNDLE_MODULES.h
	local SingleEventManager = __DARKLUA_BUNDLE_MODULES.q
	local getDefaultInstanceProperty = __DARKLUA_BUNDLE_MODULES.r
	local Ref = __DARKLUA_BUNDLE_MODULES.s
	local Type = __DARKLUA_BUNDLE_MODULES.f
	local internalAssert = __DARKLUA_BUNDLE_MODULES.k
	local config = __DARKLUA_BUNDLE_MODULES.b.get()
	local applyPropsError = "Error applying props:\n\t%s\nIn element:\n%s\n"
	local updatePropsError = "Error updating props:\n\t%s\nIn element:\n%s\n"

	local function identity(...)
		return ...
	end
	local function applyRef(ref, newHostObject)
		if ref == nil then
			return
		end
		if typeof(ref) == "function" then
			ref(newHostObject)
		elseif Type.of(ref) == Type.Binding then
			Binding.update(ref, newHostObject)
		else
			error(("Invalid ref: Expected type Binding but got %s"):format(typeof(ref)))
		end
	end
	local function setRobloxInstanceProperty(hostObject, key, newValue)
		if newValue == nil then
			local hostClass = hostObject.ClassName
			local _, defaultValue = getDefaultInstanceProperty(hostClass, key)

			newValue = defaultValue
		end

		hostObject[key] = newValue

		return
	end
	local function removeBinding(virtualNode, key)
		local disconnect = virtualNode.bindings[key]

		disconnect()

		virtualNode.bindings[key] = nil
	end
	local function attachBinding(virtualNode, key, newBinding)
		local function updateBoundProperty(newValue)
			local success, errorMessage = xpcall(function()
				setRobloxInstanceProperty(virtualNode.hostObject, key, newValue)
			end, identity)

			if not success then
				local source = virtualNode.currentElement.source

				if source == nil then
					source = "<enable element tracebacks>"
				end

				local fullMessage = updatePropsError:format(errorMessage, source)

				error(fullMessage, 0)
			end
		end

		if virtualNode.bindings == nil then
			virtualNode.bindings = {}
		end

		virtualNode.bindings[key] = Binding.subscribe(newBinding, updateBoundProperty)

		updateBoundProperty(newBinding:getValue())
	end
	local function detachAllBindings(virtualNode)
		if virtualNode.bindings ~= nil then
			for _, disconnect in pairs(virtualNode.bindings) do
				disconnect()
			end

			virtualNode.bindings = nil
		end
	end
	local function applyProp(virtualNode, key, newValue, oldValue)
		if newValue == oldValue then
			return
		end
		if key == Ref or key == Children then
			return
		end

		local internalKeyType = Type.of(key)

		if internalKeyType == Type.HostEvent or internalKeyType == Type.HostChangeEvent then
			if virtualNode.eventManager == nil then
				virtualNode.eventManager = SingleEventManager.new(virtualNode.hostObject)
			end

			local eventName = key.name

			if internalKeyType == Type.HostChangeEvent then
				virtualNode.eventManager:connectPropertyChange(eventName, newValue)
			else
				virtualNode.eventManager:connectEvent(eventName, newValue)
			end

			return
		end

		local newIsBinding = Type.of(newValue) == Type.Binding
		local oldIsBinding = Type.of(oldValue) == Type.Binding

		if oldIsBinding then
			removeBinding(virtualNode, key)
		end
		if newIsBinding then
			attachBinding(virtualNode, key, newValue)
		else
			setRobloxInstanceProperty(virtualNode.hostObject, key, newValue)
		end
	end
	local function applyProps(virtualNode, props)
		for propKey, value in pairs(props) do
			applyProp(virtualNode, propKey, value, nil)
		end
	end
	local function updateProps(virtualNode, oldProps, newProps)
		for propKey, newValue in pairs(newProps) do
			local oldValue = oldProps[propKey]

			applyProp(virtualNode, propKey, newValue, oldValue)
		end
		for propKey, oldValue in pairs(oldProps) do
			local newValue = newProps[propKey]

			if newValue == nil then
				applyProp(virtualNode, propKey, nil, oldValue)
			end
		end
	end

	local RobloxRenderer = {}

	function RobloxRenderer.isHostObject(target)
		return typeof(target) == "Instance"
	end
	function RobloxRenderer.mountHostNode(reconciler, virtualNode)
		local element = virtualNode.currentElement
		local hostParent = virtualNode.hostParent
		local hostKey = virtualNode.hostKey

		if config.internalTypeChecks then
			internalAssert(ElementKind.of(element) == ElementKind.Host, "Element at given node is not a host Element")
		end
		if config.typeChecks then
			assert(element.props.Name == nil, [[Name can not be specified as a prop to a host component in Roact.]])
			assert(element.props.Parent == nil, [[Parent can not be specified as a prop to a host component in Roact.]])
		end

		local instance = Instance.new(element.component)

		virtualNode.hostObject = instance

		local success, errorMessage = xpcall(function()
			applyProps(virtualNode, element.props)
		end, identity)

		if not success then
			local source = element.source

			if source == nil then
				source = "<enable element tracebacks>"
			end

			local fullMessage = applyPropsError:format(errorMessage, source)

			error(fullMessage, 0)
		end

		instance.Name = tostring(hostKey)

		local children = element.props[Children]

		if children ~= nil then
			reconciler.updateVirtualNodeWithChildren(virtualNode, virtualNode.hostObject, children)
		end

		instance.Parent = hostParent
		virtualNode.hostObject = instance

		applyRef(element.props[Ref], instance)

		if virtualNode.eventManager ~= nil then
			virtualNode.eventManager:resume()
		end
	end
	function RobloxRenderer.unmountHostNode(reconciler, virtualNode)
		local element = virtualNode.currentElement

		applyRef(element.props[Ref], nil)

		for _, childNode in pairs(virtualNode.children) do
			reconciler.unmountVirtualNode(childNode)
		end

		detachAllBindings(virtualNode)
		virtualNode.hostObject:Destroy()
	end
	function RobloxRenderer.updateHostNode(reconciler, virtualNode, newElement)
		local oldProps = virtualNode.currentElement.props
		local newProps = newElement.props

		if virtualNode.eventManager ~= nil then
			virtualNode.eventManager:suspend()
		end
		if oldProps[Ref] ~= newProps[Ref] then
			applyRef(oldProps[Ref], nil)
			applyRef(newProps[Ref], virtualNode.hostObject)
		end

		local success, errorMessage = xpcall(function()
			updateProps(virtualNode, oldProps, newProps)
		end, identity)

		if not success then
			local source = newElement.source

			if source == nil then
				source = "<enable element tracebacks>"
			end

			local fullMessage = updatePropsError:format(errorMessage, source)

			error(fullMessage, 0)
		end

		local children = newElement.props[Children]

		if children ~= nil or oldProps[Children] ~= nil then
			reconciler.updateVirtualNodeWithChildren(virtualNode, virtualNode.hostObject, children)
		end
		if virtualNode.eventManager ~= nil then
			virtualNode.eventManager:resume()
		end

		return virtualNode
	end

	__DARKLUA_BUNDLE_MODULES.t = RobloxRenderer
end
do
	local Symbol = __DARKLUA_BUNDLE_MODULES.c
	local None = Symbol.named "None"

	__DARKLUA_BUNDLE_MODULES.u = None
end
do
	local None = __DARKLUA_BUNDLE_MODULES.u

	local function assign(target, ...)
		for index = 1, select("#", ...) do
			local source = select(index, ...)

			if source ~= nil then
				for key, value in pairs(source) do
					if value == None then
						target[key] = nil
					else
						target[key] = value
					end
				end
			end
		end

		return target
	end

	__DARKLUA_BUNDLE_MODULES.v = assign
end
do
	local Symbol = __DARKLUA_BUNDLE_MODULES.c
	local strict = __DARKLUA_BUNDLE_MODULES.e
	local ComponentLifecyclePhase = strict({
		Init = Symbol.named "init",
		Render = Symbol.named "render",
		ShouldUpdate = Symbol.named "shouldUpdate",
		WillUpdate = Symbol.named "willUpdate",
		DidMount = Symbol.named "didMount",
		DidUpdate = Symbol.named "didUpdate",
		WillUnmount = Symbol.named "willUnmount",
		ReconcileChildren = Symbol.named "reconcileChildren",
		Idle = Symbol.named "idle",
	}, "ComponentLifecyclePhase")

	__DARKLUA_BUNDLE_MODULES.w = ComponentLifecyclePhase
end
do
	local ComponentLifecyclePhase = __DARKLUA_BUNDLE_MODULES.w
	local invalidSetStateMessages = {}

	invalidSetStateMessages[ComponentLifecyclePhase.WillUpdate] =
		[[setState cannot be used in the willUpdate lifecycle method.
Consider using the didUpdate method instead, or using getDerivedStateFromProps.

Check the definition of willUpdate in the component %q.]]
	invalidSetStateMessages[ComponentLifecyclePhase.ShouldUpdate] =
		[[setState cannot be used in the shouldUpdate lifecycle method.
shouldUpdate must be a pure function that only depends on props and state.

Check the definition of shouldUpdate in the component %q.]]
	invalidSetStateMessages[ComponentLifecyclePhase.Render] = [[setState cannot be used in the render method.
render must be a pure function that only depends on props and state.

Check the definition of render in the component %q.]]
	invalidSetStateMessages["default"] = [[setState can not be used in the current situation, because Roact doesn't know
which part of the lifecycle this component is in.

This is a bug in Roact.
It was triggered by the component %q.
]]
	__DARKLUA_BUNDLE_MODULES.x = invalidSetStateMessages
end
do
	local assign = __DARKLUA_BUNDLE_MODULES.v
	local ComponentLifecyclePhase = __DARKLUA_BUNDLE_MODULES.w
	local Type = __DARKLUA_BUNDLE_MODULES.f
	local Symbol = __DARKLUA_BUNDLE_MODULES.c
	local invalidSetStateMessages = __DARKLUA_BUNDLE_MODULES.x
	local internalAssert = __DARKLUA_BUNDLE_MODULES.k
	local config = __DARKLUA_BUNDLE_MODULES.b.get()
	local MAX_PENDING_UPDATES = 100
	local InternalData = Symbol.named "InternalData"
	local componentMissingRenderMessage = [[The component %q is missing the `render` method.
`render` must be defined when creating a Roact component!]]
	local tooManyUpdatesMessage = [[The component %q has reached the setState update recursion limit.
When using `setState` in `didUpdate`, make sure that it won't repeat infinitely!]]
	local componentClassMetatable = {}

	function componentClassMetatable:__tostring()
		return self.__componentName
	end

	local Component = {}

	setmetatable(Component, componentClassMetatable)

	Component[Type] = Type.StatefulComponentClass
	Component.__index = Component
	Component.__componentName = "Component"

	function Component:extend(name)
		if config.typeChecks then
			assert(Type.of(self) == Type.StatefulComponentClass, "Invalid `self` argument to `extend`.")
			assert(typeof(name) == "string", "Component class name must be a string")
		end

		local class = {}

		for key, value in pairs(self) do
			if key ~= "extend" then
				class[key] = value
			end
		end

		class[Type] = Type.StatefulComponentClass
		class.__index = class
		class.__componentName = name

		setmetatable(class, componentClassMetatable)

		return class
	end
	function Component:__getDerivedState(incomingProps, incomingState)
		if config.internalTypeChecks then
			internalAssert(Type.of(self) == Type.StatefulComponentInstance, "Invalid use of `__getDerivedState`")
		end

		local internalData = self[InternalData]
		local componentClass = internalData.componentClass

		if componentClass.getDerivedStateFromProps ~= nil then
			local derivedState = componentClass.getDerivedStateFromProps(incomingProps, incomingState)

			if derivedState ~= nil then
				if config.typeChecks then
					assert(typeof(derivedState) == "table", "getDerivedStateFromProps must return a table!")
				end

				return derivedState
			end
		end

		return nil
	end
	function Component:setState(mapState)
		if config.typeChecks then
			assert(Type.of(self) == Type.StatefulComponentInstance, "Invalid `self` argument to `extend`.")
		end

		local internalData = self[InternalData]
		local lifecyclePhase = internalData.lifecyclePhase

		if
			lifecyclePhase == ComponentLifecyclePhase.ShouldUpdate
			or lifecyclePhase == ComponentLifecyclePhase.WillUpdate
			or lifecyclePhase == ComponentLifecyclePhase.Render
		then
			local messageTemplate = invalidSetStateMessages[internalData.lifecyclePhase]
			local message = messageTemplate:format(tostring(internalData.componentClass))

			error(message, 2)
		elseif lifecyclePhase == ComponentLifecyclePhase.WillUnmount then
			return
		end

		local pendingState = internalData.pendingState
		local partialState

		if typeof(mapState) == "function" then
			partialState = mapState(pendingState or self.state, self.props)

			if partialState == nil then
				return
			end
		elseif typeof(mapState) == "table" then
			partialState = mapState
		else
			error("Invalid argument to setState, expected function or table", 2)
		end

		local newState

		if pendingState ~= nil then
			newState = assign(pendingState, partialState)
		else
			newState = assign({}, self.state, partialState)
		end
		if lifecyclePhase == ComponentLifecyclePhase.Init then
			local derivedState = self:__getDerivedState(self.props, newState)

			self.state = assign(newState, derivedState)
		elseif
			lifecyclePhase == ComponentLifecyclePhase.DidMount
			or lifecyclePhase == ComponentLifecyclePhase.DidUpdate
			or lifecyclePhase == ComponentLifecyclePhase.ReconcileChildren
		then
			local derivedState = self:__getDerivedState(self.props, newState)

			internalData.pendingState = assign(newState, derivedState)
		elseif lifecyclePhase == ComponentLifecyclePhase.Idle then
			self:__update(nil, newState)
		else
			local messageTemplate = invalidSetStateMessages.default
			local message = messageTemplate:format(tostring(internalData.componentClass))

			error(message, 2)
		end
	end
	function Component:getElementTraceback()
		return self[InternalData].virtualNode.currentElement.source
	end
	function Component:render()
		local internalData = self[InternalData]
		local message = componentMissingRenderMessage:format(tostring(internalData.componentClass))

		error(message, 0)
	end
	function Component:__getContext(key)
		if config.internalTypeChecks then
			internalAssert(Type.of(self) == Type.StatefulComponentInstance, "Invalid use of `__getContext`")
			internalAssert(key ~= nil, "Context key cannot be nil")
		end

		local virtualNode = self[InternalData].virtualNode
		local context = virtualNode.context

		return context[key]
	end
	function Component:__addContext(key, value)
		if config.internalTypeChecks then
			internalAssert(Type.of(self) == Type.StatefulComponentInstance, "Invalid use of `__addContext`")
		end

		local virtualNode = self[InternalData].virtualNode

		if virtualNode.originalContext == nil then
			virtualNode.originalContext = virtualNode.context
		end

		local existing = virtualNode.context

		virtualNode.context = assign({}, existing, { [key] = value })
	end
	function Component:__validateProps(props)
		if not config.propValidation then
			return
		end

		local validator = self[InternalData].componentClass.validateProps

		if validator == nil then
			return
		end
		if typeof(validator) ~= "function" then
			error(([[validateProps must be a function, but it is a %s.
Check the definition of the component %q.]]):format(typeof(validator), self.__componentName))
		end

		local success, failureReason = validator(props)

		if not success then
			failureReason = failureReason or "<Validator function did not supply a message>"

			error(
				("Property validation failed in %s: %s\n\n%s"):format(
					self.__componentName,
					tostring(failureReason),
					self:getElementTraceback() or "<enable element tracebacks>"
				),
				0
			)
		end
	end
	function Component:__mount(reconciler, virtualNode)
		if config.internalTypeChecks then
			internalAssert(Type.of(self) == Type.StatefulComponentClass, "Invalid use of `__mount`")
			internalAssert(Type.of(virtualNode) == Type.VirtualNode, "Expected arg #2 to be of type VirtualNode")
		end

		local currentElement = virtualNode.currentElement
		local hostParent = virtualNode.hostParent
		local internalData = {
			reconciler = reconciler,
			virtualNode = virtualNode,
			componentClass = self,
			lifecyclePhase = ComponentLifecyclePhase.Init,
			pendingState = nil,
		}
		local instance = {
			[Type] = Type.StatefulComponentInstance,
			[InternalData] = internalData,
		}

		setmetatable(instance, self)

		virtualNode.instance = instance

		local props = currentElement.props

		if self.defaultProps ~= nil then
			props = assign({}, self.defaultProps, props)
		end

		instance:__validateProps(props)

		instance.props = props

		local newContext = assign({}, virtualNode.legacyContext)

		instance._context = newContext
		instance.state = assign({}, instance:__getDerivedState(instance.props, {}))

		if instance.init ~= nil then
			instance:init(instance.props)
			assign(instance.state, instance:__getDerivedState(instance.props, instance.state))
		end

		virtualNode.legacyContext = instance._context
		internalData.lifecyclePhase = ComponentLifecyclePhase.Render

		local renderResult = instance:render()

		internalData.lifecyclePhase = ComponentLifecyclePhase.ReconcileChildren

		reconciler.updateVirtualNodeWithRenderResult(virtualNode, hostParent, renderResult)

		if instance.didMount ~= nil then
			internalData.lifecyclePhase = ComponentLifecyclePhase.DidMount

			instance:didMount()
		end
		if internalData.pendingState ~= nil then
			instance:__update(nil, nil)
		end

		internalData.lifecyclePhase = ComponentLifecyclePhase.Idle
	end
	function Component:__unmount()
		if config.internalTypeChecks then
			internalAssert(Type.of(self) == Type.StatefulComponentInstance, "Invalid use of `__unmount`")
		end

		local internalData = self[InternalData]
		local virtualNode = internalData.virtualNode
		local reconciler = internalData.reconciler

		if self.willUnmount ~= nil then
			internalData.lifecyclePhase = ComponentLifecyclePhase.WillUnmount

			self:willUnmount()
		end

		for _, childNode in pairs(virtualNode.children) do
			reconciler.unmountVirtualNode(childNode)
		end
	end
	function Component:__update(updatedElement, updatedState)
		if config.internalTypeChecks then
			internalAssert(Type.of(self) == Type.StatefulComponentInstance, "Invalid use of `__update`")
			internalAssert(
				Type.of(updatedElement) == Type.Element or updatedElement == nil,
				"Expected arg #1 to be of type Element or nil"
			)
			internalAssert(
				typeof(updatedState) == "table" or updatedState == nil,
				"Expected arg #2 to be of type table or nil"
			)
		end

		local internalData = self[InternalData]
		local componentClass = internalData.componentClass
		local newProps = self.props

		if updatedElement ~= nil then
			newProps = updatedElement.props

			if componentClass.defaultProps ~= nil then
				newProps = assign({}, componentClass.defaultProps, newProps)
			end

			self:__validateProps(newProps)
		end

		local updateCount = 0

		repeat
			local finalState
			local pendingState = nil

			if internalData.pendingState ~= nil then
				pendingState = internalData.pendingState
				internalData.pendingState = nil
			end
			if updatedState ~= nil or newProps ~= self.props then
				if pendingState == nil then
					finalState = updatedState or self.state
				else
					finalState = assign(pendingState, updatedState)
				end

				local derivedState = self:__getDerivedState(newProps, finalState)

				if derivedState ~= nil then
					finalState = assign({}, finalState, derivedState)
				end

				updatedState = nil
			else
				finalState = pendingState
			end
			if not self:__resolveUpdate(newProps, finalState) then
				return false
			end

			updateCount = updateCount + 1

			if updateCount > MAX_PENDING_UPDATES then
				error(tooManyUpdatesMessage:format(tostring(internalData.componentClass)), 3)
			end
		until internalData.pendingState == nil

		return true
	end
	function Component:__resolveUpdate(incomingProps, incomingState)
		if config.internalTypeChecks then
			internalAssert(Type.of(self) == Type.StatefulComponentInstance, "Invalid use of `__resolveUpdate`")
		end

		local internalData = self[InternalData]
		local virtualNode = internalData.virtualNode
		local reconciler = internalData.reconciler
		local oldProps = self.props
		local oldState = self.state

		if incomingProps == nil then
			incomingProps = oldProps
		end
		if incomingState == nil then
			incomingState = oldState
		end
		if self.shouldUpdate ~= nil then
			internalData.lifecyclePhase = ComponentLifecyclePhase.ShouldUpdate

			local continueWithUpdate = self:shouldUpdate(incomingProps, incomingState)

			if not continueWithUpdate then
				internalData.lifecyclePhase = ComponentLifecyclePhase.Idle

				return false
			end
		end
		if self.willUpdate ~= nil then
			internalData.lifecyclePhase = ComponentLifecyclePhase.WillUpdate

			self:willUpdate(incomingProps, incomingState)
		end

		internalData.lifecyclePhase = ComponentLifecyclePhase.Render
		self.props = incomingProps
		self.state = incomingState

		local renderResult = virtualNode.instance:render()

		internalData.lifecyclePhase = ComponentLifecyclePhase.ReconcileChildren

		reconciler.updateVirtualNodeWithRenderResult(virtualNode, virtualNode.hostParent, renderResult)

		if self.didUpdate ~= nil then
			internalData.lifecyclePhase = ComponentLifecyclePhase.DidUpdate

			self:didUpdate(oldProps, oldState)
		end

		internalData.lifecyclePhase = ComponentLifecyclePhase.Idle

		return true
	end

	__DARKLUA_BUNDLE_MODULES.y = Component
end
do
	local Children = __DARKLUA_BUNDLE_MODULES.j
	local ElementKind = __DARKLUA_BUNDLE_MODULES.h
	local Logging = __DARKLUA_BUNDLE_MODULES.m
	local Type = __DARKLUA_BUNDLE_MODULES.f
	local config = __DARKLUA_BUNDLE_MODULES.b.get()
	local multipleChildrenMessage =
		'The prop `Roact.Children` was defined but was overridden by the third parameter to createElement!\nThis can happen when a component passes props through to a child element but also uses the `children` argument:\n\n\tRoact.createElement("Frame", passedProps, {\n\t\tchild = ...\n\t})\n\nInstead, consider using a utility function to merge tables of children together:\n\n\tlocal children = mergeTables(passedProps[Roact.Children], {\n\t\tchild = ...\n\t})\n\n\tlocal fullProps = mergeTables(passedProps, {\n\t\t[Roact.Children] = children\n\t})\n\n\tRoact.createElement("Frame", fullProps)'

	local function createElement(component, props, children)
		if config.typeChecks then
			assert(component ~= nil, "`component` is required")
			assert(typeof(props) == "table" or props == nil, "`props` must be a table or nil")
			assert(typeof(children) == "table" or children == nil, "`children` must be a table or nil")
		end
		if props == nil then
			props = {}
		end
		if children ~= nil then
			if props[Children] ~= nil then
				Logging.warnOnce(multipleChildrenMessage)
			end

			props[Children] = children
		end

		local elementKind = ElementKind.fromComponent(component)
		local element = {
			[Type] = Type.Element,
			[ElementKind] = elementKind,
			component = component,
			props = props,
		}

		if config.elementTracing then
			element.source = debug.traceback("", 2):sub(2)
		end

		return element
	end

	__DARKLUA_BUNDLE_MODULES.z = createElement
end
do
	local ElementKind = __DARKLUA_BUNDLE_MODULES.h
	local Type = __DARKLUA_BUNDLE_MODULES.f

	local function createFragment(elements)
		return {
			[Type] = Type.Element,
			[ElementKind] = ElementKind.Fragment,
			elements = elements,
		}
	end

	__DARKLUA_BUNDLE_MODULES.A = createFragment
end
do
	local function oneChild(children)
		if not children then
			return nil
		end

		local key, child = next(children)

		if not child then
			return nil
		end

		local after = next(children, key)

		if after then
			error("Expected at most child, had more than one child.", 2)
		end

		return child
	end

	__DARKLUA_BUNDLE_MODULES.B = oneChild
end
do
	local Component = __DARKLUA_BUNDLE_MODULES.y
	local PureComponent = Component:extend "PureComponent"

	PureComponent.extend = Component.extend

	function PureComponent:shouldUpdate(newProps, newState)
		if newState ~= self.state then
			return true
		end
		if newProps == self.props then
			return false
		end

		for key, value in pairs(newProps) do
			if self.props[key] ~= value then
				return true
			end
		end
		for key, value in pairs(self.props) do
			if newProps[key] ~= value then
				return true
			end
		end

		return false
	end

	__DARKLUA_BUNDLE_MODULES.C = PureComponent
end
do
	local Binding = __DARKLUA_BUNDLE_MODULES.p

	local function createRef()
		local binding, _ = Binding.create(nil)
		local ref = {}

		setmetatable(ref, {
			__index = function(_self, key)
				if key == "current" then
					return binding:getValue()
				else
					return binding[key]
				end
			end,
			__newindex = function(_self, key, value)
				if key == "current" then
					error("Cannot assign to the 'current' property of refs", 2)
				end

				binding[key] = value
			end,
			__tostring = function(_self)
				return ("RoactRef(%s)"):format(tostring(binding:getValue()))
			end,
		})

		return ref
	end

	__DARKLUA_BUNDLE_MODULES.D = createRef
end
do
	local assign = __DARKLUA_BUNDLE_MODULES.v
	local None = __DARKLUA_BUNDLE_MODULES.u
	local Ref = __DARKLUA_BUNDLE_MODULES.s
	local config = __DARKLUA_BUNDLE_MODULES.b.get()
	local excludeRef = { [Ref] = None }

	local function forwardRef(render)
		if config.typeChecks then
			assert(typeof(render) == "function", "Expected arg #1 to be a function")
		end

		return function(props)
			local ref = props[Ref]
			local propsWithoutRef = assign({}, props, excludeRef)

			return render(propsWithoutRef, ref)
		end
	end

	__DARKLUA_BUNDLE_MODULES.E = forwardRef
end
do
	local Symbol = __DARKLUA_BUNDLE_MODULES.c
	local createFragment = __DARKLUA_BUNDLE_MODULES.A
	local createSignal = __DARKLUA_BUNDLE_MODULES.o
	local Children = __DARKLUA_BUNDLE_MODULES.j
	local Component = __DARKLUA_BUNDLE_MODULES.y

	local function createContextEntry(currentValue)
		return {
			value = currentValue,
			onUpdate = createSignal(),
		}
	end
	local function createProvider(context)
		local Provider = Component:extend "Provider"

		function Provider:init(props)
			self.contextEntry = createContextEntry(props.value)

			self:__addContext(context.key, self.contextEntry)
		end
		function Provider:willUpdate(nextProps)
			if nextProps.value ~= self.props.value then
				self.contextEntry.value = nextProps.value
			end
		end
		function Provider:didUpdate(prevProps)
			if prevProps.value ~= self.props.value then
				self.contextEntry.onUpdate:fire(self.props.value)
			end
		end
		function Provider:render()
			return createFragment(self.props[Children])
		end

		return Provider
	end
	local function createConsumer(context)
		local Consumer = Component:extend "Consumer"

		function Consumer.validateProps(props)
			if type(props.render) ~= "function" then
				return false, "Consumer expects a `render` function"
			else
				return true
			end
		end
		function Consumer:init(_props)
			self.contextEntry = self:__getContext(context.key)
		end
		function Consumer:render()
			local value

			if self.contextEntry ~= nil then
				value = self.contextEntry.value
			else
				value = context.defaultValue
			end

			return self.props.render(value)
		end
		function Consumer:didUpdate()
			if self.contextEntry ~= nil then
				self.lastValue = self.contextEntry.value
			end
		end
		function Consumer:didMount()
			if self.contextEntry ~= nil then
				self.disconnect = self.contextEntry.onUpdate:subscribe(function(newValue)
					if newValue ~= self.lastValue then
						self:setState {}
					end
				end)
			end
		end
		function Consumer:willUnmount()
			if self.disconnect ~= nil then
				self.disconnect()

				self.disconnect = nil
			end
		end

		return Consumer
	end

	local Context = {}

	Context.__index = Context

	function Context.new(defaultValue)
		return setmetatable({
			defaultValue = defaultValue,
			key = Symbol.named "ContextKey",
		}, Context)
	end
	function Context:__tostring()
		return "RoactContext"
	end

	local function createContext(defaultValue)
		local context = Context.new(defaultValue)

		return {
			Provider = createProvider(context),
			Consumer = createConsumer(context),
		}
	end

	__DARKLUA_BUNDLE_MODULES.F = createContext
end
do
	local Type = __DARKLUA_BUNDLE_MODULES.f
	local Change = {}
	local changeMetatable = {
		__tostring = function(self)
			return ("RoactHostChangeEvent(%s)"):format(self.name)
		end,
	}

	setmetatable(Change, {
		__index = function(_self, propertyName)
			local changeListener = {
				[Type] = Type.HostChangeEvent,
				name = propertyName,
			}

			setmetatable(changeListener, changeMetatable)

			Change[propertyName] = changeListener

			return changeListener
		end,
	})

	__DARKLUA_BUNDLE_MODULES.G = Change
end
do
	local Type = __DARKLUA_BUNDLE_MODULES.f
	local Event = {}
	local eventMetatable = {
		__tostring = function(self)
			return ("RoactHostEvent(%s)"):format(self.name)
		end,
	}

	setmetatable(Event, {
		__index = function(_self, eventName)
			local event = {
				[Type] = Type.HostEvent,
				name = eventName,
			}

			setmetatable(event, eventMetatable)

			Event[eventName] = event

			return event
		end,
	})

	__DARKLUA_BUNDLE_MODULES.H = Event
end

local GlobalConfig = __DARKLUA_BUNDLE_MODULES.b
local createReconciler = __DARKLUA_BUNDLE_MODULES.l
local createReconcilerCompat = __DARKLUA_BUNDLE_MODULES.n
local RobloxRenderer = __DARKLUA_BUNDLE_MODULES.t
local strict = __DARKLUA_BUNDLE_MODULES.e
local Binding = __DARKLUA_BUNDLE_MODULES.p
local robloxReconciler = createReconciler(RobloxRenderer)
local reconcilerCompat = createReconcilerCompat(robloxReconciler)
local Roact = strict {
	Component = __DARKLUA_BUNDLE_MODULES.y,
	createElement = __DARKLUA_BUNDLE_MODULES.z,
	createFragment = __DARKLUA_BUNDLE_MODULES.A,
	oneChild = __DARKLUA_BUNDLE_MODULES.B,
	PureComponent = __DARKLUA_BUNDLE_MODULES.C,
	None = __DARKLUA_BUNDLE_MODULES.u,
	Portal = __DARKLUA_BUNDLE_MODULES.g,
	createRef = __DARKLUA_BUNDLE_MODULES.D,
	forwardRef = __DARKLUA_BUNDLE_MODULES.E,
	createBinding = Binding.create,
	joinBindings = Binding.join,
	createContext = __DARKLUA_BUNDLE_MODULES.F,
	Change = __DARKLUA_BUNDLE_MODULES.G,
	Children = __DARKLUA_BUNDLE_MODULES.j,
	Event = __DARKLUA_BUNDLE_MODULES.H,
	Ref = __DARKLUA_BUNDLE_MODULES.s,
	mount = robloxReconciler.mountVirtualTree,
	unmount = robloxReconciler.unmountVirtualTree,
	update = robloxReconciler.updateVirtualTree,
	reify = reconcilerCompat.reify,
	teardown = reconcilerCompat.teardown,
	reconcile = reconcilerCompat.reconcile,
	setGlobalConfig = GlobalConfig.set,
	UNSTABLE = {},
}

return Roact
