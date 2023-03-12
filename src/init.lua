-- The entirety of Roact as a single file:

local _assertDeepEqual, _assign, _Binding, _Change, _Children, _Component, _ComponentLifecyclePhase, _Config, _createContext, _createElement, _createFragment, _createReconciler, _createReconcilerCompat, _createRef, _createSignal, _createSpy, _ElementKind, _ElementUtils, _Event, _forwardRef, _getDefaultInstanceProperty, _GlobalConfig, _internalAssert, _invalidSetStateMessages, _Logging, _None, _NoopRenderer, _oneChild, _Portal, _PureComponent, _Ref, _RobloxRenderer, _SingleEventManager, _strict, _Symbol, _Type
--######################################--
_assign = function()
	local None = _None()

	--[[
	Merges values from zero or more tables onto a target table. If a value is
	set to None, it will instead be removed from the table.

	This function is identical in functionality to JavaScript's Object.assign.
	]]
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

	return assign
end
--######################################--
_Binding = function()
	local createSignal = _createSignal()
	local Symbol = _Symbol()
	local Type = _Type()

	local config = _GlobalConfig().get()

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
			error("Bindings created by Binding:map(fn) cannot be updated directly", 2)
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
					local message = ("Expected arg #1 to contain only bindings, but key %q had a non-binding value"):format(
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
			error("Bindings created by joinBindings(...) cannot be updated directly", 2)
		end

		function impl.getValue()
			return getValue()
		end

		return setmetatable({
			[Type] = Type.Binding,
			[BindingImpl] = impl,
		}, BindingPublicMeta)
	end

	return BindingInternalApi
end
--######################################--
_Change = function()
	--[[
	Change is used to generate special prop keys that can be used to connect to
	GetPropertyChangedSignal.

	Generally, Change is indexed by a Roblox property name:

		Roact.createElement("TextBox", {
			[Roact.Change.Text] = function(rbx)
				print("The TextBox", rbx, "changed text to", rbx.Text)
			end,
		})
	]]

	local Type = _Type()

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

	return Change
end
--######################################--
_Children = function()
	local Symbol = _Symbol()

	local Children = Symbol.named "Children"

	return Children
end
--######################################--
_Component = function()
	local assign = _assign()
	local ComponentLifecyclePhase = _ComponentLifecyclePhase()
	local Type = _Type()
	local Symbol = _Symbol()
	local invalidSetStateMessages = _invalidSetStateMessages()
	local internalAssert = _internalAssert()

	local config = _GlobalConfig().get()

	--[[
	Calling setState during certain lifecycle allowed methods has the potential
	to create an infinitely updating component. Rather than time out, we exit
	with an error if an unreasonable number of self-triggering updates occur
]]
	local MAX_PENDING_UPDATES = 100

	local InternalData = Symbol.named "InternalData"

	local componentMissingRenderMessage = [[
The component %q is missing the `render` method.
`render` must be defined when creating a Roact component!]]

	local tooManyUpdatesMessage = [[
The component %q has reached the setState update recursion limit.
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

	--[[
	A method called by consumers of Roact to create a new component class.
	Components can not be extended beyond this point, with the exception of
	PureComponent.
]]
	function Component:extend(name)
		if config.typeChecks then
			assert(Type.of(self) == Type.StatefulComponentClass, "Invalid `self` argument to `extend`.")
			assert(typeof(name) == "string", "Component class name must be a string")
		end

		local class = {}

		for key, value in pairs(self) do
			-- Roact opts to make consumers use composition over inheritance, which
			-- lines up with React.
			-- https://reactjs.org/docs/composition-vs-inheritance.html
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

		--[[
		When preparing to update, render, or unmount, it is not safe
		to call `setState` as it will interfere with in-flight updates. It's
		also disallowed during unmounting
	]]
		if
			lifecyclePhase == ComponentLifecyclePhase.ShouldUpdate
			or lifecyclePhase == ComponentLifecyclePhase.WillUpdate
			or lifecyclePhase == ComponentLifecyclePhase.Render
		then
			local messageTemplate = invalidSetStateMessages[internalData.lifecyclePhase]

			local message = messageTemplate:format(tostring(internalData.componentClass))
			error(message, 2)
		elseif lifecyclePhase == ComponentLifecyclePhase.WillUnmount then
			-- Should not print error message. See https://github.com/facebook/react/pull/22114
			return
		end

		local pendingState = internalData.pendingState

		local partialState
		if typeof(mapState) == "function" then
			partialState = mapState(pendingState or self.state, self.props)

			-- Abort the state update if the given state updater function returns nil
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
			-- If `setState` is called in `init`, we can skip triggering an update!
			local derivedState = self:__getDerivedState(self.props, newState)
			self.state = assign(newState, derivedState)
		elseif
			lifecyclePhase == ComponentLifecyclePhase.DidMount
			or lifecyclePhase == ComponentLifecyclePhase.DidUpdate
			or lifecyclePhase == ComponentLifecyclePhase.ReconcileChildren
		then
			--[[
			During certain phases of the component lifecycle, it's acceptable to
			allow `setState` but defer the update until we're done with ones in flight.
			We do this by collapsing it into any pending updates we have.
		]]
			local derivedState = self:__getDerivedState(self.props, newState)
			internalData.pendingState = assign(newState, derivedState)
		elseif lifecyclePhase == ComponentLifecyclePhase.Idle then
			-- Outside of our lifecycle, the state update is safe to make immediately
			self:__update(nil, newState)
		else
			local messageTemplate = invalidSetStateMessages.default

			local message = messageTemplate:format(tostring(internalData.componentClass))

			error(message, 2)
		end
	end

	--[[
	Returns the stack trace of where the element was created that this component
	instance's properties are based on.

	Intended to be used primarily by diagnostic tools.
]]
	function Component:getElementTraceback()
		return self[InternalData].virtualNode.currentElement.source
	end

	--[[
	Returns a snapshot of this component given the current props and state. Must
	be overridden by consumers of Roact and should be a pure function with
	regards to props and state.

	TODO (#199): Accept props and state as arguments.
]]
	function Component:render()
		local internalData = self[InternalData]

		local message = componentMissingRenderMessage:format(tostring(internalData.componentClass))

		error(message, 0)
	end

	--[[
	Retrieves the context value corresponding to the given key. Can return nil
	if a requested context key is not present
]]
	function Component:__getContext(key)
		if config.internalTypeChecks then
			internalAssert(Type.of(self) == Type.StatefulComponentInstance, "Invalid use of `__getContext`")
			internalAssert(key ~= nil, "Context key cannot be nil")
		end

		local virtualNode = self[InternalData].virtualNode
		local context = virtualNode.context

		return context[key]
	end

	--[[
	Adds a new context entry to this component's context table (which will be
	passed down to child components).
]]
	function Component:__addContext(key, value)
		if config.internalTypeChecks then
			internalAssert(Type.of(self) == Type.StatefulComponentInstance, "Invalid use of `__addContext`")
		end
		local virtualNode = self[InternalData].virtualNode

		-- Make sure we store a reference to the component's original, unmodified
		-- context the virtual node. In the reconciler, we'll restore the original
		-- context if we need to replace the node (this happens when a node gets
		-- re-rendered as a different component)
		if virtualNode.originalContext == nil then
			virtualNode.originalContext = virtualNode.context
		end

		-- Build a new context table on top of the existing one, then apply it to
		-- our virtualNode
		local existing = virtualNode.context
		virtualNode.context = assign({}, existing, { [key] = value })
	end

	--[[
	Performs property validation if the static method validateProps is declared.
	validateProps should follow assert's expected arguments:
	(false, message: string) | true. The function may return a message in the
	true case; it will be ignored. If this fails, the function will throw the
	error.
]]
	function Component:__validateProps(props)
		if not config.propValidation then
			return
		end

		local validator = self[InternalData].componentClass.validateProps

		if validator == nil then
			return
		end

		if typeof(validator) ~= "function" then
			error(
				("validateProps must be a function, but it is a %s.\nCheck the definition of the component %q."):format(
					typeof(validator),
					self.__componentName
				)
			)
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

	--[[
	An internal method used by the reconciler to construct a new component
	instance and attach it to the given virtualNode.
]]
	function Component:__mount(reconciler, virtualNode)
		if config.internalTypeChecks then
			internalAssert(Type.of(self) == Type.StatefulComponentClass, "Invalid use of `__mount`")
			internalAssert(Type.of(virtualNode) == Type.VirtualNode, "Expected arg #2 to be of type VirtualNode")
		end

		local currentElement = virtualNode.currentElement
		local hostParent = virtualNode.hostParent

		-- Contains all the information that we want to keep from consumers of
		-- Roact, or even other parts of the codebase like the reconciler.
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

		-- It's possible for init() to redefine _context!
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
			-- __update will handle pendingState, so we don't pass any new element or state
			instance:__update(nil, nil)
		end

		internalData.lifecyclePhase = ComponentLifecyclePhase.Idle
	end

	--[[
	Internal method used by the reconciler to clean up any resources held by
	this component instance.
]]
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

	--[[
	Internal method used by setState (to trigger updates based on state) and by
	the reconciler (to trigger updates based on props)

	Returns true if the update was completed, false if it was cancelled by shouldUpdate
]]
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

			-- Consume any pending state we might have
			if internalData.pendingState ~= nil then
				pendingState = internalData.pendingState
				internalData.pendingState = nil
			end

			-- Consume a standard update to state or props
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
				-- If the update was short-circuited, bubble the result up to the caller
				return false
			end

			updateCount = updateCount + 1

			if updateCount > MAX_PENDING_UPDATES then
				error(tooManyUpdatesMessage:format(tostring(internalData.componentClass)), 3)
			end
		until internalData.pendingState == nil

		return true
	end

	--[[
	Internal method used by __update to apply new props and state

	Returns true if the update was completed, false if it was cancelled by shouldUpdate
]]
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

	return Component
end
--######################################--
_ComponentLifecyclePhase = function()
	local Symbol = _Symbol()
	local strict = _strict()

	local ComponentLifecyclePhase = strict({
		-- Component methods
		Init = Symbol.named "init",
		Render = Symbol.named "render",
		ShouldUpdate = Symbol.named "shouldUpdate",
		WillUpdate = Symbol.named "willUpdate",
		DidMount = Symbol.named "didMount",
		DidUpdate = Symbol.named "didUpdate",
		WillUnmount = Symbol.named "willUnmount",

		-- Phases describing reconciliation status
		ReconcileChildren = Symbol.named "reconcileChildren",
		Idle = Symbol.named "idle",
	}, "ComponentLifecyclePhase")

	return ComponentLifecyclePhase
end
--######################################--
_Config = function()
	--[[
	Exposes an interface to set global configuration values for Roact.

	Configuration can only occur once, and should only be done by an application
	using Roact, not a library.

	Any keys that aren't recognized will cause errors. Configuration is only
	intended for configuring Roact itself, not extensions or libraries.

	Configuration is expected to be set immediately after loading Roact. Setting
	configuration values after an application starts may produce unpredictable
	behavior.
	]]

	-- Every valid configuration value should be non-nil in this table.
	local defaultConfig = {
		-- Enables asserts for internal Roact APIs. Useful for debugging Roact itself.
		["internalTypeChecks"] = false,
		-- Enables stricter type asserts for Roact's public API.
		["typeChecks"] = false,
		-- Enables storage of `debug.traceback()` values on elements for debugging.
		["elementTracing"] = false,
		-- Enables validation of component props in stateful components.
		["propValidation"] = false,
	}

	-- Build a list of valid configuration values up for debug messages.
	local defaultConfigKeys = {}
	for key in pairs(defaultConfig) do
		table.insert(defaultConfigKeys, key)
	end

	local Config = {}

	function Config.new()
		local self = {}

		self._currentConfig = setmetatable({}, {
			__index = function(_, key)
				local message = ("Invalid global configuration key %q. Valid configuration keys are: %s"):format(
					tostring(key),
					table.concat(defaultConfigKeys, ", ")
				)

				error(message, 3)
			end,
		})

		-- We manually bind these methods here so that the Config's methods can be
		-- used without passing in self, since they eventually get exposed on the
		-- root Roact object.
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
		-- Validate values without changing any configuration.
		-- We only want to apply this configuration if it's valid!
		for key, value in pairs(configValues) do
			if defaultConfig[key] == nil then
				local message = ("Invalid global configuration key %q (type %s). Valid configuration keys are: %s"):format(
					tostring(key),
					typeof(key),
					table.concat(defaultConfigKeys, ", ")
				)

				error(message, 3)
			end

			-- Right now, all configuration values must be boolean.
			if typeof(value) ~= "boolean" then
				local message = ("Invalid value %q (type %s) for global configuration key %q. Valid values are: true, false"):format(
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
		local previousValues = {}
		for key, value in pairs(self._currentConfig) do
			previousValues[key] = value
		end

		self.set(configValues)

		local success, result = pcall(callback)

		self.set(previousValues)

		assert(success, result)
	end

	return Config
end
--######################################--
_createContext = function()
	local Symbol = _Symbol()
	local createFragment = _createFragment()
	local createSignal = _createSignal()
	local Children = _Children()
	local Component = _Component()

	--[[
	Construct the value that is assigned to Roact's context storage.
	]]
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
			-- If the provided value changed, immediately update the context entry.
			--
			-- During this update, any components that are reachable will receive
			-- this updated value at the same time as any props and state updates
			-- that are being applied.
			if nextProps.value ~= self.props.value then
				self.contextEntry.value = nextProps.value
			end
		end

		function Provider:didUpdate(prevProps)
			-- If the provided value changed, after we've updated every reachable
			-- component, fire a signal to update the rest.
			--
			-- This signal will notify all context consumers. It's expected that
			-- they will compare the last context value they updated with and only
			-- trigger an update on themselves if this value is different.
			--
			-- This codepath will generally only update consumer components that has
			-- a component implementing shouldUpdate between them and the provider.
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
			-- This value may be nil, which indicates that our consumer is not a
			-- descendant of a provider for this context item.
			self.contextEntry = self:__getContext(context.key)
		end

		function Consumer:render()
			-- Render using the latest available for this context item.
			--
			-- We don't store this value in state in order to have more fine-grained
			-- control over our update behavior.
			local value
			if self.contextEntry ~= nil then
				value = self.contextEntry.value
			else
				value = context.defaultValue
			end

			return self.props.render(value)
		end

		function Consumer:didUpdate()
			-- Store the value that we most recently updated with.
			--
			-- This value is compared in the contextEntry onUpdate hook below.
			if self.contextEntry ~= nil then
				self.lastValue = self.contextEntry.value
			end
		end

		function Consumer:didMount()
			if self.contextEntry ~= nil then
				-- When onUpdate is fired, a new value has been made available in
				-- this context entry, but we may have already updated in the same
				-- update cycle.
				--
				-- To avoid sending a redundant update, we compare the new value
				-- with the last value that we updated with (set in didUpdate) and
				-- only update if they differ. This may happen when an update from a
				-- provider was blocked by an intermediate component that returned
				-- false from shouldUpdate.
				self.disconnect = self.contextEntry.onUpdate:subscribe(function(newValue)
					if newValue ~= self.lastValue then
						-- Trigger a dummy state update.
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

	return createContext
end
--######################################--
_createElement = function()
	local Children = _Children()
	local ElementKind = _ElementKind()
	local Logging = _Logging()
	local Type = _Type()

	local config = _GlobalConfig().get()

	local multipleChildrenMessage = [[
The prop `Roact.Children` was defined but was overridden by the third parameter to createElement!
This can happen when a component passes props through to a child element but also uses the `children` argument:

	Roact.createElement("Frame", passedProps, {
		child = ...
	})

Instead, consider using a utility function to merge tables of children together:

	local children = mergeTables(passedProps[Roact.Children], {
		child = ...
	})

	local fullProps = mergeTables(passedProps, {
		[Roact.Children] = children
	})

	Roact.createElement("Frame", fullProps)]]

	--[[
	Creates a new element representing the given component.

	Elements are lightweight representations of what a component instance should
	look like.

	Children is a shorthand for specifying `Roact.Children` as a key inside
	props. If specified, the passed `props` table is mutated!
	]]
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
			-- We trim out the leading newline since there's no way to specify the
			-- trace level without also specifying a message.
			element.source = debug.traceback("", 2):sub(2)
		end

		return element
	end

	return createElement
end
--######################################--
_createFragment = function()
	local ElementKind = _ElementKind()
	local Type = _Type()

	local function createFragment(elements)
		return {
			[Type] = Type.Element,
			[ElementKind] = ElementKind.Fragment,
			elements = elements,
		}
	end

	return createFragment
end
--######################################--
_createReconciler = function()
	--!nonstrict
	local Type = _Type()
	local ElementKind = _ElementKind()
	local ElementUtils = _ElementUtils()
	local Children = _Children()
	local Symbol = _Symbol()
	local internalAssert = _internalAssert()

	local config = _GlobalConfig().get()

	local InternalData = Symbol.named "InternalData"

	--[[
	The reconciler is the mechanism in Roact that constructs the virtual tree
	that later gets turned into concrete objects by the renderer.

	Roact's reconciler is constructed with the renderer as an argument, which
	enables switching to different renderers for different platforms or
	scenarios.

	When testing the reconciler itself, it's common to use `NoopRenderer` with
	spies replacing some methods. The default (and only) reconciler interface
	exposed by Roact right now uses `RobloxRenderer`.
	]]
	local function createReconciler(renderer)
		local reconciler
		local mountVirtualNode
		local updateVirtualNode
		local unmountVirtualNode

		--[[
		Unmount the given virtualNode, replacing it with a new node described by
		the given element.

		Preserves host properties, depth, and legacyContext from parent.
	]]
		local function replaceVirtualNode(virtualNode, newElement)
			local hostParent = virtualNode.hostParent
			local hostKey = virtualNode.hostKey
			local depth = virtualNode.depth
			local parent = virtualNode.parent

			-- If the node that is being replaced has modified context, we need to
			-- use the original *unmodified* context for the new node
			-- The `originalContext` field will be nil if the context was unchanged
			local context = virtualNode.originalContext or virtualNode.context
			local parentLegacyContext = virtualNode.parentLegacyContext

			-- If updating this node has caused a component higher up the tree to re-render
			-- and updateChildren to be re-entered then this node could already have been
			-- unmounted in the previous updateChildren pass.
			if not virtualNode.wasUnmounted then
				unmountVirtualNode(virtualNode)
			end
			local newNode = mountVirtualNode(newElement, hostParent, hostKey, context, parentLegacyContext)

			-- mountVirtualNode can return nil if the element is a boolean
			if newNode ~= nil then
				newNode.depth = depth
				newNode.parent = parent
			end

			return newNode
		end

		--[[
		Utility to update the children of a virtual node based on zero or more
		updated children given as elements.
	]]
		local function updateChildren(virtualNode, hostParent, newChildElements)
			if config.internalTypeChecks then
				internalAssert(Type.of(virtualNode) == Type.VirtualNode, "Expected arg #1 to be of type VirtualNode")
			end

			virtualNode.updateChildrenCount = virtualNode.updateChildrenCount + 1

			local currentUpdateChildrenCount = virtualNode.updateChildrenCount

			local removeKeys = {}

			-- Changed or removed children
			for childKey, childNode in pairs(virtualNode.children) do
				local newElement = ElementUtils.getElementByKey(newChildElements, childKey)
				local newNode = updateVirtualNode(childNode, newElement)

				-- If updating this node has caused a component higher up the tree to re-render
				-- and updateChildren to be re-entered for this virtualNode then
				-- this result is invalid and needs to be disgarded.
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

			-- Added children
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

					-- If updating this node has caused a component higher up the tree to re-render
					-- and updateChildren to be re-entered for this virtualNode then
					-- this result is invalid and needs to be discarded.
					if virtualNode.updateChildrenCount ~= currentUpdateChildrenCount then
						if childNode then
							unmountVirtualNode(childNode)
						end
						return
					end

					-- mountVirtualNode can return nil if the element is a boolean
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

		--[[
		Unmounts the given virtual node and releases any held resources.
	]]
		function unmountVirtualNode(virtualNode)
			if config.internalTypeChecks then
				internalAssert(Type.of(virtualNode) == Type.VirtualNode, "Expected arg #1 to be of type VirtualNode")
			end

			virtualNode.wasUnmounted = true

			local kind = ElementKind.of(virtualNode.currentElement)

			-- selene: allow(if_same_then_else)
			if kind == ElementKind.Host then
				renderer.unmountHostNode(reconciler, virtualNode)
			elseif kind == ElementKind.Function then
				for _, childNode in pairs(virtualNode.children) do
					unmountVirtualNode(childNode)
				end
			elseif kind == ElementKind.Stateful then
				virtualNode.instance:__unmount()
			elseif kind == ElementKind.Portal then
				for _, childNode in pairs(virtualNode.children) do
					unmountVirtualNode(childNode)
				end
			elseif kind == ElementKind.Fragment then
				for _, childNode in pairs(virtualNode.children) do
					unmountVirtualNode(childNode)
				end
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

		--[[
		Update the given virtual node using a new element describing what it
		should transform into.

		`updateVirtualNode` will return a new virtual node that should replace
		the passed in virtual node. This is because a virtual node can be
		updated with an element referencing a different component!

		In that case, `updateVirtualNode` will unmount the input virtual node,
		mount a new virtual node, and return it in this case, while also issuing
		a warning to the user.
	]]
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

			-- If nothing changed, we can skip this update
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

			-- Stateful components can abort updates via shouldUpdate. If that
			-- happens, we should stop doing stuff at this point.
			if not shouldContinueUpdate then
				return virtualNode
			end

			virtualNode.currentElement = newElement

			return virtualNode
		end

		--[[
		Constructs a new virtual node but not does mount it.
	]]
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

				-- Legacy Context API
				-- A table of context values inherited from the parent node
				legacyContext = legacyContext,

				-- A saved copy of the parent context, used when replacing a node
				parentLegacyContext = legacyContext,

				-- Context API
				-- A table of context values inherited from the parent node
				context = context or {},

				-- A saved copy of the unmodified context; this will be updated when
				-- a component adds new context and used when a node is replaced
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

		--[[
		Constructs a new virtual node and mounts it, but does not place it into
		the tree.
	]]
		function mountVirtualNode(element, hostParent, hostKey, context, legacyContext)
			print(element)
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

			-- Boolean values render as nil to enable terse conditional rendering.
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

		--[[
		Constructs a new Roact virtual tree, constructs a root node for
		it, and mounts it.
	]]
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
					-- The root node of the tree, which starts into the hierarchy of
					-- Roact component instances.
					rootNode = nil,
					mounted = true,
				},
			}

			tree[InternalData].rootNode = mountVirtualNode(element, hostParent, hostKey)

			return tree
		end

		--[[
		Unmounts the virtual tree, freeing all of its resources.

		No further operations should be done on the tree after it's been
		unmounted, as indicated by its the `mounted` field.
	]]
		local function unmountVirtualTree(tree)
			local internalData = tree[InternalData]
			if config.typeChecks then
				assert(Type.of(tree) == Type.VirtualTree, "Expected arg #1 to be a Roact handle")
				assert(internalData.mounted, "Cannot unmounted a Roact tree that has already been unmounted")
			end

			internalData.mounted = false

			if internalData.rootNode ~= nil then
				unmountVirtualNode(internalData.rootNode)
			end
		end

		--[[
		Utility method for updating the root node of a virtual tree given a new
		element.
	]]
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

	return createReconciler
end
--######################################--
_createReconcilerCompat = function()
	--[[
	Contains deprecated methods from Reconciler. Broken out so that removing
	this shim is easy -- just delete this file and remove it from init.
	]]

	local Logging = _Logging()

	local reifyMessage = [[
Roact.reify has been renamed to Roact.mount and will be removed in a future release.
Check the call to Roact.reify at:
	]]

	local teardownMessage = [[
Roact.teardown has been renamed to Roact.unmount and will be removed in a future release.
Check the call to Roact.teardown at:
	]]

	local reconcileMessage = [[
Roact.reconcile has been renamed to Roact.update and will be removed in a future release.
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

	return createReconcilerCompat
end
--######################################--
_createRef = function()
	--[[
	A ref is nothing more than a binding with a special field 'current'
	that maps to the getValue method of the binding
	]]
	local Binding = _Binding()

	local function createRef()
		local binding, _ = Binding.create(nil)

		local ref = {}

		--[[
		A ref is just redirected to a binding via its metatable
	]]
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

	return createRef
end
--######################################--
_createSignal = function()
	--[[
	This is a simple signal implementation that has a dead-simple API.

		local signal = createSignal()

		local disconnect = signal:subscribe(function(foo)
			print("Cool foo:", foo)
		end)

		signal:fire("something")

		disconnect()
	]]

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

			-- If the callback is already registered, don't add to the suspendedConnection. Otherwise, this will disable
			-- the existing one.
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

	return createSignal
end
--######################################--
_ElementKind = function()
	--[[
	Contains markers for annotating the type of an element.

	Use `ElementKind` as a key, and values from it as the value.

		local element = {
			[ElementKind] = ElementKind.Host,
		}
	]]

	local Symbol = _Symbol()
	local strict = _strict()
	local Portal = _Portal()

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

	return ElementKind
end
--######################################--
_ElementUtils = function()
	local Type = _Type()
	local Symbol = _Symbol()

	local function noop()
		return nil
	end

	local ElementUtils = {}

	--[[
	A signal value indicating that a child should use its parent's key, because
	it has no key of its own.

	This occurs when you return only one element from a function component or
	stateful render function.
	]]
	ElementUtils.UseParentKey = Symbol.named "UseParentKey"

	--[[
	Returns an iterator over the children of an element.
	`elementOrElements` may be one of:
	* a boolean
	* nil
	* a single element
	* a fragment
	* a table of elements

	If `elementOrElements` is a boolean or nil, this will return an iterator with
	zero elements.

	If `elementOrElements` is a single element, this will return an iterator with
	one element: a tuple where the first value is ElementUtils.UseParentKey, and
	the second is the value of `elementOrElements`.

	If `elementOrElements` is a fragment or a table, this will return an iterator
	over all the elements of the array.

	If `elementOrElements` is none of the above, this function will throw.
	]]
	function ElementUtils.iterateElements(elementOrElements)
		local richType = Type.of(elementOrElements)

		-- Single child
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

	--[[
	Gets the child corresponding to a given key, respecting Roact's rules for
	children. Specifically:
	* If `elements` is nil or a boolean, this will return `nil`, regardless of
		the key given.
	* If `elements` is a single element, this will return `nil`, unless the key
		is ElementUtils.UseParentKey.
	* If `elements` is a table of elements, this will return `elements[key]`.
	]]
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

	return ElementUtils
end
--######################################--
_Event = function()
	--[[
	Index into `Event` to get a prop key for attaching to an event on a Roblox
	Instance.

	Example:

		Roact.createElement("TextButton", {
			Text = "Hello, world!",

			[Roact.Event.MouseButton1Click] = function(rbx)
				print("Clicked", rbx)
			end
		})
	]]

	local Type = _Type()

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

	return Event
end
--######################################--
_forwardRef = function()
	local assign = _assign()
	local None = _None()
	local Ref = _Ref()

	local config = _GlobalConfig().get()

	local excludeRef = {
		[Ref] = None,
	}

	--[[
	Allows forwarding of refs to underlying host components. Accepts a render
	callback which accepts props and a ref, and returns an element.
	]]
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

	return forwardRef
end
--######################################--
_getDefaultInstanceProperty = function()
	--[[
	Attempts to get the default value of a given property on a Roblox instance.

	This is used by the reconciler in cases where a prop was previously set on a
	primitive component, but is no longer present in a component's new props.

	Eventually, Roblox might provide a nicer API to query the default property
	of an object without constructing an instance of it.
	]]

	local Symbol = _Symbol()

	local Nil = Symbol.named "Nil"
	local _cachedPropertyValues = {}

	local function getDefaultInstanceProperty(className, propertyName)
		local classCache = _cachedPropertyValues[className]

		if classCache then
			local propValue = classCache[propertyName]

			-- We have to use a marker here, because Lua doesn't distinguish
			-- between 'nil' and 'not in a table'
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

	return getDefaultInstanceProperty
end
--######################################--
_GlobalConfig = function()
	--[[
	Exposes a single instance of a configuration as Roact's GlobalConfig.
	]]

	local Config = _Config()

	return Config.new()
end
--######################################--
_internalAssert = function()
	local function internalAssert(condition, message)
		if not condition then
			error(message .. " (This is probably a bug in Roact!)", 3)
		end
	end

	return internalAssert
end
--######################################--
_invalidSetStateMessages = function()
	--[[
	These messages are used by Component to help users diagnose when they're
	calling setState in inappropriate places.

	The indentation may seem odd, but it's necessary to avoid introducing extra
	whitespace into the error messages themselves.
	]]
	local ComponentLifecyclePhase = _ComponentLifecyclePhase()

	local invalidSetStateMessages = {}

	invalidSetStateMessages[ComponentLifecyclePhase.WillUpdate] = [[
setState cannot be used in the willUpdate lifecycle method.
Consider using the didUpdate method instead, or using getDerivedStateFromProps.

Check the definition of willUpdate in the component %q.]]

	invalidSetStateMessages[ComponentLifecyclePhase.ShouldUpdate] = [[
setState cannot be used in the shouldUpdate lifecycle method.
shouldUpdate must be a pure function that only depends on props and state.

Check the definition of shouldUpdate in the component %q.]]

	invalidSetStateMessages[ComponentLifecyclePhase.Render] = [[
setState cannot be used in the render method.
render must be a pure function that only depends on props and state.

Check the definition of render in the component %q.]]

	invalidSetStateMessages["default"] = [[
setState can not be used in the current situation, because Roact doesn't know
which part of the lifecycle this component is in.

This is a bug in Roact.
It was triggered by the component %q.
	]]

	return invalidSetStateMessages
end
--######################################--
_Logging = function()
	--[[
	Centralized place to handle logging. Lets us:
	- Unit test log output via `Logging.capture`
	- Disable verbose log messages when not debugging Roact

	This should be broken out into a separate library with the addition of
	scoping and logging configuration.
	]]

	-- Determines whether log messages will go to stdout/stderr
	local outputEnabled = true

	-- A set of LogInfo objects that should have messages inserted into them.
	-- This is a set so that nested calls to Logging.capture will behave.
	local collectors = {}

	-- A set of all stack traces that have called warnOnce.
	local onceUsedLocations = {}

	--[[
	Indent a potentially multi-line string with the given number of tabs, in
	addition to any indentation the string already has.
	]]
	local function indent(source, indentLevel)
		local indentString = ("\t"):rep(indentLevel)

		return indentString .. source:gsub("\n", "\n" .. indentString)
	end

	--[[
	Indents a list of strings and then concatenates them together with newlines
	into a single string.
	]]
	local function indentLines(lines, indentLevel)
		local outputBuffer = {}

		for _, line in ipairs(lines) do
			table.insert(outputBuffer, indent(line, indentLevel))
		end

		return table.concat(outputBuffer, "\n")
	end

	local logInfoMetatable = {}

	--[[
	Automatic coercion to strings for LogInfo objects to enable debugging them
	more easily.
	]]
	function logInfoMetatable:__tostring()
		local outputBuffer = { "LogInfo {" }

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

	--[[
	Invokes `callback`, capturing all output that happens during its execution.

	Output will not go to stdout or stderr and will instead be put into a
	LogInfo object that is returned. If `callback` throws, the error will be
	bubbled up to the caller of `Logging.capture`.
	]]
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

	--[[
	Issues a warning with an automatically attached stack trace.
	]]
	function Logging.warn(messageTemplate, ...)
		local message = messageTemplate:format(...)

		for collector in pairs(collectors) do
			table.insert(collector.warnings, message)
		end

		-- debug.traceback inserts a leading newline, so we trim it here
		local trace = debug.traceback("", 2):sub(2)
		local fullMessage = ("%s\n%s"):format(message, indent(trace, 1))

		if outputEnabled then
			warn(fullMessage)
		end
	end

	--[[
	Issues a warning like `Logging.warn`, but only outputs once per call site.

	This is useful for marking deprecated functions that might be called a lot;
	using `warnOnce` instead of `warn` will reduce output noise while still
	correctly marking all call sites.
	]]
	function Logging.warnOnce(messageTemplate, ...)
		local trace = debug.traceback()

		if onceUsedLocations[trace] then
			return
		end

		onceUsedLocations[trace] = true
		Logging.warn(messageTemplate, ...)
	end

	return Logging
end
--######################################--
_None = function()
	local Symbol = _Symbol()

	-- Marker used to specify that the value is nothing, because nil cannot be
	-- stored in tables.
	local None = Symbol.named "None"

	return None
end
--######################################--
_NoopRenderer = function()
	--[[
	Reference renderer intended for use in tests as well as for documenting the
	minimum required interface for a Roact renderer.
	]]

	local NoopRenderer = {}

	function NoopRenderer.isHostObject(target)
		-- Attempting to use NoopRenderer to target a Roblox instance is almost
		-- certainly a mistake.
		return target == nil
	end

	function NoopRenderer.mountHostNode(_reconciler, _node) end

	function NoopRenderer.unmountHostNode(_reconciler, _node) end

	function NoopRenderer.updateHostNode(_reconciler, node, _newElement)
		return node
	end

	return NoopRenderer
end
--######################################--
_oneChild = function()
	--[[
	Retrieves at most one child from the children passed to a component.

	If passed nil or an empty table, will return nil.

	Throws an error if passed more than one child.
	]]
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

	return oneChild
end
--######################################--
_Portal = function()
	local Symbol = _Symbol()

	local Portal = Symbol.named "Portal"

	return Portal
end
--######################################--
_PureComponent = function()
	--[[
	A version of Component with a `shouldUpdate` method that forces the
	resulting component to be pure.
	]]

	local Component = _Component()

	local PureComponent = Component:extend "PureComponent"

	-- When extend()ing a component, you don't get an extend method.
	-- This is to promote composition over inheritance.
	-- PureComponent is an exception to this rule.
	PureComponent.extend = Component.extend

	function PureComponent:shouldUpdate(newProps, newState)
		-- In a vast majority of cases, if state updated, something has updated.
		-- We don't bother checking in this case.
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

	return PureComponent
end
--######################################--
_Ref = function()
	local Symbol = _Symbol()

	local Ref = Symbol.named "Ref"

	return Ref
end
--######################################--
_RobloxRenderer = function()
	--[[
	Renderer that deals in terms of Roblox Instances. This is the most
	well-supported renderer after NoopRenderer and is currently the only
	renderer that does anything.
	]]

	local Binding = _Binding()
	local Children = _Children()
	local ElementKind = _ElementKind()
	local SingleEventManager = _SingleEventManager()
	local getDefaultInstanceProperty = _getDefaultInstanceProperty()
	local Ref = _Ref()
	local Type = _Type()
	local internalAssert = _internalAssert()

	local config = _GlobalConfig().get()

	local applyPropsError = [[
Error applying props:
	%s
In element:
%s
	]]

	local updatePropsError = [[
Error updating props:
	%s
In element:
%s
	]]

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
			-- TODO (#197): Better error message
			error(("Invalid ref: Expected type Binding but got %s"):format(typeof(ref)))
		end
	end

	local function setRobloxInstanceProperty(hostObject, key, newValue)
		if newValue == nil then
			local hostClass = hostObject.ClassName
			local _, defaultValue = getDefaultInstanceProperty(hostClass, key)
			newValue = defaultValue
		end

		-- Assign the new value to the object
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
			-- Refs and children are handled in a separate pass
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
		-- Apply props that were added or updated
		for propKey, newValue in pairs(newProps) do
			local oldValue = oldProps[propKey]

			applyProp(virtualNode, propKey, newValue, oldValue)
		end

		-- Clean up props that were removed
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
			assert(element.props.Name == nil, "Name can not be specified as a prop to a host component in Roact.")
			assert(element.props.Parent == nil, "Parent can not be specified as a prop to a host component in Roact.")
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

		-- If refs changed, detach the old ref and attach the new one
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

	return RobloxRenderer
end
--######################################--
_SingleEventManager = function()
	--[[
	A manager for a single host virtual node's connected events.
	]]

	local Logging = _Logging()

	local CHANGE_PREFIX = "Change."

	local EventStatus = {
		-- No events are processed at all; they're silently discarded
		Disabled = "Disabled",

		-- Events are stored in a queue; listeners are invoked when the manager is resumed
		Suspended = "Suspended",

		-- Event listeners are invoked as the events fire
		Enabled = "Enabled",
	}

	local SingleEventManager = {}
	SingleEventManager.__index = SingleEventManager

	function SingleEventManager.new(instance)
		local self = setmetatable({
			-- The queue of suspended events
			_suspendedEventQueue = {},

			-- All the event connections being managed
			-- Events are indexed by a string key
			_connections = {},

			-- All the listeners being managed
			-- These are stored distinctly from the connections
			-- Connections can have their listeners replaced at runtime
			_listeners = {},

			-- The suspension status of the manager
			-- Managers start disabled and are "resumed" after the initial render
			_status = EventStatus.Disabled,

			-- If true, the manager is processing queued events right now.
			_isResuming = false,

			-- The Roblox instance the manager is managing
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
		-- If the listener doesn't exist we can just disconnect the existing connection
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
						-- Store this event invocation to be fired when resume is
						-- called.

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
		-- If we're already resuming events for this instance, trying to resume
		-- again would cause a disaster.
		if self._isResuming then
			return
		end

		self._isResuming = true

		local index = 1

		-- More events might be added to the queue when evaluating events, so we
		-- need to be careful in order to preserve correct evaluation order.
		while index <= #self._suspendedEventQueue do
			local eventInvocation = self._suspendedEventQueue[index]
			local listener = self._listeners[eventInvocation[1]]
			local argumentCount = eventInvocation[2]

			-- The event might have been disconnected since suspension started; in
			-- this case, we drop the event.
			if listener ~= nil then
				-- Wrap the listener in a coroutine to catch errors and handle
				-- yielding correctly.
				local listenerCo = coroutine.create(listener)
				local success, result =
					coroutine.resume(listenerCo, self._instance, unpack(eventInvocation, 3, 2 + argumentCount))

				-- If the listener threw an error, we log it as a warning, since
				-- there's no way to write error text in Roblox Lua without killing
				-- our thread!
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

	return SingleEventManager
end
--######################################--
_strict = function()
	local function strict(t, name)
		-- FIXME Luau: Need to define a new variable since reassigning `name = ...`
		-- doesn't narrow the type
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

	return strict
end
--######################################--
_Symbol = function()
	--[[
	A 'Symbol' is an opaque marker type.

	Symbols have the type 'userdata', but when printed to the console, the name
	of the symbol is shown.
	]]

	local Symbol = {}

	--[[
	Creates a Symbol with the given name.

	When printed or coerced to a string, the symbol will turn into the string
	given as its name.
	]]
	function Symbol.named(name)
		assert(type(name) == "string", "Symbols must be created using a string name!")

		local self = newproxy(true)

		local wrappedName = ("Symbol(%s)"):format(name)

		getmetatable(self).__tostring = function()
			return wrappedName
		end

		return self
	end

	return Symbol
end
--######################################--
_Type = function()
	--[[
	Contains markers for annotating objects with types.

	To set the type of an object, use `Type` as a key and the actual marker as
	the value:

		local foo = {
			[Type] = Type.Foo,
		}
	]]

	local Symbol = _Symbol()
	local strict = _strict()

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

	return Type
end
--######################################--

local GlobalConfig = _GlobalConfig()
local createReconciler = _createReconciler()
local createReconcilerCompat = _createReconcilerCompat()
local RobloxRenderer = _RobloxRenderer()
local strict = _strict()
local Binding = _Binding()

local robloxReconciler = createReconciler(RobloxRenderer)
local reconcilerCompat = createReconcilerCompat(robloxReconciler)

local Roact = strict {
	Component = _Component(),
	createElement = _createElement(),
	createFragment = _createFragment(),
	oneChild = _oneChild(),
	PureComponent = _PureComponent(),
	None = _None(),
	Portal = _Portal(),
	createRef = _createRef(),
	forwardRef = _forwardRef(),
	createBinding = Binding.create,
	joinBindings = Binding.join,
	createContext = _createContext(),

	Change = _Change(),
	Children = _Children(),
	Event = _Event(),
	Ref = _Ref(),

	mount = robloxReconciler.mountVirtualTree,
	unmount = robloxReconciler.unmountVirtualTree,
	update = robloxReconciler.updateVirtualTree,

	reify = reconcilerCompat.reify,
	teardown = reconcilerCompat.teardown,
	reconcile = reconcilerCompat.reconcile,

	setGlobalConfig = GlobalConfig.set,

	-- APIs that may change in the future without warning
	UNSTABLE = {},
}

return Roact
