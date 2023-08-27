local Symbol = require "./Symbol"
local strict = require "./strict"

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
