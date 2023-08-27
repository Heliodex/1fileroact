--~strict
--[[
	Packages up the internals of Roact and exposes a public API for it.
]]

local GlobalConfig = require "./GlobalConfig"
local createReconciler = require "./createReconciler"
local createReconcilerCompat = require "./createReconcilerCompat"
local RobloxRenderer = require "./RobloxRenderer"
local strict = require "./strict"
local Binding = require "./Binding"

local robloxReconciler = createReconciler(RobloxRenderer)
local reconcilerCompat = createReconcilerCompat(robloxReconciler)

local Roact = strict {
	Component = require "./Component",
	createElement = require "./createElement",
	createFragment = require "./createFragment",
	oneChild = require "./oneChild",
	PureComponent = require "./PureComponent",
	None = require "./None",
	Portal = require "./Portal",
	createRef = require "./createRef",
	forwardRef = require "./forwardRef",
	createBinding = Binding.create,
	joinBindings = Binding.join,
	createContext = require "./createContext",

	Change = require "./Change",
	Children = require "./Children",
	Event = require "./Event",
	Ref = require "./Ref",

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
