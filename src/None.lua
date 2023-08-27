local Symbol = require "./Symbol"

-- Marker used to specify that the value is nothing, because nil cannot be
-- stored in tables.
local None = Symbol.named "None"

return None
