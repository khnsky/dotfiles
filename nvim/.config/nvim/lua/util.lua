local fntocmd = {}
setmetatable(fntocmd, {
    __call = function(self, fn)
        table.insert(fntocmd, fn)
        return ("lua require 'util'.fntocmd[%d]()"):format(#fntocmd)
    end,
    __index = fntocmd,
})

local function fntotb(fn)
    return setmetatable({}, {
        __call = fn,
    })
end

return setmetatable({
    fntocmd = fntocmd,
    fntotb  = fntotb,
}, {
	__index = function(_, key)
		local ok, ret = pcall(require, "util." .. key)
		if ok then
			return ret
		end
	end,
})
