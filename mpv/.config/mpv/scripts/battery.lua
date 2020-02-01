local msg, utils = require 'mp.msg', require 'mp.utils'

local linux = package.config:sub(1, 1) == '/'
if not linux then
    return
end

local function battery()
    local f = io.open('/sys/class/power_supply/AC/online', 'r')
    if f then
        local ac = f:read '*n'
        f:close()
        return ac == 0
    end
    return false
end

if not battery() then
    msg.info 'applying hq profile ... '
    mp.commmand 'apply-profile hq'
end
