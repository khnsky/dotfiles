local PACKAGE_DIRECTORY = vim.fn.stdpath("data") .. "/site/pack/packages"

local Package = {
    __call = function(self, conf)
        for k, v in pairs(conf) do
            self[k] = v
        end
        return self
    end,

    __index = function(self, key)
        local meta   = getmetatable(self)
        local getter = meta.getters[key]
        if getter then
            return getter(self)
        else
            return meta[key]
        end
    end,

    getters = {
        exists = function(package)
            return vim.fn.isdirectory(package.path) ~= 0
        end,

        path = function(package)
            local load = package.opt and "/opt/" or "/start/"
            return PACKAGE_DIRECTORY .. load .. package.name
        end,
    },
}

function Package:new(name, source)
    local package = {
        name   = name,
        source = source,
    }
    return setmetatable(package, self)
end

function Package:sync(progress)
    if self.exists then
        self.source:update(self, progress)
    else
        self.source:install(self, progress)
    end
    return self
end


local Source = {
    __call = function(self, ...)
        return self:package(...)
    end,

    all = {},
}

function Source:new(name, source)
    self.all[name] = setmetatable(source or { name = name }, self)
    return self.all[name]
end

local github = Source:new "github"

function github:update(package, progress)
    local callback = progress("updating " .. package.name)
    local handle
    handle = vim.loop.spawn(
        "git", {
            args = { "pull", "--recurse-submodules", "--update-shallow", },
            cwd  = package.path,
        },
        vim.schedule_wrap(function(code)
            callback(code)
            handle:close()
        end)
    )
end

function github:install(package, progress)
    local callback = progress("installing " .. package.name)
    local handle
    handle = vim.loop.spawn(
        "git", {
            args = {
                "clone", package.url,
                "--depth=1", "--recurse-submodules", "--shallow-submodules",
                package.path,
            },
        },
        vim.schedule_wrap(function(code)
            callback(code)
            handle:close()
        end)
    )
end

function github:package(address)
    local name   = address:match("^[%w-]+/([%w-_.]+)$")
    return Package:new(name, self) {
        url = "https://github.com/" .. address,
    }
end

local function diriterator(path)
    local fs = vim.loop.fs_scandir(path)
    return function()
        return fs and vim.loop.fs_scandir_next(fs)
    end
end

local Packages = {}
function Packages:__call(packages)
    local managed = {}
    local of = 0
    for _, package in pairs(packages) do
        of = of + 1
        managed[package.path] = package
    end

    local unmanaged = {}
    for _, load in pairs { "/start/", "/opt/" } do
        for path in diriterator(PACKAGE_DIRECTORY .. load) do
            path = PACKAGE_DIRECTORY .. load .. path
            if not managed[path] then
                of = of + 1
                table.insert(unmanaged, path)
            end
        end
    end

    -- if of equals 0 width will be -inf but progress won't be called
    local no, width = 1, math.floor(math.log10(of)) + 1
    local function progress(msg)
        local w = width - math.floor(math.log10(no)) + 1
        -- clear screen: https://stackoverflow.com/a/33854736
        vim.cmd "echon '\r\r' | echon ''"
        vim.cmd(
            string.format("echon '[%" .. w .. "s%d/%d] %s'", "", no, of, msg)
        )
        no = no + 1
        return function(code)
            if code ~= 0 then
                vim.cmd("echoerr '" .. msg .. "'")
            end
        end
    end

    for _, path in pairs(unmanaged) do
        progress("deleting " .. path)
        vim.fn.delete(path, "rf")
    end

    for _, package in pairs(managed) do
        package:sync(progress)
    end

    vim.cmd "packloadall! | silent! helptags ALL"
end

return setmetatable({
    sources = Source.all
}, Packages)
