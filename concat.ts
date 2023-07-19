// Read all files in src and concat them into all.lua
// run with deno run --allow-read --allow-write concat.ts

const files = Deno.readDirSync("src")

const filenames = []
let output = ""

for (const file of files)
    if (file.name.endsWith(".lua") && file.name != "all.lua" && file.name != "init.lua"){
        filenames.push(`require${file.name.slice(0, -4)}`)
        
        let content = Deno.readTextFileSync(`src/${file.name}`)
        content = content.replace(/require\(script\.Parent\.(\w+)\)/g, "require$1()")

        const func = `
require${file.name.slice(0, -4)} = function()
${content}
end
-- ############################################# --
    `
        output += func
    }

output = `local ${filenames.join(", ")}\n` + output

output += Deno.readTextFileSync("src/init.lua")
    .replace(/require\(script\.(\w+)\)/g, "require$1()")

Deno.writeTextFileSync("src/all.lua", output)
