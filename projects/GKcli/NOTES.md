
# .NET

Interactive - Sharprompt v3.0.1
MCP - MCP server implementation is own to minimize unknown risks.

# Features

# MCP

- `LM Studio` is beautiful and user-friendly, but when working with MCP it showed an insurmountable problem - it
constantly launches new server instances, which leads to the loss of the internal context of the tools.

- `Jan` is not as user-friendly in the interface, but it works stably with MCP servers,
although it too often resumes the list of tools.

- `GPT4All` - does not yet support the MCP protocol natively in the client.

- `Msty` - not tested yet.

Models used for testing: `Qwen3.5 4B/9B Q4_K_M/Q8` performed very well.

# TODO

- A full list of instruments and their data is sent via a system prompt to the model,
consuming tokens. Minimize instruments!

1 Priority
- a single tool for obtaining lists of records
- event - all props, age, address, sublists
- individual's sublists tools, change sex, names and their parts control, bookmarks
- family spouses replace/remove tools, change status
- sources - repo links and all props
- location - change coords, names, toplinks

2 Priority
- media - files only
- repo - address only?
- task - change props
- research - change sublists
- communication - change props
- tree check tool
- tree compare with total refactoring
- rec merge
