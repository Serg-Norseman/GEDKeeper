# GKcli (command-line interface and MCP server)

GKcli is a command-line interface (CLI) application for GEDKeeper, designed to provide powerful tools for managing genealogical data
through both direct text-based interaction and integration with LLM clients via the Model Context Protocol (MCP).

## MCP Tools

### Overview

**GKcli** is a CLI application for GEDKeeper with support for the [MCP](https://modelcontextprotocol.io/) protocol,
enabling LLM clients to interact with genealogical databases through a comprehensive set of tools.

**Launching in LLM clients:** `GKcli --mcp` (the argument is required, as without it the application launches an interactive text terminal
that partially duplicates the functionality of the MCP server).

**Protocol:** MCP `2025-06-18`, JSON-RPC 2.0 over stdin/stdout. No external dependencies.

### Key Features

- **Comprehensive API:** Extensive set of tools covering all aspects of genealogical data management.
- **Automatic Backup Creation:** When working through MCP, forced backup of each file is automatically enabled upon saving.
- **Pagination:** Tables with more than 20 records are split into pages; a hint for requesting the next page is provided at the end of the response.
- **Fuzzy Search:** The `individual_search` and `record_search` tools use a fuzzy matching algorithm with a customizable threshold.

### Supported LLM Clients

- **Jan**: Confirmed stable and successful operation of the MCP server with both local and cloud models (with tool usage marking).
- **LM Studio**: The client restarts a new instance of the MCP server every 1-2 minutes, leading to continuous context resets in the database.
- **Msty**: The client does not function properly.
- **GPT4All**: MCP protocol is not supported at all.

### Core Features

- **File Operations:** Create, load, save, validate, and merge GEDCOM files.
- **Record Management:** List, search, create, edit, and delete various types of records (individuals, families, notes, sources, etc.).
- **Individual Management:** Add and edit individuals with personal names, events, associations, and family relationships.
- **Family Management:** Create and manage family units, including spouses and children.
- **Source and Repository Management:** Link sources to records and manage repository information.
- **Multimedia Management:** Attach multimedia objects to records with support for multiple files per object.
- **Advanced Research Tools:** Manage research projects, tasks, communications, and groups.

### MCP Tools Reference

- [Tools reference in English](./MCP_TOOLS_en.md)
- [Tools reference in Russian](./MCP_TOOLS_ru.md)

## CLI Utility

The CLI utility allows direct management of genealogical data through a text interface. It provides functionality for creating, editing,
and querying genealogical records, including individuals, families, sources, multimedia, and more.

## Future Development

- [ ] 

## General support

### Installation

Not yet.

### Getting Started

Start working in LM chat with GKcli enabled by saying,
"Working with gedcom tools. Load and remember the gedcom date specification, but don't repeat it."
This will configure the tool and force the model to load the correct date formatting definitions from the `gedcom_date_spec` tool.

### Configuration

Configuration in **Jan** (`mcp_config.json`) for connection:

```
{
  "mcpServers": {
    "gkcli": {
      "active": true,
      "args": [
        "--mcp"
      ],
      "command": ".../GEDKeeper/bin/GKcli.exe",
      "env": {},
      "type": "stdio"
    },
  }
}
```

### Examples

Not yet.

### Troubleshooting

Not yet.

### Contributing

Join us and make your contribution!

### License

Licensed under the GNU General Public License (GPL) v3.
