# GKcli MCP Tools Reference

## Overview

**GKcli** is a GEDKeeper CLI application with [MCP](https://modelcontextprotocol.io/) protocol support,
allowing LLM clients to interact with genealogical databases through a set of tools.

**Running in LLM clients:** `GKcli --mcp` (argument is required, as without it an interactive text terminal is launched, partially replicating the MCP-server functionality set).

**Protocol:** MCP `2025-06-18`, JSON-RPC 2.0 over stdin/stdout. No external dependencies.

---

## LM Clients

- **Jan**: confirmed successful stable operation of the MCP server with local and cloud models (with tool usage labeling).
- **LM Studio**: the client restarts a new MCP server instance every 1-2 minutes, causing continuous DB context resets.
- **Msty**: the client does not function properly.
- **GPT4All**: MCP protocol is not supported at all.

---

## Implementation Features

- **Auto-backups:** when working through MCP, automatic forced backup of each file is enabled upon saving.
- **Pagination:** tables with >20 records are paginated; a hint for requesting the next page is provided at the end of the response.
- **Fuzzy search:** `individual_search` and `record_search` use a fuzzy matching algorithm with adjustable threshold.
- **Record XRef identifiers:** all identifiers are automatically generated when adding new records. Uniqueness is controlled by the GEDKeeper application suite core.
- **Error handling:** all errors that may occur during operation are sent in human-readable form from tools to the LM client for display to the user.
- **Localization support**: due to GKcli/MCP working as part of the GEDKeeper application suite, the tool uses the same files and localization approaches as the main GUI application. By default, when no special setting is present, the tool will use English for interaction. The setting can be configured by running GKcli from the command line without the `--mcp` flag - the settings section will then be accessible.
- **Data format and input validation**: the data format is described in detail in the tool descriptors within the MCP server, where final validation is also performed after data preparation by the model.

---

## File Operations

| Tool | Description | Parameters |
|---|---|---|
| `file_new` | Create a new empty GEDCOM database | — |
| `file_load` | Load a GEDCOM file | `path` (string) — path to `.ged` |
| `file_save` | Save the database to a GEDCOM file | `path` (string) — path for saving |
| `file_props` | Get information about the current database (author, address, record statistics) | — |
| `file_recent` | List recently opened files | — |
| `file_reload` | Reload the last opened file | — |
| `file_search` | Find all GEDCOM files on disk | `path` (string) — root directory for search |
| `file_validate` | Validate the current database | — |
| `file_merge` | Merge another GEDCOM file into the current database | `path` (string) — path to `.ged` |

---

## Records (General)

| Tool | Description | Parameters |
|---|---|---|
| `record_list` | List all records of the specified type (paginated, 20 per page) | `record_type` (string), `page` (integer, default 1) |
| `record_delete` | Delete a record | `xref` (string, e.g. `I1`) |
| `record_merge` | Merge records | `target_xref` (string, e.g. `I1`), `source_xref` (string, e.g. `I2`) |
| `record_info` | Get full information about a record | `xref` (string, e.g. `I1`) |
| `record_search` | Fuzzy search across any records (name/title) | `record_type` (string), `search_text` (string), `threshold` (number, default 0.15) |
| `record_set_restriction` | Set access/visibility restriction for a record | `xref` (string), `restriction` (string: `None`, `Locked`, `Confidential`, `Privacy`) |
| `record_list_userrefs` | List user references of a record | `record_xref` (string) |
| `record_add_userref` | Add a user reference to a record | `record_xref` (string), `string_value` (string), `reference_type` (string, optional) |
| `record_delete_userref` | Delete a user reference from a record | `record_xref` (string), `reference_index` (integer, 0-based) |
| `record_list_sources` | List source citations of a record | `record_xref` (string) |
| `record_add_source` | Add a source citation to a record | `record_xref` (string), `source_xref` (string), `page` (string, optional), `certainty` (integer, 0–3, optional) |
| `record_delete_source` | Delete a source citation from a record | `record_xref` (string), `citation_index` (integer, 0-based) |
| `record_list_multimedia` | List multimedia objects of a record | `record_xref` (string) |
| `record_add_multimedia` | Add a multimedia link to a record | `record_xref` (string), `multimedia_xref` (string), `is_primary` (boolean, default false) |
| `record_delete_multimedia` | Delete a multimedia link from a record | `record_xref` (string), `link_index` (integer, 0-based) |
| `record_list_notes` | List notes of a record | `record_xref` (string) |
| `record_add_note` | Add a note link to a record | `record_xref` (string), `note_xref` (string) |
| `record_delete_note` | Delete a note link from a record | `record_xref` (string), `note_index` (integer, 0-based) |
| `event_type_list` | List all available event types for individuals or families | `record_type` (string) |

**Record types:** `Individual`, `Family`, `Note`, `Source`, `Repository`, `Multimedia`, `Group`, `Task`, `Research`, `Communication`, `Location`

---

## Individuals

| Tool | Description | Parameters |
|---|---|---|
| `individual_search` | Fuzzy search by name (16% threshold) | `name` (string) |
| `individual_upsert` | Add an individual or edit an existing one | `xref` (string, optional), `name` (string), `sex` (string: `m`/`f`), `nickname` (string, optional) |
| `individual_list_associations` | List all associations of an individual | `individual_xref` (string) |
| `individual_upsert_association` | Add an association to an individual or edit an existing one | `individual_xref` (string), `association_index` (integer, 0-based), `associate_xref` (string), `relation` (string) |
| `individual_delete_association` | Delete an association from an individual | `individual_xref` (string), `association_index` (integer, 0-based) |
| `individual_list_events` | List all events of an individual | `individual_xref` (string) |
| `individual_delete_event` | Delete an individual's event | `individual_xref` (string), `event_index` (integer, 0-based) |
| `individual_upsert_event` | Add an event to an individual or edit an existing one | `individual_xref` (string), `event_index` (integer, 0-based), `type` (string), `date` (string), `place` (string), `location_xref` (string), `cause` (string), `agency` (string), `value` (string), `age` (string) |
| `individual_list_personal_names` | List all personal names of an individual | `individual_xref` (string) |
| `individual_upsert_personal_name` | Add a personal name or edit an existing one | `individual_xref` (string), `name_index` (integer, 0-based), `given` (string), `surname` (string), `surname_prefix` (string), `name_prefix` (string), `name_suffix` (string), `nickname` (string), `name_type` (string), `language` (string), `patronymic` (string), `married_name` (string), `religious_name` (string), `census_name` (string) |
| `individual_delete_personal_name` | Delete an individual's personal name | `individual_xref` (string), `name_index` (integer, 0-based) |
| `individual_list_spouses` | List all spouses of an individual | `individual_xref` (string) |
| `individual_list_groups` | List all groups of an individual | `individual_xref` (string) |

---

## Families

| Tool | Description | Parameters |
|---|---|---|
| `family_upsert` | Create a family or edit an existing one | `xref` (string, optional), `husband_xref` (string), `wife_xref` (string) |
| `family_list_children` | List children of a family | `family_xref` (string) |
| `family_add_child` | Add a child to a family | `family_xref` (string), `child_xref` (string), `linkage_type` (string, optional) |
| `family_delete_child` | Remove a child from a family | `family_xref` (string), `child_xref` (string) |
| `family_list_events` | List all events of a family | `family_xref` (string) |
| `family_delete_event` | Delete a family event | `family_xref` (string), `event_index` (integer, 0-based) |
| `family_upsert_event` | Add an event to a family or edit an existing one | `family_xref` (string), `event_index` (integer, 0-based), `type` (string), `date` (string), `place` (string), `location_xref` (string), `cause` (string), `agency` (string), `value` (string), `husband_age` (string), `wife_age` (string) |

---

## Notes

| Tool | Description | Parameters |
|---|---|---|
| `note_upsert` | Add a note or edit an existing one | `xref` (string, optional), `text` (string) |

---

## Sources

| Tool | Description | Parameters |
|---|---|---|
| `source_upsert` | Add a source or edit an existing one | `xref` (string, optional), `title` (string), `short_title` (string, optional), `author` (string, optional) |
| `source_list_repositories` | List repository links | `source_xref` (string) |
| `source_add_repository` | Add a repository link to a source | `source_xref` (string), `repository_xref` (string) |
| `source_delete_repository` | Delete a repository link from a source | `source_xref` (string), `repository_xref` (string) |

---

## Repositories

| Tool | Description | Parameters |
|---|---|---|
| `repository_upsert` | Add a repository or edit an existing one | `xref` (string), `name` (string) |

---

## Multimedia

| Tool | Description | Parameters |
|---|---|---|
| `multimedia_upsert` | Add a multimedia record with a file link or edit an existing one | `xref` (string), `title` (string), `file_path` (string), `media_type` (string), `store_type` (string) |
| `multimedia_get` | Get a multimedia record | `xref` (string, e.g. `O1`) |
| `multimedia_list_files` | List files in a multimedia record | `xref` (string) |
| `multimedia_upsert_file` | Add a file to a multimedia record or edit an existing one | `xref` (string), `file_index` (integer, 0-based), `title` (string), `file_path` (string), `media_type` (string), `store_type` (string) |
| `multimedia_delete_file` | Delete a file from a multimedia record | `xref` (string), `file_index` (string) |

---

## Groups

| Tool | Description | Parameters |
|---|---|---|
| `group_upsert` | Create a group or edit an existing one | `xref` (string), `name` (string) |
| `group_list_members` | List group members | `group_xref` (string) |
| `group_add_member` | Add an individual to a group | `group_xref` (string), `individual_xref` (string) |
| `group_delete_member` | Remove an individual from a group | `group_xref` (string), `individual_xref` (string) |

---

## Tasks

| Tool | Description | Parameters |
|---|---|---|
| `task_upsert` | Add a task or edit an existing one | `xref` (string, optional), `goal` (string), `priority` (enum), `start_date` (string, optional), `stop_date` (string, optional) |

---

## Researches

| Tool | Description | Parameters |
|---|---|---|
| `research_upsert` | Add a research or edit an existing one | `xref` (string), `title` (string), `priority` (enum), `status` (enum), `start_date` (string), `stop_date` (string), `percent` (integer) |
| `research_list_tasks` | List all tasks of a research by its XRef identifier | `research_xref` (string) — XRef identifier of the research (e.g., 'R1') |
| `research_add_task` | Add a task to a research by their XRef identifiers | `research_xref` (string) — XRef identifier of the research (e.g., 'R1', 'R2'), `task_xref` (string) — XRef identifier of the task (e.g., 'T1', 'T2') |
| `research_delete_task` | Delete a task from a research by their XRef identifiers | `research_xref` (string) — XRef identifier of the research (e.g., 'R1', 'R2'), `task_xref` (string) — XRef identifier of the task (e.g., 'T1', 'T2') |
| `research_list_communications` | List all communications of a research by its XRef identifier | `research_xref` (string) — XRef identifier of the research (e.g., 'R1') |
| `research_add_communication` | Add a communication to a research by their XRef identifiers | `research_xref` (string) — XRef identifier of the research (e.g., 'R1', 'R2'), `communication_xref` (string) — XRef identifier of the communication (e.g., 'C1', 'C2') |
| `research_delete_communication` | Delete a communication from a research by their XRef identifiers | `research_xref` (string) — XRef identifier of the research (e.g., 'R1', 'R2'), `communication_xref` (string) — XRef identifier of the communication (e.g., 'C1', 'C2') |
| `research_list_groups` | List all groups of a research by its XRef identifier | `research_xref` (string) — XRef identifier of the research (e.g., 'R1') |
| `research_add_group` | Add a group to a research by their XRef identifiers | `research_xref` (string) — XRef identifier of the research (e.g., 'R1', 'R2'), `group_xref` (string) — XRef identifier of the group (e.g., 'G1', 'G2') |
| `research_delete_group` | Delete a group from a research by their XRef identifiers | `research_xref` (string) — XRef identifier of the research (e.g., 'R1', 'R2'), `group_xref` (string) — XRef identifier of the group (e.g., 'G1', 'G2') |

---

## Communications

| Tool | Description | Parameters |
|---|---|---|
| `communication_upsert` | Add a communication or edit an existing one | `xref` (string), `name` (string), `type` (enum), `direction` (enum), `corresponderXRef` (string), `date` (string) |

---

## Locations

| Tool | Description | Parameters |
|---|---|---|
| `location_upsert` | Add a location or edit an existing one | `xref` (string), `name` (string), `lati` (number), `long` (number) |
| `location_list_names` | List all names of a location | `location_xref` (string, e.g. 'L1') |
| `location_upsert_name` | Add a name to a location record or edit an existing one | `location_xref` (string, e.g. 'L1'), `name_index` (integer, 0-based), `name` (string), `short_name` (string), `date` (string) |
| `location_delete_name` | Delete a name from a location | `location_xref` (string, e.g. 'L1'), `name_index` (integer, 0-based) |
| `location_list_top_links` | List all top-level links of a location | `location_xref` (string, e.g. 'L1') |
| `location_upsert_top_link` | Add a top-level link to a location record or edit an existing one | `location_xref` (string, e.g. 'L1'), `top_link_index` (integer, 0-based), `top_link_xref` (string), `date` (string) |
| `location_delete_top_link` | Delete a top-level link from a location | `location_xref` (string, e.g. 'L1'), `top_link_index` (integer, 0-based) |

---

## TODO

1. In the "Implementation Features" section, it states that fuzzy search has an adjustable threshold, but the `individual_search` tool description specifies a fixed 16% threshold, which creates confusion.
2. Lack of tools for exporting/importing in other formats.
3. No tools for advanced search by dates, places, events, or combinations of criteria.
4. No tools for generating statistics, building charts, analyzing generations, etc.
5. No mechanism for rolling back changes when an operation partially fails (e.g., when merging trees).
