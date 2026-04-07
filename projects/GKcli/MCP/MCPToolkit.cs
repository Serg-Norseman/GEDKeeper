/*
 *  GEDKeeper, the personal genealogical database editor.
 *  Copyright (C) 2009-2026 by Sergey V. Zhdanovskih.
 *
 *  Licensed under the GNU General Public License (GPL) v3.
 *  See LICENSE file in the project root for full license information.
 */

using System;
using System.Collections.Generic;
using System.IO;
using System.Text.Json;
using GDModel;
using GKCore;
using GKCore.Options;

namespace GKcli.MCP;

/// <summary>
/// Adapter that maps MCP tool calls to BaseContext.
/// </summary>
internal class MCPToolkit
{
    private readonly BaseContext fContext;

    public MCPToolkit(BaseContext context)
    {
        fContext = context;
    }

    /// <summary>
    /// Returns the list of available MCP tools.
    /// </summary>
    public MCPToolsListResult GetToolsList()
    {
        var result = new MCPToolsListResult();

        result.Tools.Add(new MCPTool {
            Name = "file_new",
            Description = "Create a new empty GEDCOM database",
            InputSchema = new MCPToolInputSchema { Properties = new(), Required = new() }
        });

        result.Tools.Add(new MCPTool {
            Name = "file_load",
            Description = "Load a GEDCOM file into the database",
            InputSchema = new MCPToolInputSchema {
                Properties = new Dictionary<string, MCPToolProperty> {
                    ["path"] = new MCPToolProperty { Type = "string", Description = "Path to the .ged file" }
                },
                Required = new List<string> { "path" }
            }
        });

        result.Tools.Add(new MCPTool {
            Name = "file_save",
            Description = "Save the current database to the specified GEDCOM file",
            InputSchema = new MCPToolInputSchema {
                Properties = new Dictionary<string, MCPToolProperty> {
                    ["path"] = new MCPToolProperty { Type = "string", Description = "Path to save the .ged file" }
                },
                Required = new List<string> { "path" }
            }
        });

        result.Tools.Add(new MCPTool {
            Name = "file_props",
            Description = "Get information about the current database (author, address, record counts)",
            InputSchema = new MCPToolInputSchema { Properties = new(), Required = new() }
        });

        result.Tools.Add(new MCPTool {
            Name = "file_recent",
            Description = "Get a list of recently opened files",
            InputSchema = new MCPToolInputSchema { Properties = new(), Required = new() }
        });

        result.Tools.Add(new MCPTool {
            Name = "file_search",
            Description = "Find all GEDCOM files",
            InputSchema = new MCPToolInputSchema { Properties = new(), Required = new() }
        });

        result.Tools.Add(new MCPTool {
            Name = "individual_list",
            Description = "List all individuals in the database",
            InputSchema = new MCPToolInputSchema { Properties = new(), Required = new() }
        });

        result.Tools.Add(new MCPTool {
            Name = "individual_add",
            Description = "Add a new individual to the database",
            InputSchema = new MCPToolInputSchema {
                Properties = new Dictionary<string, MCPToolProperty> {
                    ["name"] = new MCPToolProperty { Type = "string", Description = "Full name (e.g., 'John /Doe/')" },
                    ["sex"] = new MCPToolProperty { Type = "string", Description = "Sex: 'm' or 'f'" }
                },
                Required = new List<string> { "name", "sex" }
            }
        });

        result.Tools.Add(new MCPTool {
            Name = "family_list",
            Description = "List all families in the database",
            InputSchema = new MCPToolInputSchema { Properties = new(), Required = new() }
        });

        result.Tools.Add(new MCPTool {
            Name = "note_list",
            Description = "List all notes in the database",
            InputSchema = new MCPToolInputSchema { Properties = new(), Required = new() }
        });

        result.Tools.Add(new MCPTool {
            Name = "note_add",
            Description = "Add a new note to the database",
            InputSchema = new MCPToolInputSchema {
                Properties = new Dictionary<string, MCPToolProperty> {
                    ["text"] = new MCPToolProperty { Type = "string", Description = "Note text content" }
                },
                Required = new List<string> { "text" }
            }
        });

        return result;
    }

    /// <summary>
    /// Execute an MCP tool call by name and arguments.
    /// </summary>
    public List<MCPContent> ExecuteTool(string toolName, JsonElement? arguments)
    {
        var args = arguments?.ValueKind == JsonValueKind.Object ? arguments.Value : default;

        return toolName switch {
            "file_new" => ToolFileNew(),
            "file_load" => ToolFileLoad(args),
            "file_save" => ToolFileSave(args),
            "file_props" => ToolFileProps(),
            "file_recent" => ToolFileRecent(),
            "file_search" => ToolFileSearch(),
            "individual_list" => ToolIndividualList(),
            "individual_add" => ToolIndividualAdd(args),
            "family_list" => ToolFamilyList(),
            "note_list" => ToolNoteList(),
            "note_add" => ToolNoteAdd(args),
            _ => throw new ArgumentException($"Unknown tool: {toolName}")
        };
    }

    private List<MCPContent> ToolFileNew()
    {
        fContext.Clear();
        return new List<MCPContent>
        {
            new MCPContent { Text = $"Database created. Records: {fContext.Tree.RecordsCount}." }
        };
    }

    private List<MCPContent> ToolFileLoad(JsonElement args)
    {
        if (!args.TryGetProperty("path", out var pathElem) || pathElem.ValueKind != JsonValueKind.String)
            throw new ArgumentException("Missing required argument: path");

        string path = pathElem.GetString()!;
        var sw = System.Diagnostics.Stopwatch.StartNew();
        fContext.FileLoad(path, false).GetAwaiter().GetResult();
        sw.Stop();

        return new List<MCPContent>
        {
            new MCPContent { Text = $"Database loaded: {path}. Records: {fContext.Tree.RecordsCount}. Time: {sw.Elapsed.TotalSeconds:F3}s." }
        };
    }

    private List<MCPContent> ToolFileSave(JsonElement args)
    {
        if (!args.TryGetProperty("path", out var pathElem) || pathElem.ValueKind != JsonValueKind.String)
            throw new ArgumentException("Missing required argument: path");

        string path = pathElem.GetString()!;
        var sw = System.Diagnostics.Stopwatch.StartNew();
        fContext.FileSave(path).GetAwaiter().GetResult();
        sw.Stop();

        return new List<MCPContent>
        {
            new MCPContent { Text = $"Database saved: {path}. Time: {sw.Elapsed.TotalSeconds:F3}s." }
        };
    }

    private List<MCPContent> ToolFileProps()
    {
        var lines = new List<string> { "File properties" };

        GDMSubmitterRecord submitter = fContext.Tree.GetSubmitter();
        lines.Add($"  Author: {submitter.Name}");
        lines.Add($"  Address: {submitter.Address.Lines.Text}");
        if (submitter.Address.PhoneNumbers.Count > 0)
            lines.Add($"  Telephone: {submitter.Address.PhoneNumbers[0].StringValue}");

        int[] stats = fContext.Tree.GetRecordStats();
        lines.Add("");
        lines.Add("Record counts:");
        for (int i = 1; i < stats.Length; i++) {
            lines.Add($"  {GKData.RecordTypes[i].Name}: {stats[i]}");
        }

        return new List<MCPContent> { new MCPContent { Text = string.Join("\n", lines) } };
    }

    private List<MCPContent> ToolFileRecent()
    {
        var result = new List<MCPContent>();

        var globOpts = GlobalOptions.Instance;
        for (int i = 0; i < globOpts.MRUFiles.Count; i++) {
            var mruFile = globOpts.MRUFiles[i];
            result.Add(new MCPContent { Text = mruFile.FileName.Replace('\\', '/') });
        }

        return result;
    }

    private static IEnumerable<string> FastSearch(string rootPath, string pattern)
    {
        var options = new EnumerationOptions {
            IgnoreInaccessible = true,
            RecurseSubdirectories = true,
            ReturnSpecialDirectories = false,
            AttributesToSkip = FileAttributes.System
        };
        return Directory.EnumerateFiles(rootPath, pattern, options);
    }

    private List<MCPContent> ToolFileSearch()
    {
        var result = new List<MCPContent>();

        var gedFiles = FastSearch("d:/", "*.ged");
        foreach (var fn in gedFiles) {
            result.Add(new MCPContent { Text = fn.Replace('\\', '/') });
        }

        return result;
    }

    private List<MCPContent> ToolIndividualList()
    {
        var recList = fContext.Tree.GetRecords(GDMRecordType.rtIndividual);
        if (recList.Count == 0)
            return new List<MCPContent> { new MCPContent { Text = "No individuals in database." } };

        var lines = new List<string> { $"Individuals ({recList.Count}):" };
        foreach (var rec in recList) {
            lines.Add($"  - {GKUtils.GetRecordName(fContext.Tree, rec, false)}");
        }

        return new List<MCPContent> { new MCPContent { Text = string.Join("\n", lines) } };
    }

    private List<MCPContent> ToolIndividualAdd(JsonElement args)
    {
        if (!args.TryGetProperty("name", out var nameElem) || nameElem.ValueKind != JsonValueKind.String)
            throw new ArgumentException("Missing required argument: name");
        if (!args.TryGetProperty("sex", out var sexElem) || sexElem.ValueKind != JsonValueKind.String)
            throw new ArgumentException("Missing required argument: sex");

        string name = nameElem.GetString()!;
        string sexStr = sexElem.GetString()!.ToLowerInvariant();
        char sex = (sexStr.Length > 0) ? sexStr[0] : 'm';
        if (sex != 'm' && sex != 'f') sex = 'm';

        var indiRec = fContext.Tree.CreateIndividual();
        var persName = indiRec.AddPersonalName(new GDMPersonalName());
        persName.ParseString(name);
        indiRec.Sex = (sex == 'm') ? GDMSex.svMale : GDMSex.svFemale;
        fContext.SetModified();

        string resultName = GKUtils.GetNameString(indiRec, false);
        return new List<MCPContent>
        {
            new MCPContent { Text = $"Individual added: {resultName}" }
        };
    }

    private List<MCPContent> ToolFamilyList()
    {
        var recList = fContext.Tree.GetRecords(GDMRecordType.rtFamily);
        if (recList.Count == 0)
            return new List<MCPContent> { new MCPContent { Text = "No families in database." } };

        var lines = new List<string> { $"Families ({recList.Count}):" };
        foreach (var rec in recList) {
            lines.Add($"  - {GKUtils.GetRecordName(fContext.Tree, rec, false)}");
        }

        return new List<MCPContent> { new MCPContent { Text = string.Join("\n", lines) } };
    }

    private List<MCPContent> ToolNoteList()
    {
        var recList = fContext.Tree.GetRecords(GDMRecordType.rtNote);
        if (recList.Count == 0)
            return new List<MCPContent> { new MCPContent { Text = "No notes in database." } };

        var lines = new List<string> { $"Notes ({recList.Count}):" };
        foreach (var rec in recList) {
            string preview = (rec as GDMNoteRecord)?.Lines.Text;
            if (preview != null && preview.Length > 80)
                preview = preview.Substring(0, 80) + "...";
            lines.Add($"  - {preview}");
        }

        return new List<MCPContent> { new MCPContent { Text = string.Join("\n", lines) } };
    }

    private List<MCPContent> ToolNoteAdd(JsonElement args)
    {
        if (!args.TryGetProperty("text", out var textElem) || textElem.ValueKind != JsonValueKind.String)
            throw new ArgumentException("Missing required argument: text");

        string text = textElem.GetString()!;
        var noteRec = fContext.Tree.CreateNote();
        noteRec.Lines.Text = text;
        fContext.SetModified();

        return new List<MCPContent>
        {
            new MCPContent { Text = $"Note added: {text}" }
        };
    }
}
