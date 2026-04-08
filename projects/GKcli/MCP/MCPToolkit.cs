/*
 *  GEDKeeper, the personal genealogical database editor.
 *  Copyright (C) 2009-2026 by Sergey V. Zhdanovskih.
 *
 *  Licensed under the GNU General Public License (GPL) v3.
 *  See LICENSE file in the project root for full license information.
 */

using System;
using System.Collections.Generic;
using System.Text.Json;
using GDModel;
using GKCore;
using GKCore.Options;
using GKCore.Utilities;

namespace GKcli.MCP;

/// <summary>
/// Adapter that maps MCP tool calls to BaseContext.
/// </summary>
internal class MCPToolkit
{
    private readonly BaseContext fContext;

    public MCPToolkit(BaseContext context, MCPServer server)
    {
        fContext = context;
        RegisterTools(server);
    }

    private void RegisterTools(MCPServer server)
    {
        // Files

        server.RegisterTool(new MCPTool {
            Name = "file_new",
            Description = "Create a new empty GEDCOM database",
            InputSchema = MCPToolInputSchema.Empty
        }, ToolFileNew);

        server.RegisterTool(new MCPTool {
            Name = "file_load",
            Description = "Load a GEDCOM file into the database",
            InputSchema = new MCPToolInputSchema {
                Properties = new Dictionary<string, MCPToolProperty> {
                    ["path"] = new MCPToolProperty { Type = "string", Description = "Path to the .ged file" }
                },
                Required = new List<string> { "path" }
            }
        }, ToolFileLoad);

        server.RegisterTool(new MCPTool {
            Name = "file_save",
            Description = "Save the current database to the specified GEDCOM file",
            InputSchema = new MCPToolInputSchema {
                Properties = new Dictionary<string, MCPToolProperty> {
                    ["path"] = new MCPToolProperty { Type = "string", Description = "Path to save the .ged file" }
                },
                Required = new List<string> { "path" }
            }
        }, ToolFileSave);

        server.RegisterTool(new MCPTool {
            Name = "file_props",
            Description = "Get information about the current database (author, address, record counts)",
            InputSchema = MCPToolInputSchema.Empty
        }, ToolFileProps);

        server.RegisterTool(new MCPTool {
            Name = "file_recent",
            Description = "Get a list of recently opened files",
            InputSchema = MCPToolInputSchema.Empty
        }, ToolFileRecent);

        server.RegisterTool(new MCPTool {
            Name = "file_search",
            Description = "Find all GEDCOM files",
            InputSchema = MCPToolInputSchema.Empty
        }, ToolFileSearch);

        // Individuals

        server.RegisterTool(new MCPTool {
            Name = "individual_list",
            Description = "List all individuals in the database",
            InputSchema = MCPToolInputSchema.Empty
        }, ToolIndividualList);

        server.RegisterTool(new MCPTool {
            Name = "individual_add",
            Description = "Add a new individual to the database",
            InputSchema = new MCPToolInputSchema {
                Properties = new Dictionary<string, MCPToolProperty> {
                    ["name"] = new MCPToolProperty { Type = "string", Description = "Full name (e.g., 'John /Doe/')" },
                    ["sex"] = new MCPToolProperty { Type = "string", Description = "Sex: 'm' or 'f'" }
                },
                Required = new List<string> { "name", "sex" }
            }
        }, ToolIndividualAdd);

        // Families

        server.RegisterTool(new MCPTool {
            Name = "family_list",
            Description = "List all families in the database",
            InputSchema = MCPToolInputSchema.Empty
        }, ToolFamilyList);

        // Notes

        server.RegisterTool(new MCPTool {
            Name = "note_list",
            Description = "List all notes in the database",
            InputSchema = MCPToolInputSchema.Empty
        }, ToolNoteList);

        server.RegisterTool(new MCPTool {
            Name = "note_add",
            Description = "Add a new note to the database",
            InputSchema = new MCPToolInputSchema {
                Properties = new Dictionary<string, MCPToolProperty> {
                    ["text"] = new MCPToolProperty { Type = "string", Description = "Note text content" }
                },
                Required = new List<string> { "text" }
            }
        }, ToolNoteAdd);

        // Sources
        // Repositories
        // Multimedia
        // Groups
        // Tasks
        // Researches
        // Communications
        // Locations
    }

    #region Files

    private List<MCPContent> ToolFileNew(JsonElement args)
    {
        fContext.Clear();

        return CreateSimpleContent($"Database created. Records: {fContext.Tree.RecordsCount}.");
    }

    private List<MCPContent> ToolFileLoad(JsonElement args)
    {
        string path = GetRequiredArgument(args, "path");

        var sw = System.Diagnostics.Stopwatch.StartNew();
        fContext.FileLoad(path, false).GetAwaiter().GetResult();
        sw.Stop();

        return CreateSimpleContent($"Database loaded: {path}. Records: {fContext.Tree.RecordsCount}. Time: {sw.Elapsed.TotalSeconds:F3}s.");
    }

    private List<MCPContent> ToolFileSave(JsonElement args)
    {
        string path = GetRequiredArgument(args, "path");

        var sw = System.Diagnostics.Stopwatch.StartNew();
        fContext.FileSave(path).GetAwaiter().GetResult();
        sw.Stop();

        return CreateSimpleContent($"Database saved: {path}. Time: {sw.Elapsed.TotalSeconds:F3}s.");
    }

    private List<MCPContent> ToolFileProps(JsonElement args)
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

        return CreateSimpleContent(string.Join("\n", lines));
    }

    private List<MCPContent> ToolFileRecent(JsonElement args)
    {
        var result = new List<MCPContent>();

        var globOpts = GlobalOptions.Instance;
        for (int i = 0; i < globOpts.MRUFiles.Count; i++) {
            var mruFile = globOpts.MRUFiles[i];
            result.Add(new MCPContent { Text = mruFile.FileName.Replace('\\', '/') });
        }

        return result;
    }

    private List<MCPContent> ToolFileSearch(JsonElement args)
    {
        var result = new List<MCPContent>();

        var gedFiles = SysUtils.FastSearchFiles("d:/", "*.ged");
        foreach (var fn in gedFiles) {
            result.Add(new MCPContent { Text = fn.Replace('\\', '/') });
        }

        return result;
    }

    #endregion

    #region Individuals

    private List<MCPContent> ToolIndividualList(JsonElement args)
    {
        var recList = fContext.Tree.GetRecords(GDMRecordType.rtIndividual);
        if (recList.Count == 0)
            return CreateSimpleContent("No individuals in database.");

        var lines = new List<string> { $"Individuals ({recList.Count}):" };
        foreach (var rec in recList) {
            lines.Add($"  - {GKUtils.GetRecordName(fContext.Tree, rec, false)}");
        }

        return CreateSimpleContent(string.Join("\n", lines));
    }

    private List<MCPContent> ToolIndividualAdd(JsonElement args)
    {
        string name = GetRequiredArgument(args, "name");

        string sexStr = GetRequiredArgument(args, "sex").ToLowerInvariant();
        char sex = (sexStr.Length > 0) ? sexStr[0] : 'm';
        if (sex != 'm' && sex != 'f') sex = 'm';

        var indiRec = fContext.Tree.CreateIndividual();
        var persName = indiRec.AddPersonalName(new GDMPersonalName());
        persName.ParseString(name);
        indiRec.Sex = (sex == 'm') ? GDMSex.svMale : GDMSex.svFemale;
        fContext.SetModified();

        string resultName = GKUtils.GetNameString(indiRec, false);
        return CreateSimpleContent($"Individual added: {resultName}");
    }

    #endregion

    #region Families

    private List<MCPContent> ToolFamilyList(JsonElement args)
    {
        var recList = fContext.Tree.GetRecords(GDMRecordType.rtFamily);
        if (recList.Count == 0)
            return CreateSimpleContent("No families in database.");

        var lines = new List<string> { $"Families ({recList.Count}):" };
        foreach (var rec in recList) {
            lines.Add($"  - {GKUtils.GetRecordName(fContext.Tree, rec, false)}");
        }

        return CreateSimpleContent(string.Join("\n", lines));
    }

    #endregion

    #region Notes

    private List<MCPContent> ToolNoteList(JsonElement args)
    {
        var recList = fContext.Tree.GetRecords(GDMRecordType.rtNote);
        if (recList.Count == 0)
            return CreateSimpleContent("No notes in database.");

        var lines = new List<string> { $"Notes ({recList.Count}):" };
        foreach (var rec in recList) {
            string preview = (rec as GDMNoteRecord)?.Lines.Text;
            if (preview != null && preview.Length > 80)
                preview = preview.Substring(0, 80) + "...";
            lines.Add($"  - {preview}");
        }

        return CreateSimpleContent(string.Join("\n", lines));
    }

    private List<MCPContent> ToolNoteAdd(JsonElement args)
    {
        string text = GetRequiredArgument(args, "text");

        var noteRec = fContext.Tree.CreateNote();
        noteRec.Lines.Text = text;
        fContext.SetModified();

        return CreateSimpleContent($"Note added: {text}");
    }

    #endregion

    #region Sources
    #endregion

    #region Repositories
    #endregion

    #region Multimedia
    #endregion

    #region Groups
    #endregion

    #region Tasks
    #endregion

    #region Researches
    #endregion

    #region Communications
    #endregion

    #region Locations
    #endregion

    #region Utilities

    private static string GetRequiredArgument(JsonElement args, string argName)
    {
        if (!args.TryGetProperty(argName, out var argElem) || argElem.ValueKind != JsonValueKind.String)
            throw new ArgumentException($"Missing required argument: {argName}");

        return argElem.GetString()!;
    }

    private static List<MCPContent> CreateSimpleContent(string text)
    {
        return new List<MCPContent> { new MCPContent { Text = text } };
    }

    #endregion
}
