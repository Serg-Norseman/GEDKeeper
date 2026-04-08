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
            Name = "individual_search",
            Description = "Search for individuals by name using fuzzy matching (up to 16% difference)",
            InputSchema = new MCPToolInputSchema {
                Properties = new Dictionary<string, MCPToolProperty> {
                    ["name"] = new MCPToolProperty { Type = "string", Description = "Name to search for (e.g., 'John /Doe/')" }
                },
                Required = new List<string> { "name" }
            }
        }, ToolIndividualSearch);

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

        server.RegisterTool(new MCPTool {
            Name = "source_list",
            Description = "List all sources in the database with pagination support (20 items per page)",
            InputSchema = new MCPToolInputSchema {
                Properties = new Dictionary<string, MCPToolProperty> {
                    ["page"] = new MCPToolProperty { Type = "integer", Description = "Page number (1-based, default: 1)" }
                },
                Required = new List<string> { }
            }
        }, ToolSourceList);

        server.RegisterTool(new MCPTool {
            Name = "source_add",
            Description = "Add a new source to the database",
            InputSchema = new MCPToolInputSchema {
                Properties = new Dictionary<string, MCPToolProperty> {
                    ["title"] = new MCPToolProperty { Type = "string", Description = "Source title" }
                },
                Required = new List<string> { "title" }
            }
        }, ToolSourceAdd);

        // Repositories

        server.RegisterTool(new MCPTool {
            Name = "repository_list",
            Description = "List all repositories in the database",
            InputSchema = MCPToolInputSchema.Empty
        }, ToolRepositoryList);

        server.RegisterTool(new MCPTool {
            Name = "repository_add",
            Description = "Add a new repository to the database",
            InputSchema = new MCPToolInputSchema {
                Properties = new Dictionary<string, MCPToolProperty> {
                    ["name"] = new MCPToolProperty { Type = "string", Description = "Repository name" }
                },
                Required = new List<string> { "name" }
            }
        }, ToolRepositoryAdd);

        // Multimedia
        // Groups

        server.RegisterTool(new MCPTool {
            Name = "group_list",
            Description = "List all groups in the database",
            InputSchema = MCPToolInputSchema.Empty
        }, ToolGroupList);

        server.RegisterTool(new MCPTool {
            Name = "group_add",
            Description = "Add a new group to the database",
            InputSchema = new MCPToolInputSchema {
                Properties = new Dictionary<string, MCPToolProperty> {
                    ["name"] = new MCPToolProperty { Type = "string", Description = "Group name" }
                },
                Required = new List<string> { "name" }
            }
        }, ToolGroupAdd);

        server.RegisterTool(new MCPTool {
            Name = "group_add_member",
            Description = "Add an individual to a group by their XRef identifier",
            InputSchema = new MCPToolInputSchema {
                Properties = new Dictionary<string, MCPToolProperty> {
                    ["group_name"] = new MCPToolProperty { Type = "string", Description = "Name of the group" },
                    ["individual_xref"] = new MCPToolProperty { Type = "string", Description = "XRef identifier of the individual (e.g., 'I1', 'I2')" }
                },
                Required = new List<string> { "group_name", "individual_xref" }
            }
        }, ToolGroupAddMember);

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

        var rows = new List<string> {
            $"Individuals ({recList.Count}):",
            "| XRef | Name | Sex |",
            "|---|---|---|"
        };
        foreach (var rec in recList) {
            var iRec = (GDMIndividualRecord)rec;
            string indiName = GKUtils.GetRecordName(fContext.Tree, rec, false);
            string sex = GKData.SexData[(int)iRec.Sex].Sign;
            rows.Add($"|{rec.XRef}|{indiName}|{sex}|");
        }

        MCPServer.Log($"Successfully generated response for {rows.Count} rows");

        return CreateSimpleContent(string.Join("\n", rows));
    }

    private List<MCPContent> ToolIndividualSearch(JsonElement args)
    {
        string searchName = GetRequiredArgument(args, "name");

        var recList = fContext.Tree.GetRecords(GDMRecordType.rtIndividual);
        if (recList.Count == 0)
            return CreateSimpleContent("No individuals in database.");

        var matches = new List<string>();
        foreach (var rec in recList) {
            string indiName = GKUtils.GetRecordName(fContext.Tree, rec, false);
            int diff = SysUtils.GetDiffIndex(searchName, indiName);
            double threshold = indiName.Length * 0.16;

            if (diff <= threshold) {
                var iRec = (GDMIndividualRecord)rec;
                string sex = GKData.SexData[(int)iRec.Sex].Sign;
                matches.Add($"|{rec.XRef}|{indiName}|{sex}|{diff}|");
            }
        }

        if (matches.Count == 0)
            return CreateSimpleContent($"No matches found for: {searchName}");

        var lines = new List<string> {
            $"Search results for '{searchName}' ({matches.Count}):",
            "| XRef | Name | Sex | Diff |",
            "|---|---|---|---|"
        };
        lines.AddRange(matches);
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

        var rows = new List<string> {
            $"Families ({recList.Count}):",
            "| XRef | Family |",
            "|---|---|"
        };
        foreach (var rec in recList) {
            string familyName = GKUtils.GetRecordName(fContext.Tree, rec, false);
            rows.Add($"|{rec.XRef}|{familyName}|");
        }

        MCPServer.Log($"Successfully generated response for {rows.Count} rows");

        return CreateSimpleContent(string.Join("\n", rows));
    }

    #endregion

    #region Notes

    private List<MCPContent> ToolNoteList(JsonElement args)
    {
        var recList = fContext.Tree.GetRecords(GDMRecordType.rtNote);
        if (recList.Count == 0)
            return CreateSimpleContent("No notes in database.");

        var rows = new List<string> {
            $"Notes ({recList.Count}):",
            "| XRef | Text preview |",
            "|---|---|"
        };
        foreach (var rec in recList) {
            string preview = ((GDMNoteRecord)rec).Lines.Text;
            if (preview != null && preview.Length > 80)
                preview = preview.Substring(0, 80) + "...";
            rows.Add($"|{rec.XRef}|{preview}|");
        }

        MCPServer.Log($"Successfully generated response for {rows.Count} rows");

        return CreateSimpleContent(string.Join("\n", rows));
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

    private List<MCPContent> ToolSourceList(JsonElement args)
    {
        var recList = fContext.Tree.GetRecords(GDMRecordType.rtSource);
        if (recList.Count == 0)
            return CreateSimpleContent("No sources in database.");

        const int pageSize = 20;
        int page = GetIntArgument(args, "page", 1);
        if (page < 1) page = 1;

        int totalPages = (recList.Count + pageSize - 1) / pageSize;
        if (page > totalPages) page = totalPages;

        int startIndex = (page - 1) * pageSize;
        int endIndex = Math.Min(startIndex + pageSize, recList.Count);

        var rows = new List<string> {
            $"Sources (page {page}/{totalPages}, {startIndex + 1}-{endIndex} of {recList.Count}):",
            "| XRef | Short title | Source title |",
            "|---|---|---|"
        };
        for (int i = startIndex; i < endIndex; i++) {
            var rec = (GDMSourceRecord)recList[i];
            string sourceTitle = rec.Title.Lines.Text;
            rows.Add($"|{rec.XRef}|{rec.ShortTitle}|{sourceTitle}|");
        }

        if (page < totalPages) {
            rows.Add("");
            rows.Add($"_Next page: use parameter page={page + 1}_");
        }

        MCPServer.Log($"Successfully generated response for {rows.Count} rows (page {page}/{totalPages})");

        return CreateSimpleContent(string.Join("\n", rows));
    }

    private List<MCPContent> ToolSourceAdd(JsonElement args)
    {
        string title = GetRequiredArgument(args, "title");

        var sourceRec = fContext.Tree.CreateSource();
        sourceRec.Title.Lines.Text = title;
        fContext.SetModified();

        return CreateSimpleContent($"Source added: {title}");
    }

    #endregion

    #region Repositories

    private List<MCPContent> ToolRepositoryList(JsonElement args)
    {
        var recList = fContext.Tree.GetRecords(GDMRecordType.rtRepository);
        if (recList.Count == 0)
            return CreateSimpleContent("No repositories in database.");

        var rows = new List<string> {
            $"Repositories ({recList.Count}):",
            "| XRef | Repository |",
            "|---|---|"
        };
        foreach (var rec in recList) {
            var repoRec = (GDMRepositoryRecord)rec;
            rows.Add($"|{rec.XRef}|{repoRec.RepositoryName}|");
        }

        MCPServer.Log($"Successfully generated response for {rows.Count} rows");

        return CreateSimpleContent(string.Join("\n", rows));
    }

    private List<MCPContent> ToolRepositoryAdd(JsonElement args)
    {
        string name = GetRequiredArgument(args, "name");

        var repoRec = fContext.Tree.CreateRepository();
        repoRec.RepositoryName = name;
        fContext.SetModified();

        return CreateSimpleContent($"Repository added: {name}");
    }

    #endregion

    #region Multimedia
    #endregion

    #region Groups

    private List<MCPContent> ToolGroupList(JsonElement args)
    {
        var recList = fContext.Tree.GetRecords(GDMRecordType.rtGroup);
        if (recList.Count == 0)
            return CreateSimpleContent("No groups in database.");

        var rows = new List<string> {
            $"Groups ({recList.Count}):",
            "| XRef | Group | Members |",
            "|---|---|---|"
        };
        foreach (var rec in recList) {
            var groupRec = (GDMGroupRecord)rec;
            int membersCount = groupRec.Members.Count;
            rows.Add($"|{rec.XRef}|{groupRec.GroupName}|{membersCount}|");
        }

        MCPServer.Log($"Successfully generated response for {rows.Count} rows");

        return CreateSimpleContent(string.Join("\n", rows));
    }

    private List<MCPContent> ToolGroupAdd(JsonElement args)
    {
        string name = GetRequiredArgument(args, "name");

        var groupRec = fContext.Tree.CreateGroup();
        groupRec.GroupName = name;
        fContext.SetModified();

        return CreateSimpleContent($"Group added: {name}");
    }

    private List<MCPContent> ToolGroupAddMember(JsonElement args)
    {
        string groupName = GetRequiredArgument(args, "group_name");
        string individualXRef = GetRequiredArgument(args, "individual_xref");

        // Find the group by name
        var groupRecs = fContext.Tree.GetRecords(GDMRecordType.rtGroup);
        GDMGroupRecord targetGroup = null;
        foreach (var rec in groupRecs) {
            var gr = rec as GDMGroupRecord;
            if (gr != null && string.Equals(gr.GroupName, groupName, StringComparison.OrdinalIgnoreCase)) {
                targetGroup = gr;
                break;
            }
        }

        if (targetGroup == null)
            return CreateSimpleContent($"Group not found: {groupName}");

        // Find the individual by XRef
        var indiRec = fContext.Tree.FindXRef<GDMIndividualRecord>(individualXRef);
        if (indiRec == null)
            return CreateSimpleContent($"Individual not found with XRef: {individualXRef}");

        // Check if already a member
        if (targetGroup.IndexOfMember(indiRec) >= 0)
            return CreateSimpleContent($"Individual {individualXRef} is already a member of group '{groupName}'.");

        targetGroup.AddMember(indiRec);
        fContext.SetModified();

        string indiName = GKUtils.GetNameString(indiRec, false);
        return CreateSimpleContent($"Individual added to group '{groupName}': {indiName} ({individualXRef})");
    }

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

    private static int GetIntArgument(JsonElement args, string argName, int defaultValue)
    {
        if (!args.TryGetProperty(argName, out var argElem) || argElem.ValueKind != JsonValueKind.Number)
            return defaultValue;

        return argElem.GetInt32();
    }

    private static List<MCPContent> CreateSimpleContent(string text)
    {
        return new List<MCPContent> { new MCPContent { Text = text } };
    }

    #endregion
}
