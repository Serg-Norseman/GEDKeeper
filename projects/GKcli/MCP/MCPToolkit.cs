/*
 *  GEDKeeper, the personal genealogical database editor.
 *  Copyright (C) 2009-2026 by Sergey V. Zhdanovskih.
 *
 *  Licensed under the GNU General Public License (GPL) v3.
 *  See LICENSE file in the project root for full license information.
 */

using System;
using System.Collections.Generic;
using System.Text;
using System.Text.Json;
using GDModel;
using GKCore;
using GKCore.Locales;
using GKCore.Options;
using GKCore.Utilities;

namespace GKcli.MCP;

/// <summary>
/// Adapter that maps MCP tool calls to BaseContext.
/// </summary>
internal class MCPToolkit
{
    private const int pageSize = 20;

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
            Name = "file_reload",
            Description = "Reload the most recently opened file",
            InputSchema = MCPToolInputSchema.Empty
        }, ToolFileReload);

        server.RegisterTool(new MCPTool {
            Name = "file_search",
            Description = "Find all GEDCOM files on disk",
            InputSchema = MCPToolInputSchema.Empty
        }, ToolFileSearch);

        server.RegisterTool(new MCPTool {
            Name = "file_validate",
            Description = "Check if the current database is valid (not unknown/empty/corrupted)",
            InputSchema = MCPToolInputSchema.Empty
        }, ToolFileValidate);

        // Records common

        server.RegisterTool(new MCPTool {
            Name = "record_search",
            Description = "Search for any records by name/title using fuzzy matching. Available record types: Individual, Family, Note, Source, Repository, Multimedia, Group, Task, Research, Communication, Location.",
            InputSchema = new MCPToolInputSchema {
                Properties = new Dictionary<string, MCPToolProperty> {
                    ["record_type"] = new MCPToolProperty { Type = "string", Description = "Record type (e.g., 'Individual', 'Family', 'Note', 'Source', 'Repository', 'Multimedia', 'Group', 'Task', 'Research', 'Communication', 'Location')" },
                    ["search_text"] = new MCPToolProperty { Type = "string", Description = "Text to search for (name, title, etc.)" },
                    ["threshold"] = new MCPToolProperty { Type = "number", Description = "Fuzzy match threshold (0.0-1.0, default: 0.15). Lower = stricter match." }
                },
                Required = new List<string> { "record_type", "search_text" }
            }
        }, ToolRecordSearch);

        // Individuals

        server.RegisterTool(new MCPTool {
            Name = "individual_list",
            Description = "List all individuals in the database with pagination support (20 items per page)",
            InputSchema = new MCPToolInputSchema {
                Properties = new Dictionary<string, MCPToolProperty> {
                    ["page"] = new MCPToolProperty { Type = "integer", Description = "Page number (1-based, default: 1)" }
                },
                Required = new List<string> { }
            }
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

        server.RegisterTool(new MCPTool {
            Name = "individual_delete",
            Description = "Delete an individual from the database by their XRef identifier",
            InputSchema = new MCPToolInputSchema {
                Properties = new Dictionary<string, MCPToolProperty> {
                    ["xref"] = new MCPToolProperty { Type = "string", Description = "XRef identifier of the individual (e.g., 'I1')" }
                },
                Required = new List<string> { "xref" }
            }
        }, ToolIndividualDelete);

        // Families

        server.RegisterTool(new MCPTool {
            Name = "family_list",
            Description = "List all families in the database with pagination support (20 items per page)",
            InputSchema = new MCPToolInputSchema {
                Properties = new Dictionary<string, MCPToolProperty> {
                    ["page"] = new MCPToolProperty { Type = "integer", Description = "Page number (1-based, default: 1)" }
                },
                Required = new List<string> { }
            }
        }, ToolFamilyList);

        server.RegisterTool(new MCPTool {
            Name = "family_add",
            Description = "Add a new family record to the database with husband and wife",
            InputSchema = new MCPToolInputSchema {
                Properties = new Dictionary<string, MCPToolProperty> {
                    ["husband_xref"] = new MCPToolProperty { Type = "string", Description = "XRef identifier of the husband (e.g., 'I1')" },
                    ["wife_xref"] = new MCPToolProperty { Type = "string", Description = "XRef identifier of the wife (e.g., 'I2')" }
                },
                Required = new List<string> { "husband_xref", "wife_xref" }
            }
        }, ToolFamilyAdd);

        server.RegisterTool(new MCPTool {
            Name = "family_delete",
            Description = "Delete a family from the database by their XRef identifier",
            InputSchema = new MCPToolInputSchema {
                Properties = new Dictionary<string, MCPToolProperty> {
                    ["xref"] = new MCPToolProperty { Type = "string", Description = "XRef identifier of the family (e.g., 'F1')" }
                },
                Required = new List<string> { "xref" }
            }
        }, ToolFamilyDelete);

        // Notes

        server.RegisterTool(new MCPTool {
            Name = "note_list",
            Description = "List all notes in the database with pagination support (20 items per page)",
            InputSchema = new MCPToolInputSchema {
                Properties = new Dictionary<string, MCPToolProperty> {
                    ["page"] = new MCPToolProperty { Type = "integer", Description = "Page number (1-based, default: 1)" }
                },
                Required = new List<string> { }
            }
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

        server.RegisterTool(new MCPTool {
            Name = "note_delete",
            Description = "Delete a note from the database by their XRef identifier",
            InputSchema = new MCPToolInputSchema {
                Properties = new Dictionary<string, MCPToolProperty> {
                    ["xref"] = new MCPToolProperty { Type = "string", Description = "XRef identifier of the note (e.g., 'N1')" }
                },
                Required = new List<string> { "xref" }
            }
        }, ToolNoteDelete);

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

        server.RegisterTool(new MCPTool {
            Name = "source_delete",
            Description = "Delete a source from the database by their XRef identifier",
            InputSchema = new MCPToolInputSchema {
                Properties = new Dictionary<string, MCPToolProperty> {
                    ["xref"] = new MCPToolProperty { Type = "string", Description = "XRef identifier of the source (e.g., 'S1')" }
                },
                Required = new List<string> { "xref" }
            }
        }, ToolSourceDelete);

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

        server.RegisterTool(new MCPTool {
            Name = "repository_delete",
            Description = "Delete a repository from the database by their XRef identifier",
            InputSchema = new MCPToolInputSchema {
                Properties = new Dictionary<string, MCPToolProperty> {
                    ["xref"] = new MCPToolProperty { Type = "string", Description = "XRef identifier of the repository (e.g., 'R1')" }
                },
                Required = new List<string> { "xref" }
            }
        }, ToolRepositoryDelete);

        // Multimedia

        server.RegisterTool(new MCPTool {
            Name = "multimedia_list",
            Description = "List all multimedia records in the database with pagination support (20 items per page)",
            InputSchema = new MCPToolInputSchema {
                Properties = new Dictionary<string, MCPToolProperty> {
                    ["page"] = new MCPToolProperty { Type = "integer", Description = "Page number (1-based, default: 1)" }
                },
                Required = new List<string> { }
            }
        }, ToolMultimediaList);

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

        server.RegisterTool(new MCPTool {
            Name = "group_delete",
            Description = "Delete a group from the database by their XRef identifier",
            InputSchema = new MCPToolInputSchema {
                Properties = new Dictionary<string, MCPToolProperty> {
                    ["xref"] = new MCPToolProperty { Type = "string", Description = "XRef identifier of the group (e.g., 'G1')" }
                },
                Required = new List<string> { "xref" }
            }
        }, ToolGroupDelete);

        // Tasks

        server.RegisterTool(new MCPTool {
            Name = "task_list",
            Description = "List all tasks in the database with pagination support (20 items per page)",
            InputSchema = new MCPToolInputSchema {
                Properties = new Dictionary<string, MCPToolProperty> {
                    ["page"] = new MCPToolProperty { Type = "integer", Description = "Page number (1-based, default: 1)" }
                },
                Required = new List<string> { }
            }
        }, ToolTaskList);

        // Researches

        server.RegisterTool(new MCPTool {
            Name = "research_list",
            Description = "List all researches in the database with pagination support (20 items per page)",
            InputSchema = new MCPToolInputSchema {
                Properties = new Dictionary<string, MCPToolProperty> {
                    ["page"] = new MCPToolProperty { Type = "integer", Description = "Page number (1-based, default: 1)" }
                },
                Required = new List<string> { }
            }
        }, ToolResearchList);

        // Communications

        server.RegisterTool(new MCPTool {
            Name = "communication_list",
            Description = "List all communications in the database with pagination support (20 items per page)",
            InputSchema = new MCPToolInputSchema {
                Properties = new Dictionary<string, MCPToolProperty> {
                    ["page"] = new MCPToolProperty { Type = "integer", Description = "Page number (1-based, default: 1)" }
                },
                Required = new List<string> { }
            }
        }, ToolCommunicationList);

        // Locations

        server.RegisterTool(new MCPTool {
            Name = "location_list",
            Description = "List all locations in the database with pagination support (20 items per page)",
            InputSchema = new MCPToolInputSchema {
                Properties = new Dictionary<string, MCPToolProperty> {
                    ["page"] = new MCPToolProperty { Type = "integer", Description = "Page number (1-based, default: 1)" }
                },
                Required = new List<string> { }
            }
        }, ToolLocationList);
    }

    #region Files

    private List<MCPContent> ToolFileNew(JsonElement args)
    {
        fContext.Clear();

        return MCPContent.CreateSimpleContent($"Database created. Records: {fContext.Tree.RecordsCount}.");
    }

    private List<MCPContent> ToolFileLoad(JsonElement args)
    {
        string path = GetRequiredArgument(args, "path");

        var sw = System.Diagnostics.Stopwatch.StartNew();
        fContext.FileLoad(path, false).GetAwaiter().GetResult();
        sw.Stop();

        return MCPContent.CreateSimpleContent($"Database loaded: {path}. Records: {fContext.Tree.RecordsCount}. Time: {sw.Elapsed.TotalSeconds:F3}s.");
    }

    private List<MCPContent> ToolFileSave(JsonElement args)
    {
        string path = GetRequiredArgument(args, "path");

        var sw = System.Diagnostics.Stopwatch.StartNew();
        fContext.FileSave(path).GetAwaiter().GetResult();
        sw.Stop();

        return MCPContent.CreateSimpleContent($"Database saved: {path}. Time: {sw.Elapsed.TotalSeconds:F3}s.");
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

        return MCPContent.CreateSimpleContent(string.Join("\n", lines));
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

    private List<MCPContent> ToolFileReload(JsonElement args)
    {
        var globOpts = GlobalOptions.Instance;
        if (globOpts.MRUFiles.Count == 0)
            return MCPContent.CreateSimpleContent("No recently opened files available.");

        var mruFile = globOpts.MRUFiles[0];
        string path = mruFile.FileName;

        fContext.Clear();

        var sw = System.Diagnostics.Stopwatch.StartNew();
        fContext.FileLoad(path, false).GetAwaiter().GetResult();
        sw.Stop();

        return MCPContent.CreateSimpleContent($"Database reloaded: {path.Replace('\\', '/')}. Records: {fContext.Tree.RecordsCount}. Time: {sw.Elapsed.TotalSeconds:F3}s.");
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

    private List<MCPContent> ToolFileValidate(JsonElement args)
    {
        bool isUnknown = fContext.IsUnknown();
        int recordsCount = fContext.Tree.RecordsCount;

        if (isUnknown && recordsCount == 0) {
            return MCPContent.CreateSimpleContent("Database state: INVALID (unknown, no records). The file is new, empty, or corrupted.");
        }

        var lines = new List<string> {
            $"Database state: VALID",
            $"  Unknown: {isUnknown}",
            $"  Records: {recordsCount}"
        };

        return MCPContent.CreateSimpleContent(string.Join("\n", lines));
    }

    #endregion

    #region Record Common

    private static readonly Dictionary<string, GDMRecordType> RecordTypeMap = new Dictionary<string, GDMRecordType>(StringComparer.OrdinalIgnoreCase)
    {
        ["Individual"] = GDMRecordType.rtIndividual,
        ["Family"] = GDMRecordType.rtFamily,
        ["Note"] = GDMRecordType.rtNote,
        ["Source"] = GDMRecordType.rtSource,
        ["Repository"] = GDMRecordType.rtRepository,
        ["Multimedia"] = GDMRecordType.rtMultimedia,
        ["Group"] = GDMRecordType.rtGroup,
        ["Task"] = GDMRecordType.rtTask,
        ["Research"] = GDMRecordType.rtResearch,
        ["Communication"] = GDMRecordType.rtCommunication,
        ["Location"] = GDMRecordType.rtLocation
    };

    private List<MCPContent> ToolRecordSearch(JsonElement args)
    {
        string recordTypeStr = GetRequiredArgument(args, "record_type");
        string searchText = GetRequiredArgument(args, "search_text");
        double threshold = GetDoubleArgument(args, "threshold", 0.15);

        if (!RecordTypeMap.TryGetValue(recordTypeStr, out GDMRecordType recordType))
        {
            string availableTypes = string.Join(", ", RecordTypeMap.Keys);
            return MCPContent.CreateSimpleContent($"Unknown record type: '{recordTypeStr}'. Available types: {availableTypes}");
        }

        var recList = fContext.Tree.GetRecords(recordType);
        if (recList.Count == 0)
            return MCPContent.CreateSimpleContent($"No {recordTypeStr.ToLower()} records in database.");

        var matches = new List<string>();
        foreach (var rec in recList)
        {
            string recordName = GKUtils.GetRecordName(fContext.Tree, rec, false);
            int diff = SysUtils.GetDiffIndex(searchText, recordName);
            double matchThreshold = recordName.Length * threshold;

            if (diff <= matchThreshold)
            {
                matches.Add($"|{rec.XRef}|{recordName}|{diff}|");
            }
        }

        if (matches.Count == 0)
            return MCPContent.CreateSimpleContent($"No matches found for '{searchText}' in {recordTypeStr.ToLower()} records.");

        var lines = new List<string> {
            $"Search results for '{searchText}' in {recordTypeStr} records ({matches.Count} matches):",
            "| XRef | Name | Diff |",
            "|---|---|---|"
        };
        lines.AddRange(matches);
        return MCPContent.CreateSimpleContent(string.Join("\n", lines));
    }

    #endregion

    #region Individuals

    private List<MCPContent> ToolIndividualList(JsonElement args)
    {
        var recList = fContext.Tree.GetRecords(GDMRecordType.rtIndividual);
        return PageableTable("individuals", args, recList.Count, (int index) => {
            if (index == -1) {
                return "| XRef | Name | Sex |\n|---|---|---|";
            } else {
                var iRec = (GDMIndividualRecord)recList[index];
                string indiName = GKUtils.GetRecordName(fContext.Tree, iRec, false);
                string sex = GKData.SexData[(int)iRec.Sex].Sign;
                return $"|{iRec.XRef}|{indiName}|{sex}|";
            }
        });
    }

    private List<MCPContent> ToolIndividualSearch(JsonElement args)
    {
        string searchName = GetRequiredArgument(args, "name");

        var recList = fContext.Tree.GetRecords(GDMRecordType.rtIndividual);
        if (recList.Count == 0)
            return MCPContent.CreateSimpleContent("No individuals in database.");

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
            return MCPContent.CreateSimpleContent($"No matches found for: {searchName}");

        var lines = new List<string> {
            $"Search results for '{searchName}' ({matches.Count}):",
            "| XRef | Name | Sex | Diff |",
            "|---|---|---|---|"
        };
        lines.AddRange(matches);
        return MCPContent.CreateSimpleContent(string.Join("\n", lines));
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
        return MCPContent.CreateSimpleContent($"Individual added: {resultName}");
    }

    private List<MCPContent> ToolIndividualDelete(JsonElement args)
    {
        string xref = GetRequiredArgument(args, "xref");

        var indiRec = fContext.Tree.FindXRef<GDMIndividualRecord>(xref);
        if (indiRec == null)
            return MCPContent.CreateSimpleContent($"Individual not found with XRef: {xref}");

        fContext.DeleteRecord(indiRec);

        return MCPContent.CreateSimpleContent($"Individual deleted: {xref}");
    }

    #endregion

    #region Families

    private List<MCPContent> ToolFamilyList(JsonElement args)
    {
        var recList = fContext.Tree.GetRecords(GDMRecordType.rtFamily);
        return PageableTable("families", args, recList.Count, (int index) => {
            if (index == -1) {
                return "| XRef | Husband | Wife |\n|---|---|---|";
            } else {
                var famRec = (GDMFamilyRecord)recList[index];
                var husbandRec = fContext.Tree.GetPtrValue(famRec.Husband);
                string husbandName = husbandRec == null ? "-" : GKUtils.GetRecordName(fContext.Tree, husbandRec, false);
                var wifeRec = fContext.Tree.GetPtrValue(famRec.Wife);
                string wifeName = wifeRec == null ? "-" : GKUtils.GetRecordName(fContext.Tree, wifeRec, false);
                return $"|{famRec.XRef}|{husbandName}|{wifeName}|";
            }
        });
    }

    private List<MCPContent> ToolFamilyAdd(JsonElement args)
    {
        string husbandXRef = GetRequiredArgument(args, "husband_xref");
        string wifeXRef = GetRequiredArgument(args, "wife_xref");

        // Find the husband by XRef
        var husbandRec = fContext.Tree.FindXRef<GDMIndividualRecord>(husbandXRef);
        if (husbandRec == null)
            return MCPContent.CreateSimpleContent($"Husband not found with XRef: {husbandXRef}");

        // Find the wife by XRef
        var wifeRec = fContext.Tree.FindXRef<GDMIndividualRecord>(wifeXRef);
        if (wifeRec == null)
            return MCPContent.CreateSimpleContent($"Wife not found with XRef: {wifeXRef}");

        // Create a new family record
        var familyRec = fContext.Tree.CreateFamily();
        familyRec.AddSpouse(husbandRec);
        familyRec.AddSpouse(wifeRec);
        fContext.SetModified();

        return MCPContent.CreateSimpleContent($"Family added: husband {husbandXRef}, wife {wifeXRef}");
    }

    private List<MCPContent> ToolFamilyDelete(JsonElement args)
    {
        string xref = GetRequiredArgument(args, "xref");

        var familyRec = fContext.Tree.FindXRef<GDMFamilyRecord>(xref);
        if (familyRec == null)
            return MCPContent.CreateSimpleContent($"Family not found with XRef: {xref}");

        fContext.DeleteRecord(familyRec);

        return MCPContent.CreateSimpleContent($"Family deleted: {xref}");
    }

    #endregion

    #region Notes

    private List<MCPContent> ToolNoteList(JsonElement args)
    {
        var recList = fContext.Tree.GetRecords(GDMRecordType.rtNote);
        return PageableTable("notes", args, recList.Count, (int index) => {
            if (index == -1) {
                return "| XRef | Text preview |\n|---|---|";
            } else {
                var rec = (GDMNoteRecord)recList[index];
                string preview = rec.Lines.Text;
                if (preview != null && preview.Length > 80)
                    preview = preview.Substring(0, 80) + "...";
                return $"|{rec.XRef}|{preview}|";
            }
        });
    }

    private List<MCPContent> ToolNoteAdd(JsonElement args)
    {
        string text = GetRequiredArgument(args, "text");

        var noteRec = fContext.Tree.CreateNote();
        noteRec.Lines.Text = text;
        fContext.SetModified();

        return MCPContent.CreateSimpleContent($"Note added: {text}");
    }

    private List<MCPContent> ToolNoteDelete(JsonElement args)
    {
        string xref = GetRequiredArgument(args, "xref");

        var noteRec = fContext.Tree.FindXRef<GDMNoteRecord>(xref);
        if (noteRec == null)
            return MCPContent.CreateSimpleContent($"Note not found with XRef: {xref}");

        fContext.DeleteRecord(noteRec);

        return MCPContent.CreateSimpleContent($"Note deleted: {xref}");
    }

    #endregion

    #region Sources

    private List<MCPContent> ToolSourceList(JsonElement args)
    {
        var recList = fContext.Tree.GetRecords(GDMRecordType.rtSource);
        return PageableTable("sources", args, recList.Count, (int index) => {
            if (index == -1) {
                return "| XRef | Short title | Source title |\n|---|---|---|";
            } else {
                var rec = (GDMSourceRecord)recList[index];
                string sourceTitle = rec.Title.Lines.Text;
                return $"|{rec.XRef}|{rec.ShortTitle}|{sourceTitle}|";
            }
        });
    }

    private List<MCPContent> ToolSourceAdd(JsonElement args)
    {
        string title = GetRequiredArgument(args, "title");

        var sourceRec = fContext.Tree.CreateSource();
        sourceRec.Title.Lines.Text = title;
        fContext.SetModified();

        return MCPContent.CreateSimpleContent($"Source added: {title}");
    }

    private List<MCPContent> ToolSourceDelete(JsonElement args)
    {
        string xref = GetRequiredArgument(args, "xref");

        var sourceRec = fContext.Tree.FindXRef<GDMSourceRecord>(xref);
        if (sourceRec == null)
            return MCPContent.CreateSimpleContent($"Source not found with XRef: {xref}");

        fContext.DeleteRecord(sourceRec);

        return MCPContent.CreateSimpleContent($"Source deleted: {xref}");
    }

    #endregion

    #region Repositories

    private List<MCPContent> ToolRepositoryList(JsonElement args)
    {
        var recList = fContext.Tree.GetRecords(GDMRecordType.rtRepository);
        if (recList.Count == 0)
            return MCPContent.CreateSimpleContent("No repositories in database.");

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

        return MCPContent.CreateSimpleContent(string.Join("\n", rows));
    }

    private List<MCPContent> ToolRepositoryAdd(JsonElement args)
    {
        string name = GetRequiredArgument(args, "name");

        var repoRec = fContext.Tree.CreateRepository();
        repoRec.RepositoryName = name;
        fContext.SetModified();

        return MCPContent.CreateSimpleContent($"Repository added: {name}");
    }

    private List<MCPContent> ToolRepositoryDelete(JsonElement args)
    {
        string xref = GetRequiredArgument(args, "xref");

        var repoRec = fContext.Tree.FindXRef<GDMRepositoryRecord>(xref);
        if (repoRec == null)
            return MCPContent.CreateSimpleContent($"Repository not found with XRef: {xref}");

        fContext.DeleteRecord(repoRec);

        return MCPContent.CreateSimpleContent($"Repository deleted: {xref}");
    }

    #endregion

    #region Multimedia

    private List<MCPContent> ToolMultimediaList(JsonElement args)
    {
        var recList = fContext.Tree.GetRecords(GDMRecordType.rtMultimedia);
        return PageableTable("multimedia", args, recList.Count, (int index) => {
            if (index == -1) {
                return "| XRef | Title | Type | File |\n|---|---|---|---|";
            } else {
                var rec = (GDMMultimediaRecord)recList[index];
                var fileRef = rec.FileReferences.Count > 0 ? rec.FileReferences[0] : null;
                if (fileRef == null) return string.Empty;

                string title = fileRef.Title;
                string mediaType = LangMan.LS(GKData.MediaTypes[(int)fileRef.MediaType]);
                string file = fileRef.StringValue;
                return $"|{rec.XRef}|{title}|{mediaType}|{file}|";
            }
        });
    }

    #endregion

    #region Groups

    private List<MCPContent> ToolGroupList(JsonElement args)
    {
        var recList = fContext.Tree.GetRecords(GDMRecordType.rtGroup);
        if (recList.Count == 0)
            return MCPContent.CreateSimpleContent("No groups in database.");

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

        return MCPContent.CreateSimpleContent(string.Join("\n", rows));
    }

    private List<MCPContent> ToolGroupAdd(JsonElement args)
    {
        string name = GetRequiredArgument(args, "name");

        var groupRec = fContext.Tree.CreateGroup();
        groupRec.GroupName = name;
        fContext.SetModified();

        return MCPContent.CreateSimpleContent($"Group added: {name}");
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
            return MCPContent.CreateSimpleContent($"Group not found: {groupName}");

        // Find the individual by XRef
        var indiRec = fContext.Tree.FindXRef<GDMIndividualRecord>(individualXRef);
        if (indiRec == null)
            return MCPContent.CreateSimpleContent($"Individual not found with XRef: {individualXRef}");

        // Check if already a member
        if (targetGroup.IndexOfMember(indiRec) >= 0)
            return MCPContent.CreateSimpleContent($"Individual {individualXRef} is already a member of group '{groupName}'.");

        targetGroup.AddMember(indiRec);
        fContext.SetModified();

        string indiName = GKUtils.GetNameString(indiRec, false);
        return MCPContent.CreateSimpleContent($"Individual added to group '{groupName}': {indiName} ({individualXRef})");
    }

    private List<MCPContent> ToolGroupDelete(JsonElement args)
    {
        string xref = GetRequiredArgument(args, "xref");

        var groupRec = fContext.Tree.FindXRef<GDMGroupRecord>(xref);
        if (groupRec == null)
            return MCPContent.CreateSimpleContent($"Group not found with XRef: {xref}");

        fContext.DeleteRecord(groupRec);

        return MCPContent.CreateSimpleContent($"Group deleted: {xref}");
    }

    #endregion

    #region Tasks

    private List<MCPContent> ToolTaskList(JsonElement args)
    {
        var recList = fContext.Tree.GetRecords(GDMRecordType.rtTask);
        return PageableTable("tasks", args, recList.Count, (int index) => {
            if (index == -1) {
                return "| XRef | Goal | Priority | Start Date | Stop Date |\n|---|---|---|---|---|";
            } else {
                var rec = (GDMTaskRecord)recList[index];
                string goal = GKUtils.GetTaskGoalStr(fContext.Tree, rec);
                string priority = LangMan.LS(GKData.PriorityNames[(int)rec.Priority]);
                string startDate = GetDateValue(rec.StartDate);
                string stopDate = GetDateValue(rec.StopDate);
                return $"|{rec.XRef}|{goal}|{priority}|{startDate}|{stopDate}|";
            }
        });
    }

    #endregion

    #region Researches

    private List<MCPContent> ToolResearchList(JsonElement args)
    {
        var recList = fContext.Tree.GetRecords(GDMRecordType.rtResearch);
        return PageableTable("researches", args, recList.Count, (int index) => {
            if (index == -1) {
                return "| XRef | Name | Priority | Status | Start Date | Stop Date | Percent |\n|---|---|---|---|---|---|---|";
            } else {
                var rec = (GDMResearchRecord)recList[index];
                string name = rec.ResearchName;
                string priority = LangMan.LS(GKData.PriorityNames[(int)rec.Priority]);
                string status = LangMan.LS(GKData.StatusNames[(int)rec.Status]);
                string startDate = GetDateValue(rec.StartDate);
                string stopDate = GetDateValue(rec.StopDate);
                string percent = rec.Percent.ToString();
                return $"|{rec.XRef}|{name}|{priority}|{status}|{startDate}|{stopDate}|{percent}|";
            }
        });
    }

    #endregion

    #region Communications

    private List<MCPContent> ToolCommunicationList(JsonElement args)
    {
        var recList = fContext.Tree.GetRecords(GDMRecordType.rtCommunication);
        return PageableTable("communications", args, recList.Count, (int index) => {
            if (index == -1) {
                return "| XRef | Theme | Corresponder | Type | Date |\n|---|---|---|---|---|";
            } else {
                var rec = (GDMCommunicationRecord)recList[index];
                string theme = rec.CommName;
                string corresponder = GKUtils.GetCorresponderStr(fContext.Tree, rec, false);
                string type = LangMan.LS(GKData.CommunicationNames[(int)rec.CommunicationType]);
                string date = GetDateValue(rec.Date);
                return $"|{rec.XRef}|{theme}|{corresponder}|{type}|{date}|";
            }
        });
    }

    #endregion

    #region Locations

    private List<MCPContent> ToolLocationList(JsonElement args)
    {
        var recList = fContext.Tree.GetRecords(GDMRecordType.rtLocation);
        return PageableTable("locations", args, recList.Count, (int index) => {
            if (index == -1) {
                return "| XRef | Name | Lati | Long |\n|---|---|---|---|";
            } else {
                var rec = (GDMLocationRecord)recList[index];
                string name = rec.GetNameByDate(null, true);
                string lat = rec.Map.Lati.ToString();
                string lng = rec.Map.Long.ToString();
                return $"|{rec.XRef}|{name}|{lat}|{lng}|";
            }
        });
    }

    #endregion

    #region Utilities

    private delegate string RowBuilder(int index);

    private List<MCPContent> PageableTable(string tableName, JsonElement args, int recordCount, RowBuilder buildRow)
    {
        if (recordCount == 0)
            return MCPContent.CreateSimpleContent($"No {tableName} in database.");

        int page = GetIntArgument(args, "page", 1);
        if (page < 1) page = 1;

        int totalPages = (recordCount + pageSize - 1) / pageSize;
        if (page > totalPages) page = totalPages;

        int startIndex = (page - 1) * pageSize;
        int endIndex = Math.Min(startIndex + pageSize, recordCount);

        var lines = new StringBuilder();
        lines.Append($"{tableName} (page {page}/{totalPages}, {startIndex + 1}-{endIndex} of {recordCount}):\n");
        lines.Append(buildRow(-1)); // header
        for (int i = startIndex; i < endIndex; i++) {
            lines.Append("\n");
            lines.Append(buildRow(i));
        }
        if (page < totalPages) {
            lines.Append($"\n\n_Next page: use parameter page={page + 1}_");
        }

        MCPServer.Log($"Successfully generated response for {recordCount} rows (page {page}/{totalPages})");

        return MCPContent.CreateSimpleContent(lines.ToString());
    }

    protected static string GetDateValue(GDMCustomDate date)
    {
        string result;

        if (date == null) {
            result = string.Empty;
        } else {
            GlobalOptions glob = GlobalOptions.Instance;
            result = date.GetDisplayString(glob.DefDateFormat, glob.ShowDatesSign, glob.ShowDatesCalendar);
        }

        return result;
    }

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

    private static double GetDoubleArgument(JsonElement args, string argName, double defaultValue)
    {
        if (!args.TryGetProperty(argName, out var argElem) || argElem.ValueKind != JsonValueKind.Number)
            return defaultValue;

        return argElem.GetDouble();
    }

    #endregion
}
