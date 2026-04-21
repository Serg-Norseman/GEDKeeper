/*
 *  GEDKeeper, the personal genealogical database editor.
 *  Copyright (C) 2009-2026 by Sergey V. Zhdanovskih.
 *
 *  Licensed under the GNU General Public License (GPL) v3.
 *  See LICENSE file in the project root for full license information.
 */

using System.Collections.Generic;
using System.Linq;
using System.Text.Json;
using BSLib;
using GDModel;
using GKcli.MCP;
using GKCore;
using GKCore.Design;
using GKCore.Locales;
using GKCore.Tools;
using GKCore.Utilities;
using GKUI.Platform;

namespace GKcli.Commands;

/// <summary>
/// For MCP use only (for console - see <RecordType>ListCommand).
///
/// A general tool has been introduced to minimize the system prompt tokens
/// that are used to pass tools to the model.
/// </summary>
internal class RecordListCommand : BaseCommand
{
    public RecordListCommand() : base("record_list", null, CommandCategory.Tools) { }

    public override void Execute(BaseContext baseContext, object obj)
    {
        // Not applicable for MCP
    }

    public override MCPTool CreateTool()
    {
        var recTypes = RuntimeData.RecordTypeMap.Keys.ToList();

        return new MCPTool {
            Name = Sign,
            Description = "List records in the database by the specified type with pagination support (20 items per page)",
            InputSchema = new MCPToolInputSchema {
                Properties = new Dictionary<string, MCPToolProperty> {
                    ["record_type"] = new MCPToolProperty { Type = "string", Description = "Record type", Enum = recTypes },
                    ["page"] = new MCPToolProperty { Type = "integer", Description = "Page number (1-based, default: 1)" }
                },
                Required = new List<string> { }
            }
        };
    }

    public override List<MCPContent> ExecuteTool(BaseContext baseContext, JsonElement args)
    {
        string recordTypeStr = MCPHelper.GetRequiredArgument(args, "record_type");
        if (!RuntimeData.RecordTypeMap.TryGetValue(recordTypeStr, out GDMRecordType recordType)) {
            string availableTypes = string.Join(", ", RuntimeData.RecordTypeMap.Keys);
            return MCPContent.CreateSimpleContent($"Unknown record type: '{recordTypeStr}'. Available types: {availableTypes}");
        }

        var recList = baseContext.Tree.GetRecords(recordType);

        switch (recordType) {
            case GDMRecordType.rtIndividual:
                return MCPHelper.PageableTable("individuals", args, recList.Count, (int index) => {
                    if (index == -1) {
                        return "| XRef | Name | Sex |\n|---|---|---|";
                    } else {
                        var iRec = (GDMIndividualRecord)recList[index];
                        string indiName = GKUtils.GetRecordName(baseContext.Tree, iRec, false);
                        string sex = GKData.SexData[(int)iRec.Sex].Sign;
                        return $"|{iRec.XRef}|{indiName}|{sex}|";
                    }
                });

            case GDMRecordType.rtFamily:
                return MCPHelper.PageableTable("families", args, recList.Count, (int index) => {
                    if (index == -1) {
                        return "| XRef | Husband | Wife |\n|---|---|---|";
                    } else {
                        var famRec = (GDMFamilyRecord)recList[index];
                        var husbandRec = baseContext.Tree.GetPtrValue(famRec.Husband);
                        string husbandName = husbandRec == null ? "-" : GKUtils.GetRecordName(baseContext.Tree, husbandRec, false);
                        var wifeRec = baseContext.Tree.GetPtrValue(famRec.Wife);
                        string wifeName = wifeRec == null ? "-" : GKUtils.GetRecordName(baseContext.Tree, wifeRec, false);
                        return $"|{famRec.XRef}|{husbandName}|{wifeName}|";
                    }
                });

            case GDMRecordType.rtNote:
                return MCPHelper.PageableTable("notes", args, recList.Count, (int index) => {
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

            case GDMRecordType.rtMultimedia:
                // filenames removed to save tokens
                return MCPHelper.PageableTable("multimedia", args, recList.Count, (int index) => {
                    if (index == -1) {
                        return "| XRef | Title | Type |\n|---|---|---|";
                    } else {
                        var rec = (GDMMultimediaRecord)recList[index];
                        var fileRef = rec.FileReferences.Count > 0 ? rec.FileReferences[0] : null;
                        if (fileRef == null) return string.Empty;

                        string title = fileRef.Title;
                        string mediaType = LangMan.LS(GKData.MediaTypes[(int)fileRef.MediaType]);
                        return $"|{rec.XRef}|{title}|{mediaType}|";
                    }
                });

            case GDMRecordType.rtSource:
                return MCPHelper.PageableTable("sources", args, recList.Count, (int index) => {
                    if (index == -1) {
                        return "| XRef | Short title | Source title |\n|---|---|---|";
                    } else {
                        var rec = (GDMSourceRecord)recList[index];
                        string sourceTitle = rec.Title.Lines.Text;
                        return $"|{rec.XRef}|{rec.ShortTitle}|{sourceTitle}|";
                    }
                });

            case GDMRecordType.rtRepository:
                return MCPHelper.PageableTable("repositories", args, recList.Count, (int index) => {
                    if (index == -1) {
                        return "| XRef | Repository |\n|---|---|";
                    } else {
                        var rec = (GDMRepositoryRecord)recList[index];
                        return $"|{rec.XRef}|{rec.RepositoryName}|";
                    }
                });

            case GDMRecordType.rtGroup:
                return MCPHelper.PageableTable("groups", args, recList.Count, (int index) => {
                    if (index == -1) {
                        return "| XRef | Group | Members |\n|---|---|---|";
                    } else {
                        var rec = (GDMGroupRecord)recList[index];
                        int membersCount = rec.Members.Count;
                        return $"|{rec.XRef}|{rec.GroupName}|{membersCount}|";
                    }
                });

            case GDMRecordType.rtResearch:
                return MCPHelper.PageableTable("researches", args, recList.Count, (int index) => {
                    if (index == -1) {
                        return "| XRef | Name | Priority | Status | Start Date | Stop Date | Percent |\n|---|---|---|---|---|---|---|";
                    } else {
                        var rec = (GDMResearchRecord)recList[index];
                        string name = rec.ResearchName;
                        string priority = LangMan.LS(GKData.PriorityNames[(int)rec.Priority]);
                        string status = LangMan.LS(GKData.StatusNames[(int)rec.Status]);
                        string startDate = GKUtils.GetDateDisplayString(rec.StartDate);
                        string stopDate = GKUtils.GetDateDisplayString(rec.StopDate);
                        string percent = rec.Percent.ToString();
                        return $"|{rec.XRef}|{name}|{priority}|{status}|{startDate}|{stopDate}|{percent}|";
                    }
                });

            case GDMRecordType.rtTask:
                return MCPHelper.PageableTable("tasks", args, recList.Count, (int index) => {
                    if (index == -1) {
                        return "| XRef | Goal | Priority | Start Date | Stop Date |\n|---|---|---|---|---|";
                    } else {
                        var rec = (GDMTaskRecord)recList[index];
                        string goal = GKUtils.GetTaskGoalStr(baseContext.Tree, rec);
                        string priority = LangMan.LS(GKData.PriorityNames[(int)rec.Priority]);
                        string startDate = GKUtils.GetDateDisplayString(rec.StartDate);
                        string stopDate = GKUtils.GetDateDisplayString(rec.StopDate);
                        return $"|{rec.XRef}|{goal}|{priority}|{startDate}|{stopDate}|";
                    }
                });

            case GDMRecordType.rtCommunication:
                return MCPHelper.PageableTable("communications", args, recList.Count, (int index) => {
                    if (index == -1) {
                        return "| XRef | Theme | Corresponder | Type | Date |\n|---|---|---|---|---|";
                    } else {
                        var rec = (GDMCommunicationRecord)recList[index];
                        string theme = rec.CommName;
                        string corresponder = GKUtils.GetCorresponderStr(baseContext.Tree, rec, false);
                        string type = LangMan.LS(GKData.CommunicationNames[(int)rec.CommunicationType]);
                        string date = GKUtils.GetDateDisplayString(rec.Date);
                        return $"|{rec.XRef}|{theme}|{corresponder}|{type}|{date}|";
                    }
                });

            case GDMRecordType.rtLocation:
                return MCPHelper.PageableTable("locations", args, recList.Count, (int index) => {
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

            default:
                return MCPContent.CreateSimpleContent($"Unsupported record type: '{recordTypeStr}'");
        }
    }
}


internal class RecordSearchCommand : BaseCommand
{
    public RecordSearchCommand() : base("record_search", LSID.Search, CommandCategory.Tools) { }

    public override void Execute(BaseContext baseContext, object obj)
    {
        // Not implemented yet
    }

    public override MCPTool CreateTool()
    {
        var recTypes = RuntimeData.RecordTypeMap.Keys.ToList();

        return new MCPTool {
            Name = "record_search",
            Description = "Search for any records by name/title using fuzzy matching. Available record types: Individual, Family, Note, Source, Repository, Multimedia, Group, Task, Research, Communication, Location.",
            InputSchema = new MCPToolInputSchema {
                Properties = new Dictionary<string, MCPToolProperty> {
                    ["record_type"] = new MCPToolProperty { Type = "string", Description = "Record type", Enum = recTypes },
                    ["search_text"] = new MCPToolProperty { Type = "string", Description = "Text to search for (name, title, etc.)" },
                    ["threshold"] = new MCPToolProperty { Type = "number", Description = "Fuzzy match threshold (0.0-1.0, default: 0.15). Lower = stricter match." }
                },
                Required = new List<string> { "record_type", "search_text" }
            }
        };
    }

    public override List<MCPContent> ExecuteTool(BaseContext baseContext, JsonElement args)
    {
        string recordTypeStr = MCPHelper.GetRequiredArgument(args, "record_type");
        string searchText = MCPHelper.GetRequiredArgument(args, "search_text");
        double threshold = MCPHelper.GetDoubleArgument(args, "threshold", 0.15);

        if (!RuntimeData.RecordTypeMap.TryGetValue(recordTypeStr, out GDMRecordType recordType)) {
            string availableTypes = string.Join(", ", RuntimeData.RecordTypeMap.Keys);
            return MCPContent.CreateSimpleContent($"Unknown record type: '{recordTypeStr}'. Available types: {availableTypes}");
        }

        var recList = baseContext.Tree.GetRecords(recordType);
        if (recList.Count == 0)
            return MCPContent.CreateSimpleContent($"No {recordTypeStr.ToLower()} records in database.");

        var matches = new List<string>();
        foreach (var rec in recList) {
            string recordName = GKUtils.GetRecordName(baseContext.Tree, rec, false);
            int diff = SysUtils.GetDiffIndex(searchText, recordName);
            double matchThreshold = recordName.Length * threshold;

            if (diff <= matchThreshold) {
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
}


internal class RecordInfoCommand : BaseCommand
{
    public RecordInfoCommand() : base("record_info", null, CommandCategory.Service) { }

    public override void Execute(BaseContext baseContext, object obj)
    {
        // Not implemented yet
    }

    public override MCPTool CreateTool()
    {
        return new MCPTool {
            Name = Sign,
            Description = "Provides complete information about a record by its XRef identifier.",
            InputSchema = new MCPToolInputSchema {
                Properties = new Dictionary<string, MCPToolProperty> {
                    ["xref"] = new MCPToolProperty { Type = "string", Description = "XRef identifier of the record (e.g., 'I1', 'F1', 'S1')" }
                },
                Required = new List<string> { "xref" }
            }
        };
    }

    public override List<MCPContent> ExecuteTool(BaseContext baseContext, JsonElement args)
    {
        string xref = MCPHelper.GetRequiredArgument(args, "xref");

        var record = baseContext.Tree.FindXRef<GDMRecord>(xref);
        if (record == null)
            return MCPContent.CreateSimpleContent($"Record not found with XRef: {xref}");

        StringList ctx = new StringList();
        GKUtils.GetRecordContent(baseContext, record, ctx, RecordContentType.Quick);
        string text = BbCodeConverter.ToMarkdown(ctx.Text);

        return MCPContent.CreateSimpleContent(text);
    }
}


/// <summary>
/// For MCP use only (for console - see <RecordType>DeleteCommand).
///
/// A general tool has been introduced to minimize the system prompt tokens
/// that are used to pass tools to the model.
/// </summary>
internal class RecordDeleteCommand : BaseCommand
{
    public RecordDeleteCommand() : base("record_delete", null, CommandCategory.Service) { }

    public override void Execute(BaseContext baseContext, object obj)
    {
        // Not applicable for MCP
    }

    public override MCPTool CreateTool()
    {
        return new MCPTool {
            Name = Sign,
            Description = "Delete a record from the database by its XRef identifier",
            InputSchema = new MCPToolInputSchema {
                Properties = new Dictionary<string, MCPToolProperty> {
                    ["xref"] = new MCPToolProperty { Type = "string", Description = "XRef identifier of the record" }
                },
                Required = new List<string> { "xref" }
            }
        };
    }

    public override List<MCPContent> ExecuteTool(BaseContext baseContext, JsonElement args)
    {
        string xref = MCPHelper.GetRequiredArgument(args, "xref");

        var record = baseContext.Tree.FindXRef<GDMRecord>(xref);
        if (record == null)
            return MCPContent.CreateSimpleContent($"Record not found with XRef: {xref}");

        baseContext.DeleteRecord(record);

        return MCPContent.CreateSimpleContent($"Record deleted: {xref}");
    }
}


internal class RecordMergeCommand : BaseCommand
{
    public RecordMergeCommand() : base("record_merge", null, CommandCategory.Service) { }

    public override void Execute(BaseContext baseContext, object obj)
    {
        // Not applicable for MCP
    }

    public override MCPTool CreateTool()
    {
        return new MCPTool {
            Name = Sign,
            Description = "Merge records by their XRef identifiers.",
            InputSchema = new MCPToolInputSchema {
                Properties = new Dictionary<string, MCPToolProperty> {
                    ["target_xref"] = new MCPToolProperty { Type = "string", Description = "XRef identifier of the target record" },
                    ["source_xref"] = new MCPToolProperty { Type = "string", Description = "XRef identifier of the record being merged" }
                },
                Required = new List<string> { "target_xref", "source_xref" }
            }
        };
    }

    public override List<MCPContent> ExecuteTool(BaseContext baseContext, JsonElement args)
    {
        string targetXRef = MCPHelper.GetRequiredArgument(args, "target_xref");
        var targetRecord = baseContext.Tree.FindXRef<GDMRecord>(targetXRef);
        if (targetRecord == null)
            return MCPContent.CreateSimpleContent($"Target record not found with XRef: {targetXRef}");

        string sourceXRef = MCPHelper.GetRequiredArgument(args, "source_xref");
        var sourceRecord = baseContext.Tree.FindXRef<GDMRecord>(sourceXRef);
        if (sourceRecord == null)
            return MCPContent.CreateSimpleContent($"Merged record not found with XRef: {sourceXRef}");

        TreeTools.MergeRecord(baseContext, targetRecord, sourceRecord, false);

        return MCPContent.CreateSimpleContent($"Record deleted: {sourceXRef}");
    }
}
