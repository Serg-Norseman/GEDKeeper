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
using GKcli.MCP;
using GKCore;
using GKCore.Locales;
using GKCore.Utilities;
using GKUI.Platform;
using Sharprompt;

namespace GKcli.Commands;

internal abstract class RecordCommand : BaseCommand
{
    public RecordCommand(string sign, Enum lsid, CommandCategory category) : base(sign, lsid, category) { }

    protected static GDMRecord SelectRecord(BaseContext baseContext, GDMRecordType recordType, string prompt, string yesMsg, string noMsg)
    {
        GDMRecord result = null;

        var recList = baseContext.Tree.GetRecords(recordType);
        if (recList.Count > 0) {
            result = Prompt.Select(prompt, recList,
                pageSize: 10,
                textSelector: (GDMRecord r) => { return GKUtils.GetRecordName(baseContext.Tree, r, false); });

            PromptHelper.WriteLine(string.Format(yesMsg, GKUtils.GetRecordName(baseContext.Tree, result, false)));
        } else {
            PromptHelper.WriteLine(noMsg);
        }

        return result;
    }

    protected static void DeleteRecord<T>(BaseContext baseContext, string expectedMsg) where T : GDMRecord
    {
        var rec = CommandController.GetVariable<T>("selectedObj");
        if (rec == null) {
            PromptHelper.WriteLine(expectedMsg);
            return;
        }

        bool result = CommandController.GetConfirm("Удалить запись");
        if (result) {
            baseContext.DeleteRecord(rec);
            CommandController.SetVariable("selectedObj", null);
        }
    }
}


internal class RecordSearchCommand : RecordCommand
{
    public RecordSearchCommand() : base("record_search", LSID.Search, CommandCategory.Tools) { }

    public override void Execute(BaseContext baseContext, object obj)
    {
        // Empty for interactive mode
    }

    private static readonly Dictionary<string, GDMRecordType> RecordTypeMap = new Dictionary<string, GDMRecordType>(StringComparer.OrdinalIgnoreCase) {
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

    public override MCPTool CreateTool()
    {
        return new MCPTool {
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
        };
    }

    public override List<MCPContent> ExecuteTool(BaseContext baseContext, JsonElement args)
    {
        string recordTypeStr = MCPHelper.GetRequiredArgument(args, "record_type");
        string searchText = MCPHelper.GetRequiredArgument(args, "search_text");
        double threshold = MCPHelper.GetDoubleArgument(args, "threshold", 0.15);

        if (!RecordTypeMap.TryGetValue(recordTypeStr, out GDMRecordType recordType)) {
            string availableTypes = string.Join(", ", RecordTypeMap.Keys);
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
