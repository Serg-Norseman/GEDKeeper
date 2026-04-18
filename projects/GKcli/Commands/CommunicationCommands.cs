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
using GDModel;
using GKcli.MCP;
using GKCore;
using GKCore.Locales;

namespace GKcli.Commands;

internal class CommunicationListCommand : BaseCommand
{
    public CommunicationListCommand() : base("communication_list", null, CommandCategory.Communication) { }

    public override void Execute(BaseContext baseContext, object obj)
    {
        // Not implemented yet
    }

    public override MCPTool CreateTool()
    {
        return new MCPTool {
            Name = Sign,
            Description = "List all communications in the database with pagination support (20 items per page)",
            InputSchema = new MCPToolInputSchema {
                Properties = new Dictionary<string, MCPToolProperty> {
                    ["page"] = new MCPToolProperty { Type = "integer", Description = "Page number (1-based, default: 1)" }
                },
                Required = new List<string> { }
            }
        };
    }

    public override List<MCPContent> ExecuteTool(BaseContext baseContext, JsonElement args)
    {
        var recList = baseContext.Tree.GetRecords(GDMRecordType.rtCommunication);
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
    }
}


internal class CommunicationAddCommand : BaseCommand
{
    public CommunicationAddCommand() : base("communication_add", null, CommandCategory.Communication) { }

    public override void Execute(BaseContext baseContext, object obj)
    {
        // Not implemented yet
    }

    public override MCPTool CreateTool()
    {
        var types = RuntimeData.CommTypeMap.Keys.ToList();
        var dirs = RuntimeData.CommDirMap.Keys.ToList();

        return new MCPTool {
            Name = Sign,
            Description = "Add a new communication record to the database",
            InputSchema = new MCPToolInputSchema {
                Properties = new Dictionary<string, MCPToolProperty> {
                    ["name"] = new MCPToolProperty { Type = "string", Description = "Name of the communication item" },
                    ["type"] = new MCPToolProperty { Type = "string", Description = "Type of the communication.", Enum = types },
                    ["direction"] = new MCPToolProperty { Type = "string", Description = "Direction of the communication.", Enum = dirs },
                    ["corresponderXRef"] = new MCPToolProperty { Type = "string", Description = "XRef identifier of the corresponder (e.g., 'I1')" },
                    ["date"] = new MCPToolProperty { Type = "string", Description = "Communication date" },
                },
                Required = new List<string> { "name", "type", "direction", "corresponderXRef", "date" }
            }
        };
    }

    public override List<MCPContent> ExecuteTool(BaseContext baseContext, JsonElement args)
    {
        string name = MCPHelper.GetRequiredArgument(args, "name");
        string typeStr = MCPHelper.GetRequiredArgument(args, "type");
        string dirStr = MCPHelper.GetRequiredArgument(args, "direction");

        if (!RuntimeData.CommTypeMap.TryGetValue(typeStr, out var commType))
            return MCPContent.CreateSimpleContent($"Invalid type: '{typeStr}'.");

        if (!RuntimeData.CommDirMap.TryGetValue(dirStr, out var dir))
            return MCPContent.CreateSimpleContent($"Invalid direction: '{dirStr}'.");

        string date = MCPHelper.GetRequiredArgument(args, "date");

        string corrXRef = MCPHelper.GetRequiredArgument(args, "corresponderXRef");
        var corrRec = baseContext.Tree.FindXRef<GDMIndividualRecord>(corrXRef);
        if (corrRec == null)
            return MCPContent.CreateSimpleContent($"Corresponder not found with XRef: {corrXRef}");

        var commRec = baseContext.Tree.CreateCommunication();
        commRec.CommName = name;
        commRec.CommunicationType = commType;
        commRec.Date.ParseString(date);
        commRec.SetCorresponder(dir, corrRec);

        baseContext.SetModified();

        return MCPContent.CreateSimpleContent($"Communication record added: {commRec.XRef} - \"{name}\"");
    }
}


internal class CommunicationEditCommand : BaseCommand
{
    public CommunicationEditCommand() : base("communication_edit", null, CommandCategory.Communication) { }

    public override void Execute(BaseContext baseContext, object obj)
    {
        // Not implemented yet
    }

    public override MCPTool CreateTool()
    {
        var types = RuntimeData.CommTypeMap.Keys.ToList();
        var dirs = RuntimeData.CommDirMap.Keys.ToList();

        return new MCPTool {
            Name = Sign,
            Description = "Edit an existing communication record to the database. Only provided fields will be updated. Use 'xref' to identify the record to modify.",
            InputSchema = new MCPToolInputSchema {
                Properties = new Dictionary<string, MCPToolProperty> {
                    ["xref"] = new MCPToolProperty { Type = "string", Description = "Unique identifier (XRef) of the record to edit" },
                    ["name"] = new MCPToolProperty { Type = "string", Description = "New name of the communication item" },
                    ["type"] = new MCPToolProperty { Type = "string", Description = "New type of the communication.", Enum = types },
                    ["direction"] = new MCPToolProperty { Type = "string", Description = "New direction of the communication.", Enum = dirs },
                    ["corresponderXRef"] = new MCPToolProperty { Type = "string", Description = "XRef identifier of the new corresponder (e.g., 'I1')" },
                    ["date"] = new MCPToolProperty { Type = "string", Description = "New communication date" },
                },
                Required = new List<string> { "xref" }
            }
        };
    }

    public override List<MCPContent> ExecuteTool(BaseContext baseContext, JsonElement args)
    {
        string xref = MCPHelper.GetRequiredArgument(args, "xref");
        var commRec = baseContext.Tree.FindXRef<GDMCommunicationRecord>(xref);
        if (commRec == null)
            return MCPContent.CreateSimpleContent($"Communication record not found: '{xref}'.");

        string name = MCPHelper.GetStringArgument(args, "name", null);
        if (name != null) {
            commRec.CommName = name;
        }

        string typeStr = MCPHelper.GetStringArgument(args, "type", null);
        if (typeStr != null) {
            if (!RuntimeData.CommTypeMap.TryGetValue(typeStr, out var commType))
                return MCPContent.CreateSimpleContent($"Invalid type: '{typeStr}'.");

            commRec.CommunicationType = commType;
        }

        string dirStr = MCPHelper.GetStringArgument(args, "direction", null);
        if (dirStr != null) {
            if (!RuntimeData.CommDirMap.TryGetValue(dirStr, out var dir))
                return MCPContent.CreateSimpleContent($"Invalid direction: '{dirStr}'.");

            var corrRec = baseContext.Tree.GetPtrValue(commRec.Corresponder);
            commRec.SetCorresponder(dir, corrRec);
        }

        string date = MCPHelper.GetStringArgument(args, "date", null);
        if (date != null) {
            commRec.Date.ParseString(date);
        }

        string corrXRef = MCPHelper.GetStringArgument(args, "corresponderXRef", null);
        if (corrXRef != null) {
            var corrRec = baseContext.Tree.FindXRef<GDMIndividualRecord>(corrXRef);
            if (corrRec == null)
                return MCPContent.CreateSimpleContent($"Corresponder not found with XRef: {corrXRef}");

            commRec.SetCorresponder(commRec.CommDirection, corrRec);
        }

        baseContext.SetModified();

        return MCPContent.CreateSimpleContent($"Communication record updated: {commRec.XRef} - \"{name}\"");
    }
}


/// <summary>
/// For console use only (for MCP - see <see cref="RecordDeleteCommand"/>).
/// </summary>
internal class CommunicationDeleteCommand : BaseCommand
{
    public CommunicationDeleteCommand() : base("communication_delete", null, CommandCategory.Communication) { }

    public override void Execute(BaseContext baseContext, object obj)
    {
        // Not implemented yet
    }
}
