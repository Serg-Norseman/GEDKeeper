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
using GKcli.Platform;
using GKCore;

namespace GKcli.Features;

internal class CommunicationUpsertTool : BaseTool
{
    public CommunicationUpsertTool() : base("communication_upsert") { }

    public override MCPTool CreateTool()
    {
        var types = RuntimeData.CommTypeMap.Keys.ToList();
        var dirs = RuntimeData.CommDirMap.Keys.ToList();

        return new MCPTool {
            Name = Sign,
            Description = "Add new communication or update existing. Provide 'xref' to edit; omit 'xref' to create. 'name', 'type', 'direction', 'corresponderXRef', and 'date' required for new communications.",
            InputSchema = new MCPToolInputSchema {
                Properties = new Dictionary<string, MCPToolProperty> {
                    ["xref"] = new MCPToolProperty { Type = "string", Description = "XRef identifier of existing communication to update (omit for new)" },
                    ["name"] = new MCPToolProperty { Type = "string", Description = "Name of the communication item" },
                    ["type"] = new MCPToolProperty { Type = "string", Description = "Type of the communication.", Enum = types },
                    ["direction"] = new MCPToolProperty { Type = "string", Description = "Direction of the communication.", Enum = dirs },
                    ["corresponderXRef"] = new MCPToolProperty { Type = "string", Description = "XRef identifier of the corresponder (e.g., 'I1')" },
                    ["date"] = new MCPToolProperty { Type = "string", Description = "Communication date, strictly with the GEDCOM Date Spec" },
                },
                Required = new List<string> { }
            }
        };
    }

    public override List<MCPContent> ExecuteTool(BaseContext baseContext, JsonElement args)
    {
        string xref = MCPHelper.GetOptionalStr(args, "xref", null);
        string name = MCPHelper.GetOptionalStr(args, "name", null);
        string typeStr = MCPHelper.GetOptionalStr(args, "type", null);
        string dirStr = MCPHelper.GetOptionalStr(args, "direction", null);
        string corrXRef = MCPHelper.GetOptionalStr(args, "corresponderXRef", null);
        string date = MCPHelper.GetOptionalStr(args, "date", null);

        bool isEdit = !string.IsNullOrEmpty(xref);
        if (isEdit) {
            var commRec = baseContext.Tree.FindXRef<GDMCommunicationRecord>(xref);
            if (commRec == null)
                return MCPContent.CreateSimpleContent($"❌ Communication record not found: '{xref}'");

            if (name != null) commRec.CommName = name;

            if (typeStr != null) {
                if (!RuntimeData.CommTypeMap.TryGetValue(typeStr, out var commType))
                    return MCPContent.CreateSimpleContent($"❌ Invalid type: '{typeStr}'.");
                commRec.CommunicationType = commType;
            }

            if (dirStr != null) {
                if (!RuntimeData.CommDirMap.TryGetValue(dirStr, out var dir))
                    return MCPContent.CreateSimpleContent($"❌ Invalid direction: '{dirStr}'.");
                var corrRec = baseContext.Tree.GetPtrValue(commRec.Corresponder);
                commRec.SetCorresponder(dir, corrRec);
            }

            if (date != null) commRec.Date.ParseString(date);

            if (corrXRef != null) {
                var corrRec = baseContext.Tree.FindXRef<GDMIndividualRecord>(corrXRef);
                if (corrRec == null)
                    return MCPContent.CreateSimpleContent($"❌ Corresponder not found with XRef: {corrXRef}");
                commRec.SetCorresponder(commRec.CommDirection, corrRec);
            }

            baseContext.SetModified();
            return MCPContent.CreateSimpleContent($"✅ Communication record updated: {commRec.XRef} - \"{name}\"");
        } else {
            if (string.IsNullOrEmpty(name))
                return MCPContent.CreateSimpleContent("❌ 'name' required for new communication");

            if (string.IsNullOrEmpty(typeStr))
                return MCPContent.CreateSimpleContent("❌ 'type' required for new communication");

            if (string.IsNullOrEmpty(dirStr))
                return MCPContent.CreateSimpleContent("❌ 'direction' required for new communication");

            if (string.IsNullOrEmpty(corrXRef))
                return MCPContent.CreateSimpleContent("❌ 'corresponderXRef' required for new communication");

            if (string.IsNullOrEmpty(date))
                return MCPContent.CreateSimpleContent("❌ 'date' required for new communication");

            if (!RuntimeData.CommTypeMap.TryGetValue(typeStr, out var commType))
                return MCPContent.CreateSimpleContent($"❌ Invalid type: '{typeStr}'.");

            if (!RuntimeData.CommDirMap.TryGetValue(dirStr, out var dir))
                return MCPContent.CreateSimpleContent($"❌ Invalid direction: '{dirStr}'.");

            var corrRec = baseContext.Tree.FindXRef<GDMIndividualRecord>(corrXRef);
            if (corrRec == null)
                return MCPContent.CreateSimpleContent($"❌ Corresponder not found with XRef: {corrXRef}");

            var commRec = baseContext.Tree.CreateCommunication();
            commRec.CommName = name;
            commRec.CommunicationType = commType;
            commRec.Date.ParseString(date);
            commRec.SetCorresponder(dir, corrRec);

            baseContext.SetModified();
            return MCPContent.CreateSimpleContent($"✅ Communication record added: {commRec.XRef} - \"{name}\"");
        }
    }
}
