/*
 *  GEDKeeper, the personal genealogical database editor.
 *  Copyright (C) 2009-2026 by Sergey V. Zhdanovskih.
 *
 *  Licensed under the GNU General Public License (GPL) v3.
 *  See LICENSE file in the project root for full license information.
 */

using System.Collections.Generic;
using System.Text.Json;
using GDModel;
using GKcli.MCP;
using GKCore;

namespace GKcli.Features;

internal class IndiListAssociationsTool : BaseTool
{
    public IndiListAssociationsTool() : base("individual_list_associations") { }

    public override MCPTool CreateTool()
    {
        return new MCPTool {
            Name = Sign,
            Description = "List all associations of an individual by their XRef identifier",
            InputSchema = new MCPToolInputSchema {
                Properties = new Dictionary<string, MCPToolProperty> {
                    ["individual_xref"] = new MCPToolProperty { Type = "string", Description = "XRef identifier of the individual (e.g., 'I1')" }
                },
                Required = new List<string> { "individual_xref" }
            }
        };
    }

    public override List<MCPContent> ExecuteTool(BaseContext baseContext, JsonElement args)
    {
        string individualXRef = MCPHelper.GetRequiredStr(args, "individual_xref");

        var indiRec = baseContext.Tree.FindXRef<GDMIndividualRecord>(individualXRef);
        if (indiRec == null)
            return MCPContent.CreateSimpleContent($"Individual not found with XRef: {individualXRef}");

        if (!indiRec.HasAssociations)
            return MCPContent.CreateSimpleContent($"Individual '{individualXRef}' has no associations.");

        var rows = new List<string> {
            $"Associations for individual '{individualXRef}' ({indiRec.Associations.Count}):",
            "| Index | Associate XRef | Relation |",
            "|---|---|---|"
        };
        for (int i = 0; i < indiRec.Associations.Count; i++) {
            var assoc = indiRec.Associations[i];
            rows.Add($"|{i}|{assoc.XRef}|{assoc.Relation}|");
        }

        return MCPContent.CreateSimpleContent(string.Join("\n", rows));
    }
}


internal class IndiUpsertAssociationTool : BaseTool
{
    public IndiUpsertAssociationTool() : base("individual_upsert_association") { }

    public override MCPTool CreateTool()
    {
        return new MCPTool {
            Name = Sign,
            Description = "Add new association or update existing. Provide 'association_index' to edit; omit 'association_index' to create. 'individual_xref' and 'associate_xref' required for new associations.",
            InputSchema = new MCPToolInputSchema {
                Properties = new Dictionary<string, MCPToolProperty> {
                    ["individual_xref"] = new MCPToolProperty { Type = "string", Description = "XRef identifier of the primary individual (e.g., 'I1')" },
                    ["association_index"] = new MCPToolProperty { Type = "integer", Description = "Zero-based index of the association in the individual's association list (omit for new)" },
                    ["associate_xref"] = new MCPToolProperty { Type = "string", Description = "XRef identifier of the associated individual (e.g., 'I2')" },
                    ["relation"] = new MCPToolProperty { Type = "string", Description = "Description of the relationship (e.g., 'Friend', 'Witness', 'Godparent')" }
                },
                Required = new List<string> { "individual_xref", "associate_xref" }
            }
        };
    }

    public override List<MCPContent> ExecuteTool(BaseContext baseContext, JsonElement args)
    {
        string individualXRef = MCPHelper.GetRequiredStr(args, "individual_xref");
        int? associationIndex = MCPHelper.GetOptionalNullableInt(args, "association_index", null);
        string associateXRef = MCPHelper.GetRequiredStr(args, "associate_xref");
        string relation = MCPHelper.GetOptionalStr(args, "relation", null);

        var indiRec = baseContext.Tree.FindXRef<GDMIndividualRecord>(individualXRef);
        if (indiRec == null)
            return MCPContent.CreateSimpleContent($"❌ Individual not found with XRef: {individualXRef}");

        var assocRec = baseContext.Tree.FindXRef<GDMIndividualRecord>(associateXRef);
        if (assocRec == null)
            return MCPContent.CreateSimpleContent($"❌ Associated individual not found with XRef: {associateXRef}");

        bool isEdit = associationIndex.HasValue;
        if (isEdit) {
            int index = associationIndex.Value;

            if (!indiRec.HasAssociations)
                return MCPContent.CreateSimpleContent($"❌ Individual '{individualXRef}' has no associations.");

            if (index < 0 || index >= indiRec.Associations.Count)
                return MCPContent.CreateSimpleContent($"❌ Invalid association index {index} for individual '{individualXRef}' (has {indiRec.Associations.Count} associations).");

            var association = indiRec.Associations[index];

            if (associateXRef != null) {
                association.XRef = associateXRef;
            }

            if (relation != null) {
                association.Relation = relation;
            }

            baseContext.SetModified();
            string assocInfo = $"associated '{association.XRef}', relation '{association.Relation}'";
            return MCPContent.CreateSimpleContent($"✅ Association updated for individual '{individualXRef}' at index {index}: {assocInfo}");
        } else {
            var association = new GDMAssociation();
            association.XRef = associateXRef;
            if (relation != null) {
                association.Relation = relation;
            }
            indiRec.Associations.Add(association);

            baseContext.SetModified();
            int assocIndex = indiRec.Associations.IndexOf(association);
            return MCPContent.CreateSimpleContent($"✅ Association added to individual '{individualXRef}' at index {assocIndex}: associated '{associateXRef}', relation '{association.Relation ?? "not specified"}'");
        }
    }
}


internal class IndiDeleteAssociationTool : BaseTool
{
    public IndiDeleteAssociationTool() : base("individual_delete_association") { }

    public override MCPTool CreateTool()
    {
        return new MCPTool {
            Name = Sign,
            Description = "Remove an association from an individual by individual XRef and association index",
            InputSchema = new MCPToolInputSchema {
                Properties = new Dictionary<string, MCPToolProperty> {
                    ["individual_xref"] = new MCPToolProperty { Type = "string", Description = "XRef identifier of the individual (e.g., 'I1')" },
                    ["association_index"] = new MCPToolProperty { Type = "integer", Description = "Zero-based index of the association in the individual's association list" }
                },
                Required = new List<string> { "individual_xref", "association_index" }
            }
        };
    }

    public override List<MCPContent> ExecuteTool(BaseContext baseContext, JsonElement args)
    {
        string individualXRef = MCPHelper.GetRequiredStr(args, "individual_xref");
        int associationIndex = MCPHelper.GetOptionalInt(args, "association_index", -1);

        var indiRec = baseContext.Tree.FindXRef<GDMIndividualRecord>(individualXRef);
        if (indiRec == null)
            return MCPContent.CreateSimpleContent($"Individual not found with XRef: {individualXRef}");

        if (!indiRec.HasAssociations)
            return MCPContent.CreateSimpleContent($"Individual '{individualXRef}' has no associations.");

        if (associationIndex < 0 || associationIndex >= indiRec.Associations.Count)
            return MCPContent.CreateSimpleContent($"Invalid association index {associationIndex} for individual '{individualXRef}' (has {indiRec.Associations.Count} associations).");

        var association = indiRec.Associations[associationIndex];
        string assocInfo = $"associated '{association.XRef}', relation '{association.Relation}'";

        indiRec.Associations.RemoveAt(associationIndex);
        baseContext.SetModified();

        return MCPContent.CreateSimpleContent($"Association removed from individual '{individualXRef}' at index {associationIndex}: {assocInfo}");
    }
}
