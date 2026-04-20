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

namespace GKcli.Commands;

internal class IndiListAssociationsCommand : BaseCommand
{
    public IndiListAssociationsCommand() : base("individual_list_associations", null, CommandCategory.Individual) { }

    public override void Execute(BaseContext baseContext, object obj)
    {
        // Not implemented yet
    }

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
        string individualXRef = MCPHelper.GetRequiredArgument(args, "individual_xref");

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


internal class IndiAddAssociationCommand : BaseCommand
{
    public IndiAddAssociationCommand() : base("individual_add_association", null, CommandCategory.Individual) { }

    public override void Execute(BaseContext baseContext, object obj)
    {
        // Not implemented yet
    }

    public override MCPTool CreateTool()
    {
        return new MCPTool {
            Name = Sign,
            Description = "Add an association (relationship) between two individuals",
            InputSchema = new MCPToolInputSchema {
                Properties = new Dictionary<string, MCPToolProperty> {
                    ["individual_xref"] = new MCPToolProperty { Type = "string", Description = "XRef identifier of the primary individual (e.g., 'I1')" },
                    ["associate_xref"] = new MCPToolProperty { Type = "string", Description = "XRef identifier of the associated individual (e.g., 'I2')" },
                    ["relation"] = new MCPToolProperty { Type = "string", Description = "Description of the relationship (e.g., 'Friend', 'Witness', 'Godparent')" }
                },
                Required = new List<string> { "individual_xref", "associate_xref", "relation" }
            }
        };
    }

    public override List<MCPContent> ExecuteTool(BaseContext baseContext, JsonElement args)
    {
        string individualXRef = MCPHelper.GetRequiredArgument(args, "individual_xref");
        string associateXRef = MCPHelper.GetRequiredArgument(args, "associate_xref");
        string relation = MCPHelper.GetRequiredArgument(args, "relation");

        var indiRec = baseContext.Tree.FindXRef<GDMIndividualRecord>(individualXRef);
        if (indiRec == null)
            return MCPContent.CreateSimpleContent($"Individual not found with XRef: {individualXRef}");

        if (!indiRec.GetAccessibleSubstructures().HasFlag(GDMStructureType.Association))
            return MCPContent.CreateSimpleContent($"Record type '{individualXRef}' ({indiRec.RecordType}) does not support associations.");

        var assocRec = baseContext.Tree.FindXRef<GDMIndividualRecord>(associateXRef);
        if (assocRec == null)
            return MCPContent.CreateSimpleContent($"Associated individual not found with XRef: {associateXRef}");

        var association = new GDMAssociation();
        association.XRef = associateXRef;
        association.Relation = relation;
        indiRec.Associations.Add(association);
        baseContext.SetModified();

        int assocIndex = indiRec.Associations.IndexOf(association);
        return MCPContent.CreateSimpleContent($"Association added to individual '{individualXRef}' at index {assocIndex}: associated '{associateXRef}', relation '{relation}'");
    }
}


internal class IndiEditAssociationCommand : BaseCommand
{
    public IndiEditAssociationCommand() : base("individual_edit_association", null, CommandCategory.Individual) { }

    public override void Execute(BaseContext baseContext, object obj)
    {
        // Not implemented yet
    }

    public override MCPTool CreateTool()
    {
        return new MCPTool {
            Name = Sign,
            Description = "Edit an association of an individual by individual XRef and association index",
            InputSchema = new MCPToolInputSchema {
                Properties = new Dictionary<string, MCPToolProperty> {
                    ["individual_xref"] = new MCPToolProperty { Type = "string", Description = "XRef identifier of the individual (e.g., 'I1')" },
                    ["association_index"] = new MCPToolProperty { Type = "integer", Description = "Zero-based index of the association in the individual's association list" },
                    ["associate_xref"] = new MCPToolProperty { Type = "string", Description = "XRef identifier of the associated individual (e.g., 'I2')" },
                    ["relation"] = new MCPToolProperty { Type = "string", Description = "Description of the relationship (e.g., 'Friend', 'Witness', 'Godparent')" }
                },
                Required = new List<string> { "individual_xref", "association_index" }
            }
        };
    }

    public override List<MCPContent> ExecuteTool(BaseContext baseContext, JsonElement args)
    {
        string individualXRef = MCPHelper.GetRequiredArgument(args, "individual_xref");
        int associationIndex = MCPHelper.GetIntArgument(args, "association_index", -1);

        var indiRec = baseContext.Tree.FindXRef<GDMIndividualRecord>(individualXRef);
        if (indiRec == null)
            return MCPContent.CreateSimpleContent($"Individual not found with XRef: {individualXRef}");

        if (!indiRec.HasAssociations)
            return MCPContent.CreateSimpleContent($"Individual '{individualXRef}' has no associations.");

        if (associationIndex < 0 || associationIndex >= indiRec.Associations.Count)
            return MCPContent.CreateSimpleContent($"Invalid association index {associationIndex} for individual '{individualXRef}' (has {indiRec.Associations.Count} associations).");

        string associateXRef = MCPHelper.GetStringArgument(args, "associate_xref", null);
        var assocRec = baseContext.Tree.FindXRef<GDMIndividualRecord>(associateXRef);
        if (assocRec == null)
            return MCPContent.CreateSimpleContent($"Associated individual not found with XRef: {associateXRef}");

        string relation = MCPHelper.GetStringArgument(args, "relation", null);

        var association = indiRec.Associations[associationIndex];

        if (associateXRef != null) {
            association.XRef = associateXRef;
        }

        if (relation != null) {
            association.Relation = relation;
        }

        baseContext.SetModified();

        string assocInfo = $"associated '{association.XRef}', relation '{association.Relation}'";
        return MCPContent.CreateSimpleContent($"Association updated from individual '{individualXRef}' at index {associationIndex}: {assocInfo}");
    }
}


internal class IndiDeleteAssociationCommand : BaseCommand
{
    public IndiDeleteAssociationCommand() : base("individual_delete_association", null, CommandCategory.Individual) { }

    public override void Execute(BaseContext baseContext, object obj)
    {
        // Not implemented yet
    }

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
        string individualXRef = MCPHelper.GetRequiredArgument(args, "individual_xref");
        int associationIndex = MCPHelper.GetIntArgument(args, "association_index", -1);

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
