/*
 *  GEDKeeper, the personal genealogical database editor.
 *  Copyright (C) 2009-2026 by Sergey V. Zhdanovskih.
 *
 *  Licensed under the GNU General Public License (GPL) v3.
 *  See LICENSE file in the project root for full license information.
 */

using System;
using System.Collections.Generic;
using System.ComponentModel.DataAnnotations;
using System.Text.Json;
using GDModel;
using GKcli.MCP;
using GKCore;
using GKCore.Controllers;
using GKCore.Events;
using GKCore.Locales;
using GKCore.Utilities;
using GKUI.Platform;
using Sharprompt;

namespace GKcli.Commands;

internal class IndiMenuCommand : BaseCommand
{
    public IndiMenuCommand() : base("individuals", LSID.RPIndividuals, CommandCategory.Application) { }

    public override void Execute(BaseContext baseContext, object obj)
    {
        CommandController.SelectCommand(CommandCategory.Individual, true, "Select a individual operation");
    }
}


internal class IndiListCommand : BaseCommand
{
    public IndiListCommand() : base("individual_list", LSID.Find, CommandCategory.Individual) { }

    public override void Execute(BaseContext baseContext, object obj)
    {
        var selected = PromptHelper.SelectRecord(baseContext, GDMRecordType.rtIndividual, "Select a individual", "Individual: {0}", "No records.");
        if (selected != null) {
            var newEvent = new GDMIndividualEvent();
            CommandController.SetVariable("selectedObj", newEvent);

            CommandController.SelectCommand(CommandCategory.Events, true, "Select an event operation");
        }
    }

    public override MCPTool CreateTool()
    {
        return new MCPTool {
            Name = Sign,
            Description = "List all individuals in the database with pagination support (20 items per page)",
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
        var recList = baseContext.Tree.GetRecords(GDMRecordType.rtIndividual);
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
    }
}


internal class IndiAddCommand : BaseCommand
{
    public IndiAddCommand() : base("individual_add", LSID.MIRecordAdd, CommandCategory.Individual) { }

    public override void Execute(BaseContext baseContext, object obj)
    {
        var name = Prompt.Input<string>("Enter the individual's name [first_name /last_name/]");
        var sex = Prompt.Input<char>("Enter the individual's sex [m/f]", validators: [Validators.Required(), SexVal()]);
        var indiRec = baseContext.Tree.CreateIndividual();
        var persName = indiRec.AddPersonalName(new GDMPersonalName());
        persName.ParseString(name);
        indiRec.Sex = (sex == 'm') ? GDMSex.svMale : GDMSex.svFemale;
        PromptHelper.WriteLine("Individual: {0}", GKUtils.GetNameString(indiRec, false));
    }

    private static Func<object, ValidationResult> SexVal(string errorMessage = null)
    {
        return delegate (object input) {
            if (!(input is char sym)) {
                return ValidationResult.Success;
            }

            sym = char.ToLowerInvariant(sym);
            return (sym == 'm' || sym == 'f') ? ValidationResult.Success : new ValidationResult(errorMessage ?? "");
        };
    }

    public override MCPTool CreateTool()
    {
        return new MCPTool {
            Name = Sign,
            Description = "Add a new individual to the database",
            InputSchema = new MCPToolInputSchema {
                Properties = new Dictionary<string, MCPToolProperty> {
                    ["name"] = new MCPToolProperty { Type = "string", Description = "Full name in one of these formats: 'Имя Отчество /Фамилия/' (Russian) or 'FirstName MiddleName /LastName/' (English) or 'FirstName /LastName/' or 'FirstName //' (if last name unknown). The last name MUST be enclosed in forward slashes like /Surname/." },
                    ["sex"] = new MCPToolProperty { Type = "string", Description = "Sex: 'm' or 'f'" },
                    ["nickname"] = new MCPToolProperty { Type = "string", Description = "Nickname or alternative name (optional)" }
                },
                Required = new List<string> { "name", "sex" }
            }
        };
    }

    public override List<MCPContent> ExecuteTool(BaseContext baseContext, JsonElement args)
    {
        string name = MCPHelper.GetRequiredArgument(args, "name");

        string sexStr = MCPHelper.GetRequiredArgument(args, "sex").ToLowerInvariant();
        char sex = (sexStr.Length > 0) ? sexStr[0] : 'm';
        if (sex != 'm' && sex != 'f') sex = 'm';
        string nickname = MCPHelper.GetStringArgument(args, "nickname", string.Empty);

        var indiRec = baseContext.Tree.CreateIndividual();
        indiRec.Sex = (sex == 'm') ? GDMSex.svMale : GDMSex.svFemale;

        var persName = indiRec.AddPersonalName(new GDMPersonalName());
        persName.ParseString(name);
        if (!string.IsNullOrEmpty(nickname)) {
            persName.Nickname = nickname;
        }

        baseContext.SetModified();

        string resultName = GKUtils.GetNameString(indiRec, false);
        return MCPContent.CreateSimpleContent($"Individual added: {resultName} with XRef `{indiRec.XRef}`");
    }
}


internal class IndiSearchCommand : BaseCommand
{
    public IndiSearchCommand() : base("individual_search", null, CommandCategory.Individual) { }

    public override void Execute(BaseContext baseContext, object obj)
    {
        // Not implemented yet
    }

    public override MCPTool CreateTool()
    {
        return new MCPTool {
            Name = Sign,
            Description = "Search for individuals by name using fuzzy matching (up to 16% difference)",
            InputSchema = new MCPToolInputSchema {
                Properties = new Dictionary<string, MCPToolProperty> {
                    ["name"] = new MCPToolProperty { Type = "string", Description = "Name to search for (e.g., 'John /Doe/')" }
                },
                Required = new List<string> { "name" }
            }
        };
    }

    public override List<MCPContent> ExecuteTool(BaseContext baseContext, JsonElement args)
    {
        string searchName = MCPHelper.GetRequiredArgument(args, "name");

        var recList = baseContext.Tree.GetRecords(GDMRecordType.rtIndividual);
        if (recList.Count == 0)
            return MCPContent.CreateSimpleContent("No individuals in database.");

        var matches = new List<string>();
        foreach (var rec in recList) {
            string indiName = GKUtils.GetRecordName(baseContext.Tree, rec, false);
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
}


internal class IndiDeleteCommand : BaseCommand
{
    public IndiDeleteCommand() : base("individual_delete", null, CommandCategory.Individual) { }

    public override void Execute(BaseContext baseContext, object obj)
    {
        // Not implemented yet
    }

    public override MCPTool CreateTool()
    {
        return new MCPTool {
            Name = Sign,
            Description = "Delete an individual from the database by their XRef identifier",
            InputSchema = new MCPToolInputSchema {
                Properties = new Dictionary<string, MCPToolProperty> {
                    ["xref"] = new MCPToolProperty { Type = "string", Description = "XRef identifier of the individual (e.g., 'I1')" }
                },
                Required = new List<string> { "xref" }
            }
        };
    }

    public override List<MCPContent> ExecuteTool(BaseContext baseContext, JsonElement args)
    {
        string xref = MCPHelper.GetRequiredArgument(args, "xref");

        var indiRec = baseContext.Tree.FindXRef<GDMIndividualRecord>(xref);
        if (indiRec == null)
            return MCPContent.CreateSimpleContent($"Individual not found with XRef: {xref}");

        baseContext.DeleteRecord(indiRec);

        return MCPContent.CreateSimpleContent($"Individual deleted: {xref}");
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


internal class IndiListEventsCommand : EventCommand
{
    public IndiListEventsCommand() : base("individual_list_events", null, CommandCategory.Individual) { }

    public override void Execute(BaseContext baseContext, object obj)
    {
        // Not implemented yet
    }

    public override MCPTool CreateTool()
    {
        return new MCPTool {
            Name = Sign,
            Description = "List all events of an individual by their XRef identifier",
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

        return GetEventsList(baseContext, "individual", indiRec);
    }
}


internal class IndiListEventTypesCommand : BaseCommand
{
    public IndiListEventTypesCommand() : base("individual_list_event_types", null, CommandCategory.Individual) { }

    public override void Execute(BaseContext baseContext, object obj)
    {
        // Not implemented yet
    }

    public override MCPTool CreateTool()
    {
        return new MCPTool {
            Name = Sign,
            Description = "List all available event types for individuals",
            InputSchema = new MCPToolInputSchema {
                Properties = new Dictionary<string, MCPToolProperty> { },
                Required = new List<string> { }
            }
        };
    }

    public override List<MCPContent> ExecuteTool(BaseContext baseContext, JsonElement args)
    {
        var eventTypes = BaseController.GetEventTypes(EventTarget.etIndividual);

        var rows = new List<string>(eventTypes.Count + 2) {
            "|DisplayName|Tag|Type|HasValue|"
        };
        for (int i = 0; i < eventTypes.Count; i++) {
            var evt = eventTypes[i];
            rows.Add($"|{evt.DisplayName}|{evt.Tag}|{evt.Type}|{evt.HasValue()}|");
        }

        return MCPContent.CreateSimpleContent(string.Join("\n", rows));
    }
}


internal class IndiAddEventCommand : EventCommand
{
    public IndiAddEventCommand() : base("individual_add_event", null, CommandCategory.Individual) { }

    public override void Execute(BaseContext baseContext, object obj)
    {
        // Not implemented yet
    }

    public override MCPTool CreateTool()
    {
        return new MCPTool {
            Name = Sign,
            Description = "Add an event to an individual by XRef identifier",
            InputSchema = new MCPToolInputSchema {
                Properties = new Dictionary<string, MCPToolProperty> {
                    ["individual_xref"] = new MCPToolProperty { Type = "string", Description = "XRef identifier of the individual (e.g., 'I1')" },
                    ["tag"] = new MCPToolProperty { Type = "string", Description = "GEDCOM tag of the event type (from individual_list_event_types)" },
                    ["type"] = new MCPToolProperty { Type = "string", Description = "Event type classification (optional, from individual_list_event_types)" },
                    ["date"] = new MCPToolProperty { Type = "string", Description = "Date string. Follow the GEDCOM date specification from the gedcom_date_spec tool or resource 'gedcom://date_spec'." },
                    ["place"] = new MCPToolProperty { Type = "string", Description = "Place as a free-form string" },
                    ["location_xref"] = new MCPToolProperty { Type = "string", Description = "XRef identifier of a location record (alternative to place string)" },
                    ["cause"] = new MCPToolProperty { Type = "string", Description = "Cause of the event" },
                    ["agency"] = new MCPToolProperty { Type = "string", Description = "Agency responsible for the event" },
                    ["value"] = new MCPToolProperty { Type = "string", Description = "Fact value (used when the event is a fact/attribute)" }
                },
                Required = new List<string> { "individual_xref", "tag" }
            }
        };
    }

    public override List<MCPContent> ExecuteTool(BaseContext baseContext, JsonElement args)
    {
        string individualXRef = MCPHelper.GetRequiredArgument(args, "individual_xref");

        var indiRec = baseContext.Tree.FindXRef<GDMIndividualRecord>(individualXRef);
        if (indiRec == null)
            return MCPContent.CreateSimpleContent($"Individual not found with XRef: {individualXRef}");

        return AddEvent(baseContext, indiRec, "individual", args);
    }
}


internal class IndiDeleteEventCommand : BaseCommand
{
    public IndiDeleteEventCommand() : base("individual_delete_event", null, CommandCategory.Individual) { }

    public override void Execute(BaseContext baseContext, object obj)
    {
        // Not implemented yet
    }

    public override MCPTool CreateTool()
    {
        return new MCPTool {
            Name = Sign,
            Description = "Remove an event from an individual by individual XRef and event index",
            InputSchema = new MCPToolInputSchema {
                Properties = new Dictionary<string, MCPToolProperty> {
                    ["individual_xref"] = new MCPToolProperty { Type = "string", Description = "XRef identifier of the individual (e.g., 'I1')" },
                    ["event_index"] = new MCPToolProperty { Type = "integer", Description = "Zero-based index of the event in the individual's event list" }
                },
                Required = new List<string> { "individual_xref", "event_index" }
            }
        };
    }

    public override List<MCPContent> ExecuteTool(BaseContext baseContext, JsonElement args)
    {
        string individualXRef = MCPHelper.GetRequiredArgument(args, "individual_xref");
        int eventIndex = MCPHelper.GetIntArgument(args, "event_index", -1);

        var indiRec = baseContext.Tree.FindXRef<GDMIndividualRecord>(individualXRef);
        if (indiRec == null)
            return MCPContent.CreateSimpleContent($"Individual not found with XRef: {individualXRef}");

        if (!indiRec.HasEvents)
            return MCPContent.CreateSimpleContent($"Individual '{individualXRef}' has no events.");

        if (eventIndex < 0 || eventIndex >= indiRec.Events.Count)
            return MCPContent.CreateSimpleContent($"Invalid event index {eventIndex} for individual '{individualXRef}' (has {indiRec.Events.Count} events).");

        var evt = indiRec.Events[eventIndex];
        string evtInfo = GKUtils.GetEventName(evt);

        indiRec.Events.RemoveAt(eventIndex);
        baseContext.SetModified();

        return MCPContent.CreateSimpleContent($"Event removed from individual '{individualXRef}' at index {eventIndex}: {evtInfo}");
    }
}
