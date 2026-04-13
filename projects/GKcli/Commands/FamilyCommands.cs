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
using GKCore.Controllers;
using GKCore.Events;
using GKCore.Locales;

namespace GKcli.Commands;

internal class FamMenuCommand : BaseCommand
{
    public FamMenuCommand() : base("families", LSID.RPFamilies, CommandCategory.Application) { }

    public override void Execute(BaseContext baseContext, object obj)
    {
        CommandController.SelectCommand(CommandCategory.Family, true, "Select a family operation");
    }
}


internal class FamListCommand : RecordCommand
{
    public FamListCommand() : base("family_list", LSID.Find, CommandCategory.Family) { }

    public override void Execute(BaseContext baseContext, object obj)
    {
        SelectRecord(baseContext, GDMRecordType.rtFamily, "Select a family", "Family: {0}", "No records.");
    }

    public override MCPTool CreateTool()
    {
        return new MCPTool {
            Name = Sign,
            Description = "List all families in the database with pagination support (20 items per page)",
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
        var recList = baseContext.Tree.GetRecords(GDMRecordType.rtFamily);
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
    }
}


internal class FamAddCommand : BaseCommand
{
    public FamAddCommand() : base("family_add", null, CommandCategory.Family) { }

    public override void Execute(BaseContext baseContext, object obj)
    {
        // Empty for interactive mode
    }

    public override MCPTool CreateTool()
    {
        return new MCPTool {
            Name = Sign,
            Description = "Add a new family record to the database with husband and wife",
            InputSchema = new MCPToolInputSchema {
                Properties = new Dictionary<string, MCPToolProperty> {
                    ["husband_xref"] = new MCPToolProperty { Type = "string", Description = "XRef identifier of the husband (e.g., 'I1')" },
                    ["wife_xref"] = new MCPToolProperty { Type = "string", Description = "XRef identifier of the wife (e.g., 'I2')" }
                },
                Required = new List<string> { "husband_xref", "wife_xref" }
            }
        };
    }

    public override List<MCPContent> ExecuteTool(BaseContext baseContext, JsonElement args)
    {
        string husbandXRef = MCPHelper.GetRequiredArgument(args, "husband_xref");
        string wifeXRef = MCPHelper.GetRequiredArgument(args, "wife_xref");

        var husbandRec = baseContext.Tree.FindXRef<GDMIndividualRecord>(husbandXRef);
        if (husbandRec == null)
            return MCPContent.CreateSimpleContent($"Husband not found with XRef: {husbandXRef}");

        var wifeRec = baseContext.Tree.FindXRef<GDMIndividualRecord>(wifeXRef);
        if (wifeRec == null)
            return MCPContent.CreateSimpleContent($"Wife not found with XRef: {wifeXRef}");

        var familyRec = baseContext.Tree.CreateFamily();
        familyRec.AddSpouse(husbandRec);
        familyRec.AddSpouse(wifeRec);
        baseContext.SetModified();

        return MCPContent.CreateSimpleContent($"Family with XRef `{familyRec.XRef}` added: husband {husbandXRef}, wife {wifeXRef}");
    }
}


internal class FamDeleteCommand : BaseCommand
{
    public FamDeleteCommand() : base("family_delete", null, CommandCategory.Family) { }

    public override void Execute(BaseContext baseContext, object obj)
    {
        // Empty for interactive mode
    }

    public override MCPTool CreateTool()
    {
        return new MCPTool {
            Name = Sign,
            Description = "Delete a family from the database by their XRef identifier",
            InputSchema = new MCPToolInputSchema {
                Properties = new Dictionary<string, MCPToolProperty> {
                    ["xref"] = new MCPToolProperty { Type = "string", Description = "XRef identifier of the family (e.g., 'F1')" }
                },
                Required = new List<string> { "xref" }
            }
        };
    }

    public override List<MCPContent> ExecuteTool(BaseContext baseContext, JsonElement args)
    {
        string xref = MCPHelper.GetRequiredArgument(args, "xref");

        var familyRec = baseContext.Tree.FindXRef<GDMFamilyRecord>(xref);
        if (familyRec == null)
            return MCPContent.CreateSimpleContent($"Family not found with XRef: {xref}");

        baseContext.DeleteRecord(familyRec);

        return MCPContent.CreateSimpleContent($"Family deleted: {xref}");
    }
}


internal class FamAddChildCommand : BaseCommand
{
    public FamAddChildCommand() : base("family_add_child", null, CommandCategory.Family) { }

    public override void Execute(BaseContext baseContext, object obj)
    {
        // Empty for interactive mode
    }

    public override MCPTool CreateTool()
    {
        return new MCPTool {
            Name = Sign,
            Description = "Add a child to a family by their XRef identifiers",
            InputSchema = new MCPToolInputSchema {
                Properties = new Dictionary<string, MCPToolProperty> {
                    ["family_xref"] = new MCPToolProperty { Type = "string", Description = "XRef identifier of the family (e.g., 'F1')" },
                    ["child_xref"] = new MCPToolProperty { Type = "string", Description = "XRef identifier of the child (e.g., 'I3')" }
                },
                Required = new List<string> { "family_xref", "child_xref" }
            }
        };
    }

    public override List<MCPContent> ExecuteTool(BaseContext baseContext, JsonElement args)
    {
        string familyXRef = MCPHelper.GetRequiredArgument(args, "family_xref");
        string childXRef = MCPHelper.GetRequiredArgument(args, "child_xref");

        var familyRec = baseContext.Tree.FindXRef<GDMFamilyRecord>(familyXRef);
        if (familyRec == null)
            return MCPContent.CreateSimpleContent($"Family not found with XRef: {familyXRef}");

        var childRec = baseContext.Tree.FindXRef<GDMIndividualRecord>(childXRef);
        if (childRec == null)
            return MCPContent.CreateSimpleContent($"Child not found with XRef: {childXRef}");

        if (familyRec.IndexOfChild(childRec) >= 0)
            return MCPContent.CreateSimpleContent($"Child {childXRef} is already a member of family '{familyXRef}'.");

        familyRec.AddChild(childRec);
        baseContext.SetModified();

        string childName = GKUtils.GetNameString(childRec, false);
        return MCPContent.CreateSimpleContent($"Child added to family '{familyXRef}': {childName} ({childXRef})");
    }
}


internal class FamDeleteChildCommand : BaseCommand
{
    public FamDeleteChildCommand() : base("family_delete_child", null, CommandCategory.Family) { }

    public override void Execute(BaseContext baseContext, object obj)
    {
        // Empty for interactive mode
    }

    public override MCPTool CreateTool()
    {
        return new MCPTool {
            Name = Sign,
            Description = "Remove a child from a family by their XRef identifiers",
            InputSchema = new MCPToolInputSchema {
                Properties = new Dictionary<string, MCPToolProperty> {
                    ["family_xref"] = new MCPToolProperty { Type = "string", Description = "XRef identifier of the family (e.g., 'F1')" },
                    ["child_xref"] = new MCPToolProperty { Type = "string", Description = "XRef identifier of the child (e.g., 'I3')" }
                },
                Required = new List<string> { "family_xref", "child_xref" }
            }
        };
    }

    public override List<MCPContent> ExecuteTool(BaseContext baseContext, JsonElement args)
    {
        string familyXRef = MCPHelper.GetRequiredArgument(args, "family_xref");
        string childXRef = MCPHelper.GetRequiredArgument(args, "child_xref");

        var familyRec = baseContext.Tree.FindXRef<GDMFamilyRecord>(familyXRef);
        if (familyRec == null)
            return MCPContent.CreateSimpleContent($"Family not found with XRef: {familyXRef}");

        var childRec = baseContext.Tree.FindXRef<GDMIndividualRecord>(childXRef);
        if (childRec == null)
            return MCPContent.CreateSimpleContent($"Child not found with XRef: {childXRef}");

        if (familyRec.IndexOfChild(childRec) < 0)
            return MCPContent.CreateSimpleContent($"Child {childXRef} is not a member of family '{familyXRef}'.");

        familyRec.RemoveChild(childRec);
        baseContext.SetModified();

        string childName = GKUtils.GetNameString(childRec, false);
        return MCPContent.CreateSimpleContent($"Child removed from family '{familyXRef}': {childName} ({childXRef})");
    }
}


internal class FamListChildrenCommand : BaseCommand
{
    public FamListChildrenCommand() : base("family_list_children", null, CommandCategory.Family) { }

    public override void Execute(BaseContext baseContext, object obj)
    {
        // Empty for interactive mode
    }

    public override MCPTool CreateTool()
    {
        return new MCPTool {
            Name = Sign,
            Description = "List all children of a family by its XRef identifier",
            InputSchema = new MCPToolInputSchema {
                Properties = new Dictionary<string, MCPToolProperty> {
                    ["family_xref"] = new MCPToolProperty { Type = "string", Description = "XRef identifier of the family (e.g., 'F1')" }
                },
                Required = new List<string> { "family_xref" }
            }
        };
    }

    public override List<MCPContent> ExecuteTool(BaseContext baseContext, JsonElement args)
    {
        string familyXRef = MCPHelper.GetRequiredArgument(args, "family_xref");

        var familyRec = baseContext.Tree.FindXRef<GDMFamilyRecord>(familyXRef);
        if (familyRec == null)
            return MCPContent.CreateSimpleContent($"Family not found with XRef: {familyXRef}");

        if (familyRec.Children.Count <= 0)
            return MCPContent.CreateSimpleContent($"Family '{familyXRef}' has no children.");

        var rows = new List<string> {
            $"Children of family '{familyXRef}' ({familyRec.Children.Count}):",
            "| Index | Child XRef | Name | Sex |",
            "|---|---|---|---|"
        };
        for (int i = 0; i < familyRec.Children.Count; i++) {
            var childPtr = familyRec.Children[i];
            var childRec = baseContext.Tree.GetPtrValue<GDMIndividualRecord>(childPtr);

            if (childRec != null) {
                string childName = GKUtils.GetNameString(childRec, false);
                string sex = GKData.SexData[(int)childRec.Sex].Sign;
                rows.Add($"|{i}|{childPtr.XRef}|{childName}|{sex}|");
            } else {
                rows.Add($"|{i}|{childPtr.XRef}|(not found)|-|");
            }
        }

        return MCPContent.CreateSimpleContent(string.Join("\n", rows));
    }
}


internal class FamListEventsCommand : ListEventsCommand
{
    public FamListEventsCommand() : base("family_list_events", null, CommandCategory.Family) { }

    public override void Execute(BaseContext baseContext, object obj)
    {
        // Empty for interactive mode
    }

    public override MCPTool CreateTool()
    {
        return new MCPTool {
            Name = Sign,
            Description = "List all events of a family by its XRef identifier",
            InputSchema = new MCPToolInputSchema {
                Properties = new Dictionary<string, MCPToolProperty> {
                    ["family_xref"] = new MCPToolProperty { Type = "string", Description = "XRef identifier of the family (e.g., 'F1')" }
                },
                Required = new List<string> { "family_xref" }
            }
        };
    }

    public override List<MCPContent> ExecuteTool(BaseContext baseContext, JsonElement args)
    {
        string familyXRef = MCPHelper.GetRequiredArgument(args, "family_xref");

        var familyRec = baseContext.Tree.FindXRef<GDMFamilyRecord>(familyXRef);
        if (familyRec == null)
            return MCPContent.CreateSimpleContent($"Family not found with XRef: {familyXRef}");

        return GetList(baseContext, "family", familyRec);
    }
}


internal class FamListEventTypesCommand : BaseCommand
{
    public FamListEventTypesCommand() : base("family_list_event_types", null, CommandCategory.Family) { }

    public override void Execute(BaseContext baseContext, object obj)
    {
        // Empty for interactive mode
    }

    public override MCPTool CreateTool()
    {
        return new MCPTool {
            Name = Sign,
            Description = "List all available event types for families",
            InputSchema = new MCPToolInputSchema {
                Properties = new Dictionary<string, MCPToolProperty> { },
                Required = new List<string> { }
            }
        };
    }

    public override List<MCPContent> ExecuteTool(BaseContext baseContext, JsonElement args)
    {
        var eventTypes = BaseController.GetEventTypes(EventTarget.etFamily);

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


internal class FamAddEventCommand : BaseCommand
{
    public FamAddEventCommand() : base("family_add_event", null, CommandCategory.Family) { }

    public override void Execute(BaseContext baseContext, object obj)
    {
        // Empty for interactive mode
    }

    public override MCPTool CreateTool()
    {
        return new MCPTool {
            Name = Sign,
            Description = "Add an event to a family by XRef identifier",
            InputSchema = new MCPToolInputSchema {
                Properties = new Dictionary<string, MCPToolProperty> {
                    ["family_xref"] = new MCPToolProperty { Type = "string", Description = "XRef identifier of the family (e.g., 'F1')" },
                    ["tag"] = new MCPToolProperty { Type = "string", Description = "GEDCOM tag of the event type (from family_list_event_types)" },
                    ["type"] = new MCPToolProperty { Type = "string", Description = "Event type classification (optional, from family_list_event_types)" },
                    ["date"] = new MCPToolProperty { Type = "string", Description = "Date string in dd/mm/yyyy format (any part may be empty, e.g., '15/04/1990', '/04/1990', '//1990')" },
                    ["place"] = new MCPToolProperty { Type = "string", Description = "Place as a free-form string" },
                    ["location_xref"] = new MCPToolProperty { Type = "string", Description = "XRef identifier of a location record (alternative to place string)" },
                    ["cause"] = new MCPToolProperty { Type = "string", Description = "Cause of the event" },
                    ["agency"] = new MCPToolProperty { Type = "string", Description = "Agency responsible for the event" },
                    ["value"] = new MCPToolProperty { Type = "string", Description = "Fact value (used when the event is a fact/attribute)" }
                },
                Required = new List<string> { "family_xref", "tag" }
            }
        };
    }

    public override List<MCPContent> ExecuteTool(BaseContext baseContext, JsonElement args)
    {
        string familyXRef = MCPHelper.GetRequiredArgument(args, "family_xref");

        var familyRec = baseContext.Tree.FindXRef<GDMFamilyRecord>(familyXRef);
        if (familyRec == null)
            return MCPContent.CreateSimpleContent($"Family not found with XRef: {familyXRef}");

        string tag = MCPHelper.GetRequiredArgument(args, "tag");
        string argType = MCPHelper.GetStringArgument(args, "type", "");
        EventDef matchedDef = AppHost.EventDefinitions.Find(tag, argType);

        GDMFamilyEvent newEvent = new GDMFamilyEvent();
        newEvent.SetName(tag);
        newEvent.Classification = matchedDef != null ? matchedDef.Type : argType;

        // Date
        string dateStr = MCPHelper.GetStringArgument(args, "date", "");
        if (!string.IsNullOrEmpty(dateStr)) {
            newEvent.Date.ParseString(dateStr);
        }

        // Place (string or location xref)
        string placeStr = MCPHelper.GetStringArgument(args, "place", "");
        string locationXRef = MCPHelper.GetStringArgument(args, "location_xref", "");
        if (!string.IsNullOrEmpty(locationXRef)) {
            var locRec = baseContext.Tree.FindXRef<GDMLocationRecord>(locationXRef);
            if (locRec == null)
                return MCPContent.CreateSimpleContent($"Location not found with XRef: {locationXRef}");
            baseContext.Tree.SetPtrValue(newEvent.Place.Location, locRec);
        } else if (!string.IsNullOrEmpty(placeStr)) {
            newEvent.Place.StringValue = placeStr;
        }

        // Cause
        string cause = MCPHelper.GetStringArgument(args, "cause", "");
        if (!string.IsNullOrEmpty(cause)) {
            newEvent.Cause = cause;
        }

        // Agency
        string agency = MCPHelper.GetStringArgument(args, "agency", "");
        if (!string.IsNullOrEmpty(agency)) {
            newEvent.Agency = agency;
        }

        // Value (for facts)
        string value = MCPHelper.GetStringArgument(args, "value", "");
        if (!string.IsNullOrEmpty(value)) {
            newEvent.StringValue = value;
        }

        familyRec.Events.Add(newEvent);
        baseContext.SetModified();

        string evtName = matchedDef != null ? matchedDef.DisplayName : tag;
        return MCPContent.CreateSimpleContent($"Event '{evtName}' added to family '{familyXRef}' at index {familyRec.Events.Count - 1}");
    }
}


internal class FamDeleteEventCommand : BaseCommand
{
    public FamDeleteEventCommand() : base("family_delete_event", null, CommandCategory.Family) { }

    public override void Execute(BaseContext baseContext, object obj)
    {
        // Empty for interactive mode
    }

    public override MCPTool CreateTool()
    {
        return new MCPTool {
            Name = Sign,
            Description = "Remove an event from a family by family XRef and event index",
            InputSchema = new MCPToolInputSchema {
                Properties = new Dictionary<string, MCPToolProperty> {
                    ["family_xref"] = new MCPToolProperty { Type = "string", Description = "XRef identifier of the family (e.g., 'F1')" },
                    ["event_index"] = new MCPToolProperty { Type = "integer", Description = "Zero-based index of the event in the family's event list" }
                },
                Required = new List<string> { "family_xref", "event_index" }
            }
        };
    }

    public override List<MCPContent> ExecuteTool(BaseContext baseContext, JsonElement args)
    {
        string familyXRef = MCPHelper.GetRequiredArgument(args, "family_xref");
        int eventIndex = MCPHelper.GetIntArgument(args, "event_index", -1);

        var familyRec = baseContext.Tree.FindXRef<GDMFamilyRecord>(familyXRef);
        if (familyRec == null)
            return MCPContent.CreateSimpleContent($"Family not found with XRef: {familyXRef}");

        if (!familyRec.HasEvents)
            return MCPContent.CreateSimpleContent($"Family '{familyXRef}' has no events.");

        if (eventIndex < 0 || eventIndex >= familyRec.Events.Count)
            return MCPContent.CreateSimpleContent($"Invalid event index {eventIndex} for family '{familyXRef}' (has {familyRec.Events.Count} events).");

        var evt = familyRec.Events[eventIndex];
        string evtInfo = GKUtils.GetEventName(evt);

        familyRec.Events.RemoveAt(eventIndex);
        baseContext.SetModified();

        return MCPContent.CreateSimpleContent($"Event removed from family '{familyXRef}' at index {eventIndex}: {evtInfo}");
    }
}
