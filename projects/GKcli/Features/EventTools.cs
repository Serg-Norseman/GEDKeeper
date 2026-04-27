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
using GKCore.Controllers;
using GKCore.Events;
using GKUI.Platform;

namespace GKcli.Features;

internal abstract class EventTool : BaseTool
{
    protected EventTool(string sign) : base(sign) { }

    protected static List<string> GetEventTypes(EventTarget eventTarget)
    {
        var eventTypes = BaseController.GetEventTypes(eventTarget);
        var rows = new List<string>(eventTypes.Count);
        for (int i = 0; i < eventTypes.Count; i++) {
            var evt = eventTypes[i];
            rows.Add(evt.DisplayName);
        }
        return rows;
    }

    protected static List<MCPContent> GetEventTypesTable(EventTarget eventTarget)
    {
        var eventTypes = BaseController.GetEventTypes(eventTarget);

        var rows = new List<string>(eventTypes.Count + 2) {
            "|DisplayName|Tag|Type|\n|---|---|---|"
        };
        for (int i = 0; i < eventTypes.Count; i++) {
            var evt = eventTypes[i];
            rows.Add($"|{evt.DisplayName}|{evt.Tag}|{evt.Type}|");
        }

        return MCPContent.CreateSimpleContent(string.Join("\n", rows));
    }

    protected static List<MCPContent> GetEventsList(BaseContext baseContext, string recName, GDMRecordWithEvents recordWithEvents)
    {
        if (!recordWithEvents.HasEvents)
            return MCPContent.CreateSimpleContent($"{CLIHelper.ToUpperFirst(recName)} '{recordWithEvents.XRef}' has no events.");

        var rows = new List<string> {
            $"Events for {recName} '{recordWithEvents.XRef}' ({recordWithEvents.Events.Count}):",
            "| Index | Event | Date | Age | Place | Cause | Sources | Notes | Media |",
            "|---|---|---|---|---|---|---|---|---|"
        };
        for (int i = 0; i < recordWithEvents.Events.Count; i++) {
            var evt = recordWithEvents.Events[i];
            string eventName = GKUtils.GetEventName(evt);
            string dateStr = GKUtils.GetDateDisplayString(evt.Date.Value);
            string ageStr = GKUtils.GetAgeDisplayStr(evt);
            string placeStr = GKUtils.GetEventPlaceAndAttributeValues(evt);
            string causeStr = GKUtils.GetEventCause(evt);
            string sourcesStr = evt.HasSourceCitations ? evt.SourceCitations.Count.ToString() : string.Empty;
            string notesStr = evt.HasNotes ? evt.Notes.Count.ToString() : string.Empty;
            string mediaStr = evt.HasMultimediaLinks ? evt.MultimediaLinks.Count.ToString() : string.Empty;
            rows.Add($"|{i + 1}|{eventName}|{dateStr}|{ageStr}|{placeStr}|{causeStr}|{sourcesStr}|{notesStr}|{mediaStr}|");
        }

        return MCPContent.CreateSimpleContent(string.Join("\n", rows));
    }

    protected static List<MCPContent> UpsertEvent(BaseContext baseContext, GDMRecordWithEvents record, string recName, JsonElement args)
    {
        int eventIndex = MCPHelper.GetOptionalInt(args, "event_index", -1);

        bool isEdit = eventIndex >= 0;

        if (isEdit) {
            if (!record.HasEvents)
                return MCPContent.CreateSimpleContent($"❌ Record '{recName}' has no events.");

            if (eventIndex < 0 || eventIndex >= record.Events.Count)
                return MCPContent.CreateSimpleContent($"❌ Invalid event index {eventIndex} for record '{record.XRef}' (has {record.Events.Count} events).");

            var evt = record.Events[eventIndex];

            //string tag = MCPHelper.GetOptionalStr(args, "tag", null);
            //string argType = MCPHelper.GetOptionalStr(args, "type", null);
            //EventDef matchedDef = AppHost.EventDefinitions.Find(tag, argType);
            EventDef matchedDef = AppHost.EventDefinitions.Find(evt);

            // Determine if this tag corresponds to a fact
            bool isFact = matchedDef != null && matchedDef.HasValue();

            bool isIndi = (record is GDMIndividualRecord);

            //newEvent.SetName(tag);
            //newEvent.Classification = matchedDef != null ? matchedDef.Type : argType;

            // Date, prompt is configured to transmit only GEDCOM-compatible strings.
            string dateStr = MCPHelper.GetOptionalStr(args, "date", null);
            if (dateStr != null) {
                evt.Date.ParseString(dateStr);
            }

            // Place (string or location xref)
            string placeStr = MCPHelper.GetOptionalStr(args, "place", null);
            string locationXRef = MCPHelper.GetOptionalStr(args, "location_xref", null);
            if (locationXRef != null) {
                var locRec = baseContext.Tree.FindXRef<GDMLocationRecord>(locationXRef);
                if (!string.IsNullOrEmpty(locationXRef) && locRec == null)
                    return MCPContent.CreateSimpleContent($"❌ Location not found with XRef: {locationXRef}");

                baseContext.Tree.SetPtrValue(evt.Place.Location, locRec);
            } else if (placeStr != null) {
                evt.Place.StringValue = placeStr;
            }

            // Cause
            string cause = MCPHelper.GetOptionalStr(args, "cause", null);
            if (cause != null) {
                evt.Cause = cause;
            }

            // Agency
            string agency = MCPHelper.GetOptionalStr(args, "agency", null);
            if (agency != null) {
                evt.Agency = agency;
            }

            // Value (for facts)
            if (isFact) {
                string value = MCPHelper.GetOptionalStr(args, "value", null);
                if (value != null) {
                    evt.StringValue = value;
                }
            }

            if (isIndi) {
                string indiAge = MCPHelper.GetOptionalStr(args, "age", null);
                if (!string.IsNullOrEmpty(indiAge)) {
                    ((GDMIndividualEventDetail)evt).Age.StringValue = AgeEditDlgController.GetAgeStr(indiAge);
                }
            } else {
                string husbAge = MCPHelper.GetOptionalStr(args, "husband_age", null);
                if (!string.IsNullOrEmpty(husbAge)) {
                    ((GDMFamilyEvent)evt).HusbandAge.StringValue = AgeEditDlgController.GetAgeStr(husbAge);
                }

                string wifeAge = MCPHelper.GetOptionalStr(args, "wife_age", null);
                if (!string.IsNullOrEmpty(wifeAge)) {
                    ((GDMFamilyEvent)evt).WifeAge.StringValue = AgeEditDlgController.GetAgeStr(wifeAge);
                }
            }

            baseContext.SetModified();
            string evtName = GKUtils.GetEventName(evt);
            return MCPContent.CreateSimpleContent($"✅ Event '{evtName}' updated to {recName} '{record.XRef}' at index {eventIndex}");
        } else {
            string argType = MCPHelper.GetOptionalStr(args, "type", null);
            if (string.IsNullOrEmpty(argType))
                return MCPContent.CreateSimpleContent("❌ 'type' required for new event");

            EventDef matchedDef = AppHost.EventDefinitions.Find(argType);
            if (matchedDef == null)
                return MCPContent.CreateSimpleContent($"❌ Event type '{argType}' not found");

            // Determine if this tag corresponds to a fact
            bool isFact = matchedDef.HasValue();

            bool isIndi = (record is GDMIndividualRecord);
            GDMCustomEvent newEvent;
            if (isIndi) {
                if (isFact) {
                    newEvent = new GDMIndividualAttribute();
                } else {
                    newEvent = new GDMIndividualEvent();
                }
            } else {
                newEvent = new GDMFamilyEvent();
            }
            newEvent.SetName(matchedDef.Tag);
            newEvent.Classification = matchedDef.Type;

            // Date
            string dateStr = MCPHelper.GetOptionalStr(args, "date", "");
            if (!string.IsNullOrEmpty(dateStr)) {
                // Prompt is configured to transmit only GEDCOM-compatible strings.
                newEvent.Date.ParseString(dateStr);
            }

            // Place (string or location xref)
            string placeStr = MCPHelper.GetOptionalStr(args, "place", "");
            string locationXRef = MCPHelper.GetOptionalStr(args, "location_xref", "");
            if (!string.IsNullOrEmpty(locationXRef)) {
                var locRec = baseContext.Tree.FindXRef<GDMLocationRecord>(locationXRef);
                if (locRec == null)
                    return MCPContent.CreateSimpleContent($"Location not found with XRef: {locationXRef}");
                baseContext.Tree.SetPtrValue(newEvent.Place.Location, locRec);
            } else if (!string.IsNullOrEmpty(placeStr)) {
                newEvent.Place.StringValue = placeStr;
            }

            // Cause
            string cause = MCPHelper.GetOptionalStr(args, "cause", "");
            if (!string.IsNullOrEmpty(cause)) {
                newEvent.Cause = cause;
            }

            // Agency
            string agency = MCPHelper.GetOptionalStr(args, "agency", "");
            if (!string.IsNullOrEmpty(agency)) {
                newEvent.Agency = agency;
            }

            // Value (for facts)
            if (isFact) {
                string value = MCPHelper.GetOptionalStr(args, "value", "");
                newEvent.StringValue = value;
            }

            if (isIndi) {
                string indiAge = MCPHelper.GetOptionalStr(args, "age", null);
                if (!string.IsNullOrEmpty(indiAge)) {
                    ((GDMIndividualEventDetail)newEvent).Age.StringValue = AgeEditDlgController.GetAgeStr(indiAge);
                }
            } else {
                string husbAge = MCPHelper.GetOptionalStr(args, "husband_age", null);
                if (!string.IsNullOrEmpty(husbAge)) {
                    ((GDMFamilyEvent)newEvent).HusbandAge.StringValue = AgeEditDlgController.GetAgeStr(husbAge);
                }

                string wifeAge = MCPHelper.GetOptionalStr(args, "wife_age", null);
                if (!string.IsNullOrEmpty(wifeAge)) {
                    ((GDMFamilyEvent)newEvent).WifeAge.StringValue = AgeEditDlgController.GetAgeStr(wifeAge);
                }
            }

            record.Events.Add(newEvent);

            baseContext.SetModified();
            return MCPContent.CreateSimpleContent($"Event '{argType}' added to {recName} '{record.XRef}' at index {record.Events.Count - 1}");
        }
    }
}


// FIXME: too large, causes context exhaustion, not suitable for use in other tools
internal class EventTypeListTool : EventTool
{
    public EventTypeListTool() : base("event_type_list") { }

    public override MCPTool CreateTool()
    {
        var recTypes = RuntimeData.RWETypeMap.Keys.ToList();

        return new MCPTool {
            Name = Sign,
            Description = "List all available event types for individuals or families",
            InputSchema = new MCPToolInputSchema {
                Properties = new Dictionary<string, MCPToolProperty> {
                    ["record_type"] = new MCPToolProperty { Type = "string", Description = "Record type", Enum = recTypes },
                },
                Required = new List<string> { "record_type" }
            }
        };
    }

    public override List<MCPContent> ExecuteTool(BaseContext baseContext, JsonElement args)
    {
        string recordTypeStr = MCPHelper.GetRequiredStr(args, "record_type");
        if (!RuntimeData.RWETypeMap.TryGetValue(recordTypeStr, out EventTarget eventTarget)) {
            string availableTypes = string.Join(", ", RuntimeData.RecordTypeMap.Keys);
            return MCPContent.CreateSimpleContent($"Unknown record type: '{recordTypeStr}'. Available types: {availableTypes}");
        }

        return GetEventTypesTable(eventTarget);
    }
}


internal class GEDCOMDateSpecTool : BaseTool
{
    public GEDCOMDateSpecTool() : base("gedcom_date_spec") { }

    public override MCPTool CreateTool()
    {
        return new MCPTool {
            Name = Sign,
            Description = "Get GEDCOM date specification",
            InputSchema = MCPToolInputSchema.Empty
        };
    }

    public override List<MCPContent> ExecuteTool(BaseContext baseContext, JsonElement args)
    {
        return MCPContent.CreateSimpleContent(RuntimeData.GetDateSpec());
    }
}


internal class GEDCOMDateSpecResource : BaseResource
{
    public GEDCOMDateSpecResource() : base(@"gedcom://date_spec") { }

    public override MCPResource CreateResource()
    {
        return new MCPResource {
            Uri = @"gedcom://date_spec",
            Name = "date_spec",
            Description = "GEDCOM date specification",
            MimeType = "text/plain"
        };
    }

    public override List<MCPResourceContents> Get(BaseContext baseContext)
    {
        return new List<MCPResourceContents> {
            new MCPResourceContents {
                Uri = fUri,
                MimeType = "text/plain",
                Text = RuntimeData.GetDateSpec()
            }
        };
    }
}
