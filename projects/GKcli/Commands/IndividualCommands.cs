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


/// <summary>
/// For console use only (for MCP - see <see cref="RecordListCommand"/>).
/// </summary>
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
                    ["name"] = new MCPToolProperty { Type = "string", Description = "Full name in one of these formats: 'Имя Отчество /Фамилия/' or 'FirstName MiddleName /LastName/'. Any part may be missing. The last name MUST be enclosed in slashes if present." },
                    /*
                    ["first_name"] = new MCPToolProperty { Type = "string", Description = "A individual's first name (given)." },
                    ["middle_name"] = new MCPToolProperty { Type = "string", Description = "A individual's middle name or patronymic." },
                    ["last_name"] = new MCPToolProperty { Type = "string", Description = "A individual's last name (surname)." },
                     */
                    ["sex"] = new MCPToolProperty { Type = "string", Description = "Sex: 'm' or 'f'" },
                    ["nickname"] = new MCPToolProperty { Type = "string", Description = "Nickname or alternative name (optional)" }
                },
                Required = new List<string> { "name", "sex" }
            }
        };
    }

    public override List<MCPContent> ExecuteTool(BaseContext baseContext, JsonElement args)
    {
        string name = MCPHelper.GetRequiredStr(args, "name");

        string sexStr = MCPHelper.GetRequiredStr(args, "sex").ToLowerInvariant();
        char sex = (sexStr.Length > 0) ? sexStr[0] : 'm';
        if (sex != 'm' && sex != 'f') sex = 'm';
        string nickname = MCPHelper.GetOptionalStr(args, "nickname", string.Empty);

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


internal class IndiEditCommand : BaseCommand
{
    public IndiEditCommand() : base("individual_edit", null, CommandCategory.Individual) { }

    public override void Execute(BaseContext baseContext, object obj)
    {
        // Not implemented yet
    }

    public override MCPTool CreateTool()
    {
        return new MCPTool {
            Name = Sign,
            Description = "Edit an existing individual to the database. Only provided fields will be updated. Use 'xref' to identify the record to modify.",
            InputSchema = new MCPToolInputSchema {
                Properties = new Dictionary<string, MCPToolProperty> {
                    ["xref"] = new MCPToolProperty { Type = "string", Description = "XRef identifier of the record" },
                    ["name"] = new MCPToolProperty { Type = "string", Description = "New full name in one of these formats: 'Имя Отчество /Фамилия/' or 'FirstName MiddleName /LastName/'. Any part may be missing. The last name MUST be enclosed in slashes if present." },
                    ["sex"] = new MCPToolProperty { Type = "string", Description = "New sex: 'm' or 'f'" },
                    ["nickname"] = new MCPToolProperty { Type = "string", Description = "New nickname or alternative name" }
                },
                Required = new List<string> { "xref" }
            }
        };
    }

    public override List<MCPContent> ExecuteTool(BaseContext baseContext, JsonElement args)
    {
        string xref = MCPHelper.GetRequiredStr(args, "xref");
        var indiRec = baseContext.Tree.FindXRef<GDMIndividualRecord>(xref);
        if (indiRec == null)
            return MCPContent.CreateSimpleContent($"Individual not found with XRef: {xref}");

        var persName = (indiRec.PersonalNames.Count > 0) ? indiRec.PersonalNames[0] : indiRec.AddPersonalName(new GDMPersonalName());

        string name = MCPHelper.GetOptionalStr(args, "name", null);
        if (name != null) {
            persName.ParseString(name);
        }

        string sexStr = MCPHelper.GetOptionalStr(args, "sex", null);
        if (sexStr != null) {
            sexStr = sexStr.ToLowerInvariant();
            char sex = (sexStr.Length > 0) ? sexStr[0] : 'm';
            if (sex != 'm' && sex != 'f') sex = 'm';
            indiRec.Sex = (sex == 'm') ? GDMSex.svMale : GDMSex.svFemale;
        }

        string nickname = MCPHelper.GetOptionalStr(args, "nickname", null);
        if (nickname != null) {
            if (!string.IsNullOrEmpty(nickname)) {
                persName.Nickname = nickname;
            }
        }

        baseContext.SetModified();
        string resultName = GKUtils.GetNameString(indiRec, false);
        return MCPContent.CreateSimpleContent($"Individual updated: {resultName} with XRef `{indiRec.XRef}`");
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
        string searchName = MCPHelper.GetRequiredStr(args, "name");

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


/// <summary>
/// For console use only (for MCP - see <see cref="RecordDeleteCommand"/>).
/// </summary>
internal class IndiDeleteCommand : BaseCommand
{
    public IndiDeleteCommand() : base("individual_delete", null, CommandCategory.Individual) { }

    public override void Execute(BaseContext baseContext, object obj)
    {
        // Not implemented yet
    }
}


internal class IndiListSpousesCommand : BaseCommand
{
    public IndiListSpousesCommand() : base("individual_list_spouses", null, CommandCategory.Individual) { }

    public override void Execute(BaseContext baseContext, object obj)
    {
        // Not implemented yet
    }

    public override MCPTool CreateTool()
    {
        return new MCPTool {
            Name = Sign,
            Description = "List all spouses of an individual by their XRef identifier",
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

        if (indiRec.SpouseToFamilyLinks.Count <= 0)
            return MCPContent.CreateSimpleContent($"Individual '{individualXRef}' has no spouses.");

        var rows = new List<string> {
            $"Spouses for individual '{individualXRef}' ({indiRec.SpouseToFamilyLinks.Count}):",
            "| Index | Family XRef | Spouse XRef | Spouse Name |",
            "|---|---|---|---|"
        };
        for (int i = 0; i < indiRec.SpouseToFamilyLinks.Count; i++) {
            var stfLink = indiRec.SpouseToFamilyLinks[i];
            var familyRec = baseContext.Tree.GetPtrValue<GDMFamilyRecord>(stfLink);
            var spouse = (familyRec != null) ? baseContext.Tree.GetSpouseBy(familyRec, indiRec) : null;
            var spouseName = GKUtils.GetNameString(spouse, false);
            rows.Add($"|{i}|{stfLink.XRef}|{familyRec.XRef}|{spouseName}|");
        }

        return MCPContent.CreateSimpleContent(string.Join("\n", rows));
    }
}


internal class IndiListGroupsCommand : BaseCommand
{
    public IndiListGroupsCommand() : base("individual_list_groups", null, CommandCategory.Individual) { }

    public override void Execute(BaseContext baseContext, object obj)
    {
        // Not implemented yet
    }

    public override MCPTool CreateTool()
    {
        return new MCPTool {
            Name = Sign,
            Description = "List all groups an individual belongs to by their XRef identifier",
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

        if (!indiRec.HasGroups)
            return MCPContent.CreateSimpleContent($"Individual '{individualXRef}' belongs to no groups.");

        var rows = new List<string> {
            $"Groups for individual '{individualXRef}' ({indiRec.Groups.Count}):",
            "| Index | Group XRef | Group Name |",
            "|---|---|---|"
        };
        for (int i = 0; i < indiRec.Groups.Count; i++) {
            var groupLink = indiRec.Groups[i];
            var groupRec = baseContext.Tree.GetPtrValue<GDMGroupRecord>(groupLink);
            rows.Add($"|{i}|{groupLink.XRef}|{groupRec.GroupName}|");
        }

        return MCPContent.CreateSimpleContent(string.Join("\n", rows));
    }
}
