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

internal class IndiListPersonalNamesTool : BaseTool
{
    public IndiListPersonalNamesTool() : base("individual_list_personal_names") { }

    public override MCPTool CreateTool()
    {
        return new MCPTool {
            Name = Sign,
            Description = "List all personal names of an individual by their XRef identifier",
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

        if (indiRec.PersonalNames.Count <= 0)
            return MCPContent.CreateSimpleContent($"Individual '{individualXRef}' has no personal names.");

        var rows = new List<string> {
            $"Personal names for individual '{individualXRef}' ({indiRec.PersonalNames.Count}):",
            "| Index | First Middle/Patronymic | Surname | Name Type | Nickname |",
            "|---|---|---|---|---|"
        };
        for (int i = 0; i < indiRec.PersonalNames.Count; i++) {
            var name = indiRec.PersonalNames[i];
            rows.Add($"|{i}|{name.FirstPart}|{name.Surname}|{name.NameType}|{name.Nickname}|");
        }

        return MCPContent.CreateSimpleContent(string.Join("\n", rows));
    }
}


internal class IndiUpsertPersonalNameTool : BaseTool
{
    public IndiUpsertPersonalNameTool() : base("individual_upsert_personal_name") { }

    public override MCPTool CreateTool()
    {
        var nameTypes = RuntimeData.NameTypeMap.Keys.ToList();
        var langs = RuntimeData.LangMap.Keys.ToList();

        return new MCPTool {
            Name = Sign,
            Description = "Add personal name to individual or update existing. Provide 'name_index' to edit; omit 'name_index' to create. 'individual_xref', 'given' and 'surname' required for new names.",
            InputSchema = new MCPToolInputSchema {
                Properties = new Dictionary<string, MCPToolProperty> {
                    ["individual_xref"] = new MCPToolProperty { Type = "string", Description = "XRef identifier of the individual (e.g., 'I1')" },
                    ["name_index"] = new MCPToolProperty { Type = "integer", Description = "Zero-based index of the personal name in the individual's personal names list (omit for new)" },
                    ["given"] = new MCPToolProperty { Type = "string", Description = "Given name (first name)" },
                    ["surname"] = new MCPToolProperty { Type = "string", Description = "Surname (family name)" },
                    ["surname_prefix"] = new MCPToolProperty { Type = "string", Description = "Surname prefix (e.g., 'de', 'van')" },
                    ["name_prefix"] = new MCPToolProperty { Type = "string", Description = "Name prefix (e.g., 'Dr.', 'Sir')" },
                    ["name_suffix"] = new MCPToolProperty { Type = "string", Description = "Name suffix (e.g., 'Jr.', 'III')" },
                    ["nickname"] = new MCPToolProperty { Type = "string", Description = "Nickname or alias" },
                    ["name_type"] = new MCPToolProperty { Type = "string", Description = "Type of name (Birth, Married, Aka, etc.)", Enum = nameTypes },
                    ["language"] = new MCPToolProperty { Type = "string", Description = "Language of the name", Enum = langs },
                    ["patronymic"] = new MCPToolProperty { Type = "string", Description = "Patronymic name" },
                    ["married_name"] = new MCPToolProperty { Type = "string", Description = "Married name" },
                    ["religious_name"] = new MCPToolProperty { Type = "string", Description = "Religious name" },
                    ["census_name"] = new MCPToolProperty { Type = "string", Description = "Name as it appears in census records" }
                },
                Required = new List<string> { "individual_xref" }
            }
        };
    }

    public override List<MCPContent> ExecuteTool(BaseContext baseContext, JsonElement args)
    {
        string individualXRef = MCPHelper.GetRequiredStr(args, "individual_xref");
        int? nameIndex = MCPHelper.GetOptionalNullableInt(args, "name_index", null);
        string given = MCPHelper.GetOptionalStr(args, "given", null);
        string surname = MCPHelper.GetOptionalStr(args, "surname", null);

        string surnamePrefix = MCPHelper.GetOptionalStr(args, "surname_prefix", null);
        string namePrefix = MCPHelper.GetOptionalStr(args, "name_prefix", null);
        string nameSuffix = MCPHelper.GetOptionalStr(args, "name_suffix", null);
        string nickname = MCPHelper.GetOptionalStr(args, "nickname", null);
        string nameTypeStr = MCPHelper.GetOptionalStr(args, "name_type", null);
        string languageStr = MCPHelper.GetOptionalStr(args, "language", null);
        string patronymic = MCPHelper.GetOptionalStr(args, "patronymic", null);
        string marriedName = MCPHelper.GetOptionalStr(args, "married_name", null);
        string religiousName = MCPHelper.GetOptionalStr(args, "religious_name", null);
        string censusName = MCPHelper.GetOptionalStr(args, "census_name", null);

        var indiRec = baseContext.Tree.FindXRef<GDMIndividualRecord>(individualXRef);
        if (indiRec == null)
            return MCPContent.CreateSimpleContent($"❌ Individual not found with XRef: {individualXRef}");

        bool isEdit = nameIndex.HasValue;
        if (isEdit) {
            int index = nameIndex.Value;

            if (indiRec.PersonalNames.Count <= 0)
                return MCPContent.CreateSimpleContent($"❌ Individual '{individualXRef}' has no personal names.");

            if (index < 0 || index >= indiRec.PersonalNames.Count)
                return MCPContent.CreateSimpleContent($"❌ Invalid name index {index} for individual '{individualXRef}' (has {indiRec.PersonalNames.Count} personal names).");

            var personalName = indiRec.PersonalNames[index];

            if (given != null) {
                personalName.Given = given;
            }

            if (surname != null) {
                personalName.Surname = surname;
            }

            if (surnamePrefix != null) {
                personalName.SurnamePrefix = surnamePrefix;
            }

            if (namePrefix != null) {
                personalName.NamePrefix = namePrefix;
            }

            if (nameSuffix != null) {
                personalName.NameSuffix = nameSuffix;
            }

            if (nickname != null) {
                personalName.Nickname = nickname;
            }

            if (nameTypeStr != null) {
                if (RuntimeData.NameTypeMap.TryGetValue(nameTypeStr, out GDMNameType nameType)) {
                    personalName.NameType = nameType;
                } else {
                    return MCPContent.CreateSimpleContent($"❌ Invalid name type: {nameTypeStr}");
                }
            }

            if (languageStr != null) {
                if (RuntimeData.LangMap.TryGetValue(languageStr, out GDMLanguageID language)) {
                    personalName.Language = language;
                } else {
                    return MCPContent.CreateSimpleContent($"❌ Invalid language: {languageStr}");
                }
            }

            if (patronymic != null) {
                personalName.PatronymicName = patronymic;
            }

            if (marriedName != null) {
                personalName.MarriedName = marriedName;
            }

            if (religiousName != null) {
                personalName.ReligiousName = religiousName;
            }

            if (censusName != null) {
                personalName.CensusName = censusName;
            }

            baseContext.SetModified();
            string nameInfo = $"{personalName.FullName}";
            return MCPContent.CreateSimpleContent($"✅ Personal name updated for individual '{individualXRef}' at index {index}: {nameInfo}");
        } else {
            if (given == null)
                return MCPContent.CreateSimpleContent("❌ 'given' required for new personal name");

            if (surname == null)
                return MCPContent.CreateSimpleContent("❌ 'surname' required for new personal name");

            var personalName = new GDMPersonalName();
            personalName.Given = given;
            personalName.Surname = surname;

            if (surnamePrefix != null) {
                personalName.SurnamePrefix = surnamePrefix;
            }

            if (namePrefix != null) {
                personalName.NamePrefix = namePrefix;
            }

            if (nameSuffix != null) {
                personalName.NameSuffix = nameSuffix;
            }

            if (nickname != null) {
                personalName.Nickname = nickname;
            }

            if (nameTypeStr != null) {
                if (RuntimeData.NameTypeMap.TryGetValue(nameTypeStr, out GDMNameType nameType)) {
                    personalName.NameType = nameType;
                } else {
                    return MCPContent.CreateSimpleContent($"❌ Invalid name type: {nameTypeStr}");
                }
            }

            if (languageStr != null) {
                if (RuntimeData.LangMap.TryGetValue(languageStr, out GDMLanguageID language)) {
                    personalName.Language = language;
                } else {
                    return MCPContent.CreateSimpleContent($"❌ Invalid language: {languageStr}");
                }
            }

            if (patronymic != null) {
                personalName.PatronymicName = patronymic;
            }

            if (marriedName != null) {
                personalName.MarriedName = marriedName;
            }

            if (religiousName != null) {
                personalName.ReligiousName = religiousName;
            }

            if (censusName != null) {
                personalName.CensusName = censusName;
            }

            indiRec.AddPersonalName(personalName);

            baseContext.SetModified();
            int newIndex = indiRec.PersonalNames.IndexOf(personalName);
            return MCPContent.CreateSimpleContent($"✅ Personal name added to individual '{individualXRef}' at index {newIndex}: {personalName.FullName}");
        }
    }
}


internal class IndiDeletePersonalNameTool : BaseTool
{
    public IndiDeletePersonalNameTool() : base("individual_delete_personal_name") { }

    public override MCPTool CreateTool()
    {
        return new MCPTool {
            Name = Sign,
            Description = "Remove a personal name from an individual by individual XRef and name index",
            InputSchema = new MCPToolInputSchema {
                Properties = new Dictionary<string, MCPToolProperty> {
                    ["individual_xref"] = new MCPToolProperty { Type = "string", Description = "XRef identifier of the individual (e.g., 'I1')" },
                    ["name_index"] = new MCPToolProperty { Type = "integer", Description = "Zero-based index of the personal name in the individual's personal names list" }
                },
                Required = new List<string> { "individual_xref", "name_index" }
            }
        };
    }

    public override List<MCPContent> ExecuteTool(BaseContext baseContext, JsonElement args)
    {
        string individualXRef = MCPHelper.GetRequiredStr(args, "individual_xref");
        int nameIndex = MCPHelper.GetOptionalInt(args, "name_index", -1);

        var indiRec = baseContext.Tree.FindXRef<GDMIndividualRecord>(individualXRef);
        if (indiRec == null)
            return MCPContent.CreateSimpleContent($"Individual not found with XRef: {individualXRef}");

        if (indiRec.PersonalNames.Count <= 0)
            return MCPContent.CreateSimpleContent($"Individual '{individualXRef}' has no personal names.");

        if (nameIndex < 0 || nameIndex >= indiRec.PersonalNames.Count)
            return MCPContent.CreateSimpleContent($"Invalid name index {nameIndex} for individual '{individualXRef}' (has {indiRec.PersonalNames.Count} personal names).");

        var personalName = indiRec.PersonalNames[nameIndex];
        string nameInfo = $"{personalName.FullName}";

        indiRec.PersonalNames.RemoveAt(nameIndex);
        baseContext.SetModified();

        return MCPContent.CreateSimpleContent($"Personal name removed from individual '{individualXRef}' at index {nameIndex}: {nameInfo}");
    }
}
