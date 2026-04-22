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

namespace GKcli.Commands;

internal class IndiListPersonalNamesCommand : BaseCommand
{
    public IndiListPersonalNamesCommand() : base("individual_list_personal_names", null, CommandCategory.Individual) { }

    public override void Execute(BaseContext baseContext, object obj)
    {
        // Not implemented yet
    }

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


internal class IndiAddPersonalNameCommand : BaseCommand
{
    public IndiAddPersonalNameCommand() : base("individual_add_personal_name", null, CommandCategory.Individual) { }

    public override void Execute(BaseContext baseContext, object obj)
    {
        // Not implemented yet
    }

    public override MCPTool CreateTool()
    {
        var nameTypes = RuntimeData.NameTypeMap.Keys.ToList();
        var langs = RuntimeData.LangMap.Keys.ToList();

        return new MCPTool {
            Name = Sign,
            Description = "Add a personal name to an individual",
            InputSchema = new MCPToolInputSchema {
                Properties = new Dictionary<string, MCPToolProperty> {
                    ["individual_xref"] = new MCPToolProperty { Type = "string", Description = "XRef identifier of the individual (e.g., 'I1')" },
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
                Required = new List<string> { "individual_xref", "given", "surname" }
            }
        };
    }

    public override List<MCPContent> ExecuteTool(BaseContext baseContext, JsonElement args)
    {
        string individualXRef = MCPHelper.GetRequiredStr(args, "individual_xref");
        string given = MCPHelper.GetRequiredStr(args, "given");
        string surname = MCPHelper.GetRequiredStr(args, "surname");

        var indiRec = baseContext.Tree.FindXRef<GDMIndividualRecord>(individualXRef);
        if (indiRec == null)
            return MCPContent.CreateSimpleContent($"Individual not found with XRef: {individualXRef}");

        if (!indiRec.GetAccessibleSubstructures().HasFlag(GDMStructureType.PersonalName))
            return MCPContent.CreateSimpleContent($"Record type '{individualXRef}' ({indiRec.RecordType}) does not support personal names.");

        var personalName = new GDMPersonalName();
        personalName.Given = given;
        personalName.Surname = surname;

        string surnamePrefix = MCPHelper.GetOptionalStr(args, "surname_prefix", null);
        if (!string.IsNullOrEmpty(surnamePrefix)) {
            personalName.SurnamePrefix = surnamePrefix;
        }

        string namePrefix = MCPHelper.GetOptionalStr(args, "name_prefix", null);
        if (!string.IsNullOrEmpty(namePrefix)) {
            personalName.NamePrefix = namePrefix;
        }

        string nameSuffix = MCPHelper.GetOptionalStr(args, "name_suffix", null);
        if (!string.IsNullOrEmpty(nameSuffix)) {
            personalName.NameSuffix = nameSuffix;
        }

        string nickname = MCPHelper.GetOptionalStr(args, "nickname", null);
        if (!string.IsNullOrEmpty(nickname)) {
            personalName.Nickname = nickname;
        }

        string nameTypeStr = MCPHelper.GetOptionalStr(args, "name_type", null);
        if (!string.IsNullOrEmpty(nameTypeStr)) {
            if (RuntimeData.NameTypeMap.TryGetValue(nameTypeStr, out GDMNameType nameType)) {
                personalName.NameType = nameType;
            } else {
                return MCPContent.CreateSimpleContent($"Invalid name type: {nameTypeStr}");
            }
        }

        string languageStr = MCPHelper.GetOptionalStr(args, "language", null);
        if (!string.IsNullOrEmpty(languageStr)) {
            if (RuntimeData.LangMap.TryGetValue(languageStr, out GDMLanguageID language)) {
                personalName.Language = language;
            } else {
                return MCPContent.CreateSimpleContent($"Invalid language: {languageStr}");
            }
        }

        string patronymic = MCPHelper.GetOptionalStr(args, "patronymic", null);
        if (!string.IsNullOrEmpty(patronymic)) {
            personalName.PatronymicName = patronymic;
        }

        string marriedName = MCPHelper.GetOptionalStr(args, "married_name", null);
        if (!string.IsNullOrEmpty(marriedName)) {
            personalName.MarriedName = marriedName;
        }

        string religiousName = MCPHelper.GetOptionalStr(args, "religious_name", null);
        if (!string.IsNullOrEmpty(religiousName)) {
            personalName.ReligiousName = religiousName;
        }

        string censusName = MCPHelper.GetOptionalStr(args, "census_name", null);
        if (!string.IsNullOrEmpty(censusName)) {
            personalName.CensusName = censusName;
        }

        indiRec.AddPersonalName(personalName);
        baseContext.SetModified();

        int nameIndex = indiRec.PersonalNames.IndexOf(personalName);
        return MCPContent.CreateSimpleContent($"Personal name added to individual '{individualXRef}' at index {nameIndex}: {personalName.FullName}");
    }
}


internal class IndiEditPersonalNameCommand : BaseCommand
{
    public IndiEditPersonalNameCommand() : base("individual_edit_personal_name", null, CommandCategory.Individual) { }

    public override void Execute(BaseContext baseContext, object obj)
    {
        // Not implemented yet
    }

    public override MCPTool CreateTool()
    {
        var nameTypes = RuntimeData.NameTypeMap.Keys.ToList();
        var langs = RuntimeData.LangMap.Keys.ToList();

        return new MCPTool {
            Name = Sign,
            Description = "Edit a personal name of an individual by individual XRef and name index",
            InputSchema = new MCPToolInputSchema {
                Properties = new Dictionary<string, MCPToolProperty> {
                    ["individual_xref"] = new MCPToolProperty { Type = "string", Description = "XRef identifier of the individual (e.g., 'I1')" },
                    ["name_index"] = new MCPToolProperty { Type = "integer", Description = "Zero-based index of the personal name in the individual's personal names list" },
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

        string given = MCPHelper.GetOptionalStr(args, "given", null);
        if (!string.IsNullOrEmpty(given)) {
            personalName.Given = given;
        }

        string surname = MCPHelper.GetOptionalStr(args, "surname", null);
        if (!string.IsNullOrEmpty(surname)) {
            personalName.Surname = surname;
        }

        string surnamePrefix = MCPHelper.GetOptionalStr(args, "surname_prefix", null);
        if (!string.IsNullOrEmpty(surnamePrefix)) {
            personalName.SurnamePrefix = surnamePrefix;
        }

        string namePrefix = MCPHelper.GetOptionalStr(args, "name_prefix", null);
        if (!string.IsNullOrEmpty(namePrefix)) {
            personalName.NamePrefix = namePrefix;
        }

        string nameSuffix = MCPHelper.GetOptionalStr(args, "name_suffix", null);
        if (!string.IsNullOrEmpty(nameSuffix)) {
            personalName.NameSuffix = nameSuffix;
        }

        string nickname = MCPHelper.GetOptionalStr(args, "nickname", null);
        if (!string.IsNullOrEmpty(nickname)) {
            personalName.Nickname = nickname;
        }

        string nameTypeStr = MCPHelper.GetOptionalStr(args, "name_type", null);
        if (!string.IsNullOrEmpty(nameTypeStr)) {
            if (RuntimeData.NameTypeMap.TryGetValue(nameTypeStr, out GDMNameType nameType)) {
                personalName.NameType = nameType;
            } else {
                return MCPContent.CreateSimpleContent($"Invalid name type: {nameTypeStr}");
            }
        }

        string languageStr = MCPHelper.GetOptionalStr(args, "language", null);
        if (!string.IsNullOrEmpty(languageStr)) {
            if (RuntimeData.LangMap.TryGetValue(languageStr, out GDMLanguageID language)) {
                personalName.Language = language;
            } else {
                return MCPContent.CreateSimpleContent($"Invalid language: {languageStr}");
            }
        }

        string patronymic = MCPHelper.GetOptionalStr(args, "patronymic", null);
        if (!string.IsNullOrEmpty(patronymic)) {
            personalName.PatronymicName = patronymic;
        }

        string marriedName = MCPHelper.GetOptionalStr(args, "married_name", null);
        if (!string.IsNullOrEmpty(marriedName)) {
            personalName.MarriedName = marriedName;
        }

        string religiousName = MCPHelper.GetOptionalStr(args, "religious_name", null);
        if (!string.IsNullOrEmpty(religiousName)) {
            personalName.ReligiousName = religiousName;
        }

        string censusName = MCPHelper.GetOptionalStr(args, "census_name", null);
        if (!string.IsNullOrEmpty(censusName)) {
            personalName.CensusName = censusName;
        }

        baseContext.SetModified();

        string nameInfo = $"{personalName.FullName}";
        return MCPContent.CreateSimpleContent($"Personal name updated for individual '{individualXRef}' at index {nameIndex}: {nameInfo}");
    }
}


internal class IndiDeletePersonalNameCommand : BaseCommand
{
    public IndiDeletePersonalNameCommand() : base("individual_delete_personal_name", null, CommandCategory.Individual) { }

    public override void Execute(BaseContext baseContext, object obj)
    {
        // Not implemented yet
    }

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
