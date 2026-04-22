/*
 *  GEDKeeper, the personal genealogical database editor.
 *  Copyright (C) 2009-2026 by Sergey V. Zhdanovskih.
 *
 *  Licensed under the GNU General Public License (GPL) v3.
 *  See LICENSE file in the project root for full license information.
 */

using System.Collections.Generic;
using System.Text.Json;
using GKcli.MCP;
using GKCore;
using GKCore.Locales;
using GKCore.Tools;
using GKUI.Platform;

namespace GKcli.Commands;

internal class ToolsMenuCommand : BaseCommand
{
    public ToolsMenuCommand() : base("tools", LSID.MITreeTools, CommandCategory.Service) { }

    public override void Execute(BaseContext baseContext, object obj)
    {
        CommandController.SelectCommand(CommandCategory.Tools, true, "Select a tool");
    }
}


internal class TreeCheckCommand : BaseCommand
{
    public TreeCheckCommand() : base("tree_check", LSID.TreeCheck, CommandCategory.Tools) { }

    public override void Execute(BaseContext baseContext, object obj)
    {
    }
}


internal class TreeCompareCommand : BaseCommand
{
    public TreeCompareCommand() : base("tree_compare", LSID.TreeCompare, CommandCategory.Tools) { }

    public override void Execute(BaseContext baseContext, object obj)
    {
        // Not implemented yet
    }
}


internal class TreeMergeCommand : BaseCommand
{
    public TreeMergeCommand() : base("tree_merge", LSID.TreeMerge, CommandCategory.Tools) { }

    public override void Execute(BaseContext baseContext, object obj)
    {
    }

    public override MCPTool CreateTool()
    {
        return new MCPTool {
            Name = Sign,
            Description = "Merge another GEDCOM file into the current database",
            InputSchema = new MCPToolInputSchema {
                Properties = new Dictionary<string, MCPToolProperty> {
                    ["path"] = new MCPToolProperty { Type = "string", Description = "Path to the .ged file" }
                },
                Required = new List<string> { "path" }
            }
        };
    }

    public override List<MCPContent> ExecuteTool(BaseContext baseContext, JsonElement args)
    {
        string path = MCPHelper.GetRequiredStr(args, "path");

        var textLog = new TextOutput();

        //var sw = Stopwatch.StartNew();
        TreeTools.MergeTreeFile(baseContext.Tree, path, textLog, true);
        baseContext.SetModified();
        //sw.Stop();
        //return MCPContent.CreateSimpleContent($"Databases merged: {path}. Records: {baseContext.Tree.RecordsCount}. Time: {sw.Elapsed.TotalSeconds:F3}s.");

        return MCPContent.CreateSimpleContent(textLog.ToString());
    }
}


internal class TreeSplitCommand : BaseCommand
{
    public TreeSplitCommand() : base("tree_split", LSID.TreeSplit, CommandCategory.Tools) { }

    public override void Execute(BaseContext baseContext, object obj)
    {
    }
}


internal class PlacesManagerCommand : BaseCommand
{
    public PlacesManagerCommand() : base("places_manager", LSID.PlacesManager, CommandCategory.Tools) { }

    public override void Execute(BaseContext baseContext, object obj)
    {
    }
}


internal class RecMergeCommand : BaseCommand
{
    public RecMergeCommand() : base("record_merge", LSID.MergeDuplicates, CommandCategory.Tools) { }

    public override void Execute(BaseContext baseContext, object obj)
    {
    }
}


internal class FamilyGroupsCommand : BaseCommand
{
    public FamilyGroupsCommand() : base("family_groups", LSID.FragmentSearch, CommandCategory.Tools) { }

    public override void Execute(BaseContext baseContext, object obj)
    {
    }
}


internal class PatSearchCommand : BaseCommand
{
    public PatSearchCommand() : base("patriarch_search", LSID.PatSearch, CommandCategory.Tools) { }

    public override void Execute(BaseContext baseContext, object obj)
    {
    }
}
