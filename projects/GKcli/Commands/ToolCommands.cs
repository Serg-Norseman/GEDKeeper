/*
 *  GEDKeeper, the personal genealogical database editor.
 *  Copyright (C) 2009-2026 by Sergey V. Zhdanovskih.
 *
 *  Licensed under the GNU General Public License (GPL) v3.
 *  See LICENSE file in the project root for full license information.
 */

using GKCore;
using GKCore.Locales;

namespace GKcli.Commands;

internal class ToolsMenuCommand : BaseCommand
{
    public ToolsMenuCommand() : base("tools", LangMan.LS(LSID.MITreeTools), CommandCategory.Service) { }

    public override void Execute(BaseContext baseContext, object obj)
    {
        CommandController.Instance.SelectCommand(CommandCategory.Tools, true, "Select a tool");
    }
}


internal class TreeCheckCommand : BaseCommand
{
    public TreeCheckCommand() : base("tree_check", LangMan.LS(LSID.TreeCheck), CommandCategory.Tools) { }

    public override void Execute(BaseContext baseContext, object obj)
    {
    }
}


internal class TreeCompareCommand : BaseCommand
{
    public TreeCompareCommand() : base("tree_compare", LangMan.LS(LSID.TreeCompare), CommandCategory.Tools) { }

    public override void Execute(BaseContext baseContext, object obj)
    {
        // Not implemented yet
    }
}


internal class TreeMergeCommand : BaseCommand
{
    public TreeMergeCommand() : base("tree_merge", LangMan.LS(LSID.TreeMerge), CommandCategory.Tools) { }

    public override void Execute(BaseContext baseContext, object obj)
    {
    }
}


internal class TreeSplitCommand : BaseCommand
{
    public TreeSplitCommand() : base("tree_split", LangMan.LS(LSID.TreeSplit), CommandCategory.Tools) { }

    public override void Execute(BaseContext baseContext, object obj)
    {
    }
}


internal class PlacesManagerCommand : BaseCommand
{
    public PlacesManagerCommand() : base("places_manager", LangMan.LS(LSID.PlacesManager), CommandCategory.Tools) { }

    public override void Execute(BaseContext baseContext, object obj)
    {
    }
}


internal class RecMergeCommand : BaseCommand
{
    public RecMergeCommand() : base("record_merge", LangMan.LS(LSID.RecMerge), CommandCategory.Tools) { }

    public override void Execute(BaseContext baseContext, object obj)
    {
    }
}


internal class FamilyGroupsCommand : BaseCommand
{
    public FamilyGroupsCommand() : base("family_groups", LangMan.LS(LSID.FragmentSearch), CommandCategory.Tools) { }

    public override void Execute(BaseContext baseContext, object obj)
    {
    }
}


internal class PatSearchCommand : BaseCommand
{
    public PatSearchCommand() : base("patriarch_search", LangMan.LS(LSID.PatSearch), CommandCategory.Tools) { }

    public override void Execute(BaseContext baseContext, object obj)
    {
    }
}
