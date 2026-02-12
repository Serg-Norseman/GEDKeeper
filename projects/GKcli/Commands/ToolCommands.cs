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
    public RecMergeCommand() : base("record_merge", LSID.RecMerge, CommandCategory.Tools) { }

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
