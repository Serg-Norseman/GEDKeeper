/*
 *  GEDKeeper, the personal genealogical database editor.
 *  Copyright (C) 2009-2026 by Sergey V. Zhdanovskih.
 *
 *  Licensed under the GNU General Public License (GPL) v3.
 *  See LICENSE file in the project root for full license information.
 */

namespace GKcli.Features;

internal class TreeCheckTool : BaseTool
{
    public TreeCheckTool() : base("tree_check") { }
}


internal class TreeCompareTool : BaseTool
{
    public TreeCompareTool() : base("tree_compare") { }
}


internal class TreeSplitTool : BaseTool
{
    public TreeSplitTool() : base("tree_split") { }
}


internal class PlacesManagerTool : BaseTool
{
    public PlacesManagerTool() : base("places_manager") { }
}


internal class FamilyGroupsTool : BaseTool
{
    public FamilyGroupsTool() : base("family_groups") { }
}


internal class PatSearchTool : BaseTool
{
    public PatSearchTool() : base("patriarch_search") { }
}
