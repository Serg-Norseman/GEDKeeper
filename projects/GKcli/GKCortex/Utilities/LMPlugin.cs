/*
 *  GEDKeeper, the personal genealogical database editor.
 *  Copyright (C) 2009-2026 by Sergey V. Zhdanovskih.
 *
 *  Licensed under the GNU General Public License (GPL) v3.
 *  See LICENSE file in the project root for full license information.
 */

using GKCore.Plugins;
using GKCortex.Features;

namespace GKCortex.Utilities;

public abstract class LMPlugin : OrdinaryPlugin
{
    static LMPlugin()
    {
        MCPController.InitFeatures(embedded: true, pureMode: false, tdeMode: true, ragMode: true);
    }
}
