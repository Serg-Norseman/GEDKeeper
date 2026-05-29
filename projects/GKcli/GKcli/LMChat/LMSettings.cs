/*
 *  GEDKeeper, the personal genealogical database editor.
 *  Copyright (C) 2009-2026 by Sergey V. Zhdanovskih.
 *
 *  Licensed under the GNU General Public License (GPL) v3.
 *  See LICENSE file in the project root for full license information.
 */

namespace GKcli.LMChat;

public class LMSettings
{
    public string LocalAPI { get; set; } = string.Empty;
    public string LocalModel { get; set; }
    public int CharThreshold { get; set; }
    public bool Stream { get; set; }
}
