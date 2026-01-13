/*
 *  GEDKeeper, the personal genealogical database editor.
 *  Copyright (C) 2009-2026 by Sergey V. Zhdanovskih.
 *
 *  Licensed under the GNU General Public License (GPL) v3.
 *  See LICENSE file in the project root for full license information.
 */

using GKCore.Locales;

namespace GKCore.Options
{
    /// <summary>
    /// 
    /// </summary>
    public interface IOptionsControl : ILocalizable
    {
        IOptions Options { get; set; }
        
        void AcceptChanges();
        void UpdateControls();
    }
}
