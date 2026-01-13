/*
 *  GEDKeeper, the personal genealogical database editor.
 *  Copyright (C) 2009-2026 by Sergey V. Zhdanovskih.
 *
 *  Licensed under the GNU General Public License (GPL) v3.
 *  See LICENSE file in the project root for full license information.
 */

namespace GKCore.Options
{
    /// <summary>
    /// 
    /// </summary>
    public interface IOptions
    {
        void Assign(IOptions source);
        void ResetDefaults();
    }


    public interface IFontOptions
    {
        string DefFontName { get; set; }
        int DefFontSize { get; set; }
    }
}
