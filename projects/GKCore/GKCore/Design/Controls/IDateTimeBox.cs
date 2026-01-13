/*
 *  GEDKeeper, the personal genealogical database editor.
 *  Copyright (C) 2009-2026 by Sergey V. Zhdanovskih.
 *
 *  Licensed under the GNU General Public License (GPL) v3.
 *  See LICENSE file in the project root for full license information.
 */

using System;

namespace GKCore.Design.Controls
{
    /// <summary>
    /// 
    /// </summary>
    public interface IDateTimeBox : IBaseControl
    {
        bool Checked { get; set; }
        DateTime Value { get; set; }
    }
}
