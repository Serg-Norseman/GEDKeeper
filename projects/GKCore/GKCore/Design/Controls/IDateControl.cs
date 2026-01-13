/*
 *  GEDKeeper, the personal genealogical database editor.
 *  Copyright (C) 2009-2026 by Sergey V. Zhdanovskih.
 *
 *  Licensed under the GNU General Public License (GPL) v3.
 *  See LICENSE file in the project root for full license information.
 */

using System;
using GDModel;

namespace GKCore.Design.Controls
{
    /// <summary>
    /// 
    /// </summary>
    public interface IDateControl : IBaseControl
    {
        GDMCustomDate Date { get; set; }
        GDMDateType FixedDateType { get; set; }

        event EventHandler DateChanged;

        void PasteValue(string value);
    }
}
