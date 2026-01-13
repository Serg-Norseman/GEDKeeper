/*
 *  GEDKeeper, the personal genealogical database editor.
 *  Copyright (C) 2009-2026 by Sergey V. Zhdanovskih.
 *
 *  Licensed under the GNU General Public License (GPL) v3.
 *  See LICENSE file in the project root for full license information.
 */

using GKCore.Design.Controls;
using GKCore.Design.Graphics;

namespace GKUI.Components
{
    /// <summary>
    /// 
    /// </summary>
    public class GKComboItem<T> : ComboItem<T>
    {
        public GKComboItem(string text) : base(text)
        {
        }

        public GKComboItem(string text, T tag) : base(text, tag)
        {
        }

        public GKComboItem(string text, T tag, IImage image) : base(text, tag, image)
        {
        }
    }
}
