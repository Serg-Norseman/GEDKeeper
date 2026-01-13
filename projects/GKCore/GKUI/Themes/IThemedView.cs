/*
 *  GEDKeeper, the personal genealogical database editor.
 *  Copyright (C) 2009-2026 by Sergey V. Zhdanovskih.
 *
 *  Licensed under the GNU General Public License (GPL) v3.
 *  See LICENSE file in the project root for full license information.
 */

using System;

namespace GKUI.Themes
{
    public interface IThemedView
    {
        void ApplyTheme();
    }


    public interface IThemedForm : IThemedView
    {
        bool SkipTheme(IDisposable control);
    }
}
