/*
 *  GEDKeeper, the personal genealogical database editor.
 *  Copyright (C) 2009-2026 by Sergey V. Zhdanovskih.
 *
 *  Licensed under the GNU General Public License (GPL) v3.
 *  See LICENSE file in the project root for full license information.
 */

using System.Collections.Generic;
using GKCore.Design.Graphics;

namespace GKUI.Themes
{
    public interface IThemeManager
    {
        List<Theme> Themes { get; }

        void ApplyTheme(IThemedView view);
        void ApplyTheme(IThemedView view, object component);
        IImage GetThemeImage(ThemeElement element, bool require = false);
        void LoadThemes();
        bool SetTheme(string name);
    }
}
