/*
 *  "GEDKeeper", the personal genealogical database editor.
 *  Copyright (C) 2009-2022 by Sergey V. Zhdanovskih.
 *
 *  This file is part of "GEDKeeper".
 *
 *  This program is free software: you can redistribute it and/or modify
 *  it under the terms of the GNU General Public License as published by
 *  the Free Software Foundation, either version 3 of the License, or
 *  (at your option) any later version.
 *
 *  This program is distributed in the hope that it will be useful,
 *  but WITHOUT ANY WARRANTY; without even the implied warranty of
 *  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 *  GNU General Public License for more details.
 *
 *  You should have received a copy of the GNU General Public License
 *  along with this program.  If not, see <http://www.gnu.org/licenses/>.
 */

namespace GKUI.Themes
{
    public enum ThemeElement
    {
        Font,       // Form, Window, Dialog
        FontSize,

        Editor,     // TextBox and derived, ComboBox
        EditorText,

        Control,    // Panel, GroupBox, TabPage
        ControlText,

        Window,     // Form
        WindowText,

        Dialog,     // Form
        DialogText,

        ButtonFace, // Buttons
        AccentButtonFace,
        ButtonBorder,
        ButtonText,

        Strip,      // MenuStrip, ToolStrip
        Dropdown,
        MenuBorder,
        MenuItemSelected,

        Link,

        Grid,       // ListView and DataGridView
        GridHeader,
        GridHeaderText,
        GridText,

        Tab,        // TabControl
        TabHighlight,
        TabSelected,

        // GEDKeeper specific
        HighlightReadabilityRows,
        HighlightUnparentedIndi,
        HighlightUnmarriedIndi,
        HighlightInaccessibleFiles,

        Glyph_Settings,
        Glyph_Maps,
    }


    public enum ThemeElementType
    {
        String,
        Float,
        Color,
        Image
    }
}
