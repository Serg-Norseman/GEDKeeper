/*
 *  "GEDKeeper", the personal genealogical database editor.
 *  Copyright (C) 2009-2025 by Sergey V. Zhdanovskih.
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

        Glyph_FileNew,
        Glyph_FileLoad,
        Glyph_FileSave,
        Glyph_FileProperties,
        Glyph_Export,
        Glyph_ExportTable,
        Glyph_Exit,

        Glyph_RecordAdd,
        Glyph_RecordEdit,
        Glyph_RecordDelete,
        Glyph_Search,
        Glyph_Filter,

        Glyph_TreeAncestors,
        Glyph_TreeDescendants,
        Glyph_TreeBoth,
        Glyph_Pedigree,
        Glyph_Maps,
        Glyph_Stats,

        Glyph_Organizer,
        Glyph_Slideshow,
        Glyph_Settings,

        Glyph_Help,
        Glyph_About,

        Glyph_Prev,
        Glyph_Next,
        Glyph_SendMail,
        Glyph_PartialView,

        Glyph_Accept,
        Glyph_Cancel,

        Glyph_ItemAdd,
        Glyph_ItemEdit,
        Glyph_ItemDelete,

        Glyph_LinkJump,
        Glyph_MoveUp,
        Glyph_MoveDown,

        Glyph_Copy,
        Glyph_Cut,
        Glyph_Paste,

        Glyph_ImageSave,
        Glyph_DocPrint,
        Glyph_DocPreview,

        Glyph_Start,
        Glyph_Stop,

        Glyph_Undo, // not used yet
        Glyph_Redo, // not used yet

        Glyph_Attach,
        Glyph_Detach,

        Glyph_SizeToFit,
        Glyph_ZoomIn,
        Glyph_ZoomOut,
        Glyph_SetPortrait,
    }


    public enum ThemeElementType
    {
        String,
        Float,
        Color,
        Image
    }
}
