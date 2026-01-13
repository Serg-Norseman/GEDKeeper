/*
 *  GEDKeeper, the personal genealogical database editor.
 *  Copyright (C) 2009-2026 by Sergey V. Zhdanovskih.
 *
 *  Licensed under the GNU General Public License (GPL) v3.
 *  See LICENSE file in the project root for full license information.
 */

namespace GKUI.Themes
{
    public enum ThemeElement
    {
        None,

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

        ToolItemsImageSize,

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
        Glyph_CopyName,

        Glyph_GenderFemale,
        Glyph_GenderMale,
        Glyph_Info,
        Glyph_Bookmark,
        Glyph_Expand,
        Glyph_Expand2,

        Glyph_ShieldNone,
        Glyph_ShieldMid,
        Glyph_ShieldMax,

        Glyph_GEDNew,
        Glyph_GEDLoad,
        Glyph_GEDSave,

        Glyph_CircleAncestors,
        Glyph_CircleDescendants,

        Glyph_Scripts,
        Glyph_FindAndReplace,
        Glyph_View,
        Glyph_Tools,
    }


    public enum ThemeElementType
    {
        String,
        Float,
        Integer,
        Color,
        Image
    }
}
