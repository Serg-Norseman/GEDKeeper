/*
 *  GEDKeeper, the personal genealogical database editor.
 *  Copyright (C) 2009-2026 by Sergey V. Zhdanovskih.
 *
 *  Licensed under the GNU General Public License (GPL) v3.
 *  See LICENSE file in the project root for full license information.
 */

using System;
using NStack;
using Terminal.Gui;

namespace GKUI.Components
{
    public class GKTabControl : TabView
    {
    }

    public class GroupBox : View
    {
    }

    public class TextBox : TextField
    {
    }

    public class MaskedTextBox : TextField
    {
    }

    public class TabPage : TabView.Tab
    {
    }

    public class Panel : View
    {
    }

    public class RadioButton : View
    {
    }

    public class NumericUpDown : View
    {
    }

    public class ImageView : View
    {
    }

    public class GKMapBrowser : View
    {
    }

    public class LogChart : View
    {
    }

    public class TreeMapViewer : View
    {
    }

    public class ToolStrip : MenuBar
    {
    }

    public class ToolStripItem : MenuItem
    {
        public ToolStripItem()
        {
        }

        public ToolStripItem(ustring title, ustring help, Action action) : base(title, help, action)
        {
        }
    }

    public class ToolStripMenuItem : ToolStripItem
    {
        public ToolStripMenuItem()
        {
        }

        public ToolStripMenuItem(ustring title, ustring help, Action action) : base(title, help, action)
        {
        }
    }

    public class ToolStripButton : MenuBarItem
    {
    }

    public class ToolStripDropDownButton : ToolStripButton
    {
    }

    public class SplitContainer : View
    {
    }

    public class ToolStripComboBox : View
    {
    }

    public class ToolStripSeparator : View
    {
    }

    public class TableLayoutPanel : View
    {
    }

    public class KeyEventArgs
    {
    }

    public class KeyPressEventArgs
    {
    }

    public class HintRequestEventArgs
    {
    }

    public class PaintItemEventArgs
    {
    }

    public class FormClosingEventArgs
    {
    }
}
