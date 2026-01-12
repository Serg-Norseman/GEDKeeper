/*
 *  GEDKeeper, the personal genealogical database editor.
 *  Copyright (C) 2009-2026 by Sergey V. Zhdanovskih.
 *
 *  Licensed under the GNU General Public License (GPL) v3.
 *  See LICENSE file in the project root for full license information.
 */

using System.Windows.Forms;
using GKCore.Design;
using GKCore.Design.Controls;
using GKCore.Design.Graphics;

namespace GKUI.Platform.Handlers
{
    public sealed class LabelToolItemHandler : ControlHandler<ToolStripLabel, LabelToolItemHandler>, ILabel
    {
        public LabelToolItemHandler(ToolStripLabel control) : base(control)
        {
        }

        public IColor BackColor
        {
            get { return new ColorHandler(Control.BackColor); }
            set { Control.BackColor = ((ColorHandler)value).Handle; }
        }

        public string Text
        {
            get { return Control.Text; }
            set { Control.Text = value; }
        }

        public bool Visible
        {
            get { return Control.Visible; }
            set { Control.Visible = value; }
        }

        public bool Enabled
        {
            get { return true; }
            set { }
        }

        public void Activate()
        {
            // dummy
        }
    }
}
