/*
 *  GEDKeeper, the personal genealogical database editor.
 *  Copyright (C) 2009-2026 by Sergey V. Zhdanovskih.
 *
 *  Licensed under the GNU General Public License (GPL) v3.
 *  See LICENSE file in the project root for full license information.
 */

using Eto.Forms;
using GKCore.Design.Graphics;
using GKCore.Design.Controls;

namespace GKUI.Platform.Handlers
{
    public sealed class LinkButtonHandler : BaseControlHandler<LinkButton, LinkButtonHandler>, ILabel
    {
        public LinkButtonHandler(LinkButton control) : base(control)
        {
        }

        public IColor BackColor
        {
            get { return new ColorHandler(Control.BackgroundColor); }
            set { Control.BackgroundColor = ((ColorHandler)value).Handle; }
        }

        public string Text
        {
            get { return Control.Text; }
            set { Control.Text = value; }
        }
    }
}
