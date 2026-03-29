/*
 *  GEDKeeper, the personal genealogical database editor.
 *  Copyright (C) 2009-2026 by Sergey V. Zhdanovskih.
 *
 *  Licensed under the GNU General Public License (GPL) v3.
 *  See LICENSE file in the project root for full license information.
 */

using GKCore.Design.Controls;
using Terminal.Gui;

namespace GKUI.Platform.Handlers
{
    public sealed class GroupBoxHandler : BaseControlHandler<FrameView, GroupBoxHandler>, IGroupBox
    {
        public GroupBoxHandler(FrameView control) : base(control)
        {
        }

        public string Text
        {
            get { return Control.Title; }
            set { Control.Title = value; }
        }
    }
}
