/*
 *  GEDKeeper, the personal genealogical database editor.
 *  Copyright (C) 2009-2026 by Sergey V. Zhdanovskih.
 *
 *  Licensed under the GNU General Public License (GPL) v3.
 *  See LICENSE file in the project root for full license information.
 */

using System.Windows.Forms;
using GKCore.Design.Controls;

namespace GKUI.Platform.Handlers
{
    public sealed class SplitterHandler : BaseControlHandler<Splitter, SplitterHandler>, ISplitter
    {
        public SplitterHandler(Splitter control) : base(control)
        {
        }

        public int Position
        {
            get { return Control.SplitPosition; }
            set { Control.SplitPosition = value; }
        }
    }
}
