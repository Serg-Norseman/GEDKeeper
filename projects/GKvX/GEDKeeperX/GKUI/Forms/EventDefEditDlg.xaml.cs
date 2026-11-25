/*
 *  GEDKeeper, the personal genealogical database editor.
 *  Copyright (C) 2009-2026 by Sergey V. Zhdanovskih.
 *
 *  Licensed under the GNU General Public License (GPL) v3.
 *  See LICENSE file in the project root for full license information.
 */

using GKCore.Controllers;
using GKCore.Design.Views;
using GKCore.Events;

namespace GKUI.Forms
{
    public sealed partial class EventDefEditDlg : CommonDialog<IEventDefEditDlg, EventDefEditDlgController>, IEventDefEditDlg
    {
        public EventDef EventDef
        {
            get { return fController.EventDef; }
            set { fController.EventDef = value; }
        }

        public EventDefEditDlg()
        {
            InitializeComponent();

            fController = new EventDefEditDlgController(this);
            fController.InitView(null);
        }
    }
}
