/*
 *  GEDKeeper, the personal genealogical database editor.
 *  Copyright (C) 2009-2026 by Sergey V. Zhdanovskih.
 *
 *  Licensed under the GNU General Public License (GPL) v3.
 *  See LICENSE file in the project root for full license information.
 */

using GKCore.Controllers;
using GKCore.Design;
using GKCore.Design.Views;

namespace GKUI.Forms
{
    public partial class ChronicleWin : CommonDialog<IChronicleWin, ChronicleController>, IChronicleWin
    {
        public ChronicleWin(IBaseWindow baseWin)
        {
            InitializeComponent();

            fController = new ChronicleController(this);
            fController.Init(baseWin);
            fController.UpdateView();
        }
    }
}
