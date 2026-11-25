/*
 *  GEDKeeper, the personal genealogical database editor.
 *  Copyright (C) 2009-2026 by Sergey V. Zhdanovskih.
 *
 *  Licensed under the GNU General Public License (GPL) v3.
 *  See LICENSE file in the project root for full license information.
 */

using GDModel;
using GKCore.Controllers;
using GKCore.Design;
using GKCore.Design.Views;

namespace GKUI.Forms
{
    public partial class ResearchEditDlg : CommonDialog<IResearchEditDlg, ResearchEditDlgController>, IResearchEditDlg
    {
        public GDMResearchRecord ResearchRecord
        {
            get { return fController.ResearchRecord; }
            set { fController.ResearchRecord = value; }
        }

        public ResearchEditDlg(IBaseWindow baseWin)
        {
            InitializeComponent();

            fController = new ResearchEditDlgController(this);
            fController.InitView(baseWin);
        }
    }
}
