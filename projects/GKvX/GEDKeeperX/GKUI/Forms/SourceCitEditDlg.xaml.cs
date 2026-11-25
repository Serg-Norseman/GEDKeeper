/*
 *  GEDKeeper, the personal genealogical database editor.
 *  Copyright (C) 2009-2026 by Sergey V. Zhdanovskih.
 *
 *  Licensed under the GNU General Public License (GPL) v3.
 *  See LICENSE file in the project root for full license information.
 */

using System;
using GDModel;
using GKCore.Controllers;
using GKCore.Design;
using GKCore.Design.Views;

namespace GKUI.Forms
{
    public sealed partial class SourceCitEditDlg : CommonDialog<ISourceCitEditDlg, SourceCitEditDlgController>, ISourceCitEditDlg
    {
        public GDMSourceCitation SourceCitation
        {
            get { return fController.SourceCitation; }
            set { fController.SourceCitation = value; }
        }

        public SourceCitEditDlg(IBaseWindow baseWin)
        {
            InitializeComponent();

            fController = new SourceCitEditDlgController(this);
            fController.InitView(baseWin);
        }

        private void btnSourceAdd_Click(object sender, EventArgs e)
        {
            fController.AddSource();
        }
    }
}
