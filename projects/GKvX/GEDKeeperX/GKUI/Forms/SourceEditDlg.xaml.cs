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
using GKCore.Locales;

namespace GKUI.Forms
{
    public sealed partial class SourceEditDlg : CommonDialog<ISourceEditDlg, SourceEditDlgController>, ISourceEditDlg
    {
        public GDMSourceRecord SourceRecord
        {
            get { return fController.SourceRecord; }
            set { fController.SourceRecord = value; }
        }

        public SourceEditDlg(IBaseWindow baseWin)
        {
            InitializeComponent();

            fController = new SourceEditDlgController(this);
            fController.InitView(baseWin);
        }

        private void EditShortTitle_TextChanged(object sender, EventArgs e)
        {
            Title = string.Format("{0} \"{1}\"", LangMan.LS(LSID.Source), txtShortTitle.Text);
        }
    }
}
