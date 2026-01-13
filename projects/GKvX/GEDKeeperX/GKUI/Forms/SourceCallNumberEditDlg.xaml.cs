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
using GKCore.Design.Controls;
using GKCore.Design.Views;

namespace GKUI.Forms
{
    public sealed partial class SourceCallNumberEditDlg : CommonDialog<ISourceCallNumberEditDlg, SourceCallNumberDlgController>, ISourceCallNumberEditDlg
    {
        public GDMSourceCallNumber CallNumber
        {
            get { return fController.CallNumber; }
            set { fController.CallNumber = value; }
        }

        #region View Interface

        ITextBox ISourceCallNumberEditDlg.CallNumberText
        {
            get { return GetControlHandler<ITextBox>(txtNumber); }
        }

        IComboBox ISourceCallNumberEditDlg.MediaTypeCombo
        {
            get { return GetControlHandler<IComboBox>(cmbMediaType); }
        }

        #endregion

        public SourceCallNumberEditDlg(IBaseWindow baseWin)
        {
            InitializeComponent();

            fController = new SourceCallNumberDlgController(this);
            fController.Init(baseWin);
        }
    }
}
