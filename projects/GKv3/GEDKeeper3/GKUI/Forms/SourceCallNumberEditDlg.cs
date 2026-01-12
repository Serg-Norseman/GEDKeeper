/*
 *  GEDKeeper, the personal genealogical database editor.
 *  Copyright (C) 2009-2026 by Sergey V. Zhdanovskih.
 *
 *  Licensed under the GNU General Public License (GPL) v3.
 *  See LICENSE file in the project root for full license information.
 */

using Eto.Forms;
using Eto.Serialization.Xaml;
using GDModel;
using GKCore.Controllers;
using GKCore.Design;
using GKCore.Design.Controls;
using GKCore.Design.Views;

namespace GKUI.Forms
{
    public sealed partial class SourceCallNumberEditDlg : CommonDialog<ISourceCallNumberEditDlg, SourceCallNumberDlgController>, ISourceCallNumberEditDlg
    {
        #region Design components
#pragma warning disable CS0169, CS0649, IDE0044, IDE0051

        private Button btnAccept;
        private Button btnCancel;
        private Label lblCallNumber;
        private TextBox txtNumber;
        private Label lblMediaType;
        private ComboBox cmbMediaType;

#pragma warning restore CS0169, CS0649, IDE0044, IDE0051
        #endregion

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
            XamlReader.Load(this);

            fController = new SourceCallNumberDlgController(this);
            fController.Init(baseWin);
        }
    }
}
