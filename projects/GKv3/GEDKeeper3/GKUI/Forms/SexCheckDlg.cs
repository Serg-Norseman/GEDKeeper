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
using GKCore.Design.Views;
using GKUI.Components;

namespace GKUI.Forms
{
    public sealed partial class SexCheckDlg : CommonDialog<ISexCheckDlg, SexCheckDlgController>, ISexCheckDlg
    {
        #region Design components
#pragma warning disable CS0169, CS0649, IDE0044, IDE0051

        private TextBox txtName;
        private GroupBox grpSex;
        private RadioButton rbNone;
        private RadioButton rbMale;
        private RadioButton rbFemale;
        private Button btnAccept;
        private Button btnCancel;

#pragma warning restore CS0169, CS0649, IDE0044, IDE0051
        #endregion

        public string IndividualName
        {
            get { return txtName.Text; }
            set { txtName.Text = value; }
        }

        public GDMSex Sex
        {
            get { return fController.Sex; }
            set { fController.Sex = value; }
        }

        public SexCheckDlg()
        {
            XamlReader.Load(this);

            UIHelper.FixRadioButtons(this, grpSex);

            fController = new SexCheckDlgController(this);
        }
    }
}
