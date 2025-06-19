/*
 *  "GEDKeeper", the personal genealogical database editor.
 *  Copyright (C) 2009-2025 by Sergey V. Zhdanovskih.
 *
 *  This file is part of "GEDKeeper".
 *
 *  This program is free software: you can redistribute it and/or modify
 *  it under the terms of the GNU General Public License as published by
 *  the Free Software Foundation, either version 3 of the License, or
 *  (at your option) any later version.
 *
 *  This program is distributed in the hope that it will be useful,
 *  but WITHOUT ANY WARRANTY; without even the implied warranty of
 *  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 *  GNU General Public License for more details.
 *
 *  You should have received a copy of the GNU General Public License
 *  along with this program.  If not, see <http://www.gnu.org/licenses/>.
 */

using System.Globalization;
using Eto.Forms;
using Eto.Serialization.Xaml;
using GDModel;
using GKCore;
using GKCore.Controllers;
using GKCore.Design.Controls;
using GKCore.Design.Views;
using GKCore.Interfaces;
using GKUI.Components;

namespace GKUI.Forms
{
    public sealed partial class AgeEditDlg : CommonDialog<IAgeEditDlg, AgeEditDlgController>, IAgeEditDlg
    {
        #region Design components
#pragma warning disable CS0169, CS0649, IDE0044, IDE0051

        private Button btnAccept;
        private Button btnCancel;
        private GroupBox grpBox;
        private ComboBox cmbRel1;
        private ComboBox cmbRel2;
        private Label lblAge1;
        private MaskedTextBox txtVal1;
        private Label lblAge2;
        private MaskedTextBox txtVal2;

#pragma warning restore CS0169, CS0649, IDE0044, IDE0051
        #endregion

        public GDMCustomEvent Event
        {
            get { return fController.Event; }
            set { fController.Event = value; }
        }

        #region View Interface

        IComboBox IAgeEditDlg.RelativeCombo1
        {
            get { return GetControlHandler<IComboBox>(cmbRel1); }
        }

        ITextBox IAgeEditDlg.ValueText1
        {
            get { return GetControlHandler<ITextBox>(txtVal1); }
        }

        IComboBox IAgeEditDlg.RelativeCombo2
        {
            get { return GetControlHandler<IComboBox>(cmbRel2); }
        }

        ITextBox IAgeEditDlg.ValueText2
        {
            get { return GetControlHandler<ITextBox>(txtVal2); }
        }

        #endregion

        public AgeEditDlg(IBaseWindow baseWin)
        {
            XamlReader.Load(this);

            txtVal1.Provider = new FixedMaskedTextProvider(LangMan.LS(LSID.AgeInputMask), CultureInfo.InvariantCulture);
            txtVal2.Provider = new FixedMaskedTextProvider(LangMan.LS(LSID.AgeInputMask), CultureInfo.InvariantCulture);

            btnAccept.Image = UIHelper.LoadResourceImage("Resources.btn_accept.gif");
            btnCancel.Image = UIHelper.LoadResourceImage("Resources.btn_cancel.gif");

            fController = new AgeEditDlgController(this);
            fController.Init(baseWin);
        }
    }
}
