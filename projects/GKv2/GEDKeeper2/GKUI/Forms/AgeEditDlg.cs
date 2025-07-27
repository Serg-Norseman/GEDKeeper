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
using System.Windows.Forms;
using GDModel;
using GKCore;
using GKCore.Controllers;
using GKCore.Design;
using GKCore.Design.Controls;
using GKCore.Design.Views;

namespace GKUI.Forms
{
    public sealed partial class AgeEditDlg : CommonDialog<IAgeEditDlg, AgeEditDlgController>, IAgeEditDlg
    {
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
            InitializeComponent();

            txtVal1.Culture = CultureInfo.InvariantCulture;
            txtVal1.TextMaskFormat = MaskFormat.IncludePromptAndLiterals;
            txtVal1.Mask = LangMan.LS(LSID.AgeInputMask);

            txtVal2.Culture = CultureInfo.InvariantCulture;
            txtVal2.TextMaskFormat = MaskFormat.IncludePromptAndLiterals;
            txtVal2.Mask = LangMan.LS(LSID.AgeInputMask);

            fController = new AgeEditDlgController(this);
            fController.Init(baseWin);
        }
    }
}
