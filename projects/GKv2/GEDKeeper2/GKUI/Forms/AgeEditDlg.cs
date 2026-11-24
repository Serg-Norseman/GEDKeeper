/*
 *  GEDKeeper, the personal genealogical database editor.
 *  Copyright (C) 2009-2026 by Sergey V. Zhdanovskih.
 *
 *  Licensed under the GNU General Public License (GPL) v3.
 *  See LICENSE file in the project root for full license information.
 */

using System.Globalization;
using System.Windows.Forms;
using GDModel;
using GKCore.Controllers;
using GKCore.Design;
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

        public AgeEditDlg(IBaseWindow baseWin)
        {
            InitializeComponent();

            var mask = AgeEditDlgController.GetFixedMask();

            txtVal1.Culture = CultureInfo.InvariantCulture;
            txtVal1.TextMaskFormat = MaskFormat.IncludePromptAndLiterals;
            txtVal1.Mask = mask;

            txtVal2.Culture = CultureInfo.InvariantCulture;
            txtVal2.TextMaskFormat = MaskFormat.IncludePromptAndLiterals;
            txtVal2.Mask = mask;

            fController = new AgeEditDlgController(this);
            fController.InitView(baseWin);
        }
    }
}
