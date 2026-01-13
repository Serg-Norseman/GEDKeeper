/*
 *  GEDKeeper, the personal genealogical database editor.
 *  Copyright (C) 2009-2026 by Sergey V. Zhdanovskih.
 *
 *  Licensed under the GNU General Public License (GPL) v3.
 *  See LICENSE file in the project root for full license information.
 */

using GKCore.Design.Controls;
using GKCore.Design.Views;
using GKCore.Locales;

namespace GKCore.Controllers
{
    public class MobPersonEditDlgController : PersonEditDlgController<IMobPersonEditDlg>
    {
        public MobPersonEditDlgController(IMobPersonEditDlg view) : base(view)
        {
        }

        public override void SetLocale()
        {
            base.SetLocale();

            GetControl<ITabPage>("pagePortrait").Text = LangMan.LS(LSID.Portrait);
        }
    }
}
