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

using GDModel;
using GKCore.Design.Controls;
using GKCore.Interfaces;
using GKCore.Lists;
using GKCore.Design.Views;
using GKCore.Types;
using GKUI.Themes;

namespace GKCore.Controllers
{
    public class StdPersonEditDlgController : PersonEditDlgController<IStdPersonEditDlg>
    {
        public StdPersonEditDlgController(IStdPersonEditDlg view) : base(view)
        {
        }

        public override void Init(IBaseWindow baseWin)
        {
            base.Init(baseWin);

            fView.ChildrenList.ListModel = new IndividualChildrenListModel(fView, baseWin, fLocalUndoman);
        }

        public override void Done()
        {
            base.Done();

            fView.ChildrenList.ListModel.SaveSettings();
        }

        protected override void UpdateListModels(GDMIndividualRecord indiRec)
        {
            base.UpdateListModels(indiRec);

            fView.ChildrenList.ListModel.DataOwner = indiRec;
        }

        protected override void UpdateLocked(bool locked)
        {
            base.UpdateLocked(locked);

            fView.NamePrefix.Enabled = !locked;
            fView.SurnamePrefix.Enabled = !locked;
            fView.NameSuffix.Enabled = !locked;

            fView.ChildrenList.ReadOnly = locked;
        }

        public override void UpdateNameControls(GDMPersonalName np)
        {
            base.UpdateNameControls(np);

            if (np != null) {
                fView.NamePrefix.Text = np.NamePrefix;
                fView.SurnamePrefix.Text = np.SurnamePrefix;
                fView.NameSuffix.Text = np.NameSuffix;
            } else {
                fView.NamePrefix.Text = "";
                fView.SurnamePrefix.Text = "";
                fView.NameSuffix.Text = "";
            }
        }

        protected override void AcceptNameParts(GDMPersonalName persName)
        {
            base.AcceptNameParts(persName);

            persName.NamePrefix = fView.NamePrefix.Text;
            persName.SurnamePrefix = fView.SurnamePrefix.Text;
            persName.NameSuffix = fView.NameSuffix.Text;
        }

        public override void SetLocale()
        {
            base.SetLocale();

            GetControl<ILabel>("lblSurnamePrefix").Text = LangMan.LS(LSID.SurnamePrefix);
            GetControl<ILabel>("lblNamePrefix").Text = LangMan.LS(LSID.NamePrefix);
            GetControl<ILabel>("lblNameSuffix").Text = LangMan.LS(LSID.NameSuffix);

            GetControl<ITabPage>("pageChilds").Text = LangMan.LS(LSID.Childs);
        }

        public override void ApplyTheme()
        {
            base.ApplyTheme();

            if (!AppHost.Instance.HasFeatureSupport(Feature.Themes)) return;

            fView.ChildrenList.ApplyTheme();
        }
    }
}
