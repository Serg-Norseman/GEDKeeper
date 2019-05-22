/*
 *  "GEDKeeper", the personal genealogical database editor.
 *  Copyright (C) 2009-2018 by Sergey V. Zhdanovskih.
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

using System;
using GKCommon.GEDCOM;
using GKCore.MVP;
using GKCore.MVP.Views;
using GKCore.Types;

namespace GKCore.Controllers
{
    /// <summary>
    /// 
    /// </summary>
    public sealed class UserRefEditDlgController : DialogController<IUserRefEditDlg>
    {
        private GDMUserReference fUserRef;

        public GDMUserReference UserRef
        {
            get { return fUserRef; }
            set {
                if (fUserRef != value) {
                    fUserRef = value;
                    UpdateView();
                }
            }
        }


        public UserRefEditDlgController(IUserRefEditDlg view) : base(view)
        {
            fView.Ref.Add("");
            for (SpecialUserRef ur = SpecialUserRef.urRI_StGeorgeCross; ur <= SpecialUserRef.urLast; ur++) {
                string sur = LangMan.LS(GKData.SpecialUserRefs[(int)ur].Title);
                fView.Ref.Add(sur);
            }
        }

        public override bool Accept()
        {
            try {
                fUserRef.StringValue = fView.Ref.Text;
                fUserRef.ReferenceType = fView.RefType.Text;

                return true;
            } catch (Exception ex) {
                Logger.LogWrite("UserRefEditDlgController.Accept(): " + ex.Message);
                return false;
            }
        }

        public override void UpdateView()
        {
            fView.Ref.Text = fUserRef.StringValue;
            fView.RefType.Text = fUserRef.ReferenceType;
        }
    }
}
