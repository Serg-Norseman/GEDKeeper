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

using System;
using GDModel;
using GDModel.Providers.GEDCOM;
using GKCore.Locales;

namespace GKCore.Validation.Concrete
{
    public class GDMEventValidator : BaseValidator<GDMCustomEvent>
    {
        public override ValidationResult Validate(GDMCustomEvent obj, bool suppressWarnings = false)
        {
            if (obj == null)
                throw new ArgumentNullException(nameof(obj));

            var result = new ValidationResult();

            if (!CheckMaximumLength(obj.Cause, GEDCOMConsts.Event_Cause_MaxLength))
                result.AddError(string.Format(LangMan.LS(LSID.FieldIsLonger), LangMan.LS(LSID.Cause), GEDCOMConsts.Event_Cause_MaxLength));

            if (!CheckMaximumLength(obj.Agency, GEDCOMConsts.Event_Agency_MaxLength))
                result.AddError(string.Format(LangMan.LS(LSID.FieldIsLonger), LangMan.LS(LSID.Agency), GEDCOMConsts.Event_Agency_MaxLength));

            return result;
        }
    }
}
